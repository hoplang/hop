use crate::hop::typing::{
    DataType, EnumVariant, RecordField, ResolvedType, Ty, Type, TypeRegistry, TypeRegistryBuilder,
};
use crate::ir::flat_module::FlatModule;
use crate::ir::flat_module_builder::{
    ArmBindings, DeclaredFunction, FlatBlockBuilder, FlatModuleBuilder, Pat,
};
use crate::ir::ir_function::IrFunction;
use crate::ir::runtime::random::random_value;
use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use arbitrary::Unstructured;
use rand::Rng;
use std::collections::HashMap;
use std::ops::RangeInclusive;

/// How deep the blocks of a body nest, in the arms of matches and the
/// bodies of loops.
const MAX_BLOCK_DEPTH: usize = 5;

/// How deep the ops of an expression nest, an op reading the result of
/// another op as an operand.
const MAX_EXPRESSION_DEPTH: usize = 15;

/// How deep the values of a literal nest before they start to converge.
const MAX_LITERAL_DEPTH: usize = 5;

/// One block in this many binds a value nothing asked for.
const DEAD_BINDING_ODDS: usize = 128;

const FLOATS: &[f64] = &[
    0.0,
    -0.0,
    0.1,
    0.2,
    0.3,
    0.5,
    -0.5,
    1.5,
    -2.5,
    3.75,
    99.75,
    100.25,
    2147483647.5,
    -2147483648.5,
    9.2e18,
    1e300,
    -1e300,
    5e-324,
    f64::MAX,
    f64::INFINITY,
    f64::NEG_INFINITY,
    f64::NAN,
];

const SCALARS: &[Ty] = &[Ty::Int, Ty::String, Ty::Bool, Ty::Float];

/// Elements to generate, each with whether it is void.
const ELEMENTS: &[(&str, bool)] = &[
    ("div", false),
    ("span", false),
    ("p", false),
    ("my-widget", false),
    ("br", true),
    ("input", true),
];

/// Markup text to generate, which never contains `<`, `{` or `}`.
const TEXTS: &[&str] = &[
    "",
    "foo",
    "&amp;",
    "a\"b'c",
    "back\\slash",
    "line\nbreak",
    "cr\rtab\t",
];

const STRING_LITERALS: &[&str] = &[
    "",
    "foo",
    "bar",
    "<b>",
    "&amp;",
    "a\"b'c",
    "back\\slash",
    "`${x}`",
    "{curly}",
    "line\nbreak",
    "cr\rtab\t",
];

/// The names a block may read, in the order they came into scope, each
/// with whether the path has used it. The block builder knows their
/// types.
#[derive(Default)]
struct Scope {
    names: Vec<(ValueId, bool)>,
}

impl Scope {
    /// Record that `value` is bound in the block being generated, and
    /// give it back.
    fn bound(&mut self, value: ValueId) -> ValueId {
        self.names.push((value, false));
        value
    }

    /// Record that the path uses the Html value `value`, which it may not
    /// read again.
    fn used(&mut self, value: ValueId) {
        if let Some((_, used)) = self.names.iter_mut().rev().find(|(name, _)| *name == value) {
            *used = true;
        }
    }

    /// The names in scope the path may still read whose type fits, in
    /// the order they came into scope.
    fn names_where<'a>(
        &'a self,
        types: &'a [Type],
        fits: impl Fn(&Type) -> bool + 'a,
    ) -> impl Iterator<Item = ValueId> + 'a {
        self.names
            .iter()
            .filter(move |(name, used)| !used && fits(&types[name.index()]))
            .map(|(name, _)| *name)
    }

    /// Which names the path has used, to come back to at a fork.
    fn snapshot(&self) -> Vec<bool> {
        self.names.iter().map(|(_, used)| *used).collect()
    }

    /// Forget the names bound since `snapshot` was taken and take its
    /// record of which names are used.
    fn restore(&mut self, snapshot: &[bool]) {
        self.names.truncate(snapshot.len());
        for ((_, used), saved) in self.names.iter_mut().zip(snapshot) {
            *used = *saved;
        }
    }

    /// Add the names this path has used to `joined`, which the paths of a
    /// fork share, so that after the fork a name used on any path is used.
    fn join(&self, joined: &mut [bool]) {
        for ((_, used), joined) in self.names.iter().zip(joined) {
            *joined |= *used;
        }
    }
}

/// Which named types may be referenced at a type position.
#[derive(Clone, Copy)]
enum NamedTypes {
    /// Every name the module declares, including declarations that are
    /// not complete yet.
    Any,
    /// Only names whose declarations are already complete.
    Complete,
}

struct FlatGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    declared_names: Vec<String>,
    /// The names of the records whose declarations are complete, in the
    /// order they are declared.
    record_names: Vec<String>,
    /// The names of the enums whose declarations are complete, in the
    /// order they are declared.
    enum_names: Vec<String>,
    /// The records of the module, in the order of `record_names`.
    records: Vec<Type>,
    /// The enums of the module, in the order of `enum_names`.
    enums: Vec<Type>,
    /// How many more ops the expression being generated may nest.
    expression_depth: usize,
}

/// The definition of `typ`, when it is data.
fn resolve<'t>(registry: &'t TypeRegistry, typ: &'t Type) -> Option<ResolvedType<'t>> {
    typ.as_data().and_then(|data| registry.resolve(data))
}

/// Generate a random well-typed Flat module.
pub fn random_module(u: &mut Unstructured<'_>) -> (FlatModule, TypeRegistry) {
    random_module_inner(u, false)
}

pub fn random_module_with_test_view(u: &mut Unstructured<'_>) -> (FlatModule, TypeRegistry) {
    random_module_inner(u, true)
}

fn random_module_inner(
    u: &mut Unstructured<'_>,
    single_test_entrypoint: bool,
) -> (FlatModule, TypeRegistry) {
    let mut g = FlatGenerator {
        u,
        declared_names: Vec::new(),
        record_names: Vec::new(),
        enum_names: Vec::new(),
        records: Vec::new(),
        enums: Vec::new(),
        expression_depth: MAX_EXPRESSION_DEPTH,
    };

    // Name every type before generating any field types, so that a field
    // can refer to a type declared later or to the one being declared.
    let record_count = g.count(0..=3);
    let enum_count = g.count(0..=3);
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut types = TypeRegistryBuilder::new();

    // Generate records and enums in a shuffled interleaved order.
    enum Decl {
        Record(usize),
        Enum(usize),
    }
    let decls: Vec<Decl> = (0..record_count)
        .map(Decl::Record)
        .chain((0..enum_count).map(Decl::Enum))
        .collect();
    for decl in g.shuffled(decls) {
        match decl {
            Decl::Record(i) => {
                let name = format!("R{i}");
                let fields = (0..g.count(0..=4))
                    .map(|j| (format!("f{j}"), g.random_field_type(2)))
                    .collect::<Vec<_>>();
                types = types.record(&name, fields.iter().map(|(f, t)| (f.as_str(), t.clone())));
                g.record_names.push(name);
            }
            Decl::Enum(i) => {
                let name = format!("E{i}");
                let variants: Vec<(String, Vec<(String, Ty)>)> = (0..g.count(1..=3))
                    .map(|j| {
                        let fields = (0..g.count(0..=2))
                            .map(|k| (format!("f{k}"), g.random_field_type(2)))
                            .collect();
                        (format!("W{j}"), fields)
                    })
                    .collect();
                types = types.enum_(
                    &name,
                    variants.iter().map(|(v, fs)| {
                        (
                            v.as_str(),
                            fs.iter().map(|(f, t)| (f.as_str(), t.clone())).collect(),
                        )
                    }),
                );
                g.enum_names.push(name);
            }
        }
    }

    g.records = g
        .record_names
        .iter()
        .map(|name| Ty::named(name).into())
        .collect();
    g.enums = g
        .enum_names
        .iter()
        .map(|name| Ty::named(name).into())
        .collect();
    let mut builder = FlatModuleBuilder::with_registry(types.build());

    // Declare every function before defining any, so that a body can call
    // a function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let params: Vec<(String, Ty)> = (0..g.count(0..=3))
            .map(|i| (format!("p{i}"), g.random_type(2)))
            .collect();
        let return_type = g.random_type(2);
        builder = builder.declare(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.clone())),
            return_type,
        );
    }

    // Generate entrypoint functions, each rendering Html as a page body does.
    // Their parameters are drawn before any body, like those of the
    // functions above.
    let entrypoints: Vec<(String, Vec<(String, Ty)>)> = if single_test_entrypoint {
        vec![("Test".to_string(), Vec::new())]
    } else {
        (0..g.count(1..=3))
            .map(|i| {
                let params = (0..g.count(0..=3))
                    .map(|i| (format!("p{i}"), g.random_type(2)))
                    .collect();
                (format!("V{i}"), params)
            })
            .collect()
    };

    for name in &function_names {
        builder = builder.define(name, |b, declared| {
            let mut scope = Scope::default();
            for parameter in &declared.parameters {
                scope.bound(parameter.binder.var);
            }
            g.block(b, &mut scope, &declared.return_type, MAX_BLOCK_DEPTH)
        });
    }
    for (name, params) in &entrypoints {
        builder = builder.entrypoint(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.clone())),
            Ty::Html,
            |b| {
                let mut scope = Scope::default();
                for (n, _) in params {
                    scope.bound(b.param(n));
                }
                g.block(b, &mut scope, &Type::Html, MAX_BLOCK_DEPTH)
            },
        );
    }
    builder.build_with_registry()
}

/// Random arguments for each entrypoint function of the module, named by its
/// parameters.
pub fn random_entrypoint_args(
    module: &FlatModule,
    rng: &mut impl Rng,
    registry: &TypeRegistry,
) -> Vec<(IrFunction, HashMap<AttributeName, Value>)> {
    module
        .functions
        .iter()
        .filter(|decl| decl.entrypoint)
        .map(|decl| {
            let args = decl
                .parameters
                .iter()
                .map(|param| {
                    let value = random_value(rng, &param.binder.typ, None, registry);
                    (param.name.clone(), value)
                })
                .collect();
            (decl.function.clone(), args)
        })
        .collect()
}

impl FlatGenerator<'_, '_> {
    /// A choice within `range`, resolving to the lower bound once the
    /// input is exhausted.
    fn count(&mut self, range: RangeInclusive<usize>) -> usize {
        self.u.int_in_range(range).unwrap()
    }

    /// A choice of index into a nonempty collection of length `len`.
    fn index(&mut self, len: usize) -> usize {
        self.u.choose_index(len).unwrap()
    }

    fn coin(&mut self) -> bool {
        self.u.arbitrary().unwrap()
    }

    /// A zigzag-encoded choice in `-magnitude..=magnitude`.
    fn zigzag(&mut self, magnitude: i32) -> i32 {
        let n = self.u.int_in_range(0..=2 * magnitude).unwrap();
        if n % 2 == 0 { -n / 2 } else { n / 2 + 1 }
    }

    /// A random permutation of items.
    fn shuffled<T>(&mut self, mut items: Vec<T>) -> Vec<T> {
        let mut out = Vec::with_capacity(items.len());
        while !items.is_empty() {
            out.push(items.swap_remove(self.index(items.len())));
        }
        out
    }

    /// Pairs of `(record, field)` where the field type is the given type.
    fn record_fields_of_type<'s>(
        &'s self,
        b: &FlatBlockBuilder<'s>,
        target: &'s Type,
    ) -> impl Iterator<Item = (&'s Type, &'s RecordField)> + 's {
        let registry = b.registry();
        self.records.iter().flat_map(move |record| {
            let Some(ResolvedType::Record { fields, .. }) = resolve(registry, record) else {
                unreachable!("a record of the module resolves to its declaration")
            };
            fields
                .iter()
                .filter(move |field| target.as_data() == Some(&field.typ))
                .map(move |field| (record, field))
        })
    }

    /// The functions whose calls produce a value of `target`. An entrypoint is
    /// never called.
    fn callees<'t>(
        b: &FlatBlockBuilder<'t>,
        target: &'t Type,
    ) -> impl Iterator<Item = &'t DeclaredFunction> + 't {
        b.functions()
            .iter()
            .filter(move |function| !function.entrypoint && function.return_type == *target)
    }

    /// With even odds, pick a name in scope whose type fits, when there is
    /// one, with its type.
    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&Type) -> bool,
    ) -> Option<(ValueId, Type)> {
        let types = b.value_types();
        if scope.names_where(&types, &fits).next().is_none() || !self.coin() {
            return None;
        }
        let count = scope.names_where(&types, &fits).count();
        let index = self.index(count);
        let name = scope.names_where(&types, &fits).nth(index).unwrap();
        Some((name, types[name.index()].clone()))
    }

    /// A random type, Html included.
    fn random_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Any, true)
    }

    /// A random type of data, which can be part of another type.
    fn random_data_type(&mut self, depth: usize) -> DataType {
        self.random_type_where(depth, NamedTypes::Any, false).into()
    }

    /// A random type for a field of the record or enum currently being
    /// declared.
    fn random_field_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Complete, false)
    }

    /// A random type. It is Html only when `html` allows it, and the types
    /// it is made of never are, since Html is never part of another type.
    fn random_type_where(&mut self, depth: usize, named: NamedTypes, html: bool) -> Ty {
        enum P {
            Scalar,
            Html,
            Named,
            Array,
            Option,
            Tuple,
        }
        let mut kinds = vec![P::Scalar];
        let named_count = self.named_type_count(named);
        if html {
            kinds.push(P::Html);
        }
        if named_count > 0 {
            kinds.push(P::Named);
        }
        if depth > 0 {
            kinds.extend([P::Array, P::Option, P::Tuple]);
        }
        match self.u.choose(&kinds).unwrap() {
            P::Scalar => SCALARS[self.index(SCALARS.len())].clone(),
            P::Html => Ty::Html,
            P::Named => {
                let i = self.index(named_count);
                self.named_type(named, i)
            }
            P::Array => Ty::array(self.random_type_where(depth - 1, NamedTypes::Any, false)),
            P::Option => Ty::option(self.random_type_where(depth - 1, NamedTypes::Any, false)),
            P::Tuple => {
                let arity = self.count(0..=3);
                Ty::tuple((0..arity).map(|_| self.random_type_where(depth - 1, named, false)))
            }
        }
    }

    fn named_type_count(&self, named: NamedTypes) -> usize {
        match named {
            NamedTypes::Any => self.declared_names.len(),
            NamedTypes::Complete => self.record_names.len() + self.enum_names.len(),
        }
    }

    fn named_type(&self, named: NamedTypes, i: usize) -> Ty {
        match named {
            NamedTypes::Any => Ty::named(&self.declared_names[i]),
            NamedTypes::Complete if i < self.record_names.len() => Ty::named(&self.record_names[i]),
            NamedTypes::Complete => Ty::named(&self.enum_names[i - self.record_names.len()]),
        }
    }

    /// Generate the body of one arm of a match, starting from the scope
    /// as it was `before` the match, and record in `after` what the arm
    /// uses.
    fn arm(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        before: &[bool],
        after: &mut [bool],
        bindings: &ArmBindings,
        binds: &[(String, Type)],
        target: &Type,
        depth: usize,
    ) -> ValueId {
        scope.restore(before);
        for (name, _) in binds {
            scope.bound(bindings.binding(name));
        }
        let body = self.block(b, scope, target, depth);
        scope.join(after);
        body
    }

    /// Generate the body of a loop, which may not read the Html values
    /// bound outside it, and leave the scope as it was.
    fn loop_body(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        var: Option<ValueId>,
        depth: usize,
    ) -> ValueId {
        let before = scope.snapshot();
        {
            let types = b.value_types();
            for (name, used) in &mut scope.names {
                *used |= types[name.index()] == Type::Html;
            }
        }
        if let Some(var) = var {
            scope.bound(var);
        }
        let body = self.block(b, scope, &Type::Html, depth);
        scope.restore(&before);
        body
    }

    /// Generate the result of a block of the target type, binding what it
    /// reads before it. The blocks it nests nest at most `depth` deep. The
    /// caller uses the result once, so an Html result counts as used.
    fn block(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
    ) -> ValueId {
        // One block in DEAD_BINDING_ODDS binds a value nothing asked for,
        // so that the transforms that eliminate dead bindings have some to
        // eliminate. A later op may still read it. The last index is the
        // one an exhausted input never gives, so a minimal module has no
        // dead binding.
        if self.index(DEAD_BINDING_ODDS) == DEAD_BINDING_ODDS - 1 {
            let typ = Type::from(self.random_type(1));
            self.production(b, scope, &typ, depth);
        }
        let result = if self.coin() {
            self.operand(b, scope, target, depth)
        } else {
            self.production(b, scope, target, depth)
        };
        if *target == Type::Html {
            scope.used(result);
        }
        result
    }

    /// A value of type `typ` for an op to read: two times in three an op of
    /// its own while the expression may still nest, else a name in scope
    /// or a literal. The blocks it nests nest at most `depth` deep. An
    /// Html value counts as used.
    fn operand(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &Type,
        depth: usize,
    ) -> ValueId {
        if self.expression_depth == 0 || self.index(3) == 0 {
            return self.value(b, scope, typ, MAX_LITERAL_DEPTH);
        }
        self.expression_depth -= 1;
        let value = self.production(b, scope, typ, depth);
        self.expression_depth += 1;
        if *typ == Type::Html {
            scope.used(value);
        }
        value
    }

    /// A name in scope of type `typ` that may be read, or a literal that
    /// nests at most `depth` values deep. An Html value counts as used.
    fn value(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &Type,
        depth: usize,
    ) -> ValueId {
        let candidate = {
            let types = b.value_types();
            let count = scope
                .names_where(&types, |candidate| *candidate == *typ)
                .count();
            let index = self.index(count + 1);
            scope
                .names_where(&types, |candidate| *candidate == *typ)
                .nth(index)
        };
        let value = match candidate {
            Some(name) => name,
            None => self.leaf(b, scope, typ, depth),
        };
        if *typ == Type::Html {
            scope.used(value);
        }
        value
    }

    /// Plan the arms of an enum match: every variant in a random order,
    /// each with the fields it binds and their types.
    fn enum_match_arms(&mut self, variants: &[EnumVariant]) -> Vec<(String, Vec<(String, Type)>)> {
        let mut arms = Vec::new();
        for variant in self.shuffled(variants.iter().collect()) {
            let mut bindings = Vec::new();
            for field in &variant.fields {
                if self.coin() {
                    bindings.push((
                        field.name.as_str().to_string(),
                        Type::Data(field.typ.clone()),
                    ));
                }
            }
            arms.push((variant.name.as_str().to_string(), bindings));
        }
        arms
    }

    /// A pattern that binds a name of type `typ`, added to `binds` under a
    /// name numbered by its position among the names bound so far.
    fn bind(binds: &mut Vec<(String, Type)>, typ: &Type) -> Pat {
        let name = format!("x{}", binds.len());
        binds.push((name.clone(), typ.clone()));
        Pat::Bind(name)
    }

    /// A random pattern for values of `typ` that tests at most `depth`
    /// constructors on any path from its root, with the names it binds and
    /// their types added to `binds`. A pattern of depth zero matches every
    /// value.
    fn pattern(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        typ: &Type,
        depth: usize,
        binds: &mut Vec<(String, Type)>,
    ) -> Pat {
        if depth > 0 && self.index(3) > 0 {
            match typ {
                Type::Data(DataType::Bool) => {
                    return if self.coin() { Pat::True } else { Pat::False };
                }
                Type::Data(DataType::Option(inner)) => {
                    return if self.coin() {
                        let inner = Type::Data((**inner).clone());
                        Pat::some(self.pattern(b, &inner, depth - 1, binds))
                    } else {
                        Pat::None
                    };
                }
                Type::Data(DataType::Tuple(elements)) => {
                    return Pat::Tuple(
                        elements
                            .iter()
                            .map(|element| {
                                self.pattern(b, &Type::Data(element.clone()), depth - 1, binds)
                            })
                            .collect(),
                    );
                }
                Type::Data(DataType::Named { .. }) => {
                    let (variant, fields) = match resolve(b.registry(), typ) {
                        Some(ResolvedType::Record { fields, .. }) => (None, fields),
                        Some(ResolvedType::Enum { variants, .. }) => {
                            let variant = self.u.choose(variants).unwrap();
                            (Some(variant.name.as_str().to_string()), &variant.fields[..])
                        }
                        _ => unreachable!("a named type is a declared record or enum"),
                    };
                    let fields = fields
                        .iter()
                        .map(|field| {
                            let pattern =
                                self.pattern(b, &Type::Data(field.typ.clone()), depth - 1, binds);
                            (field.name.as_str().to_string(), pattern)
                        })
                        .collect();
                    return match variant {
                        Some(variant) => Pat::Variant(variant, fields),
                        None => Pat::Record(fields),
                    };
                }
                _ => {}
            }
        }
        if self.coin() {
            return Pat::Wildcard;
        }
        Self::bind(binds, typ)
    }

    /// Bind a random op of the target type in the block `b` builds, with
    /// its operands taken from what is in scope, and give its name. The
    /// blocks it nests nest at most `depth` deep.
    fn production(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
    ) -> ValueId {
        enum P {
            Leaf,
            FieldAccess,
            MatchPattern,
            MatchBool,
            MatchOption,
            MatchEnum,
            Eq,
            Lt,
            Lte,
            Not,
            Concat,
            IntToString,
            FloatToInt,
            IntToFloat,
            OptionIsNone,
            OptionIsSome,
            StringIsEmpty,
            ArrayIsEmpty,
            ArrayLength,
            ArrayFromRange,
            TupleIndex,
            Neg,
            Add,
            Sub,
            Mul,
            HtmlEscape,
            HtmlElement,
            HtmlText,
            HtmlConcat,
            HtmlForArray,
            HtmlForRange,
            Call,
        }
        let mut productions = vec![P::Leaf];
        if self.record_fields_of_type(b, target).next().is_some() {
            productions.push(P::FieldAccess);
        }
        if *target != Type::Html {
            productions.push(P::TupleIndex);
        }
        if *target == Type::BOOL {
            productions.extend([
                P::Eq,
                P::Lt,
                P::Lte,
                P::Not,
                P::OptionIsNone,
                P::OptionIsSome,
                P::StringIsEmpty,
                P::ArrayIsEmpty,
            ]);
        }
        if *target == Type::INT {
            productions.extend([
                P::ArrayLength,
                P::Neg,
                P::Add,
                P::Sub,
                P::Mul,
                P::FloatToInt,
            ]);
        }
        if *target == Type::FLOAT {
            productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::IntToFloat]);
        }
        if matches!(target.as_data(), Some(DataType::Array(element)) if **element == DataType::Int)
        {
            productions.push(P::ArrayFromRange);
        }
        if *target == Type::STRING {
            productions.extend([P::Concat, P::IntToString]);
        }
        if *target == Type::Html {
            productions.extend([P::HtmlText, P::HtmlEscape, P::HtmlElement, P::HtmlConcat]);
        }
        if Self::callees(b, target).next().is_some() {
            productions.push(P::Call);
        }
        // Only an op that may still nest a block is one that nests one.
        if depth > 0 {
            productions.extend([P::MatchBool, P::MatchOption, P::MatchPattern]);
            if !self.enums.is_empty() {
                productions.push(P::MatchEnum);
            }
            if *target == Type::Html {
                productions.extend([P::HtmlForArray, P::HtmlForRange]);
            }
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::Leaf => return self.leaf(b, scope, target, MAX_LITERAL_DEPTH),
            P::FieldAccess => {
                let record_fields: Vec<(Type, String)> = self
                    .record_fields_of_type(b, target)
                    .map(|(record, field)| (record.clone(), field.name.as_str().to_string()))
                    .collect();
                let var = self.var_in_scope(b, scope, |ty| {
                    record_fields.iter().any(|(record, _)| record == ty)
                });
                match var {
                    Some((object, record)) => {
                        let fields: Vec<&String> = record_fields
                            .iter()
                            .filter(|(candidate, _)| *candidate == record)
                            .map(|(_, field)| field)
                            .collect();
                        b.field_access(object, self.u.choose(&fields).unwrap())
                    }
                    None => {
                        let (record, field) = self.u.choose(&record_fields).unwrap().clone();
                        let object = self.operand(b, scope, &record, depth);
                        b.field_access(object, &field)
                    }
                }
            }
            P::MatchBool => {
                let subject = self.operand(b, scope, &Type::BOOL, depth);
                let before = scope.snapshot();
                let mut after = before.clone();
                let result = b.match_(subject, |m| {
                    m.arm(Pat::True, |b, bindings| {
                        self.arm(
                            b,
                            scope,
                            &before,
                            &mut after,
                            bindings,
                            &[],
                            target,
                            depth - 1,
                        )
                    });
                    m.arm(Pat::False, |b, bindings| {
                        self.arm(
                            b,
                            scope,
                            &before,
                            &mut after,
                            bindings,
                            &[],
                            target,
                            depth - 1,
                        )
                    });
                });
                scope.restore(&after);
                result
            }
            P::MatchOption => {
                let var =
                    self.var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Option(_))));
                let (subject, option_ty) = match var {
                    Some(pair) => pair,
                    None => {
                        let option_ty = Type::option(self.random_data_type(1));
                        (self.operand(b, scope, &option_ty, depth), option_ty)
                    }
                };
                let Type::Data(DataType::Option(inner)) = option_ty else {
                    unreachable!("the subject is an option")
                };
                let mut binds = Vec::new();
                let inner = if self.coin() {
                    Self::bind(&mut binds, &Type::Data(*inner))
                } else {
                    Pat::Wildcard
                };
                let before = scope.snapshot();
                let mut after = before.clone();
                let result = b.match_(subject, |m| {
                    m.arm(Pat::some(inner), |b, bindings| {
                        self.arm(
                            b,
                            scope,
                            &before,
                            &mut after,
                            bindings,
                            &binds,
                            target,
                            depth - 1,
                        )
                    });
                    m.arm(Pat::None, |b, bindings| {
                        self.arm(
                            b,
                            scope,
                            &before,
                            &mut after,
                            bindings,
                            &[],
                            target,
                            depth - 1,
                        )
                    });
                });
                scope.restore(&after);
                result
            }
            P::MatchEnum => {
                let enums = self.enums.clone();
                let var = self.var_in_scope(b, scope, |ty| enums.contains(ty));
                let subject_ty = match &var {
                    Some((_, typ)) => typ.clone(),
                    None => self.u.choose(&enums).unwrap().clone(),
                };
                let subject = match var {
                    Some((var, _)) => var,
                    None => self.operand(b, scope, &subject_ty, depth),
                };
                let Some(ResolvedType::Enum { variants, .. }) = resolve(b.registry(), &subject_ty)
                else {
                    unreachable!("an enum of the module resolves to its declaration")
                };
                let arm_plan = self.enum_match_arms(variants);
                let before = scope.snapshot();
                let mut after = before.clone();
                let result = b.match_(subject, |m| {
                    for (variant, bound) in arm_plan {
                        let mut binds = Vec::new();
                        let fields = bound
                            .into_iter()
                            .map(|(field, typ)| (field, Self::bind(&mut binds, &typ)))
                            .collect();
                        m.arm(Pat::Variant(variant, fields), |b, bindings| {
                            self.arm(
                                b,
                                scope,
                                &before,
                                &mut after,
                                bindings,
                                &binds,
                                target,
                                depth - 1,
                            )
                        });
                    }
                });
                scope.restore(&after);
                result
            }
            P::Eq => {
                let ty = match self.index(4) {
                    0 => Type::INT,
                    1 => Type::STRING,
                    2 => Type::BOOL,
                    _ => Type::FLOAT,
                };
                let left = self.operand(b, scope, &ty, depth);
                let right = self.operand(b, scope, &ty, depth);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.operand(b, scope, &ty, depth);
                let right = self.operand(b, scope, &ty, depth);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.operand(b, scope, &ty, depth);
                let right = self.operand(b, scope, &ty, depth);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.operand(b, scope, &Type::BOOL, depth);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.operand(b, scope, &Type::STRING, depth))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.operand(b, scope, &Type::INT, depth);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.operand(b, scope, &Type::FLOAT, depth);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.operand(b, scope, &Type::INT, depth);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(1));
                        self.operand(b, scope, &option_ty, depth)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(1));
                        self.operand(b, scope, &option_ty, depth)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.operand(b, scope, &Type::STRING, depth);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(1));
                        self.operand(b, scope, &array_ty, depth)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(1));
                        self.operand(b, scope, &array_ty, depth)
                    });
                b.array_length(operand)
            }
            P::ArrayFromRange => {
                // Literal bounds only, generated arithmetic can produce huge
                // Int values, which would make evaluation iterate forever.
                let start = b.int(self.zigzag(3));
                scope.bound(start);
                let end = b.int(self.zigzag(3));
                scope.bound(end);
                b.array_from_range(start, end)
            }
            P::TupleIndex => {
                let Type::Data(target_data) = target else {
                    unreachable!("a tuple holds data, so it is indexed only for data")
                };
                let var = self.var_in_scope(b, scope,
                    |ty| matches!(ty, Type::Data(DataType::Tuple(elements)) if elements.contains(target_data)),
                );
                match var {
                    Some((tuple, tuple_ty)) => {
                        let Type::Data(DataType::Tuple(elements)) = tuple_ty else {
                            unreachable!("the variable has a tuple type")
                        };
                        let indices: Vec<usize> = (0..elements.len())
                            .filter(|&index| elements[index] == *target_data)
                            .collect();
                        b.tuple_index(tuple, *self.u.choose(&indices).unwrap())
                    }
                    None => {
                        let arity = self.count(1..=3);
                        let index = self.index(arity);
                        let elements = (0..arity)
                            .map(|i| {
                                if i == index {
                                    target_data.clone()
                                } else {
                                    self.random_data_type(1)
                                }
                            })
                            .collect();
                        let tuple =
                            self.operand(b, scope, &Type::Data(DataType::Tuple(elements)), depth);
                        b.tuple_index(tuple, index)
                    }
                }
            }
            P::MatchPattern => {
                let var = self.var_in_scope(b, scope, |ty| {
                    matches!(
                        ty,
                        Type::Data(
                            DataType::Bool
                                | DataType::Option(_)
                                | DataType::Tuple(_)
                                | DataType::Named { .. }
                        )
                    )
                });
                let (subject, subject_ty) = match var {
                    Some(pair) => pair,
                    None => {
                        let elements = (0..self.count(1..=3))
                            .map(|_| self.random_data_type(1))
                            .collect();
                        let tuple_ty = Type::Data(DataType::Tuple(elements));
                        (self.operand(b, scope, &tuple_ty, depth), tuple_ty)
                    }
                };
                let mut pats: Vec<(Pat, Vec<(String, Type)>)> = (0..self.count(1..=3))
                    .map(|_| {
                        let mut binds = Vec::new();
                        let pat = self.pattern(b, &subject_ty, 2, &mut binds);
                        (pat, binds)
                    })
                    .collect();
                // A last arm that matches every value makes the match
                // exhaustive, whether or not the arms before it are.
                let mut binds = Vec::new();
                let pat = self.pattern(b, &subject_ty, 0, &mut binds);
                pats.push((pat, binds));
                let before = scope.snapshot();
                let mut after = before.clone();
                let result = b.match_(subject, |m| {
                    for (pat, binds) in pats {
                        m.arm(pat, |b, bindings| {
                            self.arm(
                                b,
                                scope,
                                &before,
                                &mut after,
                                bindings,
                                &binds,
                                target,
                                depth - 1,
                            )
                        });
                    }
                });
                scope.restore(&after);
                result
            }
            P::Neg => {
                let operand = self.operand(b, scope, target, depth);
                b.neg(operand)
            }
            P::Add => {
                let left = self.operand(b, scope, target, depth);
                let right = self.operand(b, scope, target, depth);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.operand(b, scope, target, depth);
                let right = self.operand(b, scope, target, depth);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.operand(b, scope, target, depth);
                let right = self.operand(b, scope, target, depth);
                b.mul(left, right)
            }
            P::HtmlText => b.text(self.u.choose(TEXTS).unwrap()),
            P::HtmlEscape => {
                let operand = self.operand(b, scope, &Type::STRING, depth);
                b.escape(operand)
            }
            P::HtmlElement => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                // Attribute names are unique within an element, so each
                // candidate is included at most once.
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.operand(b, scope, &Type::STRING, depth);
                        attributes.push(b.attr_value(name, value));
                    }
                }
                if self.coin() {
                    let present = self.operand(b, scope, &Type::BOOL, depth);
                    attributes.push(b.attr_presence("disabled", present));
                }
                let children = if void {
                    // The element reads its children, so they never come
                    // into scope.
                    b.html_concat(Vec::new())
                } else {
                    self.operand(b, scope, &Type::Html, depth)
                };
                b.element(tag, attributes, children)
            }
            P::HtmlConcat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.operand(b, scope, &Type::Html, depth))
                    .collect();
                b.html_concat(parts)
            }
            P::HtmlForArray => {
                let var =
                    self.var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Array(_))));
                let array = match var {
                    Some((array, _)) => array,
                    None => {
                        let array_ty = Type::array(self.random_data_type(1));
                        self.operand(b, scope, &array_ty, depth)
                    }
                };
                b.html_for(array, |b, var| {
                    self.loop_body(b, scope, Some(var), depth - 1)
                })
            }
            P::HtmlForRange => {
                // Literal bounds only, as for an array from a range.
                let start = b.int(self.zigzag(3));
                scope.bound(start);
                let end = b.int(self.zigzag(3));
                scope.bound(end);
                let array = b.array_from_range(start, end);
                scope.bound(array);
                if self.coin() {
                    b.html_for(array, |b, var| {
                        self.loop_body(b, scope, Some(var), depth - 1)
                    })
                } else {
                    b.html_for_unbound(array, |b| self.loop_body(b, scope, None, depth - 1))
                }
            }
            P::Call => {
                let callees: Vec<_> = Self::callees(b, target).collect();
                let callee = *self.u.choose(&callees).unwrap();
                let args: Vec<_> = callee
                    .parameters
                    .iter()
                    .map(|parameter| self.operand(b, scope, &parameter.binder.typ, depth))
                    .collect();
                b.call(callee.function.name.as_str(), args)
            }
        };
        scope.bound(value)
    }

    /// A leaf of the target type, its parts nesting at most `depth`
    /// values deep (or deeper if the target type forces it).
    fn leaf(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
    ) -> ValueId {
        let Type::Data(data) = target else {
            let value = match self.index(3) {
                0 => b.html_concat(Vec::new()),
                1 => b.text(self.u.choose(TEXTS).unwrap()),
                _ => {
                    let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                    let children = if void || depth == 0 {
                        b.html_concat(Vec::new())
                    } else {
                        self.value(b, scope, &Type::Html, depth - 1)
                    };
                    b.element(tag, Vec::new(), children)
                }
            };
            return scope.bound(value);
        };
        let value = match data {
            DataType::String => b.str(self.u.choose(STRING_LITERALS).unwrap()),
            DataType::Int => {
                if self.count(0..=7) == 0 {
                    b.int(
                        *self
                            .u
                            .choose(&[i32::MIN, i32::MIN + 1, i32::MAX - 1, i32::MAX])
                            .unwrap(),
                    )
                } else {
                    b.int(self.zigzag(100))
                }
            }
            DataType::Bool => b.bool(self.coin()),
            DataType::Float => b.float(*self.u.choose(FLOATS).unwrap()),
            DataType::Array(inner) => {
                let len = if depth == 0 { 0 } else { self.count(0..=3) };
                let elements: Vec<_> = (0..len)
                    .map(|_| {
                        self.value(
                            b,
                            scope,
                            &Type::Data((**inner).clone()),
                            depth.saturating_sub(1),
                        )
                    })
                    .collect();
                if elements.is_empty() {
                    b.empty_array(Type::Data(inner.as_ref().clone()))
                } else {
                    b.array(elements)
                }
            }
            DataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.value(
                        b,
                        scope,
                        &Type::Data((**inner).clone()),
                        depth.saturating_sub(1),
                    );
                    b.some(value)
                } else {
                    b.none(Type::Data(inner.as_ref().clone()))
                }
            }
            DataType::Named { .. } => match b.registry().resolve(data) {
                Some(ResolvedType::Record { fields, .. }) => {
                    let fields = self.shuffled(fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        let ty = Type::Data(field.typ.clone());
                        values.push((
                            field.name.as_str(),
                            self.value(b, scope, &ty, depth.saturating_sub(1)),
                        ));
                    }
                    b.record(target.clone(), values)
                }
                Some(ResolvedType::Enum { variants, .. }) => {
                    let variant = self.u.choose(variants).unwrap();
                    let fields = self.shuffled(variant.fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        let ty = Type::Data(field.typ.clone());
                        values.push((
                            field.name.as_str(),
                            self.value(b, scope, &ty, depth.saturating_sub(1)),
                        ));
                    }
                    b.enum_variant_with_fields(target.clone(), variant.name.as_str(), values)
                }
                _ => unreachable!("named type must be a generated record or enum"),
            },
            DataType::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|element| {
                        self.value(
                            b,
                            scope,
                            &Type::Data(element.clone()),
                            depth.saturating_sub(1),
                        )
                    })
                    .collect();
                b.tuple(values)
            }
        };
        scope.bound(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::flat_module::{FlatBlock, FlatOp};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use expect_test::{Expect, expect};
    use rand::{RngExt, SeedableRng, rngs::SmallRng};
    use std::collections::{BTreeMap, HashSet};

    fn check(seed: u64, len: usize, expected: Expect) {
        let mut rng = SmallRng::seed_from_u64(seed);
        let bytes: Vec<u8> = (0..len).map(|_| rng.random()).collect();
        let (module, registry) = random_module(&mut Unstructured::new(&bytes));
        expected.assert_eq(&format!("{registry}\n{module}"));
    }

    /// Mark the names read on the way to the result of `block`, walking the
    /// bindings backwards as dead binding elimination does. `live` is
    /// shared with nested blocks, since names are unique.
    fn mark_live(block: &FlatBlock, live: &mut HashSet<ValueId>) {
        live.insert(block.result);
        for binding in block.bindings.iter().rev() {
            if !live.contains(&binding.binder.var) {
                continue;
            }
            match &binding.op {
                FlatOp::Match(match_) => {
                    for arm in &match_.arms {
                        mark_live(&arm.body, live);
                    }
                }
                FlatOp::HtmlFor(for_) => mark_live(&for_.body, live),
                _ => {}
            }
            binding.op.for_each_operand(&mut |operand| {
                live.insert(operand);
            });
        }
    }

    /// Mark the names whose values depend on the parameters, walking the
    /// bindings forwards from the parameters. A binding depends on them
    /// when an operand does, a pattern binding or loop variable when the
    /// subject or array does, and a match or loop also when the result of
    /// a nested block does. Gives whether the result of `block` depends on
    /// them. `dynamic` is shared with nested blocks, since names are unique.
    fn mark_dynamic(block: &FlatBlock, dynamic: &mut HashSet<ValueId>) -> bool {
        for binding in &block.bindings {
            let mut depends = false;
            binding.op.for_each_operand(&mut |operand| {
                depends |= dynamic.contains(&operand);
            });
            match &binding.op {
                FlatOp::Match(match_) => {
                    let subject = depends;
                    for arm in &match_.arms {
                        if subject {
                            arm.pattern.for_each_binding(&mut |binder| {
                                dynamic.insert(binder.var);
                            });
                        }
                        depends |= mark_dynamic(&arm.body, dynamic);
                    }
                }
                FlatOp::HtmlFor(for_) => {
                    if depends && let Some(var) = &for_.var {
                        dynamic.insert(var.var);
                    }
                    depends |= mark_dynamic(&for_.body, dynamic);
                }
                _ => {}
            }
            if depends {
                dynamic.insert(binding.binder.var);
            }
        }
        dynamic.contains(&block.result)
    }

    /// Count the ops of `block` by kind: every op, the ops that are live,
    /// and the live ops that depend on the parameters.
    fn count_ops(
        block: &FlatBlock,
        live: &HashSet<ValueId>,
        dynamic: &HashSet<ValueId>,
        counts: &mut BTreeMap<String, [usize; 3]>,
    ) {
        for binding in &block.bindings {
            let debug = format!("{:?}", binding.op);
            let kind = debug
                .trim_start_matches("Data(")
                .split('(')
                .next()
                .unwrap()
                .trim_end_matches(')');
            let column = counts.entry(kind.to_string()).or_default();
            column[0] += 1;
            if live.contains(&binding.binder.var) {
                column[1] += 1;
                if dynamic.contains(&binding.binder.var) {
                    column[2] += 1;
                }
            }
            match &binding.op {
                FlatOp::Match(match_) => {
                    for arm in &match_.arms {
                        count_ops(&arm.body, live, dynamic, counts);
                    }
                }
                FlatOp::HtmlFor(for_) => count_ops(&for_.body, live, dynamic, counts),
                _ => {}
            }
        }
    }

    /// Report how often each kind of op occurs in a generated module, what
    /// share of those is live, that is read on the way to the result, what
    /// share is live and depends on the parameters, and how many
    /// entrypoints hit the limits of the evaluator. Every input is as long
    /// as the longest arbtest gives.
    #[test]
    fn should_report_generator_stats() {
        const SAMPLES: usize = 100;
        const INPUT_LEN: usize = 65_536;
        let mut rng = SmallRng::seed_from_u64(0);
        let mut counts = BTreeMap::new();
        let (mut entrypoints, mut diverged, mut too_long) = (0, 0, 0);
        let (mut consumed, mut most_consumed, mut exhausted) = (0, 0, 0);
        for _ in 0..SAMPLES {
            let bytes: Vec<u8> = (0..INPUT_LEN).map(|_| rng.random()).collect();
            let mut u = Unstructured::new(&bytes);
            let (module, registry) = random_module(&mut u);
            let used = INPUT_LEN - u.len();
            consumed += used;
            most_consumed = most_consumed.max(used);
            exhausted += u.is_empty() as usize;
            for (function, args) in random_entrypoint_args(&module, &mut rng, &registry) {
                entrypoints += 1;
                let result = evaluate_entrypoint(&module, &function, args);
                diverged += matches!(result, Err(EvalError::RecursionLimit { .. })) as usize;
                too_long += matches!(result, Err(EvalError::ArrayLengthLimit { .. })) as usize;
            }
            let mut live = HashSet::new();
            let mut dynamic = HashSet::new();
            for function in &module.functions {
                mark_live(&function.body, &mut live);
                dynamic.extend(function.parameters.iter().map(|param| param.binder.var));
                mark_dynamic(&function.body, &mut dynamic);
            }
            for function in &module.functions {
                count_ops(&function.body, &live, &dynamic, &mut counts);
            }
        }
        let mut report = format!(
            "{} of {INPUT_LEN} input bytes consumed per module on average, at most {most_consumed}\n\
             {exhausted} of {SAMPLES} modules exhaust the input\n\
             {diverged} of {entrypoints} entrypoints hit the recursion limit\n\
             {too_long} of {entrypoints} entrypoints hit the array length limit\n\n\
             {:<20}{:>10}{:>8}{:>8}\n",
            consumed / SAMPLES,
            "op",
            "ops/module",
            "% live",
            "% dyn"
        );
        for (kind, [generated, live, dynamic]) in counts {
            let per_module = generated as f64 / SAMPLES as f64;
            let live = 100.0 * live as f64 / generated as f64;
            let dynamic = 100.0 * dynamic as f64 / generated as f64;
            report.push_str(&format!(
                "{kind:<20}{per_module:>10.1}{live:>8.1}{dynamic:>8.1}\n"
            ));
        }
        expect![[r#"
            3696 of 65536 input bytes consumed per module on average, at most 27553
            0 of 100 modules exhaust the input
            4 of 210 entrypoints hit the recursion limit
            0 of 210 entrypoints hit the array length limit

            op                  ops/module  % live   % dyn
            Array                    108.1    97.8    10.8
            ArrayFromRange             1.3    98.4     0.0
            ArrayIsEmpty               3.0    97.7    27.7
            ArrayLength                4.0    97.5    24.7
            BoolLiteral               31.9    98.1     0.0
            BoolNegation               2.7    97.8    25.4
            Call                       2.6    98.1    40.5
            Enum                      82.7    98.0    10.1
            Equals                     3.1    97.7    32.2
            FieldAccess                1.9    96.4    32.3
            FloatLiteral              50.3    98.0     0.0
            FloatToInt                 3.7    97.3    17.6
            HtmlConcat                 5.7    92.6     7.9
            HtmlElement                4.4    92.9    15.4
            HtmlEscape                 1.6    93.3    23.6
            HtmlFor                    2.1    97.7    29.6
            HtmlText                   4.3    92.3     0.0
            IntLiteral                50.1    98.1     0.0
            IntToFloat                 4.5    98.7    19.6
            IntToString                4.3    97.4    14.7
            LessThan                   3.0    97.7    38.1
            LessThanOrEqual            2.9    98.3    39.4
            Match                    108.1    98.6    49.0
            NumericAdd                 8.8    96.4    29.5
            NumericMultiply            8.3    96.3    30.0
            NumericNegation            8.1    97.3    19.3
            NumericSubtract            7.6    98.6    28.7
            OptionIsNone               2.6    97.3    23.7
            OptionIsSome               3.0    97.0    26.0
            OptionNone                85.5    98.1     0.0
            OptionSome                68.7    97.8    13.4
            Record                    62.1    97.4    15.9
            StringConcat               4.0    96.8    19.4
            StringIsEmpty              3.1    97.7    19.0
            StringLiteral             46.2    97.6     0.0
            Tuple                    281.6    97.9    21.3
            TupleIndex               102.7    97.0    31.2
        "#]]
        .assert_eq(&report);
    }

    #[test]
    fn should_generate_a_minimal_module_from_no_input() {
        check(
            0,
            0,
            expect![[r#"

                entrypoint fn V0@f0() -> Html {
                  let v0: Html = concat()
                  v0
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_a_module_from_seed_16() {
        check(
            16,
            128,
            expect![[r#"
                enum E0 {
                  W0,
                }

                enum E1 {
                  W0 { f0: (Array[E1], Bool, Array[Int]), f1: (Float, Int, R0) },
                }

                record R0 {
                  f0: Array[Array[Bool]],
                  f1: R1,
                }

                record R1 {}

                entrypoint fn V0@f0(p0@v0: (Option[E0], (E1, Int, Bool)), p1@v1: E1) -> Html {
                  let v2: E0 = W0
                  let v37: Html = match v2 {
                    E0::W0 => {
                      let v3: E0 = W0
                      let v32: Html = match v3 {
                        E0::W0 => {
                          let v4: Array[E1] = [v1]
                          let v5: Option[Array[E1]] = Some(v4)
                          let v6: String = "`${x}`"
                          let v7: Bool = true
                          let v8: Int = -99
                          let v9: Int = 70
                          let v10: Array[Int] = [v8, v9]
                          let v11: (Array[E1], Bool, Array[Int]) = (v4, v7, v10)
                          let v12: Float = 99.75
                          let v13: R1 = {}
                          let v14: Array[Bool] = []
                          let v15: Array[Bool] = []
                          let v16: Array[Array[Bool]] = [v14, v15]
                          let v17: R0 = {f1: v13, f0: v16}
                          let v18: (Float, Int, R0) = (v12, v8, v17)
                          let v19: E1 = W0 {f0: v11, f1: v18}
                          let v20: (Option[Array[E1]], String, E1) = (v5, v6, v19)
                          let v21: Option[Array[E1]] = v20.0
                          let v31: Html = match v21 {
                            Some(v22: Array[E1]) => {
                              let v23: Html = concat()
                              v23
                            }
                            None => {
                              let v24: Option[Int] = None
                              let v27: Bool = match v24 {
                                Some(_) => {
                                  let v25: Bool = false
                                  v25
                                }
                                None => {
                                  let v26: Bool = false
                                  v26
                                }
                              }
                              let v30: Html = match v27 {
                                true => {
                                  let v28: Html = concat()
                                  v28
                                }
                                false => {
                                  let v29: Html = concat()
                                  v29
                                }
                              }
                              v30
                            }
                          }
                          v31
                        }
                      }
                      let v33: Html = concat()
                      let v34: Html = concat()
                      let v35: Html = concat()
                      let v36: Html = concat(v32, v33, v34, v35)
                      v36
                    }
                  }
                  v37
                }
                entrypoint fn V1@f1(p0@v38: Array[E0], p1@v39: R1, p2@v40: Array[(Bool, E1)]) -> Html {
                  let v41: Html = concat()
                  v41
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_a_module_from_seed_24() {
        check(
            24,
            128,
            expect![[r#"
                enum E0 {
                  W0 { f0: Float },
                  W1 { f0: Bool },
                }

                enum E1 {
                  W0 { f0: Option[Option[Float]], f1: Int },
                  W1,
                }

                entrypoint fn V0@f0(p0@v0: Bool, p1@v1: Int) -> Html {
                  let v2: Html = text("a\"b'c")
                  v2
                }
                entrypoint fn V1@f1() -> Html {
                  let v3: Html = text("&amp;")
                  v3
                }
            "#]],
        );
    }
}
