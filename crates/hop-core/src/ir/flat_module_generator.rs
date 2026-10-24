use crate::hop::typing::{
    DataType, RecordField, ResolvedType, Ty, Type, TypeRegistry, TypeRegistryBuilder,
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
const MAX_BLOCK_DEPTH: usize = 6;

/// How deep the ops of an expression nest, an op reading the result of
/// another op as an operand.
const MAX_EXPRESSION_DEPTH: usize = 30;

/// How deep the values of a literal nest before they start to converge.
const MAX_LITERAL_DEPTH: usize = 5;

/// One block in this many binds a value nothing asked for.
const DEAD_BINDING_ODDS: usize = 64;

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
    let mut builder = FlatModuleBuilder::with_registry(types.build());

    // Declare every function before defining any, so that a body can call
    // a function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let params: Vec<(String, Ty)> = (0..g.count(0..=8))
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
                let params = (0..g.count(0..=8))
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

    /// Nine times in ten, pick a name in scope whose type fits, when there
    /// is one, with its type.
    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&Type) -> bool,
    ) -> Option<(ValueId, Type)> {
        let types = b.value_types();
        if scope.names_where(&types, &fits).next().is_none() || self.index(10) == 0 {
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
    /// reads before it. The blocks it nests nest at most `depth` deep.
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
            self.op(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
        }
        self.op(b, scope, target, depth, MAX_EXPRESSION_DEPTH)
    }

    /// A name in scope of type `typ` that may be read, or a literal whose
    /// parts nest at most `depth` values deep (or deeper if the type
    /// forces it). An Html value counts as used.
    fn leaf(
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
            None => {
                let literal = match typ {
                    Type::Html => match self.index(3) {
                        0 => b.html_concat(Vec::new()),
                        1 => b.text(self.u.choose(TEXTS).unwrap()),
                        _ => {
                            let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                            let children = if void || depth == 0 {
                                b.html_concat(Vec::new())
                            } else {
                                self.leaf(b, scope, &Type::Html, depth - 1)
                            };
                            b.element(tag, Vec::new(), children)
                        }
                    },
                    Type::Data(DataType::String) => b.str(self.u.choose(STRING_LITERALS).unwrap()),
                    Type::Data(DataType::Int) => {
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
                    Type::Data(DataType::Bool) => b.bool(self.coin()),
                    Type::Data(DataType::Float) => b.float(*self.u.choose(FLOATS).unwrap()),
                    Type::Data(DataType::Array(inner)) => {
                        let len = if depth == 0 { 0 } else { self.count(0..=3) };
                        let elements: Vec<_> = (0..len)
                            .map(|_| {
                                self.leaf(
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
                    Type::Data(DataType::Option(inner)) => {
                        if depth > 0 && self.coin() {
                            let value = self.leaf(
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
                    Type::Data(data @ DataType::Named { .. }) => match b.registry().resolve(data) {
                        Some(ResolvedType::Record { fields, .. }) => {
                            let fields = self.shuffled(fields.iter().collect());
                            let mut values = Vec::new();
                            for field in fields {
                                let ty = Type::Data(field.typ.clone());
                                values.push((
                                    field.name.as_str(),
                                    self.leaf(b, scope, &ty, depth.saturating_sub(1)),
                                ));
                            }
                            b.record(typ.clone(), values)
                        }
                        Some(ResolvedType::Enum { variants, .. }) => {
                            let variant = self.u.choose(variants).unwrap();
                            let fields = self.shuffled(variant.fields.iter().collect());
                            let mut values = Vec::new();
                            for field in fields {
                                let ty = Type::Data(field.typ.clone());
                                values.push((
                                    field.name.as_str(),
                                    self.leaf(b, scope, &ty, depth.saturating_sub(1)),
                                ));
                            }
                            b.enum_variant_with_fields(typ.clone(), variant.name.as_str(), values)
                        }
                        _ => unreachable!("named type must be a generated record or enum"),
                    },
                    Type::Data(DataType::Tuple(elements)) => {
                        let values: Vec<_> = elements
                            .iter()
                            .map(|element| {
                                self.leaf(
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
                scope.bound(literal)
            }
        };
        if *typ == Type::Html {
            scope.used(value);
        }
        value
    }

    /// A pattern that binds a name of type `typ`, added to `binds` under a
    /// name numbered by its position among the names bound so far.
    fn bind(binds: &mut Vec<(String, Type)>, typ: &Type) -> Pat {
        let name = format!("x{}", binds.len());
        binds.push((name.clone(), typ.clone()));
        Pat::Bind(name)
    }

    /// One pattern per constructor of `typ` with wildcard parts, which
    /// together match every value.
    fn constructors(b: &FlatBlockBuilder<'_>, typ: &Type) -> Vec<Pat> {
        match typ {
            Type::Data(DataType::Bool) => vec![Pat::True, Pat::False],
            Type::Data(DataType::Option(_)) => vec![Pat::some(Pat::Wildcard), Pat::None],
            Type::Data(DataType::Tuple(elements)) => {
                vec![Pat::Tuple(elements.iter().map(|_| Pat::Wildcard).collect())]
            }
            Type::Data(DataType::Named { .. }) => match resolve(b.registry(), typ) {
                Some(ResolvedType::Record { .. }) => vec![Pat::Record(Vec::new())],
                Some(ResolvedType::Enum { variants, .. }) => variants
                    .iter()
                    .map(|variant| Pat::Variant(variant.name.as_str().to_string(), Vec::new()))
                    .collect(),
                _ => unreachable!("a named type is a declared record or enum"),
            },
            _ => unreachable!("a match subject is a bool, an option, a tuple or a named type"),
        }
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

    /// A value of the target type for an op or a block to read: two times
    /// in three an op of its own bound in the block `b` builds while the
    /// expression may still nest, with its operands taken from what is in
    /// scope, else a name in scope or a literal. The blocks it nests nest
    /// at most `depth` deep and the ops at most `nesting` deep. An Html
    /// value counts as used.
    fn op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if nesting == 0 || self.index(4) == 0 {
            return self.leaf(b, scope, target, MAX_LITERAL_DEPTH);
        }
        enum P {
            FieldAccess,
            MatchPattern,
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
        let mut productions: Vec<P> = Vec::new();
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
            productions.push(P::MatchPattern);
            if *target == Type::Html {
                productions.extend([P::HtmlForArray, P::HtmlForRange]);
            }
        }
        let value = match self.u.choose(&productions).unwrap() {
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
                        let object = self.op(b, scope, &record, depth, nesting - 1);
                        b.field_access(object, &field)
                    }
                }
            }
            P::Eq => {
                let ty = match self.index(4) {
                    0 => Type::INT,
                    1 => Type::STRING,
                    2 => Type::BOOL,
                    _ => Type::FLOAT,
                };
                let left = self.op(b, scope, &ty, depth, nesting - 1);
                let right = self.op(b, scope, &ty, depth, nesting - 1);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.op(b, scope, &ty, depth, nesting - 1);
                let right = self.op(b, scope, &ty, depth, nesting - 1);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.op(b, scope, &ty, depth, nesting - 1);
                let right = self.op(b, scope, &ty, depth, nesting - 1);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.op(b, scope, &Type::BOOL, depth, nesting - 1);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.op(b, scope, &Type::STRING, depth, nesting - 1))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.op(b, scope, &Type::INT, depth, nesting - 1);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.op(b, scope, &Type::FLOAT, depth, nesting - 1);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.op(b, scope, &Type::INT, depth, nesting - 1);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(1));
                        self.op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(1));
                        self.op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.op(b, scope, &Type::STRING, depth, nesting - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(1));
                        self.op(b, scope, &array_ty, depth, nesting - 1)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(1));
                        self.op(b, scope, &array_ty, depth, nesting - 1)
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
                        let tuple = self.op(
                            b,
                            scope,
                            &Type::Data(DataType::Tuple(elements)),
                            depth,
                            nesting - 1,
                        );
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
                        let named = self.declared_names.len();
                        let subject_ty: Type = match self.index(3 + named) {
                            0 => Ty::Bool,
                            1 => Ty::option(self.random_type_where(1, NamedTypes::Any, false)),
                            2 => Ty::tuple(
                                (0..self.count(1..=3))
                                    .map(|_| self.random_type_where(1, NamedTypes::Any, false)),
                            ),
                            i => Ty::named(&self.declared_names[i - 3]),
                        }
                        .into();
                        (
                            self.op(b, scope, &subject_ty, depth, nesting - 1),
                            subject_ty,
                        )
                    }
                };
                let mut pats: Vec<(Pat, Vec<(String, Type)>)> = (0..self.count(0..=2))
                    .map(|_| {
                        let mut binds = Vec::new();
                        let pat = self.pattern(b, &subject_ty, 2, &mut binds);
                        (pat, binds)
                    })
                    .collect();
                // The last arms make the match exhaustive, whether or not
                // the arms before them are: one arm per constructor of the
                // subject type, or one arm that matches every value.
                if self.coin() {
                    let constructors = Self::constructors(b, &subject_ty);
                    pats.extend(constructors.into_iter().map(|pat| (pat, Vec::new())));
                } else {
                    let mut binds = Vec::new();
                    let pat = self.pattern(b, &subject_ty, 0, &mut binds);
                    pats.push((pat, binds));
                }
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
                let operand = self.op(b, scope, target, depth, nesting - 1);
                b.neg(operand)
            }
            P::Add => {
                let left = self.op(b, scope, target, depth, nesting - 1);
                let right = self.op(b, scope, target, depth, nesting - 1);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.op(b, scope, target, depth, nesting - 1);
                let right = self.op(b, scope, target, depth, nesting - 1);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.op(b, scope, target, depth, nesting - 1);
                let right = self.op(b, scope, target, depth, nesting - 1);
                b.mul(left, right)
            }
            P::HtmlText => b.text(self.u.choose(TEXTS).unwrap()),
            P::HtmlEscape => {
                let operand = self.op(b, scope, &Type::STRING, depth, nesting - 1);
                b.escape(operand)
            }
            P::HtmlElement => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                // Attribute names are unique within an element, so each
                // candidate is included at most once.
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.op(b, scope, &Type::STRING, depth, nesting - 1);
                        attributes.push(b.attr_value(name, value));
                    }
                }
                if self.coin() {
                    let present = self.op(b, scope, &Type::BOOL, depth, nesting - 1);
                    attributes.push(b.attr_presence("disabled", present));
                }
                let children = if void {
                    // The element reads its children, so they never come
                    // into scope.
                    b.html_concat(Vec::new())
                } else {
                    self.op(b, scope, &Type::Html, depth, nesting - 1)
                };
                b.element(tag, attributes, children)
            }
            P::HtmlConcat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.op(b, scope, &Type::Html, depth, nesting - 1))
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
                        self.op(b, scope, &array_ty, depth, nesting - 1)
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
                    .map(|parameter| self.op(b, scope, &parameter.binder.typ, depth, nesting - 1))
                    .collect();
                b.call(callee.function.name.as_str(), args)
            }
        };
        scope.bound(value);
        if *target == Type::Html {
            scope.used(value);
        }
        value
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

    /// Count the references to each name in `block` and the blocks it
    /// nests, as an operand or as the result of a block, and add the Html
    /// names it binds to `html`.
    fn count_html_references(
        block: &FlatBlock,
        references: &mut HashMap<ValueId, usize>,
        html: &mut HashSet<ValueId>,
    ) {
        for binding in &block.bindings {
            if binding.binder.typ == Type::Html {
                html.insert(binding.binder.var);
            }
            binding.op.for_each_operand(&mut |operand| {
                *references.entry(operand).or_default() += 1;
            });
            match &binding.op {
                FlatOp::Match(match_) => {
                    for arm in &match_.arms {
                        count_html_references(&arm.body, references, html);
                    }
                }
                FlatOp::HtmlFor(for_) => count_html_references(&for_.body, references, html),
                _ => {}
            }
        }
        *references.entry(block.result).or_default() += 1;
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
        let (mut html_bindings, mut shared_html) = (0, 0);
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
            let mut references = HashMap::new();
            let mut html = HashSet::new();
            for function in &module.functions {
                count_html_references(&function.body, &mut references, &mut html);
            }
            html_bindings += html.len();
            shared_html += html
                .iter()
                .filter(|name| references.get(name).copied().unwrap_or(0) > 1)
                .count();
        }
        let mut report = format!(
            "{} of {INPUT_LEN} input bytes consumed per module on average, at most {most_consumed}\n\
             {exhausted} of {SAMPLES} modules exhaust the input\n\
             {diverged} of {entrypoints} entrypoints hit the recursion limit\n\
             {too_long} of {entrypoints} entrypoints hit the array length limit\n\
             {shared_html} of {html_bindings} Html bindings have more than one reference\n\n\
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
            3164 of 65536 input bytes consumed per module on average, at most 37009
            0 of 100 modules exhaust the input
            1 of 194 entrypoints hit the recursion limit
            0 of 194 entrypoints hit the array length limit
            0 of 1592 Html bindings have more than one reference

            op                  ops/module  % live   % dyn
            Array                    112.6    94.2    10.1
            ArrayFromRange             1.3    90.2     0.0
            ArrayIsEmpty               0.2    94.7    68.4
            ArrayLength                7.8    98.8    50.0
            BoolLiteral               34.0    95.3     0.0
            BoolNegation               0.2    94.1    41.2
            Call                       3.4    97.0    94.3
            Enum                      54.4    94.2    13.8
            Equals                     0.2   100.0    84.2
            FieldAccess                0.7    90.5    68.9
            FloatLiteral              30.4    95.9     0.0
            FloatToInt                 7.3    99.0    81.1
            HtmlConcat                 3.9    93.3    25.1
            HtmlElement                3.1    91.9    46.9
            HtmlEscape                 1.9    90.1    58.6
            HtmlFor                    2.3    95.6    51.3
            HtmlText                   3.3    93.1     0.0
            IntLiteral                32.6    95.9     0.0
            IntToFloat                11.3    98.0    75.3
            IntToString                2.8    93.5    63.4
            LessThan                   0.2    89.5    68.4
            LessThanOrEqual            0.2   100.0    68.2
            Match                     53.0    95.7    86.4
            NumericAdd                20.3    98.8    91.8
            NumericMultiply           19.5    97.9    90.5
            NumericNegation           18.9    98.4    80.6
            NumericSubtract           18.9    98.1    90.8
            OptionIsNone               0.3    86.2    48.3
            OptionIsSome               0.2    94.1    64.7
            OptionNone                71.0    94.6     0.0
            OptionSome                29.0    94.7    21.8
            Record                    62.8    92.3    17.0
            StringConcat               2.8    92.9    65.0
            StringIsEmpty              0.2    94.1    70.6
            StringLiteral             36.4    95.5     0.0
            Tuple                    420.4    95.0    46.6
            TupleIndex               201.7    94.1    73.0
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
                  let v2: String = "`${x}`"
                  let v3: Html = escape(v2)
                  v3
                }
                entrypoint fn V1@f1(p0@v4: Array[E0], p1@v5: R1, p2@v6: Array[(Bool, E1)]) -> Html {
                  let v7: Int = -1
                  let v8: Int = -3
                  let v9: Array[Int] = v7..=v8
                  let v18: Html = for v10: Int in v9 {
                    let v11: Int = 1
                    let v12: Int = -1
                    let v13: Array[Int] = v11..=v12
                    let v17: Html = for v14: Int in v13 {
                      let v15: Html = concat()
                      let v16: Html = element("input", {}, v15)
                      v16
                    }
                    v17
                  }
                  v18
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
                  let v2: Html = concat()
                  v2
                }
                entrypoint fn V1@f1() -> Html {
                  let v3: String = "back\\slash"
                  let v4: Bool = true
                  let v5: Int = -86
                  let v6: Option[Float] = None
                  let v7: Option[Option[Float]] = Some(v6)
                  let v8: E1 = W0 {f1: v5, f0: v7}
                  let v9: (String, Bool, E1) = (v3, v4, v8)
                  let v10: Option[Int] = None
                  let v11: (Bool, E1) = (v4, v8)
                  let v12: ((String, Bool, E1), Option[Int], (Bool, E1)) = (v9, v10, v11)
                  let v13: (String, Bool, E1) = v12.0
                  let v14: Bool = v13.1
                  let v23: Html = match v14 {
                    true => {
                      let v15: Html = text("&amp;")
                      v15
                    }
                    true => {
                      let v16: Int = -1
                      let v17: Int = 0
                      let v18: Array[Int] = v16..=v17
                      let v20: Html = for _ in v18 {
                        let v19: Html = concat()
                        v19
                      }
                      v20
                    }
                    false => {
                      let v21: Html = concat()
                      let v22: Html = element("input", {}, v21)
                      v22
                    }
                  }
                  v23
                }
            "#]],
        );
    }
}
