use crate::hop::typing::{
    DataType, EnumVariant, ResolvedType, Type, TypeRegistry, TypeRegistryBuilder,
};
use crate::ir::flat_module::FlatModule;
use crate::ir::flat_module_builder::{DeclaredFunction, FlatBlockBuilder, FlatModuleBuilder, Pat};
use crate::ir::ir_function::IrFunction;
use crate::ir::runtime::random::random_value;
use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use arbitrary::Unstructured;
use rand::Rng;
use std::collections::{HashMap, HashSet};
use std::ops::RangeInclusive;

/// How deep the blocks of a body nest, in the arms of matches and the
/// bodies of loops.
const MAX_BLOCK_DEPTH: usize = 3;

/// The most bindings a block has before its result.
const MAX_BINDINGS_PER_BLOCK: usize = 3;

/// How deep the values of a literal nest.
const MAX_LITERAL_DEPTH: usize = 2;

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

const SCALARS: &[&str] = &["Int", "String", "Bool", "Float"];

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
    next_var: usize,
    /// The Html values the path being generated uses. An Html value is used
    /// at most once on every path, as the typechecker requires of the
    /// source.
    used: HashSet<ValueId>,
    /// The Html values bound outside the loop body being generated, which
    /// it may not use, since it runs once per element.
    outside: HashSet<ValueId>,
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
        next_var: 0,
        used: HashSet::new(),
        outside: HashSet::new(),
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
                    .map(|j| (format!("f{j}"), g.random_field_type_string(2)))
                    .collect::<Vec<_>>();
                types = types.record(&name, fields.iter().map(|(f, t)| (f.as_str(), t.as_str())));
                g.record_names.push(name);
            }
            Decl::Enum(i) => {
                let name = format!("E{i}");
                let variants: Vec<(String, Vec<(String, String)>)> = (0..g.count(1..=3))
                    .map(|j| {
                        let fields = (0..g.count(0..=2))
                            .map(|k| (format!("f{k}"), g.random_field_type_string(2)))
                            .collect();
                        (format!("W{j}"), fields)
                    })
                    .collect();
                types = types.enum_(
                    &name,
                    variants.iter().map(|(v, fs)| {
                        (
                            v.as_str(),
                            fs.iter().map(|(f, t)| (f.as_str(), t.as_str())).collect(),
                        )
                    }),
                );
                g.enum_names.push(name);
            }
        }
    }

    let types = types.build();
    g.records = g
        .record_names
        .iter()
        .map(|name| types.named(name))
        .collect();
    g.enums = g.enum_names.iter().map(|name| types.named(name)).collect();
    let mut builder = FlatModuleBuilder::with_types(types);

    // Declare every function before defining any, so that a body can call
    // a function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let params: Vec<(String, String)> = (0..g.count(0..=3))
            .map(|_| (g.fresh_var_name(), g.random_type_string(2)))
            .collect();
        let return_type = g.random_type_string(2);
        builder = builder.declare(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
            &return_type,
        );
    }

    // Generate entrypoint functions, each rendering Html as a page body does.
    // Their parameters are drawn before any body, like those of the
    // functions above.
    let entrypoints: Vec<(String, Vec<(String, String)>)> = if single_test_entrypoint {
        vec![("Test".to_string(), Vec::new())]
    } else {
        (0..g.count(1..=3))
            .map(|i| {
                let params = (0..g.count(0..=3))
                    .map(|_| (g.fresh_var_name(), g.random_type_string(2)))
                    .collect();
                (format!("V{i}"), params)
            })
            .collect()
    };

    // Each body uses only its own Html values.
    for name in &function_names {
        builder = builder.define(name, |b, declared| {
            g.used.clear();
            g.outside.clear();
            g.block(b, &declared.return_type, MAX_BLOCK_DEPTH)
        });
    }
    for (name, params) in &entrypoints {
        builder = builder.entrypoint(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
            "Html",
            |b| {
                g.used.clear();
                g.outside.clear();
                g.block(b, &Type::Html, MAX_BLOCK_DEPTH)
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
    fn record_fields_of_type(
        &self,
        b: &FlatBlockBuilder<'_>,
        target: &Type,
    ) -> Vec<(Type, String)> {
        let mut candidates = Vec::new();
        for record in &self.records {
            let Some(ResolvedType::Record { fields, .. }) = resolve(b.registry(), record) else {
                unreachable!("a record of the module resolves to its declaration")
            };
            for field in fields {
                if target.as_data() == Some(&field.typ) {
                    candidates.push((record.clone(), field.name.as_str().to_string()));
                }
            }
        }
        candidates
    }

    /// The functions whose calls produce a value of `target`. An entrypoint is
    /// never called.
    fn callees<'t>(b: &FlatBlockBuilder<'t>, target: &Type) -> Vec<&'t DeclaredFunction> {
        b.functions()
            .iter()
            .filter(|function| !function.entrypoint && function.return_type == *target)
            .collect()
    }

    /// With even odds, pick a reference to a name visible in `b` whose type
    /// fits, when there is one.
    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        fits: impl Fn(&Type) -> bool,
    ) -> Option<ValueId> {
        let candidates = b.values_where(fits);
        if candidates.is_empty() || !self.coin() {
            return None;
        }
        Some(*self.u.choose(&candidates).unwrap())
    }

    fn fresh_var_name(&mut self) -> String {
        let n = self.next_var;
        self.next_var += 1;
        format!("v{n}")
    }

    /// A random source-syntax type string. E.g. `Array[String]`.
    fn random_type_string(&mut self, depth: usize) -> String {
        self.type_string(depth, NamedTypes::Any, true)
    }

    /// A random type of data, which can be part of another type.
    fn random_data_type(&mut self, b: &FlatBlockBuilder<'_>, depth: usize) -> DataType {
        let Type::Data(typ) = b.resolve_type(&self.type_string(depth, NamedTypes::Any, false))
        else {
            unreachable!("a type string without Html is data")
        };
        typ
    }

    /// A random source-syntax type string for a field of the record or
    /// enum currently being declared.
    fn random_field_type_string(&mut self, depth: usize) -> String {
        self.type_string(depth, NamedTypes::Complete, false)
    }

    /// A random type. It is Html only when `html` allows it, and the types
    /// it is made of never are, since Html is never part of another type.
    fn type_string(&mut self, depth: usize, named: NamedTypes, html: bool) -> String {
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
            P::Scalar => SCALARS[self.index(SCALARS.len())].to_string(),
            P::Html => "Html".to_string(),
            P::Named => {
                let i = self.index(named_count);
                self.named_type(named, i)
            }
            P::Array => format!(
                "Array[{}]",
                self.type_string(depth - 1, NamedTypes::Any, false)
            ),
            P::Option => format!(
                "Option[{}]",
                self.type_string(depth - 1, NamedTypes::Any, false)
            ),
            P::Tuple => {
                let arity = self.count(0..=3);
                let elements: Vec<String> = (0..arity)
                    .map(|_| self.type_string(depth - 1, named, false))
                    .collect();
                if arity == 1 {
                    format!("({},)", elements[0])
                } else {
                    format!("({})", elements.join(", "))
                }
            }
        }
    }

    fn named_type_count(&self, named: NamedTypes) -> usize {
        match named {
            NamedTypes::Any => self.declared_names.len(),
            NamedTypes::Complete => self.record_names.len() + self.enum_names.len(),
        }
    }

    fn named_type(&self, named: NamedTypes, i: usize) -> String {
        match named {
            NamedTypes::Any => self.declared_names[i].clone(),
            NamedTypes::Complete if i < self.record_names.len() => self.record_names[i].clone(),
            NamedTypes::Complete => self.enum_names[i - self.record_names.len()].clone(),
        }
    }

    /// The names in scope of type `target` that an op may read: not
    /// an Html value that the path already uses or that is bound outside
    /// the loop body being generated.
    fn readable(&self, b: &FlatBlockBuilder<'_>, target: &Type) -> Vec<ValueId> {
        b.values_where(|ty| *ty == *target)
            .into_iter()
            .filter(|name| {
                *target != Type::Html || !(self.used.contains(name) || self.outside.contains(name))
            })
            .collect()
    }

    /// An arm of a match, generated from the Html values `before` uses, as
    /// a path of its own, with the Html values it uses added to `after`.
    fn arm(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        target: &Type,
        depth: usize,
        before: &HashSet<ValueId>,
        after: &mut HashSet<ValueId>,
    ) -> ValueId {
        self.used = before.clone();
        let body = self.block(b, target, depth);
        after.extend(self.used.iter().copied());
        body
    }

    /// The body of a loop, which may not use the Html values bound outside
    /// it.
    fn loop_body(&mut self, b: &FlatBlockBuilder<'_>, depth: usize) -> ValueId {
        let outside = self.outside.clone();
        self.outside.extend(b.values_where(|ty| *ty == Type::Html));
        let body = self.block(b, &Type::Html, depth);
        self.outside = outside;
        body
    }

    /// Generate the bindings of a block of the target type, each reading
    /// what the bindings before it bind, and give the result of the block.
    /// The blocks it nests nest at most `depth` deep. The caller uses the
    /// result once, so an Html result counts as used.
    fn block(&mut self, b: &FlatBlockBuilder<'_>, target: &Type, depth: usize) -> ValueId {
        let html = *target == Type::Html;
        let mut bound = Vec::new();
        for _ in 0..self.count(0..=MAX_BINDINGS_PER_BLOCK) {
            // Half of what a block of Html binds is Html, as in a template,
            // and a block of data binds none, since it could never reach
            // the result.
            let typ = if html && self.coin() {
                Type::Html
            } else {
                b.resolve_type(&self.type_string(0, NamedTypes::Any, false))
            };
            let value = self.production(b, &typ, depth);
            if typ == Type::Html {
                bound.push(value);
            }
        }
        let result = if self.coin() {
            self.operand(b, target)
        } else {
            self.production(b, target, depth)
        };
        if !html {
            return result;
        }
        self.used.insert(result);
        // The Html the block binds and nothing reads is part of its
        // result, as a template renders what it builds.
        let mut parts: Vec<ValueId> = bound
            .into_iter()
            .filter(|value| !self.used.contains(value))
            .collect();
        if parts.is_empty() {
            return result;
        }
        parts.push(result);
        self.used.extend(parts.iter().copied());
        let result = b.html_concat(parts);
        self.used.insert(result);
        result
    }

    /// A value of type `typ` for an op to read. An Html value counts as
    /// used.
    fn operand(&mut self, b: &FlatBlockBuilder<'_>, typ: &Type) -> ValueId {
        self.value(b, typ, MAX_LITERAL_DEPTH)
    }

    /// A name in scope of type `typ` that may be read, or a literal that
    /// nests at most `depth` values deep. An Html value counts as used.
    fn value(&mut self, b: &FlatBlockBuilder<'_>, typ: &Type, depth: usize) -> ValueId {
        let candidates = self.readable(b, typ);
        let value = match candidates.get(self.index(candidates.len() + 1)) {
            Some(name) => *name,
            None => self.leaf(b, typ, depth),
        };
        if *typ == Type::Html {
            self.used.insert(value);
        }
        value
    }

    /// Plan the arms of an enum match.
    fn enum_match_arms(
        &mut self,
        variants: &[EnumVariant],
    ) -> Vec<(String, Vec<(String, String)>)> {
        let mut remaining: Vec<&EnumVariant> = variants.iter().collect();
        let mut arms = Vec::new();
        while !remaining.is_empty() {
            let variant = remaining.swap_remove(self.index(remaining.len()));
            let mut bindings = Vec::new();
            for field in &variant.fields {
                if self.coin() {
                    bindings.push((field.name.as_str().to_string(), self.fresh_var_name()));
                }
            }
            arms.push((variant.name.as_str().to_string(), bindings));
        }
        arms
    }

    /// A random pattern for values of `typ` that tests at most `depth`
    /// constructors on any path from its root. A pattern of depth zero
    /// matches every value.
    fn pattern(&mut self, b: &FlatBlockBuilder<'_>, typ: &Type, depth: usize) -> Pat {
        if depth > 0 && self.index(3) > 0 {
            match typ {
                Type::Data(DataType::Bool) => {
                    return if self.coin() { Pat::True } else { Pat::False };
                }
                Type::Data(DataType::Option(inner)) => {
                    return if self.coin() {
                        Pat::some(self.pattern(b, &Type::Data((**inner).clone()), depth - 1))
                    } else {
                        Pat::None
                    };
                }
                Type::Data(DataType::Tuple(elements)) => {
                    return Pat::Tuple(
                        elements
                            .iter()
                            .map(|element| self.pattern(b, &Type::Data(element.clone()), depth - 1))
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
                                self.pattern(b, &Type::Data(field.typ.clone()), depth - 1);
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
        Pat::Bind(self.fresh_var_name())
    }

    /// Bind a random op of the target type in the block `b` builds, with
    /// its operands taken from what is in scope, and give its name. The
    /// blocks it nests nest at most `depth` deep.
    fn production(&mut self, b: &FlatBlockBuilder<'_>, target: &Type, depth: usize) -> ValueId {
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
        let record_fields = self.record_fields_of_type(b, target);
        if !record_fields.is_empty() {
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
        if *target == Type::array(DataType::Int) {
            productions.push(P::ArrayFromRange);
        }
        if *target == Type::STRING {
            productions.extend([P::Concat, P::IntToString]);
        }
        if *target == Type::Html {
            productions.extend([P::HtmlText, P::HtmlEscape, P::HtmlElement, P::HtmlConcat]);
        }
        if !Self::callees(b, target).is_empty() {
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
        match self.u.choose(&productions).unwrap() {
            P::Leaf => self.leaf(b, target, MAX_LITERAL_DEPTH),
            P::FieldAccess => {
                let var =
                    self.var_in_scope(b, |ty| record_fields.iter().any(|(record, _)| record == ty));
                if let Some(object) = var {
                    let fields: Vec<&String> = record_fields
                        .iter()
                        .filter(|(record, _)| *record == b.typ(object))
                        .map(|(_, field)| field)
                        .collect();
                    return b.field_access(object, self.u.choose(&fields).unwrap());
                }
                let (record, field) = self.u.choose(&record_fields).unwrap().clone();
                let object = self.operand(b, &record);
                b.field_access(object, &field)
            }
            P::MatchBool => {
                let subject = self.operand(b, &Type::BOOL);
                let before = self.used.clone();
                let mut after = before.clone();
                let result = b.match_expr(subject, |m| {
                    m.arm(Pat::True, |b| {
                        self.arm(b, target, depth - 1, &before, &mut after)
                    });
                    m.arm(Pat::False, |b| {
                        self.arm(b, target, depth - 1, &before, &mut after)
                    });
                });
                self.used = after;
                result
            }
            P::MatchOption => {
                let subject = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(b, 1));
                        self.operand(b, &option_ty)
                    });
                let inner = match self.coin() {
                    true => Pat::Bind(self.fresh_var_name()),
                    false => Pat::Wildcard,
                };
                let before = self.used.clone();
                let mut after = before.clone();
                let result = b.match_expr(subject, |m| {
                    m.arm(Pat::some(inner), |b| {
                        self.arm(b, target, depth - 1, &before, &mut after)
                    });
                    m.arm(Pat::None, |b| {
                        self.arm(b, target, depth - 1, &before, &mut after)
                    });
                });
                self.used = after;
                result
            }
            P::MatchEnum => {
                let enums = self.enums.clone();
                let var = self.var_in_scope(b, |ty| enums.contains(ty));
                let subject_ty = match var {
                    Some(var) => b.typ(var),
                    None => self.u.choose(&enums).unwrap().clone(),
                };
                let subject = var.unwrap_or_else(|| self.operand(b, &subject_ty));
                let Some(ResolvedType::Enum { variants, .. }) = resolve(b.registry(), &subject_ty)
                else {
                    unreachable!("an enum of the module resolves to its declaration")
                };
                let arm_plan = self.enum_match_arms(variants);
                let before = self.used.clone();
                let mut after = before.clone();
                let result = b.match_expr(subject, |m| {
                    for (variant, bindings) in arm_plan {
                        let fields = bindings
                            .into_iter()
                            .map(|(field, var)| (field, Pat::Bind(var)))
                            .collect();
                        m.arm(Pat::Variant(variant, fields), |b| {
                            self.arm(b, target, depth - 1, &before, &mut after)
                        });
                    }
                });
                self.used = after;
                result
            }
            P::Eq => {
                let ty = match self.index(4) {
                    0 => Type::INT,
                    1 => Type::STRING,
                    2 => Type::BOOL,
                    _ => Type::FLOAT,
                };
                let left = self.operand(b, &ty);
                let right = self.operand(b, &ty);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.operand(b, &ty);
                let right = self.operand(b, &ty);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() { Type::INT } else { Type::FLOAT };
                let left = self.operand(b, &ty);
                let right = self.operand(b, &ty);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.operand(b, &Type::BOOL);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.operand(b, &Type::STRING))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.operand(b, &Type::INT);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.operand(b, &Type::FLOAT);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.operand(b, &Type::INT);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(b, 1));
                        self.operand(b, &option_ty)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Option(_))))
                    .unwrap_or_else(|| {
                        let option_ty = Type::option(self.random_data_type(b, 1));
                        self.operand(b, &option_ty)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.operand(b, &Type::STRING);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(b, 1));
                        self.operand(b, &array_ty)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(b, 1));
                        self.operand(b, &array_ty)
                    });
                b.array_length(operand)
            }
            P::ArrayFromRange => {
                // Literal bounds only, generated arithmetic can produce huge
                // Int values, which would make evaluation iterate forever.
                let start = b.int(self.zigzag(3));
                let end = b.int(self.zigzag(3));
                b.array_from_range(start, end)
            }
            P::TupleIndex => {
                let Type::Data(target_data) = target else {
                    unreachable!("a tuple holds data, so it is indexed only for data")
                };
                let var = self.var_in_scope(
                    b,
                    |ty| matches!(ty, Type::Data(DataType::Tuple(elements)) if elements.contains(target_data)),
                );
                if let Some(tuple) = var {
                    let Type::Data(DataType::Tuple(elements)) = b.typ(tuple) else {
                        unreachable!("the variable has a tuple type")
                    };
                    let indices: Vec<usize> = (0..elements.len())
                        .filter(|&index| elements[index] == *target_data)
                        .collect();
                    return b.tuple_index(tuple, *self.u.choose(&indices).unwrap());
                }
                let arity = self.count(1..=3);
                let index = self.index(arity);
                let elements = (0..arity)
                    .map(|i| {
                        if i == index {
                            target_data.clone()
                        } else {
                            self.random_data_type(b, 1)
                        }
                    })
                    .collect();
                let tuple = self.operand(b, &Type::Data(DataType::Tuple(elements)));
                b.tuple_index(tuple, index)
            }
            P::MatchPattern => {
                let subject = self
                    .var_in_scope(b, |ty| {
                        matches!(
                            ty,
                            Type::Data(
                                DataType::Bool
                                    | DataType::Option(_)
                                    | DataType::Tuple(_)
                                    | DataType::Named { .. }
                            )
                        )
                    })
                    .unwrap_or_else(|| {
                        let elements = (0..self.count(1..=3))
                            .map(|_| self.random_data_type(b, 1))
                            .collect();
                        self.operand(b, &Type::Data(DataType::Tuple(elements)))
                    });
                let subject_ty = b.typ(subject);
                let mut pats: Vec<Pat> = (0..self.count(1..=3))
                    .map(|_| self.pattern(b, &subject_ty, 2))
                    .collect();
                // A last arm that matches every value makes the match
                // exhaustive, whether or not the arms before it are.
                pats.push(self.pattern(b, &subject_ty, 0));
                let before = self.used.clone();
                let mut after = before.clone();
                let result = b.match_expr(subject, |m| {
                    for pat in pats {
                        m.arm(pat, |b| self.arm(b, target, depth - 1, &before, &mut after));
                    }
                });
                self.used = after;
                result
            }
            P::Neg => {
                let operand = self.operand(b, target);
                b.neg(operand)
            }
            P::Add => {
                let left = self.operand(b, target);
                let right = self.operand(b, target);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.operand(b, target);
                let right = self.operand(b, target);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.operand(b, target);
                let right = self.operand(b, target);
                b.mul(left, right)
            }
            P::HtmlText => b.text(self.u.choose(TEXTS).unwrap()),
            P::HtmlEscape => {
                let operand = self.operand(b, &Type::STRING);
                b.escape(operand)
            }
            P::HtmlElement => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                // Attribute names are unique within an element, so each
                // candidate is included at most once.
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.operand(b, &Type::STRING);
                        attributes.push(b.attr(name, value));
                    }
                }
                if self.coin() {
                    let present = self.operand(b, &Type::BOOL);
                    attributes.push(b.presence("disabled", present));
                }
                let children = if void {
                    // The element reads its children, so they count as used.
                    let children = b.html_concat(Vec::new());
                    self.used.insert(children);
                    children
                } else {
                    self.operand(b, &Type::Html)
                };
                b.element(tag, attributes, children)
            }
            P::HtmlConcat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.operand(b, &Type::Html))
                    .collect();
                b.html_concat(parts)
            }
            P::HtmlForArray => {
                let array = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Data(DataType::Array(_))))
                    .unwrap_or_else(|| {
                        let array_ty = Type::array(self.random_data_type(b, 1));
                        self.operand(b, &array_ty)
                    });
                let var = self.fresh_var_name();
                b.html_for(Some(&var), array, |b| self.loop_body(b, depth - 1))
            }
            P::HtmlForRange => {
                // Literal bounds only, as for an array from a range.
                let start = b.int(self.zigzag(3));
                let end = b.int(self.zigzag(3));
                let array = b.array_from_range(start, end);
                let var = self.coin().then(|| self.fresh_var_name());
                b.html_for(var.as_deref(), array, |b| self.loop_body(b, depth - 1))
            }
            P::Call => {
                let callee = *self.u.choose(&Self::callees(b, target)).unwrap();
                let args: Vec<_> = callee
                    .parameters
                    .iter()
                    .map(|parameter| self.operand(b, &parameter.binder.typ))
                    .collect();
                b.call(callee.function.name.as_str(), args)
            }
        }
    }

    /// A leaf of the target type, its parts nesting at most `depth`
    /// values deep (or deeper if the target type forces it).
    fn leaf(&mut self, b: &FlatBlockBuilder<'_>, target: &Type, depth: usize) -> ValueId {
        let Type::Data(target) = target else {
            return match self.index(3) {
                0 => b.html_concat(Vec::new()),
                1 => b.text(self.u.choose(TEXTS).unwrap()),
                _ => {
                    let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                    let children = if void || depth == 0 {
                        let children = b.html_concat(Vec::new());
                        self.used.insert(children);
                        children
                    } else {
                        self.value(b, &Type::Html, depth - 1)
                    };
                    b.element(tag, Vec::new(), children)
                }
            };
        };
        match target {
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
                    .map(|_| self.value(b, &Type::Data((**inner).clone()), depth.saturating_sub(1)))
                    .collect();
                if elements.is_empty() {
                    b.empty_array(Type::Data(inner.as_ref().clone()))
                } else {
                    b.array(elements)
                }
            }
            DataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value =
                        self.value(b, &Type::Data((**inner).clone()), depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none(Type::Data(inner.as_ref().clone()))
                }
            }
            DataType::Named { name, .. } => match b.registry().resolve(target) {
                Some(ResolvedType::Record { fields, .. }) => {
                    let fields = self.shuffled(fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        let ty = Type::Data(field.typ.clone());
                        values.push((
                            field.name.as_str(),
                            self.value(b, &ty, depth.saturating_sub(1)),
                        ));
                    }
                    b.record(name.as_str(), values)
                }
                Some(ResolvedType::Enum { variants, .. }) => {
                    let variant = self.u.choose(variants).unwrap();
                    let fields = self.shuffled(variant.fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        let ty = Type::Data(field.typ.clone());
                        values.push((
                            field.name.as_str(),
                            self.value(b, &ty, depth.saturating_sub(1)),
                        ));
                    }
                    b.enum_variant_with_fields(name.as_str(), variant.name.as_str(), values)
                }
                _ => unreachable!("named type must be a generated record or enum"),
            },
            DataType::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|element| {
                        self.value(b, &Type::Data(element.clone()), depth.saturating_sub(1))
                    })
                    .collect();
                b.tuple(values)
            }
        }
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
    use std::collections::BTreeMap;

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
            let kind = debug.trim_start_matches("Data(").split('(').next().unwrap();
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
        const SAMPLES: usize = 200;
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
            1019 of 65536 input bytes consumed per module on average, at most 4272
            0 of 200 modules exhaust the input
            8 of 408 entrypoints hit the recursion limit
            0 of 408 entrypoints hit the array length limit

            op                  ops/module  % live   % dyn
            Array                     25.3    12.7     0.4
            ArrayFromRange             0.9   100.0     0.0
            ArrayIsEmpty               0.9     3.2     1.6
            ArrayLength                1.2     1.6     0.4
            BoolLiteral               12.3    18.7     0.0
            BoolNegation               1.0     1.0     0.0
            Call                       1.8    27.9     7.3
            Enum                      28.7     8.5     0.3
            Equals                     1.0     0.5     0.0
            FieldAccess                1.4     1.8     0.4
            FloatLiteral              16.0     6.3     0.0
            FloatToInt                 1.2     0.8     0.0
            HtmlConcat                11.5    98.0     7.5
            HtmlElement                5.3    96.3     5.9
            HtmlEscape                 1.8    99.2     2.5
            HtmlFor                    1.8    98.9    11.6
            HtmlText                   5.3    96.4     0.0
            IntLiteral                18.7    15.3     0.0
            IntToFloat                 1.3     1.1     0.0
            IntToString                2.4     3.6     0.2
            LessThan                   1.1     2.3     0.0
            LessThanOrEqual            0.9     2.2     0.0
            Match                     20.9    22.9     6.1
            NumericAdd                 2.5     1.2     0.2
            NumericMultiply            2.6     0.4     0.0
            NumericNegation            2.4     0.8     0.0
            NumericSubtract            2.3     1.3     0.0
            Option                    24.9    14.1     0.4
            OptionIsNone               0.9     3.3     0.6
            OptionIsSome               0.9     1.7     0.0
            Record                    23.4     6.2     0.4
            StringConcat               2.4     3.8     0.0
            StringIsEmpty              0.9     1.1     0.0
            StringLiteral             17.3    23.2     0.0
            Tuple                     44.3     8.0     0.6
            TupleIndex                22.9     3.0     0.3
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

                entrypoint fn V0@f0(v0@v0: (Option[E0], (E1, Int, Bool)), v1@v1: E1) -> Html {
                  let v2: E0 = W0
                  let v8: Html = match v2 {
                    E0::W0 => {
                      let v3: String = ""
                      let v4: Html = concat()
                      let v5: Html = element("input", {id: v3, data-x: v3}, v4)
                      let v6: Html = concat()
                      let v7: Html = concat(v5, v6)
                      v7
                    }
                  }
                  v8
                }
                entrypoint fn V1@f1(v2@v9: Array[E0], v3@v10: R1, v4@v11: Array[(Bool, E1)]) -> Html {
                  let v12: Html = concat()
                  let v13: Html = element("span", {}, v12)
                  v13
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

                entrypoint fn V0@f0(v0@v0: Bool, v1@v1: Int) -> Html {
                  let v10: String = match v0 {
                    false => {
                      let v2: E0 = W1 {f0: v0}
                      let v3: (E0, Int) = (v2, v1)
                      let v4: String = ""
                      let v5: ((E0, Int), String) = (v3, v4)
                      let v6: String = v5.1
                      let v7: String = "<b>"
                      v7
                    }
                    v8: Bool => {
                      let v9: String = "cr\rtab\t"
                      v9
                    }
                  }
                  let v11: Html = text("a\"b'c")
                  let v12: Html = escape(v10)
                  let v13: Html = concat(v11, v12)
                  v13
                }
                entrypoint fn V1@f1() -> Html {
                  let v14: Html = text("back\\slash")
                  v14
                }
            "#]],
        );
    }
}
