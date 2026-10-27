use crate::hop::typing::{
    DataType, RecordField, ResolvedType, Ty, Type, TypeRegistry, TypeRegistryBuilder,
};
use crate::ir::flat_module::{FlatFunctionDeclaration, FlatModule};
use crate::ir::flat_module_builder::{DeclaredFunction, FlatBlockBuilder, FlatModuleBuilder, Pat};
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
const MAX_EXPRESSION_DEPTH: usize = 10;

/// How deep the values of a literal nest before they start to converge.
const MAX_LITERAL_DEPTH: usize = 5;

/// How deep the patterns of a match nest.
const MAX_PATTERN_DEPTH: usize = 2;

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
    ("b", false),
    ("section", false),
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

#[derive(Clone, Default)]
struct Scope {
    names: Vec<ValueId>,
}

impl Scope {
    fn push(&mut self, value: ValueId) -> ValueId {
        self.names.push(value);
        value
    }

    fn names_where<'a>(
        &'a self,
        types: &'a [Type],
        fits: impl Fn(&Type) -> bool + 'a,
    ) -> impl Iterator<Item = ValueId> + 'a {
        self.names
            .iter()
            .copied()
            .filter(move |name| fits(&types[name.index()]))
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

/// The pattern of an arm and the names it binds.
type Arm = (Pat, Vec<String>);

/// A constructor of a pattern, which takes the patterns of its fields.
enum Ctor {
    True,
    False,
    None,
    Some,
    Tuple,
    Record,
    Variant(String),
}

impl Ctor {
    fn apply(&self, fields: Vec<(String, Pat)>) -> Pat {
        match self {
            Ctor::True => Pat::True,
            Ctor::False => Pat::False,
            Ctor::None => Pat::None,
            Ctor::Some => Pat::some(fields.into_iter().next().unwrap().1),
            Ctor::Tuple => Pat::Tuple(fields.into_iter().map(|(_, pat)| pat).collect()),
            Ctor::Record => Pat::Record(fields),
            Ctor::Variant(name) => Pat::Variant(name.clone(), fields),
        }
    }
}

struct FlatGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    declared_names: Vec<String>,
    record_names: Vec<String>,
    enum_names: Vec<String>,
    records: Vec<DataType>,
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

    // Name every type before generating any field types, so that a field can
    // refer to a type declared later or to the one being declared.
    let record_count = g.count(0..=3);
    let enum_count = g.count(0..=3);
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut types = TypeRegistryBuilder::new();

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
                let fields = (0..g.count(0..=3))
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

    // Declare every function before defining any, so that a body can call a
    // function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let html = g.coin();
        let return_type = if html {
            Ty::Html
        } else {
            g.random_type_where(2, NamedTypes::Any, false)
        };
        let params: Vec<(String, Ty)> = (0..g.count(0..=8))
            .map(|i| {
                (
                    format!("p{i}"),
                    g.random_type_where(2, NamedTypes::Any, html),
                )
            })
            .collect();
        builder = builder.declare(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.clone())),
            return_type,
        );
    }

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
                scope.push(parameter.binder.var);
            }
            match &declared.return_type {
                Type::Html(_) => g.html_block(b, &mut scope, MAX_BLOCK_DEPTH),
                Type::Data(target) => g.data_block(b, &mut scope, target, MAX_BLOCK_DEPTH),
            }
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
                    scope.push(b.param(n));
                }
                g.html_block(b, &mut scope, MAX_BLOCK_DEPTH)
            },
        );
    }
    builder.build_with_registry()
}

pub fn random_entrypoint_args(
    module: &FlatModule,
    rng: &mut impl Rng,
    registry: &TypeRegistry,
) -> Vec<(IrFunction, HashMap<AttributeName, Value>)> {
    module
        .functions
        .iter()
        .filter(|decl| match decl {
            FlatFunctionDeclaration::Data(decl) => decl.entrypoint,
            FlatFunctionDeclaration::Html(decl) => decl.entrypoint,
        })
        .map(|decl| {
            let args = decl
                .parameters()
                .iter()
                .map(|param| {
                    let value = random_value(rng, &param.binder.typ, None, registry);
                    (param.name.clone(), value)
                })
                .collect();
            (decl.function().clone(), args)
        })
        .collect()
}

impl FlatGenerator<'_, '_> {
    fn count(&mut self, range: RangeInclusive<usize>) -> usize {
        self.u.int_in_range(range).unwrap()
    }

    fn index(&mut self, len: usize) -> usize {
        self.u.choose_index(len).unwrap()
    }

    fn coin(&mut self) -> bool {
        self.u.arbitrary().unwrap()
    }

    fn zigzag(&mut self, magnitude: i32) -> i32 {
        let n = self.u.int_in_range(0..=2 * magnitude).unwrap();
        if n % 2 == 0 { -n / 2 } else { n / 2 + 1 }
    }

    fn shuffled<T>(&mut self, mut items: Vec<T>) -> Vec<T> {
        let mut out = Vec::with_capacity(items.len());
        while !items.is_empty() {
            out.push(items.swap_remove(self.index(items.len())));
        }
        out
    }

    fn record_fields_of_type<'s>(
        &'s self,
        b: &FlatBlockBuilder<'s>,
        target: &'s DataType,
    ) -> impl Iterator<Item = (&'s DataType, &'s RecordField)> + 's {
        let registry = b.registry();
        self.records.iter().flat_map(move |record| {
            let Some(ResolvedType::Record { fields, .. }) = registry.resolve(record) else {
                unreachable!("a record of the module resolves to its declaration")
            };
            fields
                .iter()
                .filter(move |field| field.typ == *target)
                .map(move |field| (record, field))
        })
    }

    fn callees<'t>(
        b: &FlatBlockBuilder<'t>,
        target: &'t Type,
    ) -> impl Iterator<Item = &'t DeclaredFunction> + 't {
        b.functions()
            .iter()
            .filter(move |function| !function.entrypoint && function.return_type == *target)
    }

    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&DataType) -> bool,
    ) -> Option<(ValueId, DataType)> {
        let types = b.value_types();
        let fits = |typ: &Type| typ.as_data().is_some_and(&fits);
        if scope.names_where(&types, fits).next().is_none() || self.index(10) == 0 {
            return None;
        }
        let count = scope.names_where(&types, fits).count();
        let index = self.index(count);
        let name = scope.names_where(&types, fits).nth(index).unwrap();
        let Type::Data(typ) = &types[name.index()] else {
            unreachable!()
        };
        Some((name, typ.clone()))
    }

    fn random_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Any, true)
    }

    fn random_data_type(&mut self, depth: usize) -> DataType {
        self.random_type_where(depth, NamedTypes::Any, false).into()
    }

    fn random_field_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Complete, false)
    }

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

    fn for_loop_body(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        var: Option<ValueId>,
        depth: usize,
    ) -> ValueId {
        let mut scope = scope.clone();
        if let Some(var) = var {
            scope.push(var);
        }
        self.html_block(b, &mut scope, depth)
    }

    fn html_block(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        if self.index(DEAD_BINDING_ODDS) == DEAD_BINDING_ODDS - 1 {
            let typ = Type::from(self.random_type(1));
            match &typ {
                Type::Html(_) => self.html_op(b, scope, depth, MAX_EXPRESSION_DEPTH),
                Type::Data(target) => self.data_op(b, scope, target, depth, MAX_EXPRESSION_DEPTH),
            };
        }
        self.html_op(b, scope, depth, MAX_EXPRESSION_DEPTH)
    }

    fn data_block(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &DataType,
        depth: usize,
    ) -> ValueId {
        if self.index(DEAD_BINDING_ODDS) == DEAD_BINDING_ODDS - 1 {
            let typ = self.random_data_type(1);
            self.data_op(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
        }
        self.data_op(b, scope, target, depth, MAX_EXPRESSION_DEPTH)
    }

    fn name_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        typ: &Type,
    ) -> Option<ValueId> {
        let types = b.value_types();
        let count = scope
            .names_where(&types, |candidate| candidate == typ)
            .count();
        let index = self.index(count + 1);
        scope
            .names_where(&types, |candidate| candidate == typ)
            .nth(index)
    }

    fn html_leaf(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        match self.name_in_scope(b, scope, &Type::HTML) {
            Some(name) => name,
            None => {
                let literal = match self.index(3) {
                    0 => b.html_concat(Vec::new()),
                    1 => b.text(self.u.choose(TEXTS).unwrap()),
                    _ => {
                        let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                        let children = if void || depth == 0 {
                            b.html_concat(Vec::new())
                        } else {
                            self.html_leaf(b, scope, depth - 1)
                        };
                        b.element(tag, Vec::new(), children)
                    }
                };
                scope.push(literal)
            }
        }
    }

    fn data_leaf(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &DataType,
        depth: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(b, scope, &Type::Data(typ.clone())) {
            return name;
        }
        let literal = match typ {
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
                    .map(|_| self.data_leaf(b, scope, inner, depth.saturating_sub(1)))
                    .collect();
                if elements.is_empty() {
                    b.empty_array(Type::Data((**inner).clone()))
                } else {
                    b.array(elements)
                }
            }
            DataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.data_leaf(b, scope, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none(Type::Data((**inner).clone()))
                }
            }
            DataType::Named { .. } => match b.registry().resolve(typ) {
                Some(ResolvedType::Record { fields, .. }) => {
                    let fields = self.shuffled(fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        values.push((
                            field.name.as_str(),
                            self.data_leaf(b, scope, &field.typ, depth.saturating_sub(1)),
                        ));
                    }
                    b.record(Type::Data(typ.clone()), values)
                }
                Some(ResolvedType::Enum { variants, .. }) => {
                    let variant = self.u.choose(variants).unwrap();
                    let fields = self.shuffled(variant.fields.iter().collect());
                    let mut values = Vec::new();
                    for field in fields {
                        values.push((
                            field.name.as_str(),
                            self.data_leaf(b, scope, &field.typ, depth.saturating_sub(1)),
                        ));
                    }
                    b.enum_variant_with_fields(
                        Type::Data(typ.clone()),
                        variant.name.as_str(),
                        values,
                    )
                }
                _ => unreachable!("named type must be a generated record or enum"),
            },
            DataType::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|element| self.data_leaf(b, scope, element, depth.saturating_sub(1)))
                    .collect();
                b.tuple(values)
            }
        };
        scope.push(literal)
    }

    fn constructors(
        registry: &TypeRegistry,
        typ: &DataType,
    ) -> Vec<(Ctor, Vec<(String, DataType)>)> {
        let fields = |fields: &[RecordField]| {
            fields
                .iter()
                .map(|field| (field.name.as_str().to_string(), field.typ.clone()))
                .collect()
        };
        match typ {
            DataType::Bool => vec![(Ctor::True, Vec::new()), (Ctor::False, Vec::new())],
            DataType::Option(inner) => vec![
                (Ctor::None, Vec::new()),
                (Ctor::Some, vec![("0".to_string(), (**inner).clone())]),
            ],
            DataType::Tuple(elements) => vec![(
                Ctor::Tuple,
                elements
                    .iter()
                    .enumerate()
                    .map(|(i, element)| (i.to_string(), element.clone()))
                    .collect(),
            )],
            DataType::Named { .. } => match registry.resolve(typ) {
                Some(ResolvedType::Record { fields: fs, .. }) => vec![(Ctor::Record, fields(fs))],
                Some(ResolvedType::Enum { variants, .. }) => variants
                    .iter()
                    .map(|variant| {
                        let name = variant.name.as_str().to_string();
                        (Ctor::Variant(name), fields(&variant.fields))
                    })
                    .collect(),
                _ => unreachable!("a named type is a declared record or enum"),
            },
            _ => Vec::new(),
        }
    }

    fn can_branch(registry: &TypeRegistry, typ: &DataType, depth: usize) -> bool {
        if depth == 0 {
            return false;
        }
        let ctors = Self::constructors(registry, typ);
        ctors.len() > 1
            || ctors
                .iter()
                .flat_map(|(_, fields)| fields)
                .any(|(_, field)| Self::can_branch(registry, field, depth - 1))
    }

    fn partition(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        typ: &DataType,
        depth: usize,
        forced: bool,
        next_bind: &mut usize,
    ) -> Vec<Arm> {
        let ctors = Self::constructors(b.registry(), typ);
        if !forced && (depth == 0 || ctors.is_empty() || self.index(3) == 0) {
            return vec![self.leaf(next_bind)];
        }
        let single = ctors.len() == 1;
        // Arms of different constructors match disjoint values, so they can
        // go in any order. Sometimes the last constructors share one arm,
        // which matches every value the arms before it do not. At least one
        // constructor keeps arms of its own, so a partition that branched
        // still does.
        let ctors = self.shuffled(ctors);
        let listed = if ctors.len() > 1 && self.index(4) == 0 {
            self.count(1..=ctors.len() - 1)
        } else {
            ctors.len()
        };
        let mut arms = Vec::new();
        for (ctor, fields) in &ctors[..listed] {
            // A type with one constructor branches in one of its fields.
            let forced_field = if forced && single {
                let branching: Vec<usize> = (0..fields.len())
                    .filter(|&i| Self::can_branch(b.registry(), &fields[i].1, depth - 1))
                    .collect();
                Some(*self.u.choose(&branching).unwrap())
            } else {
                None
            };
            // Rows in lexicographic order of the arms of their fields, so
            // that every row matches some value no earlier row matches.
            let mut rows: Vec<(Vec<(String, Pat)>, Vec<String>)> = vec![(Vec::new(), Vec::new())];
            for (i, (name, field)) in fields.iter().enumerate() {
                let field_arms =
                    self.partition(b, field, depth - 1, forced_field == Some(i), next_bind);
                rows = rows
                    .into_iter()
                    .flat_map(|(pats, binds)| {
                        field_arms.iter().map(move |(pat, field_binds)| {
                            let mut pats = pats.clone();
                            pats.push((name.clone(), pat.clone()));
                            let binds = binds.iter().chain(field_binds).cloned().collect();
                            (pats, binds)
                        })
                    })
                    .collect();
            }
            arms.extend(
                rows.into_iter()
                    .map(|(pats, binds)| (ctor.apply(pats), binds)),
            );
        }
        if listed < ctors.len() {
            arms.push(self.leaf(next_bind));
        }
        arms
    }

    /// An arm that matches every value, which binds it or not.
    fn leaf(&mut self, next_bind: &mut usize) -> Arm {
        if self.coin() {
            return (Pat::Wildcard, Vec::new());
        }
        let name = format!("x{next_bind}");
        *next_bind += 1;
        (Pat::bind(&name), vec![name])
    }

    fn branching_type(&mut self, registry: &TypeRegistry) -> DataType {
        let named: Vec<DataType> = self
            .declared_names
            .iter()
            .map(|name| DataType::from(Ty::named(name)))
            .filter(|typ| Self::can_branch(registry, typ, MAX_PATTERN_DEPTH))
            .collect();
        let base: DataType = match self.index(2 + named.len()) {
            0 => Ty::Bool.into(),
            1 => Ty::option(self.random_type_where(1, NamedTypes::Any, false)).into(),
            i => return named[i - 2].clone(),
        };
        if self.coin() {
            return base;
        }
        // A bool or an option still branches one level down, in a tuple.
        let arity = self.count(1..=3);
        let index = self.index(arity);
        let elements = (0..arity)
            .map(|i| {
                if i == index {
                    base.clone()
                } else {
                    self.random_data_type(1)
                }
            })
            .collect();
        DataType::Tuple(elements)
    }

    fn html_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if nesting == 0 || self.index(6) == 0 {
            return self.html_leaf(b, scope, MAX_LITERAL_DEPTH);
        }
        enum P {
            Escape,
            Element,
            Concat,
            Call,
            Match,
            ForArray,
            ForRange,
        }
        let mut productions = vec![P::Escape, P::Element, P::Concat];
        if Self::callees(b, &Type::HTML).next().is_some() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::ForArray, P::ForRange]);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::Escape => {
                let operand = self.data_op(b, scope, &DataType::String, depth, nesting - 1);
                b.escape(operand)
            }
            P::Element => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.data_op(b, scope, &DataType::String, depth, nesting - 1);
                        attributes.push(b.attr_value(name, value));
                    }
                }
                if self.coin() {
                    let present = self.data_op(b, scope, &DataType::Bool, depth, nesting - 1);
                    attributes.push(b.attr_presence("disabled", present));
                }
                let count = if void { 0 } else { self.count(0..=4) };
                let mut children: Vec<_> = (0..count)
                    .map(|_| self.html_op(b, scope, depth, nesting - 1))
                    .collect();
                let children = if children.len() == 1 {
                    children.pop().unwrap()
                } else {
                    b.html_concat(children)
                };
                b.element(tag, attributes, children)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.html_op(b, scope, depth, nesting - 1))
                    .collect();
                b.html_concat(parts)
            }
            P::Call => self.call(b, scope, &Type::HTML, depth, nesting),
            P::Match => self.match_(b, scope, &Type::HTML, depth, nesting),
            P::ForArray => {
                let var = self.var_in_scope(b, scope, |ty| matches!(ty, DataType::Array(_)));
                let array = match var {
                    Some((array, _)) => array,
                    None => {
                        let array_ty = DataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    }
                };
                b.html_for(array, |b, var| {
                    self.for_loop_body(b, scope, Some(var), depth - 1)
                })
            }
            P::ForRange => {
                let start = b.int(self.zigzag(3));
                scope.push(start);
                let end = b.int(self.zigzag(3));
                scope.push(end);
                let array = b.array_from_range(start, end);
                scope.push(array);
                if self.coin() {
                    b.html_for(array, |b, var| {
                        self.for_loop_body(b, scope, Some(var), depth - 1)
                    })
                } else {
                    b.html_for_unbound(array, |b| self.for_loop_body(b, scope, None, depth - 1))
                }
            }
        };
        scope.push(value)
    }

    fn data_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &DataType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if nesting == 0 || self.index(4) == 0 {
            return self.data_leaf(b, scope, target, MAX_LITERAL_DEPTH);
        }
        enum P {
            FieldAccess,
            Match,
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
            Call,
        }
        let typ = Type::Data(target.clone());
        let mut productions: Vec<P> = Vec::new();
        if self.record_fields_of_type(b, target).next().is_some() {
            productions.push(P::FieldAccess);
        }
        productions.push(P::TupleIndex);
        if *target == DataType::Bool {
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
        if *target == DataType::Int {
            productions.extend([
                P::ArrayLength,
                P::Neg,
                P::Add,
                P::Sub,
                P::Mul,
                P::FloatToInt,
            ]);
        }
        if *target == DataType::Float {
            productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::IntToFloat]);
        }
        if matches!(target, DataType::Array(element) if **element == DataType::Int) {
            productions.push(P::ArrayFromRange);
        }
        if *target == DataType::String {
            productions.extend([P::Concat, P::IntToString]);
        }
        if Self::callees(b, &typ).next().is_some() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.push(P::Match);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::FieldAccess => {
                let record_fields: Vec<(DataType, String)> = self
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
                        let object = self.data_op(b, scope, &record, depth, nesting - 1);
                        b.field_access(object, &field)
                    }
                }
            }
            P::Eq => {
                let ty = match self.index(4) {
                    0 => DataType::Int,
                    1 => DataType::String,
                    2 => DataType::Bool,
                    _ => DataType::Float,
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() {
                    DataType::Int
                } else {
                    DataType::Float
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() {
                    DataType::Int
                } else {
                    DataType::Float
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.data_op(b, scope, &DataType::Bool, depth, nesting - 1);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=3))
                    .map(|_| self.data_op(b, scope, &DataType::String, depth, nesting - 1))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.data_op(b, scope, &DataType::Int, depth, nesting - 1);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.data_op(b, scope, &DataType::Float, depth, nesting - 1);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.data_op(b, scope, &DataType::Int, depth, nesting - 1);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, DataType::Option(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = DataType::Option(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, DataType::Option(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = DataType::Option(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.data_op(b, scope, &DataType::String, depth, nesting - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, DataType::Array(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = DataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, DataType::Array(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = DataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    });
                b.array_length(operand)
            }
            P::ArrayFromRange => {
                let start = b.int(self.zigzag(3));
                scope.push(start);
                let end = b.int(self.zigzag(3));
                scope.push(end);
                b.array_from_range(start, end)
            }
            P::TupleIndex => {
                let var = self.var_in_scope(
                    b,
                    scope,
                    |ty| matches!(ty, DataType::Tuple(elements) if elements.contains(target)),
                );
                match var {
                    Some((tuple, tuple_ty)) => {
                        let DataType::Tuple(elements) = tuple_ty else {
                            unreachable!("the variable has a tuple type")
                        };
                        let indices: Vec<usize> = (0..elements.len())
                            .filter(|&index| elements[index] == *target)
                            .collect();
                        b.tuple_index(tuple, *self.u.choose(&indices).unwrap())
                    }
                    None => {
                        let arity = self.count(1..=3);
                        let index = self.index(arity);
                        let elements = (0..arity)
                            .map(|i| {
                                if i == index {
                                    target.clone()
                                } else {
                                    self.random_data_type(1)
                                }
                            })
                            .collect();
                        let tuple =
                            self.data_op(b, scope, &DataType::Tuple(elements), depth, nesting - 1);
                        b.tuple_index(tuple, index)
                    }
                }
            }
            P::Match => self.match_(b, scope, &typ, depth, nesting),
            P::Neg => {
                let operand = self.data_op(b, scope, target, depth, nesting - 1);
                b.neg(operand)
            }
            P::Add => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.mul(left, right)
            }
            P::Call => self.call(b, scope, &typ, depth, nesting),
        };
        scope.push(value)
    }

    fn call(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let callees: Vec<_> = Self::callees(b, target).collect();
        let callee = *self.u.choose(&callees).unwrap();
        let args: Vec<_> = callee
            .parameters
            .iter()
            .map(|parameter| match &parameter.binder.typ {
                Type::Html(_) => self.html_op(b, scope, depth, nesting - 1),
                Type::Data(target) => self.data_op(b, scope, target, depth, nesting - 1),
            })
            .collect();
        b.call(callee.function.name.as_str(), args)
    }

    fn match_(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &Type,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let registry = b.registry();
        let var = self.var_in_scope(b, scope, |ty| {
            Self::can_branch(registry, ty, MAX_PATTERN_DEPTH)
        });
        let (subject, subject_ty) = match var {
            Some(pair) => pair,
            None => {
                let subject_ty = self.branching_type(registry);
                (
                    self.data_op(b, scope, &subject_ty, depth, nesting - 1),
                    subject_ty,
                )
            }
        };
        let arms = self.partition(b, &subject_ty, MAX_PATTERN_DEPTH, true, &mut 0);
        b.match_(subject, |m| {
            for (pat, binds) in arms {
                m.arm(pat, |b, bindings| {
                    let depth = depth - 1;
                    let mut scope = scope.clone();
                    for name in binds {
                        scope.push(bindings.binding(&name));
                    }
                    match target {
                        Type::Html(_) => self.html_block(b, &mut scope, depth),
                        Type::Data(target) => self.data_block(b, &mut scope, target, depth),
                    }
                });
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::flat_module::{
        FlatDataBinding, FlatDataBlock, FlatDataOp, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp,
    };
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

    #[derive(Clone, Copy)]
    enum Block<'a> {
        Data(&'a FlatDataBlock),
        Html(&'a FlatHtmlBlock),
    }

    #[derive(Clone, Copy)]
    enum Binding<'a> {
        Data(&'a FlatDataBinding),
        Html(&'a FlatHtmlBinding),
    }

    impl<'a> Block<'a> {
        fn of(function: &'a FlatFunctionDeclaration) -> Self {
            match function {
                FlatFunctionDeclaration::Data(function) => Block::Data(&function.body),
                FlatFunctionDeclaration::Html(function) => Block::Html(&function.body),
            }
        }

        fn result(self) -> ValueId {
            match self {
                Block::Data(block) => block.result,
                Block::Html(block) => block.result,
            }
        }

        fn bindings(self) -> Vec<Binding<'a>> {
            match self {
                Block::Data(block) => block.bindings.iter().map(Binding::Data).collect(),
                Block::Html(block) => block
                    .bindings
                    .iter()
                    .map(Binding::Data)
                    .chain(block.html.iter().map(Binding::Html))
                    .collect(),
            }
        }
    }

    impl<'a> Binding<'a> {
        fn var(self) -> ValueId {
            match self {
                Binding::Data(binding) => binding.binder.var,
                Binding::Html(binding) => binding.binder.var,
            }
        }

        fn for_each_operand(self, f: &mut impl FnMut(ValueId)) {
            match self {
                Binding::Data(binding) => binding.op.for_each_operand(f),
                Binding::Html(binding) => binding.op.for_each_operand(f),
            }
        }

        fn kind(self) -> String {
            let debug = match self {
                Binding::Data(binding) => format!("{:?}", binding.op),
                Binding::Html(binding) => format!("{:?}", binding.op),
            };
            let kind = debug
                .trim_start_matches("Data(")
                .split('(')
                .next()
                .unwrap()
                .trim_end_matches(')');
            match self {
                Binding::Html(binding)
                    if !matches!(binding.op, FlatHtmlOp::Call(..) | FlatHtmlOp::Match(_)) =>
                {
                    format!("Html{kind}")
                }
                _ => kind.to_string(),
            }
        }

        fn nested(self) -> Vec<(Vec<ValueId>, Block<'a>)> {
            let bound = |pattern: &crate::ir::ir_match::IrPattern| {
                let mut names = Vec::new();
                pattern.for_each_binding(&mut |binder| names.push(binder.var));
                names
            };
            match self {
                Binding::Data(FlatDataBinding {
                    op: FlatDataOp::Match(match_),
                    ..
                }) => match_
                    .arms
                    .iter()
                    .map(|arm| (bound(&arm.pattern), Block::Data(&arm.body)))
                    .collect(),
                Binding::Html(FlatHtmlBinding {
                    op: FlatHtmlOp::Match(match_),
                    ..
                }) => match_
                    .arms
                    .iter()
                    .map(|arm| (bound(&arm.pattern), Block::Html(&arm.body)))
                    .collect(),
                Binding::Html(FlatHtmlBinding {
                    op: FlatHtmlOp::For(for_),
                    ..
                }) => {
                    let var = for_.var.iter().map(|binder| binder.var).collect();
                    vec![(var, Block::Html(&for_.body))]
                }
                _ => Vec::new(),
            }
        }
    }

    fn mark_live(block: Block<'_>, live: &mut HashSet<ValueId>) {
        live.insert(block.result());
        for binding in block.bindings().into_iter().rev() {
            if !live.contains(&binding.var()) {
                continue;
            }
            for (_, body) in binding.nested() {
                mark_live(body, live);
            }
            binding.for_each_operand(&mut |operand| {
                live.insert(operand);
            });
        }
    }

    fn mark_dynamic(block: Block<'_>, dynamic: &mut HashSet<ValueId>) -> bool {
        for binding in block.bindings() {
            let mut depends = false;
            binding.for_each_operand(&mut |operand| {
                depends |= dynamic.contains(&operand);
            });
            let operands = depends;
            for (bound, body) in binding.nested() {
                if operands {
                    dynamic.extend(bound);
                }
                depends |= mark_dynamic(body, dynamic);
            }
            if depends {
                dynamic.insert(binding.var());
            }
        }
        dynamic.contains(&block.result())
    }

    fn count_ops(
        block: Block<'_>,
        live: &HashSet<ValueId>,
        dynamic: &HashSet<ValueId>,
        counts: &mut BTreeMap<String, [usize; 3]>,
    ) {
        for binding in block.bindings() {
            let column = counts.entry(binding.kind()).or_default();
            column[0] += 1;
            if live.contains(&binding.var()) {
                column[1] += 1;
                if dynamic.contains(&binding.var()) {
                    column[2] += 1;
                }
            }
            for (_, body) in binding.nested() {
                count_ops(body, live, dynamic, counts);
            }
        }
    }

    fn count_html_references(
        block: Block<'_>,
        references: &mut HashMap<ValueId, usize>,
        html: &mut HashSet<ValueId>,
    ) {
        for binding in block.bindings() {
            if let Binding::Html(binding) = binding {
                html.insert(binding.binder.var);
            }
            binding.for_each_operand(&mut |operand| {
                *references.entry(operand).or_default() += 1;
            });
            for (_, body) in binding.nested() {
                count_html_references(body, references, html);
            }
        }
        *references.entry(block.result()).or_default() += 1;
    }

    #[derive(Default)]
    struct MatchStats {
        arm_counts: Vec<usize>,
        binding_arms: usize,
    }

    impl MatchStats {
        fn count_block(&mut self, block: Block<'_>) {
            for binding in block.bindings() {
                let arms = match binding {
                    Binding::Data(FlatDataBinding {
                        op: FlatDataOp::Match(m),
                        ..
                    }) => m.arms.iter().map(|arm| &arm.pattern).collect(),
                    Binding::Html(FlatHtmlBinding {
                        op: FlatHtmlOp::Match(m),
                        ..
                    }) => m.arms.iter().map(|arm| &arm.pattern).collect(),
                    _ => Vec::new(),
                };
                if !arms.is_empty() {
                    assert!(arms.len() > 1, "generated matches branch");
                    self.arm_counts.push(arms.len());
                }
                for pattern in arms {
                    let mut binds = false;
                    pattern.for_each_binding(&mut |_| binds = true);
                    self.binding_arms += binds as usize;
                }
                for (_, body) in binding.nested() {
                    self.count_block(body);
                }
            }
        }
    }

    #[test]
    fn should_report_generator_stats() {
        const SAMPLES: usize = 100;
        const INPUT_LEN: usize = 65_536;
        let mut rng = SmallRng::seed_from_u64(0);
        let mut counts = BTreeMap::new();
        let mut entrypoints = 0;
        let mut call_depth_limit = 0;
        let mut array_len_limit = 0;
        let mut string_len_limit = 0;
        let mut html_size_limit = 0;
        let mut op_count_limit = 0;
        let mut consumed = 0;
        let mut most_consumed = 0;
        let mut exhausted = 0;
        let mut html_bindings = 0;
        let mut shared_html = 0;
        let mut match_stats = MatchStats::default();
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
                call_depth_limit +=
                    matches!(result, Err(EvalError::CallDepthLimit { .. })) as usize;
                array_len_limit +=
                    matches!(result, Err(EvalError::ArrayLengthLimit { .. })) as usize;
                string_len_limit +=
                    matches!(result, Err(EvalError::StringLengthLimit { .. })) as usize;
                html_size_limit += matches!(result, Err(EvalError::HtmlSizeLimit { .. })) as usize;
                op_count_limit += matches!(result, Err(EvalError::OpCountLimit { .. })) as usize;
            }
            let mut live = HashSet::new();
            let mut dynamic = HashSet::new();
            let bodies: Vec<Block<'_>> = module.functions.iter().map(Block::of).collect();
            for (function, body) in module.functions.iter().zip(&bodies) {
                mark_live(*body, &mut live);
                dynamic.extend(function.parameters().iter().map(|param| param.binder.var));
                mark_dynamic(*body, &mut dynamic);
                match_stats.count_block(*body);
            }
            let mut module_counts = BTreeMap::new();
            for body in &bodies {
                count_ops(*body, &live, &dynamic, &mut module_counts);
            }
            for (kind, [generated, live, dynamic]) in module_counts {
                let column: &mut [usize; 4] = counts.entry(kind).or_default();
                column[0] += generated;
                column[1] += 1;
                column[2] += live;
                column[3] += dynamic;
            }
            let mut references = HashMap::new();
            let mut html = HashSet::new();
            for body in &bodies {
                count_html_references(*body, &mut references, &mut html);
            }
            html_bindings += html.len();
            shared_html += html
                .iter()
                .filter(|name| references.get(name).copied().unwrap_or(0) > 1)
                .count();
        }
        let MatchStats {
            arm_counts,
            binding_arms,
        } = match_stats;
        let arms: usize = arm_counts.iter().sum();
        let fewest_arms = arm_counts.iter().min().unwrap();
        let most_arms = arm_counts.iter().max().unwrap();
        let mut report = format!(
            "{} of {INPUT_LEN} input bytes consumed per module on average, at most {most_consumed}\n\
             {exhausted} of {SAMPLES} modules exhaust the input\n\
             {call_depth_limit} of {entrypoints} entrypoints hit the call depth limit\n\
             {array_len_limit} of {entrypoints} entrypoints hit the array length limit\n\
             {string_len_limit} of {entrypoints} entrypoints hit the string length limit\n\
             {html_size_limit} of {entrypoints} entrypoints hit the Html size limit\n\
             {op_count_limit} of {entrypoints} entrypoints hit the op count limit\n\
             {shared_html} of {html_bindings} Html bindings have more than one reference\n\
             {:.1} arms per match on average, at least {fewest_arms} and at most {most_arms}\n\
             {binding_arms} of {arms} match arms bind names\n\n\
             {:<20}{:>10}{:>8}{:>8}{:>8}\n",
            consumed / SAMPLES,
            arms as f64 / arm_counts.len() as f64,
            "op",
            "ops/module",
            "% mods",
            "% live",
            "% dyn"
        );
        for (kind, [generated, modules, live, dynamic]) in counts {
            let per_module = generated as f64 / SAMPLES as f64;
            let modules = 100.0 * modules as f64 / SAMPLES as f64;
            let live = 100.0 * live as f64 / generated as f64;
            let dynamic = 100.0 * dynamic as f64 / generated as f64;
            report.push_str(&format!(
                "{kind:<20}{per_module:>10.1}{modules:>8.1}{live:>8.1}{dynamic:>8.1}\n"
            ));
        }
        expect![[r#"
            8386 of 65536 input bytes consumed per module on average, at most 65536
            3 of 100 modules exhaust the input
            3 of 197 entrypoints hit the call depth limit
            0 of 197 entrypoints hit the array length limit
            0 of 197 entrypoints hit the string length limit
            0 of 197 entrypoints hit the Html size limit
            3 of 197 entrypoints hit the op count limit
            3283 of 20192 Html bindings have more than one reference
            2.4 arms per match on average, at least 2 and at most 13
            11503 of 29486 match arms bind names

            op                  ops/module  % mods  % live   % dyn
            Array                    218.8    88.0    96.5    15.8
            ArrayFromRange             6.0    69.0    98.7     0.0
            ArrayIsEmpty               2.2    37.0    98.1    37.0
            ArrayLength               22.1    73.0    98.6    45.6
            BoolLiteral               62.7    86.0    95.6     0.0
            BoolNegation               2.4    34.0    99.6    54.1
            Call                      33.8    47.0    99.8    93.9
            Enum                     116.2    64.0    95.4    19.8
            Equals                     2.2    35.0    98.2    70.1
            FieldAccess                8.3    36.0    98.8    90.0
            FloatLiteral              62.7    88.0    96.4     0.0
            FloatToInt                22.3    72.0    98.4    54.2
            HtmlConcat                72.8    86.0    99.9    55.9
            HtmlElement               40.2    71.0    99.9    80.1
            HtmlEscape                39.7    80.0    99.9    57.6
            HtmlFor                   12.0    76.0    99.7    69.3
            HtmlText                   1.4    54.0   100.0     0.0
            IntLiteral                73.3    89.0    97.4     0.0
            IntToFloat                22.4    61.0    98.1    51.6
            IntToString               35.8    76.0    99.8    49.6
            LessThan                   2.0    39.0    99.0    60.1
            LessThanOrEqual            2.5    41.0    98.4    61.4
            Match                    123.9    91.0    97.3    80.8
            NumericAdd                45.3    69.0    98.0    65.3
            NumericMultiply           43.8    71.0    98.1    66.7
            NumericNegation           45.2    71.0    98.1    53.7
            NumericSubtract           44.3    70.0    97.9    66.3
            OptionIsNone               2.0    43.0    99.0    56.9
            OptionIsSome               2.3    39.0    99.6    60.5
            OptionNone               157.6    87.0    96.5     0.0
            OptionSome                68.0    85.0    95.9    27.8
            Record                   102.7    66.0    97.3    29.0
            StringConcat              35.9    65.0    99.7    52.2
            StringIsEmpty              2.2    41.0    99.1    52.4
            StringLiteral             65.5    87.0    97.0     0.0
            Tuple                    906.8    91.0    96.7    50.1
            TupleIndex               545.6    89.0    96.7    71.3
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
                  W1 { f0: (Array[String], Float, Option[E1]) },
                }

                enum E1 {
                  W0,
                  W1 { f0: () },
                }

                record R0 {
                  f0: Bool,
                  f1: Array[()],
                  f2: (Float, Int, R1),
                }

                record R1 {
                  f0: ((), Option[R1]),
                  f1: (Option[E1], Float),
                }

                fn C0@f0(p0@v0: Option[()], p1@v1: ((Bool, E1),), p2@v2: E0, p3@v3: Html, p4@v4: Bool, p5@v5: Array[R1]) -> Html {
                  let v9: Int = -2
                  let v10: Int = 1
                  let v11: Array[Int] = v9..=v10
                  let v21: Html = for v12: Int in v11 {
                    let v20: Html = for v13: R1 in v5 {
                      let v19: Html = match v0 {
                        Some(()) => {
                          let v14: Html = text("back\\slash")
                          v14
                        }
                        None => {
                          let v17: String = match v0 {
                            Some(()) => {
                              let v15: String = ""
                              v15
                            }
                            None => {
                              let v16: String = ""
                              v16
                            }
                          }
                          let v18: Html = escape(v17)
                          v18
                        }
                      }
                      v19
                    }
                    v20
                  }
                  v21
                }
                fn C1@f1(p0@v6: Html, p1@v7: (E0, Array[E0], (R1,)), p2@v8: Array[Option[R0]]) -> Html {
                  v6
                }
                entrypoint fn V0@f2() -> Html {
                  let v22: Html = concat()
                  v22
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
                  let v2: Float = 0.2
                  let v7: Int = match v0 {
                    true => {
                      v1
                    }
                    _ => {
                      let v3: String = "`${x}`"
                      let v4: Int = -56
                      let v5: (String, Int) = (v3, v4)
                      let v6: Int = v5.1
                      v6
                    }
                  }
                  let v8: Float = v7.to_float()
                  let v14: Float = match v0 {
                    false => {
                      let v9: Float = 0.5
                      let v10: Float = v9 * v2
                      let v11: Float = v10 + v9
                      v11
                    }
                    true => {
                      let v12: Float = 1.5
                      let v13: Float = -v12
                      v13
                    }
                  }
                  let v15: Float = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005
                  let v16: Float = v8 + v15
                  let v17: Float = v14 - v16
                  let v18: Float = -v17
                  let v19: Float = v15 + v8
                  let v20: Float = -v19
                  let v21: Float = v14 + v20
                  let v22: Float = v18 * v21
                  let v23: Float = v8 - v22
                  let v24: Float = v2 - v23
                  let v25: Int = v24.to_int()
                  let v31: (Array[E0],) = match v0 {
                    true => {
                      let v26: E0 = W0 {f0: v2}
                      let v27: Array[E0] = [v26]
                      let v28: (Array[E0],) = (v27,)
                      v28
                    }
                    _ => {
                      let v29: Array[E0] = []
                      let v30: (Array[E0],) = (v29,)
                      v30
                    }
                  }
                  let v32: Array[E0] = v31.0
                  let v33: Int = v32.len()
                  let v34: Int = v33 + v1
                  let v35: Int = v25 * v34
                  let v36: String = v35.to_string()
                  let v37: Html = escape(v36)
                  v37
                }
                entrypoint fn V1@f1() -> Html {
                  let v38: Html = concat()
                  v38
                }
            "#]],
        );
    }
}
