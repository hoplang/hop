use crate::hop::typing::Ty;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_program::FlatProgram;
use crate::ir::flat_program_builder::{
    DeclaredFunction, FlatBlockBuilder, FlatProgramBuilder, Pat,
};
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_type::{
    IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrResultType, IrSignature,
    IrType,
};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
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

/// How often a block binds a value nothing asked for.
const DEAD_BINDING_ODDS: u8 = 10;

/// How often a value that a block that renders Html binds is a cell.
const CELL_ODDS: u8 = 64;

/// How often an element has a click.
const CLICK_ODDS: u8 = 204;

/// How often a loop that binds its variable has a key.
const KEY_ODDS: u8 = 204;

/// How often an op that renders Html may be a toggle: a cell, a match on
/// it and a click that writes it.
const TOGGLE_ODDS: u8 = 64;

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

#[derive(Clone, Copy)]
struct Name {
    var: ValueId,
    /// Whether the path has used the Html.
    used: bool,
    /// Whether a cell binds the name, which a cell write can then write.
    cell: bool,
}

/// The names a path can read.
#[derive(Clone, Default)]
struct Scope {
    names: Vec<Name>,
    html: bool,
}

impl Scope {
    fn push(&mut self, value: ValueId) -> ValueId {
        self.push_name(value, false)
    }

    fn push_cell(&mut self, var: ValueId) -> ValueId {
        self.push_name(var, true)
    }

    fn push_name(&mut self, var: ValueId, cell: bool) -> ValueId {
        self.names.push(Name {
            var,
            used: false,
            cell,
        });
        var
    }

    fn set_used(&mut self, value: ValueId, used: bool) {
        if let Some(name) = self.names.iter_mut().rev().find(|name| name.var == value) {
            name.used = used;
        }
    }

    fn inner(&self, types: &[IrType], html: bool) -> Scope {
        Scope {
            names: (self.names.iter().copied())
                .filter(|name| types[name.var.index()] != IrType::HTML)
                .collect(),
            html,
        }
    }

    // `arm` is this scope with names pushed after it forked, and after the
    // fork a name is used when any path used it.
    fn join(&mut self, arm: &Scope) {
        for (name, arm_name) in self.names.iter_mut().zip(&arm.names) {
            name.used |= arm_name.used;
        }
    }

    fn names_where<'a>(
        &'a self,
        types: &'a [IrType],
        fits: impl Fn(&IrType) -> bool + 'a,
    ) -> impl Iterator<Item = ValueId> + 'a {
        self.names
            .iter()
            .filter(move |name| !name.used && fits(&types[name.var.index()]))
            .map(|name| name.var)
    }

    fn cells(&self) -> impl Iterator<Item = ValueId> + '_ {
        (self.names.iter())
            .filter(|name| name.cell)
            .map(|name| name.var)
    }
}

/// Which named types may be referenced at a type position.
#[derive(Clone, Copy)]
enum NamedTypes {
    /// Every name the program declares, including declarations that are
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
    records: Vec<IrDataType>,
    types: IrTypeDeclarations,
}

pub fn random_program(u: &mut Unstructured<'_>) -> FlatProgram {
    random_program_inner(u, false)
}

pub fn random_program_with_test_view(u: &mut Unstructured<'_>) -> FlatProgram {
    random_program_inner(u, true)
}

fn random_program_inner(u: &mut Unstructured<'_>, single_test_entrypoint: bool) -> FlatProgram {
    let mut g = FlatGenerator {
        u,
        declared_names: Vec::new(),
        record_names: Vec::new(),
        enum_names: Vec::new(),
        records: Vec::new(),
        types: IrTypeDeclarations::default(),
    };

    // Name every type before generating any field types, so that a field can
    // refer to a type declared later or to the one being declared.
    let record_count = g.count(0..=3);
    let enum_count = g.count(0..=3);
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut types = IrTypeDeclarationsBuilder::new();

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

    g.types = types.build();
    g.records = g
        .record_names
        .iter()
        .map(|name| g.data_type(Ty::named(name)))
        .collect();
    let mut builder = FlatProgramBuilder::with_types(g.types.clone());

    // Declare every function before defining any, so that a body can call a
    // function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let html = g.coin();
        let return_type = if html {
            IrType::HTML
        } else {
            IrType::result(&g.random_result_type())
        };
        let params: Vec<(String, IrType)> = (0..g.count(0..=8))
            .map(|i| {
                let typ = if g.index(4) == 0 {
                    g.random_closure_type(html, 1)
                } else if g.index(8) == 0 {
                    IrType::EFFECT
                } else {
                    let ty = g.random_type_where(2, NamedTypes::Any, html);
                    g.ir_type(ty)
                };
                (format!("p{i}"), typ)
            })
            .collect();
        builder = builder.declare_with_types(
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
                    .map(|i| {
                        (
                            format!("p{i}"),
                            g.random_type_where(2, NamedTypes::Any, false),
                        )
                    })
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
            g.block_of_type(b, &mut scope, &declared.return_type, MAX_BLOCK_DEPTH)
        });
    }
    for (name, params) in &entrypoints {
        builder = builder.entrypoint(
            name,
            params.iter().map(|(n, t)| (n.as_str(), t.clone())),
            |b| {
                let mut scope = Scope::default();
                for (n, _) in params {
                    scope.push(b.param(n));
                }
                g.html_block(b, &mut scope, MAX_BLOCK_DEPTH)
            },
        );
    }
    builder.build().unwrap()
}

pub fn random_entrypoint_args(
    program: &FlatProgram,
    rng: &mut impl Rng,
) -> Vec<(EntrypointId, HashMap<AttributeName, Value<'static>>)> {
    program
        .entrypoints
        .iter_enumerated()
        .map(|(id, decl)| {
            let args = decl
                .parameters
                .iter()
                .map(|param| {
                    let value = random_value(rng, &param.binder.typ, &program.types);
                    (param.name.clone(), value)
                })
                .collect();
            (id, args)
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

    fn chance(&mut self, odds: u8) -> bool {
        self.u.ratio(odds, u8::MAX).unwrap()
    }

    fn zigzag(&mut self, magnitude: i32) -> i32 {
        let n = self.u.int_in_range(0..=2 * magnitude).unwrap();
        if n % 2 == 0 { -n / 2 } else { n / 2 + 1 }
    }

    fn ir_type(&self, ty: Ty) -> IrType {
        self.types.ty(ty)
    }

    fn data_type(&self, ty: Ty) -> IrDataType {
        let IrType::Data(IrFunctionDataType::Data(typ)) = self.ir_type(ty) else {
            panic!("a value has a data type");
        };
        typ
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
        target: &'s IrDataType,
    ) -> impl Iterator<Item = (&'s IrDataType, &'s IrField)> + 's {
        let types = b.types();
        self.records.iter().flat_map(move |record| {
            let IrDataType::Record(name) = record else {
                unreachable!("a record of the program has a Record type")
            };
            types
                .record(name)
                .expect("a record of the program is declared")
                .fields
                .iter()
                .filter(move |field| field.typ == *target)
                .map(move |field| (record, field))
        })
    }

    fn callees<'t>(
        b: &FlatBlockBuilder<'t>,
        target: &'t IrType,
    ) -> impl Iterator<Item = &'t DeclaredFunction> + 't {
        b.functions()
            .iter()
            .filter(move |function| function.return_type == *target)
    }

    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&IrDataType) -> bool,
    ) -> Option<(ValueId, IrDataType)> {
        let types = b.value_types();
        let fits = |typ: &IrType| typ.as_data().is_some_and(&fits);
        if scope.names_where(&types, fits).next().is_none() || self.index(10) == 0 {
            return None;
        }
        let count = scope.names_where(&types, fits).count();
        let index = self.index(count);
        let name = scope.names_where(&types, fits).nth(index).unwrap();
        let IrType::Data(IrFunctionDataType::Data(typ)) = &types[name.index()] else {
            unreachable!()
        };
        Some((name, typ.clone()))
    }

    fn random_closure_type(&mut self, html: bool, depth: usize) -> IrType {
        if html && self.coin() {
            IrType::Html(IrFunctionHtmlType::Function(
                self.random_html_signature(depth),
            ))
        } else {
            IrType::Data(IrFunctionDataType::Function(
                self.random_value_signature(depth, None),
            ))
        }
    }

    fn random_value_signature(
        &mut self,
        depth: usize,
        result: Option<IrResultType>,
    ) -> IrSignature<IrFunctionDataType, IrResultType> {
        let parameters = (0..self.count(0..=2))
            .map(|_| {
                if depth > 0 && self.index(4) == 0 {
                    IrFunctionDataType::Function(self.random_value_signature(depth - 1, None))
                } else if self.index(8) == 0 {
                    IrFunctionDataType::Effect(IrDataType::UNIT)
                } else {
                    IrFunctionDataType::Data(self.random_data_type(1))
                }
            })
            .collect();
        let result = result.unwrap_or_else(|| self.random_result_type());
        IrSignature { parameters, result }
    }

    fn random_result_type(&mut self) -> IrResultType {
        if self.index(4) == 0 {
            IrResultType::Effect(IrDataType::UNIT)
        } else {
            IrResultType::Data(self.random_data_type(1))
        }
    }

    fn random_html_signature(&mut self, depth: usize) -> IrSignature<IrType, IrHtmlType> {
        let parameters = (0..self.count(0..=2))
            .map(|_| match self.index(if depth > 0 { 4 } else { 2 }) {
                0 => IrType::Data(IrFunctionDataType::Data(self.random_data_type(1))),
                1 => IrType::HTML,
                2 => self.random_closure_type(false, depth - 1),
                _ => self.random_closure_type(true, depth - 1),
            })
            .collect();
        IrSignature {
            parameters,
            result: IrHtmlType,
        }
    }

    fn random_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Any, true)
    }

    fn random_data_type(&mut self, depth: usize) -> IrDataType {
        let ty = self.random_type_where(depth, NamedTypes::Any, false);
        self.data_type(ty)
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

    fn for_loop(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        array: ValueId,
        depth: usize,
    ) -> ValueId {
        if !self.chance(KEY_ODDS) {
            return b.html_for(array, |b, var| {
                self.for_loop_body(b, scope, Some(var), depth - 1)
            });
        }
        b.html_for_keyed(array, |b, var| self.key(b, scope, var, depth - 1))
            .body(|b, var| self.for_loop_body(b, scope, Some(var), depth - 1))
    }

    fn key(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        var: ValueId,
        depth: usize,
    ) -> ValueId {
        let mut scope = scope.inner(&b.value_types(), false);
        scope.push(var);
        let typ = b.value_types()[var.index()].clone();
        if (typ == IrType::INT || typ == IrType::STRING) && self.index(4) != 0 {
            return var;
        }
        let target = if self.coin() {
            IrDataType::Int
        } else {
            IrDataType::String
        };
        self.data_op(b, &mut scope, &target, depth, MAX_EXPRESSION_DEPTH)
    }

    fn for_loop_body(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        var: Option<ValueId>,
        depth: usize,
    ) -> ValueId {
        let mut scope = scope.inner(&b.value_types(), true);
        if let Some(var) = var {
            scope.push(var);
        }
        self.html_block(b, &mut scope, depth)
    }

    fn html_block(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        scope.html = true;
        if self.chance(DEAD_BINDING_ODDS) {
            let ty = self.random_type(1);
            let typ = self.ir_type(ty);
            let dead = self.op_of_type(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
            // Html that nothing has read yet, which a later op may read.
            scope.set_used(dead, false);
        }
        self.html_op(b, scope, depth, MAX_EXPRESSION_DEPTH)
    }

    fn data_block(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &IrDataType,
        depth: usize,
    ) -> ValueId {
        scope.html = false;
        if self.chance(DEAD_BINDING_ODDS) {
            let typ = self.random_data_type(1);
            self.data_op(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
        }
        self.data_op(b, scope, target, depth, MAX_EXPRESSION_DEPTH)
    }

    fn block_of_type(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrType,
        depth: usize,
    ) -> ValueId {
        match typ {
            IrType::Data(IrFunctionDataType::Data(target)) => {
                self.data_block(b, scope, target, depth)
            }
            IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => self.html_block(b, scope, depth),
            IrType::Data(IrFunctionDataType::Effect(_)) => self.effect_block(b, scope, depth),
            IrType::Data(IrFunctionDataType::Function(_))
            | IrType::Html(IrFunctionHtmlType::Function(_)) => {
                unreachable!("a block produces no closure")
            }
        }
    }

    fn op_of_type(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        match typ {
            IrType::Data(IrFunctionDataType::Data(target)) => {
                self.data_op(b, scope, target, depth, nesting)
            }
            IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                self.html_op(b, scope, depth, nesting)
            }
            IrType::Data(IrFunctionDataType::Effect(_)) => self.effect_op(b, scope, depth, nesting),
            IrType::Data(IrFunctionDataType::Function(_))
            | IrType::Html(IrFunctionHtmlType::Function(_)) => {
                self.closure_op(b, scope, typ, depth, nesting)
            }
        }
    }

    // The body of a lambda goes on with the expression, a level further
    // from its leaves, rather than start a block afresh, so that lambdas in
    // lambdas stop at the leaves.
    fn closure_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(b, scope, typ) {
            return name;
        }
        let parameters: Vec<IrType> = match typ {
            IrType::Data(IrFunctionDataType::Function(signature)) => {
                (signature.parameters.iter().cloned())
                    .map(IrType::Data)
                    .collect()
            }
            IrType::Html(IrFunctionHtmlType::Function(signature)) => signature.parameters.clone(),
            IrType::Data(IrFunctionDataType::Data(_) | IrFunctionDataType::Effect(_))
            | IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                unreachable!("closure_op makes closures")
            }
        };
        let mut inner = scope.inner(&b.value_types(), matches!(typ, IrType::Html(_)));
        let depth = depth.saturating_sub(1);
        let nesting = nesting.saturating_sub(1);
        let lambda = b.lambda(parameters, |b, params| {
            for param in params {
                inner.push(*param);
            }
            match typ {
                IrType::Data(IrFunctionDataType::Function(signature)) => match &signature.result {
                    IrResultType::Data(result) => {
                        self.data_op(b, &mut inner, result, depth, nesting)
                    }
                    IrResultType::Effect(_) => self.effect_op(b, &mut inner, depth, nesting),
                },
                _ => self.html_op(b, &mut inner, depth, nesting),
            }
        });
        scope.push(lambda)
    }

    fn apply(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        result: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let fits = |typ: &IrType| match (typ, result) {
            (
                IrType::Data(IrFunctionDataType::Function(signature)),
                IrType::Data(IrFunctionDataType::Data(result)),
            ) => signature.result == IrResultType::Data(result.clone()),
            (
                IrType::Data(IrFunctionDataType::Function(signature)),
                IrType::Data(IrFunctionDataType::Effect(payload)),
            ) => signature.result == IrResultType::Effect(payload.clone()),
            (
                IrType::Html(IrFunctionHtmlType::Function(_)),
                IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)),
            ) => true,
            _ => false,
        };
        let fitting: Vec<ValueId> = scope.names_where(&b.value_types(), fits).collect();
        let closure = if !fitting.is_empty() && self.coin() {
            fitting[self.index(fitting.len())]
        } else {
            let typ = match result {
                IrType::Data(IrFunctionDataType::Data(result)) => {
                    IrType::Data(IrFunctionDataType::Function(
                        self.random_value_signature(1, Some(IrResultType::Data(result.clone()))),
                    ))
                }
                IrType::Data(IrFunctionDataType::Effect(payload)) => IrType::Data(
                    IrFunctionDataType::Function(
                        self.random_value_signature(1, Some(IrResultType::Effect(payload.clone()))),
                    ),
                ),
                _ => IrType::Html(IrFunctionHtmlType::Function(self.random_html_signature(1))),
            };
            self.closure_op(b, scope, &typ, depth, nesting - 1)
        };
        let parameters: Vec<IrType> = match &b.value_types()[closure.index()] {
            IrType::Data(IrFunctionDataType::Function(signature)) => {
                (signature.parameters.iter().cloned())
                    .map(IrType::Data)
                    .collect()
            }
            IrType::Html(IrFunctionHtmlType::Function(signature)) => signature.parameters.clone(),
            IrType::Data(IrFunctionDataType::Data(_) | IrFunctionDataType::Effect(_))
            | IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                unreachable!("an application applies a closure")
            }
        };
        let args: Vec<ValueId> = (parameters.iter())
            .map(|parameter| self.op_of_type(b, scope, parameter, depth, nesting - 1))
            .collect();
        b.apply(closure, args)
    }

    fn name_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        typ: &IrType,
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
        let value = match self.name_in_scope(b, scope, &IrType::HTML) {
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
                        self.element(b, scope, tag, Vec::new(), children, 0, 0)
                    }
                };
                scope.push(literal)
            }
        };
        scope.set_used(value, true);
        value
    }

    fn element(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        tag: &str,
        attributes: Vec<(AttributeName, IrAttribute)>,
        children: ValueId,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if !self.chance(CLICK_ODDS) {
            return b.element(tag, attributes, children);
        }
        let handler_type = IrType::data_function([], IrType::EFFECT);
        let handler = self.op_of_type(b, scope, &handler_type, depth, nesting);
        b.element_on_click(tag, attributes, handler, children)
    }

    fn data_leaf(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrDataType,
        depth: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(
            b,
            scope,
            &IrType::Data(IrFunctionDataType::Data(typ.clone())),
        ) {
            return self.cell(b, scope, name);
        }
        self.literal(b, scope, typ, depth)
    }

    fn literal(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrDataType,
        depth: usize,
    ) -> ValueId {
        let literal = match typ {
            IrDataType::String => b.str(self.u.choose(STRING_LITERALS).unwrap()),
            IrDataType::Int => b.int(match self.index(8) {
                0 => *self
                    .u
                    .choose(&[i32::MIN, i32::MIN + 1, i32::MAX - 1, i32::MAX])
                    .unwrap(),
                1 | 2 => self.zigzag(100),
                _ => self.zigzag(2),
            }),
            IrDataType::Bool => b.bool(self.coin()),
            IrDataType::Float => b.float(*self.u.choose(FLOATS).unwrap()),
            IrDataType::Array(inner) => {
                let len = if depth == 0 { 0 } else { self.count(0..=3) };
                let elements: Vec<_> = (0..len)
                    .map(|_| self.data_leaf(b, scope, inner, depth.saturating_sub(1)))
                    .collect();
                if elements.is_empty() {
                    b.empty_array(IrType::Data(IrFunctionDataType::Data((**inner).clone())))
                } else {
                    b.array(elements)
                }
            }
            IrDataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.data_leaf(b, scope, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none(IrType::Data(IrFunctionDataType::Data((**inner).clone())))
                }
            }
            IrDataType::Record(name) => {
                let fields = &b.types().record(name).unwrap().fields;
                let fields = self.shuffled(fields.iter().collect());
                let mut values = Vec::new();
                for field in fields {
                    values.push((
                        field.name.as_str(),
                        self.data_leaf(b, scope, &field.typ, depth.saturating_sub(1)),
                    ));
                }
                b.record(IrType::Data(IrFunctionDataType::Data(typ.clone())), values)
            }
            IrDataType::Enum(name) => {
                let variants = &b.types().enumeration(name).unwrap().variants;
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
                    IrType::Data(IrFunctionDataType::Data(typ.clone())),
                    variant.name.as_str(),
                    values,
                )
            }
            IrDataType::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|element| self.data_leaf(b, scope, element, depth.saturating_sub(1)))
                    .collect();
                b.tuple(values)
            }
        };
        self.bind(b, scope, literal)
    }

    fn constructors(
        types: &IrTypeDeclarations,
        typ: &IrDataType,
    ) -> Vec<(Ctor, Vec<(String, IrDataType)>)> {
        let fields = |fields: &[IrField]| {
            fields
                .iter()
                .map(|field| (field.name.as_str().to_string(), field.typ.clone()))
                .collect()
        };
        match typ {
            IrDataType::Bool => vec![(Ctor::True, Vec::new()), (Ctor::False, Vec::new())],
            IrDataType::Option(inner) => vec![
                (Ctor::None, Vec::new()),
                (Ctor::Some, vec![("0".to_string(), (**inner).clone())]),
            ],
            IrDataType::Tuple(elements) => vec![(
                Ctor::Tuple,
                elements
                    .iter()
                    .enumerate()
                    .map(|(i, element)| (i.to_string(), element.clone()))
                    .collect(),
            )],
            IrDataType::Record(name) => {
                let record = types.record(name).expect("a record is declared");
                vec![(Ctor::Record, fields(&record.fields))]
            }
            IrDataType::Enum(name) => types
                .enumeration(name)
                .expect("an enum is declared")
                .variants
                .iter()
                .map(|variant| {
                    let name = variant.name.as_str().to_string();
                    (Ctor::Variant(name), fields(&variant.fields))
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    fn can_branch(types: &IrTypeDeclarations, typ: &IrDataType, depth: usize) -> bool {
        if depth == 0 {
            return false;
        }
        let ctors = Self::constructors(types, typ);
        ctors.len() > 1
            || ctors
                .iter()
                .flat_map(|(_, fields)| fields)
                .any(|(_, field)| Self::can_branch(types, field, depth - 1))
    }

    fn partition(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        typ: &IrDataType,
        depth: usize,
        forced: bool,
        next_bind: &mut usize,
    ) -> Vec<Arm> {
        let ctors = Self::constructors(b.types(), typ);
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
                    .filter(|&i| Self::can_branch(b.types(), &fields[i].1, depth - 1))
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

    fn branching_type(&mut self, types: &IrTypeDeclarations) -> IrDataType {
        let named: Vec<IrDataType> = self
            .declared_names
            .iter()
            .map(|name| self.data_type(Ty::named(name)))
            .filter(|typ| Self::can_branch(types, typ, MAX_PATTERN_DEPTH))
            .collect();
        let base = match self.index(2 + named.len()) {
            0 => IrDataType::Bool,
            1 => IrDataType::Option(Box::new(self.random_data_type(1))),
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
        IrDataType::Tuple(elements)
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
            Apply,
            Toggle,
        }
        let mut productions = vec![P::Escape, P::Element, P::Concat];
        if Self::callees(b, &IrType::HTML).next().is_some() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::ForArray, P::ForRange, P::Apply]);
        }
        if depth > 0 && scope.html && self.chance(TOGGLE_ODDS) {
            productions.push(P::Toggle);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::Escape => {
                let operand = self.data_op(b, scope, &IrDataType::String, depth, nesting - 1);
                b.escape(operand)
            }
            P::Element => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.data_op(b, scope, &IrDataType::String, depth, nesting - 1);
                        attributes.push(b.attr_value(name, value));
                    }
                }
                if self.coin() {
                    let present = self.data_op(b, scope, &IrDataType::Bool, depth, nesting - 1);
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
                self.element(b, scope, tag, attributes, children, depth, nesting - 1)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=4))
                    .map(|_| self.html_op(b, scope, depth, nesting - 1))
                    .collect();
                b.html_concat(parts)
            }
            P::Toggle => self.toggle(b, scope, depth),
            P::Call => self.call(b, scope, &IrType::HTML, depth, nesting),
            P::Match => self.match_(b, scope, &IrType::HTML, depth, nesting),
            P::Apply => self.apply(b, scope, &IrType::HTML, depth, nesting),
            P::ForArray => {
                let var = self.var_in_scope(b, scope, |ty| matches!(ty, IrDataType::Array(_)));
                let array = match var {
                    Some((array, _)) => array,
                    None => {
                        let array_ty = IrDataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    }
                };
                let array = if self.index(4) == 0 {
                    let enumerated = b.array_enumerate(array);
                    self.bind(b, scope, enumerated)
                } else {
                    array
                };
                self.for_loop(b, scope, array, depth)
            }
            P::ForRange => {
                let start = b.int(self.zigzag(3));
                let start = self.bind(b, scope, start);
                let end = b.int(self.zigzag(3));
                let end = self.bind(b, scope, end);
                let array = b.array_from_range(start, end);
                let array = self.bind(b, scope, array);
                if self.coin() {
                    self.for_loop(b, scope, array, depth)
                } else {
                    b.html_for_unbound(array, |b| self.for_loop_body(b, scope, None, depth - 1))
                }
            }
        };
        scope.push(value);
        scope.set_used(value, true);
        value
    }

    // A cell, a click handler that writes a literal to it and a match on it
    // that renders Html, in an element with the handler. The handler is in
    // scope for the arms, so a click on an element of an arm can take the
    // element away. A handler writing a value that a match on the cell gives
    // instead, or the handler only in scope and not around the match, makes
    // far fewer clicks switch an arm.
    fn toggle(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        let subject_ty = self.branching_type(b.types());
        let initial = self.literal(b, scope, &subject_ty, MAX_LITERAL_DEPTH);
        let cell = scope.push_cell(b.cell(initial));
        let mut inner = scope.inner(&b.value_types(), false);
        let handler = b.lambda(Vec::new(), |b, _| {
            let value = self.literal(b, &mut inner, &subject_ty, MAX_LITERAL_DEPTH);
            b.cell_write(cell, value)
        });
        let handler = scope.push(handler);
        let shown = self.match_on(b, scope, cell, subject_ty, &IrType::HTML, depth);
        scope.push(shown);
        scope.set_used(shown, true);
        let tags: Vec<&str> = (ELEMENTS.iter())
            .filter(|(_, void)| !void)
            .map(|(tag, _)| *tag)
            .collect();
        b.element_on_click(self.u.choose(&tags).unwrap(), Vec::new(), handler, shown)
    }

    fn data_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &IrDataType,
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
            ArrayGet,
            ArrayEnumerate,
            ArrayFromRange,
            TupleIndex,
            Neg,
            Add,
            Sub,
            Mul,
            Call,
            Apply,
        }
        let typ = IrType::Data(IrFunctionDataType::Data(target.clone()));
        let mut productions: Vec<P> = Vec::new();
        if self.record_fields_of_type(b, target).next().is_some() {
            productions.push(P::FieldAccess);
        }
        productions.push(P::TupleIndex);
        if *target == IrDataType::Bool {
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
        if *target == IrDataType::Int {
            productions.extend([
                P::ArrayLength,
                P::Neg,
                P::Add,
                P::Sub,
                P::Mul,
                P::FloatToInt,
            ]);
        }
        if *target == IrDataType::Float {
            productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::IntToFloat]);
        }
        if matches!(target, IrDataType::Array(element) if **element == IrDataType::Int) {
            productions.push(P::ArrayFromRange);
        }
        if matches!(target, IrDataType::Option(_)) {
            productions.push(P::ArrayGet);
        }
        if matches!(target, IrDataType::Array(pair)
            if matches!(pair.as_ref(), IrDataType::Tuple(types)
                if matches!(types.as_slice(), [IrDataType::Int, _])))
        {
            productions.push(P::ArrayEnumerate);
        }
        if *target == IrDataType::String {
            productions.extend([P::Concat, P::IntToString]);
        }
        if Self::callees(b, &typ).next().is_some() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::Apply]);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::FieldAccess => {
                let record_fields: Vec<(IrDataType, String)> = self
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
                    0 => IrDataType::Int,
                    1 => IrDataType::String,
                    2 => IrDataType::Bool,
                    _ => IrDataType::Float,
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() {
                    IrDataType::Int
                } else {
                    IrDataType::Float
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() {
                    IrDataType::Int
                } else {
                    IrDataType::Float
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.data_op(b, scope, &IrDataType::Bool, depth, nesting - 1);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=3))
                    .map(|_| self.data_op(b, scope, &IrDataType::String, depth, nesting - 1))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.data_op(b, scope, &IrDataType::Int, depth, nesting - 1);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.data_op(b, scope, &IrDataType::Float, depth, nesting - 1);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.data_op(b, scope, &IrDataType::Int, depth, nesting - 1);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, IrDataType::Option(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = IrDataType::Option(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, IrDataType::Option(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let option_ty = IrDataType::Option(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &option_ty, depth, nesting - 1)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.data_op(b, scope, &IrDataType::String, depth, nesting - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, IrDataType::Array(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = IrDataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, scope, |ty| matches!(ty, IrDataType::Array(_)))
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| {
                        let array_ty = IrDataType::Array(Box::new(self.random_data_type(1)));
                        self.data_op(b, scope, &array_ty, depth, nesting - 1)
                    });
                b.array_length(operand)
            }
            P::ArrayGet => {
                let IrDataType::Option(element) = target else {
                    unreachable!("ArrayGet is offered for an Option only")
                };
                let array_ty = IrDataType::Array(element.clone());
                let array = self
                    .var_in_scope(b, scope, |ty| *ty == array_ty)
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| self.data_op(b, scope, &array_ty, depth, nesting - 1));
                let index = if self.coin() {
                    let index = b.int(self.zigzag(3));
                    self.bind(b, scope, index)
                } else {
                    self.data_op(b, scope, &IrDataType::Int, depth, nesting - 1)
                };
                b.array_get(array, index)
            }
            P::ArrayEnumerate => {
                let IrDataType::Array(pair) = target else {
                    unreachable!("ArrayEnumerate is offered for an Array only")
                };
                let IrDataType::Tuple(types) = pair.as_ref() else {
                    unreachable!("ArrayEnumerate is offered for an Array of pairs only")
                };
                let array_ty = IrDataType::Array(Box::new(types[1].clone()));
                let array = self
                    .var_in_scope(b, scope, |ty| *ty == array_ty)
                    .map(|(var, _)| var)
                    .unwrap_or_else(|| self.data_op(b, scope, &array_ty, depth, nesting - 1));
                b.array_enumerate(array)
            }
            P::ArrayFromRange => {
                let start = b.int(self.zigzag(3));
                let start = self.bind(b, scope, start);
                let end = b.int(self.zigzag(3));
                let end = self.bind(b, scope, end);
                b.array_from_range(start, end)
            }
            P::TupleIndex => {
                let var = self.var_in_scope(
                    b,
                    scope,
                    |ty| matches!(ty, IrDataType::Tuple(elements) if elements.contains(target)),
                );
                match var {
                    Some((tuple, tuple_ty)) => {
                        let IrDataType::Tuple(elements) = tuple_ty else {
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
                        let tuple = self.data_op(
                            b,
                            scope,
                            &IrDataType::Tuple(elements),
                            depth,
                            nesting - 1,
                        );
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
            P::Apply => self.apply(b, scope, &typ, depth, nesting),
        };
        self.bind(b, scope, value)
    }

    fn bind(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, value: ValueId) -> ValueId {
        scope.push(value);
        self.cell(b, scope, value)
    }

    fn cell(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, value: ValueId) -> ValueId {
        if !scope.html || !self.chance(CELL_ODDS) {
            return value;
        }
        scope.push_cell(b.cell(value))
    }

    fn effect_block(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        depth: usize,
    ) -> ValueId {
        scope.html = false;
        self.effect_op(b, scope, depth, MAX_EXPRESSION_DEPTH)
    }

    fn cell_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
    ) -> Option<(ValueId, IrDataType)> {
        let cells: Vec<ValueId> = scope.cells().collect();
        if cells.is_empty() {
            return None;
        }
        let cell = cells[self.index(cells.len())];
        let typ = b.value_types()[cell.index()]
            .as_data()
            .expect("a cell holds a value")
            .clone();
        Some((cell, typ))
    }

    fn effect_leaf(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope) -> ValueId {
        if self.coin()
            && let Some(name) = self.name_in_scope(b, scope, &IrType::EFFECT)
        {
            return name;
        }
        let leaf = match self.cell_in_scope(b, scope) {
            Some((cell, typ)) if self.index(4) != 0 => {
                let value = self.literal(b, scope, &typ, MAX_LITERAL_DEPTH);
                b.cell_write(cell, value)
            }
            _ => b.effect_pure(b.tuple([])),
        };
        scope.push(leaf)
    }

    fn effect_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if nesting == 0 || self.index(3) == 0 {
            return self.effect_leaf(b, scope);
        }
        enum P {
            CellWrite,
            CellRead,
            Bind,
            Call,
            Match,
            Apply,
        }
        let mut productions = vec![P::Bind];
        if scope.cells().next().is_some() {
            productions.extend([P::CellWrite, P::CellRead]);
        }
        if Self::callees(b, &IrType::EFFECT).next().is_some() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::Apply]);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::CellWrite => {
                let (cell, typ) = self
                    .cell_in_scope(b, scope)
                    .expect("a cell write is made when a cell is in scope");
                let value = self.data_op(b, scope, &typ, depth, nesting - 1);
                b.cell_write(cell, value)
            }
            P::CellRead => {
                let (cell, typ) = self
                    .cell_in_scope(b, scope)
                    .expect("a cell read is made when a cell is in scope");
                let read = b.cell_read(cell);
                let parameter = IrType::Data(IrFunctionDataType::Data(typ));
                self.effect_bind(b, scope, read, parameter, depth, nesting)
            }
            P::Bind => {
                let first = self.effect_op(b, scope, depth, nesting - 1);
                let unit = IrType::Data(IrFunctionDataType::Data(IrDataType::UNIT));
                self.effect_bind(b, scope, first, unit, depth, nesting)
            }
            P::Call => self.call(b, scope, &IrType::EFFECT, depth, nesting),
            P::Match => self.match_(b, scope, &IrType::EFFECT, depth, nesting),
            P::Apply => self.apply(b, scope, &IrType::EFFECT, depth, nesting),
        };
        scope.push(value)
    }

    fn effect_bind(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        effect: ValueId,
        parameter: IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let mut inner = scope.inner(&b.value_types(), false);
        let continuation = b.lambda([parameter], |b, params| {
            inner.push(params[0]);
            self.effect_op(b, &mut inner, depth, nesting - 1)
        });
        b.effect_bind(effect, continuation)
    }

    fn call(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let callees: Vec<_> = Self::callees(b, target).collect();
        let callee = *self.u.choose(&callees).unwrap();
        let args: Vec<_> = callee
            .parameters
            .iter()
            .map(|parameter| self.op_of_type(b, scope, &parameter.binder.typ, depth, nesting - 1))
            .collect();
        b.call(callee.function.name.as_str(), args)
    }

    fn match_(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let types = b.types();
        let var = self.var_in_scope(b, scope, |ty| {
            Self::can_branch(types, ty, MAX_PATTERN_DEPTH)
        });
        let (subject, subject_ty) = match var {
            Some(pair) => pair,
            None => {
                let subject_ty = self.branching_type(types);
                (
                    self.data_op(b, scope, &subject_ty, depth, nesting - 1),
                    subject_ty,
                )
            }
        };
        self.match_on(b, scope, subject, subject_ty, target, depth)
    }

    fn match_on(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        subject: ValueId,
        subject_ty: IrDataType,
        target: &IrType,
        depth: usize,
    ) -> ValueId {
        let arms = self.partition(b, &subject_ty, MAX_PATTERN_DEPTH, true, &mut 0);
        let mut joined = scope.clone();
        let value = b.match_(subject, |m| {
            for (pat, binds) in arms {
                m.arm(pat, |b, bindings| {
                    let depth = depth - 1;
                    let mut arm = scope.clone();
                    for name in binds {
                        arm.push(bindings.binding(&name));
                    }
                    let body = self.block_of_type(b, &mut arm, target, depth);
                    joined.join(&arm);
                    body
                });
            }
        });
        *scope = joined;
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::flat_program::{
        FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatPureBlock, FlatPureOp,
    };
    use crate::ir::ir_binder::IrBinder;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::clicks::clicks;
    use crate::ir::runtime::flat_evaluator::invoke_entrypoint;
    use crate::ir::runtime::html_node::Html;
    use crate::ir::runtime::state::{Place, State, Step};
    use expect_test::{Expect, expect};
    use rand::{RngExt, SeedableRng, rngs::SmallRng};
    use std::collections::{BTreeMap, BTreeSet, HashSet};

    fn check(seed: u64, len: usize, expected: Expect) {
        let mut rng = SmallRng::seed_from_u64(seed);
        let bytes: Vec<u8> = (0..len).map(|_| rng.random()).collect();
        let program = random_program(&mut Unstructured::new(&bytes));
        expected.assert_eq(&format!("{}\n{program}", program.types));
    }

    #[derive(Clone, Copy)]
    enum Block<'a> {
        Pure(&'a FlatPureBlock),
        Html(&'a FlatHtmlBlock),
    }

    #[derive(Clone, Copy)]
    enum Binding<'a> {
        Pure(&'a IrBinding<IrFunctionDataType, FlatPureOp>),
        Cell(&'a IrBinder<IrDataType>, ValueId),
        Html(&'a IrBinding<IrFunctionHtmlType, FlatHtmlOp>),
    }

    impl<'a> Block<'a> {
        fn of(function: &'a FlatFunction) -> Self {
            match function {
                FlatFunction::Pure(function) => Block::Pure(&function.body),
                FlatFunction::Html(function) => Block::Html(&function.body),
            }
        }

        fn result(self) -> ValueId {
            match self {
                Block::Pure(block) => block.result,
                Block::Html(block) => block.result,
            }
        }

        fn bindings(self) -> Vec<Binding<'a>> {
            match self {
                Block::Pure(block) => block.bindings.iter().map(Binding::Pure).collect(),
                Block::Html(block) => (block.bindings.iter())
                    .map(|binding| match binding {
                        FlatHtmlBinding::Pure(binding) => Binding::Pure(binding),
                        FlatHtmlBinding::Cell(binder, initial) => Binding::Cell(binder, *initial),
                        FlatHtmlBinding::Html(binding) => Binding::Html(binding),
                    })
                    .collect(),
            }
        }
    }

    impl<'a> Binding<'a> {
        fn var(self) -> ValueId {
            match self {
                Binding::Pure(binding) => binding.binder.var,
                Binding::Cell(binder, _) => binder.var,
                Binding::Html(binding) => binding.binder.var,
            }
        }

        fn for_each_operand(self, f: &mut impl FnMut(ValueId)) {
            match self {
                Binding::Pure(binding) => binding.op.for_each_operand(f),
                Binding::Cell(_, initial) => f(initial),
                Binding::Html(binding) => binding.op.for_each_operand(f),
            }
        }

        fn kind(self) -> String {
            let debug = match self {
                Binding::Pure(binding) => format!("{:?}", binding.op),
                Binding::Cell(..) => return "Cell".to_string(),
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
                Binding::Pure(binding)
                    if matches!(binding.binder.typ, IrFunctionDataType::Effect(_))
                        && matches!(
                            binding.op,
                            FlatPureOp::Call(..) | FlatPureOp::Match(_) | FlatPureOp::Apply(..)
                        ) =>
                {
                    format!("Effect{kind}")
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
                Binding::Pure(IrBinding {
                    op: FlatPureOp::Match(match_),
                    ..
                }) => match_
                    .arms
                    .iter()
                    .map(|arm| (bound(&arm.pattern), Block::Pure(&arm.body)))
                    .collect(),
                Binding::Html(IrBinding {
                    op: FlatHtmlOp::Match(match_),
                    ..
                }) => match_
                    .arms
                    .iter()
                    .map(|arm| (bound(&arm.pattern), Block::Html(&arm.body)))
                    .collect(),
                Binding::Html(IrBinding {
                    op: FlatHtmlOp::For(for_),
                    ..
                }) => {
                    let var: Vec<ValueId> = for_.var.iter().map(|binder| binder.var).collect();
                    let mut nested = Vec::new();
                    if let Some(key) = &for_.key {
                        nested.push((var.clone(), Block::Pure(key)));
                    }
                    nested.push((var, Block::Html(&for_.body)));
                    nested
                }
                Binding::Pure(IrBinding {
                    op: FlatPureOp::Lambda(lambda),
                    ..
                }) => {
                    let params = lambda.parameters.iter().map(|param| param.var).collect();
                    vec![(params, Block::Pure(&lambda.body))]
                }
                Binding::Html(IrBinding {
                    op: FlatHtmlOp::Lambda(lambda),
                    ..
                }) => {
                    let params = lambda.parameters.iter().map(|param| param.var).collect();
                    vec![(params, Block::Html(&lambda.body))]
                }
                _ => Vec::new(),
            }
        }

        fn is_lambda(self) -> bool {
            matches!(
                self,
                Binding::Pure(IrBinding {
                    op: FlatPureOp::Lambda(_),
                    ..
                }) | Binding::Html(IrBinding {
                    op: FlatHtmlOp::Lambda(_),
                    ..
                })
            )
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
            // The parameters of a lambda take what each application passes.
            let operands = depends || binding.is_lambda();
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
    struct ShapeStats {
        arm_counts: Vec<usize>,
        binding_arms: usize,
        loops: usize,
        keyed_loops: usize,
        elements: usize,
        clicks: usize,
        /// The parameters of the functions and the lambdas of the program
        /// seen so far. Per program.
        parameters: HashSet<ValueId>,
        /// How many clicks each handler of the program has.
        handler_clicks: HashMap<ValueId, usize>,
        /// Handlers with more than one click.
        shared_handlers: usize,
        /// Clicks whose handler is bound outside the block of the element.
        clicks_from_outside: usize,
        /// Clicks whose handler is a parameter.
        clicks_on_parameters: usize,
        cells: usize,
    }

    impl ShapeStats {
        /// Fold what was counted by name for the program into the totals,
        /// before the names of the next program.
        fn finish_program(&mut self) {
            self.shared_handlers += self
                .handler_clicks
                .values()
                .filter(|uses| **uses > 1)
                .count();
            self.handler_clicks.clear();
            self.parameters.clear();
        }

        fn count_block(&mut self, block: Block<'_>) {
            let here: HashSet<ValueId> = block.bindings().iter().map(|b| b.var()).collect();
            for binding in block.bindings() {
                match binding {
                    Binding::Html(IrBinding {
                        op: FlatHtmlOp::For(for_),
                        ..
                    }) => {
                        self.loops += 1;
                        self.keyed_loops += usize::from(for_.key.is_some());
                    }
                    Binding::Html(IrBinding {
                        op: FlatHtmlOp::Element(element),
                        ..
                    }) => {
                        self.elements += 1;
                        if let Some(handler) = element.on_click {
                            self.clicks += 1;
                            *self.handler_clicks.entry(handler).or_default() += 1;
                            self.clicks_from_outside += usize::from(!here.contains(&handler));
                            self.clicks_on_parameters +=
                                usize::from(self.parameters.contains(&handler));
                        }
                    }
                    Binding::Html(IrBinding {
                        op: FlatHtmlOp::Lambda(lambda),
                        ..
                    }) => {
                        self.parameters
                            .extend(lambda.parameters.iter().map(|param| param.var));
                    }
                    Binding::Pure(IrBinding {
                        op: FlatPureOp::Lambda(lambda),
                        ..
                    }) => {
                        self.parameters
                            .extend(lambda.parameters.iter().map(|param| param.var));
                    }
                    Binding::Cell(..) => self.cells += 1,
                    _ => {}
                }
                let arms = match binding {
                    Binding::Pure(IrBinding {
                        op: FlatPureOp::Match(m),
                        ..
                    }) => m.arms.iter().map(|arm| &arm.pattern).collect(),
                    Binding::Html(IrBinding {
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

    #[derive(Default)]
    struct ClickStats {
        /// The clicks rendered, over all programs.
        clicks: usize,
        /// Programs with a click.
        clicked: usize,
        /// Programs with a click that writes a cell a value other than the
        /// one it holds.
        changing: usize,
        /// Programs with a click after which the page renders differently.
        rerendering: usize,
        /// Programs with a click after which an element is evaluated or no
        /// longer evaluated although each iteration it is in is evaluated
        /// both before and after, as a match took another arm.
        switching_arms: usize,
        /// Programs with a click after which a cell is bound or no longer
        /// bound although each iteration it is in is evaluated both before
        /// and after, as a match took another arm. A cell that is no longer
        /// bound starts over when its arm is taken again.
        switching_cells: usize,
        /// Programs with a click after which a loop has other iterations:
        /// it gained, lost or re-keyed some. A match that takes another arm
        /// with a loop in it counts here as well.
        changing_loops: usize,
        /// Programs with a click that changes an array cell.
        changing_arrays: usize,
        /// What the clicks of the program being counted have done so far.
        program: Clicked,
    }

    #[derive(Default)]
    struct Clicked {
        any: bool,
        changing: bool,
        rerendering: bool,
        switching_arms: bool,
        switching_cells: bool,
        changing_loops: bool,
        changing_arrays: bool,
    }

    /// The iterations of the places, each as the steps up to and including
    /// it.
    fn iterations<'p>(places: impl Iterator<Item = &'p Place>) -> BTreeSet<Vec<Step>> {
        let mut iterations = BTreeSet::new();
        for place in places {
            let steps: Vec<Step> = place.iter().cloned().collect();
            for (index, step) in steps.iter().enumerate() {
                if matches!(step, Step::Iteration(..)) {
                    iterations.insert(steps[..=index].to_vec());
                }
            }
        }
        iterations
    }

    /// Whether each iteration of the place is in `both`, the iterations two
    /// invocations both evaluate, so that a place only one of them
    /// evaluates is not explained by a loop.
    fn in_both_iterations(place: &Place, both: &BTreeSet<Vec<Step>>) -> bool {
        let steps: Vec<Step> = place.iter().cloned().collect();
        (steps.iter().enumerate()).all(|(index, step)| {
            !matches!(step, Step::Iteration(..)) || both.contains(&steps[..=index])
        })
    }

    impl ClickStats {
        fn finish_program(&mut self) {
            let program = std::mem::take(&mut self.program);
            self.clicked += usize::from(program.any);
            self.changing += usize::from(program.changing);
            self.rerendering += usize::from(program.rerendering);
            self.switching_arms += usize::from(program.switching_arms);
            self.switching_cells += usize::from(program.switching_cells);
            self.changing_loops += usize::from(program.changing_loops);
            self.changing_arrays += usize::from(program.changing_arrays);
        }

        fn count<'a>(
            &mut self,
            program: &'a FlatProgram,
            entrypoint: EntrypointId,
            args: &HashMap<AttributeName, Value<'a>>,
            html: &Html<'a>,
            state: &State<'a>,
        ) {
            let mut markup = String::new();
            html.write(&mut markup);
            for (_, target) in clicks(&program.functions, html) {
                self.clicks += 1;
                self.program.any = true;
                let Ok(effect) = target else {
                    continue;
                };
                let mut next = state.clone();
                if next.run(&effect, &program.functions).is_err() {
                    continue;
                }
                let changed: Vec<&Value<'_>> = next
                    .cells
                    .iter()
                    .filter(|(place, value)| !state.cells[place].total_eq(value))
                    .map(|(_, value)| value)
                    .collect();
                if changed.is_empty() {
                    continue;
                }
                self.program.changing = true;
                self.program.changing_arrays |=
                    changed.iter().any(|value| matches!(value, Value::Array(_)));
                if let Ok((value, next_state)) =
                    invoke_entrypoint(program, entrypoint, args.clone(), &next)
                {
                    let mut next_markup = String::new();
                    value.unwrap_html().write(&mut next_markup);
                    self.program.rerendering |= markup != next_markup;
                    // A loop iteration can hold cells and no elements, so
                    // the iterations come from both.
                    let before = iterations(state.elements.keys().chain(state.cells.keys()));
                    let after =
                        iterations(next_state.elements.keys().chain(next_state.cells.keys()));
                    let both = before.intersection(&after).cloned().collect();
                    self.program.changing_loops |= before != after;
                    let (elements, next_elements) = (&state.elements, &next_state.elements);
                    let dropped =
                        (elements.keys()).filter(|place| !next_elements.contains_key(*place));
                    let bound =
                        (next_elements.keys()).filter(|place| !elements.contains_key(*place));
                    self.program.switching_arms |= dropped
                        .chain(bound)
                        .any(|place| in_both_iterations(place, &both));
                    let (cells, next_cells) = (&state.cells, &next_state.cells);
                    let dropped = (cells.keys()).filter(|place| !next_cells.contains_key(*place));
                    let bound = (next_cells.keys()).filter(|place| !cells.contains_key(*place));
                    self.program.switching_cells |= dropped
                        .chain(bound)
                        .any(|place| in_both_iterations(place, &both));
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
        const SIZE_EDGES: [usize; 5] = [256, 1024, 4096, 16384, INPUT_LEN];
        let mut sizes = [0usize; SIZE_EDGES.len() + 1];
        let mut html_bindings = 0;
        let mut shared_html = 0;
        let mut cells_bound = 0;
        let mut shape_stats = ShapeStats::default();
        let mut click_stats = ClickStats::default();
        for _ in 0..SAMPLES {
            let bytes: Vec<u8> = (0..INPUT_LEN).map(|_| rng.random()).collect();
            let mut u = Unstructured::new(&bytes);
            let program = random_program(&mut u);
            let used = INPUT_LEN - u.len();
            consumed += used;
            most_consumed = most_consumed.max(used);
            exhausted += u.is_empty() as usize;
            let bucket = SIZE_EDGES.iter().position(|edge| used < *edge);
            sizes[bucket.unwrap_or(SIZE_EDGES.len())] += 1;
            for (entrypoint, args) in random_entrypoint_args(&program, &mut rng) {
                entrypoints += 1;
                let result =
                    invoke_entrypoint(&program, entrypoint, args.clone(), &State::default());
                call_depth_limit +=
                    matches!(result, Err(EvalError::CallDepthLimit { .. })) as usize;
                array_len_limit +=
                    matches!(result, Err(EvalError::ArrayLengthLimit { .. })) as usize;
                string_len_limit +=
                    matches!(result, Err(EvalError::StringLengthLimit { .. })) as usize;
                html_size_limit += matches!(result, Err(EvalError::HtmlSizeLimit { .. })) as usize;
                op_count_limit += matches!(result, Err(EvalError::OpCountLimit { .. })) as usize;
                if let Ok((value, state)) = result {
                    cells_bound += state.cells.len();
                    let html = value.unwrap_html();
                    click_stats.count(&program, entrypoint, &args, &html, &state);
                }
            }
            click_stats.finish_program();
            let mut live = HashSet::new();
            let mut dynamic = HashSet::new();
            let bodies: Vec<Block<'_>> = program
                .functions
                .iter()
                .map(Block::of)
                .chain(
                    program
                        .entrypoints
                        .iter()
                        .map(|entrypoint| Block::Html(&entrypoint.body)),
                )
                .collect();
            let parameters = program
                .functions
                .iter()
                .map(|function| function.parameters())
                .chain(program.entrypoints.iter().map(|entrypoint| {
                    entrypoint
                        .parameters
                        .iter()
                        .map(|param| param.widen())
                        .collect()
                }));
            for (parameters, body) in parameters.zip(&bodies) {
                mark_live(*body, &mut live);
                dynamic.extend(parameters.iter().map(|param| param.binder.var));
                mark_dynamic(*body, &mut dynamic);
                shape_stats
                    .parameters
                    .extend(parameters.iter().map(|param| param.binder.var));
                shape_stats.count_block(*body);
            }
            shape_stats.finish_program();
            let mut program_counts = BTreeMap::new();
            for body in &bodies {
                count_ops(*body, &live, &dynamic, &mut program_counts);
            }
            for (kind, [generated, live, dynamic]) in program_counts {
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
        let ShapeStats {
            arm_counts,
            binding_arms,
            loops,
            keyed_loops,
            elements,
            clicks,
            parameters: _,
            handler_clicks: _,
            shared_handlers,
            clicks_from_outside,
            clicks_on_parameters,
            cells,
        } = shape_stats;
        let ClickStats {
            clicks: targets,
            clicked,
            changing,
            rerendering,
            switching_arms,
            switching_cells,
            changing_loops,
            changing_arrays,
            program: _,
        } = click_stats;
        let arms: usize = arm_counts.iter().sum();
        let fewest_arms = arm_counts.iter().min().unwrap();
        let most_arms = arm_counts.iter().max().unwrap();
        let size_labels = [
            "under 256",
            "under 1K",
            "under 4K",
            "under 16K",
            "under 64K",
            "at 64K",
        ];
        let widest = sizes.iter().copied().max().unwrap().max(1);
        let sizes: String = size_labels
            .iter()
            .zip(sizes)
            .map(|(label, count)| {
                let bar = "#".repeat(count * 40 / widest);
                let line = format!("{label:<10}{count:>5}  {bar}");
                format!("{}\n", line.trim_end())
            })
            .collect();
        let mut report = format!(
            "-- generation --\n\
             {} of {INPUT_LEN} input bytes consumed per program on average, at most {most_consumed}\n\
             {exhausted} of {SAMPLES} programs exhaust the input\n\n\
             -- sizes --\n\
             {sizes}\n\
             -- programs --\n\
             {shared_html} of {html_bindings} Html bindings have more than one reference\n\
             {:.1} arms per match on average, at least {fewest_arms} and at most {most_arms}\n\
             {binding_arms} of {arms} match arms bind names\n\
             {cells} cells\n\
             {keyed_loops} of {loops} loops have a key\n\
             {clicks} of {elements} elements have a click\n\
             {shared_handlers} handlers have more than one click\n\
             {clicks_from_outside} clicks apply a handler bound outside their block, \
             {clicks_on_parameters} of them a parameter\n\n\
             -- invocations --\n\
             {entrypoints} invocations\n\
             {call_depth_limit} invocations hit the call depth limit\n\
             {array_len_limit} invocations hit the array length limit\n\
             {string_len_limit} invocations hit the string length limit\n\
             {html_size_limit} invocations hit the Html size limit\n\
             {op_count_limit} invocations hit the op count limit\n\
             {cells_bound} cells bound\n\n\
             {targets} clicks\n\
             {clicked} programs have a click\n\
             {changing} programs have a click that changes a cell\n\
             {rerendering} programs have a click that changes the page\n\
             {switching_arms} programs have a click that switches an arm of a match\n\
             {switching_cells} programs have a click that switches an arm of a match that binds cells\n\
             {changing_loops} programs have a click that changes the iterations of a loop\n\
             {changing_arrays} programs have a click that changes an array cell\n\n\
             -- ops --\n\
             {:<20}{:>12}{:>8}{:>8}{:>8}\n",
            consumed / SAMPLES,
            arms as f64 / arm_counts.len() as f64,
            "op",
            "ops/program",
            "% progs",
            "% live",
            "% dyn"
        );
        for (kind, [generated, programs, live, dynamic]) in counts {
            let per_program = generated as f64 / SAMPLES as f64;
            let programs = 100.0 * programs as f64 / SAMPLES as f64;
            let live = 100.0 * live as f64 / generated as f64;
            let dynamic = 100.0 * dynamic as f64 / generated as f64;
            report.push_str(&format!(
                "{kind:<20}{per_program:>12.1}{programs:>8.1}{live:>8.1}{dynamic:>8.1}\n"
            ));
        }
        expect![[r#"
            -- generation --
            8261 of 65536 input bytes consumed per program on average, at most 65536
            1 of 100 programs exhaust the input

            -- sizes --
            under 256     7  #####
            under 1K      7  #####
            under 4K     26  #####################
            under 16K    49  ########################################
            under 64K    10  ########
            at 64K        1

            -- programs --
            73 of 9292 Html bindings have more than one reference
            2.4 arms per match on average, at least 2 and at most 12
            8092 of 22025 match arms bind names
            5199 cells
            457 of 776 loops have a key
            1590 of 1966 elements have a click
            279 handlers have more than one click
            446 clicks apply a handler bound outside their block, 21 of them a parameter

            -- invocations --
            209 invocations
            12 invocations hit the call depth limit
            0 invocations hit the array length limit
            0 invocations hit the string length limit
            0 invocations hit the Html size limit
            5 invocations hit the op count limit
            1471 cells bound

            239 clicks
            53 programs have a click
            27 programs have a click that changes a cell
            13 programs have a click that changes the page
            3 programs have a click that switches an arm of a match
            6 programs have a click that switches an arm of a match that binds cells
            5 programs have a click that changes the iterations of a loop
            2 programs have a click that changes an array cell

            -- ops --
            op                   ops/program % progs  % live   % dyn
            Apply                       79.4    94.0    92.3    72.9
            Array                      189.4    97.0    91.0    17.0
            ArrayEnumerate               1.1    47.0   100.0    50.0
            ArrayFromRange               5.5    83.0    94.7     0.0
            ArrayGet                    18.8    88.0    92.4    52.7
            ArrayIsEmpty                 1.2    48.0    92.3    51.3
            ArrayLength                 15.7    89.0    93.6    46.8
            BoolLiteral                 64.5    95.0    91.4     0.0
            BoolNegation                 1.2    53.0    93.4    43.0
            Call                         4.2    45.0    94.1    77.0
            Cell                        52.0    93.0    96.7    39.7
            CellRead                     6.8    69.0    94.7    39.1
            CellWrite                   18.8    82.0    94.8    45.8
            EffectApply                  7.8    81.0    95.0    67.7
            EffectBind                 183.5    89.0    94.1    34.8
            EffectCall                  19.4    10.0    96.1    93.5
            EffectMatch                  7.6    80.0    95.3    84.2
            EffectPure                 119.2    90.0    94.2     0.0
            Enum                        98.5    68.0    90.7    18.8
            Equals                       1.1    44.0    91.2    60.2
            FieldAccess                  4.4    42.0    89.8    54.6
            FloatLiteral                76.8    96.0    91.2     0.0
            FloatToInt                  15.2    87.0    94.0    43.3
            HtmlApply                    3.7    67.0    99.7    84.3
            HtmlConcat                  30.9    90.0    98.7    39.0
            HtmlElement                 19.7    83.0    98.3    65.9
            HtmlEscape                  12.2    84.0    98.9    51.7
            HtmlFor                      7.8    80.0    99.2    72.8
            HtmlLambda                   4.5    67.0    97.3    63.2
            HtmlText                     6.3    70.0    98.6     0.0
            IntLiteral                  78.6    94.0    92.7     0.0
            IntToFloat                  12.8    85.0    93.5    44.9
            IntToString                 19.8    89.0    96.5    43.1
            Lambda                     302.6    95.0    93.7    37.4
            LessThan                     1.2    52.0    90.5    53.4
            LessThanOrEqual              1.2    54.0    90.8    68.1
            Match                       84.0    95.0    92.9    84.8
            NumericAdd                  28.1    88.0    93.4    61.2
            NumericMultiply             27.2    86.0    93.4    62.4
            NumericNegation             28.1    89.0    92.9    46.4
            NumericSubtract             28.3    86.0    93.5    61.2
            OptionIsNone                 1.2    50.0    89.9    56.3
            OptionIsSome                 1.1    49.0    94.6    54.5
            OptionNone                 118.0    94.0    91.0     0.0
            OptionSome                  65.9    93.0    90.8    23.6
            Record                      84.2    65.0    91.5    26.2
            StringConcat                19.1    82.0    95.9    47.5
            StringIsEmpty                1.1    57.0    90.7    52.8
            StringLiteral               59.7    98.0    91.4     0.0
            Tuple                      773.7    97.0    91.0    39.6
            TupleIndex                 439.7    95.0    90.2    67.6
        "#]]
        .assert_eq(&report);
    }

    #[test]
    fn should_generate_a_minimal_program_from_no_input() {
        check(
            0,
            0,
            expect![[r#"

                entrypoint V0@e0() -> Html {
                  let v0: Int = -2147483648
                  let v1: Int = cell(v0)
                  let v2: Html = concat()
                  v2
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_a_program_from_seed_16() {
        check(
            16,
            128,
            expect![[r#"
                record R0 {
                  f0: Bool,
                  f1: Array[()],
                  f2: (Float, Int, R1),
                }

                record R1 {
                  f0: ((), Option[R1]),
                  f1: (Option[E1], Float),
                }

                enum E0 {
                  W0,
                  W1 {f0: (Array[String], Float, Option[E1])},
                }

                enum E1 {
                  W0,
                  W1 {f0: ()},
                }

                fn C0@f0(p0@v0: ((Bool, E1), (Option[String]) -> (Float, E1, Bool)) -> Array[Int], p1@v1: Html, p2@v2: Html, p3@v3: () -> E1, p4@v4: Array[R0], p5@v5: E0) -> Html {
                  let v9: Int = -2147483648
                  let v10: Int = cell(v9)
                  v1
                }
                fn C1@f1(p0@v6: ((), String, R1), p1@v7: Html, p2@v8: ((R1, Int),)) -> Html {
                  let v11: Int = -2147483648
                  let v12: Int = cell(v11)
                  v7
                }
                entrypoint V0@e0() -> Html {
                  let v13: Int = -2147483648
                  let v14: Int = cell(v13)
                  let v15: Html = concat()
                  v15
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_a_program_from_seed_24() {
        check(
            24,
            128,
            expect![[r#"
                enum E0 {
                  W0 {f0: Float},
                  W1 {f0: Bool},
                }

                enum E1 {
                  W0 {f0: Option[Option[Float]], f1: Int},
                  W1,
                }

                entrypoint V0@e0(p0@v0: Option[Int], p1@v1: Option[Array[Int]]) -> Html {
                  let v2: Html = text("a\"b'c")
                  v2
                }
                entrypoint V1@e1() -> Html {
                  let v3: Html = concat()
                  let v6: () -> Effect[()] = fn() {
                    let v4: () = ()
                    let v5: Effect[()] = pure(v4)
                    v5
                  }
                  let v7: Html = element("section", {}, v3) on_click v6
                  v7
                }
            "#]],
        );
    }
}
