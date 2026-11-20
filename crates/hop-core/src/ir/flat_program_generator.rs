use crate::hop::typing::{ComparableType, EquatableType, NumericType, Ty};
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_program::FlatProgram;
use crate::ir::flat_program_builder::{
    DeclaredFunction, FlatBlockBuilder, FlatProgramBuilder, Pat,
};
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_type::{IrDataType, IrModifier, IrSignature, IrType};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
use crate::ir::runtime::random::random_value;
use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use arbitrary::Unstructured;
use rand::{SeedableRng, rngs::SmallRng};
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

/// How often a function that returns a value has the render modifier.
const RENDER_VALUE_ODDS: u8 = 64;

/// How often a loop that binds its variable has a key.
const KEY_ODDS: u8 = 204;

/// How often a block that renders Html mints an identity before the ops
/// that may take it, so that an element in each arm of a match can share it.
const EARLY_MINT_ODDS: u8 = 64;

/// How often an op that renders Html may be a toggle: a cell, a match on
/// it and a click that writes it.
const TOGGLE_ODDS: u8 = 64;

/// How often a closure applied in a body that mutates mutates too.
const MUT_ODDS: u8 = 128;

/// How often a function that returns a value without the render modifier
/// mutates.
const MUT_FUNCTION_ODDS: u8 = 64;

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
    /// Whether the path has used the Html or the identity.
    used: bool,
}

/// The names a path can read.
#[derive(Clone)]
struct Scope {
    names: Vec<Name>,
    modifier: IrModifier,
}

impl Scope {
    fn push(&mut self, var: ValueId) -> ValueId {
        self.names.push(Name { var, used: false });
        var
    }

    fn set_used(&mut self, value: ValueId, used: bool) {
        if let Some(name) = self.names.iter_mut().rev().find(|name| name.var == value) {
            name.used = used;
        }
    }

    fn inner(&self, types: &[IrType], modifier: IrModifier) -> Scope {
        Scope {
            names: (self.names.iter().copied())
                .filter(|name| {
                    let typ = &types[name.var.index()];
                    !typ.is_html_or_html_closure() && *typ != IrType::ElementId
                })
                .collect(),
            modifier,
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
}

enum TyPat {
    Var(usize),
    Ground(IrDataType),
    Array(Box<TyPat>),
    Option(Box<TyPat>),
}

enum Layer {
    Data,
    Cell,
}

type Subst = HashMap<usize, IrDataType>;

/// Whether typ has the shape of pattern, binding the variables of the
/// pattern in subst.
fn match_type(pattern: &TyPat, typ: &IrDataType, subst: &mut Subst) -> bool {
    match (pattern, typ) {
        (TyPat::Var(var), _) => subst.entry(*var).or_insert_with(|| typ.clone()) == typ,
        (TyPat::Ground(ground), _) => ground == typ,
        (TyPat::Array(element), IrDataType::Array(typ))
        | (TyPat::Option(element), IrDataType::Option(typ)) => match_type(element, typ, subst),
        _ => false,
    }
}

/// The data type pattern stands for under subst, binding each variable
/// still unbound to a type from fresh.
fn instantiate(
    pattern: &TyPat,
    subst: &mut Subst,
    fresh: &mut impl FnMut() -> IrDataType,
) -> IrDataType {
    match pattern {
        TyPat::Var(var) => subst.entry(*var).or_insert_with(&mut *fresh).clone(),
        TyPat::Ground(typ) => typ.clone(),
        TyPat::Array(element) => IrDataType::Array(Box::new(instantiate(element, subst, fresh))),
        TyPat::Option(element) => IrDataType::Option(Box::new(instantiate(element, subst, fresh))),
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

fn handler_type() -> IrType {
    IrType::function(IrModifier::Mut, [], IrType::Data(IrDataType::UNIT))
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
            g.random_result_type()
        };
        let render_modifier = html || g.chance(RENDER_VALUE_ODDS);
        let modifier = if render_modifier {
            IrModifier::Render
        } else if g.chance(MUT_FUNCTION_ODDS) {
            IrModifier::Mut
        } else {
            IrModifier::Pure
        };
        let params: Vec<(String, IrType)> = (0..g.count(0..=8))
            .map(|i| {
                let typ = if g.index(4) == 0 {
                    g.random_closure_type(html, render_modifier, 1)
                } else if g.index(8) == 0 {
                    handler_type()
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
            modifier,
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
            let mut scope = Scope {
                names: Vec::new(),
                modifier: declared.modifier,
            };
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
                let mut scope = Scope {
                    names: Vec::new(),
                    modifier: IrModifier::Render,
                };
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
    u: &mut Unstructured<'_>,
) -> arbitrary::Result<Vec<(EntrypointId, HashMap<AttributeName, Value<'static>>)>> {
    let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
    Ok(program
        .entrypoints
        .iter_enumerated()
        .map(|(id, decl)| {
            let args = decl
                .parameters
                .iter()
                .map(|param| {
                    let value = random_value(&mut rng, &param.binder.typ, &program.types);
                    (param.name.clone(), value)
                })
                .collect();
            (id, args)
        })
        .collect())
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
        let IrType::Data(typ) = self.ir_type(ty) else {
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
        scope: &Scope,
        target: &IrType,
    ) -> Vec<&'t DeclaredFunction> {
        (b.functions().iter())
            .filter(|function| {
                function.return_type == *target && scope.modifier.allows(function.modifier)
            })
            .collect()
    }

    fn closure_parameters(typ: &IrType) -> Vec<IrType> {
        match typ {
            IrType::Function(signature) => signature.parameters.clone(),
            _ => unreachable!("{typ} is not a closure type"),
        }
    }

    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&IrDataType) -> bool,
    ) -> Option<(ValueId, IrDataType)> {
        let types = b.value_types();
        let mut vars: Vec<(ValueId, IrDataType)> = (scope
            .names_where(&types, |typ| typ.as_data().is_some_and(&fits)))
        .map(|name| (name, types[name.index()].as_data().unwrap().clone()))
        .collect();
        drop(types);
        if vars.is_empty() || self.index(10) == 0 {
            return None;
        }
        let index = self.index(vars.len());
        Some(vars.swap_remove(index))
    }

    /// The names in scope that are values, or cells, by layer, of a data
    /// type that matches pattern, each with what the variables of the
    /// pattern bind to.
    fn candidates(
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        layer: Layer,
        pattern: &TyPat,
    ) -> Vec<(ValueId, Subst)> {
        let types = b.value_types();
        (scope.names_where(&types, |_| true))
            .filter_map(|name| {
                let ((Layer::Data, IrType::Data(typ)) | (Layer::Cell, IrType::Cell(typ))) =
                    (&layer, &types[name.index()])
                else {
                    return None;
                };
                let mut subst = Subst::new();
                match_type(pattern, typ, &mut subst).then_some((name, subst))
            })
            .collect()
    }

    /// An operand whose data type matches pattern, with what the variables
    /// of the pattern bind to: one of the values in scope that match, or,
    /// one time in ten or when none matches, an op built at depth and
    /// nesting for the pattern with its variables bound to random types.
    fn operand(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        pattern: &TyPat,
        depth: usize,
        nesting: usize,
    ) -> (ValueId, Subst) {
        let mut candidates = Self::candidates(b, scope, Layer::Data, pattern);
        if !candidates.is_empty() && self.index(10) != 0 {
            return candidates.swap_remove(self.index(candidates.len()));
        }
        let mut subst = Subst::new();
        let typ = instantiate(pattern, &mut subst, &mut || self.random_data_type(1));
        (self.data_op(b, scope, &typ, depth, nesting), subst)
    }

    fn random_closure_type(&mut self, html: bool, render: bool, depth: usize) -> IrType {
        if html && self.coin() {
            IrType::Function(self.random_html_signature(depth))
        } else {
            IrType::Function(self.random_value_signature(render, depth, None))
        }
    }

    fn random_value_signature(
        &mut self,
        render: bool,
        depth: usize,
        result: Option<IrType>,
    ) -> IrSignature {
        let parameters = (0..self.count(0..=2))
            .map(|_| {
                if depth > 0 && self.index(4) == 0 {
                    IrType::Function(self.random_value_signature(render, depth - 1, None))
                } else if self.index(8) == 0 {
                    handler_type()
                } else {
                    IrType::Data(self.random_data_type(1))
                }
            })
            .collect();
        let result = result.unwrap_or_else(|| self.random_result_type());
        IrSignature {
            modifier: if render && self.chance(RENDER_VALUE_ODDS) {
                IrModifier::Render
            } else {
                IrModifier::Pure
            },
            parameters,
            result: Box::new(result),
        }
    }

    fn random_result_type(&mut self) -> IrType {
        IrType::Data(self.random_data_type(1))
    }

    fn random_html_signature(&mut self, depth: usize) -> IrSignature {
        let parameters = (0..self.count(0..=2))
            .map(|_| match self.index(if depth > 0 { 4 } else { 2 }) {
                0 => IrType::Data(self.random_data_type(1)),
                1 => IrType::HTML,
                2 => self.random_closure_type(false, true, depth - 1),
                _ => self.random_closure_type(true, true, depth - 1),
            })
            .collect();
        IrSignature {
            modifier: IrModifier::Render,
            parameters,
            result: Box::new(IrType::Html),
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
        let mut scope = scope.inner(&b.value_types(), IrModifier::Pure);
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
        let mut scope = scope.inner(&b.value_types(), IrModifier::Render);
        if let Some(var) = var {
            scope.push(var);
        }
        self.html_block(b, &mut scope, depth)
    }

    fn html_block(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        scope.modifier = IrModifier::Render;
        if self.chance(EARLY_MINT_ODDS) {
            scope.push(b.mint());
        }
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
        if self.chance(DEAD_BINDING_ODDS) {
            let typ = self.random_data_type(1);
            self.data_op(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
        }
        if scope.modifier.allows(IrModifier::Mut) {
            self.writes(b, scope, 0..=2, depth, MAX_EXPRESSION_DEPTH);
        }
        self.data_op(b, scope, target, depth, MAX_EXPRESSION_DEPTH)
    }

    fn writes(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        count: RangeInclusive<usize>,
        depth: usize,
        nesting: usize,
    ) {
        for _ in 0..self.count(count) {
            let mut cells = Self::candidates(b, scope, Layer::Cell, &TyPat::Var(0));
            if cells.is_empty() || nesting == 0 {
                return;
            }
            let (cell, subst) = cells.swap_remove(self.index(cells.len()));
            let typ = subst[&0].clone();
            let value = if self.coin() {
                self.literal(b, scope, &typ, MAX_LITERAL_DEPTH)
            } else {
                self.data_op(b, scope, &typ, depth, nesting - 1)
            };
            b.cell_write(cell, value);
        }
    }

    fn block_of_type(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrType,
        depth: usize,
    ) -> ValueId {
        match typ {
            IrType::Data(target) => self.data_block(b, scope, target, depth),
            IrType::Html => self.html_block(b, scope, depth),
            IrType::Cell(_) => unreachable!("a block produces no cell"),
            IrType::ElementId => unreachable!("a block produces no identity"),
            IrType::Function(_) => {
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
            IrType::Data(target) => self.data_op(b, scope, target, depth, nesting),
            IrType::Html => self.html_op(b, scope, depth, nesting),
            IrType::Cell(_) => unreachable!("a cell is bound for a block, not asked for"),
            IrType::ElementId => {
                unreachable!("an identity is minted for an element, not asked for")
            }
            IrType::Function(_) => self.closure_op(b, scope, typ, depth, nesting),
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
        let parameters = Self::closure_parameters(typ);
        let IrType::Function(signature) = typ else {
            unreachable!("{typ} is not a closure type");
        };
        let render = signature.modifier == IrModifier::Render;
        let mutates = signature.modifier == IrModifier::Mut;
        let mut inner = scope.inner(&b.value_types(), signature.modifier);
        let depth = depth.saturating_sub(1);
        let nesting = nesting.saturating_sub(1);
        let body = |b: &FlatBlockBuilder<'_>, params: &[ValueId]| {
            for param in params {
                inner.push(*param);
            }
            match &*signature.result {
                IrType::Data(result) if mutates => {
                    self.writes(b, &mut inner, 1..=3, depth, nesting);
                    self.data_op(b, &mut inner, result, depth, nesting)
                }
                IrType::Data(result) => self.data_op(b, &mut inner, result, depth, nesting),
                _ => self.html_op(b, &mut inner, depth, nesting),
            }
        };
        let lambda = if mutates {
            b.mut_lambda(parameters, body)
        } else if render {
            b.render_lambda(parameters, body)
        } else {
            b.lambda(parameters, body)
        };
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
        let fits = |typ: &IrType| match typ {
            IrType::Function(signature) => {
                scope.modifier.allows(signature.modifier) && *signature.result == *result
            }
            _ => false,
        };
        let types = b.value_types();
        let fitting: Vec<ValueId> = scope.names_where(&types, fits).collect();
        drop(types);
        let closure = if !fitting.is_empty() && self.coin() {
            fitting[self.index(fitting.len())]
        } else {
            let typ = match result {
                IrType::Data(result) => {
                    let mut signature = self.random_value_signature(
                        scope.modifier.allows(IrModifier::Render),
                        1,
                        Some(IrType::Data(result.clone())),
                    );
                    if scope.modifier.allows(IrModifier::Mut) && self.chance(MUT_ODDS) {
                        signature.modifier = IrModifier::Mut;
                    }
                    IrType::Function(signature)
                }
                _ => IrType::Function(self.random_html_signature(1)),
            };
            self.closure_op(b, scope, &typ, depth, nesting - 1)
        };
        let parameters = Self::closure_parameters(&b.value_types()[closure.index()]);
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
                        let children = if void {
                            None
                        } else if depth == 0 {
                            Some(b.html_concat(Vec::new()))
                        } else {
                            Some(self.html_leaf(b, scope, depth - 1))
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
        children: Option<ValueId>,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let identity = match self.name_in_scope(b, scope, &IrType::ElementId) {
            Some(early) => {
                scope.set_used(early, true);
                early
            }
            None => b.mint(),
        };
        if !self.chance(CLICK_ODDS) {
            return b.element(identity, tag, attributes, children);
        }
        let handler = self.op_of_type(b, scope, &handler_type(), depth, nesting);
        b.element_on_click(identity, tag, attributes, handler, children)
    }

    fn data_leaf(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &IrDataType,
        depth: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(b, scope, &IrType::Data(typ.clone())) {
            return self.cell(b, scope, name);
        }
        if scope.modifier == IrModifier::Mut && self.coin() {
            let mut cells = Self::candidates(b, scope, Layer::Cell, &TyPat::Ground(typ.clone()));
            if !cells.is_empty() {
                let (cell, _) = cells.swap_remove(self.index(cells.len()));
                return scope.push(b.cell_read(cell));
            }
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
                    b.empty_array(IrType::Data((**inner).clone()))
                } else {
                    b.array(elements)
                }
            }
            IrDataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.data_leaf(b, scope, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none(IrType::Data((**inner).clone()))
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
                b.record(IrType::Data(typ.clone()), values)
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
                b.enum_variant_with_fields(IrType::Data(typ.clone()), variant.name.as_str(), values)
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
        if !Self::callees(b, scope, &IrType::HTML).is_empty() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::ForArray, P::ForRange, P::Apply]);
        }
        if depth > 0 && scope.modifier.allows(IrModifier::Render) && self.chance(TOGGLE_ODDS) {
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
                let children = if void {
                    None
                } else {
                    let mut children: Vec<_> = (0..self.count(0..=4))
                        .map(|_| self.html_op(b, scope, depth, nesting - 1))
                        .collect();
                    Some(if children.len() == 1 {
                        children.pop().unwrap()
                    } else {
                        b.html_concat(children)
                    })
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
                let (array, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Array(Box::new(TyPat::Var(0))),
                    depth,
                    nesting - 1,
                );
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
        let mut initial = scope.inner(&b.value_types(), IrModifier::Pure);
        let cell =
            scope.push(b.cell(|b| self.literal(b, &mut initial, &subject_ty, MAX_LITERAL_DEPTH)));
        let value = scope.push(b.cell_read(cell));
        let mut inner = scope.inner(&b.value_types(), IrModifier::Pure);
        let handler = b.event_handler(|b| {
            let value = self.literal(b, &mut inner, &subject_ty, MAX_LITERAL_DEPTH);
            b.cell_write(cell, value)
        });
        let handler = scope.push(handler);
        let shown = self.match_on(b, scope, value, subject_ty, &IrType::HTML, depth);
        scope.push(shown);
        scope.set_used(shown, true);
        let tags: Vec<&str> = (ELEMENTS.iter())
            .filter(|(_, void)| !void)
            .map(|(tag, _)| *tag)
            .collect();
        b.element_on_click(
            b.mint(),
            self.u.choose(&tags).unwrap(),
            Vec::new(),
            handler,
            Some(shown),
        )
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
            Neg(NumericType),
            Add(NumericType),
            Sub(NumericType),
            Mul(NumericType),
            Call,
            Apply,
        }
        let typ = IrType::Data(target.clone());
        let record_fields: Vec<(IrDataType, String)> = self
            .record_fields_of_type(b, target)
            .map(|(record, field)| (record.clone(), field.name.as_str().to_string()))
            .collect();
        let mut records: Vec<&IrDataType> =
            (record_fields.iter()).map(|(record, _)| record).collect();
        // record_fields_of_type yields the fields of each record together,
        // so removing consecutive repeats leaves each record once.
        records.dedup();
        let objects: Vec<(ValueId, &IrDataType)> = (records.into_iter())
            .flat_map(|record| {
                let pattern = TyPat::Ground(record.clone());
                (Self::candidates(b, scope, Layer::Data, &pattern).into_iter())
                    .map(move |(object, _)| (object, record))
            })
            .collect();
        let mut productions: Vec<P> = Vec::new();
        if !objects.is_empty() {
            productions.push(P::FieldAccess);
        }
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
                P::Neg(NumericType::Int),
                P::Add(NumericType::Int),
                P::Sub(NumericType::Int),
                P::Mul(NumericType::Int),
                P::FloatToInt,
            ]);
        }
        if *target == IrDataType::Float {
            productions.extend([
                P::Neg(NumericType::Float),
                P::Add(NumericType::Float),
                P::Sub(NumericType::Float),
                P::Mul(NumericType::Float),
                P::IntToFloat,
            ]);
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
        if !Self::callees(b, scope, &typ).is_empty() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.extend([P::Match, P::Apply]);
        }
        if productions.is_empty() {
            return self.data_leaf(b, scope, target, MAX_LITERAL_DEPTH);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::FieldAccess => {
                let (object, record) = objects[self.index(objects.len())];
                let fields: Vec<&String> = (record_fields.iter())
                    .filter(|(candidate, _)| candidate == record)
                    .map(|(_, field)| field)
                    .collect();
                b.field_access(object, self.u.choose(&fields).unwrap())
            }
            P::Eq => {
                let (row, ty) = match self.index(4) {
                    0 => (EquatableType::Int, IrDataType::Int),
                    1 => (EquatableType::String, IrDataType::String),
                    2 => (EquatableType::Bool, IrDataType::Bool),
                    _ => (EquatableType::Float, IrDataType::Float),
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.eq(row, left, right)
            }
            P::Lt => {
                let (row, ty) = if self.coin() {
                    (ComparableType::Int, IrDataType::Int)
                } else {
                    (ComparableType::Float, IrDataType::Float)
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lt(row, left, right)
            }
            P::Lte => {
                let (row, ty) = if self.coin() {
                    (ComparableType::Int, IrDataType::Int)
                } else {
                    (ComparableType::Float, IrDataType::Float)
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lte(row, left, right)
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
                let (operand, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Option(Box::new(TyPat::Var(0))),
                    depth,
                    nesting - 1,
                );
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let (operand, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Option(Box::new(TyPat::Var(0))),
                    depth,
                    nesting - 1,
                );
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.data_op(b, scope, &IrDataType::String, depth, nesting - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let (operand, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Array(Box::new(TyPat::Var(0))),
                    depth,
                    nesting - 1,
                );
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let (operand, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Array(Box::new(TyPat::Var(0))),
                    depth,
                    nesting - 1,
                );
                b.array_length(operand)
            }
            P::ArrayGet => {
                let IrDataType::Option(element) = target else {
                    unreachable!("ArrayGet is offered for an Option only")
                };
                let (array, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Ground(IrDataType::Array(element.clone())),
                    depth,
                    nesting - 1,
                );
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
                let (array, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Ground(IrDataType::Array(Box::new(types[1].clone()))),
                    depth,
                    nesting - 1,
                );
                b.array_enumerate(array)
            }
            P::ArrayFromRange => {
                let start = b.int(self.zigzag(3));
                let start = self.bind(b, scope, start);
                let end = b.int(self.zigzag(3));
                let end = self.bind(b, scope, end);
                b.array_from_range(start, end)
            }
            P::Match => self.match_(b, scope, &typ, depth, nesting),
            P::Neg(row) => {
                let operand = self.data_op(b, scope, target, depth, nesting - 1);
                b.neg(*row, operand)
            }
            P::Add(row) => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.add(*row, left, right)
            }
            P::Sub(row) => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.sub(*row, left, right)
            }
            P::Mul(row) => {
                let left = self.data_op(b, scope, target, depth, nesting - 1);
                let right = self.data_op(b, scope, target, depth, nesting - 1);
                b.mul(*row, left, right)
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
        if !scope.modifier.allows(IrModifier::Render) || !self.chance(CELL_ODDS) {
            return value;
        }
        let cell = scope.push(b.cell(|_| value));
        scope.push(b.cell_read(cell))
    }

    fn call(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &IrType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let callees = Self::callees(b, scope, target);
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
    use crate::ir::flat_program::{FlatBinding, FlatBlock, FlatOp};
    use expect_test::{Expect, expect};
    use rand::RngExt;
    use std::collections::{BTreeMap, HashSet};

    fn check(seed: u64, len: usize, expected: Expect) {
        let mut rng = SmallRng::seed_from_u64(seed);
        let bytes: Vec<u8> = (0..len).map(|_| rng.random()).collect();
        let program = random_program(&mut Unstructured::new(&bytes));
        expected.assert_eq(&format!("{}\n{program}", program.types));
    }

    fn kind(binding: &FlatBinding) -> String {
        let debug = match &binding.op {
            FlatOp::Match(_) => "Match".to_string(),
            FlatOp::Cell(_) => "Cell".to_string(),
            FlatOp::For(_) => "For".to_string(),
            FlatOp::Lambda(_) => "Lambda".to_string(),
            op => format!("{op:?}"),
        };
        let kind = debug
            .trim_start_matches("Data(")
            .split('(')
            .next()
            .unwrap()
            .trim_end_matches(')');
        let typ = &binding.binder.typ;
        match &binding.op {
            FlatOp::Lambda(_)
                if matches!(typ, IrType::Function(signature)
                    if signature.modifier == IrModifier::Mut) =>
            {
                format!("Mut{kind}")
            }
            FlatOp::Call(..) | FlatOp::Match(_) => kind.to_string(),
            _ if typ.is_html_or_html_closure() => format!("Html{kind}"),
            _ => kind.to_string(),
        }
    }

    fn nested(binding: &FlatBinding) -> Vec<(Vec<ValueId>, &FlatBlock)> {
        let bound = |pattern: &crate::ir::ir_match::IrPattern| {
            let mut names = Vec::new();
            pattern.for_each_binding(&mut |binder| names.push(binder.var));
            names
        };
        match &binding.op {
            FlatOp::Match(match_) => match_
                .arms
                .iter()
                .map(|arm| (bound(&arm.pattern), &arm.body))
                .collect(),
            FlatOp::For(for_) => {
                let var: Vec<ValueId> = for_.var.iter().map(|binder| binder.var).collect();
                let mut nested = Vec::new();
                if let Some(key) = &for_.key {
                    nested.push((var.clone(), key));
                }
                nested.push((var, &for_.body));
                nested
            }
            FlatOp::Lambda(lambda) => {
                let params = lambda.parameters.iter().map(|param| param.var).collect();
                vec![(params, &lambda.body)]
            }
            FlatOp::Cell(initial) => vec![(Vec::new(), initial)],
            _ => Vec::new(),
        }
    }

    fn mark_live(block: &FlatBlock, live: &mut HashSet<ValueId>, keep: bool) {
        live.insert(block.result);
        for binding in block.bindings.iter().rev() {
            if keep {
                live.insert(binding.binder.var);
            } else if !live.contains(&binding.binder.var) {
                continue;
            }
            let mutates = matches!(
                (&binding.op, &binding.binder.typ),
                (FlatOp::Lambda(_), IrType::Function(signature))
                    if signature.modifier == IrModifier::Mut
            );
            for (_, body) in nested(binding) {
                mark_live(body, live, keep || mutates);
            }
            binding.op.for_each_operand(&mut |operand| {
                live.insert(operand);
            });
        }
    }

    fn mark_dynamic(block: &FlatBlock, dynamic: &mut HashSet<ValueId>) -> bool {
        for binding in &block.bindings {
            let mut depends = false;
            binding.op.for_each_operand(&mut |operand| {
                depends |= dynamic.contains(&operand);
            });
            // The parameters of a lambda take what each application passes.
            let operands = depends || matches!(binding.op, FlatOp::Lambda(_));
            for (bound, body) in nested(binding) {
                if operands {
                    dynamic.extend(bound);
                }
                depends |= mark_dynamic(body, dynamic);
            }
            if depends {
                dynamic.insert(binding.binder.var);
            }
        }
        dynamic.contains(&block.result)
    }

    fn count_ops(
        block: &FlatBlock,
        live: &HashSet<ValueId>,
        dynamic: &HashSet<ValueId>,
        counts: &mut BTreeMap<String, [usize; 3]>,
    ) {
        for binding in &block.bindings {
            let column = counts.entry(kind(binding)).or_default();
            column[0] += 1;
            if live.contains(&binding.binder.var) {
                column[1] += 1;
                if dynamic.contains(&binding.binder.var) {
                    column[2] += 1;
                }
            }
            for (_, body) in nested(binding) {
                count_ops(body, live, dynamic, counts);
            }
        }
    }

    fn count_references(
        block: &FlatBlock,
        references: &mut HashMap<ValueId, usize>,
        html: &mut HashSet<ValueId>,
        ids: &mut HashSet<ValueId>,
    ) {
        for binding in &block.bindings {
            match binding.op {
                FlatOp::Mint => {
                    ids.insert(binding.binder.var);
                }
                _ if binding.binder.typ.is_html_or_html_closure() => {
                    html.insert(binding.binder.var);
                }
                _ => {}
            }
            binding.op.for_each_operand(&mut |operand| {
                *references.entry(operand).or_default() += 1;
            });
            for (_, body) in nested(binding) {
                count_references(body, references, html, ids);
            }
        }
        *references.entry(block.result).or_default() += 1;
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

        fn count_block(&mut self, block: &FlatBlock) {
            let here: HashSet<ValueId> = (block.bindings.iter())
                .map(|binding| binding.binder.var)
                .collect();
            for binding in &block.bindings {
                match &binding.op {
                    FlatOp::For(for_) => {
                        self.loops += 1;
                        self.keyed_loops += usize::from(for_.key.is_some());
                    }
                    FlatOp::Element(element) => {
                        self.elements += 1;
                        if let Some(handler) = element.on_click {
                            self.clicks += 1;
                            *self.handler_clicks.entry(handler).or_default() += 1;
                            self.clicks_from_outside += usize::from(!here.contains(&handler));
                            self.clicks_on_parameters +=
                                usize::from(self.parameters.contains(&handler));
                        }
                    }
                    FlatOp::Lambda(lambda) => {
                        (self.parameters).extend(lambda.parameters.iter().map(|param| param.var));
                    }
                    FlatOp::Cell(_) => self.cells += 1,
                    FlatOp::Match(match_) => {
                        assert!(match_.arms.len() > 1, "generated matches branch");
                        self.arm_counts.push(match_.arms.len());
                        for arm in &match_.arms {
                            let mut binds = false;
                            arm.pattern.for_each_binding(&mut |_| binds = true);
                            self.binding_arms += binds as usize;
                        }
                    }
                    _ => {}
                }
                for (_, body) in nested(binding) {
                    self.count_block(body);
                }
            }
        }
    }

    #[test]
    fn should_report_generator_stats() {
        const SAMPLES: usize = 32;
        const INPUT_LEN: usize = 65_536;
        let mut rng = SmallRng::seed_from_u64(0);
        let mut counts = BTreeMap::new();
        let mut consumed = 0;
        let mut most_consumed = 0;
        let mut exhausted = 0;
        const SIZE_EDGES: [usize; 5] = [256, 1024, 4096, 16384, INPUT_LEN];
        let mut sizes = [0usize; SIZE_EDGES.len() + 1];
        let mut html_bindings = 0;
        let mut shared_html = 0;
        let mut id_bindings = 0;
        let mut shared_ids = 0;
        let mut shape_stats = ShapeStats::default();
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
            let mut live = HashSet::new();
            let mut dynamic = HashSet::new();
            let bodies: Vec<&FlatBlock> = (program.functions.iter())
                .map(|function| &function.body)
                .chain(
                    program
                        .entrypoints
                        .iter()
                        .map(|entrypoint| &entrypoint.body),
                )
                .collect();
            let parameters = program
                .functions
                .iter()
                .map(|function| function.parameters.clone())
                .chain(program.entrypoints.iter().map(|entrypoint| {
                    entrypoint
                        .parameters
                        .iter()
                        .map(|param| param.widen())
                        .collect()
                }));
            for (parameters, &body) in parameters.zip(&bodies) {
                mark_live(body, &mut live, false);
                dynamic.extend(parameters.iter().map(|param| param.binder.var));
                mark_dynamic(body, &mut dynamic);
                (shape_stats.parameters).extend(parameters.iter().map(|param| param.binder.var));
                shape_stats.count_block(body);
            }
            shape_stats.finish_program();
            let mut program_counts = BTreeMap::new();
            for &body in &bodies {
                count_ops(body, &live, &dynamic, &mut program_counts);
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
            let mut ids = HashSet::new();
            for &body in &bodies {
                count_references(body, &mut references, &mut html, &mut ids);
            }
            id_bindings += ids.len();
            shared_ids += ids
                .iter()
                .filter(|name| references.get(name).copied().unwrap_or(0) > 1)
                .count();
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
             {shared_ids} of {id_bindings} mints have more than one reference\n\
             {:.1} arms per match on average, at least {fewest_arms} and at most {most_arms}\n\
             {binding_arms} of {arms} match arms bind names\n\
             {cells} cells\n\
             {keyed_loops} of {loops} loops have a key\n\
             {clicks} of {elements} elements have a click\n\
             {shared_handlers} handlers have more than one click\n\
             {clicks_from_outside} clicks apply a handler bound outside their block, \
             {clicks_on_parameters} of them a parameter\n\n\
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
            12742 of 65536 input bytes consumed per program on average, at most 65536
            1 of 32 programs exhaust the input

            -- sizes --
            under 256     6  #################
            under 1K      1  ##
            under 4K      3  ########
            under 16K    14  ########################################
            under 64K     7  ####################
            at 64K        1  ##

            -- programs --
            66 of 2529 Html bindings have more than one reference
            3 of 626 mints have more than one reference
            2.4 arms per match on average, at least 2 and at most 13
            5886 of 14819 match arms bind names
            11106 cells
            112 of 193 loops have a key
            402 of 505 elements have a click
            58 handlers have more than one click
            162 clicks apply a handler bound outside their block, 58 of them a parameter

            -- ops --
            op                   ops/program % progs  % live   % dyn
            Apply                      187.1    84.4    85.7    65.2
            Array                      218.6    84.4    81.2    26.6
            ArrayEnumerate               1.6    34.4    64.7    43.1
            ArrayFromRange              12.3    87.5    84.2     0.0
            ArrayGet                    43.0    81.2    81.9    49.6
            ArrayIsEmpty                 2.4    68.8    90.9    62.3
            ArrayLength                108.3    84.4    87.2    55.9
            BoolLiteral                 62.4    81.2    73.3     0.0
            BoolNegation                 2.6    62.5    91.5    59.8
            Call                        10.3    43.8    93.1    67.4
            Cell                       347.1    96.9    85.2    44.2
            CellRead                   353.8    96.9    85.4    44.4
            CellWrite                  103.1    75.0    94.4    64.2
            Enum                       106.2    68.8    83.4    26.6
            Equals                       2.3    62.5    90.7    62.7
            FieldAccess                 15.4    43.8    85.0    65.9
            FloatLiteral               115.4    87.5    86.6     0.0
            FloatToInt                 101.4    87.5    88.1    45.8
            HtmlApply                    3.8    68.8    95.9    72.1
            HtmlConcat                  20.5    84.4    98.5    45.3
            HtmlElement                 15.8    81.2    98.4    66.9
            HtmlEscape                  10.0    93.8    97.5    46.7
            HtmlFor                      6.0    75.0    94.3    65.3
            HtmlLambda                   8.0    65.6    98.1    57.6
            HtmlText                     4.9    71.9    97.4     0.0
            IntLiteral                 137.7    93.8    82.1     0.0
            IntToFloat                 128.8    84.4    87.1    50.6
            IntToString                 52.7    90.6    93.4    53.6
            Lambda                     205.8    84.4    85.4    48.6
            LessThan                     2.2    59.4    87.0    62.3
            LessThanOrEqual              2.2    62.5    85.5    68.1
            Match                      189.3    84.4    86.5    79.0
            Mint                        19.6    93.8    78.4     0.0
            MutLambda                   32.0    90.6    90.6    38.6
            NumericAdd                 245.8    87.5    87.0    63.0
            NumericMultiply            233.2    81.2    87.0    63.2
            NumericNegation            238.1    87.5    87.1    51.6
            NumericSubtract            240.9    84.4    86.9    63.3
            OptionIsNone                 2.7    59.4    87.2    65.1
            OptionIsSome                 1.9    59.4    85.2    62.3
            OptionNone                  51.7    81.2    74.9     0.0
            OptionSome                  48.1    81.2    75.6    26.3
            Record                      87.4    62.5    78.2    26.5
            StringConcat                53.8    78.1    93.8    47.9
            StringIsEmpty                1.9    65.6    86.9    44.3
            StringLiteral               60.0    90.6    89.3     0.0
            Tuple                      186.4    90.6    81.0    29.3
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
                  let v0: ElementId = mint()
                  let v1: Int = -2147483648
                  let v2: Cell[Int] = cell {
                    v1
                  }
                  let v3: Int = cell_read(v2)
                  let v4: Html = concat()
                  v4
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
                  f1: Array[Tuple[]],
                  f2: Tuple[Float, Int, R1],
                }

                record R1 {
                  f0: Tuple[Tuple[], Option[R1]],
                  f1: Tuple[Option[E1], Float],
                }

                enum E0 {
                  W0,
                  W1 {f0: Tuple[Array[String], Float, Option[E1]]},
                }

                enum E1 {
                  W0,
                  W1 {f0: Tuple[]},
                }

                render fn C0@f0(
                  p0@v0: (Tuple[Bool, E1], (Option[String]) -> R1) -> Option[R1],
                  p1@v1: Array[Array[Int]],
                  p2@v2: Html,
                  p3@v3: Html,
                  p4@v4: render () -> Tuple[],
                  p5@v5: Array[R0],
                ) -> Html {
                  let v6: ElementId = mint()
                  let v7: Int = -2147483648
                  let v8: Cell[Int] = cell {
                    v7
                  }
                  let v9: Int = cell_read(v8)
                  v2
                }
                render fn C1@f1() -> Html {
                  let v10: ElementId = mint()
                  let v11: Int = -2147483648
                  let v12: Cell[Int] = cell {
                    v11
                  }
                  let v13: Int = cell_read(v12)
                  let v14: Html = concat()
                  v14
                }
                entrypoint V0@e0() -> Html {
                  let v15: ElementId = mint()
                  let v16: Int = -2147483648
                  let v17: Cell[Int] = cell {
                    v16
                  }
                  let v18: Int = cell_read(v17)
                  let v19: Html = concat()
                  v19
                }
                entrypoint V1@e1(
                  p0@v20: Array[Tuple[E1, R0]],
                  p1@v21: String,
                  p2@v22: R1,
                  p3@v23: Tuple[Float, Option[R1]],
                ) -> Html {
                  let v24: ElementId = mint()
                  let v25: Int = -2147483648
                  let v26: Cell[Int] = cell {
                    v25
                  }
                  let v27: Int = cell_read(v26)
                  let v28: Html = concat()
                  v28
                }
                entrypoint V2@e2(
                  p0@v29: Array[Tuple[Int]],
                  p1@v30: Int,
                ) -> Html {
                  let v31: ElementId = mint()
                  let v32: Cell[Int] = cell {
                    v30
                  }
                  let v33: Int = cell_read(v32)
                  let v34: Html = concat()
                  v34
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

                entrypoint V0@e0(
                  p0@v0: Option[Int],
                  p1@v1: Option[Array[Int]],
                ) -> Html {
                  let v2: Int = -1
                  let v3: Int = 2
                  let v4: Array[Int] = array_from_range(v2, v3)
                  let v8: Html = for v5: Int in v4 key {
                    v5
                  } {
                    let v6: String = "`${x}`"
                    let v7: Html = escape(v6)
                    v7
                  }
                  v8
                }
                entrypoint V1@e1() -> Html {
                  let v9: String = "&amp;"
                  let v10: Html = escape(v9)
                  v10
                }
            "#]],
        );
    }
}
