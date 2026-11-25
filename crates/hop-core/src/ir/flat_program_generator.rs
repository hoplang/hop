use crate::hop::typing::{ComparableType, EquatableType, NumericType, Ty};
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_html_element::FlatAttribute;
use crate::ir::flat_program::FlatProgram;
use crate::ir::flat_program_builder::{
    DeclaredFunction, FlatBlockBuilder, FlatProgramBuilder, Pat,
};
use crate::ir::flat_type::{FlatDataType, FlatSignature, FlatType, IrModifier};
use crate::ir::flat_type_declarations::{FlatField, FlatTypeDeclarations};
use crate::ir::flat_type_declarations_builder::FlatTypeDeclarationsBuilder;
use crate::ir::runtime::random::random_value;
use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use arbitrary::Unstructured;
use rand::{SeedableRng, rngs::SmallRng};
use std::borrow::Cow;
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

/// How often a function or a closure that returns a value returns a
/// closure instead.
const CLOSURE_RESULT_ODDS: u8 = 48;

/// How often a closure is produced by a call of a function or an
/// application of a closure in scope that returns one, if any does, rather
/// than by a lambda.
const PRODUCED_CLOSURE_ODDS: u8 = 128;

/// How often a closure is produced by a match, each arm producing one,
/// rather than by a call, an application or a lambda.
const MATCHED_CLOSURE_ODDS: u8 = 32;

/// How often a loop that binds its variable has a key.
const KEY_ODDS: u8 = 204;

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
    /// Whether the path has used the Html.
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

    fn inner(&self, types: &[FlatType], modifier: IrModifier) -> Scope {
        Scope {
            names: (self.names.iter().copied())
                .filter(|name| types[name.var.index()] != FlatType::Html)
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
        types: &'a [FlatType],
        fits: impl Fn(&FlatType) -> bool + 'a,
    ) -> impl Iterator<Item = ValueId> + 'a {
        self.names
            .iter()
            .filter(move |name| !name.used && fits(&types[name.var.index()]))
            .map(|name| name.var)
    }
}

enum TyPat {
    Var(usize),
    Ground(FlatDataType),
    Array(Box<TyPat>),
    Option(Box<TyPat>),
}

enum Layer {
    Data,
    Cell,
}

type Subst = HashMap<usize, FlatDataType>;

/// Whether typ has the shape of pattern, binding the variables of the
/// pattern in subst.
fn match_type(pattern: &TyPat, typ: &FlatDataType, subst: &mut Subst) -> bool {
    match (pattern, typ) {
        (TyPat::Var(var), _) => subst.entry(*var).or_insert_with(|| typ.clone()) == typ,
        (TyPat::Ground(ground), _) => ground == typ,
        (TyPat::Array(element), FlatDataType::Array(typ))
        | (TyPat::Option(element), FlatDataType::Option(typ)) => match_type(element, typ, subst),
        _ => false,
    }
}

/// The data type pattern stands for under subst, binding each variable
/// still unbound to a type from fresh.
fn instantiate(
    pattern: &TyPat,
    subst: &mut Subst,
    fresh: &mut impl FnMut() -> FlatDataType,
) -> FlatDataType {
    match pattern {
        TyPat::Var(var) => subst.entry(*var).or_insert_with(&mut *fresh).clone(),
        TyPat::Ground(typ) => typ.clone(),
        TyPat::Array(element) => FlatDataType::Array(Box::new(instantiate(element, subst, fresh))),
        TyPat::Option(element) => {
            FlatDataType::Option(Box::new(instantiate(element, subst, fresh)))
        }
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
    /// Only names whose types hold no closure, at a position of a type that
    /// holds none either.
    Plain,
}

/// The pattern of an arm and the names it binds.
type Arm = (Pat, Vec<String>);

/// A constructor of a pattern, which takes the patterns of its fields.
enum Ctor<'a> {
    True,
    False,
    None,
    Some,
    Tuple,
    Record,
    Variant(&'a str),
}

impl Ctor<'_> {
    fn apply(&self, fields: Vec<(String, Pat)>) -> Pat {
        match self {
            Ctor::True => Pat::True,
            Ctor::False => Pat::False,
            Ctor::None => Pat::None,
            Ctor::Some => Pat::some(fields.into_iter().next().unwrap().1),
            Ctor::Tuple => Pat::Tuple(fields.into_iter().map(|(_, pat)| pat).collect()),
            Ctor::Record => Pat::Record(fields),
            Ctor::Variant(name) => Pat::Variant(name.to_string(), fields),
        }
    }
}

struct FlatGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    declared_names: Vec<String>,
    record_names: Vec<String>,
    enum_names: Vec<String>,
    records: Vec<FlatDataType>,
    /// The declared names whose types hold no closure.
    plain_names: Vec<String>,
    types: FlatTypeDeclarations,
}

fn handler_type() -> FlatType {
    FlatType::function(IrModifier::Mut, [], FlatType::Data(FlatDataType::UNIT))
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
        plain_names: Vec::new(),
        types: FlatTypeDeclarations::default(),
    };

    // Name every type before generating any field types, so that a field can
    // refer to a type declared later or to the one being declared.
    let record_count = g.count(0..=3);
    let enum_count = g.count(0..=3);
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut types = FlatTypeDeclarationsBuilder::new();

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
    g.plain_names = (g.declared_names.iter())
        .filter(|name| !g.types.holds_closure(&g.data_type(Ty::named(name))))
        .cloned()
        .collect();
    let mut builder = FlatProgramBuilder::with_types(g.types.clone());

    // Declare every function before defining any, so that a body can call a
    // function declared after it.
    let function_names: Vec<String> = (0..g.count(0..=2)).map(|i| format!("C{i}")).collect();
    for name in &function_names {
        let html = g.coin();
        let render_modifier = if html {
            g.index(4) != 0
        } else {
            g.chance(RENDER_VALUE_ODDS)
        };
        let return_type = if html {
            FlatType::HTML
        } else {
            g.random_result_type(render_modifier, 1)
        };
        let modifier = if render_modifier {
            IrModifier::Render
        } else if g.chance(MUT_FUNCTION_ODDS) {
            IrModifier::Mut
        } else {
            IrModifier::Pure
        };
        let params: Vec<(String, FlatType)> = (0..g.count(0..=8))
            .map(|i| {
                let typ = if g.index(4) == 0 {
                    g.random_closure_type(true, render_modifier, 1)
                } else if g.index(8) == 0 {
                    handler_type()
                } else {
                    let ty = g.random_type_where(2, NamedTypes::Any, true);
                    g.flat_type(ty)
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
                            g.random_type_where(2, NamedTypes::Plain, false),
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
        .iter()
        .enumerate()
        .map(|(index, decl)| {
            let args = decl
                .parameters
                .iter()
                .map(|param| {
                    let value = random_value(&mut rng, &param.binder.typ, &program.types);
                    (param.name.clone(), value)
                })
                .collect();
            (EntrypointId::new(index), args)
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

    fn flat_type(&self, ty: Ty) -> FlatType {
        self.types.ty(ty)
    }

    fn data_type(&self, ty: Ty) -> FlatDataType {
        let FlatType::Data(typ) = self.flat_type(ty) else {
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
        target: &'s FlatDataType,
    ) -> impl Iterator<Item = (&'s FlatDataType, &'s FlatField)> + 's {
        let types = b.types();
        self.records.iter().flat_map(move |record| {
            let FlatDataType::Record(name) = record else {
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
        target: &FlatType,
    ) -> Vec<&'t DeclaredFunction> {
        (b.functions().iter())
            .filter(|function| {
                function.return_type == *target && scope.modifier.allows(function.modifier)
            })
            .collect()
    }

    fn closure_parameters(typ: &FlatType) -> Vec<FlatType> {
        match typ {
            FlatType::Data(FlatDataType::Function(signature)) => signature.parameters.clone(),
            _ => unreachable!("{typ} is not a closure type"),
        }
    }

    fn var_in_scope(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &Scope,
        fits: impl Fn(&FlatDataType) -> bool,
    ) -> Option<(ValueId, FlatDataType)> {
        let types = b.value_types();
        let mut vars: Vec<(ValueId, FlatDataType)> = (scope
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
                let ((Layer::Data, FlatType::Data(typ)) | (Layer::Cell, FlatType::Cell(typ))) =
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

    fn random_closure_type(&mut self, html: bool, render: bool, depth: usize) -> FlatType {
        FlatType::Data(FlatDataType::Function(if html && self.coin() {
            self.random_html_signature(depth)
        } else {
            self.random_value_signature(render, depth, None)
        }))
    }

    fn random_value_signature(
        &mut self,
        render: bool,
        depth: usize,
        result: Option<FlatType>,
    ) -> FlatSignature {
        let parameters = (0..self.count(0..=2))
            .map(|_| {
                if depth > 0 && self.index(4) == 0 {
                    self.random_closure_type(true, render, depth - 1)
                } else if self.index(8) == 0 {
                    handler_type()
                } else if self.index(8) == 0 {
                    FlatType::HTML
                } else {
                    FlatType::Data(self.random_data_type(1))
                }
            })
            .collect();
        let result = result.unwrap_or_else(|| self.random_result_type(render, depth));
        FlatSignature {
            modifier: if render && self.chance(RENDER_VALUE_ODDS) {
                IrModifier::Render
            } else {
                IrModifier::Pure
            },
            parameters,
            result: Box::new(result),
        }
    }

    fn random_result_type(&mut self, render: bool, depth: usize) -> FlatType {
        if depth > 0 && self.chance(CLOSURE_RESULT_ODDS) {
            self.random_closure_type(true, render, depth - 1)
        } else {
            FlatType::Data(self.random_data_type(1))
        }
    }

    fn random_html_signature(&mut self, depth: usize) -> FlatSignature {
        let parameters = (0..self.count(0..=2))
            .map(|_| match self.index(if depth > 0 { 4 } else { 2 }) {
                0 => FlatType::Data(self.random_data_type(1)),
                1 => FlatType::HTML,
                2 => self.random_closure_type(false, true, depth - 1),
                _ => self.random_closure_type(true, true, depth - 1),
            })
            .collect();
        FlatSignature {
            modifier: if self.index(4) != 0 {
                IrModifier::Render
            } else {
                IrModifier::Pure
            },
            parameters,
            result: Box::new(FlatType::Html),
        }
    }

    fn random_type(&mut self, depth: usize) -> Ty {
        self.random_type_where(depth, NamedTypes::Any, true)
    }

    fn random_data_type(&mut self, depth: usize) -> FlatDataType {
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
            Function,
        }
        // What an array or an option holds can be of a type whose
        // declaration is not complete, as an empty array and None end it.
        let held = match named {
            NamedTypes::Any | NamedTypes::Complete => NamedTypes::Any,
            NamedTypes::Plain => NamedTypes::Plain,
        };
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
        if depth > 0 && !matches!(named, NamedTypes::Plain) {
            kinds.push(P::Function);
        }
        match self.u.choose(&kinds).unwrap() {
            P::Scalar => SCALARS[self.index(SCALARS.len())].clone(),
            P::Html => Ty::Html,
            P::Named => {
                let i = self.index(named_count);
                self.named_type(named, i)
            }
            P::Array => Ty::array(self.random_type_where(depth - 1, held, false)),
            P::Option => Ty::option(self.random_type_where(depth - 1, held, false)),
            P::Tuple => {
                let arity = self.count(0..=3);
                Ty::tuple((0..arity).map(|_| self.random_type_where(depth - 1, named, false)))
            }
            P::Function => {
                let parameters: Vec<Ty> = (0..self.count(0..=2))
                    .map(|_| self.random_type_where(depth - 1, NamedTypes::Any, true))
                    .collect();
                match self.index(4) {
                    0 => Ty::mut_function([], Ty::tuple([])),
                    1 if self.index(4) == 0 => Ty::function(parameters, Ty::Html),
                    1 => Ty::render_function(parameters, Ty::Html),
                    _ => {
                        let result = self.random_type_where(depth - 1, named, false);
                        match self.index(4) {
                            0 => Ty::render_function(parameters, result),
                            1 => Ty::mut_function(parameters, result),
                            _ => Ty::function(parameters, result),
                        }
                    }
                }
            }
        }
    }

    fn named_type_count(&self, named: NamedTypes) -> usize {
        match named {
            NamedTypes::Any => self.declared_names.len(),
            NamedTypes::Complete => self.record_names.len() + self.enum_names.len(),
            NamedTypes::Plain => self.plain_names.len(),
        }
    }

    fn named_type(&self, named: NamedTypes, i: usize) -> Ty {
        match named {
            NamedTypes::Any => Ty::named(&self.declared_names[i]),
            NamedTypes::Complete if i < self.record_names.len() => Ty::named(&self.record_names[i]),
            NamedTypes::Complete => Ty::named(&self.enum_names[i - self.record_names.len()]),
            NamedTypes::Plain => Ty::named(&self.plain_names[i]),
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
        if (typ == FlatType::INT || typ == FlatType::STRING) && self.index(4) != 0 {
            return var;
        }
        let target = if self.coin() {
            FlatDataType::Int
        } else {
            FlatDataType::String
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
        if self.chance(DEAD_BINDING_ODDS) {
            let ty = self.random_type(1);
            let typ = self.flat_type(ty);
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
        target: &FlatDataType,
        depth: usize,
    ) -> ValueId {
        if self.chance(DEAD_BINDING_ODDS) {
            let ty = self.random_type(1);
            let typ = self.flat_type(ty);
            let dead = self.op_of_type(b, scope, &typ, depth, MAX_EXPRESSION_DEPTH);
            // Html that nothing has read yet, which a later op may pass on.
            scope.set_used(dead, false);
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
            let typ = FlatType::Data(subst[&0].clone());
            let update_type = FlatType::function(IrModifier::Pure, [typ.clone()], typ);
            let update = self.op_of_type(b, scope, &update_type, depth, nesting - 1);
            b.cell_update(cell, update);
        }
    }

    fn block_of_type(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &FlatType,
        depth: usize,
    ) -> ValueId {
        match typ {
            FlatType::Data(target) => self.data_block(b, scope, target, depth),
            FlatType::Html => self.html_block(b, scope, depth),
            FlatType::Cell(_) => unreachable!("a block produces no cell"),
        }
    }

    fn op_of_type(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &FlatType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        match typ {
            FlatType::Data(target) => self.data_op(b, scope, target, depth, nesting),
            FlatType::Html => self.html_op(b, scope, depth, nesting),
            FlatType::Cell(_) => unreachable!("a cell is bound for a block, not asked for"),
        }
    }

    // The body of a lambda goes on with the expression, a level further
    // from its leaves, rather than start a block afresh, so that lambdas in
    // lambdas stop at the leaves.
    fn closure_op(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &FlatType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(b, scope, typ) {
            return name;
        }
        if depth > 0 && nesting > 0 && self.chance(MATCHED_CLOSURE_ODDS) {
            let value = self.match_(b, scope, typ, depth, nesting);
            return scope.push(value);
        }
        if nesting > 0 && self.chance(PRODUCED_CLOSURE_ODDS) {
            let types = b.value_types();
            let producers: Vec<ValueId> = (scope.names_where(&types, |candidate| {
                matches!(candidate, FlatType::Data(FlatDataType::Function(signature))
                    if *signature.result == *typ && scope.modifier.allows(signature.modifier))
            }))
            .collect();
            drop(types);
            let callees = Self::callees(b, scope, typ).len();
            let choice = self.index(callees + producers.len() + 1);
            if choice < callees {
                let value = self.call(b, scope, typ, depth, nesting);
                return scope.push(value);
            }
            if let Some(&closure) = producers.get(choice - callees) {
                let parameters = Self::closure_parameters(&b.value_types()[closure.index()]);
                let args: Vec<ValueId> = (parameters.iter())
                    .map(|parameter| self.op_of_type(b, scope, parameter, depth, nesting - 1))
                    .collect();
                return scope.push(b.apply(closure, args));
            }
        }
        let parameters = Self::closure_parameters(typ);
        let FlatType::Data(FlatDataType::Function(signature)) = typ else {
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
                FlatType::Data(result) => {
                    if mutates {
                        self.writes(b, &mut inner, 1..=3, depth, nesting);
                    }
                    self.data_op(b, &mut inner, result, depth, nesting)
                }
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
        result: &FlatType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        let fits = |typ: &FlatType| match typ {
            FlatType::Data(FlatDataType::Function(signature)) => {
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
            let signature = match result {
                FlatType::Data(FlatDataType::Function(_)) => self.random_value_signature(
                    scope.modifier.allows(IrModifier::Render),
                    0,
                    Some(result.clone()),
                ),
                FlatType::Data(_) => {
                    let mut signature = self.random_value_signature(
                        scope.modifier.allows(IrModifier::Render),
                        1,
                        Some(result.clone()),
                    );
                    if scope.modifier.allows(IrModifier::Mut) && self.chance(MUT_ODDS) {
                        signature.modifier = IrModifier::Mut;
                    }
                    signature
                }
                _ => self.random_html_signature(1),
            };
            let typ = FlatType::Data(FlatDataType::Function(signature));
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
        typ: &FlatType,
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
        let value = match self.name_in_scope(b, scope, &FlatType::HTML) {
            Some(name) => name,
            None => {
                let literals = if scope.modifier == IrModifier::Render {
                    3
                } else {
                    2
                };
                let literal = match self.index(literals) {
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
        attributes: Vec<(AttributeName, FlatAttribute)>,
        children: Option<ValueId>,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if !self.chance(CLICK_ODDS) {
            return b.element(tag, attributes, children);
        }
        let handler = self.op_of_type(b, scope, &handler_type(), depth, nesting);
        b.element_on_click(tag, attributes, handler, children)
    }

    fn data_leaf(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &FlatDataType,
        depth: usize,
    ) -> ValueId {
        if let Some(name) = self.name_in_scope(b, scope, &FlatType::Data(typ.clone())) {
            return self.cell(b, scope, name);
        }
        self.literal(b, scope, typ, depth)
    }

    fn literal(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        typ: &FlatDataType,
        depth: usize,
    ) -> ValueId {
        let literal = match typ {
            FlatDataType::String => b.str(self.u.choose(STRING_LITERALS).unwrap()),
            FlatDataType::Int => b.int(match self.index(8) {
                0 => *self
                    .u
                    .choose(&[i32::MIN, i32::MIN + 1, i32::MAX - 1, i32::MAX])
                    .unwrap(),
                1 | 2 => self.zigzag(100),
                _ => self.zigzag(2),
            }),
            FlatDataType::Bool => b.bool(self.coin()),
            FlatDataType::Float => b.float(*self.u.choose(FLOATS).unwrap()),
            FlatDataType::Array(inner) => {
                let len = if depth == 0 { 0 } else { self.count(0..=3) };
                let elements: Vec<_> = (0..len)
                    .map(|_| self.data_leaf(b, scope, inner, depth.saturating_sub(1)))
                    .collect();
                if elements.is_empty() {
                    b.empty_array(FlatType::Data((**inner).clone()))
                } else {
                    b.array(elements)
                }
            }
            FlatDataType::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.data_leaf(b, scope, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none(FlatType::Data((**inner).clone()))
                }
            }
            FlatDataType::Record(name) => {
                let fields = &b.types().record(name).unwrap().fields;
                let fields = self.shuffled(fields.iter().collect());
                let mut values = Vec::new();
                for field in fields {
                    values.push((
                        field.name.as_str(),
                        self.data_leaf(b, scope, &field.typ, depth.saturating_sub(1)),
                    ));
                }
                b.record(FlatType::Data(typ.clone()), values)
            }
            FlatDataType::Enum(name) => {
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
                    FlatType::Data(typ.clone()),
                    variant.name.as_str(),
                    values,
                )
            }
            FlatDataType::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|element| self.data_leaf(b, scope, element, depth.saturating_sub(1)))
                    .collect();
                b.tuple(values)
            }
            FlatDataType::Function(signature) => {
                let mut inner = scope.inner(&b.value_types(), signature.modifier);
                let depth = depth.saturating_sub(1);
                let body = |b: &FlatBlockBuilder<'_>, params: &[ValueId]| {
                    for param in params {
                        inner.push(*param);
                    }
                    match &*signature.result {
                        FlatType::Data(result) => self.data_leaf(b, &mut inner, result, depth),
                        _ => self.html_leaf(b, &mut inner, depth),
                    }
                };
                let parameters = signature.parameters.clone();
                match signature.modifier {
                    IrModifier::Mut => b.mut_lambda(parameters, body),
                    IrModifier::Render => b.render_lambda(parameters, body),
                    IrModifier::Pure => b.lambda(parameters, body),
                    IrModifier::Test => unreachable!("only a declared function tests"),
                }
            }
        };
        self.bind(b, scope, literal)
    }

    fn constructors<'t>(
        types: &'t FlatTypeDeclarations,
        typ: &'t FlatDataType,
    ) -> Vec<(Ctor<'t>, Vec<(Cow<'t, str>, &'t FlatDataType)>)> {
        let fields = |fields: &'t [FlatField]| {
            fields
                .iter()
                .map(|field| (Cow::Borrowed(field.name.as_str()), &field.typ))
                .collect()
        };
        match typ {
            FlatDataType::Bool => vec![(Ctor::True, Vec::new()), (Ctor::False, Vec::new())],
            FlatDataType::Option(inner) => vec![
                (Ctor::None, Vec::new()),
                (Ctor::Some, vec![(Cow::Borrowed("0"), &**inner)]),
            ],
            FlatDataType::Tuple(elements) => vec![(
                Ctor::Tuple,
                elements
                    .iter()
                    .enumerate()
                    .map(|(i, element)| (Cow::Owned(i.to_string()), element))
                    .collect(),
            )],
            FlatDataType::Record(name) => {
                let record = types.record(name).expect("a record is declared");
                vec![(Ctor::Record, fields(&record.fields))]
            }
            FlatDataType::Enum(name) => types
                .enumeration(name)
                .expect("an enum is declared")
                .variants
                .iter()
                .map(|variant| {
                    (
                        Ctor::Variant(variant.name.as_str()),
                        fields(&variant.fields),
                    )
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    fn can_branch(types: &FlatTypeDeclarations, typ: &FlatDataType, depth: usize) -> bool {
        if depth == 0 {
            return false;
        }
        let can_branch = |field: &FlatDataType| Self::can_branch(types, field, depth - 1);
        match typ {
            FlatDataType::Bool | FlatDataType::Option(_) => true,
            FlatDataType::Tuple(elements) => elements.iter().any(can_branch),
            FlatDataType::Record(name) => types
                .record(name)
                .expect("a record is declared")
                .fields
                .iter()
                .any(|field| can_branch(&field.typ)),
            FlatDataType::Enum(name) => {
                let variants = &types
                    .enumeration(name)
                    .expect("an enum is declared")
                    .variants;
                variants.len() > 1
                    || variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .any(|field| can_branch(&field.typ))
            }
            FlatDataType::String
            | FlatDataType::Int
            | FlatDataType::Float
            | FlatDataType::Array(_)
            | FlatDataType::Function(_) => false,
        }
    }

    fn partition(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        typ: &FlatDataType,
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
                    .filter(|&i| Self::can_branch(b.types(), fields[i].1, depth - 1))
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
                            pats.push((name.to_string(), pat.clone()));
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

    /// A type a match can branch on, made of the named types given.
    fn branching_type(&mut self, types: &FlatTypeDeclarations, named: NamedTypes) -> FlatDataType {
        let names = match named {
            NamedTypes::Any | NamedTypes::Complete => &self.declared_names,
            NamedTypes::Plain => &self.plain_names,
        };
        let branching: Vec<FlatDataType> = names
            .iter()
            .map(|name| self.data_type(Ty::named(name)))
            .filter(|typ| Self::can_branch(types, typ, MAX_PATTERN_DEPTH))
            .collect();
        let base = match self.index(2 + branching.len()) {
            0 => FlatDataType::Bool,
            1 => {
                let ty = self.random_type_where(1, named, false);
                FlatDataType::Option(Box::new(self.data_type(ty)))
            }
            i => return branching[i - 2].clone(),
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
                    let ty = self.random_type_where(1, named, false);
                    self.data_type(ty)
                }
            })
            .collect();
        FlatDataType::Tuple(elements)
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
        let render = scope.modifier == IrModifier::Render;
        let mut productions = vec![P::Escape, P::Concat];
        if render {
            productions.push(P::Element);
        }
        if !Self::callees(b, scope, &FlatType::HTML).is_empty() {
            productions.push(P::Call);
        }
        if depth > 0 {
            productions.push(P::Match);
        }
        if depth > 0 && render {
            productions.extend([P::ForArray, P::ForRange, P::Apply]);
        }
        if depth > 0 && scope.modifier.allows(IrModifier::Render) && self.chance(TOGGLE_ODDS) {
            productions.push(P::Toggle);
        }
        let value = match self.u.choose(&productions).unwrap() {
            P::Escape => {
                let operand = self.data_op(b, scope, &FlatDataType::String, depth, nesting - 1);
                b.escape(operand)
            }
            P::Element => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value =
                            self.data_op(b, scope, &FlatDataType::String, depth, nesting - 1);
                        attributes.push(b.attr_value(name, value));
                    }
                }
                if self.coin() {
                    let present = self.data_op(b, scope, &FlatDataType::Bool, depth, nesting - 1);
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
            P::Call => self.call(b, scope, &FlatType::HTML, depth, nesting),
            P::Match => self.match_(b, scope, &FlatType::HTML, depth, nesting),
            P::Apply => self.apply(b, scope, &FlatType::HTML, depth, nesting),
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

    // A cell, a click handler that updates it to a literal and a match on it
    // that renders Html, in an element with the handler. The handler is in
    // scope for the arms, so a click on an element of an arm can take the
    // element away. A handler updating it to a value that a match on the cell gives
    // instead, or the handler only in scope and not around the match, makes
    // far fewer clicks switch an arm.
    fn toggle(&mut self, b: &FlatBlockBuilder<'_>, scope: &mut Scope, depth: usize) -> ValueId {
        let subject_ty = self.branching_type(b.types(), NamedTypes::Plain);
        let mut initial = scope.inner(&b.value_types(), IrModifier::Pure);
        let cell =
            scope.push(b.cell(|b| self.literal(b, &mut initial, &subject_ty, MAX_LITERAL_DEPTH)));
        let value = scope.push(b.cell_read(cell));
        let mut inner = scope.inner(&b.value_types(), IrModifier::Pure);
        let handler = b.event_handler(|b| {
            let typ = FlatType::Data(subject_ty.clone());
            let update = b.lambda([typ], |b, _| {
                self.literal(b, &mut inner, &subject_ty, MAX_LITERAL_DEPTH)
            });
            b.cell_update(cell, update)
        });
        let handler = scope.push(handler);
        let shown = self.match_on(b, scope, value, subject_ty, &FlatType::HTML, depth);
        scope.push(shown);
        scope.set_used(shown, true);
        let tags: Vec<&str> = (ELEMENTS.iter())
            .filter(|(_, void)| !void)
            .map(|(tag, _)| *tag)
            .collect();
        b.element_on_click(
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
        target: &FlatDataType,
        depth: usize,
        nesting: usize,
    ) -> ValueId {
        if matches!(target, FlatDataType::Function(_)) && self.index(4) != 0 {
            return self.closure_op(b, scope, &FlatType::Data(target.clone()), depth, nesting);
        }
        if nesting == 0 || self.index(4) == 0 {
            return self.data_leaf(b, scope, target, MAX_LITERAL_DEPTH);
        }
        enum P {
            FieldAccess,
            TupleIndex,
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
        let typ = FlatType::Data(target.clone());
        let record_fields: Vec<(FlatDataType, String)> = self
            .record_fields_of_type(b, target)
            .map(|(record, field)| (record.clone(), field.name.as_str().to_string()))
            .collect();
        let mut records: Vec<&FlatDataType> =
            (record_fields.iter()).map(|(record, _)| record).collect();
        // record_fields_of_type yields the fields of each record together,
        // so removing consecutive repeats leaves each record once.
        records.dedup();
        let objects: Vec<(ValueId, &FlatDataType)> = (records.into_iter())
            .flat_map(|record| {
                let pattern = TyPat::Ground(record.clone());
                (Self::candidates(b, scope, Layer::Data, &pattern).into_iter())
                    .map(move |(object, _)| (object, record))
            })
            .collect();
        let types = b.value_types();
        let tuples: Vec<ValueId> = (scope.names_where(&types, |typ| {
            matches!(typ, FlatType::Data(FlatDataType::Tuple(elements)) if elements.contains(target))
        }))
        .collect();
        drop(types);
        let mut productions: Vec<P> = Vec::new();
        if !objects.is_empty() {
            productions.push(P::FieldAccess);
        }
        if !tuples.is_empty() {
            productions.push(P::TupleIndex);
        }
        if *target == FlatDataType::Bool {
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
        if *target == FlatDataType::Int {
            productions.extend([
                P::ArrayLength,
                P::Neg(NumericType::Int),
                P::Add(NumericType::Int),
                P::Sub(NumericType::Int),
                P::Mul(NumericType::Int),
                P::FloatToInt,
            ]);
        }
        if *target == FlatDataType::Float {
            productions.extend([
                P::Neg(NumericType::Float),
                P::Add(NumericType::Float),
                P::Sub(NumericType::Float),
                P::Mul(NumericType::Float),
                P::IntToFloat,
            ]);
        }
        if matches!(target, FlatDataType::Array(element) if **element == FlatDataType::Int) {
            productions.push(P::ArrayFromRange);
        }
        if matches!(target, FlatDataType::Option(_)) {
            productions.push(P::ArrayGet);
        }
        if matches!(target, FlatDataType::Array(pair)
            if matches!(pair.as_ref(), FlatDataType::Tuple(types)
                if matches!(types.as_slice(), [FlatDataType::Int, _])))
        {
            productions.push(P::ArrayEnumerate);
        }
        if *target == FlatDataType::String {
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
            P::TupleIndex => {
                let tuple = tuples[self.index(tuples.len())];
                let FlatType::Data(FlatDataType::Tuple(elements)) =
                    b.value_types()[tuple.index()].clone()
                else {
                    unreachable!("{tuple} is a tuple");
                };
                let indices: Vec<usize> = (0..elements.len())
                    .filter(|index| elements[*index] == *target)
                    .collect();
                b.tuple_index(tuple, indices[self.index(indices.len())])
            }
            P::Eq => {
                let (row, ty) = match self.index(4) {
                    0 => (EquatableType::Int, FlatDataType::Int),
                    1 => (EquatableType::String, FlatDataType::String),
                    2 => (EquatableType::Bool, FlatDataType::Bool),
                    _ => (EquatableType::Float, FlatDataType::Float),
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.eq(row, left, right)
            }
            P::Lt => {
                let (row, ty) = if self.coin() {
                    (ComparableType::Int, FlatDataType::Int)
                } else {
                    (ComparableType::Float, FlatDataType::Float)
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lt(row, left, right)
            }
            P::Lte => {
                let (row, ty) = if self.coin() {
                    (ComparableType::Int, FlatDataType::Int)
                } else {
                    (ComparableType::Float, FlatDataType::Float)
                };
                let left = self.data_op(b, scope, &ty, depth, nesting - 1);
                let right = self.data_op(b, scope, &ty, depth, nesting - 1);
                b.lte(row, left, right)
            }
            P::Not => {
                let operand = self.data_op(b, scope, &FlatDataType::Bool, depth, nesting - 1);
                b.not(operand)
            }
            P::Concat => {
                let parts: Vec<_> = (0..self.count(0..=3))
                    .map(|_| self.data_op(b, scope, &FlatDataType::String, depth, nesting - 1))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.data_op(b, scope, &FlatDataType::Int, depth, nesting - 1);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.data_op(b, scope, &FlatDataType::Float, depth, nesting - 1);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.data_op(b, scope, &FlatDataType::Int, depth, nesting - 1);
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
                let operand = self.data_op(b, scope, &FlatDataType::String, depth, nesting - 1);
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
                let FlatDataType::Option(element) = target else {
                    unreachable!("ArrayGet is offered for an Option only")
                };
                let (array, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Ground(FlatDataType::Array(element.clone())),
                    depth,
                    nesting - 1,
                );
                let index = if self.coin() {
                    let index = b.int(self.zigzag(3));
                    self.bind(b, scope, index)
                } else {
                    self.data_op(b, scope, &FlatDataType::Int, depth, nesting - 1)
                };
                b.array_get(array, index)
            }
            P::ArrayEnumerate => {
                let FlatDataType::Array(pair) = target else {
                    unreachable!("ArrayEnumerate is offered for an Array only")
                };
                let FlatDataType::Tuple(types) = pair.as_ref() else {
                    unreachable!("ArrayEnumerate is offered for an Array of pairs only")
                };
                let (array, _) = self.operand(
                    b,
                    scope,
                    &TyPat::Ground(FlatDataType::Array(Box::new(types[1].clone()))),
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
        let holds_closure = match &b.value_types()[value.index()] {
            FlatType::Data(typ) => b.types().holds_closure(typ),
            _ => false,
        };
        if holds_closure {
            return value;
        }
        let cell = scope.push(b.cell(|_| value));
        scope.push(b.cell_read(cell))
    }

    fn call(
        &mut self,
        b: &FlatBlockBuilder<'_>,
        scope: &mut Scope,
        target: &FlatType,
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
        target: &FlatType,
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
                let subject_ty = self.branching_type(types, NamedTypes::Any);
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
        subject_ty: FlatDataType,
        target: &FlatType,
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
    use crate::ir::ir_data_op::IrDataOp;
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
            FlatOp::Lambda(..) => "Lambda".to_string(),
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
            FlatOp::Lambda(..)
                if matches!(typ, FlatType::Data(FlatDataType::Function(signature))
                    if signature.modifier == IrModifier::Mut) =>
            {
                format!("Mut{kind}")
            }
            FlatOp::Call(..) | FlatOp::Match(_) => kind.to_string(),
            _ if match typ {
                FlatType::Html => true,
                FlatType::Data(FlatDataType::Function(signature)) => {
                    *signature.result == FlatType::Html
                }
                _ => false,
            } =>
            {
                format!("Html{kind}")
            }
            _ => kind.to_string(),
        }
    }

    fn nested(binding: &FlatBinding) -> Vec<(Vec<ValueId>, &FlatBlock)> {
        let bound = |pattern: &crate::ir::flat_program::FlatPattern| {
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
            for (_, body) in nested(binding) {
                mark_live(body, live, keep);
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
            for (bound, body) in nested(binding) {
                if depends {
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
    ) {
        for binding in &block.bindings {
            let renders = match &binding.binder.typ {
                FlatType::Html => true,
                FlatType::Data(FlatDataType::Function(signature)) => {
                    *signature.result == FlatType::Html
                }
                _ => false,
            };
            if renders {
                html.insert(binding.binder.var);
            }
            binding.op.for_each_operand(&mut |operand| {
                *references.entry(operand).or_default() += 1;
            });
            for (_, body) in nested(binding) {
                count_references(body, references, html);
            }
        }
        *references.entry(block.result).or_default() += 1;
    }

    #[derive(Default)]
    struct ShapeStats {
        arm_counts: Vec<usize>,
        binding_arms: usize,
        closure_matches: usize,
        /// Records, enums, tuples, arrays and options constructed of a
        /// type that holds a closure.
        closure_holders: usize,
        /// Closures that a field access, a tuple index, a pattern or a
        /// loop takes out of a value that holds them.
        closures_taken_out: usize,
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

        fn count_block(&mut self, block: &FlatBlock, types: &FlatTypeDeclarations) {
            let here: HashSet<ValueId> = (block.bindings.iter())
                .map(|binding| binding.binder.var)
                .collect();
            for binding in &block.bindings {
                match &binding.op {
                    FlatOp::Data(
                        IrDataOp::Record(_)
                        | IrDataOp::Enum(..)
                        | IrDataOp::Tuple(_)
                        | IrDataOp::Array(_)
                        | IrDataOp::OptionSome(_),
                    ) => {
                        self.closure_holders +=
                            usize::from(matches!(&binding.binder.typ, FlatType::Data(typ)
                                if types.holds_closure(typ)));
                    }
                    FlatOp::Data(IrDataOp::FieldAccess(..) | IrDataOp::TupleIndex(..)) => {
                        self.closures_taken_out += usize::from(matches!(
                            binding.binder.typ,
                            FlatType::Data(FlatDataType::Function(_))
                        ));
                    }
                    FlatOp::For(for_) => {
                        self.loops += 1;
                        self.keyed_loops += usize::from(for_.key.is_some());
                        self.closures_taken_out += usize::from(matches!(
                            &for_.var,
                            Some(var) if matches!(var.typ, FlatDataType::Function(_))
                        ));
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
                    FlatOp::Cell(_) => self.cells += 1,
                    FlatOp::Match(match_) => {
                        assert!(match_.arms.len() > 1, "generated matches branch");
                        self.arm_counts.push(match_.arms.len());
                        self.closure_matches += usize::from(matches!(
                            binding.binder.typ,
                            FlatType::Data(FlatDataType::Function(_))
                        ));
                        for arm in &match_.arms {
                            let mut binds = false;
                            arm.pattern.for_each_binding(&mut |binder| {
                                binds = true;
                                self.closures_taken_out +=
                                    usize::from(matches!(binder.typ, FlatDataType::Function(_)));
                            });
                            self.binding_arms += binds as usize;
                        }
                    }
                    _ => {}
                }
                for (_, body) in nested(binding) {
                    self.count_block(body, types);
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
            let functions = program.functions.iter().map(|function| {
                let parameters = (function.parameters.iter())
                    .map(|param| param.binder.var)
                    .collect();
                (Vec::new(), parameters, &function.body, false)
            });
            let lambdas = program.lambdas.iter().map(|lambda| {
                let captures = lambda.captures.iter().map(|binder| binder.var).collect();
                let parameters = (lambda.parameters.iter())
                    .map(|binder| binder.var)
                    .collect();
                let keep = lambda.modifier == IrModifier::Mut;
                (captures, parameters, &lambda.body, keep)
            });
            let entrypoints = program.entrypoints.iter().map(|entrypoint| {
                let parameters = (entrypoint.parameters.iter())
                    .map(|param| param.binder.var)
                    .collect();
                (Vec::new(), parameters, &entrypoint.body, false)
            });
            let declared: Vec<(Vec<ValueId>, Vec<ValueId>, &FlatBlock, bool)> =
                functions.chain(lambdas).chain(entrypoints).collect();
            let bodies: Vec<&FlatBlock> = declared.iter().map(|(_, _, body, _)| *body).collect();
            for (captures, parameters, body, keep) in &declared {
                mark_live(body, &mut live, *keep);
                dynamic.extend(captures.iter().chain(parameters).copied());
                mark_dynamic(body, &mut dynamic);
                shape_stats.parameters.extend(parameters.iter().copied());
                shape_stats.count_block(body, &program.types);
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
            for &body in &bodies {
                count_references(body, &mut references, &mut html);
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
            closure_matches,
            closure_holders,
            closures_taken_out,
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
             {:.1} arms per match on average, at least {fewest_arms} and at most {most_arms}\n\
             {binding_arms} of {arms} match arms bind names\n\
             {closure_matches} of {} matches produce a closure\n\
             {closure_holders} values are constructed of a type that holds a closure\n\
             {closures_taken_out} closures are taken out of a value that holds them\n\
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
            arm_counts.len(),
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
            24112 of 65536 input bytes consumed per program on average, at most 65536
            3 of 32 programs exhaust the input

            -- sizes --
            under 256     2  ######
            under 1K      0
            under 4K      4  #############
            under 16K    12  ########################################
            under 64K    11  ####################################
            at 64K        3  ##########

            -- programs --
            276 of 32035 Html bindings have more than one reference
            2.6 arms per match on average, at least 2 and at most 18
            9066 of 23008 match arms bind names
            2001 of 8891 matches produce a closure
            12123 values are constructed of a type that holds a closure
            2697 closures are taken out of a value that holds them
            19232 cells
            341 of 602 loops have a key
            3565 of 4396 elements have a click
            666 handlers have more than one click
            1090 clicks apply a handler bound outside their block, 10 of them a parameter

            -- ops --
            op                   ops/program % progs  % live   % dyn
            Apply                      203.8    93.8    96.0    78.0
            Array                      251.2    93.8    96.3    33.1
            ArrayEnumerate               2.8    59.4    98.9    74.4
            ArrayFromRange              18.8    93.8    96.0     0.0
            ArrayGet                    50.8    93.8    92.4    63.9
            ArrayIsEmpty                 8.0    78.1    94.1    67.6
            ArrayLength                198.2    93.8    95.5    70.2
            BoolLiteral                 68.0    90.6    96.6     0.0
            BoolNegation                 7.4    81.2    95.0    64.7
            Call                        46.5    59.4    97.8    94.0
            Cell                       601.0    96.9    95.5    60.3
            CellRead                   601.0    96.9    95.5    60.3
            CellUpdate                 114.2    90.6   100.0   100.0
            Enum                       146.1    81.2    96.2    34.6
            Equals                       7.2    78.1    94.8    82.7
            FieldAccess                 18.0    46.9    96.5    95.1
            FloatLiteral               145.3    93.8    96.3     0.0
            FloatToInt                 192.8    93.8    96.0    59.6
            HtmlApply                   14.3    93.8    95.8    75.9
            HtmlConcat                 355.6    96.9    94.6    42.7
            HtmlElement                137.4    93.8    96.9    72.0
            HtmlEscape                 223.0    96.9    94.0    60.6
            HtmlFor                     18.8    93.8    96.5    73.6
            HtmlLambda                  87.0    93.8    95.9    37.6
            HtmlText                    90.0    96.9    95.9     0.0
            IntLiteral                 189.5    93.8    94.8     0.0
            IntToFloat                 177.9    93.8    95.3    66.8
            IntToString                240.1    93.8    94.8    66.2
            Lambda                     419.2    93.8    96.6    56.8
            LessThan                     6.9    84.4    95.5    80.5
            LessThanOrEqual              7.7    81.2    96.3    81.3
            Match                      277.8    93.8    96.1    89.9
            MutLambda                  119.6    93.8    96.4    41.2
            NumericAdd                 381.5    93.8    95.3    77.8
            NumericMultiply            375.5    93.8    95.5    78.2
            NumericNegation            387.6    93.8    95.5    66.2
            NumericSubtract            375.6    93.8    95.5    77.6
            OptionIsNone                 6.4    68.8    96.1    74.5
            OptionIsSome                 6.8    68.8    94.4    69.9
            OptionNone                  63.1    93.8    96.4     0.0
            OptionSome                  55.8    93.8    95.3    42.7
            Record                     117.7    68.8    95.8    36.5
            StringConcat               243.7    93.8    94.8    54.3
            StringIsEmpty                6.8    75.0    97.2    63.3
            StringLiteral               83.6    96.9    95.7     0.0
            Tuple                      257.2    93.8    97.1    31.7
            TupleIndex                 108.9    93.8    93.9    86.7
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
                  let v1: Cell[Int] = cell {
                    v0
                  }
                  let v2: Int = cell_read(v1)
                  let v3: Html = concat()
                  v3
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
                  f0: R1,
                  f1: mut () -> Tuple[],
                  f2: Array[Array[E1]],
                }

                record R1 {
                  f0: Option[Tuple[Bool, R1, E0]],
                  f1: Array[Option[R0]],
                }

                enum E0 {
                  W0,
                  W1,
                  W2 {f0: Tuple[Option[Bool], (R1, Html) -> R0]},
                }

                enum E1 {
                  W0,
                  W1 {f0: Int},
                }

                render fn C0@f0(
                  p0@v0: String,
                  p1@v1: Option[Float],
                  p2@v2: mut () -> Tuple[],
                  p3@v3: render () -> Html,
                ) -> Html {
                  let v7: Int = -2147483648
                  let v8: Cell[Int] = cell {
                    v7
                  }
                  let v9: Int = cell_read(v8)
                  let v10: Html = concat()
                  v10
                }
                render fn C1@f1(
                  p0@v4: Html,
                  p1@v5: E1,
                  p2@v6: mut () -> Tuple[],
                ) -> Html {
                  let v11: Int = -2147483648
                  let v12: Cell[Int] = cell {
                    v11
                  }
                  let v13: Int = cell_read(v12)
                  v4
                }
                entrypoint V0@e0(
                  p0@v14: E1,
                  p1@v15: E1,
                ) -> Html {
                  let v16: Int = -2147483648
                  let v17: Cell[Int] = cell {
                    v16
                  }
                  let v18: Int = cell_read(v17)
                  let v19: Html = concat()
                  v19
                }
                entrypoint V1@e1(
                  p0@v20: Array[Option[Int]],
                  p1@v21: Tuple[],
                  p2@v22: Array[Option[Float]],
                  p3@v23: E1,
                  p4@v24: Option[Array[E1]],
                  p5@v25: Tuple[Array[Bool], Option[E1]],
                  p6@v26: E1,
                  p7@v27: Tuple[],
                ) -> Html {
                  let v28: Int = -2147483648
                  let v29: Cell[Int] = cell {
                    v28
                  }
                  let v30: Int = cell_read(v29)
                  let v31: Html = concat()
                  v31
                }
                entrypoint V2@e2(
                  p0@v32: Bool,
                  p1@v33: Tuple[Tuple[E1, E1, E1], Tuple[Float, E1, Int], Int],
                  p2@v34: Int,
                  p3@v35: Int,
                  p4@v36: Int,
                  p5@v37: Int,
                ) -> Html {
                  let v38: Cell[Int] = cell {
                    v34
                  }
                  let v39: Int = cell_read(v38)
                  let v40: Html = concat()
                  v40
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
                  W0 {f0: Array[Bool]},
                }

                enum E1 {
                  W0 {f0: render (E1, Option[String]) -> Html, f1: Float},
                  W1 {f0: Bool},
                }

                mut lambda l0[]() -> Tuple[] {
                  let v1: Tuple[] = ()
                  v1
                }
                entrypoint V0@e0() -> Html {
                  let v0: Html = text("a\"b'c")
                  let v2: mut () -> Tuple[] = l0[]
                  let v3: Html = element("span", {}, v0) on_click v2
                  v3
                }
                entrypoint V1@e1() -> Html {
                  let v4: Html = text("a\"b'c")
                  v4
                }
            "#]],
        );
    }
}
