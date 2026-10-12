use crate::hop::typing::{Constructor, Pattern, PatternField, Type, TypeRegistry};
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::IrPattern;
use crate::ir::pure_module::{PureExpr, PureModule};
use crate::ir::pure_module_builder::{PureBuilder, PureModuleBuilder, ScopedVar};
use crate::ir::runtime::random::random_value;
use crate::ir::runtime::value::Value;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use arbitrary::Unstructured;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::RangeInclusive;

/// Expression recursion budget.
const DEPTH: usize = 3;

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

#[derive(Clone)]
struct RecordInfo {
    name: String,
    fields: Vec<(String, String)>,
}

#[derive(Clone)]
struct EnumInfo {
    name: String,
    variants: Vec<(String, Vec<(String, String)>)>,
}

#[derive(Clone)]
struct FunctionInfo {
    name: String,
    params: Vec<(String, String)>,
    return_type: String,
}

#[derive(Clone)]
pub struct GeneratorConfig {
    pub record_count: RangeInclusive<usize>,
    pub enum_count: RangeInclusive<usize>,
    pub function_count: RangeInclusive<usize>,
    /// Ignored for the module with a test view, which has a single entry.
    pub entry_count: RangeInclusive<usize>,
    /// For functions and entries alike.
    pub parameter_count: RangeInclusive<usize>,
    pub arrays: bool,
    pub options: bool,
    pub tuples: bool,
    /// Loops over arrays and ranges.
    pub loops: bool,
    /// Matches on patterns of any shape, which may bind.
    pub pattern_matches: bool,
    /// Escaped strings in Html.
    pub escapes: bool,
    /// Parameters of any type. Without it, every parameter of a function or
    /// entry is an Int, Float, String or Bool.
    pub complex_parameters: bool,
}

impl Default for GeneratorConfig {
    /// Every feature, in the counts `random_module` has always generated.
    fn default() -> Self {
        GeneratorConfig {
            record_count: 0..=3,
            enum_count: 0..=3,
            function_count: 0..=2,
            entry_count: 1..=3,
            parameter_count: 0..=3,
            arrays: true,
            options: true,
            tuples: true,
            loops: true,
            pattern_matches: true,
            escapes: true,
            complex_parameters: true,
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

struct PureGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    config: GeneratorConfig,
    declared_names: Vec<String>,
    records: Vec<RecordInfo>,
    enums: Vec<EnumInfo>,
    functions: Vec<FunctionInfo>,
    next_var: usize,
}

/// Generate a random well-typed Pure module.
pub fn random_module(u: &mut Unstructured<'_>) -> (PureModule, TypeRegistry) {
    random_module_inner(u, false, GeneratorConfig::default())
}

pub fn random_module_with_test_view(u: &mut Unstructured<'_>) -> (PureModule, TypeRegistry) {
    random_module_inner(u, true, GeneratorConfig::default())
}

fn random_module_inner(
    u: &mut Unstructured<'_>,
    single_test_entry: bool,
    config: GeneratorConfig,
) -> (PureModule, TypeRegistry) {
    let mut g = PureGenerator {
        u,
        config: config.clone(),
        declared_names: Vec::new(),
        records: Vec::new(),
        enums: Vec::new(),
        functions: Vec::new(),
        next_var: 0,
    };

    // Name every type before generating any field types, so that a field
    // can refer to a type declared later or to the one being declared.
    let record_count = g.count(config.record_count.clone());
    let enum_count = g.count(config.enum_count.clone());
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut builder = PureModuleBuilder::new();

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
                builder =
                    builder.record(&name, fields.iter().map(|(f, t)| (f.as_str(), t.as_str())));
                g.records.push(RecordInfo { name, fields });
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
                builder = builder.enum_(
                    &name,
                    variants.iter().map(|(v, fs)| {
                        (
                            v.as_str(),
                            fs.iter().map(|(f, t)| (f.as_str(), t.as_str())).collect(),
                        )
                    }),
                );
                g.enums.push(EnumInfo { name, variants });
            }
        }
    }

    let generator = RefCell::new(g);
    let g = &generator;
    let mut bodies = builder.freeze();

    // Generate functions
    let function_count = g.borrow_mut().count(config.function_count.clone());
    for i in 0..function_count {
        let info = {
            let mut g = g.borrow_mut();
            let params = (0..g.count(config.parameter_count.clone()))
                .map(|_| {
                    (
                        g.fresh_var_name(),
                        g.type_string(2, NamedTypes::Any, config.complex_parameters),
                    )
                })
                .collect();
            let info = FunctionInfo {
                name: format!("C{i}"),
                params,
                return_type: g.random_type_string(2),
            };
            g.functions.push(info.clone());
            info
        };
        bodies = bodies.function(
            &info.name,
            info.params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
            &info.return_type,
            move |b| {
                let mut g = g.borrow_mut();
                let target = b.resolve_type(&g.functions[i].return_type);
                g.expr(b, &target, DEPTH)
            },
        );
    }

    // Generate entry functions, each rendering Html as a page body does.
    if single_test_entry {
        bodies = bodies.entry("Test", [], "Html", |b| {
            g.borrow_mut().expr(b, &Type::Html, DEPTH)
        });
    } else {
        let entry_count = g.borrow_mut().count(config.entry_count.clone());
        for i in 0..entry_count {
            let params: Vec<(String, String)> = {
                let mut g = g.borrow_mut();
                (0..g.count(config.parameter_count.clone()))
                    .map(|_| {
                        (
                            g.fresh_var_name(),
                            g.type_string(2, NamedTypes::Any, config.complex_parameters),
                        )
                    })
                    .collect()
            };
            bodies = bodies.entry(
                &format!("V{i}"),
                params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
                "Html",
                |b| g.borrow_mut().expr(b, &Type::Html, DEPTH),
            );
        }
    }
    bodies.build_with_registry()
}

/// Random arguments for each entry function of the module, named by its
/// parameters.
pub fn random_entry_args(
    module: &PureModule,
    rng: &mut impl Rng,
    registry: &TypeRegistry,
) -> Vec<(IrFunction, HashMap<AttributeName, Value>)> {
    module
        .functions
        .iter()
        .filter(|decl| decl.entry)
        .map(|decl| {
            let args = decl
                .parameters
                .iter()
                .map(|param| {
                    let value = random_value(rng, &param.typ, None, registry);
                    (param.name.clone(), value)
                })
                .collect();
            (decl.function.clone(), args)
        })
        .collect()
}

impl PureGenerator<'_, '_> {
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
    fn record_fields_of_type(&self, b: &PureBuilder, target: &Type) -> Vec<(String, String)> {
        let mut candidates = Vec::new();
        for rec in &self.records {
            for (field, field_ty) in &rec.fields {
                if b.resolve_type(field_ty) == *target {
                    candidates.push((rec.name.clone(), field.clone()));
                }
            }
        }
        candidates
    }

    /// With even odds, pick a reference to a variable in scope whose type fits,
    /// when there is one.
    fn var_in_scope(&mut self, b: &PureBuilder, fits: impl Fn(&Type) -> bool) -> Option<PureExpr> {
        let candidates: Vec<&str> = b
            .vars()
            .iter()
            .filter(|(_, _, ty)| fits(ty))
            .map(|(name, _, _)| name.as_str())
            .collect();
        if candidates.is_empty() || !self.coin() {
            return None;
        }
        Some(b.var(self.u.choose(&candidates).unwrap()))
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

    /// A random source-syntax type string for a field of the record or
    /// enum currently being declared.
    fn random_field_type_string(&mut self, depth: usize) -> String {
        self.type_string(depth, NamedTypes::Complete, true)
    }

    /// A type of the kinds the config allows, or a scalar when it is not
    /// `complex`.
    fn type_string(&mut self, depth: usize, named: NamedTypes, complex: bool) -> String {
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
        if complex {
            kinds.push(P::Html);
            if named_count > 0 {
                kinds.push(P::Named);
            }
            if depth > 0 {
                if self.config.arrays {
                    kinds.push(P::Array);
                }
                if self.config.options {
                    kinds.push(P::Option);
                }
                if self.config.tuples {
                    kinds.push(P::Tuple);
                }
            }
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
                self.type_string(depth - 1, NamedTypes::Any, complex)
            ),
            P::Option => format!(
                "Option[{}]",
                self.type_string(depth - 1, NamedTypes::Any, complex)
            ),
            P::Tuple => {
                let arity = self.count(0..=3);
                let elements: Vec<String> = (0..arity)
                    .map(|_| self.type_string(depth - 1, named, complex))
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
            NamedTypes::Complete => self.records.len() + self.enums.len(),
        }
    }

    fn named_type(&self, named: NamedTypes, i: usize) -> String {
        match named {
            NamedTypes::Any => self.declared_names[i].clone(),
            NamedTypes::Complete if i < self.records.len() => self.records[i].name.clone(),
            NamedTypes::Complete => self.enums[i - self.records.len()].name.clone(),
        }
    }

    /// Plan the arms of an enum match.
    fn enum_match_arms(
        &mut self,
        variants: &[(String, Vec<(String, String)>)],
    ) -> Vec<(String, Vec<(String, String)>)> {
        let mut remaining: Vec<&(String, Vec<(String, String)>)> = variants.iter().collect();
        let mut arms = Vec::new();
        while !remaining.is_empty() {
            let (variant, fields) = remaining.swap_remove(self.index(remaining.len()));
            let mut bindings = Vec::new();
            for (field, _) in fields {
                if self.coin() {
                    bindings.push((field.clone(), self.fresh_var_name()));
                }
            }
            arms.push((variant.clone(), bindings));
        }
        arms
    }

    /// A random pattern for values of `typ` that tests at most `depth`
    /// constructors on any path from its root. A pattern of depth zero
    /// matches every value. The variables it binds are pushed to `vars`.
    fn pattern(
        &mut self,
        b: &PureBuilder,
        typ: &Type,
        depth: usize,
        vars: &mut Vec<ScopedVar>,
    ) -> IrPattern {
        if depth > 0 && self.index(3) > 0 {
            match typ {
                Type::Bool => {
                    let constructor = if self.coin() {
                        Constructor::BoolTrue
                    } else {
                        Constructor::BoolFalse
                    };
                    return Pattern::Constructor {
                        constructor,
                        args: Vec::new(),
                        fields: Vec::new(),
                    };
                }
                Type::Option(inner) => {
                    return if self.coin() {
                        Pattern::Constructor {
                            constructor: Constructor::OptionSome,
                            args: vec![self.pattern(b, inner, depth - 1, vars)],
                            fields: Vec::new(),
                        }
                    } else {
                        Pattern::Constructor {
                            constructor: Constructor::OptionNone,
                            args: Vec::new(),
                            fields: Vec::new(),
                        }
                    };
                }
                Type::Tuple(elements) => {
                    return Pattern::Constructor {
                        constructor: Constructor::Tuple,
                        args: elements
                            .iter()
                            .map(|element| self.pattern(b, element, depth - 1, vars))
                            .collect(),
                        fields: Vec::new(),
                    };
                }
                Type::Named { .. } => {
                    let record = self
                        .records
                        .iter()
                        .find(|record| b.resolve_type(&record.name) == *typ)
                        .cloned();
                    let (constructor, fields) = match record {
                        Some(record) => (
                            Constructor::Record {
                                type_name: TypeName::parse(&record.name).unwrap(),
                            },
                            record.fields,
                        ),
                        None => {
                            let info = self
                                .enums
                                .iter()
                                .find(|info| b.resolve_type(&info.name) == *typ)
                                .cloned()
                                .expect("a named type is a declared record or enum");
                            let (variant, fields) = self.u.choose(&info.variants).unwrap().clone();
                            (
                                Constructor::EnumVariant {
                                    type_name: TypeName::parse(&info.name).unwrap(),
                                    variant_name: TypeName::parse(&variant).unwrap(),
                                },
                                fields,
                            )
                        }
                    };
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, (field, field_ty))| PatternField {
                            name: FieldName::parse(field).unwrap(),
                            index,
                            pattern: self.pattern(b, &b.resolve_type(field_ty), depth - 1, vars),
                        })
                        .collect();
                    return Pattern::Constructor {
                        constructor,
                        args: Vec::new(),
                        fields,
                    };
                }
                _ => {}
            }
        }
        if self.coin() {
            return Pattern::Wildcard;
        }
        let (pattern, var) = b.binding_pattern(&self.fresh_var_name(), typ.clone());
        vars.push(var);
        pattern
    }

    /// Generate a random expression of the target type.
    fn expr(&mut self, b: &PureBuilder, target: &Type, depth: usize) -> PureExpr {
        enum P {
            Lit,
            Var,
            FieldAccess,
            Let,
            MatchPattern,
            MatchBool,
            MatchOption,
            MatchEnum,
            Eq,
            Lt,
            Lte,
            Not,
            And,
            Or,
            Concat,
            IntToString,
            FloatToInt,
            IntToFloat,
            OptionIsNone,
            OptionIsSome,
            StringIsEmpty,
            ArrayIsEmpty,
            ArrayLength,
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
        let mut productions = vec![P::Lit];
        if b.vars().iter().any(|(_, _, ty)| *ty == *target) {
            productions.push(P::Var);
        }
        let record_fields = self.record_fields_of_type(b, target);
        if depth > 0 {
            productions.extend([P::Let, P::MatchBool]);
            if !record_fields.is_empty() {
                productions.push(P::FieldAccess);
            }
            if self.config.options {
                productions.push(P::MatchOption);
            }
            if self.config.pattern_matches {
                productions.push(P::MatchPattern);
            }
            if self.config.tuples {
                productions.push(P::TupleIndex);
            }
            if !self.enums.is_empty() {
                productions.push(P::MatchEnum);
            }
            if *target == Type::Bool {
                productions.extend([P::Eq, P::Lt, P::Lte, P::Not, P::And, P::Or]);
                if self.config.options {
                    productions.extend([P::OptionIsNone, P::OptionIsSome]);
                }
                productions.push(P::StringIsEmpty);
                if self.config.arrays {
                    productions.push(P::ArrayIsEmpty);
                }
            }
            if *target == Type::Int {
                if self.config.arrays {
                    productions.push(P::ArrayLength);
                }
                productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::FloatToInt]);
            }
            if *target == Type::Float {
                productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::IntToFloat]);
            }
            if *target == Type::String {
                productions.extend([P::Concat, P::IntToString]);
            }
            if *target == Type::Html {
                productions.push(P::HtmlText);
                if self.config.escapes {
                    productions.push(P::HtmlEscape);
                }
                productions.extend([P::HtmlElement, P::HtmlConcat]);
                if self.config.loops && self.config.arrays {
                    productions.push(P::HtmlForArray);
                }
                if self.config.loops {
                    productions.push(P::HtmlForRange);
                }
            }
            if self
                .functions
                .iter()
                .any(|f| b.resolve_type(&f.return_type) == *target)
            {
                productions.push(P::Call);
            }
        }
        match self.u.choose(&productions).unwrap() {
            P::Lit => self.literal(b, target, depth),
            P::Var => {
                let candidates: Vec<&str> = b
                    .vars()
                    .iter()
                    .filter(|(_, _, ty)| *ty == *target)
                    .map(|(name, _, _)| name.as_str())
                    .collect();
                b.var(self.u.choose(&candidates).unwrap())
            }
            P::FieldAccess => {
                let var = self.var_in_scope(b, |ty| {
                    record_fields
                        .iter()
                        .any(|(record, _)| b.resolve_type(record) == *ty)
                });
                if let Some(object) = var {
                    let fields: Vec<&String> = record_fields
                        .iter()
                        .filter(|(record, _)| b.resolve_type(record) == object.typ())
                        .map(|(_, field)| field)
                        .collect();
                    return b.field_access(object, self.u.choose(&fields).unwrap());
                }
                let (record, field) = self.u.choose(&record_fields).unwrap().clone();
                let record_ty = b.resolve_type(&record);
                let object = self.expr(b, &record_ty, depth - 1);
                b.field_access(object, &field)
            }
            P::Let => {
                let value_ty = b.resolve_type(&self.random_type_string(2));
                let value = self.expr(b, &value_ty, depth - 1);
                b.let_expr(&self.fresh_var_name(), value, |b| {
                    self.expr(b, target, depth - 1)
                })
            }
            P::MatchBool => {
                let subject = self.expr(b, &Type::Bool, depth - 1);
                let true_body = self.expr(b, target, depth - 1);
                let false_body = self.expr(b, target, depth - 1);
                b.bool_match_expr(subject, true_body, false_body)
            }
            P::MatchOption => {
                let subject = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Option(_)))
                    .unwrap_or_else(|| {
                        let option_ty =
                            Type::Option(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &option_ty, depth - 1)
                    });
                if self.coin() {
                    let binding = self.fresh_var_name();
                    let none_body = self.expr(b, target, depth - 1);
                    b.option_match_expr_with_binding(
                        subject,
                        &binding,
                        |b| self.expr(b, target, depth - 1),
                        none_body,
                    )
                } else {
                    let some_body = self.expr(b, target, depth - 1);
                    let none_body = self.expr(b, target, depth - 1);
                    b.option_match_expr(subject, some_body, none_body)
                }
            }
            P::MatchEnum => {
                let enum_types: Vec<Type> = self
                    .enums
                    .iter()
                    .map(|info| b.resolve_type(&info.name))
                    .collect();
                let var = self.var_in_scope(b, |ty| enum_types.contains(ty));
                let info = match &var {
                    Some(var) => {
                        let index = enum_types.iter().position(|ty| *ty == var.typ()).unwrap();
                        self.enums[index].clone()
                    }
                    None => self.u.choose(&self.enums).unwrap().clone(),
                };
                let subject_ty = b.resolve_type(&info.name);
                let subject = var.unwrap_or_else(|| self.expr(b, &subject_ty, depth - 1));
                let arm_plan = self.enum_match_arms(&info.variants);
                b.enum_match_expr(subject, |arms| {
                    for (variant, bindings) in &arm_plan {
                        arms.arm_bound(
                            variant,
                            bindings.iter().map(|(f, v)| (f.as_str(), v.as_str())),
                            |b| self.expr(b, target, depth - 1),
                        );
                    }
                })
            }
            P::Eq => {
                let ty = match self.index(4) {
                    0 => Type::Int,
                    1 => Type::String,
                    2 => Type::Bool,
                    _ => Type::Float,
                };
                let left = self.expr(b, &ty, depth - 1);
                let right = self.expr(b, &ty, depth - 1);
                b.eq(left, right)
            }
            P::Lt => {
                let ty = if self.coin() { Type::Int } else { Type::Float };
                let left = self.expr(b, &ty, depth - 1);
                let right = self.expr(b, &ty, depth - 1);
                b.lt(left, right)
            }
            P::Lte => {
                let ty = if self.coin() { Type::Int } else { Type::Float };
                let left = self.expr(b, &ty, depth - 1);
                let right = self.expr(b, &ty, depth - 1);
                b.lte(left, right)
            }
            P::Not => {
                let operand = self.expr(b, &Type::Bool, depth - 1);
                b.not(operand)
            }
            P::And => {
                let left = self.expr(b, &Type::Bool, depth - 1);
                let right = self.expr(b, &Type::Bool, depth - 1);
                b.and(left, right)
            }
            P::Or => {
                let left = self.expr(b, &Type::Bool, depth - 1);
                let right = self.expr(b, &Type::Bool, depth - 1);
                b.or(left, right)
            }
            P::Concat => {
                let parts = (0..self.count(0..=4))
                    .map(|_| self.expr(b, &Type::String, depth - 1))
                    .collect();
                b.string_concat(parts)
            }
            P::IntToString => {
                let operand = self.expr(b, &Type::Int, depth - 1);
                b.int_to_string(operand)
            }
            P::FloatToInt => {
                let operand = self.expr(b, &Type::Float, depth - 1);
                b.float_to_int(operand)
            }
            P::IntToFloat => {
                let operand = self.expr(b, &Type::Int, depth - 1);
                b.int_to_float(operand)
            }
            P::OptionIsNone => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Option(_)))
                    .unwrap_or_else(|| {
                        let option_ty =
                            Type::Option(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &option_ty, depth - 1)
                    });
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Option(_)))
                    .unwrap_or_else(|| {
                        let option_ty =
                            Type::Option(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &option_ty, depth - 1)
                    });
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.expr(b, &Type::String, depth - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Array(_)))
                    .unwrap_or_else(|| {
                        let array_ty =
                            Type::Array(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &array_ty, depth - 1)
                    });
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let operand = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Array(_)))
                    .unwrap_or_else(|| {
                        let array_ty =
                            Type::Array(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &array_ty, depth - 1)
                    });
                b.array_length(operand)
            }
            P::TupleIndex => {
                let var = self.var_in_scope(
                    b,
                    |ty| matches!(ty, Type::Tuple(elements) if elements.contains(target)),
                );
                if let Some(tuple) = var {
                    let Type::Tuple(elements) = tuple.typ() else {
                        unreachable!("the variable has a tuple type")
                    };
                    let indices: Vec<usize> = (0..elements.len())
                        .filter(|&index| elements[index] == *target)
                        .collect();
                    return b.tuple_index(tuple, *self.u.choose(&indices).unwrap());
                }
                let arity = self.count(1..=3);
                let index = self.index(arity);
                let elements = (0..arity)
                    .map(|i| {
                        if i == index {
                            target.clone()
                        } else {
                            b.resolve_type(&self.random_type_string(1))
                        }
                    })
                    .collect();
                let tuple = self.expr(b, &Type::Tuple(elements), depth - 1);
                b.tuple_index(tuple, index)
            }
            P::MatchPattern => {
                let subject = self
                    .var_in_scope(b, |ty| {
                        matches!(
                            ty,
                            Type::Bool | Type::Option(_) | Type::Tuple(_) | Type::Named { .. }
                        )
                    })
                    .unwrap_or_else(|| {
                        if !self.config.tuples {
                            return self.expr(b, &Type::Bool, depth - 1);
                        }
                        let elements = (0..self.count(1..=3))
                            .map(|_| b.resolve_type(&self.random_type_string(1)))
                            .collect();
                        self.expr(b, &Type::Tuple(elements), depth - 1)
                    });
                let subject_ty = subject.typ();
                let mut arms = Vec::new();
                for _ in 0..self.count(1..=3) {
                    let mut vars = Vec::new();
                    let pattern = self.pattern(b, &subject_ty, 2, &mut vars);
                    arms.push((pattern, vars));
                }
                // A last arm that matches every value makes the match
                // exhaustive, whether or not the arms before it are.
                let mut vars = Vec::new();
                let pattern = self.pattern(b, &subject_ty, 0, &mut vars);
                arms.push((pattern, vars));
                b.pattern_match_expr(subject, arms, |b| self.expr(b, target, depth - 1))
            }
            P::Neg => {
                let operand = self.expr(b, target, depth - 1);
                b.neg(operand)
            }
            P::Add => {
                let left = self.expr(b, target, depth - 1);
                let right = self.expr(b, target, depth - 1);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.expr(b, target, depth - 1);
                let right = self.expr(b, target, depth - 1);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.expr(b, target, depth - 1);
                let right = self.expr(b, target, depth - 1);
                b.mul(left, right)
            }
            P::HtmlText => b.text(self.u.choose(TEXTS).unwrap()),
            P::HtmlEscape => {
                let operand = self.expr(b, &Type::String, depth - 1);
                b.escape(operand)
            }
            P::HtmlElement => {
                let (tag, void) = *self.u.choose(ELEMENTS).unwrap();
                // Attribute names are unique within an element, so each
                // candidate is included at most once.
                let mut attributes = Vec::new();
                for name in ["class", "id", "data-x"] {
                    if self.coin() {
                        let value = self.expr(b, &Type::String, depth - 1);
                        attributes.push(b.attr(name, value));
                    }
                }
                if self.coin() {
                    let present = self.expr(b, &Type::Bool, depth - 1);
                    attributes.push(b.presence("disabled", present));
                }
                let children = if void {
                    Vec::new()
                } else {
                    (0..self.count(0..=3))
                        .map(|_| self.expr(b, &Type::Html, depth - 1))
                        .collect()
                };
                b.element(tag, attributes, children)
            }
            P::HtmlConcat => {
                let parts = (0..self.count(0..=4))
                    .map(|_| self.expr(b, &Type::Html, depth - 1))
                    .collect();
                b.concat(parts)
            }
            P::HtmlForArray => {
                let array = self
                    .var_in_scope(b, |ty| matches!(ty, Type::Array(_)))
                    .unwrap_or_else(|| {
                        let array_ty =
                            Type::Array(Box::new(b.resolve_type(&self.random_type_string(1))));
                        self.expr(b, &array_ty, depth)
                    });
                let var = self.fresh_var_name();
                b.html_for(Some(&var), array, |b| self.expr(b, &Type::Html, depth - 1))
            }
            P::HtmlForRange => {
                // Literal bounds only, generated arithmetic can produce huge
                // Int values, which would make evaluation iterate forever.
                let start = b.int(self.zigzag(3));
                let end = b.int(self.zigzag(3));
                let var = self.coin().then(|| self.fresh_var_name());
                b.html_for_range(var.as_deref(), start, end, |b| {
                    self.expr(b, &Type::Html, depth - 1)
                })
            }
            P::Call => {
                let candidates: Vec<FunctionInfo> = self
                    .functions
                    .iter()
                    .filter(|f| b.resolve_type(&f.return_type) == *target)
                    .cloned()
                    .collect();
                let info = self.u.choose(&candidates).unwrap().clone();
                let args = info
                    .params
                    .iter()
                    .map(|(n, t)| {
                        let ty = b.resolve_type(t);
                        (n.as_str(), self.expr(b, &ty, depth))
                    })
                    .collect();
                b.call(&info.name, args)
            }
        }
    }

    /// A literal expression of the target type.
    fn literal(&mut self, b: &PureBuilder, target: &Type, depth: usize) -> PureExpr {
        // Note: we use saturating_sub here since we might be forced to construct
        // something deeper than depth.
        match &target {
            Type::String => b.str(self.u.choose(STRING_LITERALS).unwrap()),
            Type::Int => {
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
            Type::Bool => b.bool(self.coin()),
            Type::Float => b.float(*self.u.choose(FLOATS).unwrap()),
            Type::Array(inner) => {
                let len = if depth == 0 { 0 } else { self.count(0..=3) };
                let elements = (0..len)
                    .map(|_| self.expr(b, inner, depth.saturating_sub(1)))
                    .collect();
                b.array_typed(inner.as_ref().clone(), elements)
            }
            Type::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.expr(b, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none_typed(inner.as_ref().clone())
                }
            }
            Type::Named { name, .. } => {
                if let Some(rec) = self.records.iter().find(|r| r.name == name.as_str()) {
                    let fields = self.shuffled(rec.fields.clone());
                    let mut values = Vec::new();
                    for (field, field_ty) in &fields {
                        let ty = b.resolve_type(field_ty);
                        values.push((field.as_str(), self.expr(b, &ty, depth.saturating_sub(1))));
                    }
                    b.record(name.as_str(), values)
                } else {
                    let info = self
                        .enums
                        .iter()
                        .find(|e| e.name == name.as_str())
                        .expect("named type must be a generated record or enum")
                        .clone();
                    let (variant, fields) = self.u.choose(&info.variants).unwrap();
                    let fields = self.shuffled(fields.clone());
                    let mut values = Vec::new();
                    for (field, field_ty) in &fields {
                        let ty = b.resolve_type(field_ty);
                        values.push((field.as_str(), self.expr(b, &ty, depth.saturating_sub(1))));
                    }
                    b.enum_variant_with_fields(name.as_str(), variant, values)
                }
            }
            Type::Html => b.concat(Vec::new()),
            Type::Tuple(elements) => {
                let values = elements
                    .iter()
                    .map(|element| self.expr(b, element, depth.saturating_sub(1)))
                    .collect();
                b.tuple(values)
            }
        }
    }
}
