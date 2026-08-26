use crate::expr::Type;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::ir::ir_module::{IrExpr, IrModule};
use crate::ir::ir_module_builder::{IrBuilder, IrModuleBuilder};
use arbitrary::Unstructured;
use std::cell::RefCell;
use std::ops::RangeInclusive;

/// Statement/expression recursion budget.
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
struct ComponentInfo {
    name: String,
    params: Vec<(String, String)>,
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

struct IrGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    declared_names: Vec<String>,
    records: Vec<RecordInfo>,
    enums: Vec<EnumInfo>,
    components: Vec<ComponentInfo>,
    next_var: usize,
}

/// Generate a random well-typed IR module.
pub fn random_ir_module(u: &mut Unstructured<'_>) -> (IrModule, TypeRegistry) {
    random_ir_module_inner(u, false)
}

pub fn random_ir_module_with_test_view(u: &mut Unstructured<'_>) -> (IrModule, TypeRegistry) {
    random_ir_module_inner(u, true)
}

fn random_ir_module_inner(
    u: &mut Unstructured<'_>,
    single_test_view: bool,
) -> (IrModule, TypeRegistry) {
    let mut g = IrGenerator {
        u,
        declared_names: Vec::new(),
        records: Vec::new(),
        enums: Vec::new(),
        components: Vec::new(),
        next_var: 0,
    };

    // Name every type before generating any field types, so that a field
    // can refer to a type declared later or to the one being declared.
    let record_count = g.count(0..=3);
    let enum_count = g.count(0..=3);
    g.declared_names = (0..record_count)
        .map(|i| format!("R{i}"))
        .chain((0..enum_count).map(|i| format!("E{i}")))
        .collect();

    let mut builder = IrModuleBuilder::new();

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

    let mut bodies = builder.freeze();

    // Generate components
    for i in 0..g.count(0..=2) {
        let name = format!("C{i}");
        let params: Vec<(String, String)> = (0..g.count(0..=3))
            .map(|_| (g.fresh_var_name(), g.random_type_string(2)))
            .collect();
        bodies = bodies.component(
            &name,
            params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
            |b| g.stmts(b, DEPTH),
        );
        g.components.push(ComponentInfo { name, params });
    }

    // Generate views
    if single_test_view {
        bodies = bodies.view_no_params("Test", |b| g.stmts(b, DEPTH));
    } else {
        for i in 0..g.count(1..=3) {
            let params: Vec<(String, String)> = (0..g.count(0..=3))
                .map(|_| (g.fresh_var_name(), g.random_type_string(2)))
                .collect();
            bodies = bodies.view(
                &format!("V{i}"),
                params.iter().map(|(n, t)| (n.as_str(), t.as_str())),
                |b| g.stmts(b, DEPTH),
            );
        }
    }
    bodies.build_with_registry()
}

impl IrGenerator<'_, '_> {
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
    fn record_fields_of_type(&self, b: &IrBuilder, target: &Type) -> Vec<(String, String)> {
        let mut candidates = Vec::new();
        for rec in &self.records {
            for (field, field_ty) in &rec.fields {
                if *b.resolve_type(field_ty) == *target {
                    candidates.push((rec.name.clone(), field.clone()));
                }
            }
        }
        candidates
    }

    fn fresh_var_name(&mut self) -> String {
        let n = self.next_var;
        self.next_var += 1;
        format!("v{n}")
    }

    /// A random source-syntax type string. E.g. `Array[String]`.
    fn random_type_string(&mut self, depth: usize) -> String {
        self.type_string(depth, NamedTypes::Any)
    }

    /// A random source-syntax type string for a field of the record or
    /// enum currently being declared.
    fn random_field_type_string(&mut self, depth: usize) -> String {
        self.type_string(depth, NamedTypes::Complete)
    }

    fn type_string(&mut self, depth: usize, named: NamedTypes) -> String {
        enum P {
            Scalar,
            Named,
            Array,
            Option,
        }
        let mut kinds = vec![P::Scalar];
        let named_count = self.named_type_count(named);
        if named_count > 0 {
            kinds.push(P::Named);
        }
        if depth > 0 {
            kinds.extend([P::Array, P::Option]);
        }
        match self.u.choose(&kinds).unwrap() {
            P::Scalar => {
                let scalars = ["Int", "String", "Bool", "Float", "Fragment"];
                scalars[self.index(scalars.len())].to_string()
            }
            P::Named => {
                let i = self.index(named_count);
                self.named_type(named, i)
            }
            P::Array => format!("Array[{}]", self.type_string(depth - 1, NamedTypes::Any)),
            P::Option => format!("Option[{}]", self.type_string(depth - 1, NamedTypes::Any)),
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

    fn stmts(&mut self, b: &mut IrBuilder, depth: usize) {
        for _ in 0..self.count(0..=4) {
            self.stmt(b, depth);
        }
    }

    /// Generate a random statement.
    fn stmt(&mut self, b: &mut IrBuilder, depth: usize) {
        enum P {
            Write,
            WriteString,
            WriteFragment,
            InvokeComponent,
            If,
            ForRange,
            ForLoop,
            Let,
            BoolMatch,
            OptionMatch,
            EnumMatch,
        }
        let mut productions = vec![P::Write, P::WriteString, P::WriteFragment];
        if !self.components.is_empty() {
            productions.push(P::InvokeComponent);
        }
        if depth > 0 {
            productions.extend([
                P::If,
                P::ForRange,
                P::ForLoop,
                P::Let,
                P::BoolMatch,
                P::OptionMatch,
            ]);
            if !self.enums.is_empty() {
                productions.push(P::EnumMatch);
            }
        }
        match self.u.choose(&productions).unwrap() {
            P::Write => b.write(self.u.choose(STRING_LITERALS).unwrap()),
            P::WriteString => {
                b.write_string(self.expr(b, &Type::String, depth));
            }
            P::WriteFragment => {
                b.write_fragment(self.expr(b, &Type::Fragment, depth));
            }
            P::InvokeComponent => {
                let info = self.u.choose(&self.components).unwrap().clone();
                let args = info
                    .params
                    .iter()
                    .map(|(n, t)| {
                        let ty = b.resolve_type(t);
                        (n.as_str(), self.expr(b, &ty, depth))
                    })
                    .collect();
                b.invoke_component(&info.name, args);
            }
            P::If => {
                let condition = self.expr(b, &Type::Bool, depth);
                b.if_stmt(condition, |b| self.stmts(b, depth - 1));
            }
            P::ForRange => {
                // Literal bounds only: generated arithmetic can produce huge
                // Int values, which would make evaluation iterate forever.
                let start = b.int(self.zigzag(3));
                let end = b.int(self.zigzag(3));
                let var = self.coin().then(|| self.fresh_var_name());
                b.for_range(var.as_deref(), start, end, |b| self.stmts(b, depth - 1));
            }
            P::ForLoop => {
                let array_ty = Type::Array(b.resolve_type(&self.random_type_string(1)));
                let array = self.expr(b, &array_ty, depth);
                let var = self.fresh_var_name();
                b.for_loop(&var, array, |b| self.stmts(b, depth - 1));
            }
            P::Let => {
                let ty = b.resolve_type(&self.random_type_string(2));
                let value = self.expr(b, &ty, depth);
                let var = self.fresh_var_name();
                b.let_stmt(&var, value, |b| self.stmts(b, depth - 1));
            }
            P::BoolMatch => {
                let subject = self.expr(b, &Type::Bool, depth);
                let this = RefCell::new(self);
                b.bool_match_stmt(
                    subject,
                    |b| this.borrow_mut().stmts(b, depth - 1),
                    |b| this.borrow_mut().stmts(b, depth - 1),
                );
            }
            P::OptionMatch => {
                let option_ty = Type::Option(b.resolve_type(&self.random_type_string(1)));
                let subject = self.expr(b, &option_ty, depth);
                let binding = self.coin().then(|| self.fresh_var_name());
                let this = RefCell::new(self);
                b.option_match_stmt(
                    subject,
                    binding.as_deref(),
                    |b| this.borrow_mut().stmts(b, depth - 1),
                    |b| this.borrow_mut().stmts(b, depth - 1),
                );
            }
            P::EnumMatch => {
                let info = self.u.choose(&self.enums).unwrap().clone();
                let subject_ty = b.resolve_type(&info.name);
                let subject = self.expr(b, &subject_ty, depth);
                let arm_plan = self.enum_match_arms(&info.variants);
                b.enum_match_stmt(subject, |arms| {
                    for (variant, bindings) in &arm_plan {
                        arms.arm_bound(
                            variant,
                            bindings.iter().map(|(f, v)| (f.as_str(), v.as_str())),
                            |b| self.stmts(b, depth - 1),
                        );
                    }
                });
            }
        }
    }

    /// Generate a random expression of the target type.
    fn expr(&mut self, b: &IrBuilder, target: &Type, depth: usize) -> IrExpr {
        enum P {
            Lit,
            Var,
            FieldAccess,
            Let,
            BoolMatch,
            OptionMatch,
            EnumMatch,
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
            Neg,
            Add,
            Sub,
            Mul,
            Fragment,
        }
        let mut productions = vec![P::Lit];
        if b.vars().iter().any(|(_, _, ty)| **ty == *target) {
            productions.push(P::Var);
        }
        let record_fields = self.record_fields_of_type(b, target);
        if depth > 0 {
            if !record_fields.is_empty() {
                productions.push(P::FieldAccess);
            }
            productions.extend([P::Let, P::BoolMatch, P::OptionMatch]);
            if !self.enums.is_empty() {
                productions.push(P::EnumMatch);
            }
            if *target == Type::Bool {
                productions.extend([
                    P::Eq,
                    P::Lt,
                    P::Lte,
                    P::Not,
                    P::And,
                    P::Or,
                    P::OptionIsNone,
                    P::OptionIsSome,
                    P::StringIsEmpty,
                    P::ArrayIsEmpty,
                ]);
            }
            if *target == Type::Int {
                productions.extend([
                    P::ArrayLength,
                    P::Neg,
                    P::Add,
                    P::Sub,
                    P::Mul,
                    P::FloatToInt,
                ]);
            }
            if *target == Type::Float {
                productions.extend([P::Neg, P::Add, P::Sub, P::Mul, P::IntToFloat]);
            }
            if *target == Type::String {
                productions.extend([P::Concat, P::IntToString]);
            }
            if *target == Type::Fragment {
                productions.push(P::Fragment);
            }
        }
        match self.u.choose(&productions).unwrap() {
            P::Lit => self.literal(b, target, depth),
            P::Var => {
                let candidates: Vec<&str> = b
                    .vars()
                    .iter()
                    .filter(|(_, _, ty)| **ty == *target)
                    .map(|(name, _, _)| name.as_str())
                    .collect();
                b.var(self.u.choose(&candidates).unwrap())
            }
            P::FieldAccess => {
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
            P::BoolMatch => {
                let subject = self.expr(b, &Type::Bool, depth - 1);
                let true_body = self.expr(b, target, depth - 1);
                let false_body = self.expr(b, target, depth - 1);
                b.bool_match_expr(subject, true_body, false_body)
            }
            P::OptionMatch => {
                let option_ty = Type::Option(b.resolve_type(&self.random_type_string(1)));
                let subject = self.expr(b, &option_ty, depth - 1);
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
            P::EnumMatch => {
                let info = self.u.choose(&self.enums).unwrap().clone();
                let subject_ty = b.resolve_type(&info.name);
                let subject = self.expr(b, &subject_ty, depth - 1);
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
                let left = self.expr(b, &Type::String, depth - 1);
                let right = self.expr(b, &Type::String, depth - 1);
                b.string_concat(left, right)
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
                let option_ty = Type::Option(b.resolve_type(&self.random_type_string(1)));
                let operand = self.expr(b, &option_ty, depth - 1);
                b.option_is_none(operand)
            }
            P::OptionIsSome => {
                let option_ty = Type::Option(b.resolve_type(&self.random_type_string(1)));
                let operand = self.expr(b, &option_ty, depth - 1);
                b.option_is_some(operand)
            }
            P::StringIsEmpty => {
                let operand = self.expr(b, &Type::String, depth - 1);
                b.string_is_empty(operand)
            }
            P::ArrayIsEmpty => {
                let array_ty = Type::Array(b.resolve_type(&self.random_type_string(1)));
                let operand = self.expr(b, &array_ty, depth - 1);
                b.array_is_empty(operand)
            }
            P::ArrayLength => {
                let array_ty = Type::Array(b.resolve_type(&self.random_type_string(1)));
                let operand = self.expr(b, &array_ty, depth - 1);
                b.array_length(operand)
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
            P::Fragment => b.fragment(|b| self.stmts(b, depth - 1)),
        }
    }

    /// A literal expression of the target type.
    fn literal(&mut self, b: &IrBuilder, target: &Type, depth: usize) -> IrExpr {
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
                b.array_typed(inner.clone(), elements)
            }
            Type::Option(inner) => {
                if depth > 0 && self.coin() {
                    let value = self.expr(b, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none_typed(inner.clone())
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
            Type::Fragment => b.fragment(|_b| {}),
        }
    }
}
