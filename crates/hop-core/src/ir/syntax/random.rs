use super::builder::{IrBuilder, IrModuleBuilder};
use crate::expr::Type;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::ir::ast::{IrExpr, IrModule};
use arbitrary::Unstructured;
use std::cell::RefCell;
use std::ops::RangeInclusive;

/// Statement/expression recursion budget.
const DEPTH: usize = 3;

const WORDS: &[&str] = &["foo", "bar", "baz"];

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

struct IrGenerator<'a, 'b> {
    u: &'a mut Unstructured<'b>,
    records: Vec<RecordInfo>,
    enums: Vec<EnumInfo>,
    components: Vec<ComponentInfo>,
    next_var: usize,
}

/// Generate a random well-typed IR module.
pub fn random_ir_module(u: &mut Unstructured<'_>) -> (IrModule, TypeRegistry) {
    let mut g = IrGenerator {
        u,
        records: Vec::new(),
        enums: Vec::new(),
        components: Vec::new(),
        next_var: 0,
    };

    let mut builder = IrModuleBuilder::new();

    // Generate records
    for i in 0..g.count(0..=3) {
        let name = format!("R{i}");
        let fields = (0..g.count(0..=4))
            .map(|j| (format!("f{j}"), g.random_type_string(2)))
            .collect::<Vec<_>>();
        builder = builder.record(&name, fields.iter().map(|(f, t)| (f.as_str(), t.as_str())));
        g.records.push(RecordInfo { name, fields });
    }

    // Generate enums
    for i in 0..g.count(0..=3) {
        let name = format!("E{i}");
        let variants: Vec<(String, Vec<(String, String)>)> = (0..g.count(1..=3))
            .map(|j| {
                let fields = (0..g.count(0..=2))
                    .map(|k| (format!("f{k}"), g.random_type_string(2)))
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

    let mut bodies = builder.types_done();

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
    fn zigzag(&mut self, magnitude: i64) -> i64 {
        let n = self.u.int_in_range(0..=2 * magnitude).unwrap();
        if n % 2 == 0 { -n / 2 } else { n / 2 + 1 }
    }

    fn fresh_var_name(&mut self) -> String {
        let n = self.next_var;
        self.next_var += 1;
        format!("v{n}")
    }

    /// A random source-syntax type string. E.g. `Array[String]`.
    fn random_type_string(&mut self, depth: usize) -> String {
        let named = self.records.len() + self.enums.len();
        let n = 3 + named + if depth > 0 { 2 } else { 0 };
        match self.index(n) {
            0 => "Int".to_string(),
            1 => "String".to_string(),
            2 => "Bool".to_string(),
            i if i < 3 + self.records.len() => self.records[i - 3].name.clone(),
            i if i < 3 + named => self.enums[i - 3 - self.records.len()].name.clone(),
            i if i == 3 + named => format!("Array[{}]", self.random_type_string(depth - 1)),
            _ => format!("Option[{}]", self.random_type_string(depth - 1)),
        }
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
            WriteExpr,
            InvokeComponent,
            If,
            ForRange,
            ForLoop,
            Let,
            LetFragment,
            BoolMatch,
            OptionMatch,
            EnumMatch,
        }
        let mut productions = vec![P::Write, P::WriteExpr];
        if !self.components.is_empty() {
            productions.push(P::InvokeComponent);
        }
        if depth > 0 {
            productions.extend([
                P::If,
                P::ForRange,
                P::ForLoop,
                P::Let,
                P::LetFragment,
                P::BoolMatch,
                P::OptionMatch,
            ]);
            if !self.enums.is_empty() {
                productions.push(P::EnumMatch);
            }
        }
        match self.u.choose(&productions).unwrap() {
            P::Write => b.write(self.u.choose(WORDS).unwrap()),
            P::WriteExpr => {
                let expr = self.expr(b, &Type::String, depth);
                b.write_expr_escaped(expr);
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
            P::LetFragment => {
                // The two body closures both need `&mut self`; the builder
                // runs them sequentially, so route the reborrow through a
                // RefCell to satisfy the borrow checker.
                let var = self.fresh_var_name();
                let this = RefCell::new(self);
                b.let_fragment(
                    &var,
                    |b| this.borrow_mut().stmts(b, depth - 1),
                    |b| this.borrow_mut().stmts(b, depth - 1),
                );
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
                b.enum_match_stmt(subject, |arms| {
                    for (variant, fields) in &info.variants {
                        let bindings: Vec<(String, String)> = fields
                            .iter()
                            .map(|(field, _)| (field.clone(), self.fresh_var_name()))
                            .collect();
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
            OptionIsNone,
            OptionIsSome,
            StringIsEmpty,
            ArrayIsEmpty,
            ArrayLength,
            Neg,
            Add,
            Sub,
            Mul,
        }
        let mut productions = vec![P::Lit];
        if b.vars().iter().any(|(_, ty)| **ty == *target) {
            productions.push(P::Var);
        }
        if b.vars().iter().any(|(_, var_ty)| match &**var_ty {
            Type::Named { name, .. } => self
                .records
                .iter()
                .find(|r| r.name == name.as_str())
                .is_some_and(|rec| {
                    rec.fields
                        .iter()
                        .any(|(_, field_ty)| *b.resolve_type(field_ty) == *target)
                }),
            _ => false,
        }) {
            productions.push(P::FieldAccess);
        }
        if depth > 0 {
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
                productions.extend([P::ArrayLength, P::Neg, P::Add, P::Sub, P::Mul]);
            }
            if *target == Type::String {
                productions.extend([P::Concat, P::IntToString]);
            }
        }
        match self.u.choose(&productions).unwrap() {
            P::Lit => self.literal(b, target, depth),
            P::Var => {
                let candidates: Vec<&str> = b
                    .vars()
                    .iter()
                    .filter(|(_, ty)| **ty == *target)
                    .map(|(name, _)| name.as_str())
                    .collect();
                b.var(self.u.choose(&candidates).unwrap())
            }
            P::FieldAccess => {
                let mut candidates: Vec<(&str, &str)> = Vec::new();
                for (var_name, var_ty) in b.vars() {
                    if let Type::Named { name, .. } = &**var_ty {
                        if let Some(rec) = self.records.iter().find(|r| r.name == name.as_str()) {
                            for (field, field_ty) in &rec.fields {
                                if *b.resolve_type(field_ty) == *target {
                                    candidates.push((var_name.as_str(), field.as_str()));
                                }
                            }
                        }
                    }
                }
                let (var, field) = *self.u.choose(&candidates).unwrap();
                let object = b.var(var);
                b.field_access(object, field)
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
                b.enum_match_expr(subject, |arms| {
                    for (variant, fields) in &info.variants {
                        let bindings: Vec<(String, String)> = fields
                            .iter()
                            .map(|(field, _)| (field.clone(), self.fresh_var_name()))
                            .collect();
                        arms.arm_bound(
                            variant,
                            bindings.iter().map(|(f, v)| (f.as_str(), v.as_str())),
                            |b| self.expr(b, target, depth - 1),
                        );
                    }
                })
            }
            P::Eq => {
                let ty = match self.index(3) {
                    0 => Type::Int,
                    1 => Type::String,
                    _ => Type::Bool,
                };
                let left = self.expr(b, &ty, depth - 1);
                let right = self.expr(b, &ty, depth - 1);
                b.eq(left, right)
            }
            P::Lt => {
                let left = self.expr(b, &Type::Int, depth - 1);
                let right = self.expr(b, &Type::Int, depth - 1);
                b.lt(left, right)
            }
            P::Lte => {
                let left = self.expr(b, &Type::Int, depth - 1);
                let right = self.expr(b, &Type::Int, depth - 1);
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
                let operand = self.expr(b, &Type::Int, depth - 1);
                b.neg(operand)
            }
            P::Add => {
                let left = self.expr(b, &Type::Int, depth - 1);
                let right = self.expr(b, &Type::Int, depth - 1);
                b.add(left, right)
            }
            P::Sub => {
                let left = self.expr(b, &Type::Int, depth - 1);
                let right = self.expr(b, &Type::Int, depth - 1);
                b.sub(left, right)
            }
            P::Mul => {
                let left = self.expr(b, &Type::Int, depth - 1);
                let right = self.expr(b, &Type::Int, depth - 1);
                b.mul(left, right)
            }
        }
    }

    /// A literal expression of the target type.
    fn literal(&mut self, b: &IrBuilder, target: &Type, depth: usize) -> IrExpr {
        // Note: we use saturating_sub here since we might be forced to construct
        // something deeper than depth.
        match &target {
            Type::String => b.str(self.u.choose(WORDS).unwrap()),
            Type::Int => b.int(self.zigzag(100)),
            Type::Bool => b.bool(self.coin()),
            Type::Array(inner) => {
                let elements = (0..self.count(0..=3))
                    .map(|_| self.expr(b, inner, depth.saturating_sub(1)))
                    .collect();
                b.array_typed(inner.clone(), elements)
            }
            Type::Option(inner) => {
                if self.coin() {
                    let value = self.expr(b, inner, depth.saturating_sub(1));
                    b.some(value)
                } else {
                    b.none_typed(inner.clone())
                }
            }
            Type::Named { name, .. } => {
                if let Some(rec) = self.records.iter().find(|r| r.name == name.as_str()) {
                    let fields = rec.fields.clone();
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
                    let mut values = Vec::new();
                    for (field, field_ty) in fields {
                        let ty = b.resolve_type(field_ty);
                        values.push((field.as_str(), self.expr(b, &ty, depth.saturating_sub(1))));
                    }
                    b.enum_variant_with_fields(name.as_str(), variant, values)
                }
            }
            Type::Float | Type::Fragment => {
                unreachable!("Float and Fragment are never generation targets")
            }
        }
    }
}
