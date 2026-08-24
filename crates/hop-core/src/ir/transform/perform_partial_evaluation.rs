use std::collections::HashMap;
use std::sync::Arc;

use crate::document::CheapString;
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::Type;
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::ir::{
    IrExpr,
    ir_module::{ExprId, ExprIdCounter},
    ir_module::{IrStatement, VarId, traverse_statements_mut},
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use datafrog::{Iteration, Relation};
use tailwind_merge::tw_merge;

/// Binary operations that can be evaluated when both operands are constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum BinaryOp {
    Equals,
    StringConcat,
    LogicalOr,
    LogicalAnd,
    NumericAdd,
    NumericSubtract,
    NumericMultiply,
    LessThan,
    LessThanOrEqual,
}

/// Unary operations that can be evaluated when the operand is constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum UnaryOp {
    TwMerge,
    BooleanNegation,
    NumericNegation,
    IntToString,
    FloatToInt,
    IntToFloat,
    StringIsEmpty,
    OptionIsSome,
    OptionIsNone,
    ArrayIsEmpty,
    ArrayLength,
}

/// Constant values that can be tracked during partial evaluation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum Const {
    Bool(bool),
    String(CheapString),
    Int(i32),
    /// A float stored as its bit pattern (u64).
    Float(u64),
    Enum {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field expression IDs for reconstructing the enum literal.
        /// Empty for unit variants.
        fields: Vec<(FieldName, ExprId)>,
    },
    Option(Option<ExprId>),
    Array(Vec<ExprId>),
    Record {
        record_name: TypeName,
        /// Field expression IDs for reconstructing the record literal.
        fields: Vec<(FieldName, ExprId)>,
    },
}

impl Const {
    /// Convert a Const back to an IrExpr. Returns None if conversion is not possible
    /// (e.g. if inner value of an Option is not in const_map).
    fn to_expr(
        &self,
        id: ExprId,
        kind: Arc<Type>,
        known_expr_map: &HashMap<ExprId, Const>,
        registry: &TypeRegistry,
        expr_ids: &mut ExprIdCounter,
    ) -> Option<IrExpr> {
        Some(match self {
            Const::Bool(b) => IrExpr::BooleanLiteral { value: *b, id },
            Const::String(s) => IrExpr::StringLiteral {
                value: s.clone(),
                id,
            },
            Const::Int(n) => IrExpr::IntLiteral { value: *n, id },
            Const::Float(bits) => IrExpr::FloatLiteral {
                value: f64::from_bits(*bits),
                id,
            },
            Const::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                // Reconstruct field expressions from const_map
                let Some(ResolvedType::Enum { variants, .. }) = registry.resolve(&kind) else {
                    panic!("Const::Enum must have enum type, got {:?}", kind);
                };
                let variant_fields = &variants
                    .iter()
                    .find(|variant| variant.name.as_str() == variant_name.as_str())
                    .unwrap_or_else(|| panic!("enum {enum_name} has no variant {variant_name}"))
                    .fields;

                let reconstructed_fields: Option<Vec<_>> = fields
                    .iter()
                    .map(|(field_name, field_id)| {
                        let field_type = variant_fields
                            .iter()
                            .find(|(f, _, _)| f.as_str() == field_name.as_str())
                            .map(|(_, t, _)| t.clone())
                            .unwrap_or_else(|| {
                                panic!(
                                    "variant {variant_name} of enum {enum_name} has no field {}",
                                    field_name.as_str()
                                )
                            });

                        let field_const = known_expr_map.get(field_id)?;
                        let field_expr = field_const.to_expr(
                            expr_ids.next(),
                            field_type,
                            known_expr_map,
                            registry,
                            expr_ids,
                        )?;
                        Some((field_name.clone(), field_expr))
                    })
                    .collect();

                IrExpr::EnumLiteral {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    fields: reconstructed_fields?,
                    kind: kind.clone(),
                    id,
                }
            }
            Const::Option(inner_id) => {
                let inner_expr = match inner_id {
                    None => None,
                    Some(id) => {
                        let inner_const = known_expr_map.get(id)?;
                        // Extract the inner type: Option<T> -> T
                        let inner_kind = match &*kind {
                            Type::Option(inner) => inner.clone(),
                            _ => panic!("Const::Option must have Option type, got {:?}", kind),
                        };
                        Some(Box::new(inner_const.to_expr(
                            expr_ids.next(),
                            inner_kind,
                            known_expr_map,
                            registry,
                            expr_ids,
                        )?))
                    }
                };
                IrExpr::OptionLiteral {
                    value: inner_expr,
                    kind,
                    id,
                }
            }
            Const::Array(element_ids) => {
                // Extract the element type: Array<T> -> T
                let element_kind = match &*kind {
                    Type::Array(inner) => inner.clone(),
                    _ => panic!("Const::Array must have array type, got {:?}", kind),
                };
                let elements: Option<Vec<_>> = element_ids
                    .iter()
                    .map(|element_id| {
                        let element_const = known_expr_map.get(element_id)?;
                        element_const.to_expr(
                            expr_ids.next(),
                            element_kind.clone(),
                            known_expr_map,
                            registry,
                            expr_ids,
                        )
                    })
                    .collect();
                IrExpr::ArrayLiteral {
                    elements: elements?,
                    kind,
                    id,
                }
            }
            Const::Record {
                record_name,
                fields,
            } => {
                // Reconstruct field expressions from const_map
                let Some(ResolvedType::Record {
                    fields: declared_fields,
                    ..
                }) = registry.resolve(&kind)
                else {
                    panic!("Const::Record must have record type, got {:?}", kind);
                };

                let reconstructed_fields: Option<Vec<_>> = fields
                    .iter()
                    .map(|(field_name, field_id)| {
                        let field_type = declared_fields
                            .iter()
                            .find(|(f, _, _)| f.as_str() == field_name.as_str())
                            .map(|(_, t, _)| t.clone())
                            .unwrap_or_else(|| {
                                panic!("record {record_name} has no field {}", field_name.as_str())
                            });

                        let field_const = known_expr_map.get(field_id)?;
                        let field_expr = field_const.to_expr(
                            expr_ids.next(),
                            field_type,
                            known_expr_map,
                            registry,
                            expr_ids,
                        )?;
                        Some((field_name.clone(), field_expr))
                    })
                    .collect();

                IrExpr::RecordLiteral {
                    record_name: record_name.clone(),
                    fields: reconstructed_fields?,
                    kind: kind.clone(),
                    id,
                }
            }
        })
    }
}

/// A datafrog-based partial evaluation pass that tracks and propagates
/// constant values.
pub fn perform_partial_evaluation(
    body: &mut Vec<IrStatement>,
    registry: &TypeRegistry,
    expr_ids: &mut ExprIdCounter,
) {
    let mut iteration = Iteration::new();

    // Binding maps.
    //
    // Every binder within a declaration has a distinct VarId, so bindings can
    // go into flat HashMaps without any bookkeeping of scope.
    let mut variable_bindings: HashMap<VarId, ExprId> = HashMap::new();
    let mut option_bindings: HashMap<VarId, ExprId> = HashMap::new();
    let mut enum_field_bindings: HashMap<VarId, (ExprId, TypeName, FieldName)> = HashMap::new();

    let mut initial_constants: Vec<(ExprId, Const)> = Vec::new();

    // Unary operations: (operand_expr_id => parent_expr_id)
    let mut unary_operands: Vec<(ExprId, ExprId)> = Vec::new();
    let mut unary_ops: Vec<(ExprId, UnaryOp)> = Vec::new();

    // Binary operands: (operand_expr_id => parent_expr_id)
    let mut binary_left_operands: Vec<(ExprId, ExprId)> = Vec::new();
    let mut binary_right_operands: Vec<(ExprId, ExprId)> = Vec::new();
    let mut binary_ops: Vec<(ExprId, BinaryOp)> = Vec::new();

    // Variable references: (defining_expr_id => reference_expr_id)
    let mut variable_references: Vec<(ExprId, ExprId)> = Vec::new();
    // Option binding references: (defining_expr_id => reference_expr_id)
    let mut option_binding_references: Vec<(ExprId, ExprId)> = Vec::new();
    // Enum binding references: ((defining_expr_id, variant_name, field_name) => reference_expr_id)
    let mut enum_binding_references: Vec<((ExprId, TypeName, FieldName), ExprId)> = Vec::new();

    let mut enum_match_subjects: Vec<(ExprId, ExprId)> = Vec::new();
    let mut enum_match_arm_entries: Vec<((ExprId, TypeName, TypeName), ExprId)> = Vec::new();

    let mut option_match_subjects: Vec<(ExprId, ExprId)> = Vec::new();
    let mut option_match_arm_entries: Vec<((ExprId, bool), ExprId)> = Vec::new();

    let mut bool_match_subjects: Vec<(ExprId, ExprId)> = Vec::new();
    let mut bool_match_arm_entries: Vec<((ExprId, bool), ExprId)> = Vec::new();

    // Let expr bodies: (body_expr_id => let_expr_id)
    let mut let_expr_bodies: Vec<(ExprId, ExprId)> = Vec::new();

    // Enum field values: (enum_expr_id => (variant_name, field_name, field_expr_id))
    let mut enum_fields: Vec<(ExprId, (TypeName, FieldName, ExprId))> = Vec::new();

    // Record field values: (record_expr_id => (field_name, field_expr_id))
    let mut record_fields: Vec<(ExprId, (FieldName, ExprId))> = Vec::new();

    // Field accesses: ((record_expr_id, field_name) => access_expr_id)
    let mut field_access_entries: Vec<((ExprId, FieldName), ExprId)> = Vec::new();

    // Options contained values: (option_expr_id => contained_expr_id)
    let mut option_contained_values: Vec<(ExprId, ExprId)> = Vec::new();

    for stmt in body.iter() {
        stmt.traverse(&mut |s| {
            // Collect statement-level bindings
            match s {
                IrStatement::Let { var, value, .. } => {
                    variable_bindings.insert(var.id, value.id());
                }
                IrStatement::For { .. } => {}
                IrStatement::Match { .. } => {}
                IrStatement::Write { .. }
                | IrStatement::WriteExpr { .. }
                | IrStatement::ComponentInvocation { .. } => {
                    // No bindings
                }
            }

            s.traverse_exprs(&mut |expr| {
                match expr {
                    // Simple constants
                    IrExpr::BooleanLiteral { value, .. } => {
                        initial_constants.push((expr.id(), Const::Bool(*value)));
                    }
                    IrExpr::StringLiteral { value, .. } => {
                        initial_constants.push((expr.id(), Const::String(value.clone())));
                    }
                    IrExpr::IntLiteral { value, .. } => {
                        initial_constants.push((expr.id(), Const::Int(*value)));
                    }
                    IrExpr::FloatLiteral { value, .. } => {
                        initial_constants.push((expr.id(), Const::Float(value.to_bits())));
                    }
                    // Unary ops
                    IrExpr::BooleanNegation { operand, .. } => {
                        unary_operands.push((operand.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::BooleanNegation));
                    }
                    IrExpr::TwMerge { operand, .. } => {
                        unary_operands.push((operand.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::TwMerge));
                    }
                    IrExpr::NumericNegation { operand, .. } => {
                        unary_operands.push((operand.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::NumericNegation));
                    }
                    IrExpr::IntToString { value, .. } => {
                        unary_operands.push((value.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::IntToString));
                    }
                    IrExpr::FloatToInt { value, .. } => {
                        unary_operands.push((value.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::FloatToInt));
                    }
                    IrExpr::IntToFloat { value, .. } => {
                        unary_operands.push((value.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::IntToFloat));
                    }
                    // Binary ops
                    IrExpr::BooleanLogicalOr { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::LogicalOr));
                    }
                    IrExpr::BooleanLogicalAnd { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::LogicalAnd));
                    }
                    IrExpr::Equals { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::Equals));
                    }
                    IrExpr::StringConcat { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::StringConcat));
                    }
                    IrExpr::NumericAdd { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::NumericAdd));
                    }
                    IrExpr::NumericSubtract { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::NumericSubtract));
                    }
                    IrExpr::NumericMultiply { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::NumericMultiply));
                    }
                    IrExpr::LessThan { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::LessThan));
                    }
                    IrExpr::LessThanOrEqual { left, right, .. } => {
                        binary_left_operands.push((left.id(), expr.id()));
                        binary_right_operands.push((right.id(), expr.id()));
                        binary_ops.push((expr.id(), BinaryOp::LessThanOrEqual));
                    }
                    // Other
                    IrExpr::EnumLiteral {
                        enum_name,
                        variant_name,
                        fields,
                        ..
                    } => {
                        // Track enum as constant (variant info for match
                        // selection + field IDs for reconstruction)
                        let field_ids: Vec<(FieldName, ExprId)> = fields
                            .iter()
                            .map(|(name, expr)| (name.clone(), expr.id()))
                            .collect();
                        initial_constants.push((
                            expr.id(),
                            Const::Enum {
                                enum_name: enum_name.clone(),
                                variant_name: variant_name.clone(),
                                fields: field_ids.clone(),
                            },
                        ));
                        // Track field values for binding propagation
                        for (field_name, field_id) in field_ids {
                            enum_fields
                                .push((expr.id(), (variant_name.clone(), field_name, field_id)));
                        }
                    }
                    IrExpr::Match { match_, .. } => match match_ {
                        Match::Enum { subject, arms } => {
                            let subject_id = subject.id();
                            enum_match_subjects.push((subject_id, expr.id()));
                            for arm in arms {
                                let EnumPattern::Variant { variant_name, .. } = &arm.pattern;
                                for (field_name, binding_name) in &arm.bindings {
                                    enum_field_bindings.insert(
                                        binding_name.id,
                                        (subject_id, variant_name.clone(), field_name.clone()),
                                    );
                                }
                            }
                            for arm in arms {
                                match &arm.pattern {
                                    EnumPattern::Variant {
                                        enum_name,
                                        variant_name,
                                    } => {
                                        enum_match_arm_entries.push((
                                            (expr.id(), enum_name.clone(), variant_name.clone()),
                                            arm.body.id(),
                                        ));
                                    }
                                }
                            }
                        }
                        Match::Bool {
                            subject,
                            true_body,
                            false_body,
                        } => {
                            let subject_id = subject.id();
                            bool_match_subjects.push((subject_id, expr.id()));
                            bool_match_arm_entries.push(((expr.id(), true), true_body.id()));
                            bool_match_arm_entries.push(((expr.id(), false), false_body.id()));
                        }
                        Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body,
                            none_arm_body,
                        } => {
                            let subject_id = subject.id();
                            option_match_subjects.push((subject_id, expr.id()));
                            option_match_arm_entries.push(((expr.id(), true), some_arm_body.id()));
                            option_match_arm_entries.push(((expr.id(), false), none_arm_body.id()));
                            if let Some(binding) = some_arm_binding {
                                option_bindings.insert(binding.id, subject_id);
                            }
                        }
                    },
                    IrExpr::Var { value: var, .. } => {
                        if let Some(def_expr_id) = variable_bindings.get(&var.id) {
                            variable_references.push((*def_expr_id, expr.id()));
                        } else if let Some(subject_def_id) = option_bindings.get(&var.id) {
                            option_binding_references.push((*subject_def_id, expr.id()));
                        } else if let Some((subject_def_id, variant_name, field_name)) =
                            enum_field_bindings.get(&var.id)
                        {
                            enum_binding_references.push((
                                (*subject_def_id, variant_name.clone(), field_name.clone()),
                                expr.id(),
                            ));
                        }
                    }
                    IrExpr::OptionLiteral { value, .. } => {
                        let inner_id = value.as_ref().map(|inner| inner.id());
                        initial_constants.push((expr.id(), Const::Option(inner_id)));
                        if let Some(inner) = value {
                            option_contained_values.push((expr.id(), inner.id()));
                        }
                    }
                    IrExpr::Let {
                        var, value, body, ..
                    } => {
                        variable_bindings.insert(var.id, value.id());
                        let_expr_bodies.push((body.id(), expr.id()));
                    }
                    IrExpr::RecordLiteral {
                        record_name,
                        fields,
                        ..
                    } => {
                        // Track record as constant (field IDs for reconstruction)
                        let field_ids: Vec<(FieldName, ExprId)> = fields
                            .iter()
                            .map(|(name, expr)| (name.clone(), expr.id()))
                            .collect();
                        initial_constants.push((
                            expr.id(),
                            Const::Record {
                                record_name: record_name.clone(),
                                fields: field_ids.clone(),
                            },
                        ));
                        // Track field values for field access propagation
                        for (field_name, field_id) in field_ids {
                            record_fields.push((expr.id(), (field_name, field_id)));
                        }
                    }
                    IrExpr::ArrayLiteral { elements, .. } => {
                        let element_ids: Vec<ExprId> = elements.iter().map(|e| e.id()).collect();
                        initial_constants.push((expr.id(), Const::Array(element_ids)));
                    }
                    IrExpr::FieldAccess { record, field, .. } => {
                        field_access_entries.push(((record.id(), field.clone()), expr.id()));
                    }
                    IrExpr::ArrayLength { array, .. } => {
                        unary_operands.push((array.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::ArrayLength));
                    }
                    IrExpr::ArrayIsEmpty { array, .. } => {
                        unary_operands.push((array.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::ArrayIsEmpty));
                    }
                    IrExpr::StringIsEmpty { string, .. } => {
                        unary_operands.push((string.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::StringIsEmpty));
                    }
                    IrExpr::OptionIsSome { option, .. } => {
                        unary_operands.push((option.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::OptionIsSome));
                    }
                    IrExpr::OptionIsNone { option, .. } => {
                        unary_operands.push((option.id(), expr.id()));
                        unary_ops.push((expr.id(), UnaryOp::OptionIsNone));
                    }
                    IrExpr::FragmentEmpty { .. } => {
                        // Leaf constant, no sub-expressions to analyze
                    }
                    IrExpr::Fragment { .. } => {
                        // The statements inside are visited by the enclosing
                        // statement traversal.
                    }
                }
            });
        });
    }

    // Known values of expressions: (expr_id => known_value)
    let known_expr_value = iteration.variable::<(ExprId, Const)>("known_expr_value");
    known_expr_value.extend(initial_constants);

    // Unary operations
    let unary_op = Relation::from_vec(unary_ops);
    let unary_operand = Relation::from_vec(unary_operands);
    let unary_known_value = iteration.variable::<(ExprId, Const)>("unary_op_known_value");

    // Binary operations
    let binary_op = Relation::from_vec(binary_ops);
    let binary_left_operand = Relation::from_vec(binary_left_operands);
    let binary_right_operand = Relation::from_vec(binary_right_operands);
    let binary_left_known_value = iteration.variable::<(ExprId, Const)>("binary_left_known_value");
    let binary_right_known_value =
        iteration.variable::<(ExprId, Const)>("binary_right_known_value");
    let binary_known_values = iteration.variable::<(ExprId, (Const, Const))>("binary_known_values");

    // Enum match subject expressions: (subject_id => match_id)
    let enum_match_subject = Relation::from_vec(enum_match_subjects);

    // Enum match arms: ((match_id, enum_name, variant_name) => arm_body_id)
    let enum_match_arms = Relation::from_vec(enum_match_arm_entries);

    // Enum matches with known subjects: ((match_id, enum_name, variant_name) => match_id)
    let match_expr_with_known_enum =
        iteration.variable::<((ExprId, TypeName, TypeName), ExprId)>("match_with_known_enum");

    // Option match subject expressions: (subject_id => match_id)
    let option_match_subject = Relation::from_vec(option_match_subjects);

    // Option match arms: ((match_id, is_some) => body_id)
    let option_match_arms = Relation::from_vec(option_match_arm_entries);

    // Option matches with known subjects: ((match_id, is_some) => match_id)
    let match_expr_with_known_option =
        iteration.variable::<((ExprId, bool), ExprId)>("match_with_known_option");

    // Bool match subject expressions: (subject_id => match_id)
    let bool_match_subject = Relation::from_vec(bool_match_subjects);

    // Bool match arms: ((match_id, is_true) => body_id)
    let bool_match_arms = Relation::from_vec(bool_match_arm_entries);

    // Bool matches with known subjects: ((match_id, is_true) => match_id)
    let match_expr_with_known_bool =
        iteration.variable::<((ExprId, bool), ExprId)>("match_with_known_bool");

    // Option contained value: (option_expr_id => contained_expr_id)
    let option_contained_value = iteration.variable::<(ExprId, ExprId)>("option_contained_value");
    option_contained_value.extend(option_contained_values);

    // Option binding uses: (subject_def_expr_id => binding_var_expr_id)
    let option_binding_reference = Relation::from_vec(option_binding_references);

    // Enum field expression ids: (enum_expr_id => (variant_name, field_name, field_expr_id))
    // Used for propagating through variable bindings
    let enum_field = iteration.variable::<(ExprId, (TypeName, FieldName, ExprId))>("enum_field");
    enum_field.extend(enum_fields.clone());

    // Enum field keyed by (expr_id, variant_name, field_name): ((enum_expr_id, variant_name, field_name) => field_expr_id)
    // Used for joining with binding uses. Keying by variant_name too matters
    // because different variants of the same enum may share a field name.
    let enum_field_keyed =
        iteration.variable::<((ExprId, TypeName, FieldName), ExprId)>("enum_field_keyed");
    enum_field_keyed.extend(enum_fields.into_iter().map(
        |(expr_id, (variant_name, field_name, field_id))| {
            ((expr_id, variant_name, field_name), field_id)
        },
    ));

    // Enum binding uses: ((subject_def_expr_id, variant_name, field_name) => binding_var_expr_id)
    let enum_binding_use = Relation::from_vec(enum_binding_references);

    // Record field expression ids: (record_expr_id => (field_name, field_expr_id))
    // Used for propagating through variable bindings
    let record_field = iteration.variable::<(ExprId, (FieldName, ExprId))>("record_field");
    record_field.extend(record_fields.clone());

    // Record field keyed by (expr_id, field_name): ((record_expr_id, field_name) => field_expr_id)
    // Used for joining with field accesses.
    let record_field_keyed =
        iteration.variable::<((ExprId, FieldName), ExprId)>("record_field_keyed");
    record_field_keyed.extend(
        record_fields
            .into_iter()
            .map(|(expr_id, (field_name, field_id))| ((expr_id, field_name), field_id)),
    );

    // Field access uses: ((record_expr_id, field_name) => access_expr_id)
    let field_access_use = Relation::from_vec(field_access_entries);

    // Constant propagation: `propagate_to(x, y)` means "y computes the same value as x".
    let propagate_to = iteration.variable::<(ExprId, ExprId)>("propagate_to");
    propagate_to.extend(variable_references);
    propagate_to.extend(let_expr_bodies);

    while iteration.changed() {
        // Evaluate unary operations
        {
            unary_known_value.from_join(
                &known_expr_value,
                &unary_operand,
                |_: &ExprId, known_val: &Const, parent_expr_id: &ExprId| {
                    (*parent_expr_id, known_val.clone())
                },
            );
            known_expr_value.from_join(
                &unary_known_value,
                &unary_op,
                |parent_expr_id: &ExprId, known_val: &Const, op: &UnaryOp| {
                    let result = match op {
                        UnaryOp::BooleanNegation => match known_val {
                            Const::Bool(b) => Const::Bool(!b),
                            _ => {
                                unreachable!("Boolean negation can only have boolean operands")
                            }
                        },
                        UnaryOp::TwMerge => match known_val {
                            Const::String(s) => {
                                Const::String(CheapString::new(tw_merge(s.as_str())))
                            }
                            _ => unreachable!("TwMerge can only have string operands"),
                        },
                        UnaryOp::NumericNegation => match known_val {
                            Const::Int(n) => Const::Int(n.wrapping_neg()),
                            Const::Float(bits) => Const::Float((-f64::from_bits(*bits)).to_bits()),
                            _ => unreachable!("Numeric negation can only have numeric operands"),
                        },
                        UnaryOp::IntToString => match known_val {
                            Const::Int(n) => Const::String(CheapString::new(n.to_string())),
                            _ => unreachable!("IntToString can only have int operands"),
                        },
                        UnaryOp::FloatToInt => match known_val {
                            Const::Float(bits) => Const::Int(f64::from_bits(*bits) as i32),
                            _ => unreachable!("FloatToInt can only have float operands"),
                        },
                        UnaryOp::IntToFloat => match known_val {
                            Const::Int(n) => Const::Float((*n as f64).to_bits()),
                            _ => unreachable!("IntToFloat can only have int operands"),
                        },
                        UnaryOp::StringIsEmpty => match known_val {
                            Const::String(s) => Const::Bool(s.as_str().is_empty()),
                            _ => unreachable!("StringIsEmpty can only have string operands"),
                        },
                        UnaryOp::OptionIsSome => match known_val {
                            Const::Option(inner) => Const::Bool(inner.is_some()),
                            _ => unreachable!("OptionIsSome can only have option operands"),
                        },
                        UnaryOp::OptionIsNone => match known_val {
                            Const::Option(inner) => Const::Bool(inner.is_none()),
                            _ => unreachable!("OptionIsNone can only have option operands"),
                        },
                        UnaryOp::ArrayIsEmpty => match known_val {
                            Const::Array(elements) => Const::Bool(elements.is_empty()),
                            _ => unreachable!("ArrayIsEmpty can only have array operands"),
                        },
                        UnaryOp::ArrayLength => match known_val {
                            Const::Array(elements) => Const::Int(elements.len() as i32),
                            _ => unreachable!("ArrayLength can only have array operands"),
                        },
                    };
                    (*parent_expr_id, result)
                },
            );
        }

        // Evaluate binary operations
        {
            binary_left_known_value.from_join(
                &known_expr_value,
                &binary_left_operand,
                |_: &ExprId, known_val: &Const, parent_expr_id: &ExprId| {
                    (*parent_expr_id, known_val.clone())
                },
            );
            binary_right_known_value.from_join(
                &known_expr_value,
                &binary_right_operand,
                |_: &ExprId, known_val: &Const, parent_expr_id: &ExprId| {
                    (*parent_expr_id, known_val.clone())
                },
            );
            binary_known_values.from_join(
                &binary_left_known_value,
                &binary_right_known_value,
                |parent_expr_id: &ExprId, known_left_val: &Const, known_right_val: &Const| {
                    (
                        *parent_expr_id,
                        (known_left_val.clone(), known_right_val.clone()),
                    )
                },
            );
            known_expr_value.from_join(
                &binary_known_values,
                &binary_op,
                |parent_expr_id: &ExprId,
                 (known_left_val, known_right_val): &(Const, Const),
                 op: &BinaryOp| {
                    let result = match op {
                        BinaryOp::Equals => match (known_left_val, known_right_val) {
                            (Const::Bool(l), Const::Bool(r)) => Const::Bool(l == r),
                            (Const::String(l), Const::String(r)) => Const::Bool(l == r),
                            (Const::Int(l), Const::Int(r)) => Const::Bool(l == r),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Bool(f64::from_bits(*l) == f64::from_bits(*r))
                            }
                            _ => unreachable!(
                                "Equals can only have String, Bool, Int or Float operands"
                            ),
                        },
                        BinaryOp::StringConcat => match (known_left_val, known_right_val) {
                            (Const::String(l), Const::String(r)) => {
                                let s = format!("{}{}", l, r);
                                Const::String(CheapString::new(s))
                            }
                            _ => unreachable!("StringConcat can only have string operands"),
                        },
                        BinaryOp::LogicalOr => match (known_left_val, known_right_val) {
                            (Const::Bool(l), Const::Bool(r)) => Const::Bool(*l || *r),
                            _ => unreachable!("LogicalOr can only have boolean operands"),
                        },
                        BinaryOp::LogicalAnd => match (known_left_val, known_right_val) {
                            (Const::Bool(l), Const::Bool(r)) => Const::Bool(*l && *r),
                            _ => unreachable!("LogicalAnd can only have boolean operands"),
                        },
                        BinaryOp::NumericAdd => match (known_left_val, known_right_val) {
                            (Const::Int(l), Const::Int(r)) => Const::Int(l.wrapping_add(*r)),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Float((f64::from_bits(*l) + f64::from_bits(*r)).to_bits())
                            }
                            _ => unreachable!("NumericAdd can only have numeric operands"),
                        },
                        BinaryOp::NumericSubtract => match (known_left_val, known_right_val) {
                            (Const::Int(l), Const::Int(r)) => Const::Int(l.wrapping_sub(*r)),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Float((f64::from_bits(*l) - f64::from_bits(*r)).to_bits())
                            }
                            _ => unreachable!("NumericSubtract can only have numeric operands"),
                        },
                        BinaryOp::NumericMultiply => match (known_left_val, known_right_val) {
                            (Const::Int(l), Const::Int(r)) => Const::Int(l.wrapping_mul(*r)),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Float((f64::from_bits(*l) * f64::from_bits(*r)).to_bits())
                            }
                            _ => unreachable!("NumericMultiply can only have numeric operands"),
                        },
                        BinaryOp::LessThan => match (known_left_val, known_right_val) {
                            (Const::Int(l), Const::Int(r)) => Const::Bool(l < r),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Bool(f64::from_bits(*l) < f64::from_bits(*r))
                            }
                            _ => unreachable!("LessThan can only have numeric operands"),
                        },
                        BinaryOp::LessThanOrEqual => match (known_left_val, known_right_val) {
                            (Const::Int(l), Const::Int(r)) => Const::Bool(l <= r),
                            (Const::Float(l), Const::Float(r)) => {
                                Const::Bool(f64::from_bits(*l) <= f64::from_bits(*r))
                            }
                            _ => unreachable!("LessThanOrEqual can only have numeric operands"),
                        },
                    };
                    (*parent_expr_id, result)
                },
            );
        }

        // Constant propagation
        {
            known_expr_value.from_join(
                &known_expr_value,
                &propagate_to,
                |_source: &ExprId, val: &Const, target: &ExprId| (*target, val.clone()),
            );
            propagate_to.from_join(
                &option_contained_value,
                &option_binding_reference,
                |_subject: &ExprId, inner_id: &ExprId, binding_use_id: &ExprId| {
                    (*inner_id, *binding_use_id)
                },
            );
            option_contained_value.from_join(
                &option_contained_value,
                &propagate_to,
                |_source: &ExprId, inner_id: &ExprId, target: &ExprId| (*target, *inner_id),
            );
            propagate_to.from_join(
                &enum_field_keyed,
                &enum_binding_use,
                |_key: &(ExprId, TypeName, FieldName),
                 field_id: &ExprId,
                 binding_use_id: &ExprId| { (*field_id, *binding_use_id) },
            );
            enum_field.from_join(
                &enum_field,
                &propagate_to,
                |_source: &ExprId,
                 (variant_name, field_name, field_id): &(TypeName, FieldName, ExprId),
                 target: &ExprId| {
                    (
                        *target,
                        (variant_name.clone(), field_name.clone(), *field_id),
                    )
                },
            );
            propagate_to.from_join(
                &record_field_keyed,
                &field_access_use,
                |_key: &(ExprId, FieldName), field_id: &ExprId, access_expr_id: &ExprId| {
                    (*field_id, *access_expr_id)
                },
            );
            record_field.from_join(
                &record_field,
                &propagate_to,
                |_source: &ExprId,
                 (field_name, field_id): &(FieldName, ExprId),
                 target: &ExprId| { (*target, (field_name.clone(), *field_id)) },
            );
        }

        // Keep enum_field_keyed in sync with enum_field
        {
            enum_field_keyed.from_map(
                &enum_field,
                |&(target, (ref variant_name, ref field_name, field_id))| {
                    ((target, variant_name.clone(), field_name.clone()), field_id)
                },
            );
        }

        // Keep record_field_keyed in sync with record_field
        {
            record_field_keyed.from_map(&record_field, |&(target, (ref field_name, field_id))| {
                ((target, field_name.clone()), field_id)
            });
        }

        // Evaluate IrExpr::Match over Option
        {
            match_expr_with_known_option.from_join(
                &known_expr_value,
                &option_match_subject,
                |_subject: &ExprId, known_val: &Const, match_id: &ExprId| match known_val {
                    Const::Option(inner) => ((*match_id, inner.is_some()), *match_id),
                    _ => unreachable!("option match subject must have option constant"),
                },
            );

            propagate_to.from_join(
                &match_expr_with_known_option,
                &option_match_arms,
                |_key: &(ExprId, bool), match_id: &ExprId, body_id: &ExprId| (*body_id, *match_id),
            );
        }

        // Evaluate IrExpr::Match over Enum
        {
            match_expr_with_known_enum.from_join(
                &known_expr_value,
                &enum_match_subject,
                |_subject: &ExprId, known_val: &Const, match_id: &ExprId| match known_val {
                    Const::Enum {
                        enum_name,
                        variant_name,
                        ..
                    } => (
                        (*match_id, enum_name.clone(), variant_name.clone()),
                        *match_id,
                    ),
                    _ => unreachable!("enum match subject must have enum constant"),
                },
            );
            propagate_to.from_join(
                &match_expr_with_known_enum,
                &enum_match_arms,
                |_key: &(ExprId, TypeName, TypeName), match_id: &ExprId, arm_body: &ExprId| {
                    (*arm_body, *match_id)
                },
            );
        }

        // Evaluate IrExpr::Match over Bool
        {
            match_expr_with_known_bool.from_join(
                &known_expr_value,
                &bool_match_subject,
                |_subject: &ExprId, known_val: &Const, match_id: &ExprId| match known_val {
                    Const::Bool(b) => ((*match_id, *b), *match_id),
                    _ => unreachable!("bool match subject must have bool constant"),
                },
            );
            propagate_to.from_join(
                &match_expr_with_known_bool,
                &bool_match_arms,
                |_key: &(ExprId, bool), match_id: &ExprId, body_id: &ExprId| (*body_id, *match_id),
            );
        }
    }

    let known_expr_map: HashMap<ExprId, Const> =
        known_expr_value.complete().iter().cloned().collect();

    traverse_statements_mut(body, &mut |stmts| {
        for s in stmts.iter_mut() {
            s.traverse_exprs_mut(&mut |e| {
                if let Some(known_val) = known_expr_map.get(&e.id()) {
                    if let Some(expr) =
                        known_val.to_expr(e.id(), e.get_type(), &known_expr_map, registry, expr_ids)
                    {
                        *e = expr;
                    }
                }
            });
        }
    });
}

#[cfg(test)]
mod tests {
    use crate::ir::ir_module_builder::{IrModuleBodiesBuilder, IrModuleBuilder};
    use crate::ir::ir_module_generator::random_ir_module;
    use crate::ir::runtime::{evaluator::evaluate_view, random::random_value, value::Value};
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    use super::*;

    /// Run the pass over every view and component in the module, using the
    /// module's own registry so named type structure (e.g. enum variants and
    /// fields) can be resolved during reconstruction.
    fn check(builder: impl Into<IrModuleBodiesBuilder>, expected: Expect) {
        let (mut module, registry) = builder.into().build_with_registry();
        let before = module.to_string();
        let expr_ids = &mut module.expr_ids;
        for view in &mut module.views {
            perform_partial_evaluation(&mut view.body, &registry, expr_ids);
        }
        for component in &mut module.components {
            perform_partial_evaluation(&mut component.body, &registry, expr_ids);
        }
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_partial_evaluation() {
        arbtest::arbtest(|u| {
            let (mut module, registry) = random_ir_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);

            // Generate args up-front so the exact same values are used to
            // evaluate before and after the pass.
            let view_args: Vec<(TypeName, HashMap<VarName, Value>)> = module
                .views
                .iter()
                .map(|view| {
                    let args = view
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name().clone(),
                                random_value(&mut rng, &p.typ, None, &registry),
                            )
                        })
                        .collect();
                    (view.name.clone(), args)
                })
                .collect();

            let before_module = module.to_string();
            let before_outputs: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            let expr_ids = &mut module.expr_ids;
            for view in &mut module.views {
                perform_partial_evaluation(&mut view.body, &registry, expr_ids);
            }
            for component in &mut module.components {
                perform_partial_evaluation(&mut component.body, &registry, expr_ids);
            }

            for ((view_name, args), before_output) in view_args.iter().zip(&before_outputs) {
                let after_output = evaluate_view(&module, view_name, args.clone()).unwrap();
                assert_eq!(
                    before_output, &after_output,
                    "view {view_name}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    #[test]
    fn should_evaluate_simple_boolean_negation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.not(t.bool(false)), |t| {
                    t.write("Should be true");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (!false) {
                    true => {
                      write("Should be true")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Should be true")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_double_boolean_negation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.not(t.not(t.bool(true))), |t| {
                    t.write("Double negation");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (!(!true)) {
                    true => {
                      write("Double negation")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Double negation")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_triple_boolean_negation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.not(t.not(t.not(t.bool(false)))), |t| {
                    t.write("Triple negation");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (!(!(!false))) {
                    true => {
                      write("Triple negation")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Triple negation")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_boolean_equality_comparisons() {
        let ep = IrModuleBuilder::new().view_no_params("Test", |t| {
            t.if_stmt(t.eq(t.bool(true), t.bool(true)), |t| {
                t.write("true == true");
            });
            t.if_stmt(t.eq(t.bool(false), t.bool(false)), |t| {
                t.write("false == false");
            });
            t.if_stmt(t.eq(t.bool(true), t.bool(false)), |t| {
                t.write("Should not appear");
            });
        });
        check(
            ep,
            expect![[r#"
                -- before --
                view Test() {
                  match (true == true) {
                    true => {
                      write("true == true")
                    }
                    false => {
                    }
                  }
                  match (false == false) {
                    true => {
                      write("false == false")
                    }
                    false => {
                    }
                  }
                  match (true == false) {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("true == true")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("false == false")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_equality_with_nested_negations() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(
                    t.eq(t.not(t.not(t.bool(false))), t.not(t.bool(false))),
                    |t| {
                        t.write("Should not appear");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match ((!(!false)) == (!false)) {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_constants_through_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.not(t.not(t.bool(true))), |t| {
                    t.if_stmt(t.var("x"), |t| {
                        t.write("x is true");
                    });
                    t.if_stmt(t.not(t.var("x")), |t| {
                        t.write("x is false");
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (!(!true)) in {
                    match v0 {
                      true => {
                        write("x is true")
                      }
                      false => {
                      }
                    }
                    match (!v0) {
                      true => {
                        write("x is false")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    match true {
                      true => {
                        write("x is true")
                      }
                      false => {
                      }
                    }
                    match false {
                      true => {
                        write("x is false")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_equality_with_variable_operands() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.bool(true), |t| {
                    t.let_stmt("y", t.not(t.bool(true)), |t| {
                        t.if_stmt(t.eq(t.var("x"), t.var("y")), |t| {
                            t.write("x equals y");
                        });
                        t.if_stmt(t.eq(t.var("x"), t.not(t.var("y"))), |t| {
                            t.write("x equals not y");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    let v1 = (!true) in {
                      match (v0 == v1) {
                        true => {
                          write("x equals y")
                        }
                        false => {
                        }
                      }
                      match (v0 == (!v1)) {
                        true => {
                          write("x equals not y")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    let v1 = false in {
                      match false {
                        true => {
                          write("x equals y")
                        }
                        false => {
                        }
                      }
                      match true {
                        true => {
                          write("x equals not y")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_string_constants_through_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("message", t.str("Hello, World!"), |t| {
                    t.write_expr_escaped(t.var("message"));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Hello, World!" in {
                    write_escaped(v0)
                  }
                }

                -- after --
                view Test() {
                  let v0 = "Hello, World!" in {
                    write_escaped("Hello, World!")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_nested_string_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("greeting", t.str("Hello"), |t| {
                    t.let_stmt("name", t.str("World"), |t| {
                        t.write_expr_escaped(t.var("greeting"));
                        t.write_expr_escaped(t.var("name"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Hello" in {
                    let v1 = "World" in {
                      write_escaped(v0)
                      write_escaped(v1)
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "Hello" in {
                    let v1 = "World" in {
                      write_escaped("Hello")
                      write_escaped("World")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_string_variable_to_multiple_uses() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("title", t.str("Welcome"), |t| {
                    t.write_expr_escaped(t.var("title"));
                    t.write_expr_escaped(t.var("title"));
                    t.let_stmt("subtitle", t.var("title"), |t| {
                        t.write_expr_escaped(t.var("subtitle"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Welcome" in {
                    write_escaped(v0)
                    write_escaped(v0)
                    let v1 = v0 in {
                      write_escaped(v1)
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "Welcome" in {
                    write_escaped("Welcome")
                    write_escaped("Welcome")
                    let v1 = "Welcome" in {
                      write_escaped("Welcome")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_string_equality_comparisons() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.eq(t.str("hello"), t.str("hello")), |t| {
                    t.write("Strings are equal");
                });
                t.if_stmt(t.eq(t.str("hello"), t.str("world")), |t| {
                    t.write("Should not appear");
                });
                t.let_stmt("greeting", t.str("hello"), |t| {
                    t.let_stmt("message", t.str("hello"), |t| {
                        t.if_stmt(t.eq(t.var("greeting"), t.var("message")), |t| {
                            t.write("Variables are equal");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match ("hello" == "hello") {
                    true => {
                      write("Strings are equal")
                    }
                    false => {
                    }
                  }
                  match ("hello" == "world") {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                  let v0 = "hello" in {
                    let v1 = "hello" in {
                      match (v0 == v1) {
                        true => {
                          write("Variables are equal")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Strings are equal")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                  let v0 = "hello" in {
                    let v1 = "hello" in {
                      match true {
                        true => {
                          write("Variables are equal")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_string_concatenation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write_expr_escaped(
                    t.string_concat(t.string_concat(t.str("Hello"), t.str(" ")), t.str("World")),
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped((("Hello" + " ") + "World"))
                }

                -- after --
                view Test() {
                  write_escaped("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_string_concatenation_in_equality() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(
                    t.eq(t.string_concat(t.str("foo"), t.str("bar")), t.str("foobar")),
                    |t| {
                        t.write("Concatenation matches");
                    },
                );
                t.if_stmt(
                    t.eq(
                        t.string_concat(t.str("hello"), t.str(" world")),
                        t.str("hello"),
                    ),
                    |t| {
                        t.write("Should not appear");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (("foo" + "bar") == "foobar") {
                    true => {
                      write("Concatenation matches")
                    }
                    false => {
                    }
                  }
                  match (("hello" + " world") == "hello") {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Concatenation matches")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_string_concatenation_with_propagated_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("prefix", t.str("Hello"), |t| {
                    t.let_stmt("suffix", t.string_concat(t.str(" "), t.str("World")), |t| {
                        t.let_stmt(
                            "full",
                            t.string_concat(t.var("prefix"), t.var("suffix")),
                            |t| {
                                t.write_expr_escaped(t.var("full"));
                            },
                        );
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Hello" in {
                    let v1 = (" " + "World") in {
                      let v2 = (v0 + v1) in {
                        write_escaped(v2)
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "Hello" in {
                    let v1 = " World" in {
                      let v2 = "Hello World" in {
                        write_escaped("Hello World")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_simple_match_expression() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Blue"])
                .view_no_params("Test", |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                        t.write_expr_escaped(t.enum_match_expr(t.var("color"), |m| {
                            m.arm("Red", |t| t.str("red"));
                            m.arm("Blue", |t| t.str("blue"));
                        }));
                    });
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    write_escaped(match v0 {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    write_escaped("red")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_match_with_literal_subject() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Blue"])
                .view_no_params("Test", |t| {
                    t.write_expr_escaped(t.enum_match_expr(t.enum_variant("Color", "Blue"), |m| {
                        m.arm("Red", |t| t.str("red"));
                        m.arm("Blue", |t| t.str("blue"));
                    }));
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  write_escaped(match Color::Blue {
                    Color::Red => "red",
                    Color::Blue => "blue",
                  })
                }

                -- after --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  write_escaped("blue")
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_constant_statement_match_subject() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("x")), |t| {
                    t.option_match_stmt(
                        t.var("opt"),
                        Some("v"),
                        |t| {
                            t.write_expr(t.var("v"), false);
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("x") in {
                    match v0 {
                      Some(v1) => {
                        write_expr(v1)
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("x") in {
                    match Option[String]::Some("x") {
                      Some(v1) => {
                        write_expr(v1)
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_match_with_variable_subject() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Blue"])
                .view_no_params("Test", |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Blue"), |t| {
                        t.write_expr_escaped(t.enum_match_expr(t.var("color"), |m| {
                            m.arm("Red", |t| t.str("red"));
                            m.arm("Blue", |t| t.str("blue"));
                        }));
                    });
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Blue in {
                    write_escaped(match v0 {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Blue in {
                    write_escaped("blue")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_match_with_constant_arm_body() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Blue"])
                .view_no_params("Test", |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                        t.if_stmt(
                            t.enum_match_expr(t.var("color"), |m| {
                                m.arm("Red", |t| t.not(t.bool(false)));
                                m.arm("Blue", |t| t.bool(false));
                            }),
                            |t| {
                                t.write("Match evaluated to true");
                            },
                        );
                    });
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    match match v0 {
                      Color::Red => (!false),
                      Color::Blue => false,
                    } {
                      true => {
                        write("Match evaluated to true")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    match true {
                      true => {
                        write("Match evaluated to true")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_match_in_equality() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Status", ["Active", "Inactive"])
                .view_no_params("Test", |t| {
                    t.let_stmt("status", t.enum_variant("Status", "Active"), |t| {
                        t.if_stmt(
                            t.eq(
                                t.enum_match_expr(t.var("status"), |m| {
                                    m.arm("Active", |t| t.str("on"));
                                    m.arm("Inactive", |t| t.str("off"));
                                }),
                                t.str("on"),
                            ),
                            |t| {
                                t.write("Status is active");
                            },
                        );
                    });
                }),
            expect![[r#"
                -- before --
                enum Status {
                  Active,
                  Inactive,
                }
                view Test() {
                  let v0 = Status::Active in {
                    match (match v0 {
                      Status::Active => "on",
                      Status::Inactive => "off",
                    } == "on") {
                      true => {
                        write("Status is active")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                enum Status {
                  Active,
                  Inactive,
                }
                view Test() {
                  let v0 = Status::Active in {
                    match true {
                      true => {
                        write("Status is active")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_constant_through_variables() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Blue"])
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.enum_variant("Color", "Red"), |t| {
                        t.let_stmt("y", t.var("x"), |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("y"), |m| {
                                m.arm("Red", |t| t.str("red"));
                                m.arm("Blue", |t| t.str("blue"));
                            }));
                        });
                    });
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    let v1 = v0 in {
                      write_escaped(match v1 {
                        Color::Red => "red",
                        Color::Blue => "blue",
                      })
                    }
                  }
                }

                -- after --
                enum Color {
                  Red,
                  Blue,
                }
                view Test() {
                  let v0 = Color::Red in {
                    let v1 = Color::Red in {
                      write_escaped("red")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn sibling_let_constant_propagation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("y", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("y"));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "first" in {
                    write_escaped(v0)
                  }
                  let v1 = "second" in {
                    write_escaped(v1)
                  }
                }

                -- after --
                view Test() {
                  let v0 = "first" in {
                    write_escaped("first")
                  }
                  let v1 = "second" in {
                    write_escaped("second")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_enum_variant_fields() {
        check(
            IrModuleBuilder::new()
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "x",
                        t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hi"))]),
                        |_| {},
                    );
                }),
            expect![[r#"
                -- before --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hi"} in {}
                }

                -- after --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hi"} in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_join_with_constant_strings() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                let classes = t.join(vec![t.str("flex"), t.str("items-center"), t.str("gap-4")]);
                t.write_expr_escaped(t.tw_merge(classes));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped(tw_merge((((("flex" + " ") + "items-center") + " ") + "gap-4")))
                }

                -- after --
                view Test() {
                  write_escaped("flex items-center gap-4")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_join_with_tailwind_conflicts() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                let classes = t.join(vec![t.str("px-4"), t.str("py-2"), t.str("p-6")]);
                t.write_expr_escaped(t.tw_merge(classes));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped(tw_merge((((("px-4" + " ") + "py-2") + " ") + "p-6")))
                }

                -- after --
                view Test() {
                  write_escaped("p-6")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_join_with_propagated_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("base", t.str("flex items-center"), |t| {
                    t.let_stmt("extra", t.str("gap-4 text-red-500"), |t| {
                        let classes = t.join(vec![t.var("base"), t.var("extra")]);
                        t.write_expr_escaped(t.tw_merge(classes));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "flex items-center" in {
                    let v1 = "gap-4 text-red-500" in {
                      write_escaped(tw_merge(((v0 + " ") + v1)))
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "flex items-center" in {
                    let v1 = "gap-4 text-red-500" in {
                      write_escaped("flex items-center gap-4 text-red-500")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_empty_join() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write_expr_escaped(t.join(vec![]));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped("")
                }

                -- after --
                view Test() {
                  write_escaped("")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_join() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                // Inner join just concatenates: "px-2 p-3"
                // Outer join concatenates: "px-4 px-2 p-3"
                // tw_merge resolves conflicts: "p-3"
                let classes = t.join(vec![
                    t.str("px-4"),
                    t.join(vec![t.str("px-2"), t.str("p-3")]),
                ]);
                t.write_expr_escaped(t.tw_merge(classes));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped(tw_merge((("px-4" + " ") + (("px-2" + " ") + "p-3"))))
                }

                -- after --
                view Test() {
                  write_escaped("p-3")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_tw_merge_with_enum_match() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Size", ["Small", "Large"])
                .view_no_params("Test", |t| {
                    t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                        let classes = t.join(vec![
                            t.str("px-4"),
                            t.enum_match_expr(t.var("size"), |m| {
                                m.arm("Small", |t| t.str("text-sm"));
                                m.arm("Large", |t| t.str("text-lg"));
                            }),
                        ]);
                        t.write_expr_escaped(t.tw_merge(classes));
                    });
                }),
            expect![[r#"
                -- before --
                enum Size {
                  Small,
                  Large,
                }
                view Test() {
                  let v0 = Size::Large in {
                    write_escaped(tw_merge((("px-4" + " ") + match v0 {
                      Size::Small => "text-sm",
                      Size::Large => "text-lg",
                    })))
                  }
                }

                -- after --
                enum Size {
                  Small,
                  Large,
                }
                view Test() {
                  let v0 = Size::Large in {
                    write_escaped("px-4 text-lg")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_join_with_enum_match_containing_join() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Size", ["Small", "Large"])
                .view_no_params("Test", |t| {
                    t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                        let classes = t.join(vec![
                            t.str("flex"),
                            t.enum_match_expr(t.var("size"), |m| {
                                m.arm("Small", |t| t.join(vec![t.str("p-2"), t.str("text-sm")]));
                                m.arm("Large", |t| t.join(vec![t.str("p-4"), t.str("text-lg")]));
                            }),
                        ]);
                        t.write_expr_escaped(t.tw_merge(classes));
                    });
                }),
            expect![[r#"
                -- before --
                enum Size {
                  Small,
                  Large,
                }
                view Test() {
                  let v0 = Size::Large in {
                    write_escaped(tw_merge((("flex" + " ") + match v0 {
                      Size::Small => (("p-2" + " ") + "text-sm"),
                      Size::Large => (("p-4" + " ") + "text-lg"),
                    })))
                  }
                }

                -- after --
                enum Size {
                  Small,
                  Large,
                }
                view Test() {
                  let v0 = Size::Large in {
                    write_escaped("flex p-4 text-lg")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_match_with_some() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.write_expr_escaped(t.option_match_expr(
                        t.var("opt"),
                        t.str("got some"),
                        t.str("got none"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped(match v0 {
                      Some(_) => "got some",
                      None => "got none",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped("got some")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_match_with_none() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.none("String"), |t| {
                    t.write_expr_escaped(t.option_match_expr(
                        t.var("opt"),
                        t.str("got some"),
                        t.str("got none"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::None in {
                    write_escaped(match v0 {
                      Some(_) => "got some",
                      None => "got none",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::None in {
                    write_escaped("got none")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_constant_through_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.some(t.str("hello")), |t| {
                    t.let_stmt("y", t.var("x"), |t| {
                        t.write_expr_escaped(t.option_match_expr(
                            t.var("y"),
                            t.str("got some"),
                            t.str("got none"),
                        ));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    let v1 = v0 in {
                      write_escaped(match v1 {
                        Some(_) => "got some",
                        None => "got none",
                      })
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    let v1 = Option[String]::Some("hello") in {
                      write_escaped("got some")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_value() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("opt"),
                        "inner",
                        |t| t.var("inner"),
                        t.str("default"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped(match v0 {
                      Some(v1) => v1,
                      None => "default",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped("hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_in_nested_expression() {
        // Test that the binding value propagates into nested expressions
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("opt"),
                        "inner",
                        |t| {
                            // Use the binding in an equality check and string concat
                            t.string_concat(t.var("inner"), t.str(" world"))
                        },
                        t.str("default"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped(match v0 {
                      Some(v1) => (v1 + " world"),
                      None => "default",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_in_equality() {
        // Test that the binding value propagates into equality comparisons
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.if_stmt(
                        t.option_match_expr_with_binding(
                            t.var("opt"),
                            "inner",
                            |t| t.eq(t.var("inner"), t.str("hello")),
                            t.bool(false),
                        ),
                        |t| {
                            t.write("matched hello");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match match v0 {
                      Some(v1) => (v1 == "hello"),
                      None => false,
                    } {
                      true => {
                        write("matched hello")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match true {
                      true => {
                        write("matched hello")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_through_nested_option_match() {
        // Test nested option matches using let statements to bind intermediate values
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("outer", t.some(t.some(t.str("nested"))), |t| {
                    // First match extracts inner_opt from outer
                    t.let_stmt(
                        "inner_result",
                        t.option_match_expr_with_binding(
                            t.var("outer"),
                            "inner_opt",
                            |t| t.var("inner_opt"),
                            t.none("String"),
                        ),
                        |t| {
                            // Second match extracts value from inner_result
                            t.write_expr_escaped(t.option_match_expr_with_binding(
                                t.var("inner_result"),
                                "value",
                                |t| t.var("value"),
                                t.str("inner none"),
                            ));
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("nested")) in {
                    let v2 = match v0 {
                      Some(v1) => v1,
                      None => Option[String]::None,
                    } in {
                      write_escaped(match v2 {
                        Some(v3) => v3,
                        None => "inner none",
                      })
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("nested")) in {
                    let v2 = Option[String]::Some("nested") in {
                      write_escaped("nested")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_option_match_with_inline_let() {
        // Test nested option match where inner match is inside a let expression in the Some arm
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("outer", t.some(t.some(t.str("nested"))), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("outer"),
                        "inner_opt",
                        |t| {
                            // let inner_opt_var = inner_opt in match inner_opt_var { ... }
                            t.let_expr("inner_opt_var", t.var("inner_opt"), |t| {
                                t.option_match_expr_with_binding(
                                    t.var("inner_opt_var"),
                                    "value",
                                    |t| t.var("value"),
                                    t.str("inner none"),
                                )
                            })
                        },
                        t.str("outer none"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("nested")) in {
                    write_escaped(match v0 {
                      Some(v1) => let v2 = v1 in match v2 {
                        Some(v3) => v3,
                        None => "inner none",
                      },
                      None => "outer none",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("nested")) in {
                    write_escaped("nested")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_value() {
        check(
            IrModuleBuilder::new()
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "msg",
                        t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                        |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("msg"), |m| {
                                m.arm_bound("Say", [("text", "t")], |t| t.var("t"));
                            }));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    write_escaped(match v0 {Msg::Say {text: v1} => v1})
                  }
                }

                -- after --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    write_escaped("hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_in_string_concat() {
        check(
            IrModuleBuilder::new()
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "msg",
                        t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("world"))]),
                        |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("msg"), |m| {
                                m.arm_bound("Say", [("text", "t")], |t| {
                                    t.string_concat(t.str("hello "), t.var("t"))
                                });
                            }));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "world"} in {
                    write_escaped(match v0 {
                      Msg::Say {text: v1} => ("hello " + v1),
                    })
                  }
                }

                -- after --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "world"} in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_in_equality() {
        check(
            IrModuleBuilder::new()
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "msg",
                        t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                        |t| {
                            t.if_stmt(
                                t.enum_match_expr(t.var("msg"), |m| {
                                    m.arm_bound("Say", [("text", "t")], |t| {
                                        t.eq(t.var("t"), t.str("hello"))
                                    });
                                }),
                                |t| {
                                    t.write("matched hello");
                                },
                            );
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    match match v0 {
                      Msg::Say {text: v1} => (v1 == "hello"),
                    } {
                      true => {
                        write("matched hello")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    match true {
                      true => {
                        write("matched hello")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_through_variable() {
        // Test that enum bindings work when the subject is propagated through a variable
        check(
            IrModuleBuilder::new()
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "x",
                        t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                        |t| {
                            t.let_stmt("y", t.var("x"), |t| {
                                t.write_expr_escaped(t.enum_match_expr(t.var("y"), |m| {
                                    m.arm_bound("Say", [("text", "t")], |t| t.var("t"));
                                }));
                            });
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    let v1 = v0 in {
                      write_escaped(match v1 {Msg::Say {text: v2} => v2})
                    }
                  }
                }

                -- after --
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Msg::Say {text: "hello"} in {
                    let v1 = Msg::Say {text: "hello"} in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_multiple_enum_bindings() {
        // Test multiple field bindings in a single variant
        check(
            IrModuleBuilder::new()
                .enum_(
                    "Pair",
                    [("Values", vec![("first", "String"), ("second", "String")])],
                )
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "pair",
                        t.enum_variant_with_fields(
                            "Pair",
                            "Values",
                            vec![("first", t.str("hello")), ("second", t.str("world"))],
                        ),
                        |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("pair"), |m| {
                                m.arm_bound("Values", [("first", "a"), ("second", "b")], |t| {
                                    t.string_concat(
                                        t.var("a"),
                                        t.string_concat(t.str(" "), t.var("b")),
                                    )
                                });
                            }));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Pair {
                  Values {first: String, second: String},
                }
                view Test() {
                  let v0 = Pair::Values {first: "hello", second: "world"} in {
                    write_escaped(match v0 {
                      Pair::Values {first: v1, second: v2} => (v1 + (" " + v2)),
                    })
                  }
                }

                -- after --
                enum Pair {
                  Values {first: String, second: String},
                }
                view Test() {
                  let v0 = Pair::Values {first: "hello", second: "world"} in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_match_selecting_correct_arm_with_bindings() {
        // Test that when we have multiple variants, we select the correct arm
        check(
            IrModuleBuilder::new()
                .enum_(
                    "Outcome",
                    [
                        ("Success", vec![("value", "String")]),
                        ("Failure", vec![("msg", "String")]),
                    ],
                )
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "result",
                        t.enum_variant_with_fields(
                            "Outcome",
                            "Success",
                            vec![("value", t.str("success"))],
                        ),
                        |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("result"), |m| {
                                m.arm_bound("Success", [("value", "v")], |t| t.var("v"));
                                m.arm_bound("Failure", [("msg", "m")], |t| {
                                    t.string_concat(t.str("error: "), t.var("m"))
                                });
                            }));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum Outcome {
                  Success {value: String},
                  Failure {msg: String},
                }
                view Test() {
                  let v0 = Outcome::Success {value: "success"} in {
                    write_escaped(match v0 {
                      Outcome::Success {value: v1} => v1,
                      Outcome::Failure {msg: v2} => ("error: " + v2),
                    })
                  }
                }

                -- after --
                enum Outcome {
                  Success {value: String},
                  Failure {msg: String},
                }
                view Test() {
                  let v0 = Outcome::Success {value: "success"} in {
                    write_escaped("success")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_not_confuse_same_named_fields_of_different_types_across_variants() {
        // Two variants of the same enum both name a field "f0", but with
        // different types (Bool vs String). The subject is known to be the
        // String variant, so the Bool-binding arm is never taken.
        check(
            IrModuleBuilder::new()
                .enum_(
                    "E",
                    [("V0", vec![("f0", "Bool")]), ("V1", vec![("f0", "String")])],
                )
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "subject",
                        t.enum_variant_with_fields("E", "V1", vec![("f0", t.str("hello"))]),
                        |t| {
                            t.write_expr_escaped(t.enum_match_expr(t.var("subject"), |m| {
                                m.arm_bound("V0", [("f0", "v")], |t| {
                                    t.bool_match_expr(t.var("v"), t.str("yes"), t.str("no"))
                                });
                                m.arm_bound("V1", [("f0", "w")], |t| t.var("w"));
                            }));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                enum E {
                  V0 {f0: Bool},
                  V1 {f0: String},
                }
                view Test() {
                  let v0 = E::V1 {f0: "hello"} in {
                    write_escaped(match v0 {
                      E::V0 {f0: v1} => match v1 {
                        true => "yes",
                        false => "no",
                      },
                      E::V1 {f0: v2} => v2,
                    })
                  }
                }

                -- after --
                enum E {
                  V0 {f0: Bool},
                  V1 {f0: String},
                }
                view Test() {
                  let v0 = E::V1 {f0: "hello"} in {
                    write_escaped("hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_through_match_arm_selection() {
        // When a match expression selects an arm that returns an enum with fields,
        // those field values should propagate to subsequent matches on the result.
        check(
            IrModuleBuilder::new()
                .enum_unit("Choice", ["A", "B"])
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt("choice", t.enum_variant("Choice", "A"), |t| {
                        t.let_stmt(
                            "x",
                            t.enum_match_expr(t.var("choice"), |m| {
                                m.arm("A", |t| {
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("hello"))],
                                    )
                                });
                                m.arm("B", |t| {
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("world"))],
                                    )
                                });
                            }),
                            |t| {
                                t.write_expr_escaped(t.enum_match_expr(t.var("x"), |m| {
                                    m.arm_bound("Say", [("text", "t")], |t| t.var("t"));
                                }));
                            },
                        );
                    });
                }),
            expect![[r#"
                -- before --
                enum Choice {
                  A,
                  B,
                }
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Choice::A in {
                    let v1 = match v0 {
                      Choice::A => Msg::Say {text: "hello"},
                      Choice::B => Msg::Say {text: "world"},
                    } in {
                      write_escaped(match v1 {Msg::Say {text: v2} => v2})
                    }
                  }
                }

                -- after --
                enum Choice {
                  A,
                  B,
                }
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Choice::A in {
                    let v1 = Msg::Say {text: "hello"} in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_bool_match_with_true() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.write_expr_escaped(t.bool_match_expr(
                        t.var("flag"),
                        t.str("yes"),
                        t.str("no"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    write_escaped(match v0 {true => "yes", false => "no"})
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    write_escaped("yes")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_bool_match_with_false() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(false), |t| {
                    t.write_expr_escaped(t.bool_match_expr(
                        t.var("flag"),
                        t.str("yes"),
                        t.str("no"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = false in {
                    write_escaped(match v0 {true => "yes", false => "no"})
                  }
                }

                -- after --
                view Test() {
                  let v0 = false in {
                    write_escaped("no")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_bool_constant_through_variables_in_match() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.bool(true), |t| {
                    t.let_stmt("y", t.var("x"), |t| {
                        t.write_expr_escaped(t.bool_match_expr(
                            t.var("y"),
                            t.str("yes"),
                            t.str("no"),
                        ));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    let v1 = v0 in {
                      write_escaped(match v1 {true => "yes", false => "no"})
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    let v1 = true in {
                      write_escaped("yes")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_bool_match_with_negated_subject() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("flag", t.not(t.bool(false)), |t| {
                    t.write_expr_escaped(t.bool_match_expr(
                        t.var("flag"),
                        t.str("was true"),
                        t.str("was false"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (!false) in {
                    write_escaped(match v0 {
                      true => "was true",
                      false => "was false",
                    })
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    write_escaped("was true")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn enum_binding_through_let_in_match_arm() {
        // Test that enum_field propagates through let expressions in match arm bodies.
        // When a match arm body is `let x = Enum(...) in x`, the field information should
        // flow from the enum literal -> let body -> let expr -> match expr.
        check(
            IrModuleBuilder::new()
                .enum_unit("Choice", ["A", "B"])
                .enum_("Msg", [("Say", vec![("text", "String")])])
                .view_no_params("Test", |t| {
                    t.let_stmt("choice", t.enum_variant("Choice", "A"), |t| {
                        t.let_stmt(
                            "y",
                            t.enum_match_expr(t.var("choice"), |m| {
                                // Arm body is a let expression, not a direct enum literal
                                m.arm("A", |t| {
                                    t.let_expr(
                                        "x",
                                        t.enum_variant_with_fields(
                                            "Msg",
                                            "Say",
                                            vec![("text", t.str("hello"))],
                                        ),
                                        |t| t.var("x"),
                                    )
                                });
                                m.arm("B", |t| {
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("world"))],
                                    )
                                });
                            }),
                            |t| {
                                t.write_expr_escaped(t.enum_match_expr(t.var("y"), |m| {
                                    m.arm_bound("Say", [("text", "txt")], |t| t.var("txt"));
                                }));
                            },
                        );
                    });
                }),
            expect![[r#"
                -- before --
                enum Choice {
                  A,
                  B,
                }
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Choice::A in {
                    let v2 = match v0 {
                      Choice::A => let v1 = Msg::Say {text: "hello"} in v1,
                      Choice::B => Msg::Say {text: "world"},
                    } in {
                      write_escaped(match v2 {Msg::Say {text: v3} => v3})
                    }
                  }
                }

                -- after --
                enum Choice {
                  A,
                  B,
                }
                enum Msg {
                  Say {text: String},
                }
                view Test() {
                  let v0 = Choice::A in {
                    let v2 = Msg::Say {text: "hello"} in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_logical_or_with_literals() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.or(t.bool(false), t.bool(false)), |t| {
                    t.write("false || false");
                });
                t.if_stmt(t.or(t.bool(false), t.bool(true)), |t| {
                    t.write("false || true");
                });
                t.if_stmt(t.or(t.bool(true), t.bool(false)), |t| {
                    t.write("true || false");
                });
                t.if_stmt(t.or(t.bool(true), t.bool(true)), |t| {
                    t.write("true || true");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (false || false) {
                    true => {
                      write("false || false")
                    }
                    false => {
                    }
                  }
                  match (false || true) {
                    true => {
                      write("false || true")
                    }
                    false => {
                    }
                  }
                  match (true || false) {
                    true => {
                      write("true || false")
                    }
                    false => {
                    }
                  }
                  match (true || true) {
                    true => {
                      write("true || true")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match false {
                    true => {
                      write("false || false")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("false || true")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("true || false")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("true || true")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_logical_and_with_literals() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.and(t.bool(false), t.bool(false)), |t| {
                    t.write("false && false");
                });
                t.if_stmt(t.and(t.bool(false), t.bool(true)), |t| {
                    t.write("false && true");
                });
                t.if_stmt(t.and(t.bool(true), t.bool(false)), |t| {
                    t.write("true && false");
                });
                t.if_stmt(t.and(t.bool(true), t.bool(true)), |t| {
                    t.write("true && true");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (false && false) {
                    true => {
                      write("false && false")
                    }
                    false => {
                    }
                  }
                  match (false && true) {
                    true => {
                      write("false && true")
                    }
                    false => {
                    }
                  }
                  match (true && false) {
                    true => {
                      write("true && false")
                    }
                    false => {
                    }
                  }
                  match (true && true) {
                    true => {
                      write("true && true")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match false {
                    true => {
                      write("false && false")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("false && true")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("true && false")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("true && true")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_logical_operations_with_propagated_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("a", t.bool(true), |t| {
                    t.let_stmt("b", t.bool(false), |t| {
                        t.if_stmt(t.or(t.var("a"), t.var("b")), |t| {
                            t.write("a || b");
                        });
                        t.if_stmt(t.and(t.var("a"), t.var("b")), |t| {
                            t.write("a && b");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    let v1 = false in {
                      match (v0 || v1) {
                        true => {
                          write("a || b")
                        }
                        false => {
                        }
                      }
                      match (v0 && v1) {
                        true => {
                          write("a && b")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    let v1 = false in {
                      match true {
                        true => {
                          write("a || b")
                        }
                        false => {
                        }
                      }
                      match false {
                        true => {
                          write("a && b")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_logical_operations() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                // (true && false) || (false || true) => false || true => true
                t.if_stmt(
                    t.or(
                        t.and(t.bool(true), t.bool(false)),
                        t.or(t.bool(false), t.bool(true)),
                    ),
                    |t| {
                        t.write("complex expression");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match ((true && false) || (false || true)) {
                    true => {
                      write("complex expression")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("complex expression")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_logical_operations_with_negation() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                // !false && !false => true && true => true
                t.if_stmt(t.and(t.not(t.bool(false)), t.not(t.bool(false))), |t| {
                    t.write("both negated");
                });
                // !true || false => false || false => false
                t.if_stmt(t.or(t.not(t.bool(true)), t.bool(false)), |t| {
                    t.write("should not appear");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match ((!false) && (!false)) {
                    true => {
                      write("both negated")
                    }
                    false => {
                    }
                  }
                  match ((!true) || false) {
                    true => {
                      write("should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("both negated")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_component_invocation_args() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.invoke_component(
                    "Foo",
                    vec![("x", t.not(t.bool(false))), ("y", t.str("hello"))],
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  call Foo(x = (!false), y = "hello")
                }

                -- after --
                view Test() {
                  call Foo(x = true, y = "hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_component_invocation_args_with_propagated_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("val", t.not(t.bool(true)), |t| {
                    t.invoke_component("Foo", vec![("enabled", t.var("val"))]);
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (!true) in {
                    call Foo(enabled = v0)
                  }
                }

                -- after --
                view Test() {
                  let v0 = false in {
                    call Foo(enabled = false)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_int_arithmetic() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt(
                    "x",
                    t.add(t.mul(t.int(2), t.int(3)), t.sub(t.int(10), t.int(4))),
                    |_| {},
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = ((2 * 3) + (10 - 4)) in {}
                }

                -- after --
                view Test() {
                  let v0 = 12 in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_wrap_int_arithmetic_at_i32_boundaries() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("a", t.add(t.int(i32::MAX), t.int(1)), |_| {});
                t.let_stmt("b", t.sub(t.int(i32::MIN), t.int(1)), |_| {});
                t.let_stmt("c", t.mul(t.int(i32::MAX), t.int(2)), |_| {});
                t.let_stmt("d", t.neg(t.int(i32::MIN)), |_| {});
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (2147483647 + 1) in {}
                  let v1 = (-2147483648 - 1) in {}
                  let v2 = (2147483647 * 2) in {}
                  let v3 = (--2147483648) in {}
                }

                -- after --
                view Test() {
                  let v0 = -2147483648 in {}
                  let v1 = 2147483647 in {}
                  let v2 = -2 in {}
                  let v3 = -2147483648 in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_float_arithmetic() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt(
                    "x",
                    t.add(t.mul(t.float(1.5), t.float(2.0)), t.neg(t.float(0.5))),
                    |_| {},
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = ((1.5 * 2) + (-0.5)) in {}
                }

                -- after --
                view Test() {
                  let v0 = 2.5 in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_nan_and_infinity_through_float_arithmetic() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("a", t.add(t.float(f64::NAN), t.float(1.0)), |_| {});
                t.let_stmt("b", t.add(t.float(f64::INFINITY), t.float(1.0)), |_| {});
                t.let_stmt(
                    "c",
                    t.add(t.float(f64::INFINITY), t.float(f64::NEG_INFINITY)),
                    |_| {},
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (NaN + 1) in {}
                  let v1 = (inf + 1) in {}
                  let v2 = (inf + -inf) in {}
                }

                -- after --
                view Test() {
                  let v0 = NaN in {}
                  let v1 = inf in {}
                  let v2 = NaN in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_float_equality_with_ieee_semantics() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.eq(t.float(f64::NAN), t.float(f64::NAN)), |t| {
                    t.write("false");
                });
                t.if_stmt(t.eq(t.float(0.0), t.float(-0.0)), |t| {
                    t.write("true");
                });
                t.if_stmt(t.eq(t.float(1.5), t.float(1.5)), |t| {
                    t.write("true");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match (NaN == NaN) {
                    true => {
                      write("false")
                    }
                    false => {
                    }
                  }
                  match (0 == -0) {
                    true => {
                      write("true")
                    }
                    false => {
                    }
                  }
                  match (1.5 == 1.5) {
                    true => {
                      write("true")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match false {
                    true => {
                      write("false")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("true")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("true")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_int_to_string() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write_expr_escaped(t.int_to_string(t.add(t.int(40), t.int(2))));
                t.write_expr_escaped(t.int_to_string(t.int(-7)));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped((40 + 2).to_string())
                  write_escaped(-7.to_string())
                }

                -- after --
                view Test() {
                  write_escaped("42")
                  write_escaped("-7")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_numeric_conversions() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("a", t.int_to_float(t.int(3)), |_| {});
                t.let_stmt("b", t.float_to_int(t.float(3.7)), |_| {});
                t.let_stmt("c", t.float_to_int(t.float(-3.7)), |_| {});
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = 3.to_float() in {}
                  let v1 = 3.7.to_int() in {}
                  let v2 = -3.7.to_int() in {}
                }

                -- after --
                view Test() {
                  let v0 = 3 in {}
                  let v1 = 3 in {}
                  let v2 = -3 in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_saturate_float_to_int_conversion() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("a", t.float_to_int(t.float(3000000000.0)), |_| {});
                t.let_stmt("b", t.float_to_int(t.float(-3000000000.0)), |_| {});
                t.let_stmt("c", t.float_to_int(t.float(f64::NAN)), |_| {});
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = 3000000000.to_int() in {}
                  let v1 = -3000000000.to_int() in {}
                  let v2 = NaN.to_int() in {}
                }

                -- after --
                view Test() {
                  let v0 = 2147483647 in {}
                  let v1 = -2147483648 in {}
                  let v2 = 0 in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_numeric_constants_through_variables() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_stmt("x", t.add(t.int(1), t.int(2)), |t| {
                    t.let_stmt("y", t.mul(t.var("x"), t.var("x")), |t| {
                        t.write_expr_escaped(t.int_to_string(t.var("y")));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = (1 + 2) in {
                    let v1 = (v0 * v0) in {
                      write_escaped(v1.to_string())
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = 3 in {
                    let v1 = 9 in {
                      write_escaped("9")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_int_match_arm_selection() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write_expr_escaped(t.bool_match_expr(
                    t.lt(t.int(1), t.int(2)),
                    t.str("less"),
                    t.str("not less"),
                ));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped(match (1 < 2) {
                    true => "less",
                    false => "not less",
                  })
                }

                -- after --
                view Test() {
                  write_escaped("less")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_is_some() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.option_is_some(t.some(t.str("x"))), |t| {
                    t.write("some is some");
                });
                t.if_stmt(t.option_is_some(t.none("String")), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x").is_some() {
                    true => {
                      write("some is some")
                    }
                    false => {
                    }
                  }
                  match Option[String]::None.is_some() {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("some is some")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_is_none() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.option_is_none(t.none("String")), |t| {
                    t.write("none is none");
                });
                t.if_stmt(t.option_is_none(t.some(t.str("x"))), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::None.is_none() {
                    true => {
                      write("none is none")
                    }
                    false => {
                    }
                  }
                  match Option[String]::Some("x").is_none() {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("none is none")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_string_is_empty() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(t.string_is_empty(t.str("")), |t| {
                    t.write("Empty");
                });
                t.if_stmt(t.string_is_empty(t.str("hello")), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match "".is_empty() {
                    true => {
                      write("Empty")
                    }
                    false => {
                    }
                  }
                  match "hello".is_empty() {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Empty")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_array_is_empty() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.if_stmt(
                    t.array_is_empty(t.array_typed(Arc::new(Type::String), vec![])),
                    |t| {
                        t.write("Empty");
                    },
                );
                t.if_stmt(t.array_is_empty(t.array(vec![t.int(1)])), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match [].is_empty() {
                    true => {
                      write("Empty")
                    }
                    false => {
                    }
                  }
                  match [1].is_empty() {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Empty")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Should not appear")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_array_length() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write_expr_escaped(t.int_to_string(t.array_length(t.array(vec![
                    t.str("a"),
                    t.str("b"),
                    t.str("c"),
                ]))));
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped(["a", "b", "c"].len().to_string())
                }

                -- after --
                view Test() {
                  write_escaped("3")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_array_length_with_non_constant_elements() {
        check(
            IrModuleBuilder::new().view("Test", [("x", "Int")], |t| {
                t.write_expr_escaped(t.int_to_string(t.array_length(t.array(vec![t.var("x")]))));
            }),
            expect![[r#"
                -- before --
                view Test(x@v0: Int) {
                  write_escaped([v0].len().to_string())
                }

                -- after --
                view Test(x@v0: Int) {
                  write_escaped("1")
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_field_access_on_record_literal() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String")])
                .view_no_params("Test", |t| {
                    t.write_expr_escaped(
                        t.field_access(t.record("User", vec![("name", t.str("John"))]), "name"),
                    );
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                }
                view Test() {
                  write_escaped(User {name: "John"}.name)
                }

                -- after --
                record User {
                  name: String,
                }
                view Test() {
                  write_escaped("John")
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_record_field_through_variables() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String")])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "user",
                        t.record("User", vec![("name", t.str("John"))]),
                        |t| {
                            t.write_expr_escaped(t.field_access(t.var("user"), "name"));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    write_escaped(v0.name)
                  }
                }

                -- after --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    write_escaped("John")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_field_access_with_non_constant_sibling_field() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String"), ("email", "String")])
                .view("Test", [("email", "String")], |t| {
                    t.let_stmt(
                        "user",
                        t.record(
                            "User",
                            vec![("name", t.str("John")), ("email", t.var("email"))],
                        ),
                        |t| {
                            t.write_expr_escaped(t.field_access(t.var("user"), "name"));
                            t.write_expr_escaped(t.field_access(t.var("user"), "email"));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                  email: String,
                }
                view Test(email@v0: String) {
                  let v1 = User {name: "John", email: v0} in {
                    write_escaped(v1.name)
                    write_escaped(v1.email)
                  }
                }

                -- after --
                record User {
                  name: String,
                  email: String,
                }
                view Test(email@v0: String) {
                  let v1 = User {name: "John", email: v0} in {
                    write_escaped("John")
                    write_escaped(v1.email)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_nested_record_field_access() {
        check(
            IrModuleBuilder::new()
                .record("Address", [("city", "String")])
                .record("User", [("address", "Address")])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "user",
                        t.record(
                            "User",
                            vec![(
                                "address",
                                t.record("Address", vec![("city", t.str("Paris"))]),
                            )],
                        ),
                        |t| {
                            t.write_expr_escaped(
                                t.field_access(t.field_access(t.var("user"), "address"), "city"),
                            );
                        },
                    );
                }),
            expect![[r#"
                -- before --
                record Address {
                  city: String,
                }
                record User {
                  address: Address,
                }
                view Test() {
                  let v0 = User {address: Address {city: "Paris"}} in {
                    write_escaped(v0.address.city)
                  }
                }

                -- after --
                record Address {
                  city: String,
                }
                record User {
                  address: Address,
                }
                view Test() {
                  let v0 = User {address: Address {city: "Paris"}} in {
                    write_escaped("Paris")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_record_constant_through_variables() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String")])
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.record("User", vec![("name", t.str("John"))]), |t| {
                        t.let_stmt("y", t.var("x"), |t| {
                            t.write_expr_escaped(t.field_access(t.var("y"), "name"));
                        });
                    });
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    let v1 = v0 in {
                      write_escaped(v1.name)
                    }
                  }
                }

                -- after --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    let v1 = User {name: "John"} in {
                      write_escaped("John")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_field_access_in_string_concat() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String")])
                .view_no_params("Test", |t| {
                    t.let_stmt(
                        "user",
                        t.record("User", vec![("name", t.str("John"))]),
                        |t| {
                            t.write_expr_escaped(t.string_concat(
                                t.str("Hello "),
                                t.field_access(t.var("user"), "name"),
                            ));
                        },
                    );
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    write_escaped(("Hello " + v0.name))
                  }
                }

                -- after --
                record User {
                  name: String,
                }
                view Test() {
                  let v0 = User {name: "John"} in {
                    write_escaped("Hello John")
                  }
                }
            "#]],
        );
    }
}
