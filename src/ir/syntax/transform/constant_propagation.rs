use super::Pass;
use crate::document::document::CheapString;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::ir::{
    IrExpr,
    ast::ExprId,
    ast::{IrComponentDeclaration, IrStatement},
};
use datafrog::{Iteration, Relation};
use std::collections::HashMap;
use tailwind_merge::tw_merge;

/// Constant values that can be tracked during constant folding
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum Const {
    Bool(bool),
    String(String),
    Enum {
        enum_name: String,
        variant_name: String,
        /// Field expression IDs for reconstructing the enum literal.
        /// Empty for unit variants.
        fields: Vec<(String, ExprId)>,
    },
    Option(Option<ExprId>),
}

impl Const {
    /// Convert a Const to an IrExpr. Returns None if conversion is not possible
    /// (e.g., inner value of an Option is not in const_map).
    fn to_expr(
        &self,
        id: ExprId,
        kind: &Type,
        const_map: &HashMap<ExprId, Const>,
    ) -> Option<IrExpr> {
        Some(match self {
            Const::Bool(b) => IrExpr::BooleanLiteral { value: *b, id },
            Const::String(s) => IrExpr::StringLiteral {
                value: CheapString::new(s.clone()),
                id,
            },
            Const::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                // Reconstruct field expressions from const_map
                let Type::Enum { variants, .. } = kind else {
                    return None;
                };
                let variant_fields = variants
                    .iter()
                    .find(|(v, _)| v.as_str() == variant_name)
                    .map(|(_, f)| f)?;

                let reconstructed_fields: Option<Vec<_>> = fields
                    .iter()
                    .map(|(field_name, field_id)| {
                        let field_type = variant_fields
                            .iter()
                            .find(|(f, _)| f.as_str() == field_name)
                            .map(|(_, t)| t)?;
                        let field_const = const_map.get(field_id)?;
                        let field_expr = field_const.to_expr(*field_id, field_type, const_map)?;
                        Some((FieldName::new(field_name).ok()?, field_expr))
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
                        let inner_const = const_map.get(id)?;
                        Some(Box::new(inner_const.to_expr(*id, kind, const_map)?))
                    }
                };
                IrExpr::OptionLiteral {
                    value: inner_expr,
                    kind: kind.clone(),
                    id,
                }
            }
        })
    }
}

/// A datafrog-based constant propagation pass that tracks and propagates
/// constant values.
///
/// This pass assumes that the input is in SSA form.
pub struct ConstantPropagationPass;

impl Pass for ConstantPropagationPass {
    fn run(entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        let mut iteration = Iteration::new();

        let mut initial_constants = Vec::new();
        let mut not_relations = Vec::new();
        let mut eq_left_relations = Vec::new();
        let mut eq_right_relations = Vec::new();
        let mut concat_left_relations = Vec::new();
        let mut concat_right_relations = Vec::new();
        let mut merge_left_relations = Vec::new();
        let mut merge_right_relations = Vec::new();
        let mut var_references = Vec::new();
        let mut enum_match_subjects = Vec::new();
        let mut match_arms_relations = Vec::new();
        let mut option_match_subjects = Vec::new();
        let mut option_arms_relations: Vec<((ExprId, bool), ExprId)> = Vec::new();
        let mut option_inner_ids: Vec<(ExprId, ExprId)> = Vec::new();
        let mut bool_match_subjects: Vec<(ExprId, ExprId)> = Vec::new();
        let mut bool_arms_relations: Vec<((ExprId, bool), ExprId)> = Vec::new();
        let mut option_binding_uses: Vec<(ExprId, ExprId)> = Vec::new();
        let mut let_expr_body_relations: Vec<(ExprId, ExprId)> = Vec::new();
        // Enum field values: (enum_expr_id => (field_name, field_expr_id))
        let mut enum_field_ids: Vec<(ExprId, (String, ExprId))> = Vec::new();
        // Enum binding uses: ((subject_def_id, field_name) => binding_var_expr_id)
        let mut enum_binding_uses: Vec<((ExprId, String), ExprId)> = Vec::new();

        // SSA form guarantees unique variable names, so we can collect all bindings
        // into a single HashMap without worrying about shadowing or scoping.
        let mut var_bindings: HashMap<String, ExprId> = HashMap::new();

        // Option match bindings map binding name -> subject's defining expression id.
        // These are handled separately because the binding refers to the inner value,
        // not the Option itself.
        let mut option_bindings: HashMap<String, ExprId> = HashMap::new();

        // Enum match bindings map binding name -> (subject's defining expression id, field name).
        // These are handled separately because the binding refers to a field value,
        // not the enum itself.
        let mut enum_bindings: HashMap<String, (ExprId, String)> = HashMap::new();

        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| {
                // Collect statement-level bindings
                match s {
                    IrStatement::Let { var, value, .. } => {
                        var_bindings.insert(var.to_string(), value.id());
                    }
                    IrStatement::For { var, array, .. } => {
                        var_bindings.insert(var.to_string(), array.id());
                    }
                    _ => {}
                }

                let Some(primary_expr) = s.expr() else {
                    return;
                };

                primary_expr.traverse(&mut |expr| {
                    match expr {
                        IrExpr::BooleanLiteral { value, .. } => {
                            initial_constants.push((expr.id(), Const::Bool(*value)));
                        }
                        IrExpr::StringLiteral { value, .. } => {
                            initial_constants.push((expr.id(), Const::String(value.to_string())));
                        }
                        IrExpr::BooleanNegation { operand, .. } => {
                            not_relations.push((operand.id(), expr.id()));
                        }
                        IrExpr::Equals { left, right, .. } => {
                            eq_left_relations.push((left.id(), expr.id()));
                            eq_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::StringConcat { left, right, .. } => {
                            concat_left_relations.push((left.id(), expr.id()));
                            concat_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::EnumLiteral {
                            enum_name,
                            variant_name,
                            fields,
                            ..
                        } => {
                            // Track enum as constant (variant info for match selection + field IDs for reconstruction)
                            let field_ids: Vec<(String, ExprId)> = fields
                                .iter()
                                .map(|(name, expr)| (name.as_str().to_string(), expr.id()))
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
                                enum_field_ids.push((expr.id(), (field_name, field_id)));
                            }
                        }
                        IrExpr::Match { match_, .. } => match match_ {
                            Match::Enum { subject, arms } => {
                                if let Some(def_expr_id) = var_bindings.get(subject.0.as_str()) {
                                    enum_match_subjects.push((*def_expr_id, expr.id()));

                                    // Record bindings from match arms
                                    for arm in arms {
                                        for (field_name, binding_name) in &arm.bindings {
                                            enum_bindings.insert(
                                                binding_name.as_str().to_string(),
                                                (*def_expr_id, field_name.as_str().to_string()),
                                            );
                                        }
                                    }
                                }
                                for arm in arms {
                                    match &arm.pattern {
                                        EnumPattern::Variant {
                                            enum_name,
                                            variant_name,
                                        } => {
                                            match_arms_relations.push((
                                                (
                                                    expr.id(),
                                                    enum_name.clone(),
                                                    variant_name.clone(),
                                                ),
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
                                if let Some(def_expr_id) = var_bindings.get(subject.0.as_str()) {
                                    bool_match_subjects.push((*def_expr_id, expr.id()));
                                    // Key arms by (match_id, is_true) for direct joining
                                    bool_arms_relations.push(((expr.id(), true), true_body.id()));
                                    bool_arms_relations.push(((expr.id(), false), false_body.id()));
                                }
                            }
                            Match::Option {
                                subject,
                                some_arm_binding,
                                some_arm_body,
                                none_arm_body,
                            } => {
                                if let Some(def_expr_id) = var_bindings.get(subject.0.as_str()) {
                                    option_match_subjects.push((*def_expr_id, expr.id()));
                                    // Key arms by (match_id, is_some) for direct joining
                                    option_arms_relations
                                        .push(((expr.id(), true), some_arm_body.id()));
                                    option_arms_relations
                                        .push(((expr.id(), false), none_arm_body.id()));

                                    // Record the binding - uses will be found when we visit Var expressions
                                    if let Some(binding) = some_arm_binding {
                                        option_bindings
                                            .insert(binding.as_str().to_string(), *def_expr_id);
                                    }
                                }
                            }
                        },
                        IrExpr::Var { value: name, .. } => {
                            if let Some(def_expr_id) = var_bindings.get(name.as_str()) {
                                var_references.push((*def_expr_id, expr.id()));
                            } else if let Some(subject_def_id) = option_bindings.get(name.as_str())
                            {
                                option_binding_uses.push((*subject_def_id, expr.id()));
                            } else if let Some((subject_def_id, field_name)) =
                                enum_bindings.get(name.as_str())
                            {
                                enum_binding_uses
                                    .push(((*subject_def_id, field_name.clone()), expr.id()));
                            }
                        }
                        IrExpr::MergeClasses { left, right, .. } => {
                            merge_left_relations.push((left.id(), expr.id()));
                            merge_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::OptionLiteral { value, .. } => {
                            // Track the full Option constant with inner expression id
                            let inner_id = value.as_ref().map(|inner| inner.id());
                            initial_constants.push((expr.id(), Const::Option(inner_id)));
                            // Track inner id for binding propagation
                            if let Some(inner) = value {
                                option_inner_ids.push((expr.id(), inner.id()));
                            }
                        }
                        IrExpr::Let {
                            var, value, body, ..
                        } => {
                            var_bindings.insert(var.as_str().to_string(), value.id());
                            // Keyed by body_id for joining with const_value
                            let_expr_body_relations.push((body.id(), expr.id()));
                        }
                        _ => {}
                    }
                });
            });
        }

        // Constant values of expressions: (expr_id => const_value)
        let const_value = iteration.variable::<(ExprId, Const)>("const_value");
        const_value.extend(initial_constants);

        // Values of left operands in equality expressions: (eq_expr_id => left_value)
        let eq_left_value = iteration.variable::<(ExprId, Const)>("eq_left_value");

        // Values of right operands in equality expressions: (eq_expr_id => right_value)
        let eq_right_value = iteration.variable::<(ExprId, Const)>("eq_right_value");

        // Values of left operands in string concat expressions: (concat_expr_id => left_value)
        let concat_left_value = iteration.variable::<(ExprId, Const)>("concat_left_value");

        // Values of right operands in string concat expressions: (concat_expr_id => right_value)
        let concat_right_value = iteration.variable::<(ExprId, Const)>("concat_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(not_relations);

        // Equals operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(eq_left_relations);

        // Equals operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(eq_right_relations);

        // String concat operations - left operand: (left_operand_id => expr_id)
        let concat_left = Relation::from_iter(concat_left_relations);

        // String concat operations - right operand: (right_operand_id => expr_id)
        let concat_right = Relation::from_iter(concat_right_relations);

        // MergeClasses operations - left operand: (left_operand_id => expr_id)
        let merge_left = Relation::from_iter(merge_left_relations);

        // MergeClasses operations - right operand: (right_operand_id => expr_id)
        let merge_right = Relation::from_iter(merge_right_relations);

        // Values of left operands in merge expressions: (merge_expr_id => left_value)
        let merge_left_value = iteration.variable::<(ExprId, Const)>("merge_left_value");

        // Values of right operands in merge expressions: (merge_expr_id => right_value)
        let merge_right_value = iteration.variable::<(ExprId, Const)>("merge_right_value");

        // Enum match subject expressions: (subject_id => match_id)
        let enum_match_subject = Relation::from_iter(enum_match_subjects);

        // Enum match arms: ((match_id, enum_name, variant_name) => arm_body_id)
        let match_arm_rel = Relation::from_iter(match_arms_relations);

        // Enum matches with known subjects: ((match_id, enum_name, variant_name) => match_id)
        let match_with_enum =
            iteration.variable::<((ExprId, String, String), ExprId)>("match_with_enum");

        // Selected arm bodies for all match types: (arm_body_id => match_id)
        let selected_arm = iteration.variable::<(ExprId, ExprId)>("selected_arm");

        // Option match subject expressions: (subject_id => match_id)
        let option_match_subject = Relation::from_iter(option_match_subjects);

        // Option match arms: ((match_id, is_some) => body_id)
        let option_arms = Relation::from_iter(option_arms_relations);

        // Option matches with known subjects: ((match_id, is_some) => match_id)
        let option_match_with_const =
            iteration.variable::<((ExprId, bool), ExprId)>("option_match_with_const");

        // Bool match subject expressions: (subject_id => match_id)
        let bool_match_subject = Relation::from_iter(bool_match_subjects);

        // Bool match arms: ((match_id, is_true) => body_id)
        let bool_arms = Relation::from_iter(bool_arms_relations);

        // Bool matches with known subjects: ((match_id, is_true) => match_id)
        let bool_match_with_const =
            iteration.variable::<((ExprId, bool), ExprId)>("bool_match_with_const");

        // Option inner expression ids: (option_expr_id => inner_expr_id)
        let option_inner = iteration.variable::<(ExprId, ExprId)>("option_inner");
        option_inner.extend(option_inner_ids);

        // Option binding uses: (subject_def_expr_id => binding_var_expr_id)
        let option_binding_use = Relation::from_iter(option_binding_uses);

        // Enum field expression ids: (enum_expr_id => (field_name, field_expr_id))
        // Used for propagating through variable bindings
        let enum_field = iteration.variable::<(ExprId, (String, ExprId))>("enum_field");
        enum_field.extend(enum_field_ids.clone());

        // Enum field keyed by (expr_id, field_name): ((enum_expr_id, field_name) => field_expr_id)
        // Used for joining with binding uses
        let enum_field_keyed = iteration.variable::<((ExprId, String), ExprId)>("enum_field_keyed");
        enum_field_keyed.extend(
            enum_field_ids
                .into_iter()
                .map(|(expr_id, (field_name, field_id))| ((expr_id, field_name), field_id)),
        );

        // Enum binding uses: ((subject_def_expr_id, field_name) => binding_var_expr_id)
        let enum_binding_use = Relation::from_iter(enum_binding_uses);

        // Value equivalence: `propagate_to(x, y)` means "y computes the same value as x".
        let propagate_to = iteration.variable::<(ExprId, ExprId)>("propagate_to");
        propagate_to.extend(var_references);
        propagate_to.extend(let_expr_body_relations);

        while iteration.changed() {
            // Fold boolean negation
            // const_value(e, !b) :- const_value(op, Bool(b)), not_rel(op, e).
            const_value.from_join(
                &const_value,
                &not_rel,
                |_: &ExprId, const_val: &Const, expr_id: &ExprId| match const_val {
                    Const::Bool(b) => (*expr_id, Const::Bool(!b)),
                    _ => unreachable!("Boolean negation can only have boolean operands"),
                },
            );

            // Collect left operand values for equality expressions
            // eq_left_value(eq, lv) :- const_value(l, lv), eq_left(l, eq).
            eq_left_value.from_join(
                &const_value,
                &eq_left,
                |_: &ExprId, const_val: &Const, eq_expr: &ExprId| (*eq_expr, const_val.clone()),
            );

            // Collect right operand values for equality expressions
            // eq_right_value(eq, rv) :- const_value(r, rv), eq_right(r, eq).
            eq_right_value.from_join(
                &const_value,
                &eq_right,
                |_: &ExprId, const_val: &Const, eq_expr: &ExprId| (*eq_expr, const_val.clone()),
            );

            // Fold equality comparison
            // const_value(eq, Bool(lv == rv)) :- eq_left_value(eq, lv), eq_right_value(eq, rv).
            const_value.from_join(
                &eq_left_value,
                &eq_right_value,
                |eq_expr: &ExprId, left_val: &Const, right_val: &Const| {
                    (*eq_expr, Const::Bool(left_val == right_val))
                },
            );

            // Collect left operand values for string concat expressions
            // concat_left_value(c, lv) :- const_value(l, lv), concat_left(l, c).
            concat_left_value.from_join(
                &const_value,
                &concat_left,
                |_: &ExprId, const_val: &Const, concat_expr: &ExprId| {
                    (*concat_expr, const_val.clone())
                },
            );

            // Collect right operand values for string concat expressions
            // concat_right_value(c, rv) :- const_value(r, rv), concat_right(r, c).
            concat_right_value.from_join(
                &const_value,
                &concat_right,
                |_: &ExprId, const_val: &Const, concat_expr: &ExprId| {
                    (*concat_expr, const_val.clone())
                },
            );

            // Fold string concatenation
            // const_value(c, String(lv + rv)) :- concat_left_value(c, lv), concat_right_value(c, rv).
            const_value.from_join(
                &concat_left_value,
                &concat_right_value,
                |concat_expr: &ExprId, left_val: &Const, right_val: &Const| match (
                    left_val, right_val,
                ) {
                    (Const::String(l), Const::String(r)) => {
                        let mut result = l.clone();
                        result.push_str(r);
                        (*concat_expr, Const::String(result))
                    }
                    _ => unreachable!("StringConcat can only have string operands"),
                },
            );

            // Collect left operand values for merge expressions
            // merge_left_value(m, lv) :- const_value(l, lv), merge_left(l, m).
            merge_left_value.from_join(
                &const_value,
                &merge_left,
                |_: &ExprId, const_val: &Const, merge_expr: &ExprId| {
                    (*merge_expr, const_val.clone())
                },
            );

            // Collect right operand values for merge expressions
            // merge_right_value(m, rv) :- const_value(r, rv), merge_right(r, m).
            merge_right_value.from_join(
                &const_value,
                &merge_right,
                |_: &ExprId, const_val: &Const, merge_expr: &ExprId| {
                    (*merge_expr, const_val.clone())
                },
            );

            // Fold tailwind class merge
            // const_value(m, String(tw_merge(lv, rv))) :- merge_left_value(m, lv), merge_right_value(m, rv).
            const_value.from_join(
                &merge_left_value,
                &merge_right_value,
                |merge_expr: &ExprId, left_val: &Const, right_val: &Const| match (
                    left_val, right_val,
                ) {
                    (Const::String(l), Const::String(r)) => {
                        let combined = format!("{} {}", l, r);
                        let merged = tw_merge(&combined);
                        (*merge_expr, Const::String(merged))
                    }
                    _ => unreachable!("MergeClasses can only have string operands"),
                },
            );

            const_value.from_join(
                &const_value,
                &propagate_to,
                |_source: &ExprId, val: &Const, target: &ExprId| (*target, val.clone()),
            );

            // Option bindings: `match opt { Some(x) => ... }` makes x equivalent to opt's inner value.
            propagate_to.from_join(
                &option_inner,
                &option_binding_use,
                |_subject: &ExprId, inner_id: &ExprId, binding_use_id: &ExprId| {
                    (*inner_id, *binding_use_id)
                },
            );

            option_inner.from_join(
                &option_inner,
                &propagate_to,
                |_source: &ExprId, inner_id: &ExprId, target: &ExprId| (*target, *inner_id),
            );

            // Enum bindings: `match e { V(f: x) => ... }` makes x equivalent to e's field value.
            propagate_to.from_join(
                &enum_field_keyed,
                &enum_binding_use,
                |_key: &(ExprId, String), field_id: &ExprId, binding_use_id: &ExprId| {
                    (*field_id, *binding_use_id)
                },
            );

            enum_field.from_join(
                &enum_field,
                &propagate_to,
                |_source: &ExprId, (field_name, field_id): &(String, ExprId), target: &ExprId| {
                    (*target, (field_name.clone(), *field_id))
                },
            );

            // Keep enum_field_keyed in sync with enum_field
            // enum_field_keyed((target, field_name), field_id) :- enum_field(target, (field_name, field_id))
            enum_field_keyed.from_map(&enum_field, |&(target, (ref field_name, field_id))| {
                ((target, field_name.clone()), field_id)
            });

            // Find match expressions whose subject is a constant enum
            // match_with_enum((m, e, v), m) :- const_value(s, Enum(e, v)), enum_match_subject(s, m).
            match_with_enum.from_join(
                &const_value,
                &enum_match_subject,
                |_subject: &ExprId, const_val: &Const, match_id: &ExprId| match const_val {
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

            // Select the matching arm body for each match with a constant subject
            // selected_arm(arm_body, m) :- match_with_enum((m, e, v), m), match_arm_rel((m, e, v), arm_body).
            selected_arm.from_join(
                &match_with_enum,
                &match_arm_rel,
                |_key: &(ExprId, String, String), match_id: &ExprId, arm_body: &ExprId| {
                    (*arm_body, *match_id)
                },
            );

            // Find option matches with constant subjects
            // option_match_with_const((m, is_some), m) :- const_value(s, Option(inner)), option_match_subject(s, m).
            option_match_with_const.from_join(
                &const_value,
                &option_match_subject,
                |_subject: &ExprId, const_val: &Const, match_id: &ExprId| match const_val {
                    Const::Option(inner) => ((*match_id, inner.is_some()), *match_id),
                    _ => unreachable!("option match subject must have option constant"),
                },
            );

            // Select the appropriate option arm by joining on (match_id, is_some)
            // selected_arm(body, m) :- option_match_with_const((m, is_some), m), option_arms((m, is_some), body).
            selected_arm.from_join(
                &option_match_with_const,
                &option_arms,
                |_key: &(ExprId, bool), match_id: &ExprId, body_id: &ExprId| (*body_id, *match_id),
            );

            // Find bool matches with constant subjects
            // bool_match_with_const((m, is_true), m) :- const_value(s, Bool(b)), bool_match_subject(s, m).
            bool_match_with_const.from_join(
                &const_value,
                &bool_match_subject,
                |_subject: &ExprId, const_val: &Const, match_id: &ExprId| match const_val {
                    Const::Bool(b) => ((*match_id, *b), *match_id),
                    _ => unreachable!("bool match subject must have bool constant"),
                },
            );

            // Select the appropriate bool arm by joining on (match_id, is_true)
            // selected_arm(body, m) :- bool_match_with_const((m, is_true), m), bool_arms((m, is_true), body).
            selected_arm.from_join(
                &bool_match_with_const,
                &bool_arms,
                |_key: &(ExprId, bool), match_id: &ExprId, body_id: &ExprId| (*body_id, *match_id),
            );

            // Selected arms: match expression is equivalent to its selected arm body.
            propagate_to.from_map(&selected_arm, |&(arm_body, match_id)| (arm_body, match_id));
        }

        let const_map: HashMap<ExprId, Const> = const_value.complete().iter().cloned().collect();

        let mut result = entrypoint;
        for stmt in &mut result.body {
            stmt.traverse_mut(&mut |s| {
                if let Some(expr) = s.expr_mut() {
                    expr.traverse_mut(&mut |e| {
                        if let Some(const_val) = const_map.get(&e.id()) {
                            if let Some(expr) = const_val.to_expr(e.id(), e.as_type(), &const_map) {
                                *e = expr;
                            }
                        }
                    });
                }
            });
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::syntax::builder::{build_ir, build_ir_with_enums};
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let result = ConstantPropagationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_fold_simple_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.bool(false)), |t| {
                    t.write("Should be true");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!false) {
                    write("Should be true")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Should be true")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_double_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.not(t.bool(true))), |t| {
                    t.write("Double negation");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!(!true)) {
                    write("Double negation")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Double negation")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_triple_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.not(t.not(t.bool(false)))), |t| {
                    t.write("Triple negation");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!(!(!false))) {
                    write("Triple negation")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Triple negation")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_boolean_equality_comparisons() {
        let ep = build_ir("Test", [], |t| {
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
                Test() {
                  if (true == true) {
                    write("true == true")
                  }
                  if (false == false) {
                    write("false == false")
                  }
                  if (true == false) {
                    write("Should not appear")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("true == true")
                  }
                  if true {
                    write("false == false")
                  }
                  if false {
                    write("Should not appear")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_equality_with_nested_negations() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(
                    t.eq(t.not(t.not(t.bool(false))), t.not(t.bool(false))),
                    |t| {
                        t.write("Should not appear");
                    },
                );
            }),
            expect![[r#"
                -- before --
                Test() {
                  if ((!(!false)) == (!false)) {
                    write("Should not appear")
                  }
                }

                -- after --
                Test() {
                  if false {
                    write("Should not appear")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_constants_through_variables() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let x = (!(!true)) in {
                    if x {
                      write("x is true")
                    }
                    if (!x) {
                      write("x is false")
                    }
                  }
                }

                -- after --
                Test() {
                  let x = true in {
                    if true {
                      write("x is true")
                    }
                    if false {
                      write("x is false")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_equality_with_variable_operands() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let x = true in {
                    let y = (!true) in {
                      if (x == y) {
                        write("x equals y")
                      }
                      if (x == (!y)) {
                        write("x equals not y")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let x = true in {
                    let y = false in {
                      if false {
                        write("x equals y")
                      }
                      if true {
                        write("x equals not y")
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
            build_ir("Test", [], |t| {
                t.let_stmt("message", t.str("Hello, World!"), |t| {
                    t.write_expr_escaped(t.var("message"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let message = "Hello, World!" in {
                    write_escaped(message)
                  }
                }

                -- after --
                Test() {
                  let message = "Hello, World!" in {
                    write_escaped("Hello, World!")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_nested_string_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("greeting", t.str("Hello"), |t| {
                    t.let_stmt("name", t.str("World"), |t| {
                        t.write_expr_escaped(t.var("greeting"));
                        t.write_expr_escaped(t.var("name"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let greeting = "Hello" in {
                    let name = "World" in {
                      write_escaped(greeting)
                      write_escaped(name)
                    }
                  }
                }

                -- after --
                Test() {
                  let greeting = "Hello" in {
                    let name = "World" in {
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
            build_ir("Test", [], |t| {
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
                Test() {
                  let title = "Welcome" in {
                    write_escaped(title)
                    write_escaped(title)
                    let subtitle = title in {
                      write_escaped(subtitle)
                    }
                  }
                }

                -- after --
                Test() {
                  let title = "Welcome" in {
                    write_escaped("Welcome")
                    write_escaped("Welcome")
                    let subtitle = "Welcome" in {
                      write_escaped("Welcome")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_string_equality_comparisons() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  if ("hello" == "hello") {
                    write("Strings are equal")
                  }
                  if ("hello" == "world") {
                    write("Should not appear")
                  }
                  let greeting = "hello" in {
                    let message = "hello" in {
                      if (greeting == message) {
                        write("Variables are equal")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Strings are equal")
                  }
                  if false {
                    write("Should not appear")
                  }
                  let greeting = "hello" in {
                    let message = "hello" in {
                      if true {
                        write("Variables are equal")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_string_concatenation() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(
                    t.string_concat(t.string_concat(t.str("Hello"), t.str(" ")), t.str("World")),
                );
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped((("Hello" + " ") + "World"))
                }

                -- after --
                Test() {
                  write_escaped("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_string_concatenation_in_equality() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  if (("foo" + "bar") == "foobar") {
                    write("Concatenation matches")
                  }
                  if (("hello" + " world") == "hello") {
                    write("Should not appear")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Concatenation matches")
                  }
                  if false {
                    write("Should not appear")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_string_concatenation_with_propagated_variables() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let prefix = "Hello" in {
                    let suffix = (" " + "World") in {
                      let full = (prefix + suffix) in {
                        write_escaped(full)
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let prefix = "Hello" in {
                    let suffix = " World" in {
                      let full = "Hello World" in {
                        write_escaped("Hello World")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_simple_match_expression() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                    t.write_expr_escaped(t.match_expr(
                        t.var("color"),
                        vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Red in {
                    write_escaped(match color {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                Test() {
                  let color = Color::Red in {
                    write_escaped("red")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_match_with_variable_subject() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Blue"), |t| {
                    t.write_expr_escaped(t.match_expr(
                        t.var("color"),
                        vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Blue in {
                    write_escaped(match color {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                Test() {
                  let color = Color::Blue in {
                    write_escaped("blue")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_match_with_constant_arm_body() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                    t.if_stmt(
                        t.match_expr(
                            t.var("color"),
                            vec![("Red", t.not(t.bool(false))), ("Blue", t.bool(false))],
                        ),
                        |t| {
                            t.write("Match evaluated to true");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Red in {
                    if match color {
                      Color::Red => (!false),
                      Color::Blue => false,
                    } {
                      write("Match evaluated to true")
                    }
                  }
                }

                -- after --
                Test() {
                  let color = Color::Red in {
                    if true {
                      write("Match evaluated to true")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_match_in_equality() {
        check(
            build_ir_with_enums(
                "Test",
                [],
                vec![("Status", vec!["Active", "Inactive"])],
                |t| {
                    t.let_stmt("status", t.enum_variant("Status", "Active"), |t| {
                        t.if_stmt(
                            t.eq(
                                t.match_expr(
                                    t.var("status"),
                                    vec![("Active", t.str("on")), ("Inactive", t.str("off"))],
                                ),
                                t.str("on"),
                            ),
                            |t| {
                                t.write("Status is active");
                            },
                        );
                    });
                },
            ),
            expect![[r#"
                -- before --
                Test() {
                  let status = Status::Active in {
                    if (match status {
                      Status::Active => "on",
                      Status::Inactive => "off",
                    } == "on") {
                      write("Status is active")
                    }
                  }
                }

                -- after --
                Test() {
                  let status = Status::Active in {
                    if true {
                      write("Status is active")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_constant_through_variables() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("x", t.enum_variant("Color", "Red"), |t| {
                    t.let_stmt("y", t.var("x"), |t| {
                        t.write_expr_escaped(t.match_expr(
                            t.var("y"),
                            vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                        ));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = Color::Red in {
                    let y = x in {
                      write_escaped(match y {
                        Color::Red => "red",
                        Color::Blue => "blue",
                      })
                    }
                  }
                }

                -- after --
                Test() {
                  let x = Color::Red in {
                    let y = Color::Red in {
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
            build_ir("Test", [], |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("y", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("y"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let y = "second" in {
                    write_escaped(y)
                  }
                }

                -- after --
                Test() {
                  let x = "first" in {
                    write_escaped("first")
                  }
                  let y = "second" in {
                    write_escaped("second")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_enum_variant_fields() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::IrModuleBuilder;

        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "x",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hi"))]),
                    |_| {},
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let x = Msg::Say(text: "hi") in {}
                }

                -- after --
                Test() {
                  let x = Msg::Say(text: "hi") in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_constant_strings() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("flex"),
                    t.str("items-center"),
                    t.str("gap-4"),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("flex", tw_merge("items-center", "gap-4")))
                }

                -- after --
                Test() {
                  write_escaped("flex items-center gap-4")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_tailwind_conflicts() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("px-4"),
                    t.str("py-2"),
                    t.str("p-6"),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("px-4", tw_merge("py-2", "p-6")))
                }

                -- after --
                Test() {
                  write_escaped("p-6")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_propagated_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("base", t.str("flex items-center"), |t| {
                    t.let_stmt("extra", t.str("gap-4 text-red-500"), |t| {
                        t.write_expr_escaped(t.merge_classes(vec![t.var("base"), t.var("extra")]));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let base = "flex items-center" in {
                    let extra = "gap-4 text-red-500" in {
                      write_escaped(tw_merge(base, extra))
                    }
                  }
                }

                -- after --
                Test() {
                  let base = "flex items-center" in {
                    let extra = "gap-4 text-red-500" in {
                      write_escaped("flex items-center gap-4 text-red-500")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_empty_merge_classes() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped("")
                }

                -- after --
                Test() {
                  write_escaped("")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_merge_classes() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("px-4"),
                    t.merge_classes(vec![t.str("px-2"), t.str("p-3")]),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("px-4", tw_merge("px-2", "p-3")))
                }

                -- after --
                Test() {
                  write_escaped("p-3")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_enum_match() {
        check(
            build_ir_with_enums("Test", [], vec![("Size", vec!["Small", "Large"])], |t| {
                t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                    t.write_expr_escaped(t.merge_classes(vec![
                        t.str("px-4"),
                        t.match_expr(
                            t.var("size"),
                            vec![("Small", t.str("text-sm")), ("Large", t.str("text-lg"))],
                        ),
                    ]));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let size = Size::Large in {
                    write_escaped(tw_merge("px-4", match size {
                      Size::Small => "text-sm",
                      Size::Large => "text-lg",
                    }))
                  }
                }

                -- after --
                Test() {
                  let size = Size::Large in {
                    write_escaped("px-4 text-lg")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_enum_match_containing_merge_classes() {
        check(
            build_ir_with_enums("Test", [], vec![("Size", vec!["Small", "Large"])], |t| {
                t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                    t.write_expr_escaped(t.merge_classes(vec![
                        t.str("flex"),
                        t.match_expr(
                            t.var("size"),
                            vec![
                                (
                                    "Small",
                                    t.merge_classes(vec![t.str("p-2"), t.str("text-sm")]),
                                ),
                                (
                                    "Large",
                                    t.merge_classes(vec![t.str("p-4"), t.str("text-lg")]),
                                ),
                            ],
                        ),
                    ]));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let size = Size::Large in {
                    write_escaped(tw_merge("flex", match size {
                      Size::Small => tw_merge("p-2", "text-sm"),
                      Size::Large => tw_merge("p-4", "text-lg"),
                    }))
                  }
                }

                -- after --
                Test() {
                  let size = Size::Large in {
                    write_escaped("flex p-4 text-lg")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_option_match_with_some() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let opt = Some("hello") in {
                    write_escaped(match opt {
                      Some(_) => "got some",
                      None => "got none",
                    })
                  }
                }

                -- after --
                Test() {
                  let opt = Some("hello") in {
                    write_escaped("got some")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_option_match_with_none() {
        use crate::dop::Type;

        check(
            build_ir("Test", [], |t| {
                t.let_stmt("opt", t.none(Type::String), |t| {
                    t.write_expr_escaped(t.option_match_expr(
                        t.var("opt"),
                        t.str("got some"),
                        t.str("got none"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let opt = None in {
                    write_escaped(match opt {
                      Some(_) => "got some",
                      None => "got none",
                    })
                  }
                }

                -- after --
                Test() {
                  let opt = None in {
                    write_escaped("got none")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_constant_through_variables() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let x = Some("hello") in {
                    let y = x in {
                      write_escaped(match y {
                        Some(_) => "got some",
                        None => "got none",
                      })
                    }
                  }
                }

                -- after --
                Test() {
                  let x = Some("hello") in {
                    let y = Some("hello") in {
                      write_escaped("got some")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_value() {
        use crate::dop::Type;

        check(
            build_ir("Test", [], |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("opt"),
                        "inner",
                        Type::String,
                        |t| t.var("inner"),
                        t.str("default"),
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let opt = Some("hello") in {
                    write_escaped(match opt {
                      Some(inner) => inner,
                      None => "default",
                    })
                  }
                }

                -- after --
                Test() {
                  let opt = Some("hello") in {
                    write_escaped("hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_in_nested_expression() {
        use crate::dop::Type;

        // Test that the binding value propagates into nested expressions
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("opt"),
                        "inner",
                        Type::String,
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
                Test() {
                  let opt = Some("hello") in {
                    write_escaped(match opt {
                      Some(inner) => (inner + " world"),
                      None => "default",
                    })
                  }
                }

                -- after --
                Test() {
                  let opt = Some("hello") in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_option_binding_in_equality() {
        use crate::dop::Type;

        // Test that the binding value propagates into equality comparisons
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.if_stmt(
                        t.option_match_expr_with_binding(
                            t.var("opt"),
                            "inner",
                            Type::String,
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
                Test() {
                  let opt = Some("hello") in {
                    if match opt {
                      Some(inner) => (inner == "hello"),
                      None => false,
                    } {
                      write("matched hello")
                    }
                  }
                }

                -- after --
                Test() {
                  let opt = Some("hello") in {
                    if true {
                      write("matched hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_through_nested_option_match() {
        use crate::dop::Type;

        // Test nested option matches using let statements to bind intermediate values
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("outer", t.some(t.some(t.str("nested"))), |t| {
                    // First match extracts inner_opt from outer
                    t.let_stmt(
                        "inner_result",
                        t.option_match_expr_with_binding(
                            t.var("outer"),
                            "inner_opt",
                            Type::Option(Box::new(Type::String)),
                            |t| t.var("inner_opt"),
                            t.none(Type::String),
                        ),
                        |t| {
                            // Second match extracts value from inner_result
                            t.write_expr_escaped(t.option_match_expr_with_binding(
                                t.var("inner_result"),
                                "value",
                                Type::String,
                                |t| t.var("value"),
                                t.str("inner none"),
                            ));
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let outer = Some(Some("nested")) in {
                    let inner_result = match outer {
                      Some(inner_opt) => inner_opt,
                      None => None,
                    } in {
                      write_escaped(match inner_result {
                        Some(value) => value,
                        None => "inner none",
                      })
                    }
                  }
                }

                -- after --
                Test() {
                  let outer = Some(Some("nested")) in {
                    let inner_result = Some("nested") in {
                      write_escaped("nested")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_option_match_with_inline_let() {
        use crate::dop::Type;

        // Test nested option match where inner match is inside a let expression in the Some arm
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("outer", t.some(t.some(t.str("nested"))), |t| {
                    t.write_expr_escaped(t.option_match_expr_with_binding(
                        t.var("outer"),
                        "inner_opt",
                        Type::Option(Box::new(Type::String)),
                        |t| {
                            // let inner_opt_var = inner_opt in match inner_opt_var { ... }
                            t.let_expr("inner_opt_var", t.var("inner_opt"), |t| {
                                t.option_match_expr_with_binding(
                                    t.var("inner_opt_var"),
                                    "value",
                                    Type::String,
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
                Test() {
                  let outer = Some(Some("nested")) in {
                    write_escaped(match outer {
                      Some(inner_opt) => let inner_opt_var = inner_opt in match inner_opt_var {
                        Some(value) => value,
                        None => "inner none",
                      },
                      None => "outer none",
                    })
                  }
                }

                -- after --
                Test() {
                  let outer = Some(Some("nested")) in {
                    write_escaped("nested")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_value() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "msg",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                    |t| {
                        t.write_expr_escaped(t.match_expr_with_bindings(
                            t.var("msg"),
                            vec![(
                                "Say",
                                vec![("text", "t")],
                                Box::new(|t: &IrBuilder| t.var("t")),
                            )],
                        ));
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let msg = Msg::Say(text: "hello") in {
                    write_escaped(match msg {Msg::Say(text: t) => t})
                  }
                }

                -- after --
                Test() {
                  let msg = Msg::Say(text: "hello") in {
                    write_escaped("hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_in_string_concat() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "msg",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("world"))]),
                    |t| {
                        t.write_expr_escaped(t.match_expr_with_bindings(
                            t.var("msg"),
                            vec![(
                                "Say",
                                vec![("text", "t")],
                                Box::new(|t: &IrBuilder| {
                                    t.string_concat(t.str("hello "), t.var("t"))
                                }),
                            )],
                        ));
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let msg = Msg::Say(text: "world") in {
                    write_escaped(match msg {
                      Msg::Say(text: t) => ("hello " + t),
                    })
                  }
                }

                -- after --
                Test() {
                  let msg = Msg::Say(text: "world") in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_in_equality() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "msg",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                    |t| {
                        t.if_stmt(
                            t.match_expr_with_bindings(
                                t.var("msg"),
                                vec![(
                                    "Say",
                                    vec![("text", "t")],
                                    Box::new(|t: &IrBuilder| t.eq(t.var("t"), t.str("hello"))),
                                )],
                            ),
                            |t| {
                                t.write("matched hello");
                            },
                        );
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let msg = Msg::Say(text: "hello") in {
                    if match msg {Msg::Say(text: t) => (t == "hello")} {
                      write("matched hello")
                    }
                  }
                }

                -- after --
                Test() {
                  let msg = Msg::Say(text: "hello") in {
                    if true {
                      write("matched hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_through_variable() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        // Test that enum bindings work when the subject is propagated through a variable
        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "x",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hello"))]),
                    |t| {
                        t.let_stmt("y", t.var("x"), |t| {
                            t.write_expr_escaped(t.match_expr_with_bindings(
                                t.var("y"),
                                vec![(
                                    "Say",
                                    vec![("text", "t")],
                                    Box::new(|t: &IrBuilder| t.var("t")),
                                )],
                            ));
                        });
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let x = Msg::Say(text: "hello") in {
                    let y = x in {
                      write_escaped(match y {Msg::Say(text: t) => t})
                    }
                  }
                }

                -- after --
                Test() {
                  let x = Msg::Say(text: "hello") in {
                    let y = Msg::Say(text: "hello") in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_multiple_enum_bindings() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        // Test multiple field bindings in a single variant
        let module = IrModuleBuilder::new()
            .enum_with_fields("Pair", |e| {
                e.variant_with_fields(
                    "Values",
                    vec![("first", Type::String), ("second", Type::String)],
                );
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "pair",
                    t.enum_variant_with_fields(
                        "Pair",
                        "Values",
                        vec![("first", t.str("hello")), ("second", t.str("world"))],
                    ),
                    |t| {
                        t.write_expr_escaped(t.match_expr_with_bindings(
                            t.var("pair"),
                            vec![(
                                "Values",
                                vec![("first", "a"), ("second", "b")],
                                Box::new(|t: &IrBuilder| {
                                    t.string_concat(
                                        t.var("a"),
                                        t.string_concat(t.str(" "), t.var("b")),
                                    )
                                }),
                            )],
                        ));
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let pair = Pair::Values(first: "hello", second: "world") in {
                    write_escaped(match pair {
                      Pair::Values(first: a, second: b) => (a + (" " + b)),
                    })
                  }
                }

                -- after --
                Test() {
                  let pair = Pair::Values(first: "hello", second: "world") in {
                    write_escaped("hello world")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_enum_match_selecting_correct_arm_with_bindings() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        // Test that when we have multiple variants, we select the correct arm
        let module = IrModuleBuilder::new()
            .enum_with_fields("Result", |e| {
                e.variant_with_fields("Ok", vec![("value", Type::String)]);
                e.variant_with_fields("Err", vec![("msg", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "result",
                    t.enum_variant_with_fields("Result", "Ok", vec![("value", t.str("success"))]),
                    |t| {
                        t.write_expr_escaped(t.match_expr_with_bindings(
                            t.var("result"),
                            vec![
                                (
                                    "Ok",
                                    vec![("value", "v")],
                                    Box::new(|t: &IrBuilder| t.var("v")),
                                ),
                                (
                                    "Err",
                                    vec![("msg", "m")],
                                    Box::new(|t: &IrBuilder| {
                                        t.string_concat(t.str("error: "), t.var("m"))
                                    }),
                                ),
                            ],
                        ));
                    },
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let result = Result::Ok(value: "success") in {
                    write_escaped(match result {
                      Result::Ok(value: v) => v,
                      Result::Err(msg: m) => ("error: " + m),
                    })
                  }
                }

                -- after --
                Test() {
                  let result = Result::Ok(value: "success") in {
                    write_escaped("success")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_binding_through_match_arm_selection() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        // When a match expression selects an arm that returns an enum with fields,
        // those field values should propagate to subsequent matches on the result.
        let module = IrModuleBuilder::new()
            .enum_decl("Choice", ["A", "B"])
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt("choice", t.enum_variant("Choice", "A"), |t| {
                    t.let_stmt(
                        "x",
                        t.match_expr(
                            t.var("choice"),
                            vec![
                                (
                                    "A",
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("hello"))],
                                    ),
                                ),
                                (
                                    "B",
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("world"))],
                                    ),
                                ),
                            ],
                        ),
                        |t| {
                            t.write_expr_escaped(t.match_expr_with_bindings(
                                t.var("x"),
                                vec![(
                                    "Say",
                                    vec![("text", "t")],
                                    Box::new(|t: &IrBuilder| t.var("t")),
                                )],
                            ));
                        },
                    );
                });
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let choice = Choice::A in {
                    let x = match choice {
                      Choice::A => Msg::Say(text: "hello"),
                      Choice::B => Msg::Say(text: "world"),
                    } in {
                      write_escaped(match x {Msg::Say(text: t) => t})
                    }
                  }
                }

                -- after --
                Test() {
                  let choice = Choice::A in {
                    let x = Msg::Say(text: "hello") in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_bool_match_with_true() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let flag = true in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                }

                -- after --
                Test() {
                  let flag = true in {
                    write_escaped("yes")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_bool_match_with_false() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let flag = false in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                }

                -- after --
                Test() {
                  let flag = false in {
                    write_escaped("no")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_bool_constant_through_variables_in_match() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let x = true in {
                    let y = x in {
                      write_escaped(match y {true => "yes", false => "no"})
                    }
                  }
                }

                -- after --
                Test() {
                  let x = true in {
                    let y = true in {
                      write_escaped("yes")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_bool_match_with_negated_subject() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let flag = (!false) in {
                    write_escaped(match flag {
                      true => "was true",
                      false => "was false",
                    })
                  }
                }

                -- after --
                Test() {
                  let flag = true in {
                    write_escaped("was true")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn enum_binding_through_let_in_match_arm() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};

        // Test that enum_field propagates through let expressions in match arm bodies.
        // When a match arm body is `let x = Enum(...) in x`, the field information should
        // flow from the enum literal -> let body -> let expr -> match expr.
        let module = IrModuleBuilder::new()
            .enum_decl("Choice", ["A", "B"])
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt("choice", t.enum_variant("Choice", "A"), |t| {
                    t.let_stmt(
                        "y",
                        t.match_expr(
                            t.var("choice"),
                            vec![
                                // Arm body is a let expression, not a direct enum literal
                                (
                                    "A",
                                    t.let_expr(
                                        "x",
                                        t.enum_variant_with_fields(
                                            "Msg",
                                            "Say",
                                            vec![("text", t.str("hello"))],
                                        ),
                                        |t| t.var("x"),
                                    ),
                                ),
                                (
                                    "B",
                                    t.enum_variant_with_fields(
                                        "Msg",
                                        "Say",
                                        vec![("text", t.str("world"))],
                                    ),
                                ),
                            ],
                        ),
                        |t| {
                            t.write_expr_escaped(t.match_expr_with_bindings(
                                t.var("y"),
                                vec![(
                                    "Say",
                                    vec![("text", "txt")],
                                    Box::new(|t: &IrBuilder| t.var("txt")),
                                )],
                            ));
                        },
                    );
                });
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let choice = Choice::A in {
                    let y = match choice {
                      Choice::A => let x = Msg::Say(text: "hello") in x,
                      Choice::B => Msg::Say(text: "world"),
                    } in {
                      write_escaped(match y {Msg::Say(text: txt) => txt})
                    }
                  }
                }

                -- after --
                Test() {
                  let choice = Choice::A in {
                    let y = Msg::Say(text: "hello") in {
                      write_escaped("hello")
                    }
                  }
                }
            "#]],
        );
    }
}
