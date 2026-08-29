use crate::expr::patterns::{EnumMatchArm, Match};
use crate::expr::typing::r#type::Type;
use crate::ir::ir_var::IrVar;

use super::pure_module::{
    PureExpr, PureForSource, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use super::writer_module::{
    WriterArgument, WriterExpr, WriterForSource, WriterFunctionBody, WriterFunctionDeclaration,
    WriterModule, WriterPageDeclaration, WriterStatement,
};

/// Lower a whole PureModule into a WriterModule.
pub fn lower_pure(module: PureModule) -> WriterModule {
    WriterModule {
        pages: module.pages.into_iter().map(lower_page).collect(),
        functions: module.functions.into_iter().map(lower_function).collect(),
        records: module.records,
        enums: module.enums,
        var_ids: module.var_ids,
    }
}

fn lower_page(decl: PurePageDeclaration) -> WriterPageDeclaration {
    let mut body = Vec::new();
    lower_output(decl.body, &mut body);
    WriterPageDeclaration {
        name: decl.name,
        parameters: decl.parameters,
        body,
    }
}

/// Lower a function declaration, choosing the calling convention from its
/// return type. Fragment compiles to destination-passing, everything else
/// compiles as an ordinary value-returning function.
fn lower_function(decl: PureFunctionDeclaration) -> WriterFunctionDeclaration {
    let body = if matches!(*decl.return_type, Type::Fragment) {
        let mut statements = Vec::new();
        lower_output(decl.body, &mut statements);
        WriterFunctionBody::Writes(statements)
    } else {
        WriterFunctionBody::Returns(lower_value(decl.body))
    };
    WriterFunctionDeclaration {
        name: decl.name,
        parameters: decl.parameters,
        return_type: decl.return_type,
        body,
    }
}

/// Lower a Fragment-typed PureExpr in output position.
fn lower_output(expr: PureExpr, out: &mut Vec<WriterStatement>) {
    match expr {
        PureExpr::FragmentRaw { content, .. } => {
            out.push(WriterStatement::Write { content });
        }

        PureExpr::FragmentEscape { expr, .. } => {
            let expr = lower_value(*expr);
            out.push(WriterStatement::WriteString { expr });
        }

        PureExpr::FragmentConcat { parts, .. } => {
            for part in parts {
                lower_output(part, out);
            }
        }

        PureExpr::FragmentFor {
            var, source, body, ..
        } => {
            let source = lower_for_source(*source);
            let mut body_stmts = Vec::new();
            lower_output(*body, &mut body_stmts);
            out.push(WriterStatement::For {
                var,
                source,
                body: body_stmts,
            });
        }

        PureExpr::FunctionCall {
            function_name,
            args,
            kind,
            ..
        } => {
            assert!(
                matches!(*kind, Type::Fragment),
                "non-Fragment function call in output position: {}",
                function_name
            );
            let args = args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect();
            out.push(WriterStatement::WriteFunction {
                function_name,
                args,
            });
        }

        PureExpr::Let {
            var, value, body, ..
        } => {
            let value = lower_value(*value);
            let mut body_stmts = Vec::new();
            lower_output(*body, &mut body_stmts);
            out.push(WriterStatement::Let {
                var,
                value,
                body: body_stmts,
            });
        }

        PureExpr::Match { match_, .. } => {
            let match_ = lower_match_output(match_);
            out.push(WriterStatement::Match { match_ });
        }

        PureExpr::VariableReference { ref kind, .. } | PureExpr::FieldAccess { ref kind, .. } => {
            assert!(
                matches!(**kind, Type::Fragment),
                "non-Fragment expression in output position: {:?}",
                expr
            );
            let expr = lower_value(expr);
            out.push(WriterStatement::WriteFragment { expr });
        }

        PureExpr::StringLiteral { .. }
        | PureExpr::BooleanLiteral { .. }
        | PureExpr::FloatLiteral { .. }
        | PureExpr::IntLiteral { .. }
        | PureExpr::ArrayLiteral { .. }
        | PureExpr::RecordLiteral { .. }
        | PureExpr::EnumLiteral { .. }
        | PureExpr::OptionLiteral { .. }
        | PureExpr::StringConcat { .. }
        | PureExpr::NumericAdd { .. }
        | PureExpr::NumericSubtract { .. }
        | PureExpr::NumericMultiply { .. }
        | PureExpr::NumericNegation { .. }
        | PureExpr::BooleanNegation { .. }
        | PureExpr::BooleanLogicalAnd { .. }
        | PureExpr::BooleanLogicalOr { .. }
        | PureExpr::Equals { .. }
        | PureExpr::LessThan { .. }
        | PureExpr::LessThanOrEqual { .. }
        | PureExpr::ArrayLength { .. }
        | PureExpr::ArrayIsEmpty { .. }
        | PureExpr::StringIsEmpty { .. }
        | PureExpr::OptionIsSome { .. }
        | PureExpr::OptionIsNone { .. }
        | PureExpr::IntToString { .. }
        | PureExpr::FloatToInt { .. }
        | PureExpr::IntToFloat { .. } => {
            panic!(
                "non-Fragment-typed expression in output position: {:?}",
                expr
            );
        }
    }
}

fn lower_for_source(source: PureForSource) -> WriterForSource {
    match source {
        PureForSource::Array(array) => WriterForSource::Array(lower_value(array)),
        PureForSource::RangeInclusive { start, end } => WriterForSource::RangeInclusive {
            start: lower_value(start),
            end: lower_value(end),
        },
    }
}

fn lower_match_output(
    match_: Match<PureExpr, PureExpr, IrVar>,
) -> Match<WriterExpr, Vec<WriterStatement>, IrVar> {
    match match_ {
        Match::Bool {
            subject,
            true_body,
            false_body,
        } => {
            let subject = Box::new(lower_value(*subject));
            let mut true_stmts = Vec::new();
            lower_output(*true_body, &mut true_stmts);
            let mut false_stmts = Vec::new();
            lower_output(*false_body, &mut false_stmts);
            Match::Bool {
                subject,
                true_body: Box::new(true_stmts),
                false_body: Box::new(false_stmts),
            }
        }
        Match::Option {
            subject,
            some_arm_binding,
            some_arm_body,
            none_arm_body,
        } => {
            let subject = Box::new(lower_value(*subject));
            let mut some_stmts = Vec::new();
            lower_output(*some_arm_body, &mut some_stmts);
            let mut none_stmts = Vec::new();
            lower_output(*none_arm_body, &mut none_stmts);
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body: Box::new(some_stmts),
                none_arm_body: Box::new(none_stmts),
            }
        }
        Match::Enum { subject, arms } => {
            let subject = Box::new(lower_value(*subject));
            let arms = arms
                .into_iter()
                .map(|arm| {
                    let mut body = Vec::new();
                    lower_output(arm.body, &mut body);
                    EnumMatchArm {
                        pattern: arm.pattern,
                        bindings: arm.bindings,
                        body,
                    }
                })
                .collect();
            Match::Enum { subject, arms }
        }
    }
}

fn lower_match_value(
    match_: Match<PureExpr, PureExpr, IrVar>,
) -> Match<WriterExpr, WriterExpr, IrVar> {
    match match_ {
        Match::Bool {
            subject,
            true_body,
            false_body,
        } => Match::Bool {
            subject: Box::new(lower_value(*subject)),
            true_body: Box::new(lower_value(*true_body)),
            false_body: Box::new(lower_value(*false_body)),
        },
        Match::Option {
            subject,
            some_arm_binding,
            some_arm_body,
            none_arm_body,
        } => Match::Option {
            subject: Box::new(lower_value(*subject)),
            some_arm_binding,
            some_arm_body: Box::new(lower_value(*some_arm_body)),
            none_arm_body: Box::new(lower_value(*none_arm_body)),
        },
        Match::Enum { subject, arms } => Match::Enum {
            subject: Box::new(lower_value(*subject)),
            arms: arms
                .into_iter()
                .map(|arm| EnumMatchArm {
                    pattern: arm.pattern,
                    bindings: arm.bindings,
                    body: lower_value(arm.body),
                })
                .collect(),
        },
    }
}

/// Lower a PureExpr in value position.
fn lower_value(expr: PureExpr) -> WriterExpr {
    match expr {
        expr @ (PureExpr::FragmentRaw { .. }
        | PureExpr::FragmentEscape { .. }
        | PureExpr::FragmentConcat { .. }
        | PureExpr::FragmentFor { .. }) => {
            let mut body = Vec::new();
            lower_output(expr, &mut body);
            WriterExpr::FragmentLiteral { body }
        }

        PureExpr::FunctionCall {
            function_name,
            args,
            kind,
            ..
        } if matches!(*kind, Type::Fragment) => {
            let args = args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect();
            WriterExpr::FragmentLiteral {
                body: vec![WriterStatement::WriteFunction {
                    function_name,
                    args,
                }],
            }
        }

        PureExpr::FunctionCall {
            function_name,
            args,
            kind,
            ..
        } => WriterExpr::FunctionCall {
            function_name,
            args: args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect(),
            kind,
        },

        PureExpr::Let {
            var,
            value,
            body,
            kind,
            ..
        } => WriterExpr::Let {
            var,
            value: Box::new(lower_value(*value)),
            body: Box::new(lower_value(*body)),
            kind,
        },

        PureExpr::Match { match_, kind, .. } => WriterExpr::Match {
            match_: lower_match_value(match_),
            kind,
        },

        PureExpr::VariableReference { value, kind, .. } => {
            WriterExpr::VariableReference { value, kind }
        }

        PureExpr::FieldAccess {
            record,
            field,
            kind,
            ..
        } => WriterExpr::FieldAccess {
            record: Box::new(lower_value(*record)),
            field,
            kind,
        },

        PureExpr::StringLiteral { value, .. } => WriterExpr::StringLiteral { value },

        PureExpr::BooleanLiteral { value, .. } => WriterExpr::BooleanLiteral { value },

        PureExpr::FloatLiteral { value, .. } => WriterExpr::FloatLiteral { value },

        PureExpr::IntLiteral { value, .. } => WriterExpr::IntLiteral { value },

        PureExpr::ArrayLiteral { elements, kind, .. } => WriterExpr::ArrayLiteral {
            elements: elements.into_iter().map(lower_value).collect(),
            kind,
        },

        PureExpr::RecordLiteral {
            record_name,
            fields,
            kind,
            ..
        } => WriterExpr::RecordLiteral {
            record_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_value(value)))
                .collect(),
            kind,
        },

        PureExpr::EnumLiteral {
            enum_name,
            variant_name,
            fields,
            kind,
            ..
        } => WriterExpr::EnumLiteral {
            enum_name,
            variant_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_value(value)))
                .collect(),
            kind,
        },

        PureExpr::OptionLiteral { value, kind, .. } => WriterExpr::OptionLiteral {
            value: value.map(|v| Box::new(lower_value(*v))),
            kind,
        },

        PureExpr::StringConcat { parts, .. } => WriterExpr::StringConcat {
            parts: parts.into_iter().map(lower_value).collect(),
        },

        PureExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::NumericAdd {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::NumericSubtract {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::NumericMultiply {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::NumericNegation {
            operand,
            operand_type,
            ..
        } => WriterExpr::NumericNegation {
            operand: Box::new(lower_value(*operand)),
            operand_type,
        },

        PureExpr::BooleanNegation { operand, .. } => WriterExpr::BooleanNegation {
            operand: Box::new(lower_value(*operand)),
        },

        PureExpr::BooleanLogicalAnd { left, right, .. } => WriterExpr::BooleanLogicalAnd {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
        },

        PureExpr::BooleanLogicalOr { left, right, .. } => WriterExpr::BooleanLogicalOr {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
        },

        PureExpr::Equals {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::Equals {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::LessThan {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => WriterExpr::LessThanOrEqual {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
            operand_types,
        },

        PureExpr::ArrayLength { array, .. } => WriterExpr::ArrayLength {
            array: Box::new(lower_value(*array)),
        },

        PureExpr::ArrayIsEmpty { array, .. } => WriterExpr::ArrayIsEmpty {
            array: Box::new(lower_value(*array)),
        },

        PureExpr::StringIsEmpty { string, .. } => WriterExpr::StringIsEmpty {
            string: Box::new(lower_value(*string)),
        },

        PureExpr::OptionIsSome { option, .. } => WriterExpr::OptionIsSome {
            option: Box::new(lower_value(*option)),
        },

        PureExpr::OptionIsNone { option, .. } => WriterExpr::OptionIsNone {
            option: Box::new(lower_value(*option)),
        },

        PureExpr::IntToString { value, .. } => WriterExpr::IntToString {
            value: Box::new(lower_value(*value)),
        },

        PureExpr::FloatToInt { value, .. } => WriterExpr::FloatToInt {
            value: Box::new(lower_value(*value)),
        },

        PureExpr::IntToFloat { value, .. } => WriterExpr::IntToFloat {
            value: Box::new(lower_value(*value)),
        },
    }
}
