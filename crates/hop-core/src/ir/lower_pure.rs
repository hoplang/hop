use crate::hop::typing::Type;
use crate::ir::ir_match::{EnumMatchArm, Match};

use super::document_shell::DocumentShell;
use super::pure_module::{
    PureExpr, PureForSource, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use super::writer_module::{
    WriterArgument, WriterExpr, WriterForSource, WriterFunctionBody, WriterFunctionDeclaration,
    WriterModule, WriterPageDeclaration, WriterStatement,
};

/// Lower a whole PureModule into a WriterModule.
///
/// With a shell, a page writes a whole document, the shell around its head
/// and its body. Without one, a page writes its head and its body alone.
pub fn lower_pure(module: PureModule, shell: Option<&DocumentShell>) -> WriterModule {
    WriterModule {
        pages: module
            .pages
            .into_iter()
            .map(|page| lower_page(page, shell))
            .collect(),
        functions: module.functions.into_iter().map(lower_function).collect(),
        var_ids: module.var_ids,
    }
}

fn lower_page(decl: PurePageDeclaration, shell: Option<&DocumentShell>) -> WriterPageDeclaration {
    let mut body = Vec::new();
    match shell {
        Some(shell) => {
            body.push(WriterStatement::Write {
                content: shell.before_head.to_string(),
            });
            lower_output(decl.head, &mut body);
            body.push(WriterStatement::Write {
                content: shell.after_head.clone(),
            });
            lower_output(decl.body, &mut body);
            body.push(WriterStatement::Write {
                content: shell.after_body.to_string(),
            });
        }
        None => {
            lower_output(decl.head, &mut body);
            lower_output(decl.body, &mut body);
        }
    }
    WriterPageDeclaration {
        name: decl.name,
        parameters: decl.parameters,
        body,
    }
}

/// Lower a function declaration, choosing the calling convention from its
/// return type. Html compiles to destination-passing, everything else
/// compiles as an ordinary value-returning function.
fn lower_function(decl: PureFunctionDeclaration) -> WriterFunctionDeclaration {
    let body = if matches!(decl.return_type, Type::Html) {
        let mut statements = Vec::new();
        lower_output(decl.body, &mut statements);
        WriterFunctionBody::Writes(statements)
    } else {
        WriterFunctionBody::Returns(lower_value(decl.body))
    };
    WriterFunctionDeclaration {
        function: decl.function,
        parameters: decl.parameters,
        return_type: decl.return_type,
        body,
    }
}

/// Lower a Html-typed PureExpr in output position.
fn lower_output(expr: PureExpr, out: &mut Vec<WriterStatement>) {
    match expr {
        PureExpr::HtmlRaw { content, .. } => {
            out.push(WriterStatement::Write { content });
        }

        PureExpr::HtmlEscape { expr, .. } => {
            let expr = lower_value(*expr);
            out.push(WriterStatement::WriteString { expr });
        }

        PureExpr::HtmlConcat { parts, .. } => {
            for part in parts {
                lower_output(part, out);
            }
        }

        PureExpr::HtmlFor {
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

        PureExpr::Call {
            function,
            args,
            typ,
            ..
        } => {
            assert!(
                matches!(typ, Type::Html),
                "non-Html function call in output position: {}",
                function
            );
            let args = args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect();
            out.push(WriterStatement::WriteFunction { function, args });
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

        PureExpr::VariableReference { ref typ, .. }
        | PureExpr::FieldAccess { ref typ, .. }
        | PureExpr::TupleIndex { ref typ, .. } => {
            assert!(
                matches!(*typ, Type::Html),
                "non-Html expression in output position: {:?}",
                expr
            );
            let expr = lower_value(expr);
            out.push(WriterStatement::WriteHtml { expr });
        }

        PureExpr::StringLiteral { .. }
        | PureExpr::BoolLiteral { .. }
        | PureExpr::FloatLiteral { .. }
        | PureExpr::IntLiteral { .. }
        | PureExpr::Array { .. }
        | PureExpr::Tuple { .. }
        | PureExpr::Record { .. }
        | PureExpr::Enum { .. }
        | PureExpr::Option { .. }
        | PureExpr::StringConcat { .. }
        | PureExpr::NumericAdd { .. }
        | PureExpr::NumericSubtract { .. }
        | PureExpr::NumericMultiply { .. }
        | PureExpr::NumericNegation { .. }
        | PureExpr::BoolNegation { .. }
        | PureExpr::BoolLogicalAnd { .. }
        | PureExpr::BoolLogicalOr { .. }
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
            panic!("non-Html-typed expression in output position: {:?}", expr);
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
    match_: Match<PureExpr, PureExpr>,
) -> Match<WriterExpr, Vec<WriterStatement>> {
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

fn lower_match_value(match_: Match<PureExpr, PureExpr>) -> Match<WriterExpr, WriterExpr> {
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
        expr @ (PureExpr::HtmlRaw { .. }
        | PureExpr::HtmlEscape { .. }
        | PureExpr::HtmlConcat { .. }
        | PureExpr::HtmlFor { .. }) => {
            let mut body = Vec::new();
            lower_output(expr, &mut body);
            WriterExpr::HtmlLiteral { body }
        }

        PureExpr::Call {
            function,
            args,
            typ: Type::Html,
            ..
        } => {
            let args = args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect();
            WriterExpr::HtmlLiteral {
                body: vec![WriterStatement::WriteFunction { function, args }],
            }
        }

        PureExpr::Call {
            function,
            args,
            typ,
            ..
        } => WriterExpr::Call {
            function,
            args: args
                .into_iter()
                .map(|arg| WriterArgument {
                    name: arg.name,
                    expr: lower_value(arg.expr),
                })
                .collect(),
            typ,
        },

        PureExpr::Let {
            var,
            value,
            body,
            typ,
            ..
        } => WriterExpr::Let {
            var,
            value: Box::new(lower_value(*value)),
            body: Box::new(lower_value(*body)),
            typ,
        },

        PureExpr::Match { match_, typ, .. } => WriterExpr::Match {
            match_: lower_match_value(match_),
            typ,
        },

        PureExpr::VariableReference { value, typ, .. } => {
            WriterExpr::VariableReference { value, typ }
        }

        PureExpr::FieldAccess {
            record, field, typ, ..
        } => WriterExpr::FieldAccess {
            record: Box::new(lower_value(*record)),
            field,
            typ,
        },

        PureExpr::StringLiteral { value, .. } => WriterExpr::StringLiteral { value },

        PureExpr::BoolLiteral { value, .. } => WriterExpr::BoolLiteral { value },

        PureExpr::FloatLiteral { value, .. } => WriterExpr::FloatLiteral { value },

        PureExpr::IntLiteral { value, .. } => WriterExpr::IntLiteral { value },

        PureExpr::Array { elements, typ, .. } => WriterExpr::Array {
            elements: elements.into_iter().map(lower_value).collect(),
            typ,
        },

        PureExpr::Tuple { elements, typ, .. } => WriterExpr::Tuple {
            elements: elements.into_iter().map(lower_value).collect(),
            typ,
        },

        PureExpr::TupleIndex {
            tuple, index, typ, ..
        } => WriterExpr::TupleIndex {
            tuple: Box::new(lower_value(*tuple)),
            index,
            typ,
        },

        PureExpr::Record {
            type_name,
            fields,
            typ,
            ..
        } => WriterExpr::Record {
            type_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_value(value)))
                .collect(),
            typ,
        },

        PureExpr::Enum {
            type_name,
            variant_name,
            fields,
            typ,
            ..
        } => WriterExpr::Enum {
            type_name,
            variant_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_value(value)))
                .collect(),
            typ,
        },

        PureExpr::Option { value, typ, .. } => WriterExpr::Option {
            value: value.map(|v| Box::new(lower_value(*v))),
            typ,
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

        PureExpr::BoolNegation { operand, .. } => WriterExpr::BoolNegation {
            operand: Box::new(lower_value(*operand)),
        },

        PureExpr::BoolLogicalAnd { left, right, .. } => WriterExpr::BoolLogicalAnd {
            left: Box::new(lower_value(*left)),
            right: Box::new(lower_value(*right)),
        },

        PureExpr::BoolLogicalOr { left, right, .. } => WriterExpr::BoolLogicalOr {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use expect_test::{Expect, expect};

    fn check(shell: Option<&DocumentShell>, expected: Expect) {
        let module = PureModuleBuilder::new()
            .page_no_params("Main", |t| t.raw("<p>Hello</p>"))
            .build();
        let before = module.to_string();
        let after = lower_pure(module, shell).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn writes_the_shell_around_the_page() {
        check(
            Some(&DocumentShell::new(None, Some("/scripts-deadbeef.js"))),
            expect![[r#"
                -- before --
                page Main() {
                  raw("<p>Hello</p>")
                }

                -- after --
                page Main() {
                  write("<!doctype html><html><head><meta charset=\"utf-8\"><meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">")
                  write("<script type=\"module\" src=\"/scripts-deadbeef.js\"></script></head><body>")
                  write("<p>Hello</p>")
                  write("</body></html>")
                }
            "#]],
        );
    }

    #[test]
    fn writes_the_page_alone_without_a_shell() {
        check(
            None,
            expect![[r#"
            -- before --
            page Main() {
              raw("<p>Hello</p>")
            }

            -- after --
            page Main() {
              write("<p>Hello</p>")
            }
        "#]],
        );
    }
}
