use crate::hop::typing::Type;
use crate::html::write_escaped_html;
use crate::ir::ir_match::{EnumMatchArm, Match};

use super::document_shell::DocumentShell;
use super::pure_module::{
    PureAttribute, PureExpr, PureForSource, PureFunctionDeclaration, PureModule,
    PurePageDeclaration,
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

/// The longest a constant write grows by merging with the write before it,
/// which keeps the string literals in generated code short.
const WRITE_LIMIT: usize = 60;

/// Append a constant write, merged into the write before it while the
/// combined length stays below the limit.
fn write(out: &mut Vec<WriterStatement>, content: &str) {
    if let Some(WriterStatement::Write { content: previous }) = out.last_mut() {
        if previous.len() + content.len() < WRITE_LIMIT {
            previous.push_str(content);
            return;
        }
    }
    out.push(WriterStatement::Write {
        content: content.to_string(),
    });
}

/// Lower a String-typed PureExpr written escaped in output position. A
/// constant is escaped now and a concat part by part, so only what varies
/// is escaped when the page renders.
fn lower_escaped(expr: PureExpr, out: &mut Vec<WriterStatement>) {
    match expr {
        PureExpr::StringLiteral { value, .. } => {
            let mut content = String::new();
            write_escaped_html(value.as_str(), &mut content);
            write(out, &content);
        }
        PureExpr::StringConcat { parts, .. } => {
            for part in parts {
                lower_escaped(part, out);
            }
        }
        expr => out.push(WriterStatement::WriteString {
            expr: lower_value(expr),
        }),
    }
}

/// Lower a Html-typed PureExpr in output position.
fn lower_output(expr: PureExpr, out: &mut Vec<WriterStatement>) {
    match expr {
        PureExpr::HtmlText { content, .. } => write(out, content.as_str()),

        PureExpr::HtmlEscape { expr, .. } => lower_escaped(*expr, out),

        PureExpr::HtmlElement {
            element,
            attributes,
            children,
            ..
        } => {
            // The element's writes merge among themselves first, so a
            // constant write grows across an element boundary only where
            // the whole element fits.
            let mut unit = Vec::new();
            write(&mut unit, &format!("<{}", element.as_str()));
            for attribute in attributes {
                match attribute {
                    PureAttribute::Value { name, value } => {
                        write(&mut unit, &format!(" {}=\"", name.as_str()));
                        lower_escaped(value, &mut unit);
                        write(&mut unit, "\"");
                    }
                    // A constant condition settles now whether the attribute
                    // renders.
                    PureAttribute::Presence { name, present } => match present {
                        PureExpr::BoolLiteral { value: true, .. } => {
                            write(&mut unit, &format!(" {}", name.as_str()));
                        }
                        PureExpr::BoolLiteral { value: false, .. } => {}
                        present => {
                            let mut true_body = Vec::new();
                            write(&mut true_body, &format!(" {}", name.as_str()));
                            unit.push(WriterStatement::Match {
                                match_: Match::Bool {
                                    subject: Box::new(lower_value(present)),
                                    true_body: Box::new(true_body),
                                    false_body: Box::new(Vec::new()),
                                },
                            });
                        }
                    },
                }
            }
            write(&mut unit, ">");
            if !element.is_void() {
                lower_output(*children, &mut unit);
                write(&mut unit, &format!("</{}>", element.as_str()));
            }
            for statement in unit {
                match statement {
                    WriterStatement::Write { content } => write(out, &content),
                    statement => out.push(statement),
                }
            }
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
        expr @ (PureExpr::HtmlText { .. }
        | PureExpr::HtmlEscape { .. }
        | PureExpr::HtmlElement { .. }
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

    fn check(module: PureModule, shell: Option<&DocumentShell>, expected: Expect) {
        let before = module.to_string();
        let after = lower_pure(module, shell).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn writes_the_shell_around_the_page() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Main", |t| t.element("p", vec![], vec![t.text("Hello")]))
                .build(),
            Some(&DocumentShell::new(None, Some("/scripts-deadbeef.js"))),
            expect![[r#"
                -- before --
                page Main() {
                  html(tag: "p", attrs: [], children: concat(text("Hello")))
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
            PureModuleBuilder::new()
                .page_no_params("Main", |t| t.element("p", vec![], vec![t.text("Hello")]))
                .build(),
            None,
            expect![[r#"
                -- before --
                page Main() {
                  html(tag: "p", attrs: [], children: concat(text("Hello")))
                }

                -- after --
                page Main() {
                  write("<p>Hello</p>")
                }
            "#]],
        );
    }

    #[test]
    fn should_write_an_element_with_constant_attributes_as_one_write() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.element(
                        "div",
                        vec![t.attr("class", t.str("base")), t.attr("id", t.str("a<b"))],
                        vec![t.text("Content")],
                    )
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test() {
                  html(
                    tag: "div",
                    attrs: [class: "base", id: "a<b"],
                    children: concat(text("Content")),
                  )
                }

                -- after --
                page Test() {
                  write("<div class=\"base\" id=\"a&lt;b\">Content</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_a_dynamic_attribute_value_when_rendering() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("cls", "String")], |t| {
                    t.element("div", vec![t.attr("data-value", t.var("cls"))], vec![])
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test(cls@v0: String) {
                  html(
                    tag: "div",
                    attrs: [data-value: v0],
                    children: concat(),
                  )
                }

                -- after --
                page Test(cls@v0: String) {
                  write("<div data-value=\"")
                  write_string(v0)
                  write("\"></div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_settle_a_constant_boolean_attribute_and_match_on_a_dynamic_one() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("flag", "Bool")], |t| {
                    t.element(
                        "input",
                        vec![
                            t.presence("disabled", t.bool(true)),
                            t.presence("checked", t.bool(false)),
                            t.presence("required", t.var("flag")),
                        ],
                        vec![],
                    )
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test(flag@v0: Bool) {
                  html(
                    tag: "input",
                    attrs: [disabled: true, checked: false, required: v0],
                  )
                }

                -- after --
                page Test(flag@v0: Bool) {
                  write("<input disabled")
                  match v0 {
                    true => {
                      write(" required")
                    }
                    false => {
                    }
                  }
                  write(">")
                }
            "#]],
        );
    }

    #[test]
    fn should_nest_elements() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("name", "String")], |t| {
                    t.element(
                        "ul",
                        vec![],
                        vec![t.element(
                            "li",
                            vec![],
                            vec![
                                t.text("Hi "),
                                t.escape(t.var("name")),
                                t.element("br", vec![], vec![]),
                            ],
                        )],
                    )
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test(name@v0: String) {
                  html(
                    tag: "ul",
                    attrs: [],
                    children: concat(
                      html(
                        tag: "li",
                        attrs: [],
                        children: concat(
                          text("Hi "),
                          escape(v0),
                          html(tag: "br", attrs: []),
                        ),
                      ),
                    ),
                  )
                }

                -- after --
                page Test(name@v0: String) {
                  write("<ul><li>Hi ")
                  write_string(v0)
                  write("<br></li></ul>")
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_a_constant_string_when_lowering() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| t.concat(vec![t.escape(t.str("<b> & \"q\""))]))
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test() {
                  concat(escape("<b> & \"q\""))
                }

                -- after --
                page Test() {
                  write("&lt;b&gt; &amp; &quot;q&quot;")
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_the_constant_parts_of_a_concat_when_lowering() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("name", "String")], |t| {
                    t.element(
                        "p",
                        vec![],
                        vec![t.escape(t.string_concat(vec![
                            t.str("Hi <"),
                            t.var("name"),
                            t.str("!"),
                        ]))],
                    )
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test(name@v0: String) {
                  html(
                    tag: "p",
                    attrs: [],
                    children: concat(escape(("Hi <" + v0 + "!"))),
                  )
                }

                -- after --
                page Test(name@v0: String) {
                  write("<p>Hi &lt;")
                  write_string(v0)
                  write("!</p>")
                }
            "#]],
        );
    }

    #[test]
    fn should_merge_adjacent_writes_while_they_stay_below_the_limit() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.concat(vec![
                        t.text(&"a".repeat(30)),
                        t.text(&"b".repeat(29)),
                        t.text(&"c".repeat(30)),
                        t.text("d"),
                    ])
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    text("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
                    text("bbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),
                    text("cccccccccccccccccccccccccccccc"),
                    text("d"),
                  )
                }

                -- after --
                page Test() {
                  write("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
                  write("ccccccccccccccccccccccccccccccd")
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_the_writes_of_a_loop_body_apart_from_those_around_it() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.element(
                        "ul",
                        vec![],
                        vec![t.html_for(Some("item"), t.array(vec![t.str("a")]), |t| {
                            t.element("li", vec![], vec![t.escape(t.var("item"))])
                        })],
                    )
                })
                .build(),
            None,
            expect![[r#"
                -- before --
                page Test() {
                  html(
                    tag: "ul",
                    attrs: [],
                    children: concat(
                      for v0 in ["a"] {
                        html(
                          tag: "li",
                          attrs: [],
                          children: concat(escape(v0)),
                        )
                      },
                    ),
                  )
                }

                -- after --
                page Test() {
                  write("<ul>")
                  for v0 in ["a"] {
                    write("<li>")
                    write_string(v0)
                    write("</li>")
                  }
                  write("</ul>")
                }
            "#]],
        );
    }
}
