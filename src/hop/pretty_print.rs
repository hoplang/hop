use std::collections::BTreeMap;

use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::Parser;
use crate::dop::parser::Parameter;
use crate::hop::module_name::ModuleName;
use crate::hop::parse_error::ParseError;
use crate::hop::parser::parse;
use crate::hop::token_tree::{TokenTree, build_tree};
use crate::hop::tokenizer::{Attribute, AttributeValue, Token, Tokenizer};
use itertools::Itertools as _;
use pretty::RcDoc;

// This file uses the `pretty` crate to perform Wadler-style
// pretty printing of token trees.
//
// The `pretty` crate has the following actions:
//
// * nil()            - empty document
// * text(...)        - verbatim string
// * line()           - line break that acts as a space when grouped on a single line.
// * line_()          - line break that acts as an empty string when grouped on a single line.
// * hardline()       - line break that always break the line.
// * group()          - group the nodes on a single line if possible, otherwise on separate lines.
// * flat_alt(...)    - make a node act as some other node when laid out on a single line.
// * intersperse(...) - add a separator between each node

pub trait TokenTreePrettyPrint {
    fn to_doc(&self) -> RcDoc<'static>;
    fn to_doc_with_context(&self, is_top_level: bool) -> RcDoc<'static>;
}

impl TokenTreePrettyPrint for TokenTree {
    fn to_doc(&self) -> RcDoc<'static> {
        self.to_doc_with_context(true)
    }

    fn to_doc_with_context(&self, is_top_level: bool) -> RcDoc<'static> {
        match &self.token {
            Token::Doctype { range } => {
                // Doctypes are currently left untouched.
                RcDoc::text(range.to_string())
            }

            Token::Comment { range } => {
                // Comments are currently left untouched.
                RcDoc::text(range.to_string())
            }

            Token::Text { range } => {
                let text = range.as_str();
                if text.trim().is_empty() {
                    // Skip whitespace-only text nodes
                    RcDoc::nil()
                } else {
                    // Otherwise, leave untouched.
                    RcDoc::text(text.to_string())
                }
            }

            Token::TextExpression { range, .. } => {
                // Text expressions are currently left untouched.
                RcDoc::text(range.to_string())
            }

            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                ..
            } => format_opening_tag(
                tag_name,
                attributes,
                expression.as_ref(),
                *self_closing,
                &self.children,
                self.closing_tag_name.is_some(),
                is_top_level,
            ),

            Token::ClosingTag { .. } => {
                // Closing tags should not be present in the token tree
                unreachable!()
            }
        }
    }
}

fn format_opening_tag(
    tag_name: &DocumentRange,
    attributes: &BTreeMap<StringSpan, Attribute>,
    expression: Option<&DocumentRange>,
    self_closing: bool,
    children: &[TokenTree],
    has_closing_tag: bool,
    is_top_level: bool,
) -> RcDoc<'static> {
    let mut doc = RcDoc::text("<").append(RcDoc::text(tag_name.as_str().to_string()));

    // Add attributes
    if !attributes.is_empty() {
        doc = doc.append(RcDoc::space());
        doc = doc.append(RcDoc::intersperse(
            attributes
                .values()
                .sorted_by(|a, b| a.range.start().cmp(&b.range.start()))
                .map(|attr| format_attribute(attr)),
            RcDoc::space(),
        ));
    }

    // Add expression (for component parameters)
    if let Some(expr) = expression {
        // Check if this is a top-level component definition
        let is_component_definition = is_top_level && tag_name.as_str().contains('-');

        if is_component_definition {
            // Parse and format as parameters for component definitions
            let params = Parser::from(expr.clone())
                .parse_parameters()
                .expect("Failed to parse component parameters during formatting");
            let param_doc = format_parameters(params);
            doc = doc
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(param_doc)
                .append(RcDoc::text("}"));
        } else {
            // For now, keep original formatting for component references
            doc = doc
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::text(expr.as_str().to_string()))
                .append(RcDoc::text("}"));
        }
    }

    doc = doc.append(if self_closing {
        RcDoc::text(" /")
    } else {
        RcDoc::nil()
    });

    doc = doc.append(RcDoc::text(">"));

    // Check if all children are inline
    let all_inline = children.iter().all(is_inline_token_tree);

    if all_inline {
        // All children are inline - keep them on same line to preserve spacing
        doc = doc.append(format_children(children, false));
    } else {
        // Has block-level children - safe to format with newlines
        doc = doc
            .append(
                RcDoc::hardline()
                    .append(format_children(children, false))
                    .nest(2),
            )
            .append(RcDoc::hardline());
    }

    // Add closing tag if present
    if has_closing_tag {
        doc = doc
            .append(RcDoc::text("</"))
            .append(RcDoc::text(tag_name.as_str().to_string()))
            .append(RcDoc::text(">"));
    }

    doc
}

/// Format the parameters of a component definition.
/// E.g. <foo-component {users: Array[{name: String}]}>
///                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fn format_parameters(params: Vec<Parameter>) -> RcDoc<'static> {
    RcDoc::nil()
        // soft line break after the initial '{'
        .append(RcDoc::line_())
        // format parameters
        .append(RcDoc::intersperse(
            params.iter().map(
                |Parameter {
                     var_name, var_type, ..
                 }| {
                    RcDoc::nil()
                        // key
                        .append(RcDoc::text(var_name.to_string()))
                        // separator
                        .append(RcDoc::text(": "))
                        // value
                        .append(var_type.to_string())
                },
            ),
            // intersperse with comma followed by line that acts
            // as space if laid out on a single line
            RcDoc::text(",").append(RcDoc::line()),
        ))
        // trailing comma if laid out on multiple lines
        .append(RcDoc::text(",").flat_alt(RcDoc::nil()))
        // soft line break before the last '{'
        .append(RcDoc::line_())
        .nest(2)
        .group()
}

/// Format an attribute.
/// E.g. <div foo="bar">
///           ^^^^^^^^^
fn format_attribute(attr: &Attribute) -> RcDoc<'static> {
    RcDoc::nil()
        // key
        .append(RcDoc::text(attr.name.to_string()))
        // equal sign
        .append(RcDoc::text("="))
        // value
        .append(match &attr.value {
            // case: normal value
            Some(AttributeValue::String(val)) => {
                RcDoc::nil()
                    // double quote
                    .append(RcDoc::text("\""))
                    // value
                    .append(RcDoc::text(val.as_str().to_string()))
                    // double quote
                    .append(RcDoc::text("\""))
            }
            // case: expression value
            Some(AttributeValue::Expression(val)) => {
                RcDoc::nil()
                    // '{'
                    .append(RcDoc::text("{"))
                    // value
                    .append(RcDoc::text(val.as_str().to_string()))
                    // '}'
                    .append(RcDoc::text("}"))
            }
            None => RcDoc::text(attr.name.as_str().to_string()),
        })
}

fn format_children(children: &[TokenTree], is_top_level: bool) -> RcDoc<'static> {
    // Filter out whitespace-only text nodes and group inline content
    let mut docs = Vec::new();
    let mut current_inline_group = Vec::new();

    for child in children {
        // Skip whitespace-only text nodes
        if is_whitespace_only_text(child) {
            continue;
        }

        if is_inline_token_tree(child) {
            current_inline_group.push(child.to_doc_with_context(is_top_level));
        } else {
            // Flush any accumulated inline content
            if !current_inline_group.is_empty() {
                docs.push(RcDoc::intersperse(
                    current_inline_group.drain(..),
                    RcDoc::nil(),
                ));
            }
            docs.push(child.to_doc_with_context(is_top_level));
        }
    }

    // Flush any remaining inline content
    if !current_inline_group.is_empty() {
        docs.push(RcDoc::intersperse(
            current_inline_group.drain(..),
            RcDoc::nil(),
        ));
    }

    RcDoc::intersperse(docs, RcDoc::hardline())
}

fn is_whitespace_only_text(tree: &TokenTree) -> bool {
    match &tree.token {
        Token::Text { range } => range.as_str().trim().is_empty(),
        _ => false,
    }
}

fn is_inline_token_tree(tree: &TokenTree) -> bool {
    match &tree.token {
        Token::Text { .. } | Token::TextExpression { .. } => true,
        Token::OpeningTag { tag_name, .. } => {
            is_inline_element(tag_name.as_str()) && tree.children.iter().all(is_inline_token_tree)
        }
        _ => false,
    }
}

fn is_inline_element(tag: &str) -> bool {
    matches!(
        tag,
        "a" | "abbr"
            | "b"
            | "bdi"
            | "bdo"
            | "br"
            | "cite"
            | "code"
            | "data"
            | "dfn"
            | "em"
            | "i"
            | "kbd"
            | "mark"
            | "q"
            | "rp"
            | "rt"
            | "ruby"
            | "s"
            | "samp"
            | "small"
            | "span"
            | "strong"
            | "sub"
            | "sup"
            | "time"
            | "u"
            | "var"
            | "wbr"
    )
}

/// Pretty print from a token tree with the specified width
pub fn pretty_print_token_tree(trees: &[TokenTree], width: usize) -> String {
    // Filter out whitespace-only trees at the top level
    let non_empty_trees: Vec<&TokenTree> = trees
        .iter()
        .filter(|tree| !is_whitespace_only_text(tree))
        .collect();

    // Build document with appropriate spacing
    let mut docs = Vec::new();
    for (i, tree) in non_empty_trees.iter().enumerate() {
        if i > 0 {
            let prev_is_import = is_import_node(non_empty_trees[i - 1]);
            let curr_is_import = is_import_node(tree);

            // Single newline between imports, double newline otherwise
            if prev_is_import && curr_is_import {
                docs.push(RcDoc::hardline());
            } else {
                docs.push(RcDoc::hardline());
                docs.push(RcDoc::hardline());
            }
        }
        docs.push(tree.to_doc());
    }

    let doc = RcDoc::concat(docs).append(RcDoc::hardline()); // Always add trailing newline
    doc.pretty(width).to_string()
}

fn is_import_node(tree: &TokenTree) -> bool {
    match &tree.token {
        Token::OpeningTag { tag_name, .. } => tag_name.as_str() == "import",
        _ => false,
    }
}

/// Pretty print from source code directly using token trees
pub fn pretty_print_from_source(source: &str, width: usize) -> Result<String, Vec<ParseError>> {
    use crate::error_collector::ErrorCollector;

    let mut errors = ErrorCollector::new();

    // First, run the full parser to validate the syntax
    let module_name = ModuleName::new("formatter".to_string()).unwrap();
    let tokenizer = Tokenizer::new(source.to_string());
    let _ = parse(module_name, tokenizer, &mut errors);

    // If parsing found errors, don't format
    if !errors.is_empty() {
        return Err(errors.to_vec());
    }

    // Now build the token tree for formatting (parse again for clean state)
    let mut format_errors = ErrorCollector::new();
    let trees = build_tree(Tokenizer::new(source.to_string()), &mut format_errors);

    // This should not happen since we already validated, but check anyway
    if !format_errors.is_empty() {
        return Err(format_errors.to_vec());
    }

    Ok(pretty_print_token_tree(&trees, width))
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_pretty_print(input: &str, expected: Expect) {
        match pretty_print_from_source(input, 80) {
            Ok(output) => expected.assert_eq(&output),
            Err(errors) => panic!("Parse errors: {:?}", errors),
        }
    }

    /// HTML nodes should get consistent indentation.
    #[test]
    fn test_format_adds_consistent_indentation() {
        check_pretty_print(
            indoc! {r#"
                <main-component>
                        <div>
                    <p>First paragraph</p>
                            <p>Second paragraph</p>
                                </div>
                    <ul>
                <li>Item 1</li>
                    <li>Item 2</li>
                        </ul>
                </main-component>
            "#},
            expect![[r#"
                <main-component>
                  <div>
                    <p>First paragraph</p>
                    <p>Second paragraph</p>
                  </div>
                  <ul>
                    <li>Item 1</li>
                    <li>Item 2</li>
                  </ul>
                </main-component>
            "#]],
        );
    }

    /// Attributes with inconsistent spacing should be normalized
    #[test]
    fn test_format_normalizes_attribute_spacing() {
        check_pretty_print(
            indoc! {r#"
                <main-component>
                  <div   id="test"    class="foo">
                    <p>Hello world</p>
                  </div>
                </main-component>
            "#},
            expect![[r#"
                <main-component>
                  <div id="test" class="foo">
                    <p>Hello world</p>
                  </div>
                </main-component>
            "#]],
        );
    }

    /// Comments should be left untouched for now.
    #[test]
    fn test_format_leaves_comments_untouched() {
        check_pretty_print(
            indoc! {r#"
                <main-component>
                    <!-- this is my single-line comment -->
                    <!--
                        this is my multi-line comment
                    -->
                </main-component>
            "#},
            expect![[r#"
                <main-component>
                  <!-- this is my single-line comment -->
                  <!--
                        this is my multi-line comment
                    -->
                </main-component>
            "#]],
        );
    }

    /// Text expressions should be left untouched for now.
    #[test]
    fn test_format_leaves_text_expressions_untouched() {
        check_pretty_print(
            indoc! {r#"
                <main-component>
                    {x   ==    2}
                </main-component>
            "#},
            expect![[r#"
                <main-component>{x   ==    2}</main-component>
            "#]],
        );
    }

    /// Import statements should be grouped and there should be two line
    /// breaks between the import statements and the components.
    #[test]
    fn test_multiple_imports_grouped() {
        check_pretty_print(
            indoc! {r#"
                <import from="@/hop/components/nav" component="nav-bar">
                <import from="@/hop/components/footer" component="footer-bar">
                <import from="@/hop/components/header" component="header-bar">
                <main-component>
                  <p>Content</p>
                </main-component>
            "#},
            expect![[r#"
                <import from="@/hop/components/nav" component="nav-bar">
                <import from="@/hop/components/footer" component="footer-bar">
                <import from="@/hop/components/header" component="header-bar">

                <main-component>
                  <p>Content</p>
                </main-component>
            "#]],
        );
    }
    /// Self-closing tags should keep their slash
    #[test]
    fn test_format_self_closing_tag() {
        check_pretty_print(
            indoc! {r#"
                <foo-component>
                  <bar-component />
                </foo-component>
            "#},
            expect![[r#"
                <foo-component>
                  <bar-component />
                </foo-component>
            "#]],
        );
    }

    /// A div with many attributes should keep them on one line
    #[test]
    fn test_format_div_with_many_attributes() {
        check_pretty_print(
            indoc! {r#"
                <main-component>
                  <div id="container" class="wrapper main-content" data-role="content" data-index="1" aria-label="Main content area">
                    <p>Hello world</p>
                  </div>
                </main-component>
            "#},
            expect![[r#"
                <main-component>
                  <div id="container" class="wrapper main-content" data-role="content" data-index="1" aria-label="Main content area">
                    <p>Hello world</p>
                  </div>
                </main-component>
            "#]],
        );
    }

    /// Whitespace should be added between keys and values in parameter lists
    #[test]
    fn test_format_long_parameter_list_gets_whitespace() {
        check_pretty_print(
            indoc! {r#"
                <foo-component {users:Array[{name:String}]}>
                  <div>Hello</div>
                </foo-component>
            "#},
            expect![[r#"
                <foo-component {users: Array[{name: String}]}>
                  <div>Hello</div>
                </foo-component>
            "#]],
        );
    }

    /// In parameter lists, both parameters and the object properties
    /// should have a trailing comma added if broken over multiple lines.
    #[test]
    fn test_format_long_parameter_list_has_trailing_comma() {
        check_pretty_print(
            indoc! {r#"
                <foo-component {users: Array[{name: String}], admins: Array[{name: String, email: String}], others: Array[{name: String, email: String, foo: String, bar: String, baz: String}]}>
                    <div>
                        <h1>User List</h1>
                        <for {user in users}>
                            <user-card {user: user}/>
                        </for>
                    </div>
                </foo-component>
            "#},
            expect![[r#"
                <foo-component {
                  users: Array[{name: String}],
                  admins: Array[{email: String, name: String}],
                  others: Array[{
                  bar: String,
                  baz: String,
                  email: String,
                  foo: String,
                  name: String,
                }],
                }>
                  <div>
                    <h1>User List</h1>
                    <for {user in users}>
                      <user-card {user: user} />
                    </for>
                  </div>
                </foo-component>
            "#]],
        );
    }
}
