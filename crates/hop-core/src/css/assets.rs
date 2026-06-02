use std::iter::Peekable;
use std::sync::Arc;

use crate::asset_reference::AssetReference;
use crate::asset_rewriter::AssetRewriter;
use crate::css::error::CssError;
use crate::document::{Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;

/// Lex `--asset(...)` calls out of a `Document`.
pub fn scan_for_asset_references(
    document: &Document,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<CssError>,
) {
    let mut iter = document.cursor().peekable();
    let mut prev_was_ident = false;

    while let Some(ch_range) = iter.next() {
        let ch = ch_range.ch();

        // Check for `--asset(` at word boundary
        if ch == '-' && !prev_was_ident {
            if let Some(marker_range) = try_match_asset_marker(&mut iter, ch_range) {
                // Parse the argument and closing `)`
                match parse_argument(&mut iter, marker_range.clone()) {
                    ArgumentParseResult::StringLiteral { path, close_paren } => {
                        if !path.starts_with("/") {
                            errors.push(CssError::UnclosedAssetCall {
                                range: marker_range.to(close_paren),
                            })
                        } else {
                            let document_id =
                                DocumentId::new(path.trim_start_matches('/')).unwrap();
                            asset_references.push(AssetReference {
                                range: marker_range.to(close_paren),
                                document_id,
                            });
                        }
                    }
                    ArgumentParseResult::Error { kind, last_range } => {
                        let error = match kind {
                            ArgumentErrorKind::NonStringLiteral { argument } => {
                                CssError::NonStringLiteralArgument {
                                    range: marker_range.to(last_range),
                                    argument,
                                }
                            }
                            ArgumentErrorKind::Unclosed => CssError::UnclosedAssetCall {
                                range: marker_range.to(last_range),
                            },
                        };
                        errors.push(error);
                    }
                }
                // Continue scanning after the call
            } else {
                prev_was_ident = is_ident_char(ch);
                continue;
            }
        }

        prev_was_ident = is_ident_char(ch);
    }
}

/// Returns true if `ch` is a CSS identifier character or hyphen/underscore.
fn is_ident_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '-' || ch == '_'
}

/// Returns true if `ch` is CSS whitespace.
fn is_css_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\r' | '\n' | '\x0C')
}

/// Try to match `--asset(` at the current position.
/// `first_dash` is the already-consumed first `-` of `--asset(`.
/// On success, returns a `DocumentRange` covering `--asset(` and advances the iterator.
/// On failure, returns `None` and does NOT advance the iterator.
fn try_match_asset_marker(
    iter: &mut Peekable<DocumentCursor>,
    first_dash: DocumentRange,
) -> Option<DocumentRange> {
    let mut peek = iter.clone();

    peek.next_if(|s| s.ch() == '-')?;
    peek.next_if(|s| s.ch() == 'a')?;
    peek.next_if(|s| s.ch() == 's')?;
    peek.next_if(|s| s.ch() == 's')?;
    peek.next_if(|s| s.ch() == 'e')?;
    peek.next_if(|s| s.ch() == 't')?;
    let lparen = peek.next_if(|s| s.ch() == '(')?;

    // Match confirmed, advance the real iterator
    iter.next(); // second_dash
    iter.next(); // a
    iter.next(); // s1
    iter.next(); // s2
    iter.next(); // e
    iter.next(); // t
    iter.next(); // lparen

    Some(first_dash.to(lparen))
}

#[derive(Debug)]
enum ArgumentErrorKind {
    NonStringLiteral { argument: String },
    Unclosed,
}

#[derive(Debug)]
enum ArgumentParseResult {
    StringLiteral {
        path: String,
        /// The `)` that closes the call.
        close_paren: DocumentRange,
    },
    Error {
        kind: ArgumentErrorKind,
        /// The last consumed range, used to build the error range.
        last_range: DocumentRange,
    },
}

/// Parse the argument and closing `)` of a `--asset(...)` call.
/// The iterator is positioned at the first byte after `--asset(`.
/// `marker_range` covers `--asset(`, used as fallback for error ranges on EOF.
fn parse_argument(
    iter: &mut Peekable<DocumentCursor>,
    marker_range: DocumentRange,
) -> ArgumentParseResult {
    // Skip leading whitespace
    skip_css_whitespace(iter);

    let Some(first) = iter.next() else {
        // EOF after `--asset(`
        return ArgumentParseResult::Error {
            kind: ArgumentErrorKind::Unclosed,
            last_range: marker_range,
        };
    };

    match first.ch() {
        '"' | '\'' => parse_quoted_argument(iter, first),
        _ => {
            // Non-string-literal argument: collect raw text and skip to matching `)`
            let first_clone = first.clone();
            let collected = collect_raw_from_cursor(iter, Some(first));
            let end_range = collected
                .close_paren
                .clone()
                .unwrap_or_else(|| iter.peek().cloned().unwrap_or(first_clone));

            ArgumentParseResult::Error {
                kind: if collected.close_paren.is_some() {
                    ArgumentErrorKind::NonStringLiteral {
                        argument: collected.text,
                    }
                } else {
                    ArgumentErrorKind::Unclosed
                },
                last_range: end_range,
            }
        }
    }
}

/// Parse a quoted string argument. `open_quote` is the opening `"` or `'` range.
fn parse_quoted_argument(
    iter: &mut Peekable<DocumentCursor>,
    open_quote: DocumentRange,
) -> ArgumentParseResult {
    let quote = open_quote.ch();
    let mut content_range: Option<DocumentRange> = None;
    let mut last_consumed = open_quote.clone();

    loop {
        match iter.next() {
            Some(r) if r.ch() == '\n' => {
                // Newline inside unescaped string → UnclosedAssetCall
                return ArgumentParseResult::Error {
                    kind: ArgumentErrorKind::Unclosed,
                    last_range: r,
                };
            }
            Some(r) if r.ch() == quote => {
                // Found closing quote
                last_consumed = r.clone();
                // Skip trailing whitespace and expect `)`
                skip_css_whitespace(iter);

                let path = match content_range {
                    Some(range) => range.to_cheap_string().to_string(),
                    None => String::new(),
                };

                match iter.next() {
                    Some(close_paren) if close_paren.ch() == ')' => {
                        return ArgumentParseResult::StringLiteral { path, close_paren };
                    }
                    Some(other) => {
                        // Something after the quoted string that isn't `)`, multi-arg or malformed
                        let other_clone = other.clone();
                        let collected = collect_raw_from_cursor(iter, Some(other));
                        let content_str = path.to_string();
                        let full_arg =
                            format!("{}{}{}{}", quote, content_str, quote, collected.text);
                        let last_range = collected
                            .close_paren
                            .unwrap_or_else(|| iter.peek().cloned().unwrap_or(other_clone));
                        return ArgumentParseResult::Error {
                            kind: ArgumentErrorKind::NonStringLiteral {
                                argument: full_arg.trim().to_string(),
                            },
                            last_range,
                        };
                    }
                    None => {
                        return ArgumentParseResult::Error {
                            kind: ArgumentErrorKind::Unclosed,
                            last_range: last_consumed,
                        };
                    }
                }
            }
            Some(r) if r.ch() == '\\' => {
                // Escape sequence, include backslash
                content_range = extend_content_range(content_range, r.clone());
                last_consumed = r;
                // Consume next char if available
                if let Some(next) = iter.next() {
                    content_range = extend_content_range(content_range, next.clone());
                    last_consumed = next;
                }
            }
            Some(r) => {
                content_range = extend_content_range(content_range, r.clone());
                last_consumed = r;
            }
            None => {
                // Unterminated string
                return ArgumentParseResult::Error {
                    kind: ArgumentErrorKind::Unclosed,
                    last_range: last_consumed,
                };
            }
        }
    }
}

/// Result of collecting raw argument text until a matching `)`.
struct RawCollectResult {
    /// The collected text (without the final `)`).
    text: String,
    /// The `)` range if found, None if EOF was reached.
    close_paren: Option<DocumentRange>,
}

/// Collect raw argument text from the cursor, starting from the given first range
/// (or the current position if None), scanning until matching `)` or EOF.
fn collect_raw_from_cursor(
    iter: &mut Peekable<DocumentCursor>,
    first: Option<DocumentRange>,
) -> RawCollectResult {
    let mut raw = String::new();
    match first {
        Some(r) => {
            if r.ch() == ')' {
                return RawCollectResult {
                    text: String::new(),
                    close_paren: Some(r),
                };
            }
            raw.push(r.ch());
        }
        None => {
            // No first char; start collecting from the iterator
            match iter.next() {
                Some(r) => {
                    if r.ch() == ')' {
                        return RawCollectResult {
                            text: String::new(),
                            close_paren: Some(r),
                        };
                    }
                    raw.push(r.ch());
                }
                None => {
                    return RawCollectResult {
                        text: String::new(),
                        close_paren: None,
                    };
                }
            }
        }
    }

    let mut depth = 1u32;
    for r in iter.by_ref() {
        match r.ch() {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return RawCollectResult {
                        text: raw,
                        close_paren: Some(r),
                    };
                }
            }
            _ => {}
        }
        raw.push(r.ch());
    }
    RawCollectResult {
        text: raw,
        close_paren: None,
    }
}

/// Advance the iterator past CSS whitespace characters.
fn skip_css_whitespace(iter: &mut Peekable<DocumentCursor>) {
    while iter.peek().is_some_and(|s| is_css_whitespace(s.ch())) {
        iter.next();
    }
}

/// Extend an optional content range with a new range.
fn extend_content_range(
    current: Option<DocumentRange>,
    next: DocumentRange,
) -> Option<DocumentRange> {
    Some(match current {
        Some(range) => range.to(next),
        None => next,
    })
}

/// Replace each asset reference in document using a function
/// and return the new CSS string.
pub fn rewrite_asset_paths(css: &Document, asset_rewriter: Arc<dyn AssetRewriter>) -> String {
    let mut errors = Vec::new();
    let mut asset_references = Vec::new();
    scan_for_asset_references(css, &mut asset_references, &mut errors);

    let source = css.as_str();
    let mut output = String::with_capacity(source.len());
    let mut pos = 0;

    for asset_ref in asset_references {
        let start = asset_ref.range.start();
        let end = asset_ref.range.end();

        // Push text before this asset call
        output.push_str(&source[pos..start]);

        // Push replacement
        output.push_str("url(\"");
        output.push_str(&asset_rewriter.rewrite(&asset_ref.document_id));
        output.push_str("\")");

        pos = end;
    }

    // Push remaining text after the last asset call
    output.push_str(&source[pos..]);

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_id::DocumentId;
    use crate::{document_annotator::DocumentAnnotator, simple_annotation::SimpleAnnotation};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let doc = Document::new(DocumentId::new("input.css").unwrap(), input.to_string());
        let mut asset_references = Vec::new();
        let mut errors = Vec::new();
        scan_for_asset_references(&doc, &mut asset_references, &mut errors);

        let mut asset_reference_annotations = Vec::new();
        let mut error_annotations = Vec::new();

        for asset_reference in asset_references {
            asset_reference_annotations.push(SimpleAnnotation {
                range: asset_reference.range.clone(),
                message: format!("asset: {}", asset_reference.document_id),
            });
        }

        for err in errors {
            error_annotations.push(SimpleAnnotation {
                range: err.range().clone(),
                message: err.message(),
            });
        }

        let mut output = String::new();

        if !asset_reference_annotations.is_empty() {
            output.push_str("-- calls --\n");
            output.push_str(
                &DocumentAnnotator::new()
                    .without_line_numbers()
                    .annotate(
                        &DocumentId::new("input.css").unwrap(),
                        asset_reference_annotations,
                    )
                    .render(),
            );
        }
        if !error_annotations.is_empty() {
            if !output.is_empty() {
                output.push('\n');
            }
            output.push_str("-- errors --\n");
            output.push_str(
                &DocumentAnnotator::new()
                    .without_line_numbers()
                    .annotate(&DocumentId::new("input.css").unwrap(), error_annotations)
                    .render(),
            );
        }

        expected.assert_eq(&output);
    }

    #[test]
    fn scan_single_asset_call() {
        check(
            indoc! {r#"
                @font-face {
                  src: --asset("/fonts/JetBrainsMono.woff2") format("woff2");
                }
            "#},
            expect![[r#"
                -- calls --
                asset: fonts/JetBrainsMono.woff2
                  src: --asset("/fonts/JetBrainsMono.woff2") format("woff2");
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_multiple_asset_calls() {
        check(
            indoc! {r#"
                @font-face {
                  src: --asset("/fonts/A.woff2") format("woff2");
                }
                @font-face {
                  src: --asset("/fonts/B.woff2") format("woff2");
                }
            "#},
            expect![[r#"
                -- calls --
                asset: fonts/A.woff2
                  src: --asset("/fonts/A.woff2") format("woff2");
                       ^^^^^^^^^^^^^^^^^^^^^^^^^

                asset: fonts/B.woff2
                  src: --asset("/fonts/B.woff2") format("woff2");
                       ^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_single_quoted_path() {
        check(
            indoc! {"--asset('/img/logo.png')"},
            expect![[r#"
                -- calls --
                asset: img/logo.png
                --asset('/img/logo.png')
                ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_with_whitespace_inside_parens() {
        check(
            indoc! {"--asset(  \"/fonts/A.woff2\"  )"},
            expect![[r#"
                -- calls --
                asset: fonts/A.woff2
                --asset(  "/fonts/A.woff2"  )
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_does_not_match_similar_functions() {
        check(indoc! {"background: --asseted('/x.png')"}, expect![[""]]);
    }

    #[test]
    fn scan_does_not_match_prefixed() {
        check(indoc! {"background: not--asset('/x.png')"}, expect![[""]]);
    }

    #[test]
    fn scan_non_string_literal_var() {
        check(
            indoc! {"--asset(var(--x))"},
            expect![[r#"
                -- errors --
                CSS `--asset()` call has a non-string-literal argument: `var(--x)`
                --asset(var(--x))
                ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_non_string_literal_unquoted_path() {
        check(
            indoc! {"--asset(/path)"},
            expect![[r#"
                -- errors --
                CSS `--asset()` call has a non-string-literal argument: `/path`
                --asset(/path)
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_non_string_literal_empty_args() {
        check(
            indoc! {"--asset()"},
            expect![[r#"
                -- errors --
                CSS `--asset()` call has a non-string-literal argument: ``
                --asset()
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_non_string_literal_multiple_args() {
        check(
            indoc! {r#"--asset("a", "b")"#},
            expect![[r#"
                -- errors --
                CSS `--asset()` call has a non-string-literal argument: `"a", "b"`
                --asset("a", "b")
                ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_unclosed_string() {
        check(
            indoc! {r#"--asset("foo"#},
            expect![[r#"
                -- errors --
                CSS `--asset()` call was not properly closed
                --asset("foo
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_unclosed_paren() {
        check(
            indoc! {"--asset("},
            expect![[r#"
                -- errors --
                CSS `--asset()` call was not properly closed
                --asset(
                ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_continues_past_errors() {
        check(
            indoc! {r#"--asset(bad) and --asset("/good.svg")"#},
            expect![[r#"
                -- calls --
                asset: good.svg
                --asset(bad) and --asset("/good.svg")
                                 ^^^^^^^^^^^^^^^^^^^^

                -- errors --
                CSS `--asset()` call has a non-string-literal argument: `bad`
                --asset(bad) and --asset("/good.svg")
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn scan_handles_newline_in_string_as_unclosed() {
        check(
            "--asset(\"foo\nbar\")",
            expect![[r#"
                -- errors --
                CSS `--asset()` call was not properly closed
                --asset("foo
                ^^^^^^^^^^^^
                bar")
            "#]],
        );
    }
}
