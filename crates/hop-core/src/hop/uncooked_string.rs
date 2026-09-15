use std::fmt;

use crate::document::{CheapString, DocumentRange};

/// The contents of a string literal as it was written, with its escape
/// sequences still spelled out raw, as backslash pairs.
#[derive(Clone)]
pub struct UncookedString(Option<DocumentRange>);

impl UncookedString {
    pub fn new(content: Option<DocumentRange>) -> Self {
        Self(content)
    }

    /// The literal as it was written, for printing it back out unchanged.
    pub fn as_raw_str(&self) -> &str {
        self.0.as_ref().map_or("", |range| range.as_str())
    }

    /// Resolves the escape sequences, producing the string the literal means.
    ///
    /// Every sequence that is not one is handed to `on_invalid_escape`, as the
    /// character following the backslash — or the backslash itself when nothing
    /// follows it — and the range of the pair. An empty CheapString is returned
    /// if there was any.
    ///
    /// Supported escape sequences:
    /// - `\n` → newline
    /// - `\t` → tab
    /// - `\r` → carriage return
    /// - `\\` → backslash
    /// - `\"` → double quote
    pub fn cook(&self, on_invalid_escape: &mut impl FnMut(char, DocumentRange)) -> CheapString {
        let Some(range) = &self.0 else {
            return CheapString::new(String::new());
        };

        if !range.as_str().contains('\\') {
            return range.to_cheap_string();
        }

        let mut result = String::with_capacity(range.as_str().len());
        let mut failed = false;
        let mut chars = range.cursor();
        while let Some(ch) = chars.next() {
            if ch.ch() != '\\' {
                result.push(ch.ch());
                continue;
            }
            let backslash = ch;
            let Some(escaped) = chars.next() else {
                on_invalid_escape('\\', backslash);
                failed = true;
                break;
            };
            match escaped.ch() {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                'r' => result.push('\r'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                other => {
                    on_invalid_escape(other, backslash.to(escaped));
                    failed = true;
                }
            }
        }
        if failed {
            CheapString::new(String::new())
        } else {
            CheapString::new(result)
        }
    }
}

/// Two literals are the same when they were written the same way, wherever
/// in the document each of them appears.
impl PartialEq for UncookedString {
    fn eq(&self, other: &Self) -> bool {
        self.as_raw_str() == other.as_raw_str()
    }
}

impl fmt::Debug for UncookedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_raw_str(), f)
    }
}

impl fmt::Display for UncookedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_raw_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentCursor, DocumentRange};
    use crate::document_id::DocumentId;

    /// Cooks `content`, the text a literal has between its quotes.
    fn cook(content: &str) -> (String, Vec<char>) {
        let cursor = DocumentCursor::new(DocumentId::new("test.hop").unwrap(), content.to_string());
        let range: Option<DocumentRange> = cursor.collect();
        let mut invalid = Vec::new();
        let cooked = UncookedString::new(range).cook(&mut |ch, _range| invalid.push(ch));
        (cooked.as_str().to_string(), invalid)
    }

    #[test]
    fn resolves_supported_escape_sequences() {
        assert_eq!(cook(r"a\nb").0, "a\nb");
        assert_eq!(cook(r"a\tb").0, "a\tb");
        assert_eq!(cook(r"a\rb").0, "a\rb");
        assert_eq!(cook(r"a\\b").0, r"a\b");
        assert_eq!(cook(r#"a\"b"#).0, "a\"b");
    }

    #[test]
    fn resolves_escapes_at_either_end() {
        assert_eq!(cook(r#"\""#).0, "\"");
        assert_eq!(cook(r"\\").0, r"\");
        assert_eq!(cook(r"C:\\Users\\name").0, r"C:\Users\name");
        assert_eq!(cook(r"foo\nbar").0, "foo\nbar");
    }

    #[test]
    fn keeps_text_without_escapes_verbatim() {
        assert_eq!(cook("plain").0, "plain");
        assert_eq!(cook("").0, "");
    }

    #[test]
    fn reports_an_unknown_escape_and_yields_no_value() {
        let (value, errors) = cook(r"a\qb");
        assert_eq!(value, "");
        assert_eq!(errors, ['q']);
    }

    #[test]
    fn reports_a_backslash_with_nothing_after_it() {
        let (value, errors) = cook(r"a\");
        assert_eq!(value, "");
        assert_eq!(errors, ['\\']);
    }

    #[test]
    fn reports_every_unknown_escape_in_one_literal() {
        let (value, errors) = cook(r"\q\x");
        assert_eq!(value, "");
        assert_eq!(errors, ['q', 'x']);
    }
}
