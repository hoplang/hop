use std::{fmt, iter::FromIterator, sync::Arc};

use super::Position;

/// Holds source text and precomputed line start offsets for
/// efficient position lookups.
#[derive(Debug, Clone)]
struct SourceInfo {
    /// The source text.
    text: String,
    /// Byte offsets where each line starts.
    /// First line always starts at 0.
    line_starts: Vec<usize>,
}

#[derive(Clone)]
pub struct StringCursor {
    /// The source info containing text and line starts.
    source: Arc<SourceInfo>,
    /// The current byte offset in the source string.
    offset: usize,
    /// The byte offset where iteration should stop (exclusive).
    end: usize,
}

impl StringCursor {
    pub fn new(source: String) -> Self {
        let end = source.len();
        Self {
            offset: 0,
            end,
            source: Arc::new(SourceInfo::new(source)),
        }
    }
}

impl Iterator for StringCursor {
    type Item = StringSpan;
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.end {
            return None;
        }
        let start_offset = self.offset;
        self.source.text[self.offset..self.end]
            .chars()
            .next()
            .map(|ch| {
                self.offset += ch.len_utf8();
                StringSpan {
                    source: self.source.clone(),
                    start: start_offset,
                    end: self.offset,
                }
            })
    }
}

/// A StringSpan represents a segment of text in a document.
///
/// It is always non-empty, i.e. span.start < span.end and span.as_str().len() > 0
/// always holds.
#[derive(Debug, Clone)]
pub struct StringSpan {
    /// The source info containing the document text and line starts.
    source: Arc<SourceInfo>,
    /// the start byte offset for this string span in the document (inclusive).
    start: usize,
    /// the end byte offset for this string span in the document (exclusive).
    end: usize,
}

impl StringSpan {
    /// Get the first char from the string span.
    pub fn ch(&self) -> char {
        self.source.text[self.start..].chars().next().unwrap()
    }

    /// Extend a string span to encompass another string span that occurs
    /// later in the document.
    pub fn to(self, other: StringSpan) -> Self {
        debug_assert!(other.start > self.start);
        debug_assert!(other.end > self.end);
        StringSpan {
            source: self.source,
            start: self.start,
            end: other.end,
        }
    }

    /// Extend a StringSpan with an iterator of StringSpans
    /// producing a single string span.
    ///
    /// The string spans must occur sequentially in the document.
    pub fn extend<I>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = StringSpan>,
    {
        iter.into_iter().fold(self, |acc, span| acc.to(span))
    }

    /// Get the underlying string slice for this string span.
    pub fn as_str(&self) -> &str {
        &self.source.text[self.start..self.end]
    }

    /// Get the full source text that this span is a part of.
    pub(super) fn full_source(&self) -> &str {
        &self.source.text
    }

    /// Get a string cursor for this string slice.
    pub fn cursor(&self) -> StringCursor {
        StringCursor {
            source: self.source.clone(),
            offset: self.start,
            end: self.end,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn start_utf16(&self) -> Position {
        self.source.offset_to_utf16_position(self.start)
    }

    pub fn end_utf16(&self) -> Position {
        self.source.offset_to_utf16_position(self.end)
    }

    pub fn start_utf32(&self) -> Position {
        self.source.offset_to_utf32_position(self.start)
    }

    pub fn end_utf32(&self) -> Position {
        self.source.offset_to_utf32_position(self.end)
    }

    pub fn contains(&self, other: &StringSpan) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    pub fn intersection(&self, other: &StringSpan) -> Option<StringSpan> {
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start < end {
            Some(StringSpan {
                source: self.source.clone(),
                start,
                end,
            })
        } else {
            None
        }
    }
}

impl fmt::Display for StringSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Turn an iterator of StringSpans into a single
/// Some(StringSpan) or None if the iterator is empty.
///
/// The string spans must occur sequentially in the document.
impl FromIterator<StringSpan> for Option<StringSpan> {
    fn from_iter<I: IntoIterator<Item = StringSpan>>(iter: I) -> Self {
        iter.into_iter().reduce(|acc, span| acc.to(span))
    }
}

pub trait Spanned {
    fn span(&self) -> &StringSpan;

    fn start_utf16(&self) -> Position {
        self.span().start_utf16()
    }

    fn end_utf16(&self) -> Position {
        self.span().end_utf16()
    }

    fn start_utf32(&self) -> Position {
        self.span().start_utf32()
    }

    fn end_utf32(&self) -> Position {
        self.span().end_utf32()
    }

    /// Returns true if this spanned item contains the given Position.
    fn contains_position(&self, position: Position) -> bool {
        match position {
            Position::Utf16 { .. } => {
                let start = self.start_utf16();
                let end = self.end_utf16();
                start <= position && position < end
            }
            Position::Utf32 { .. } => {
                let start = self.start_utf32();
                let end = self.end_utf32();
                start <= position && position < end
            }
        }
    }
}

impl Spanned for StringSpan {
    fn span(&self) -> &StringSpan {
        self
    }
}

impl<T: Spanned> Spanned for &T {
    fn span(&self) -> &StringSpan {
        (*self).span()
    }
}

impl SourceInfo {
    fn new(text: String) -> Self {
        let mut line_starts = vec![0];
        for (i, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(i + ch.len_utf8());
            }
        }
        Self { text, line_starts }
    }

    /// Convert a byte offset to a UTF-16 position (line, column).
    fn offset_to_utf16_position(&self, offset: usize) -> Position {
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        // Calculate UTF-16 column offset from the start of the line
        let line_start_byte = self.line_starts[line_idx];
        let line_text = &self.text[line_start_byte..offset];
        let utf16_column: usize = line_text.chars().map(|ch| ch.len_utf16()).sum();

        Position::Utf16 {
            line: line_idx,
            column: utf16_column,
        }
    }

    /// Convert a byte offset to a UTF-32 position (line, column).
    /// UTF-32 column is the character count from the start of the line.
    fn offset_to_utf32_position(&self, offset: usize) -> Position {
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        // Calculate UTF-32 column offset (character count) from the start of the line
        let line_start_byte = self.line_starts[line_idx];
        let line_text = &self.text[line_start_byte..offset];
        let utf32_column = line_text.chars().count();

        Position::Utf32 {
            line: line_idx,
            column: utf32_column,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_cursor_new() {
        let cursor = StringCursor::new("hello".to_string());
        assert_eq!(cursor.offset, 0);
        assert_eq!(cursor.end, 5);
    }

    #[test]
    fn test_string_cursor_single_line() {
        let mut cursor = StringCursor::new("abc".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span1.end_utf32(), Position::Utf32 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), 'b');
        assert_eq!(span2.start_utf32(), Position::Utf32 { line: 0, column: 1 });
        assert_eq!(span2.end_utf32(), Position::Utf32 { line: 0, column: 2 });

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'c');
        assert_eq!(span3.start_utf32(), Position::Utf32 { line: 0, column: 2 });
        assert_eq!(span3.end_utf32(), Position::Utf32 { line: 0, column: 3 });

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_multiline() {
        let mut cursor = StringCursor::new("a\nb\nc".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span1.end_utf32(), Position::Utf32 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\n');
        assert_eq!(span2.start_utf32(), Position::Utf32 { line: 0, column: 1 });
        assert_eq!(span2.end_utf32(), Position::Utf32 { line: 1, column: 0 });

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'b');
        assert_eq!(span3.start_utf32(), Position::Utf32 { line: 1, column: 0 });
        assert_eq!(span3.end_utf32(), Position::Utf32 { line: 1, column: 1 });

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch(), '\n');
        assert_eq!(span4.start_utf32(), Position::Utf32 { line: 1, column: 1 });
        assert_eq!(span4.end_utf32(), Position::Utf32 { line: 2, column: 0 });

        let span5 = cursor.next().unwrap();
        assert_eq!(span5.ch(), 'c');
        assert_eq!(span5.start_utf32(), Position::Utf32 { line: 2, column: 0 });
        assert_eq!(span5.end_utf32(), Position::Utf32 { line: 2, column: 1 });

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_span_extend() {
        let mut cursor = StringCursor::new("abc".to_string());
        let span1 = cursor.next().unwrap();
        let _span2 = cursor.next().unwrap();
        let span3 = cursor.next().unwrap();

        let extended = span1.clone().to(span3);
        assert_eq!(extended.ch(), 'a');
        assert_eq!(extended.to_string(), "abc");
        assert_eq!(
            extended.start_utf32(),
            Position::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(extended.end_utf32(), Position::Utf32 { line: 0, column: 3 });
    }

    #[test]
    fn test_string_span_to_string() {
        let mut cursor = StringCursor::new("hello world".to_string());
        let spans: Vec<_> = cursor.by_ref().take(5).collect();

        assert_eq!(spans[0].to_string(), "h");
        assert_eq!(spans[1].to_string(), "e");
        assert_eq!(spans[2].to_string(), "l");
        assert_eq!(spans[3].to_string(), "l");
        assert_eq!(spans[4].to_string(), "o");

        let extended = spans[0].clone().to(spans[4].clone());
        assert_eq!(extended.to_string(), "hello");
    }

    #[test]
    fn test_empty_string_cursor() {
        let mut cursor = StringCursor::new("".to_string());
        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_clone() {
        let cursor1 = StringCursor::new("test".to_string());
        let mut cursor2 = cursor1.clone();

        let span = cursor2.next().unwrap();
        assert_eq!(span.ch(), 't');
    }

    #[test]
    fn test_collect_string_spans() {
        let result: Option<StringSpan> = StringCursor::new("   hello".to_string())
            .take_while(|s| s.ch() == ' ')
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "   ");
        assert_eq!(span.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span.end_utf32(), Position::Utf32 { line: 0, column: 3 });
    }

    #[test]
    fn test_collect_empty_spans() {
        let result: Option<StringSpan> = StringCursor::new("hello".to_string())
            .take_while(|s| s.ch() == ' ')
            .collect();

        assert!(result.is_none());
    }

    #[test]
    fn test_collect_multiline_spans() {
        let result: Option<StringSpan> = StringCursor::new("aaa\nbbb".to_string())
            .take_while(|s| s.ch() == 'a')
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "aaa");
        assert_eq!(span.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span.end_utf32(), Position::Utf32 { line: 0, column: 3 });
    }

    #[test]
    fn test_collect_with_skip() {
        let result: Option<StringSpan> = StringCursor::new("   hello   ".to_string())
            .skip(3)
            .take_while(|s| s.ch().is_alphabetic())
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "hello");
        assert_eq!(span.start_utf32(), Position::Utf32 { line: 0, column: 3 });
        assert_eq!(span.end_utf32(), Position::Utf32 { line: 0, column: 8 });
    }

    #[test]
    fn test_string_cursor_utf16_single_line() {
        // "a\u{20AC}b" - \u{20AC} is € (Euro sign)
        // UTF-8 bytes:  a(1) €(3) b(1) = positions 0,1,4,5
        // UTF-16 units: a(1) €(1) b(1) = positions 0,1,2,3
        let mut cursor = StringCursor::new("a\u{20AC}b".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start_utf16(), Position::Utf16 { line: 0, column: 0 });
        assert_eq!(span1.end_utf16(), Position::Utf16 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\u{20AC}');
        assert_eq!(span2.start_utf16(), Position::Utf16 { line: 0, column: 1 });
        assert_eq!(span2.end_utf16(), Position::Utf16 { line: 0, column: 2 }); // Euro sign is 1 code unit in UTF-16

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'b');
        assert_eq!(span3.start_utf16(), Position::Utf16 { line: 0, column: 2 });
        assert_eq!(span3.end_utf16(), Position::Utf16 { line: 0, column: 3 });
    }

    #[test]
    fn test_string_cursor_utf16_multiline() {
        // "\u{20AC}\n\u{1F3A8}\nc" - Testing multi-line with different UTF-16 widths
        // \u{20AC} = € (1 UTF-16 code unit, in BMP)
        // \u{1F3A8} = 🎨 (2 UTF-16 code units, surrogate pair)
        // Line 0: €(1) \n(1)
        // Line 1: 🎨(2) \n(1)
        // Line 2: c(1)
        let mut cursor = StringCursor::new("\u{20AC}\n\u{1F3A8}\nc".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), '\u{20AC}');
        assert_eq!(span1.start_utf16(), Position::Utf16 { line: 0, column: 0 });
        assert_eq!(span1.end_utf16(), Position::Utf16 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\n');
        assert_eq!(span2.start_utf16(), Position::Utf16 { line: 0, column: 1 });
        assert_eq!(span2.end_utf16(), Position::Utf16 { line: 1, column: 0 });

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), '\u{1F3A8}');
        assert_eq!(span3.start_utf16(), Position::Utf16 { line: 1, column: 0 });
        assert_eq!(span3.end_utf16(), Position::Utf16 { line: 1, column: 2 }); // Emoji is 2 code units in UTF-16

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch(), '\n');
        assert_eq!(span4.start_utf16(), Position::Utf16 { line: 1, column: 2 });
        assert_eq!(span4.end_utf16(), Position::Utf16 { line: 2, column: 0 });

        let span5 = cursor.next().unwrap();
        assert_eq!(span5.ch(), 'c');
        assert_eq!(span5.start_utf16(), Position::Utf16 { line: 2, column: 0 });
        assert_eq!(span5.end_utf16(), Position::Utf16 { line: 2, column: 1 });
    }

    #[test]
    fn test_contains_position_utf16() {
        // "hello\nworld" - ASCII text for simple position testing
        let cursor = StringCursor::new("hello\nworld".to_string());
        let spans: Vec<_> = cursor.collect();

        // "hello" spans
        let hello_span = spans[0].clone().to(spans[4].clone());

        // Test UTF-16 position containment
        assert!(hello_span.contains_position(Position::Utf16 { line: 0, column: 0 }));
        assert!(hello_span.contains_position(Position::Utf16 { line: 0, column: 4 }));
        assert!(!hello_span.contains_position(Position::Utf16 { line: 0, column: 5 }));
        assert!(!hello_span.contains_position(Position::Utf16 { line: 1, column: 0 }));
    }

    #[test]
    fn test_string_cursor_utf32_single_line() {
        // "a\u{20AC}b\u{1F3A8}c" - Testing UTF-32 (character count)
        // a = 1 char, \u{20AC} (€) = 1 char, b = 1 char, \u{1F3A8} (🎨) = 1 char, c = 1 char
        // UTF-8:  a(1) €(3) b(1) 🎨(4) c(1) = byte positions
        // UTF-16: a(1) €(1) b(1) 🎨(2) c(1) = code unit positions
        // UTF-32: a(1) €(1) b(1) 🎨(1) c(1) = character positions 0,1,2,3,4,5
        let mut cursor = StringCursor::new("a\u{20AC}b\u{1F3A8}c".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span1.end_utf32(), Position::Utf32 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\u{20AC}');
        assert_eq!(span2.start_utf32(), Position::Utf32 { line: 0, column: 1 });
        assert_eq!(span2.end_utf32(), Position::Utf32 { line: 0, column: 2 });

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'b');
        assert_eq!(span3.start_utf32(), Position::Utf32 { line: 0, column: 2 });
        assert_eq!(span3.end_utf32(), Position::Utf32 { line: 0, column: 3 });

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch(), '\u{1F3A8}');
        assert_eq!(span4.start_utf32(), Position::Utf32 { line: 0, column: 3 });
        assert_eq!(span4.end_utf32(), Position::Utf32 { line: 0, column: 4 });

        let span5 = cursor.next().unwrap();
        assert_eq!(span5.ch(), 'c');
        assert_eq!(span5.start_utf32(), Position::Utf32 { line: 0, column: 4 });
        assert_eq!(span5.end_utf32(), Position::Utf32 { line: 0, column: 5 });
    }

    #[test]
    fn test_string_cursor_utf32_multiline() {
        // "\u{1F3A8}\n\u{20AC}x" - Testing UTF-32 with newlines
        // Line 0: 🎨(1 char) \n(1 char)
        // Line 1: €(1 char) x(1 char)
        let mut cursor = StringCursor::new("\u{1F3A8}\n\u{20AC}x".to_string());

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), '\u{1F3A8}');
        assert_eq!(span1.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(span1.end_utf32(), Position::Utf32 { line: 0, column: 1 });

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\n');
        assert_eq!(span2.start_utf32(), Position::Utf32 { line: 0, column: 1 });
        assert_eq!(span2.end_utf32(), Position::Utf32 { line: 1, column: 0 });

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), '\u{20AC}');
        assert_eq!(span3.start_utf32(), Position::Utf32 { line: 1, column: 0 });
        assert_eq!(span3.end_utf32(), Position::Utf32 { line: 1, column: 1 });

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch(), 'x');
        assert_eq!(span4.start_utf32(), Position::Utf32 { line: 1, column: 1 });
        assert_eq!(span4.end_utf32(), Position::Utf32 { line: 1, column: 2 });
    }

    #[test]
    fn test_contains_position_utf32() {
        // "\u{1F3A8}hello" - Emoji followed by ASCII
        let cursor = StringCursor::new("\u{1F3A8}hello".to_string());
        let spans: Vec<_> = cursor.collect();

        // Create span for "hello" (skipping the emoji)
        let hello_span = spans[1].clone().to(spans[5].clone());

        // Test UTF-32 position containment
        assert!(hello_span.contains_position(Position::Utf32 { line: 0, column: 1 }));
        assert!(hello_span.contains_position(Position::Utf32 { line: 0, column: 5 }));
        assert!(!hello_span.contains_position(Position::Utf32 { line: 0, column: 0 }));
        assert!(!hello_span.contains_position(Position::Utf32 { line: 0, column: 6 }));
    }

    #[test]
    fn test_compare_utf_encodings() {
        // "\u{1F3A8}ab" - Compare UTF-16 and UTF-32 encodings
        // 🎨 = U+1F3A8: 2 code units UTF-16, 1 char UTF-32
        let mut cursor = StringCursor::new("\u{1F3A8}ab".to_string());

        let emoji = cursor.next().unwrap();
        let a = cursor.next().unwrap();
        let b = cursor.next().unwrap();

        // Emoji positions
        assert_eq!(emoji.start_utf16(), Position::Utf16 { line: 0, column: 0 });
        assert_eq!(emoji.end_utf16(), Position::Utf16 { line: 0, column: 2 });
        assert_eq!(emoji.start_utf32(), Position::Utf32 { line: 0, column: 0 });
        assert_eq!(emoji.end_utf32(), Position::Utf32 { line: 0, column: 1 });

        // 'a' positions - notice different column values
        assert_eq!(a.start_utf16(), Position::Utf16 { line: 0, column: 2 });
        assert_eq!(a.start_utf32(), Position::Utf32 { line: 0, column: 1 });

        // 'b' positions
        assert_eq!(b.start_utf16(), Position::Utf16 { line: 0, column: 3 });
        assert_eq!(b.start_utf32(), Position::Utf32 { line: 0, column: 2 });
    }
}
