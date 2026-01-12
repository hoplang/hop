use std::{
    borrow::Borrow,
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
    ops::Deref,
    sync::Arc,
};

use super::{DocumentPosition, document_info::DocumentInfo};

#[derive(Clone)]
pub struct DocumentCursor {
    /// The source info containing text and line starts.
    source: Arc<DocumentInfo>,
    /// The current byte offset in the source string.
    offset: usize,
    /// The byte offset where iteration should stop (exclusive).
    end: usize,
}

impl DocumentCursor {
    pub fn new(source: String) -> Self {
        let end = source.len();
        Self {
            offset: 0,
            end,
            source: Arc::new(DocumentInfo::new(source)),
        }
    }
    pub fn range(&self) -> DocumentRange {
        DocumentRange {
            source: self.source.clone(),
            start: self.offset,
            end: self.end,
        }
    }
}

impl Iterator for DocumentCursor {
    type Item = DocumentRange;
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
                DocumentRange {
                    source: self.source.clone(),
                    start: start_offset,
                    end: self.offset,
                }
            })
    }
}

/// A DocumentRange represents a range in a document.
///
/// It is always non-empty, i.e. start < end and as_str().len() > 0
/// always holds.
#[derive(Clone, Debug)]
pub struct DocumentRange {
    /// The source info containing the document text and line starts.
    source: Arc<DocumentInfo>,
    /// the start byte offset for this range in the document (inclusive).
    start: usize,
    /// the end byte offset for this range in the document (exclusive).
    end: usize,
}

impl DocumentRange {
    /// Get the first char from the range.
    pub fn ch(&self) -> char {
        self.source.text[self.start..].chars().next().unwrap()
    }

    /// Extend a range to encompass another range that occurs
    /// later in the document.
    pub fn to(self, other: DocumentRange) -> Self {
        debug_assert!(other.start > self.start);
        debug_assert!(other.end > self.end);
        DocumentRange {
            source: self.source,
            start: self.start,
            end: other.end,
        }
    }

    /// Extend a document range with an iterator of document ranges
    /// producing a single document range.
    ///
    /// The document ranges must occur sequentially in the document.
    pub fn extend<I>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = DocumentRange>,
    {
        iter.into_iter().fold(self, |acc, range| acc.to(range))
    }

    /// Get the underlying string slice for this document range.
    pub fn as_str(&self) -> &str {
        &self.source.text[self.start..self.end]
    }

    /// Get the full source text for the document that this
    /// document range is a part of.
    pub(super) fn full_source(&self) -> &str {
        &self.source.text
    }

    /// Get a string cursor for this document range.
    pub fn cursor(&self) -> DocumentCursor {
        DocumentCursor {
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

    pub fn start_utf16(&self) -> DocumentPosition {
        self.source.offset_to_utf16_position(self.start)
    }

    pub fn end_utf16(&self) -> DocumentPosition {
        self.source.offset_to_utf16_position(self.end)
    }

    pub fn start_utf32(&self) -> DocumentPosition {
        self.source.offset_to_utf32_position(self.start)
    }

    pub fn end_utf32(&self) -> DocumentPosition {
        self.source.offset_to_utf32_position(self.end)
    }

    pub fn contains(&self, other: &DocumentRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Returns true if the document range contains the given Position.
    pub fn contains_position(&self, position: DocumentPosition) -> bool {
        match position {
            DocumentPosition::Utf16 { .. } => {
                let start = self.start_utf16();
                let end = self.end_utf16();
                start <= position && position < end
            }
            DocumentPosition::Utf32 { .. } => {
                let start = self.start_utf32();
                let end = self.end_utf32();
                start <= position && position < end
            }
        }
    }

    pub fn intersection(&self, other: &DocumentRange) -> Option<DocumentRange> {
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start < end {
            Some(DocumentRange {
                source: self.source.clone(),
                start,
                end,
            })
        } else {
            None
        }
    }

    /// Convert this DocumentRange into a CheapString.
    pub fn to_cheap_string(&self) -> CheapString {
        CheapString {
            source: self.source.clone(),
            start: self.start,
            end: self.end,
        }
    }
}

/// Turn an iterator of document ranges into a single Option<DocumentRange>
///
/// Returns None if the iterator contains no elements.
///
/// The document ranges must occur sequentially in the document.
impl FromIterator<DocumentRange> for Option<DocumentRange> {
    fn from_iter<I: IntoIterator<Item = DocumentRange>>(iter: I) -> Self {
        iter.into_iter().reduce(|acc, range| acc.to(range))
    }
}

impl fmt::Display for DocumentRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

pub trait Ranged {
    fn range(&self) -> &DocumentRange;
}

impl<T: Ranged> Ranged for &T {
    fn range(&self) -> &DocumentRange {
        (*self).range()
    }
}

impl Ranged for DocumentRange {
    fn range(&self) -> &DocumentRange {
        self
    }
}

/// A CheapString is an owned smart pointer to a string.
/// It has the same semantics as an owned string but does not require
/// a heap allocation.
#[derive(Clone)]
pub struct CheapString {
    /// The source info containing the document text and line starts.
    source: Arc<DocumentInfo>,
    /// the start byte offset for this span in the document (inclusive).
    start: usize,
    /// the end byte offset for this span in the document (exclusive).
    end: usize,
}

impl fmt::Debug for CheapString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Show a compact representation: just the text content
        write!(f, "{:?}", self.as_str())
    }
}

impl CheapString {
    pub fn new(s: String) -> Self {
        Self {
            end: s.len(),
            source: Arc::new(DocumentInfo::new(s)),
            start: 0,
        }
    }
    /// Get the underlying string slice for this span.
    pub fn as_str(&self) -> &str {
        &self.source.text[self.start..self.end]
    }
}

impl Hash for CheapString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl fmt::Display for CheapString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl PartialEq for CheapString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for CheapString {}

impl PartialOrd for CheapString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CheapString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Deref for CheapString {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for CheapString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_cursor_new() {
        let cursor = DocumentCursor::new("hello".to_string());
        assert_eq!(cursor.offset, 0);
        assert_eq!(cursor.end, 5);
    }

    #[test]
    fn string_cursor_single_line() {
        let mut cursor = DocumentCursor::new("abc".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(
            range1.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), 'b');
        assert_eq!(
            range2.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 2 }
        );

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'c');
        assert_eq!(
            range3.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 2 }
        );
        assert_eq!(
            range3.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );

        assert!(cursor.next().is_none());
    }

    #[test]
    fn string_cursor_multiline() {
        let mut cursor = DocumentCursor::new("a\nb\nc".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(
            range1.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(
            range2.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 0 }
        );

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(
            range3.start_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 0 }
        );
        assert_eq!(
            range3.end_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 1 }
        );

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\n');
        assert_eq!(
            range4.start_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 1 }
        );
        assert_eq!(
            range4.end_utf32(),
            DocumentPosition::Utf32 { line: 2, column: 0 }
        );

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(
            range5.start_utf32(),
            DocumentPosition::Utf32 { line: 2, column: 0 }
        );
        assert_eq!(
            range5.end_utf32(),
            DocumentPosition::Utf32 { line: 2, column: 1 }
        );

        assert!(cursor.next().is_none());
    }

    #[test]
    fn string_range_extend() {
        let mut cursor = DocumentCursor::new("abc".to_string());
        let range1 = cursor.next().unwrap();
        let _range2 = cursor.next().unwrap();
        let range3 = cursor.next().unwrap();

        let extended = range1.clone().to(range3);
        assert_eq!(extended.ch(), 'a');
        assert_eq!(extended.to_string(), "abc");
        assert_eq!(
            extended.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            extended.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );
    }

    #[test]
    fn string_range_to_string() {
        let mut cursor = DocumentCursor::new("hello world".to_string());
        let ranges: Vec<_> = cursor.by_ref().take(5).collect();

        assert_eq!(ranges[0].to_string(), "h");
        assert_eq!(ranges[1].to_string(), "e");
        assert_eq!(ranges[2].to_string(), "l");
        assert_eq!(ranges[3].to_string(), "l");
        assert_eq!(ranges[4].to_string(), "o");

        let extended = ranges[0].clone().to(ranges[4].clone());
        assert_eq!(extended.to_string(), "hello");
    }

    #[test]
    fn empty_string_cursor() {
        let mut cursor = DocumentCursor::new("".to_string());
        assert!(cursor.next().is_none());
    }

    #[test]
    fn string_cursor_clone() {
        let cursor1 = DocumentCursor::new("test".to_string());
        let mut cursor2 = cursor1.clone();

        let range = cursor2.next().unwrap();
        assert_eq!(range.ch(), 't');
    }

    #[test]
    fn collect_string_ranges() {
        let result: Option<DocumentRange> = DocumentCursor::new("   hello".to_string())
            .take_while(|s| s.ch() == ' ')
            .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "   ");
        assert_eq!(
            range.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );
    }

    #[test]
    fn collect_empty_ranges() {
        let result: Option<DocumentRange> = DocumentCursor::new("hello".to_string())
            .take_while(|s| s.ch() == ' ')
            .collect();

        assert!(result.is_none());
    }

    #[test]
    fn collect_multiline_ranges() {
        let result: Option<DocumentRange> = DocumentCursor::new("aaa\nbbb".to_string())
            .take_while(|s| s.ch() == 'a')
            .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "aaa");
        assert_eq!(
            range.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );
    }

    #[test]
    fn collect_with_skip() {
        let result: Option<DocumentRange> = DocumentCursor::new("   hello   ".to_string())
            .skip(3)
            .take_while(|s| s.ch().is_alphabetic())
            .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "hello");
        assert_eq!(
            range.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );
        assert_eq!(
            range.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 8 }
        );
    }

    #[test]
    fn string_cursor_utf16_single_line() {
        // "a\u{20AC}b" - \u{20AC} is € (Euro sign)
        // UTF-8 bytes:  a(1) €(3) b(1) = positions 0,1,4,5
        // UTF-16 units: a(1) €(1) b(1) = positions 0,1,2,3
        let mut cursor = DocumentCursor::new("a\u{20AC}b".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(
            range1.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\u{20AC}');
        assert_eq!(
            range2.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 2 }
        ); // Euro sign is 1 code unit in UTF-16

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(
            range3.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 2 }
        );
        assert_eq!(
            range3.end_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 3 }
        );
    }

    #[test]
    fn string_cursor_utf16_multiline() {
        // "\u{20AC}\n\u{1F3A8}\nc" - Testing multi-line with different UTF-16 widths
        // \u{20AC} = € (1 UTF-16 code unit, in BMP)
        // \u{1F3A8} = 🎨 (2 UTF-16 code units, surrogate pair)
        // Line 0: €(1) \n(1)
        // Line 1: 🎨(2) \n(1)
        // Line 2: c(1)
        let mut cursor = DocumentCursor::new("\u{20AC}\n\u{1F3A8}\nc".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), '\u{20AC}');
        assert_eq!(
            range1.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(
            range2.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf16(),
            DocumentPosition::Utf16 { line: 1, column: 0 }
        );

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), '\u{1F3A8}');
        assert_eq!(
            range3.start_utf16(),
            DocumentPosition::Utf16 { line: 1, column: 0 }
        );
        assert_eq!(
            range3.end_utf16(),
            DocumentPosition::Utf16 { line: 1, column: 2 }
        ); // Emoji is 2 code units in UTF-16

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\n');
        assert_eq!(
            range4.start_utf16(),
            DocumentPosition::Utf16 { line: 1, column: 2 }
        );
        assert_eq!(
            range4.end_utf16(),
            DocumentPosition::Utf16 { line: 2, column: 0 }
        );

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(
            range5.start_utf16(),
            DocumentPosition::Utf16 { line: 2, column: 0 }
        );
        assert_eq!(
            range5.end_utf16(),
            DocumentPosition::Utf16 { line: 2, column: 1 }
        );
    }

    #[test]
    fn contains_position_utf16() {
        // "hello\nworld" - ASCII text for simple position testing
        let cursor = DocumentCursor::new("hello\nworld".to_string());
        let ranges: Vec<_> = cursor.collect();

        // "hello" ranges
        let hello_range = ranges[0].clone().to(ranges[4].clone());

        // Test UTF-16 position containment
        assert!(hello_range.contains_position(DocumentPosition::Utf16 { line: 0, column: 0 }));
        assert!(hello_range.contains_position(DocumentPosition::Utf16 { line: 0, column: 4 }));
        assert!(!hello_range.contains_position(DocumentPosition::Utf16 { line: 0, column: 5 }));
        assert!(!hello_range.contains_position(DocumentPosition::Utf16 { line: 1, column: 0 }));
    }

    #[test]
    fn string_cursor_utf32_single_line() {
        // "a\u{20AC}b\u{1F3A8}c" - Testing UTF-32 (character count)
        // a = 1 char, \u{20AC} (€) = 1 char, b = 1 char, \u{1F3A8} (🎨) = 1 char, c = 1 char
        // UTF-8:  a(1) €(3) b(1) 🎨(4) c(1) = byte positions
        // UTF-16: a(1) €(1) b(1) 🎨(2) c(1) = code unit positions
        // UTF-32: a(1) €(1) b(1) 🎨(1) c(1) = character positions 0,1,2,3,4,5
        let mut cursor = DocumentCursor::new("a\u{20AC}b\u{1F3A8}c".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(
            range1.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\u{20AC}');
        assert_eq!(
            range2.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 2 }
        );

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(
            range3.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 2 }
        );
        assert_eq!(
            range3.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\u{1F3A8}');
        assert_eq!(
            range4.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 3 }
        );
        assert_eq!(
            range4.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 4 }
        );

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(
            range5.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 4 }
        );
        assert_eq!(
            range5.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 5 }
        );
    }

    #[test]
    fn string_cursor_utf32_multiline() {
        // "\u{1F3A8}\n\u{20AC}x" - Testing UTF-32 with newlines
        // Line 0: 🎨(1 char) \n(1 char)
        // Line 1: €(1 char) x(1 char)
        let mut cursor = DocumentCursor::new("\u{1F3A8}\n\u{20AC}x".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), '\u{1F3A8}');
        assert_eq!(
            range1.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            range1.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(
            range2.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );
        assert_eq!(
            range2.end_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 0 }
        );

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), '\u{20AC}');
        assert_eq!(
            range3.start_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 0 }
        );
        assert_eq!(
            range3.end_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 1 }
        );

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), 'x');
        assert_eq!(
            range4.start_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 1 }
        );
        assert_eq!(
            range4.end_utf32(),
            DocumentPosition::Utf32 { line: 1, column: 2 }
        );
    }

    #[test]
    fn contains_position_utf32() {
        // "\u{1F3A8}hello" - Emoji followed by ASCII
        let cursor = DocumentCursor::new("\u{1F3A8}hello".to_string());
        let ranges: Vec<_> = cursor.collect();

        // Create range for "hello" (skipping the emoji)
        let hello_range = ranges[1].clone().to(ranges[5].clone());

        // Test UTF-32 position containment
        assert!(hello_range.contains_position(DocumentPosition::Utf32 { line: 0, column: 1 }));
        assert!(hello_range.contains_position(DocumentPosition::Utf32 { line: 0, column: 5 }));
        assert!(!hello_range.contains_position(DocumentPosition::Utf32 { line: 0, column: 0 }));
        assert!(!hello_range.contains_position(DocumentPosition::Utf32 { line: 0, column: 6 }));
    }

    #[test]
    fn compare_utf_encodings() {
        // "\u{1F3A8}ab" - Compare UTF-16 and UTF-32 encodings
        // 🎨 = U+1F3A8: 2 code units UTF-16, 1 char UTF-32
        let mut cursor = DocumentCursor::new("\u{1F3A8}ab".to_string());

        let emoji = cursor.next().unwrap();
        let a = cursor.next().unwrap();
        let b = cursor.next().unwrap();

        // Emoji positions
        assert_eq!(
            emoji.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 0 }
        );
        assert_eq!(
            emoji.end_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 2 }
        );
        assert_eq!(
            emoji.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 0 }
        );
        assert_eq!(
            emoji.end_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        // 'a' positions - notice different column values
        assert_eq!(
            a.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 2 }
        );
        assert_eq!(
            a.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 1 }
        );

        // 'b' positions
        assert_eq!(
            b.start_utf16(),
            DocumentPosition::Utf16 { line: 0, column: 3 }
        );
        assert_eq!(
            b.start_utf32(),
            DocumentPosition::Utf32 { line: 0, column: 2 }
        );
    }
}
