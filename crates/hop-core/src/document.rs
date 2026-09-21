use crate::document_id::DocumentId;
use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::ops::Deref;
use std::sync::Arc;

/// Holds source text and precomputed line start offsets for
/// efficient position lookups.
#[derive(Debug, Clone)]
struct DocumentInfo {
    /// The id of the document.
    document_id: DocumentId,
    /// The source text.
    text: Arc<String>,
    /// Byte offsets where each line starts.
    /// First line always starts at 0.
    line_starts: Vec<usize>,
}

impl DocumentInfo {
    pub fn new(document_id: DocumentId, text: String) -> Self {
        let mut line_starts = vec![0];
        for (i, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(i + ch.len_utf8());
            }
        }
        Self {
            document_id,
            text: Arc::new(text),
            line_starts,
        }
    }

    /// The 0-based line a byte offset is on.
    fn line_of(&self, offset: usize) -> usize {
        match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        }
    }

    /// The 0-based column of a byte offset, measured in the given
    /// encoding from the start of its line.
    fn column_of(&self, offset: usize, encoding: PositionEncoding) -> usize {
        let line_start = self.line_starts[self.line_of(offset)];
        self.text[line_start..offset]
            .chars()
            .map(|ch| encoding.width(ch))
            .sum()
    }
}

/// How columns are counted when a position crosses the boundary to an
/// editor or to human-readable output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEncoding {
    /// Columns count UTF-16 code units. This is what LSP clients send.
    Utf16,
    /// Columns count Unicode code points.
    Utf32,
}

impl PositionEncoding {
    fn width(self, ch: char) -> usize {
        match self {
            PositionEncoding::Utf16 => ch.len_utf16(),
            PositionEncoding::Utf32 => 1,
        }
    }
}

/// A position in a document.
#[derive(Clone, Debug)]
pub struct DocumentPosition {
    /// The source info containing the document text and line starts.
    source: Arc<DocumentInfo>,
    /// The byte offset of the position in the document.
    offset: usize,
}

impl DocumentPosition {
    pub fn document_id(&self) -> &DocumentId {
        &self.source.document_id
    }

    /// The 0-based line of the position.
    pub fn line(&self) -> usize {
        self.source.line_of(self.offset)
    }

    /// The 0-based column of the position in UTF-16 code units.
    pub fn utf16_column(&self) -> usize {
        self.source.column_of(self.offset, PositionEncoding::Utf16)
    }

    /// The 0-based column of the position in Unicode code points.
    pub fn utf32_column(&self) -> usize {
        self.source.column_of(self.offset, PositionEncoding::Utf32)
    }
}

impl PartialEq for DocumentPosition {
    fn eq(&self, other: &Self) -> bool {
        self.source.document_id == other.source.document_id && self.offset == other.offset
    }
}

impl Eq for DocumentPosition {}

/// A Document is a shared reference to a document in the project.
#[derive(Clone, Debug)]
pub struct Document {
    source: Arc<DocumentInfo>,
}

impl Document {
    pub fn new(document_id: DocumentId, text: String) -> Self {
        Self {
            source: Arc::new(DocumentInfo::new(document_id, text)),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.source.text
    }

    /// Resolve a line and column, with the column counted in `encoding`,
    /// to a position in this document.
    ///
    /// Returns None if the line does not exist, if the column is past the end
    /// of the line, or if it lands inside a code point.
    ///
    /// A column equal to the line's width is the position just after its last
    /// character, where an editor puts the cursor at the line end.
    pub fn position(
        &self,
        encoding: PositionEncoding,
        line: usize,
        column: usize,
    ) -> Option<DocumentPosition> {
        let info = &self.source;
        let line_start = *info.line_starts.get(line)?;
        let line_end = match info.line_starts.get(line + 1) {
            Some(next_line_start) => next_line_start - 1,
            None => info.text.len(),
        };
        let mut width = 0;
        for (i, ch) in info.text[line_start..line_end].char_indices() {
            if width == column {
                return Some(DocumentPosition {
                    source: info.clone(),
                    offset: line_start + i,
                });
            }
            width += encoding.width(ch);
        }
        if width == column {
            Some(DocumentPosition {
                source: info.clone(),
                offset: line_end,
            })
        } else {
            None
        }
    }

    pub(crate) fn range(&self, span: std::ops::Range<usize>) -> DocumentRange {
        let text = &self.source.text;
        assert!(
            span.start <= span.end,
            "range {}..{} starts after it ends",
            span.start,
            span.end
        );
        assert!(
            text.is_char_boundary(span.start) && text.is_char_boundary(span.end),
            "range {}..{} is not on char boundaries of a {} byte document",
            span.start,
            span.end,
            text.len()
        );
        DocumentRange {
            source: self.source.clone(),
            start: span.start,
            end: span.end,
        }
    }

    pub(crate) fn cursor(&self) -> DocumentCursor {
        let end = self.source.text.len();
        DocumentCursor {
            source: self.source.clone(),
            offset: 0,
            end,
        }
    }
}

#[derive(Clone)]
pub(crate) struct DocumentCursor {
    /// The source info containing text and line starts.
    source: Arc<DocumentInfo>,
    /// The current byte offset in the source string.
    offset: usize,
    /// The byte offset where iteration should stop (exclusive).
    end: usize,
}

impl DocumentCursor {
    pub fn new(document_id: DocumentId, source: String) -> Self {
        let end = source.len();
        Self {
            offset: 0,
            end,
            source: Arc::new(DocumentInfo::new(document_id, source)),
        }
    }

    /// The empty range one past the last character the cursor can reach.
    pub fn eof_range(&self) -> DocumentRange {
        DocumentRange {
            source: self.source.clone(),
            start: self.end,
            end: self.end,
        }
    }

    /// Returns the next range without consuming it.
    pub fn peek(&self) -> Option<DocumentRange> {
        if self.offset >= self.end {
            return None;
        }
        self.source.text[self.offset..self.end]
            .chars()
            .next()
            .map(|ch| DocumentRange {
                source: self.source.clone(),
                start: self.offset,
                end: self.offset + ch.len_utf8(),
            })
    }

    /// Consumes and returns the next range if `predicate` accepts it.
    pub fn next_if(
        &mut self,
        predicate: impl FnOnce(&DocumentRange) -> bool,
    ) -> Option<DocumentRange> {
        let range = self.peek()?;
        if !predicate(&range) {
            return None;
        }
        self.offset = range.end;
        Some(range)
    }

    /// Consumes ranges for as long as `predicate` accepts them. Unlike
    /// `take_while`, the range that stops iteration is left unconsumed.
    pub fn peeking_take_while<F>(&mut self, predicate: F) -> PeekingTakeWhile<'_, F>
    where
        F: FnMut(&DocumentRange) -> bool,
    {
        PeekingTakeWhile {
            cursor: self,
            predicate,
        }
    }

    /// Run `f` on a copy of the cursor, and keep the copy's progress only
    /// if `f` returns `Some`. On `None` the cursor is left where it was,
    /// so a lookahead that fails part-way through consumes nothing.
    pub fn speculate<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let mut ahead = self.clone();
        let result = f(&mut ahead)?;
        *self = ahead;
        Some(result)
    }
}

pub(crate) struct PeekingTakeWhile<'a, F> {
    cursor: &'a mut DocumentCursor,
    predicate: F,
}

impl<F: FnMut(&DocumentRange) -> bool> Iterator for PeekingTakeWhile<'_, F> {
    type Item = DocumentRange;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next_if(&mut self.predicate)
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

/// A range in a document.
#[derive(Clone, Debug)]
pub struct DocumentRange {
    /// The source info containing the document text and line starts.
    source: Arc<DocumentInfo>,
    /// The start byte offset for this range in the document (inclusive).
    start: usize,
    /// The end byte offset for this range in the document (exclusive).
    end: usize,
}

impl DocumentRange {
    /// Get the first char from the range. Returns '\0' for an empty range.
    pub(crate) fn ch(&self) -> char {
        if self.start == self.end {
            return '\0';
        }
        self.source.text[self.start..self.end]
            .chars()
            .next()
            .unwrap()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Extend a range to encompass another range that occurs
    /// later in the document.
    pub(crate) fn to(self, other: DocumentRange) -> Self {
        debug_assert!(other.start >= self.start);
        debug_assert!(other.end >= self.end);
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
    pub(crate) fn extend<I>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = DocumentRange>,
    {
        iter.into_iter().fold(self, |acc, range| acc.to(range))
    }

    /// Get the underlying string slice for this document range.
    pub(crate) fn as_str(&self) -> &str {
        &self.source.text[self.start..self.end]
    }

    /// Get the full source text for the document that this
    /// document range is a part of.
    pub(crate) fn full_source(&self) -> &str {
        &self.source.text
    }

    /// Get a string cursor for this document range.
    pub(crate) fn cursor(&self) -> DocumentCursor {
        DocumentCursor {
            source: self.source.clone(),
            offset: self.start,
            end: self.end,
        }
    }

    pub(crate) fn start(&self) -> usize {
        self.start
    }

    pub(crate) fn end(&self) -> usize {
        self.end
    }

    pub fn document_id(&self) -> &DocumentId {
        &self.source.document_id
    }

    pub fn start_position(&self) -> DocumentPosition {
        DocumentPosition {
            source: self.source.clone(),
            offset: self.start,
        }
    }

    pub fn end_position(&self) -> DocumentPosition {
        DocumentPosition {
            source: self.source.clone(),
            offset: self.end,
        }
    }

    pub(crate) fn contains(&self, other: &DocumentRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    pub(crate) fn contains_position(&self, position: &DocumentPosition) -> bool {
        debug_assert_eq!(self.source.document_id, position.source.document_id);
        self.start <= position.offset && position.offset < self.end
    }

    pub(crate) fn intersection(&self, other: &DocumentRange) -> Option<DocumentRange> {
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
    pub(crate) fn to_cheap_string(&self) -> CheapString {
        CheapString {
            text: self.source.text.clone(),
            start: self.start,
            end: self.end,
        }
    }

    /// Returns a new DocumentRange with leading and trailing whitespace removed.
    pub(crate) fn trim(&self) -> DocumentRange {
        self.trim_start().trim_end()
    }

    /// Returns a new DocumentRange with leading whitespace removed.
    pub(crate) fn trim_start(&self) -> DocumentRange {
        let s = self.as_str();
        let trimmed = s.trim_start();
        let leading = trimmed.as_ptr() as usize - s.as_ptr() as usize;
        DocumentRange {
            source: self.source.clone(),
            start: self.start + leading,
            end: self.end,
        }
    }

    /// Returns a new DocumentRange with trailing whitespace removed.
    pub(crate) fn trim_end(&self) -> DocumentRange {
        let s = self.as_str();
        let trimmed = s.trim_end();
        DocumentRange {
            source: self.source.clone(),
            start: self.start,
            end: self.start + trimmed.len(),
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

/// Two ranges are equal if they span the same byte offsets in the same document.
impl PartialEq for DocumentRange {
    fn eq(&self, other: &Self) -> bool {
        self.source.document_id == other.source.document_id
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for DocumentRange {}

impl fmt::Display for DocumentRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A CheapString is an owned smart pointer to a string.
/// It has the same semantics as an owned string but does not require
/// a heap allocation.
#[derive(Clone)]
pub(crate) struct CheapString {
    /// The shared underlying text.
    text: Arc<String>,
    /// the start byte offset for this span in the text (inclusive).
    start: usize,
    /// the end byte offset for this span in the text (exclusive).
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
            text: Arc::new(s),
            start: 0,
        }
    }
    /// Get the underlying string slice for this span.
    pub fn as_str(&self) -> &str {
        &self.text[self.start..self.end]
    }
}

impl Hash for CheapString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl fmt::Display for CheapString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl PartialEq for CheapString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<str> for CheapString {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for CheapString {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<CheapString> for str {
    fn eq(&self, other: &CheapString) -> bool {
        self == other.as_str()
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
    use crate::document_id::DocumentId;

    fn utf16(position: &DocumentPosition) -> (usize, usize) {
        (position.line(), position.utf16_column())
    }

    fn utf32(position: &DocumentPosition) -> (usize, usize) {
        (position.line(), position.utf32_column())
    }

    #[test]
    fn string_cursor_new() {
        let cursor = DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "hello".to_string());
        assert_eq!(cursor.offset, 0);
        assert_eq!(cursor.end, 5);
    }

    #[test]
    fn string_cursor_single_line() {
        let mut cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "abc".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(utf32(&range1.start_position()), (0, 0));
        assert_eq!(utf32(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), 'b');
        assert_eq!(utf32(&range2.start_position()), (0, 1));
        assert_eq!(utf32(&range2.end_position()), (0, 2));

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'c');
        assert_eq!(utf32(&range3.start_position()), (0, 2));
        assert_eq!(utf32(&range3.end_position()), (0, 3));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn string_cursor_multiline() {
        let mut cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "a\nb\nc".to_string());

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(utf32(&range1.start_position()), (0, 0));
        assert_eq!(utf32(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(utf32(&range2.start_position()), (0, 1));
        assert_eq!(utf32(&range2.end_position()), (1, 0));

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(utf32(&range3.start_position()), (1, 0));
        assert_eq!(utf32(&range3.end_position()), (1, 1));

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\n');
        assert_eq!(utf32(&range4.start_position()), (1, 1));
        assert_eq!(utf32(&range4.end_position()), (2, 0));

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(utf32(&range5.start_position()), (2, 0));
        assert_eq!(utf32(&range5.end_position()), (2, 1));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn string_range_extend() {
        let mut cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "abc".to_string());
        let range1 = cursor.next().unwrap();
        let _range2 = cursor.next().unwrap();
        let range3 = cursor.next().unwrap();

        let extended = range1.to(range3);
        assert_eq!(extended.ch(), 'a');
        assert_eq!(extended.to_string(), "abc");
        assert_eq!(utf32(&extended.start_position()), (0, 0));
        assert_eq!(utf32(&extended.end_position()), (0, 3));
    }

    #[test]
    fn string_range_to_string() {
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "hello world".to_string(),
        );
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
        let mut cursor = DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "".to_string());
        assert!(cursor.next().is_none());
    }

    #[test]
    fn collect_string_ranges() {
        let result: Option<DocumentRange> =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "   hello".to_string())
                .take_while(|s| s.ch() == ' ')
                .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "   ");
        assert_eq!(utf32(&range.start_position()), (0, 0));
        assert_eq!(utf32(&range.end_position()), (0, 3));
    }

    #[test]
    fn collect_empty_ranges() {
        let result: Option<DocumentRange> =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "hello".to_string())
                .take_while(|s| s.ch() == ' ')
                .collect();

        assert!(result.is_none());
    }

    #[test]
    fn collect_multiline_ranges() {
        let result: Option<DocumentRange> =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), "aaa\nbbb".to_string())
                .take_while(|s| s.ch() == 'a')
                .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "aaa");
        assert_eq!(utf32(&range.start_position()), (0, 0));
        assert_eq!(utf32(&range.end_position()), (0, 3));
    }

    #[test]
    fn collect_with_skip() {
        let result: Option<DocumentRange> = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "   hello   ".to_string(),
        )
        .skip(3)
        .take_while(|s| s.ch().is_alphabetic())
        .collect();

        let range = result.unwrap();
        assert_eq!(range.as_str(), "hello");
        assert_eq!(utf32(&range.start_position()), (0, 3));
        assert_eq!(utf32(&range.end_position()), (0, 8));
    }

    #[test]
    fn string_cursor_utf16_single_line() {
        // "a\u{20AC}b" - \u{20AC} is € (Euro sign)
        // UTF-8 bytes:  a(1) €(3) b(1) = positions 0,1,4,5
        // UTF-16 units: a(1) €(1) b(1) = positions 0,1,2,3
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "a\u{20AC}b".to_string(),
        );

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(utf16(&range1.start_position()), (0, 0));
        assert_eq!(utf16(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\u{20AC}');
        assert_eq!(utf16(&range2.start_position()), (0, 1));
        assert_eq!(utf16(&range2.end_position()), (0, 2)); // Euro sign is 1 code unit in UTF-16

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(utf16(&range3.start_position()), (0, 2));
        assert_eq!(utf16(&range3.end_position()), (0, 3));
    }

    #[test]
    fn string_cursor_utf16_multiline() {
        // "\u{20AC}\n\u{1F3A8}\nc" - Testing multi-line with different UTF-16 widths
        // \u{20AC} = € (1 UTF-16 code unit, in BMP)
        // \u{1F3A8} = 🎨 (2 UTF-16 code units, surrogate pair)
        // Line 0: €(1) \n(1)
        // Line 1: 🎨(2) \n(1)
        // Line 2: c(1)
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "\u{20AC}\n\u{1F3A8}\nc".to_string(),
        );

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), '\u{20AC}');
        assert_eq!(utf16(&range1.start_position()), (0, 0));
        assert_eq!(utf16(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(utf16(&range2.start_position()), (0, 1));
        assert_eq!(utf16(&range2.end_position()), (1, 0));

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), '\u{1F3A8}');
        assert_eq!(utf16(&range3.start_position()), (1, 0));
        assert_eq!(utf16(&range3.end_position()), (1, 2)); // Emoji is 2 code units in UTF-16

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\n');
        assert_eq!(utf16(&range4.start_position()), (1, 2));
        assert_eq!(utf16(&range4.end_position()), (2, 0));

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(utf16(&range5.start_position()), (2, 0));
        assert_eq!(utf16(&range5.end_position()), (2, 1));
    }

    #[test]
    fn contains_position_utf16() {
        // "hello\nworld" - ASCII text for simple position testing
        let document = Document::new(
            DocumentId::new("test.hop").unwrap(),
            "hello\nworld".to_string(),
        );
        let ranges: Vec<_> = document.cursor().collect();
        let position = |line, column| {
            document
                .position(PositionEncoding::Utf16, line, column)
                .unwrap()
        };

        // "hello" ranges
        let hello_range = ranges[0].clone().to(ranges[4].clone());

        // Test UTF-16 position containment
        assert!(hello_range.contains_position(&position(0, 0)));
        assert!(hello_range.contains_position(&position(0, 4)));
        assert!(!hello_range.contains_position(&position(0, 5)));
        assert!(!hello_range.contains_position(&position(1, 0)));
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "assertion `left == right` failed")]
    fn contains_position_rejects_another_document() {
        let text = "hello".to_string();
        let a = Document::new(DocumentId::new("a.hop").unwrap(), text.clone());
        let b = Document::new(DocumentId::new("b.hop").unwrap(), text);
        let range = a.cursor().next().unwrap();

        range.contains_position(&b.position(PositionEncoding::Utf32, 0, 0).unwrap());
    }

    #[test]
    fn position_rejects_columns_outside_the_line() {
        let document = Document::new(
            DocumentId::new("test.hop").unwrap(),
            "\u{1F3A8}b\nc".to_string(),
        );

        // Line 0 is 🎨b: 2 code points, 3 UTF-16 code units. The column
        // just past the last character is the end of the line and valid.
        assert!(document.position(PositionEncoding::Utf32, 0, 2).is_some());
        assert!(document.position(PositionEncoding::Utf32, 0, 3).is_none());
        assert!(document.position(PositionEncoding::Utf16, 0, 3).is_some());
        assert!(document.position(PositionEncoding::Utf16, 0, 4).is_none());

        // A UTF-16 column between the surrogate halves is not a position.
        assert!(document.position(PositionEncoding::Utf16, 0, 1).is_none());

        // Line 1 is the last line and has no trailing newline.
        assert!(document.position(PositionEncoding::Utf32, 1, 1).is_some());
        assert!(document.position(PositionEncoding::Utf32, 1, 2).is_none());
        assert!(document.position(PositionEncoding::Utf32, 2, 0).is_none());
    }

    #[test]
    fn position_converts_back_to_either_encoding() {
        let document = Document::new(
            DocumentId::new("test.hop").unwrap(),
            "a\u{20AC}\n\u{1F3A8}c".to_string(),
        );

        let position = document.position(PositionEncoding::Utf16, 1, 2).unwrap();
        assert_eq!(utf16(&position), (1, 2));
        assert_eq!(utf32(&position), (1, 1));
        assert_eq!(
            position,
            document.position(PositionEncoding::Utf32, 1, 1).unwrap()
        );
    }

    #[test]
    fn string_cursor_utf32_single_line() {
        // "a\u{20AC}b\u{1F3A8}c" - Testing UTF-32 (character count)
        // a = 1 char, \u{20AC} (€) = 1 char, b = 1 char, \u{1F3A8} (🎨) = 1 char, c = 1 char
        // UTF-8:  a(1) €(3) b(1) 🎨(4) c(1) = byte positions
        // UTF-16: a(1) €(1) b(1) 🎨(2) c(1) = code unit positions
        // UTF-32: a(1) €(1) b(1) 🎨(1) c(1) = character positions 0,1,2,3,4,5
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "a\u{20AC}b\u{1F3A8}c".to_string(),
        );

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), 'a');
        assert_eq!(utf32(&range1.start_position()), (0, 0));
        assert_eq!(utf32(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\u{20AC}');
        assert_eq!(utf32(&range2.start_position()), (0, 1));
        assert_eq!(utf32(&range2.end_position()), (0, 2));

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), 'b');
        assert_eq!(utf32(&range3.start_position()), (0, 2));
        assert_eq!(utf32(&range3.end_position()), (0, 3));

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), '\u{1F3A8}');
        assert_eq!(utf32(&range4.start_position()), (0, 3));
        assert_eq!(utf32(&range4.end_position()), (0, 4));

        let range5 = cursor.next().unwrap();
        assert_eq!(range5.ch(), 'c');
        assert_eq!(utf32(&range5.start_position()), (0, 4));
        assert_eq!(utf32(&range5.end_position()), (0, 5));
    }

    #[test]
    fn string_cursor_utf32_multiline() {
        // "\u{1F3A8}\n\u{20AC}x" - Testing UTF-32 with newlines
        // Line 0: 🎨(1 char) \n(1 char)
        // Line 1: €(1 char) x(1 char)
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "\u{1F3A8}\n\u{20AC}x".to_string(),
        );

        let range1 = cursor.next().unwrap();
        assert_eq!(range1.ch(), '\u{1F3A8}');
        assert_eq!(utf32(&range1.start_position()), (0, 0));
        assert_eq!(utf32(&range1.end_position()), (0, 1));

        let range2 = cursor.next().unwrap();
        assert_eq!(range2.ch(), '\n');
        assert_eq!(utf32(&range2.start_position()), (0, 1));
        assert_eq!(utf32(&range2.end_position()), (1, 0));

        let range3 = cursor.next().unwrap();
        assert_eq!(range3.ch(), '\u{20AC}');
        assert_eq!(utf32(&range3.start_position()), (1, 0));
        assert_eq!(utf32(&range3.end_position()), (1, 1));

        let range4 = cursor.next().unwrap();
        assert_eq!(range4.ch(), 'x');
        assert_eq!(utf32(&range4.start_position()), (1, 1));
        assert_eq!(utf32(&range4.end_position()), (1, 2));
    }

    #[test]
    fn contains_position_utf32() {
        // "\u{1F3A8}hello" - Emoji followed by ASCII
        let document = Document::new(
            DocumentId::new("test.hop").unwrap(),
            "\u{1F3A8}hello".to_string(),
        );
        let ranges: Vec<_> = document.cursor().collect();
        let position = |line, column| {
            document
                .position(PositionEncoding::Utf32, line, column)
                .unwrap()
        };

        // Create range for "hello" (skipping the emoji)
        let hello_range = ranges[1].clone().to(ranges[5].clone());

        // Test UTF-32 position containment
        assert!(hello_range.contains_position(&position(0, 1)));
        assert!(hello_range.contains_position(&position(0, 5)));
        assert!(!hello_range.contains_position(&position(0, 0)));
        assert!(!hello_range.contains_position(&position(0, 6)));
    }

    #[test]
    fn compare_utf_encodings() {
        // "\u{1F3A8}ab" - Compare UTF-16 and UTF-32 encodings
        // 🎨 = U+1F3A8: 2 code units UTF-16, 1 char UTF-32
        let mut cursor = DocumentCursor::new(
            DocumentId::new("test.hop").unwrap(),
            "\u{1F3A8}ab".to_string(),
        );

        let emoji = cursor.next().unwrap();
        let a = cursor.next().unwrap();
        let b = cursor.next().unwrap();

        // Emoji positions
        assert_eq!(utf16(&emoji.start_position()), (0, 0));
        assert_eq!(utf16(&emoji.end_position()), (0, 2));
        assert_eq!(utf32(&emoji.start_position()), (0, 0));
        assert_eq!(utf32(&emoji.end_position()), (0, 1));

        // 'a' positions - notice different column values
        assert_eq!(utf16(&a.start_position()), (0, 2));
        assert_eq!(utf32(&a.start_position()), (0, 1));

        // 'b' positions
        assert_eq!(utf16(&b.start_position()), (0, 3));
        assert_eq!(utf32(&b.start_position()), (0, 2));
    }
}
