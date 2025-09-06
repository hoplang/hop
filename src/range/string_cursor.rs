use std::{rc::Rc, str::Chars};

use super::{Position, Range};

#[derive(Clone)]
pub struct StringCursor<'a> {
    source: Rc<String>,
    chars: Chars<'a>,
    offset: usize,
    position: Position,
}

#[derive(Debug, Clone)]
pub struct StringSpan {
    source: Rc<String>,
    pub ch: char,
    offset: (usize, usize),
    range: Range,
}

impl From<StringSpan> for (String, Range) {
    fn from(val: StringSpan) -> Self {
        (val.to_string(), val.range)
    }
}

impl StringSpan {
    // TODO: Remove this later
    pub fn new(s: String, r: Range) -> Self {
        StringSpan {
            source: Rc::new(s.clone()),
            ch: s.chars().next().unwrap(),
            offset: (0, s.len()),
            range: r,
        }
    }
    pub fn extend(self, other: StringSpan) -> Self {
        StringSpan {
            source: self.source,
            ch: self.ch,
            offset: (self.offset.0, other.offset.1),
            range: self.range.spanning(other.range),
        }
    }
    pub fn to_string(&self) -> String {
        self.source[self.offset.0..self.offset.1].to_string()
    }
    pub fn as_str(&self) -> &str {
        &self.source[self.offset.0..self.offset.1]
    }
    pub fn range(&self) -> Range {
        self.range
    }
    pub fn cursor(&self) -> StringCursor {
        StringCursor {
            chars: self.as_str().chars(),
            source: self.source.clone(),
            offset: self.offset.0,
            position: self.range.start,
        }
    }
}

impl<'a> StringCursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            offset: 0,
            position: Position::new(1, 1),
            source: Rc::new(source.to_string()),
        }
    }
}

impl Iterator for StringCursor<'_> {
    type Item = StringSpan;
    fn next(&mut self) -> Option<Self::Item> {
        let start_position = self.position;
        let start_offset = self.offset;
        self.chars.next().map(|ch| {
            self.offset += ch.len_utf8();
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += ch.len_utf8();
            }
            StringSpan {
                ch,
                source: self.source.clone(),
                offset: (start_offset, self.offset),
                range: Range::new(start_position, self.position),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_cursor_new() {
        let cursor = StringCursor::new("hello");
        assert_eq!(cursor.offset, 0);
        assert_eq!(cursor.position, Position::new(1, 1));
    }

    #[test]
    fn test_string_cursor_single_line() {
        let mut cursor = StringCursor::new("abc");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch, 'a');
        assert_eq!(span1.range.start, Position::new(1, 1));
        assert_eq!(span1.range.end, Position::new(1, 2));

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch, 'b');
        assert_eq!(span2.range.start, Position::new(1, 2));
        assert_eq!(span2.range.end, Position::new(1, 3));

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch, 'c');
        assert_eq!(span3.range.start, Position::new(1, 3));
        assert_eq!(span3.range.end, Position::new(1, 4));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_multiline() {
        let mut cursor = StringCursor::new("a\nb\nc");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch, 'a');
        assert_eq!(span1.range.start, Position::new(1, 1));
        assert_eq!(span1.range.end, Position::new(1, 2));

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch, '\n');
        assert_eq!(span2.range.start, Position::new(1, 2));
        assert_eq!(span2.range.end, Position::new(2, 1));

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch, 'b');
        assert_eq!(span3.range.start, Position::new(2, 1));
        assert_eq!(span3.range.end, Position::new(2, 2));

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch, '\n');
        assert_eq!(span4.range.start, Position::new(2, 2));
        assert_eq!(span4.range.end, Position::new(3, 1));

        let span5 = cursor.next().unwrap();
        assert_eq!(span5.ch, 'c');
        assert_eq!(span5.range.start, Position::new(3, 1));
        assert_eq!(span5.range.end, Position::new(3, 2));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_utf8() {
        let mut cursor = StringCursor::new("a€b");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch, 'a');
        assert_eq!(span1.range.end.column, 2);

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch, '€');
        assert_eq!(span2.range.start.column, 2);
        assert_eq!(span2.range.end.column, 5); // € is 3 bytes in UTF-8

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch, 'b');
        assert_eq!(span3.range.start.column, 5);
        assert_eq!(span3.range.end.column, 6);
    }

    #[test]
    fn test_string_span_new() {
        let span = StringSpan::new(
            "test".to_string(),
            Range::new(Position::new(1, 1), Position::new(1, 5)),
        );
        assert_eq!(span.ch, 't');
        assert_eq!(span.to_string(), "test");
        assert_eq!(span.as_str(), "test");
    }

    #[test]
    fn test_string_span_extend() {
        let mut cursor = StringCursor::new("abc");
        let span1 = cursor.next().unwrap();
        let _span2 = cursor.next().unwrap();
        let span3 = cursor.next().unwrap();

        let extended = span1.clone().extend(span3);
        assert_eq!(extended.ch, 'a');
        assert_eq!(extended.to_string(), "abc");
        assert_eq!(extended.range.start, Position::new(1, 1));
        assert_eq!(extended.range.end, Position::new(1, 4));
    }

    #[test]
    fn test_string_span_to_string() {
        let mut cursor = StringCursor::new("hello world");
        let spans: Vec<_> = cursor.by_ref().take(5).collect();

        assert_eq!(spans[0].to_string(), "h");
        assert_eq!(spans[1].to_string(), "e");
        assert_eq!(spans[2].to_string(), "l");
        assert_eq!(spans[3].to_string(), "l");
        assert_eq!(spans[4].to_string(), "o");

        let extended = spans[0].clone().extend(spans[4].clone());
        assert_eq!(extended.to_string(), "hello");
    }

    #[test]
    fn test_string_span_cursor() {
        let span = StringSpan::new(
            "test".to_string(),
            Range::new(Position::new(2, 3), Position::new(2, 7)),
        );
        let mut cursor = span.cursor();

        let first = cursor.next().unwrap();
        assert_eq!(first.ch, 't');
        assert_eq!(first.range.start, Position::new(2, 3));
    }

    #[test]
    fn test_string_span_from_conversion() {
        let span = StringSpan::new(
            "hello".to_string(),
            Range::new(Position::new(1, 1), Position::new(1, 6)),
        );
        let (text, range): (String, Range) = span.into();

        assert_eq!(text, "hello");
        assert_eq!(range.start, Position::new(1, 1));
        assert_eq!(range.end, Position::new(1, 6));
    }

    #[test]
    fn test_empty_string_cursor() {
        let mut cursor = StringCursor::new("");
        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_clone() {
        let cursor1 = StringCursor::new("test");
        let mut cursor2 = cursor1.clone();

        let span = cursor2.next().unwrap();
        assert_eq!(span.ch, 't');
    }
}
