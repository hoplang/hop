use std::collections::{BTreeMap, VecDeque};
use std::mem;

use crate::common::{ParseError, Position, Range, Ranged, StrCursor};
use crate::hop::ast::Attribute;
use crate::tui::source_annotator::Annotated;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Doctype {
        range: Range,
    },
    Comment {
        range: Range,
    },

    /// An Expression token represents an expression in the text position
    /// E.g. <div>hello {expression}<div>
    Expression {
        value: String,
        expression_range: Range,
        range: Range,
    },
    OpeningTag {
        self_closing: bool,
        tag_name: (String, Range),
        attributes: BTreeMap<String, Attribute>,
        expression: Option<(String, Range)>,
        range: Range,
    },
    ClosingTag {
        tag_name: (String, Range),
        range: Range,
    },
    Text {
        value: String,
        range: Range,
    },
}

impl Ranged for Token {
    fn range(&self) -> Range {
        match self {
            Token::Doctype { range } => *range,
            Token::Comment { range } => *range,
            Token::Expression { range, .. } => *range,
            Token::OpeningTag { range, .. } => *range,
            Token::ClosingTag { range, .. } => *range,
            Token::Text { range, .. } => *range,
        }
    }
}

impl Annotated for Token {
    fn message(&self) -> String {
        match self {
            Token::Text { .. } => {
                format!("Text")
            }
            Token::Doctype { .. } => {
                format!("Doctype")
            }
            Token::ClosingTag {
                tag_name: (tag_name, _),
                ..
            } => {
                format!("ClosingTag </{}>", tag_name)
            }
            Token::OpeningTag {
                tag_name: (tag_name, _),
                ..
            } => {
                format!("OpeningTag <{}>", tag_name)
            }
            Token::Comment { .. } => {
                format!("Comment")
            }
            Token::Expression { value, .. } => {
                format!("Expression {{{}}}", value)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenizerState {
    Text,
}

#[derive(Debug, Default)]
struct OptionalRangedString {
    value: Option<(String, Range)>,
}

impl OptionalRangedString {
    fn extend(&mut self, ch: char, r: Range) {
        match &mut self.value {
            Some(v) => {
                v.0.push(ch);
                v.1 = v.1.extend_to(r);
            }
            None => self.value = Some((String::from(ch), r)),
        }
    }
    fn consume(&mut self) -> Option<(String, Range)> {
        mem::take(&mut self.value)
    }
}

#[derive(Debug)]
struct RangedString(String, Range);

impl RangedString {
    fn init(ch: char, r: Range) -> RangedString {
        Self(String::from(ch), r)
    }
    fn push(&mut self, ch: char, r: Range) {
        self.0.push(ch);
        self.1 = self.1.extend_to(r);
    }
}

pub struct Tokenizer<'a> {
    cursor: StrCursor<'a>,
    stored_tag_name: String,
    // attributes are the attributes of the tag we're currently processing
    attributes: BTreeMap<String, Attribute>,

    // expression is the current value of the expression, it could be either on a tag or a
    // free-standing expression.
    expression: OptionalRangedString,

    tokens: VecDeque<Result<Token, ParseError>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: StrCursor::new(input),
            stored_tag_name: String::new(),
            expression: OptionalRangedString::default(),
            attributes: BTreeMap::default(),
            tokens: VecDeque::new(),
        }
    }

    fn push_attribute(
        &mut self,
        attribute_name: RangedString,
        attribute_value: Option<RangedString>,
        ends_at: Position,
    ) {
        let RangedString(attr_name, attr_name_range) = attribute_name;
        if self.attributes.contains_key(&attr_name) {
            self.tokens.push_back(Err(ParseError::duplicate_attribute(
                &attr_name,
                attr_name_range,
            )));
            return;
        }
        self.attributes.insert(
            attr_name.clone(),
            Attribute {
                name: attr_name,
                value: attribute_value.map(|RangedString(s, r)| (s, r)),
                range: Range::new(attr_name_range.start, ends_at),
            },
        );
    }

    fn fail_and_recover(&mut self, err: ParseError) -> Option<TokenizerState> {
        self.attributes.clear();
        self.expression.consume();
        self.tokens.push_back(Err(err));
        Some(TokenizerState::Text)
    }

    fn state_text(&mut self) -> Option<TokenizerState> {
        match self.cursor.next() {
            Some(('<', ch_range)) => self.state_tag_start(ch_range.start),
            Some(('{', ch_range)) => self.state_text_expression_start(ch_range.start),
            Some((ch, ch_range)) => {
                let mut value = String::from(ch);
                let mut range = ch_range;
                while let Some((ch, r)) = self.cursor.next_if(|(ch, _)| *ch != '{' && *ch != '<') {
                    value.push(ch);
                    range = range.extend_to(r);
                }
                self.tokens.push_back(Ok(Token::Text { value, range }));
                self.state_text()
            }
            None => None,
        }
    }

    /// In this state we have just seen a single '<'
    fn state_tag_start(&mut self, tag_start: Position) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('/', _) => self.state_closing_tag_start(tag_start),
            ('!', _) => self.state_markup_declaration(tag_start),
            (ch, ch_range) if ch.is_ascii_alphabetic() => {
                self.state_opening_tag_name(tag_start, RangedString::init(ch, ch_range))
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character after '<'".to_string(),
                ch_range,
            )),
        }
    }

    /// In this state we have just seen '</'
    fn state_closing_tag_start(&mut self, tag_start: Position) -> Option<TokenizerState> {
        match self.cursor.next()? {
            (ch, ch_range) if ch.is_ascii_alphabetic() => {
                self.state_closing_tag_name(tag_start, RangedString::init(ch, ch_range))
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character after '</'".to_string(),
                ch_range,
            )),
        }
    }

    fn state_opening_tag_name(
        &mut self,
        tag_start: Position,
        mut tag_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('/', _) => self.state_self_closing(tag_start, tag_name),
            ('{', _) => self.state_tag_expression_start(tag_start, tag_name),
            (ch, ch_range) if ch == '-' || ch.is_ascii_alphanumeric() => {
                tag_name.push(ch, ch_range);
                // TODO
                self.state_opening_tag_name(tag_start, tag_name)
            }
            (ch, _) if ch.is_whitespace() => self.state_before_attr_name(tag_start, tag_name),
            ('>', ch_range) => {
                let RangedString(tag_name, tag_name_range) = tag_name;
                if is_tag_name_with_raw_content(&tag_name) {
                    self.stored_tag_name = tag_name.clone();
                    self.tokens.push_back(Ok(Token::OpeningTag {
                        tag_name: (tag_name, tag_name_range),
                        self_closing: false,
                        attributes: mem::take(&mut self.attributes),
                        expression: self.expression.consume(),
                        range: Range::new(tag_start, ch_range.end),
                    }));
                    self.state_rawtext_data()
                } else {
                    self.tokens.push_back(Ok(Token::OpeningTag {
                        tag_name: (tag_name, tag_name_range),
                        self_closing: false,
                        attributes: mem::take(&mut self.attributes),
                        expression: self.expression.consume(),
                        range: Range::new(tag_start, ch_range.end),
                    }));
                    Some(TokenizerState::Text)
                }
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character after '<'".to_string(),
                ch_range,
            )),
        }
    }

    fn state_closing_tag_name(
        &mut self,
        tag_start: Position,
        mut tag_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            (ch, ch_range) if ch == '-' || ch.is_ascii_alphanumeric() => {
                tag_name.push(ch, ch_range);
                // TODO: Recursive
                self.state_closing_tag_name(tag_start, tag_name)
            }
            (ch, _) if ch.is_whitespace() => self.state_after_closing_tag_name(tag_start, tag_name),
            ('>', ch_range) => {
                let RangedString(tag_name, range) = tag_name;
                self.tokens.push_back(Ok(Token::ClosingTag {
                    tag_name: (tag_name, range),
                    range: Range::new(tag_start, ch_range.end),
                }));
                Some(TokenizerState::Text)
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character in end tag name".to_string(),
                ch_range,
            )),
        }
    }

    fn state_after_closing_tag_name(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
    ) -> Option<TokenizerState> {
        self.cursor.next_while(|(ch, _)| ch.is_whitespace());
        match self.cursor.next()? {
            ('>', ch_range) => {
                let RangedString(tag_name, tag_name_range) = tag_name;
                self.tokens.push_back(Ok(Token::ClosingTag {
                    tag_name: (tag_name, tag_name_range),
                    range: Range::new(tag_start, ch_range.end),
                }));
                Some(TokenizerState::Text)
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character after closing tag name".to_string(),
                ch_range,
            )),
        }
    }

    fn state_before_attr_name(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
    ) -> Option<TokenizerState> {
        self.cursor.next_while(|(ch, _)| ch.is_whitespace());

        match self.cursor.next()? {
            ('{', _) => self.state_tag_expression_start(tag_start, tag_name),
            ('/', _) => self.state_self_closing(tag_start, tag_name),
            (ch, ch_range) if ch.is_ascii_alphabetic() => {
                let mut attr_name = RangedString::init(ch, ch_range);
                // Parse attribute name
                while let Some((ch, ch_range)) = self
                    .cursor
                    .next_if(|(ch, _)| *ch == '-' || ch.is_ascii_alphanumeric())
                {
                    attr_name.push(ch, ch_range);
                }

                self.state_attr_name(tag_start, tag_name, attr_name)
            }
            ('>', ch_range) => {
                let RangedString(tag_name, tag_name_range) = tag_name;
                let tag_name_clone = tag_name.clone();
                self.tokens.push_back(Ok(Token::OpeningTag {
                    self_closing: false,
                    tag_name: (tag_name, tag_name_range),
                    attributes: mem::take(&mut self.attributes),
                    expression: self.expression.consume(),
                    range: Range::new(tag_start, ch_range.end),
                }));
                if is_tag_name_with_raw_content(&tag_name_clone) {
                    self.stored_tag_name = tag_name_clone;
                    self.state_rawtext_data()
                } else {
                    Some(TokenizerState::Text)
                }
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid character before attribute name".to_string(),
                ch_range,
            )),
        }
    }

    fn state_attr_name(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
        attr_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('=', _) => self.state_before_attr_value(tag_start, tag_name, attr_name),
            (ch, ch_range) if ch.is_whitespace() => {
                self.push_attribute(attr_name, None, ch_range.end);
                self.state_before_attr_name(tag_start, tag_name)
            }
            ('>', ch_range) => {
                // Push current attribute
                self.push_attribute(attr_name, None, ch_range.end);
                let RangedString(tag_name, tag_name_range) = tag_name;
                if is_tag_name_with_raw_content(&tag_name) {
                    self.stored_tag_name = tag_name.clone();
                    self.tokens.push_back(Ok(Token::OpeningTag {
                        self_closing: false,
                        tag_name: (tag_name, tag_name_range),
                        attributes: mem::take(&mut self.attributes),
                        expression: self.expression.consume(),
                        range: Range::new(tag_start, ch_range.end),
                    }));
                    self.state_rawtext_data()
                } else {
                    self.tokens.push_back(Ok(Token::OpeningTag {
                        self_closing: false,
                        tag_name: (tag_name, tag_name_range),
                        attributes: mem::take(&mut self.attributes),
                        expression: self.expression.consume(),
                        range: Range::new(tag_start, ch_range.end),
                    }));
                    Some(TokenizerState::Text)
                }
            }
            ('/', ch_range) => {
                // Push current attribute
                self.push_attribute(attr_name, None, ch_range.end);
                self.state_self_closing(tag_start, tag_name)
            }
            (_, ch_range) => {
                self.cursor.next();
                self.fail_and_recover(ParseError::new(
                    "Invalid character in attribute name".to_string(),
                    ch_range,
                ))
            }
        }
    }

    fn state_before_attr_value(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
        attr_name: RangedString,
    ) -> Option<TokenizerState> {
        self.cursor.next_while(|(ch, _)| ch.is_whitespace());
        match self.cursor.next()? {
            ('"', _) => self.state_attr_value_double_quote(tag_start, tag_name, attr_name),
            ('\'', _) => self.state_attr_value_single_quote(tag_start, tag_name, attr_name),
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Expected quoted attribute name".to_string(),
                ch_range,
            )),
        }
    }

    fn state_attr_value_double_quote(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
        attr_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('"', ch_range) => {
                self.push_attribute(attr_name, None, ch_range.end);
                self.state_before_attr_name(tag_start, tag_name)
            }
            (ch, ch_range) => {
                let mut attr_value = RangedString::init(ch, ch_range);
                while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| *ch != '"') {
                    attr_value.push(ch, range);
                }
                let (_, end_range) = self.cursor.next()?; // consume "
                // TODO: Handle EOF
                self.push_attribute(attr_name, Some(attr_value), end_range.end);
                self.state_before_attr_name(tag_start, tag_name)
            }
        }
    }

    fn state_attr_value_single_quote(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
        attr_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('\'', ch_range) => {
                self.push_attribute(attr_name, None, ch_range.end);
                self.state_before_attr_name(tag_start, tag_name)
            }
            (ch, ch_range) => {
                let mut attr_value = RangedString::init(ch, ch_range);
                while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| *ch != '\'') {
                    attr_value.push(ch, range);
                }
                let (_, end_range) = self.cursor.next()?; // consume '
                // TODO: Handle EOF
                self.push_attribute(attr_name, Some(attr_value), end_range.end);
                self.state_before_attr_name(tag_start, tag_name)
            }
        }
    }

    fn state_self_closing(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('>', ch_range) => {
                let RangedString(tag_name, tag_name_range) = tag_name;
                self.tokens.push_back(Ok(Token::OpeningTag {
                    self_closing: true,
                    tag_name: (tag_name, tag_name_range),
                    attributes: mem::take(&mut self.attributes),
                    expression: self.expression.consume(),
                    range: Range::new(tag_start, ch_range.end),
                }));
                Some(TokenizerState::Text)
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Expected '>' after '/'".to_string(),
                ch_range,
            )),
        }
    }

    fn state_markup_declaration(&mut self, tag_start: Position) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('-', _) if self.cursor.peek()?.0 == '-' => {
                self.cursor.next();
                self.state_comment(tag_start)
            }
            ('D', _) if self.cursor.peek_n(6)?.0 == "OCTYPE" => {
                self.cursor.next_n(6);
                self.state_doctype(tag_start)
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Invalid markup declaration".to_string(),
                ch_range,
            )),
        }
    }

    fn state_comment(&mut self, tag_start: Position) -> Option<TokenizerState> {
        loop {
            if let ('-', _) = self.cursor.next()? {
                if let ('-', _) = self.cursor.next()? {
                    if let ('>', ch_range) = self.cursor.next()? {
                        self.tokens.push_back(Ok(Token::Comment {
                            range: Range::new(tag_start, ch_range.end),
                        }));
                        return Some(TokenizerState::Text);
                    }
                }
            }
        }
    }

    fn state_doctype(&mut self, tag_start: Position) -> Option<TokenizerState> {
        self.cursor.next_while(|(ch, _)| *ch != '>');
        match self.cursor.next()? {
            ('>', ch_range) => {
                self.tokens.push_back(Ok(Token::Doctype {
                    range: Range::new(tag_start, ch_range.end),
                }));
                Some(TokenizerState::Text)
            }
            (_, ch_range) => self.fail_and_recover(ParseError::new(
                "Expected whitespace after DOCTYPE".to_string(),
                ch_range,
            )),
        }
    }

    fn state_rawtext_data(&mut self) -> Option<TokenizerState> {
        let mut text = OptionalRangedString::default();
        loop {
            match self.cursor.next()? {
                ('<', langle_range) => {
                    let end_tag = format!("/{}>", self.stored_tag_name);
                    if self.cursor.peek_n(end_tag.len())?.0 == end_tag {
                        if let Some((s, r)) = text.consume() {
                            self.tokens
                                .push_back(Ok(Token::Text { value: s, range: r }));
                        }
                        let tag_name = self.stored_tag_name.clone();
                        self.cursor.next_n(1).unwrap(); // consume /
                        let (_, tag_name_range) =
                            self.cursor.next_n(self.stored_tag_name.len()).unwrap();
                        let (_, end_range) = self.cursor.next().unwrap(); // consume >
                        self.tokens.push_back(Ok(Token::ClosingTag {
                            tag_name: (tag_name, tag_name_range),
                            range: Range::new(langle_range.start, end_range.end),
                        }));
                        return Some(TokenizerState::Text);
                    } else {
                        text.extend('<', langle_range);
                    }
                }
                (ch, ch_range) => {
                    text.extend(ch, ch_range);
                }
            }
        }
    }

    fn state_text_expression_start(
        &mut self,
        expression_start: Position,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('}', ch_range) => {
                self.fail_and_recover(ParseError::new("Empty expression".to_string(), ch_range))
            }
            (ch, ch_range) => {
                let mut text = RangedString::init(ch, ch_range);
                // TODO: Handle EOF
                while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| *ch != '}') {
                    text.push(ch, range);
                }
                let RangedString(expression_value, expression_range) = text;
                let (_, end_range) = self.cursor.next()?; // consume }
                self.tokens.push_back(Ok(Token::Expression {
                    value: expression_value,
                    expression_range,
                    range: Range::new(expression_start, end_range.end),
                }));
                Some(TokenizerState::Text)
            }
        }
    }

    fn state_tag_expression_start(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
    ) -> Option<TokenizerState> {
        match self.cursor.next()? {
            ('}', ch_range) => {
                self.fail_and_recover(ParseError::new("Empty expression".to_string(), ch_range))
            }
            (ch, ch_range) => {
                self.expression.extend(ch, ch_range);
                self.state_tag_expression_content(tag_start, tag_name)
            }
        }
    }

    fn state_tag_expression_content(
        &mut self,
        tag_start: Position,
        tag_name: RangedString,
    ) -> Option<TokenizerState> {
        // Temporary fix until we embed hop tokenizer here
        if self.cursor.matches_str("}>")
            || self.cursor.matches_str("} >")
            || self.cursor.matches_str("}/>")
            || self.cursor.matches_str("} />")
        {
            self.cursor.next();
            self.state_before_attr_name(tag_start, tag_name)
        } else {
            let (ch, ch_range) = self.cursor.next()?;
            self.expression.extend(ch, ch_range);
            // TODO: Recursive
            self.state_tag_expression_content(tag_start, tag_name)
        }
    }

    fn advance(&mut self) -> Option<Result<Token, ParseError>> {
        self.state_text();
        self.tokens.pop_front()
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance()
    }
}

fn is_tag_name_with_raw_content(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-x-raw")
}

#[cfg(test)]
mod tests {
    use crate::tui::source_annotator::{SimpleAnnotation, SourceAnnotator};

    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::new(input);
        let result: Vec<_> = tokenizer.collect();

        // Validate that ranges are contiguous
        let mut iter = result.iter().peekable();
        while let Some(token_result) = iter.next() {
            if let (Ok(current_token), Some(Ok(next_token))) = (token_result, iter.peek()) {
                let current_range = current_token.range();
                let next_range = next_token.range();
                if current_range.end != next_range.start {
                    panic!(
                        "Non-contiguous ranges detected: token ends at {:?}, but next token starts at {:?}. \
                         Current token: {:?}, Next token: {:?}",
                        current_range.end, next_range.start, current_token, next_token
                    );
                }
            }
        }

        let mut annotations = Vec::new();
        for r in result {
            match r {
                Err(err) => {
                    annotations.push(SimpleAnnotation {
                        message: err.message(),
                        range: err.range(),
                    });
                }
                Ok(ok) => {
                    annotations.push(SimpleAnnotation {
                        message: ok.message(),
                        range: ok.range(),
                    });
                }
            }
        }

        expected.assert_eq(&SourceAnnotator::new().annotate(None, input, &annotations));
    }

    #[test]
    fn test_tokenize_empty() {
        check("", expect![""]);
    }

    #[test]
    fn test_tokenize_input_with_attributes() {
        check(
            r#"<input type="" value="" disabled="">"#,
            expect![[r#"
                OpeningTag <input>
                1 | <input type="" value="" disabled="">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_attributes_without_spaces() {
        check(
            r#"<h1 foo="bar"x="y">"#,
            expect![[r#"
                OpeningTag <h1>
                1 | <h1 foo="bar"x="y">
                  | ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_over_multiple_lines() {
        check(
            "this\ntext\nspans\nmultiple lines",
            expect![[r#"
                Text
                1 | this
                  | ^^^^
                2 | text
                  | ^^^^
                3 | spans
                  | ^^^^^
                4 | multiple lines
                  | ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_attributes() {
        check(
            "<h1 foo bar/>",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1 foo bar/>
                  | ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing() {
        check(
            "<h1 foo/>",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1 foo/>
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_text() {
        check(
            indoc! {"
                <p><!-- -->
                </p>
            "},
            expect![[r#"
                OpeningTag <p>
                1 | <p><!-- -->
                  | ^^^

                Comment
                1 | <p><!-- -->
                  |    ^^^^^^^^

                Text
                1 | <p><!-- -->
                2 | </p>

                ClosingTag </p>
                2 | </p>
                  | ^^^^

                Text
                2 | </p>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_textarea_with_content() {
        check(
            indoc! {"
                <textarea>
                	<div></div>
                </textarea>
            "},
            expect![[r#"
                OpeningTag <textarea>
                1 | <textarea>
                  | ^^^^^^^^^^

                Text
                1 | <textarea>
                2 |     <div></div>
                  | ^^^^

                OpeningTag <div>
                2 |     <div></div>
                  |     ^^^^^

                ClosingTag </div>
                2 |     <div></div>
                  |          ^^^^^^

                Text
                2 |     <div></div>
                3 | </textarea>

                ClosingTag </textarea>
                3 | </textarea>
                  | ^^^^^^^^^^^

                Text
                3 | </textarea>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_title_with_self_closing() {
        check(
            "<title><slot-title/></title>",
            expect![[r#"
                OpeningTag <title>
                1 | <title><slot-title/></title>
                  | ^^^^^^^

                OpeningTag <slot-title>
                1 | <title><slot-title/></title>
                  |        ^^^^^^^^^^^^^

                ClosingTag </title>
                1 | <title><slot-title/></title>
                  |                     ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_simple() {
        check(
            "<p><!-- --></p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p><!-- --></p>
                  | ^^^

                Comment
                1 | <p><!-- --></p>
                  |    ^^^^^^^^

                ClosingTag </p>
                1 | <p><!-- --></p>
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_standalone() {
        check(
            "<!-- Comment with -- dashes -- inside -->",
            expect![[r#"
                Comment
                1 | <!-- Comment with -- dashes -- inside -->
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_comment() {
        check(
            indoc! {"
                <p><!--
                A comment
                that stretches
                over several
                lines --></p>
            "},
            expect![[r#"
                OpeningTag <p>
                1 | <p><!--
                  | ^^^

                Comment
                1 | <p><!--
                  |    ^^^^
                2 | A comment
                  | ^^^^^^^^^
                3 | that stretches
                  | ^^^^^^^^^^^^^^
                4 | over several
                  | ^^^^^^^^^^^^
                5 | lines --></p>
                  | ^^^^^^^^^

                ClosingTag </p>
                5 | lines --></p>
                  |          ^^^^

                Text
                5 | lines --></p>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_quotes() {
        check(
            r#"<!-- This comment has <tags> and "quotes" and 'apostrophes' -->"#,
            expect![[r#"
                Comment
                1 | <!-- This comment has <tags> and "quotes" and 'apostrophes' -->
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_doctype() {
        check(
            "<!DOCTYPE   html>",
            expect![[r#"
                Doctype
                1 | <!DOCTYPE   html>
                  | ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_end_tag_with_space() {
        check(
            "</div >",
            expect![[r#"
                ClosingTag </div>
                1 | </div >
                  | ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_simple() {
        check(
            "<h1/>",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1/>
                  | ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_space() {
        check(
            "<h1 />",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1 />
                  | ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_style_with_content() {
        check(
            indoc! {"
                <style>
                  body { color: red; }
                  .class { font-size: 12px; }
                </style>
            "},
            expect![[r#"
                OpeningTag <style>
                1 | <style>
                  | ^^^^^^^

                Text
                1 | <style>
                2 |   body { color: red; }
                  | ^^^^^^^^^^^^^^^^^^^^^^
                3 |   .class { font-size: 12px; }
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 | </style>

                ClosingTag </style>
                4 | </style>
                  | ^^^^^^^^

                Text
                4 | </style>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiple_headings() {
        check(
            indoc! {"
                <h1></h1>
                <h2></h2>
                <h3></h3>
                <h4></h4>
                <h5></h5>
                <h6></h6>
            "},
            expect![[r#"
                OpeningTag <h1>
                1 | <h1></h1>
                  | ^^^^

                ClosingTag </h1>
                1 | <h1></h1>
                  |     ^^^^^

                Text
                1 | <h1></h1>
                2 | <h2></h2>

                OpeningTag <h2>
                2 | <h2></h2>
                  | ^^^^

                ClosingTag </h2>
                2 | <h2></h2>
                  |     ^^^^^

                Text
                2 | <h2></h2>
                3 | <h3></h3>

                OpeningTag <h3>
                3 | <h3></h3>
                  | ^^^^

                ClosingTag </h3>
                3 | <h3></h3>
                  |     ^^^^^

                Text
                3 | <h3></h3>
                4 | <h4></h4>

                OpeningTag <h4>
                4 | <h4></h4>
                  | ^^^^

                ClosingTag </h4>
                4 | <h4></h4>
                  |     ^^^^^

                Text
                4 | <h4></h4>
                5 | <h5></h5>

                OpeningTag <h5>
                5 | <h5></h5>
                  | ^^^^

                ClosingTag </h5>
                5 | <h5></h5>
                  |     ^^^^^

                Text
                5 | <h5></h5>
                6 | <h6></h6>

                OpeningTag <h6>
                6 | <h6></h6>
                  | ^^^^

                ClosingTag </h6>
                6 | <h6></h6>
                  |     ^^^^^

                Text
                6 | <h6></h6>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_attributes() {
        check(
            indoc! {r#"
                <div
                  class="multiline"
                  id="test"
                  data-value="something">
                </div>
            "#},
            expect![[r#"
                OpeningTag <div>
                1 | <div
                  | ^^^^
                2 |   class="multiline"
                  | ^^^^^^^^^^^^^^^^^^^
                3 |   id="test"
                  | ^^^^^^^^^^^
                4 |   data-value="something">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^

                Text
                4 |   data-value="something">
                5 | </div>

                ClosingTag </div>
                5 | </div>
                  | ^^^^^^

                Text
                5 | </div>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_script_with_content() {
        check(
            indoc! {r#"
                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </script>
            "#},
            expect![[r#"
                OpeningTag <script>
                1 | <script>
                  | ^^^^^^^^

                Text
                1 | <script>
                2 |   const html = "<title>Nested</title>";
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                3 |   document.write(html);
                  | ^^^^^^^^^^^^^^^^^^^^^^^
                4 | </script>

                ClosingTag </script>
                4 | </script>
                  | ^^^^^^^^^

                Text
                4 | </script>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_simple() {
        check(
            "<hop-x-raw>foo bar</hop-x-raw>",
            expect![[r#"
                OpeningTag <hop-x-raw>
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  | ^^^^^^^^^^^

                Text
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  |            ^^^^^^^

                ClosingTag </hop-x-raw>
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  |                   ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_with_html() {
        check(
            indoc! {"
                <hop-x-raw>
                  <div>some html</div>
                </hop-x-raw>
            "},
            expect![[r#"
                OpeningTag <hop-x-raw>
                1 | <hop-x-raw>
                  | ^^^^^^^^^^^

                Text
                1 | <hop-x-raw>
                2 |   <div>some html</div>
                  | ^^^^^^^^^^^^^^^^^^^^^^
                3 | </hop-x-raw>

                ClosingTag </hop-x-raw>
                3 | </hop-x-raw>
                  | ^^^^^^^^^^^^

                Text
                3 | </hop-x-raw>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_adjacent_elements() {
        check(
            "<p></p><p></p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p></p><p></p>
                  | ^^^

                ClosingTag </p>
                1 | <p></p><p></p>
                  |    ^^^^

                OpeningTag <p>
                1 | <p></p><p></p>
                  |        ^^^

                ClosingTag </p>
                1 | <p></p><p></p>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_expression() {
        check(
            indoc! {r#"
                <main-comp {foo}>
                	<script>
                		const x = "<div></div>";
                	</script>
                </main-comp>
            "#},
            expect![[r#"
                OpeningTag <main-comp>
                1 | <main-comp {foo}>
                  | ^^^^^^^^^^^^^^^^^

                Text
                1 | <main-comp {foo}>
                2 |     <script>
                  | ^^^^

                OpeningTag <script>
                2 |     <script>
                  |     ^^^^^^^^

                Text
                2 |     <script>
                3 |         const x = "<div></div>";
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 |     </script>
                  | ^^^^

                ClosingTag </script>
                4 |     </script>
                  |     ^^^^^^^^^

                Text
                4 |     </script>
                5 | </main-comp>

                ClosingTag </main-comp>
                5 | </main-comp>
                  | ^^^^^^^^^^^^

                Text
                5 | </main-comp>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_full_html_document() {
        check(
            indoc! {r#"
                <!DOCTYPE html>
                <html>
                <head>
                  <title>My Page</title>
                </head>
                <body>
                  <div class="container">
                    Hello, world!
                  </div>
                </body>
                </html>
            "#},
            expect![[r#"
                Doctype
                 1 | <!DOCTYPE html>
                   | ^^^^^^^^^^^^^^^

                Text
                 1 | <!DOCTYPE html>
                 2 | <html>

                OpeningTag <html>
                 2 | <html>
                   | ^^^^^^

                Text
                 2 | <html>
                 3 | <head>

                OpeningTag <head>
                 3 | <head>
                   | ^^^^^^

                Text
                 3 | <head>
                 4 |   <title>My Page</title>
                   | ^^

                OpeningTag <title>
                 4 |   <title>My Page</title>
                   |   ^^^^^^^

                Text
                 4 |   <title>My Page</title>
                   |          ^^^^^^^

                ClosingTag </title>
                 4 |   <title>My Page</title>
                   |                 ^^^^^^^^

                Text
                 4 |   <title>My Page</title>
                 5 | </head>

                ClosingTag </head>
                 5 | </head>
                   | ^^^^^^^

                Text
                 5 | </head>
                 6 | <body>

                OpeningTag <body>
                 6 | <body>
                   | ^^^^^^

                Text
                 6 | <body>
                 7 |   <div class="container">
                   | ^^

                OpeningTag <div>
                 7 |   <div class="container">
                   |   ^^^^^^^^^^^^^^^^^^^^^^^

                Text
                 7 |   <div class="container">
                 8 |     Hello, world!
                   | ^^^^^^^^^^^^^^^^^
                 9 |   </div>
                   | ^^

                ClosingTag </div>
                 9 |   </div>
                   |   ^^^^^^

                Text
                 9 |   </div>
                10 | </body>

                ClosingTag </body>
                10 | </body>
                   | ^^^^^^^

                Text
                10 | </body>
                11 | </html>

                ClosingTag </html>
                11 | </html>
                   | ^^^^^^^

                Text
                11 | </html>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_svg_with_many_attributes() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                </svg>
            "#},
            expect![[r#"
                OpeningTag <svg>
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>

                OpeningTag <line>
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </line>
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                  |                                             ^^^^^^^

                Text
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>

                OpeningTag <path>
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                  |                                                                                                                                     ^^^^^^^

                Text
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>

                OpeningTag <polyline>
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </polyline>
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                  |                                                  ^^^^^^^^^^^

                Text
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>

                OpeningTag <line>
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </line>
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                  |                                          ^^^^^^^

                Text
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                6 | </svg>

                ClosingTag </svg>
                6 | </svg>
                  | ^^^^^^

                Text
                6 | </svg>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_nested_svg() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                	<g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                		<path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                		<path d="M17.54 47.09v48l35.099 12.775"></path>
                		<path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                	</g>
                </svg>
            "#},
            expect![[r#"
                OpeningTag <svg>
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                  | ^^^^

                OpeningTag <g>
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  | ^^^^^^^^

                OpeningTag <path>
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  |                                                   ^^^^^^^

                Text
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  | ^^^^^^^^

                OpeningTag <path>
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  |                                                 ^^^^^^^

                Text
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  | ^^^^^^^^

                OpeningTag <path>
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  |                                                           ^^^^^^^

                Text
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                6 |     </g>
                  | ^^^^

                ClosingTag </g>
                6 |     </g>
                  |     ^^^^

                Text
                6 |     </g>
                7 | </svg>

                ClosingTag </svg>
                7 | </svg>
                  | ^^^^^^

                Text
                7 | </svg>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_input() {
        check(
            indoc! {r#"
                <main-comp>
                	<form id="form">
                		<input type="text" required>
                		<button type="submit">Send</button>
                	</form>
                </main-comp>
            "#},
            expect![[r#"
                OpeningTag <main-comp>
                1 | <main-comp>
                  | ^^^^^^^^^^^

                Text
                1 | <main-comp>
                2 |     <form id="form">
                  | ^^^^

                OpeningTag <form>
                2 |     <form id="form">
                  |     ^^^^^^^^^^^^^^^^

                Text
                2 |     <form id="form">
                3 |         <input type="text" required>
                  | ^^^^^^^^

                OpeningTag <input>
                3 |         <input type="text" required>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text
                3 |         <input type="text" required>
                4 |         <button type="submit">Send</button>
                  | ^^^^^^^^

                OpeningTag <button>
                4 |         <button type="submit">Send</button>
                  |         ^^^^^^^^^^^^^^^^^^^^^^

                Text
                4 |         <button type="submit">Send</button>
                  |                               ^^^^

                ClosingTag </button>
                4 |         <button type="submit">Send</button>
                  |                                   ^^^^^^^^^

                Text
                4 |         <button type="submit">Send</button>
                5 |     </form>
                  | ^^^^

                ClosingTag </form>
                5 |     </form>
                  |     ^^^^^^^

                Text
                5 |     </form>
                6 | </main-comp>

                ClosingTag </main-comp>
                6 | </main-comp>
                  | ^^^^^^^^^^^^

                Text
                6 | </main-comp>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_expression() {
        check(
            "<if {foo}>",
            expect![[r#"
                OpeningTag <if>
                1 | <if {foo}>
                  | ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_class_and_expression() {
        check(
            "<div class=\"test\" {bar}>",
            expect![[r#"
                OpeningTag <div>
                1 | <div class="test" {bar}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_equality_expression() {
        check(
            "<if {user.name == 'John'}>",
            expect![[r#"
                OpeningTag <if>
                1 | <if {user.name == 'John'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_property_access() {
        check(
            "<component {obj.prop.subprop}>",
            expect![[r#"
                OpeningTag <component>
                1 | <component {obj.prop.subprop}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_button_with_attribute_and_expression() {
        check(
            "<button disabled {enabled == 'yes'}>",
            expect![[r#"
                OpeningTag <button>
                1 | <button disabled {enabled == 'yes'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_input_with_variable_name() {
        check(
            "<input {variable_name_123}>",
            expect![[r#"
                OpeningTag <input>
                1 | <input {variable_name_123}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_spaced_expression() {
        check(
            "<div class=\"test\" {  user.name  }>",
            expect![[r#"
                OpeningTag <div>
                1 | <div class="test" {  user.name  }>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_span_with_string_expression() {
        check(
            "<span {'hello world'}>",
            expect![[r#"
                OpeningTag <span>
                1 | <span {'hello world'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_parenthesized_expression() {
        check(
            "<form {(user.role == 'admin')}>",
            expect![[r#"
                OpeningTag <form>
                1 | <form {(user.role == 'admin')}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_section_with_chained_equality() {
        check(
            "<section {a == b == c}>",
            expect![[r#"
                OpeningTag <section>
                1 | <section {a == b == c}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_in_expression() {
        check(
            "<for {user in users}>",
            expect![[r#"
                OpeningTag <for>
                1 | <for {user in users}>
                  | ^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_property_access() {
        check(
            "<for {item in user.items}>",
            expect![[r#"
                OpeningTag <for>
                1 | <for {item in user.items}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_in_expression() {
        check(
            "<div {foo in bars}>",
            expect![[r#"
                OpeningTag <div>
                1 | <div {foo in bars}>
                  | ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_single_expression() {
        check(
            "<h1>Hello {name}!</h1>",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1>Hello {name}!</h1>
                  | ^^^^

                Text
                1 | <h1>Hello {name}!</h1>
                  |     ^^^^^^

                Expression {name}
                1 | <h1>Hello {name}!</h1>
                  |           ^^^^^^

                Text
                1 | <h1>Hello {name}!</h1>
                  |                 ^

                ClosingTag </h1>
                1 | <h1>Hello {name}!</h1>
                  |                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_multiple_expressions() {
        check(
            "<p>User {user.name} has {user.count} items</p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p>User {user.name} has {user.count} items</p>
                  | ^^^

                Text
                1 | <p>User {user.name} has {user.count} items</p>
                  |    ^^^^^

                Expression {user.name}
                1 | <p>User {user.name} has {user.count} items</p>
                  |         ^^^^^^^^^^^

                Text
                1 | <p>User {user.name} has {user.count} items</p>
                  |                    ^^^^^

                Expression {user.count}
                1 | <p>User {user.name} has {user.count} items</p>
                  |                         ^^^^^^^^^^^^

                Text
                1 | <p>User {user.name} has {user.count} items</p>
                  |                                     ^^^^^^

                ClosingTag </p>
                1 | <p>User {user.name} has {user.count} items</p>
                  |                                           ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_start() {
        check(
            "<span>{greeting} world!</span>",
            expect![[r#"
                OpeningTag <span>
                1 | <span>{greeting} world!</span>
                  | ^^^^^^

                Expression {greeting}
                1 | <span>{greeting} world!</span>
                  |       ^^^^^^^^^^

                Text
                1 | <span>{greeting} world!</span>
                  |                 ^^^^^^^

                ClosingTag </span>
                1 | <span>{greeting} world!</span>
                  |                        ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_end() {
        check(
            "<div>Price: {price}</div>",
            expect![[r#"
                OpeningTag <div>
                1 | <div>Price: {price}</div>
                  | ^^^^^

                Text
                1 | <div>Price: {price}</div>
                  |      ^^^^^^^

                Expression {price}
                1 | <div>Price: {price}</div>
                  |             ^^^^^^^

                ClosingTag </div>
                1 | <div>Price: {price}</div>
                  |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_only_expression() {
        check(
            "<h2>{title}</h2>",
            expect![[r#"
                OpeningTag <h2>
                1 | <h2>{title}</h2>
                  | ^^^^

                Expression {title}
                1 | <h2>{title}</h2>
                  |     ^^^^^^^

                ClosingTag </h2>
                1 | <h2>{title}</h2>
                  |            ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression_in_text() {
        check(
            "<p>Status: {user.profile.status == 'active'}</p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  | ^^^

                Text
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |    ^^^^^^^^

                Expression {user.profile.status == 'active'}
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </p>
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |                                             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_expression_with_property_access() {
        check(
            "<span>Item: {item.title}</span>",
            expect![[r#"
                OpeningTag <span>
                1 | <span>Item: {item.title}</span>
                  | ^^^^^^

                Text
                1 | <span>Item: {item.title}</span>
                  |       ^^^^^^

                Expression {item.title}
                1 | <span>Item: {item.title}</span>
                  |             ^^^^^^^^^^^^

                ClosingTag </span>
                1 | <span>Item: {item.title}</span>
                  |                         ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_mixed_tag_and_text_expressions() {
        check(
            "<div {className}>Content: {content}</div>",
            expect![[r#"
                OpeningTag <div>
                1 | <div {className}>Content: {content}</div>
                  | ^^^^^^^^^^^^^^^^^

                Text
                1 | <div {className}>Content: {content}</div>
                  |                  ^^^^^^^^^

                Expression {content}
                1 | <div {className}>Content: {content}</div>
                  |                           ^^^^^^^^^

                ClosingTag </div>
                1 | <div {className}>Content: {content}</div>
                  |                                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_error() {
        check(
            r#"<div class="foo" class="bar"></div>"#,
            expect![[r#"
                Duplicate attribute 'class'
                1 | <div class="foo" class="bar"></div>
                  |                  ^^^^^

                OpeningTag <div>
                1 | <div class="foo" class="bar"></div>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </div>
                1 | <div class="foo" class="bar"></div>
                  |                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_different_quotes() {
        check(
            r#"<input type="text" type='number'/>"#,
            expect![[r#"
                Duplicate attribute 'type'
                1 | <input type="text" type='number'/>
                  |                    ^^^^

                OpeningTag <input>
                1 | <input type="text" type='number'/>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_no_value() {
        check(
            r#"<input required required />"#,
            expect![[r#"
                Duplicate attribute 'required'
                1 | <input required required />
                  |                 ^^^^^^^^

                OpeningTag <input>
                1 | <input required required />
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_empty_expression() {
        check(
            r#"{}"#,
            expect![[r#"
                Empty expression
                1 | {}
                  |  ^
            "#]],
        );
    }
}
