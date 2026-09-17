; Highlighting queries for hop, in Helix's capture vocabulary.
;
; Following Helix's own _jsx queries: an HTML element is @tag and a component
; invocation is @constructor, attribute names are @attribute rather than
; @tag.attribute, and tag punctuation is @punctuation.bracket rather than
; @tag.delimiter. Record fields are @variable.other.member.
;
; When several patterns capture the same node the last one wins, so the broad
; fallbacks come first and the specific cases refine them further down.

; --------------------------------------------------------------- fallbacks

(identifier) @variable
(type_identifier) @type

; ------------------------------------------------------------------- types

((type_identifier) @type.builtin
  (#any-of? @type.builtin "Int" "String" "Bool" "Float" "Html" "Array" "Option"))

; `Some` and `None` lex as type identifiers; they are the language's only
; capitalised value constructors.
((type_identifier) @constant.builtin
  (#any-of? @constant.builtin "Some" "None"))

; --------------------------------------------------------------- variables

(field_declaration name: (identifier) @variable.other.member)
(field_initializer name: (identifier) @variable.other.member)
(field_pattern name: (identifier) @variable.other.member)
(field_expression field: (identifier) @variable.other.member)

(parameter name: (identifier) @variable.parameter)
(rest_parameter name: (identifier) @variable.parameter)
(named_argument name: (identifier) @variable.parameter)

; --------------------------------------------------------------- functions

(function_declaration name: (identifier) @function)
(function_declaration name: (type_identifier) @constructor)

(call_expression function: (identifier) @function)
(call_expression function: (type_identifier) @constructor)
(call_expression
  function: (field_expression field: (identifier) @function.method))

(macro_expression macro: (identifier) @function.macro)

; ---------------------------------------------------------------- literals

(string_literal) @string
(escape_sequence) @constant.character.escape
(integer_literal) @constant.numeric.integer
(float_literal) @constant.numeric.float
(boolean_literal) @constant.builtin.boolean
(wildcard_pattern) @variable.builtin

; ------------------------------------------------------------------ markup

(tag_name) @tag
(void_tag_name) @tag

; A capitalised tag is a component invocation rather than an HTML element.
(start_tag name: (type_identifier) @constructor)
(end_tag name: (type_identifier) @constructor)
(self_closing_tag name: (type_identifier) @constructor)

(attribute "=" @punctuation.delimiter)

; Attribute values are raw: no escape sequences are interpreted inside them,
; so the whole run is a single string. This rule and the href/src rule below
; are made mutually exclusive rather than relying on order: the highlighter
; resolves overlapping captures by match start position before pattern order,
; so a later pattern cannot reliably override an earlier one here.
(quoted_attribute_value) @string

; `<` and `>` are also the relational operators, so tag punctuation is matched
; only inside the nodes where it delimits a tag.
(start_tag ["<" ">"] @punctuation.bracket)
(self_closing_tag ["<" "/>"] @punctuation.bracket)
(end_tag ["</" ">"] @punctuation.bracket)
(void_element ["<" ">" "/>"] @punctuation.bracket)
(fragment_start ["<" ">"] @punctuation.bracket)
(fragment_end ["</" ">"] @punctuation.bracket)

; Text in these elements reads as formatted prose.
((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.1)
  (#eq? @_tag "h1"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.2)
  (#eq? @_tag "h2"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.3)
  (#eq? @_tag "h3"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.4)
  (#eq? @_tag "h4"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.5)
  (#eq? @_tag "h5"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.heading.6)
  (#eq? @_tag "h6"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.bold)
  (#any-of? @_tag "strong" "b"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.italic)
  (#any-of? @_tag "em" "i"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.strikethrough)
  (#any-of? @_tag "s" "del"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.raw.inline)
  (#any-of? @_tag "code" "kbd"))

((element
  (start_tag name: (tag_name) @_tag)
  (text) @markup.link.label)
  (#eq? @_tag "a"))

(attribute_name) @attribute

; --------------------------------------------------------------- operators

[
  "=="
  "!="
  "<="
  ">="
  "&&"
  "||"
  "+"
  "-"
  "*"
  "!"
  "="
  "..="
  "..."
] @operator

[
  "->"
  "=>"
] @punctuation.delimiter

[
  ","
  ";"
  ":"
  "::"
  "."
] @punctuation.delimiter

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

; ---------------------------------------------------------------- keywords

[
  "import"
  "record"
  "enum"
  "page"
  "fn"
  "let"
] @keyword

(visibility_modifier) @keyword.storage.modifier

"match" @keyword.control.conditional
"for" @keyword.control.repeat
"in" @keyword.control.repeat

; ---------------------------------------------------------------- comments

(comment) @comment
(markup_comment) @comment
