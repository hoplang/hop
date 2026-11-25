; Highlighting queries for hop, in Zed's capture vocabulary.

(identifier) @variable
(type_identifier) @type

((type_identifier) @type.builtin
  (#any-of? @type.builtin "Int" "String" "Bool" "Float" "Html" "Array" "Option"))

; The Helix query also marks a bare type_identifier in expression or pattern
; position with (_expression/type_identifier) and (_pattern/type_identifier).
; Those supertype patterns crash the tree-sitter WASM loader in Zed 0.224, see
; https://github.com/tree-sitter/tree-sitter/pull/5606, so they are left out
; until Zed ships a tree-sitter with that fix.
(scoped_identifier name: (type_identifier) @constructor)
(enum_variant name: (type_identifier) @constructor)

((type_identifier) @constant.builtin
  (#any-of? @constant.builtin "Some" "None"))

(field_identifier) @property

(parameter name: (identifier) @variable.parameter)
(rest_parameter name: (identifier) @variable.parameter)
(named_argument name: (identifier) @variable.parameter)

(function_declaration name: (identifier) @function)
(function_declaration name: (type_identifier) @constructor)

(call_expression function: (identifier) @function)
(call_expression
  function: (field_expression field: (field_identifier) @function.method))

(macro_invocation macro: (identifier) @function.macro)

(string_literal) @string
(named_argument name: (string_literal) @variable.parameter)
(escape_sequence) @string.escape
(integer_literal) @number
(float_literal) @number
(boolean_literal) @boolean
(wildcard_pattern) @variable.special

(tag_name) @tag

(function_name) @constructor

(attribute "=" @punctuation.delimiter)

(start_tag ["<" ">"] @punctuation.bracket)
(self_closing_tag ["<" "/>"] @punctuation.bracket)
(end_tag ["</" ">"] @punctuation.bracket)
(fragment_start ["<" ">"] @punctuation.bracket)
(fragment_end ["</" ">"] @punctuation.bracket)

(attribute_name) @attribute

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

; `<` and `>` are operators only inside a binary expression.
(binary_expression operator: ["<" ">"] @operator)

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
  "?"
] @punctuation.delimiter

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  "import"
  "record"
  "enum"
  "page"
  "fn"
  "let"
] @keyword

(visibility_modifier) @keyword.storage.modifier
(test_modifier) @keyword.storage.modifier

"match" @keyword.control.conditional
"for" @keyword.control.repeat
"in" @keyword.control.repeat

(comment) @comment
(markup_comment) @comment
