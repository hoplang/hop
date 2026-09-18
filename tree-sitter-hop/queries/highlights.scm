; Highlighting queries for hop, in Helix's capture vocabulary.

(identifier) @variable
(type_identifier) @type

((type_identifier) @type.builtin
  (#any-of? @type.builtin "Int" "String" "Bool" "Float" "Html" "Array" "Option"))

(_expression/type_identifier) @constructor
(_pattern/type_identifier) @constructor
(scoped_identifier name: (type_identifier) @constructor)
(enum_variant name: (type_identifier) @constructor)

((type_identifier) @constant.builtin
  (#any-of? @constant.builtin "Some" "None"))

(field_identifier) @variable.other.member

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
(escape_sequence) @constant.character.escape
(integer_literal) @constant.numeric.integer
(float_literal) @constant.numeric.float
(boolean_literal) @constant.builtin.boolean
(wildcard_pattern) @variable.builtin

(tag_name) @tag

(component_name) @constructor

(attribute "=" @punctuation.delimiter)

(quoted_attribute_value) @string

(start_tag ["<" ">"] @punctuation.bracket)
(self_closing_tag ["<" "/>"] @punctuation.bracket)
(end_tag ["</" ">"] @punctuation.bracket)
(void_element ["<" ">" "/>"] @punctuation.bracket)
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

"match" @keyword.control.conditional
"for" @keyword.control.repeat
"in" @keyword.control.repeat

(comment) @comment
(markup_comment) @comment
