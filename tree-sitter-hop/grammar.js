/// <reference types="tree-sitter-cli/dsl" />

// This grammar deliberately accepts a superset of what the projects own parser
// accepts. Return types may be omitted, blocks may be empty, closing tags need
// not match their opening tag, etc.
//
// Two places stay strict, because looseness there creates real ambiguity rather
// than removing work: the void-element list is fixed (otherwise element nesting
// cannot be recovered), and text tokens are kept out of expression positions.

const PREC = {
  or: 1,
  and: 2,
  equality: 3,
  relational: 4,
  additive: 5,
  multiplicative: 6,
  unary: 7,
  postfix: 8,
};

// The HTML void elements, from crates/hop-core/src/html/element_kind.rs.
// These may close with a bare `>`, so they cannot be treated as ordinary
// elements without making every `</tag>` ambiguous.
const VOID_ELEMENTS = [
  "area",
  "base",
  "br",
  "col",
  "embed",
  "hr",
  "img",
  "input",
  "link",
  "meta",
  "source",
  "track",
  "wbr",
];

module.exports = grammar({
  name: "hop",

  extras: ($) => [/\s/, $.comment],

  word: ($) => $.identifier,

  // Record literals are allowed in `match` and `for` subject position, which
  // the hand-written parser forbids.
  //
  // Letting the GLR parser explore both readings resolves them using the
  // contents of the following block which is strictly more
  // accurate than forbidding the literal outright.
  conflicts: ($) => [[$.record_literal, $._expression]],

  supertypes: ($) => [
    $._declaration,
    $._expression,
    $._type,
    $._pattern,
    $._markup,
    $._markup_child,
    $._attribute,
    $._tag_name,
  ],

  rules: {
    source_file: ($) => repeat($._declaration),

    comment: (_) => token(seq("//", /[^\n]*/)),

    _declaration: ($) =>
      choice(
        $.import_declaration,
        $.record_declaration,
        $.enum_declaration,
        $.page_declaration,
        $.function_declaration,
      ),

    import_declaration: ($) => seq("import", field("path", $.module_path)),

    // Path segments may be capitalised: a module may be named `Card`.
    module_path: ($) => sep1(choice($.identifier, $.type_identifier), "::"),

    record_declaration: ($) =>
      seq(
        optional($.visibility_modifier),
        "record",
        field("name", $.type_identifier),
        field("body", $.field_declaration_list),
      ),

    enum_declaration: ($) =>
      seq(
        optional($.visibility_modifier),
        "enum",
        field("name", $.type_identifier),
        field("body", $.enum_variant_list),
      ),

    enum_variant_list: ($) => seq("{", commaSep($.enum_variant), "}"),

    enum_variant: ($) =>
      seq(
        field("name", $.type_identifier),
        optional(field("body", $.field_declaration_list)),
      ),

    page_declaration: ($) =>
      seq(
        optional($.visibility_modifier),
        "page",
        field("name", $.type_identifier),
        optional(field("parameters", $.parameters)),
        field("body", $.declaration_list),
      ),

    declaration_list: ($) => seq("{", repeat($._declaration), "}"),

    function_declaration: ($) =>
      seq(
        optional($.visibility_modifier),
        "fn",
        // Capitalised function names are components, e.g. `fn Card(...)`.
        field("name", choice($.identifier, $.type_identifier)),
        field("parameters", $.parameters),
        // Permissive: the parser requires a return type.
        optional(seq("->", field("return_type", $._type))),
        field("body", $.block),
      ),

    visibility_modifier: (_) => "pub",

    field_declaration_list: ($) => seq("{", commaSep($.field_declaration), "}"),

    field_declaration: ($) =>
      seq(field("name", $._field_identifier), ":", field("type", $._type)),

    parameters: ($) =>
      seq("(", commaSep(choice($.parameter, $.rest_parameter)), ")"),

    parameter: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $._type),
        optional(seq("=", field("default", $._expression))),
      ),

    rest_parameter: ($) => seq("...", field("name", $.identifier)),

    _type: ($) => choice($.generic_type, $.tuple_type, $.type_identifier),

    generic_type: ($) =>
      seq(
        field("type", $.type_identifier),
        field("type_arguments", $.type_arguments),
      ),

    type_arguments: ($) => seq("[", commaSep1($._type), "]"),

    tuple_type: ($) => seq("(", commaSep($._type), ")"),

    // Permissive: the parser requires a trailing expression and allows `let`
    // only before it. Both are optional here so that a block stays parseable
    // while it is being written.
    block: ($) =>
      seq("{", repeat($.let_statement), optional($._expression), "}"),

    let_statement: ($) =>
      seq(
        "let",
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        ";",
      ),

    _expression: ($) =>
      choice(
        $.identifier,
        $.type_identifier,
        $.scoped_identifier,
        $.string_literal,
        $.integer_literal,
        $.float_literal,
        $.boolean_literal,
        $.array_literal,
        $.record_literal,
        $.parenthesized_expression,
        $.block,
        $.unary_expression,
        $.binary_expression,
        $.field_expression,
        $.call_expression,
        $.macro_invocation,
        $.match_expression,
        $.for_expression,
        $._markup,
      ),

    scoped_identifier: ($) =>
      seq(
        field("path", $.type_identifier),
        "::",
        field("name", $.type_identifier),
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    array_literal: ($) => seq("[", commaSep($._expression), "]"),

    record_literal: ($) =>
      seq(
        field("name", choice($.type_identifier, $.scoped_identifier)),
        field("body", $.field_initializer_list),
      ),

    field_initializer_list: ($) =>
      seq("{", commaSep(choice($.field_initializer, $.spread_element)), "}"),

    field_initializer: ($) =>
      seq(
        field("field", $._field_identifier),
        ":",
        field("value", $._expression),
      ),

    spread_element: ($) => seq("...", $._expression),

    unary_expression: ($) =>
      prec(PREC.unary, seq(choice("!", "-"), $._expression)),

    binary_expression: ($) => {
      const table = [
        [PREC.or, "||"],
        [PREC.and, "&&"],
        [PREC.equality, "=="],
        [PREC.equality, "!="],
        [PREC.relational, "<"],
        [PREC.relational, ">"],
        [PREC.relational, "<="],
        [PREC.relational, ">="],
        [PREC.additive, "+"],
        [PREC.additive, "-"],
        [PREC.multiplicative, "*"],
      ];
      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              field("operator", operator),
              field("right", $._expression),
            ),
          ),
        ),
      );
    },

    field_expression: ($) =>
      prec.left(
        PREC.postfix,
        seq(
          field("value", $._expression),
          ".",
          field("field", $._field_identifier),
        ),
      ),

    call_expression: ($) =>
      prec.left(
        PREC.postfix,
        seq(field("function", $._expression), field("arguments", $.arguments)),
      ),

    macro_invocation: ($) =>
      seq(field("macro", $.identifier), "!", field("arguments", $.arguments)),

    arguments: ($) =>
      seq("(", commaSep(choice($.named_argument, $._expression)), ")"),

    named_argument: ($) =>
      seq(field("name", $.identifier), ":", field("value", $._expression)),

    match_expression: ($) =>
      seq("match", field("value", $._expression), field("body", $.match_block)),

    match_block: ($) => seq("{", commaSep($.match_arm), "}"),

    match_arm: ($) =>
      seq(field("pattern", $._pattern), "=>", field("value", $._expression)),

    for_expression: ($) =>
      seq(
        "for",
        field("pattern", choice($.identifier, $.wildcard_pattern)),
        "in",
        field("value", choice($._expression, $.range_expression)),
        field("body", $.block),
      ),

    // A range is only ever the subject of a `for`, so it is not an
    // expression in its own right and needs no precedence.
    range_expression: ($) => seq($._expression, "..=", $._expression),

    _pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.identifier,
        $.boolean_literal,
        $.type_identifier,
        $.scoped_identifier,
        $.variant_pattern,
        $.record_pattern,
      ),

    wildcard_pattern: (_) => "_",

    // A variant with positional fields, e.g. `Some(p)` or `Shape::Circle(r)`.
    variant_pattern: ($) =>
      seq(
        field("type", choice($.type_identifier, $.scoped_identifier)),
        "(",
        commaSep($._pattern),
        ")",
      ),

    record_pattern: ($) =>
      seq(
        field("type", choice($.type_identifier, $.scoped_identifier)),
        "{",
        commaSep($.field_pattern),
        "}",
      ),

    field_pattern: ($) =>
      seq(
        field("name", $._field_identifier),
        optional(seq(":", field("pattern", $._pattern))),
      ),

    _markup: ($) =>
      choice($.element, $.void_element, $.fragment, $.markup_comment),

    // Permissive: the closing tag's name is not required to match the opening
    // tag's. Correlating them would need an external scanner to carry a tag
    // stack, and mismatches are the language server's to report.
    element: ($) =>
      choice(
        seq(
          field("open_tag", $.start_tag),
          repeat($._markup_child),
          field("close_tag", $.end_tag),
        ),
        $.self_closing_tag,
      ),

    start_tag: ($) =>
      seq("<", field("name", $._tag_name), repeat($._attribute), ">"),

    self_closing_tag: ($) =>
      seq("<", field("name", $._tag_name), repeat($._attribute), "/>"),

    end_tag: ($) => seq("</", field("name", $._tag_name), ">"),

    // A void element takes no children and needs no closing tag, so a bare
    // `>` ends it.
    void_element: ($) =>
      seq(
        "<",
        field("name", alias($.void_tag_name, $.tag_name)),
        repeat($._attribute),
        choice(">", "/>"),
      ),

    fragment: ($) =>
      seq($.fragment_start, repeat($._markup_child), $.fragment_end),

    fragment_start: (_) => seq("<", ">"),
    fragment_end: (_) => seq("</", ">"),

    // A capitalised tag names a component, and is highlighted like a call to
    // one rather than like a type.
    _tag_name: ($) =>
      choice($.tag_name, alias($.type_identifier, $.component_name)),

    _markup_child: ($) =>
      choice($.text, alias($.block, $.interpolation), $._markup),

    _attribute: ($) => choice($.attribute, $.spread_attribute),

    attribute: ($) =>
      seq(
        field("name", $.attribute_name),
        optional(
          seq(
            "=",
            field(
              "value",
              choice($.quoted_attribute_value, alias($.block, $.interpolation)),
            ),
          ),
        ),
      ),

    spread_attribute: ($) => seq("...", field("name", $.identifier)),

    // Attribute values are raw: escape sequences are not interpreted, so
    // nothing inside the quotes is highlighted as an escape.
    quoted_attribute_value: ($) => seq('"', optional($.attribute_value), '"'),

    attribute_value: (_) => token.immediate(/[^"]+/),

    // Text may contain `>`, `}` and slashes, including a leading `//`: hop has
    // no comments in text position, so `text` takes lexical precedence over
    // the `comment` extra. Text is only valid between markup children, so
    // comments elsewhere are unaffected. Whitespace around text is part of it.
    text: (_) => token(prec(1, /[^<{]+/)),

    markup_comment: (_) =>
      token(seq("<!--", repeat(choice(/[^-]/, /-[^-]/, /--[^>]/)), "-->")),

    // `void_tag_name` is declared before `tag_name` so that on an exact match,
    // where the two tokens tie on length, rule order picks the void name.
    void_tag_name: (_) => token(choice(...VOID_ELEMENTS)),

    tag_name: (_) => /[a-z][a-zA-Z0-9\-_.]*/,

    attribute_name: (_) => /[a-zA-Z_][a-zA-Z0-9\-_.:]*/,

    identifier: (_) => /[a-z_][a-zA-Z0-9_]*/,

    // An identifier that names a record field, wherever fields are named.
    _field_identifier: ($) => alias($.identifier, $.field_identifier),

    type_identifier: (_) => /[A-Z][a-zA-Z0-9_]*/,

    boolean_literal: (_) => choice("true", "false"),

    integer_literal: (_) => /\d+/,

    float_literal: (_) => /\d+\.\d+/,

    string_literal: ($) =>
      seq('"', repeat(choice($.escape_sequence, $.string_content)), '"'),

    string_content: (_) => token.immediate(/[^"\\]+/),

    escape_sequence: (_) => token.immediate(seq("\\", /./)),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
