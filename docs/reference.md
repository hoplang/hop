# hop language reference

## Notation

The grammars use W3C-style EBNF:

| Notation        | Meaning                          |
| --------------- | -------------------------------- |
| `A ::= …`       | the rule `A`                     |
| `"x"`, `'x'`    | the text `x`                     |
| `A B`           | `A` followed by `B`              |
| `A \| B`        | `A` or `B`                       |
| `A?`            | `A` or nothing                   |
| `A*`            | `A` repeated zero or more times  |
| `A+`            | `A` repeated one or more times   |
| `( … )`         | grouping                         |
| `[a-z]`, `[^"]` | a character in, or not in, a set |
| `/* … */`       | a comment                        |

A comment in an example that shows markup, such as `// items: <li>Alice</li>`,
shows the [rendering](#rendering) of an `Html` value.

<a id="lexical-structure"></a>

## 1 Lexical structure

A module is a UTF-8 text file. Outside markup and string literals, whitespace
is ignored except as it separates tokens.

A comment starts with `//` and runs to the end of the line. It can appear
wherever whitespace can, except inside markup, where a
[markup comment](#markup-nodes) is written `<!-- … -->`.

```ebnf
Comment ::= "//" [^\n]*
```

<a id="identifiers"></a>

### 1.1 Identifiers

Identifiers have two forms. A lowercase identifier names a variable, a field, a
function or a macro, and an uppercase identifier names a record, an enum, a
variant, a function or a page.

```ebnf
LowercaseIdentifier ::= [a-z] ( "_"? [a-z0-9] )*
UppercaseIdentifier ::= [A-Z] [A-Za-z0-9]*
```

<a id="keywords"></a>

### 1.2 Keywords and reserved words

None of the words below can be used as an identifier.

```
enum    false   fn      for     import  in
let     match   page    pub     record  true

None    Some

Array   Bool    Float   Html    Int     Option  String
```

Further words are reserved for future use. They are listed in
[Appendix: Reserved words](#reserved-words).

<a id="types"></a>

## 2 Types

Every value has a type: one of the built-in types below, or a record or enum
type declared in a module.

```ebnf
Type ::= "Bool"
       | "Int"
       | "Float"
       | "String"
       | "Html"
       | "Array" "[" Type "]"
       | "Option" "[" Type "]"
       | "(" Type ")"
       | UppercaseIdentifier
```

An `UppercaseIdentifier` names a [record](#record-declarations) or
[enum](#enum-declarations) type. The built-in types have these values:

| Type        | Values                                  |
| ----------- | --------------------------------------- |
| `Bool`      | `true`, `false`                         |
| `Int`       | 32-bit signed integers                  |
| `Float`     | IEEE 754 binary64, including ±∞ and NaN |
| `String`    | sequences of Unicode scalar values      |
| `Html`      | sequences of HTML elements and text     |
| `Array[T]`  | sequences of `T`                        |
| `Option[T]` | `None`, `Some(v)`                       |

<a id="expressions"></a>

## 3 Expressions

An expression computes a value. Evaluation has no side effects and cannot
fail: the value of an expression depends only on the values of its parts, and
the only way an evaluation does not produce a value is that a
[recursive function](#function-declarations) does not terminate. The order in
which the parts of an expression are evaluated is otherwise not observable.

An expression is one of the forms below:

```ebnf
Expr ::= LiteralExpr
       | ArrayExpr
       | OptionExpr
       | RecordExpr
       | EnumExpr
       | VariableReferenceExpr
       | ParenExpr
       | CallExpr
       | MacroExpr
       | FieldAccessExpr
       | MethodCallExpr
       | OperatorExpr
       | BlockExpr
       | MatchExpr
       | ForExpr
       | MarkupExpr
```

<a id="literal-expressions"></a>

### 3.1 Literal expressions

A literal expression evaluates to the `Bool`, `Int`, `Float` or `String` value
it denotes, and has that type.

```ebnf
LiteralExpr    ::= BoolLiteral
                 | IntLiteral
                 | FloatLiteral
                 | StringLiteral
BoolLiteral    ::= "true" | "false"
IntLiteral     ::= "0" | [1-9] [0-9]*
FloatLiteral   ::= ( "0" | [1-9] [0-9]* ) "." [0-9]+
StringLiteral  ::= '"' StringChar* '"'
StringChar     ::= [^"\] | EscapeSequence
EscapeSequence ::= "\" [ntr\"]
```

An integer literal must fit in an `Int`. The one exception is `2147483648` as
the immediate operand of `-`, which lets `-2147483648` be written.

<a id="array-expressions"></a>

### 3.2 Array expressions

An array expression `[a, b, …]` evaluates to the array of its elements in
order, and has the type `Array[T]`, where every element must have type `T`.

```ebnf
ArrayExpr ::= "[" ( Expr ( "," Expr )* ","? )? "]"
```

An empty array `[]` does not determine `T`. It takes its type from its
context, such as the annotation in `let tags: Array[String] = [];`. An empty
array with no such context is an error:

```hop
let tags = []; // error: Cannot infer type of empty array
```

<a id="option-expressions"></a>

### 3.3 Option expressions

An option expression is `None` or `Some(e)`. `None` evaluates to the option
with no value, and `Some(e)` to the option holding the value of `e`. Both have
the type `Option[T]`, where `e` must have type `T`.

```ebnf
OptionExpr ::= "None" | "Some" "(" Expr ")"
```

Like an empty array, `None` does not determine `T` and takes its type from
its context, such as the annotation in `let nickname: Option[String] = None;`.
A `None` with no such context is an error:

```hop
let nickname = None; // error: Cannot infer type of None without context
```

<a id="record-expressions"></a>

### 3.4 Record expressions

A record expression `R {f: e, …}` evaluates to a record of type `R`. Each
entry `f: e` gives the field `f` the value of `e`.

```ebnf
RecordExpr ::= UppercaseIdentifier "{" ( ( FieldValue | Spread ) ( "," ( FieldValue | Spread ) )* ","? )? "}"
FieldValue ::= LowercaseIdentifier ":" Expr
Spread     ::= "..." Expr
```

A record expression must give every field of `R` a value, and no field more
than once.

A spread `...r` copies from `r` the fields that are not written. `r` must have
type `R`, and a record expression can have at most one spread.

```hop
record User {
  name: String,
  age: Int,
}

let u = User {name: "Alice", age: 36};

User {...u, name: "Bob"}     // User {name: "Bob", age: 36}
User {...u, age: u.age + 1}  // User {name: "Alice", age: 37}
```

<a id="enum-expressions"></a>

### 3.5 Enum expressions

An enum expression `E::V` or `E::V {f: e, …}` evaluates to the variant `V` of
the enum type `E`, and has that type.

```ebnf
EnumExpr ::= UppercaseIdentifier "::" UppercaseIdentifier ( "{" ( FieldValue ( "," FieldValue )* ","? )? "}" )?
```

The fields are written as in [record expressions](#record-expressions), except
that a spread is not allowed.

For example, with `enum Status {Active, Away {since: String}}`, both
`Status::Active` and `Status::Away {since: "Monday"}` are `Status` values.

<a id="variable-reference-expressions"></a>

### 3.6 Variable reference expressions

A variable reference expression `x` evaluates to the value that `x` is bound
to, by a parameter, a let binding, a `for` or a pattern.

```ebnf
VariableReferenceExpr ::= LowercaseIdentifier
```

A variable must be in scope:

```hop
fn answer() -> Int {
  z // error: Undefined variable: z
}
```

A binding cannot reuse a name that is already in scope:

```hop
fn double(x: Int) -> Int {
  let x = x * 2; // error: Variable x is already defined
  x
}
```

<a id="parenthesized-expressions"></a>

### 3.7 Parenthesized expressions

A parenthesized expression evaluates to the value of the expression inside the
parentheses, and has its type.

```ebnf
ParenExpr ::= "(" Expr ")"
```

Parentheses group an expression to override the
[precedence of operators](#operator-expressions), as in `(a + b) * c`.

<a id="call-expressions"></a>

### 3.8 Call expressions

A call expression `f(…)` evaluates to the value that the function `f` returns
for its arguments, and has the return type of `f`.

```ebnf
CallExpr  ::= LowercaseIdentifier "(" Arguments? ")"
Arguments ::= Expr ( "," Expr )* ","?
            | LowercaseIdentifier ":" Expr ( "," LowercaseIdentifier ":" Expr )* ","?
```

`f(a, b)` passes its arguments by position, and `f(x: a, y: b)` by name. A call
cannot mix the two. Parameters with a default value can be left out.

Only functions with lowercase names can be called this way. Functions with
uppercase names are called as [function elements](#function-elements).

<a id="macro-expressions"></a>

### 3.9 Macro expressions

A macro expression `name!(…)` calls one of the three macros [`join!`](#join),
[`format!`](#format) and [`asset!`](#asset). Any other macro name is an error.

```ebnf
MacroExpr ::= LowercaseIdentifier "!" "(" ( Expr ( "," Expr )* ","? )? ")"
```

<a id="join"></a>

#### 3.9.1 The join macro

The join macro takes any number of values of type `String` and evaluates to the
`String` that joins them with single spaces. Empty strings are not skipped, and
with no arguments it evaluates to `""`.

```hop
join!("btn", "primary")  // "btn primary"
join!("a", "", "b")      // "a  b"
join!()                  // ""
join!("btn", 1)          // error: Mismatched type for 'join': expected String got Int
```

<a id="format"></a>

#### 3.9.2 The format macro

The format macro fills in the placeholders of a template and evaluates to the
resulting `String`. The first argument is the template, which must be a string
literal, and each `{}` in it is a placeholder.

The other arguments fill in the placeholders in order, exactly one for each
`{}`. Each argument must have type `String` or `Int`, and an `Int` value is
converted to a `String` as if by `to_string()`.

To write a brace in the template, double it: `{{` stands for `{` and `}}` for
`}`. Any other brace in the template is an error.

```hop
format!("{} is {} years old", "Alice", 36)  // "Alice is 36 years old"
format!("{{{}}}", "Alice")                  // "{Alice}"
format!("{}" + "!", "Alice")                // error: format! requires a string literal as its first argument
format!("{} and {}", "Alice")               // error: format! expects 2 argument(s) for the format string, got 1
format!("{}", 1.5)                          // error: format! arguments must be String or Int, got Float
```

<a id="asset"></a>

#### 3.9.3 The asset macro

The asset macro takes one string literal, the path of a file in the project, and
evaluates to the URL of that file as a `String`. The path must start with `/`,
which stands for the project root.

```hop
asset!("/icons/star.svg")       // the URL of icons/star.svg
asset!("icons/star.svg")        // error: invalid asset! path: path must start with '/'
asset!("/icons/" + "star.svg")  // error: asset! argument must be a string literal
```

<a id="field-access-expressions"></a>

### 3.10 Field access expressions

A field access expression `r.f` evaluates to the value of the field `f` of the
record `r`, and has the type of that field.

```ebnf
FieldAccessExpr ::= Expr "." LowercaseIdentifier
```

```hop
record User {
  name: String,
  age: Int,
}

let u = User {name: "Alice", age: 36};

u.name   // "Alice"
u.email  // error: Field 'email' not found in record 'User'
```

<a id="method-call-expressions"></a>

### 3.11 Method call expressions

A method call expression `v.m()` calls one of the built-in methods below on
the value `v`, and evaluates to the result in the table. Any other method name
is an error.

```ebnf
MethodCallExpr ::= Expr "." LowercaseIdentifier "(" ")"
```

| Receiver    | Method        | Result   | Semantics                                                               |
| ----------- | ------------- | -------- | ----------------------------------------------------------------------- |
| `Array[T]`  | `len()`       | `Int`    | number of elements                                                      |
| `Array[T]`  | `is_empty()`  | `Bool`   | `true` if the array has no elements                                     |
| `String`    | `is_empty()`  | `Bool`   | `true` if the string is `""`                                            |
| `Int`       | `to_string()` | `String` | decimal representation, such as `-42`                                   |
| `Int`       | `to_float()`  | `Float`  | the same value as a float (exact, since `Int` is 32-bit)                |
| `Float`     | `to_int()`    | `Int`    | truncates toward zero, saturates at the `Int` bounds, NaN becomes `0`   |
| `Option[T]` | `is_some()`   | `Bool`   | `true` if the option is `Some(_)`                                       |
| `Option[T]` | `is_none()`   | `Bool`   | `true` if the option is `None`                                          |

<a id="operator-expressions"></a>

### 3.12 Operator expressions

An operator expression combines values with the prefix operators `!` and `-`, or
with a binary operator for comparison, arithmetic or logic.

```ebnf
OperatorExpr ::= PrefixExpr | BinaryExpr
PrefixExpr   ::= PrefixOp Expr
BinaryExpr   ::= Expr BinaryOp Expr
PrefixOp     ::= "!" | "-"
BinaryOp     ::= "||" | "&&" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*"
```

Operators group by precedence, listed here from highest to lowest:

| Precedence | Operators             | Kind    | Associativity |
| ---------- | --------------------- | ------- | ------------- |
| 1          | `.field`, `.method()` | postfix | –             |
| 2          | `!`, `-`              | prefix  | –             |
| 3          | `*`                   | binary  | left          |
| 4          | `+`, `-`              | binary  | left          |
| 5          | `<`, `>`, `<=`, `>=`  | binary  | left          |
| 6          | `==`, `!=`            | binary  | left          |
| 7          | `&&`                  | binary  | left          |
| 8          | `\|\|`                | binary  | left          |

Both operands of a binary operator must have the same type. `1 + 1.0` is an
error unless one side is converted with `to_float()` or `to_int()`.

The tables below list every combination of operator and type that is allowed,
and any other is an error. In particular, comparing an option with `None` is an
error. Whether an option is `None` is tested with `is_none()` or a `match`.

| Operator             | Operands | Result   | Semantics                         |
| -------------------- | -------- | -------- | --------------------------------- |
| `==`, `!=`           | `String` | `Bool`   | equality                          |
|                      | `Bool`   | `Bool`   | equality                          |
|                      | `Int`    | `Bool`   | equality                          |
|                      | `Float`  | `Bool`   | IEEE 754 equality                 |
| `<`, `>`, `<=`, `>=` | `Int`    | `Bool`   | numeric ordering                  |
|                      | `Float`  | `Bool`   | IEEE 754 ordering                 |
| `+`                  | `Int`    | `Int`    | addition, wraps on overflow       |
|                      | `Float`  | `Float`  | addition                          |
|                      | `String` | `String` | concatenation                     |
| `-`                  | `Int`    | `Int`    | subtraction, wraps on overflow    |
|                      | `Float`  | `Float`  | subtraction                       |
| `*`                  | `Int`    | `Int`    | multiplication, wraps on overflow |
|                      | `Float`  | `Float`  | multiplication                    |
| `&&`                 | `Bool`   | `Bool`   | logical and, short-circuiting     |
| `\|\|`               | `Bool`   | `Bool`   | logical or, short-circuiting      |

| Operator | Operand | Result  | Semantics                   |
| -------- | ------- | ------- | --------------------------- |
| `!`      | `Bool`  | `Bool`  | logical not                 |
| `-`      | `Int`   | `Int`   | negation, wraps on overflow |
|          | `Float` | `Float` | negation                    |

<a id="block-expressions"></a>

### 3.13 Block expressions

A block expression evaluates its let bindings in order, and each binding is in
scope for the rest of the block. It evaluates to the value of its last
expression, and has its type.

```ebnf
BlockExpr  ::= "{" LetBinding* Expr "}"
LetBinding ::= "let" LowercaseIdentifier ( ":" Type )? "=" Expr ";"
```

A let binding `let x = e;` binds `x` to the value of `e`. With a type
annotation, as in `let x: T = e;`, `e` must have type `T`.

```hop
let val = {
  let name = "Alice";
  let tags: Array[String] = [];
  format!("{} has {} tags", name, tags.len())
};

val // "Alice has 0 tags"
```

<a id="match-expressions"></a>

### 3.14 Match expressions

A `match` expression compares a value, the subject, with the patterns of its
arms, and evaluates to the value of the first arm whose pattern matches.

```ebnf
MatchExpr       ::= "match" Expr "{" ( MatchArm ( "," MatchArm )* ","? )? "}"
MatchArm        ::= Pattern "=>" Expr
Pattern         ::= WildcardPattern
                  | VariablePattern
                  | BoolPattern
                  | OptionPattern
                  | RecordPattern
                  | EnumPattern
```

The subject cannot be a [record](#record-expressions) or
[enum expression](#enum-expressions) with fields unless it is wrapped in
parentheses, since its `{` would be read as the start of the arms.

The subject must have type `Bool` or `Option[T]`, or a record or enum type, and
every pattern must have the type of the subject. The expressions of all arms
must have the same type, which is the type of the `match` expression.

<a id="wildcard-and-variable-patterns"></a>

#### 3.14.1 Wildcard and variable patterns

The wildcard pattern `_` and a variable pattern `x` both match any value. The
wildcard binds nothing, while the variable binds the value to `x` in the arm.

```ebnf
WildcardPattern ::= "_"
VariablePattern ::= LowercaseIdentifier
```

As the last arm, `_` matches every value that the arms before it do not:

```hop
let nickname = Some("Alice");
match nickname {
  None => "anonymous",
  _ => "known",
}
```

Inside another pattern, it ignores part of the value, as in `Some(_)` or the
field pattern `age: _` of a [record pattern](#record-patterns).

A `match` whose only arm matches every value without binding a variable is an
error:

```hop
let b = true;
// error: Useless match expression: does not branch or bind any variables
match b {
  _ => "",
}
```

<a id="bool-patterns"></a>

#### 3.14.2 Bool patterns

The patterns `true` and `false` match a `Bool` with that value and bind nothing.

```ebnf
BoolPattern ::= "true" | "false"
```

There is no `if` expression. A `match` on a `Bool` takes its place:

```hop
let signed_in = true;
match signed_in {
  true => "yes",
  false => "no",
}
```

<a id="option-patterns"></a>

#### 3.14.3 Option patterns

The pattern `None` matches `None`, and `Some(p)` matches `Some(v)` where `v`
matches the pattern `p`, binding what `p` binds.

```ebnf
OptionPattern ::= "None" | "Some" "(" Pattern ")"
```

For example:

```hop
let nickname = Some("Alice");
match nickname {
  Some(name) => name,
  None => "anonymous",
}
```

<a id="record-patterns"></a>

#### 3.14.4 Record patterns

A record pattern `R {f: p, …}` matches a record of type `R` whose fields match
their patterns, and binds what its field patterns bind. A field pattern `f`
without `: p` is short for `f: f`: it binds the field to a variable of the same
name.

```ebnf
RecordPattern ::= UppercaseIdentifier FieldPatterns
FieldPatterns ::= "{" ( FieldPattern ( "," FieldPattern )* ","? )? "}"
FieldPattern  ::= LowercaseIdentifier ( ":" Pattern )?
```

A record pattern must list every field of its type, each once. A field pattern
`f: _` matches the field without binding it. With the `User` below,
`User {name, age: _}` matches, while `User {name}` leaves out `age`:

```hop
record User {
  name: String,
  age: Int,
}

let u = User {name: "Alice", age: 36};
match u {
  // error: Record 'User' is missing fields: age
  User {name} => name,
}
```

<a id="enum-patterns"></a>

#### 3.14.5 Enum patterns

An enum pattern `E::V` or `E::V {f: p, …}` matches the variant `V` of the enum
`E` whose fields match their patterns, and binds what its field patterns bind.

```ebnf
EnumPattern ::= UppercaseIdentifier "::" UppercaseIdentifier FieldPatterns?
```

The fields are written as in [record patterns](#record-patterns), and every
field of the variant must be listed. For example:

```hop
enum Status {
  Active,
  Away {since: String},
}

let status = Status::Away {since: "Monday"};
match status {
  Status::Active => "active",
  Status::Away {since} => since,
}
```

<a id="exhaustiveness"></a>

#### 3.14.6 Exhaustiveness

The arms of a `match` must together cover every value of the subject: both
`true` and `false` for a `Bool`, both `None` and `Some` for an option, every
variant of an enum, and a record pattern for a record. A wildcard or a variable
covers every value. Coverage is checked recursively: `Some(p)`, a variant or a
record pattern covers only the values whose parts are covered by its inner
patterns, so `Some(true)` covers half of the `Some` values, and `Some(true)`
together with `Some(false)` covers them all. The compiler names the values that
no arm covers:

```hop
let flag = Some(true);
// error: Match expression is missing arms for: Some(false)
match flag {
  Some(true) => "yes",
  None => "unknown",
}
```

<a id="reachability"></a>

#### 3.14.7 Reachability

The arms are tried in order. An arm is unreachable, and an error, if every
value it matches is matched by an arm before it:

```hop
let b = true;
match b {
  _ => "a",
  // error: Unreachable match arm for pattern 'true'
  true => "b",
}
```

<a id="for-expressions"></a>

### 3.15 For expressions

A `for` expression evaluates its body, which must have type `Html`, once for
each element of an array, and evaluates to the results concatenated in order.

A `for` can also loop over a range `a..=b`: the `Int` values from `a` to `b`
inclusive. The range is empty if `a` is greater than `b`. A range is only
allowed here, not as an expression on its own.

```ebnf
ForExpr ::= "for" ( LowercaseIdentifier | "_" ) "in" Expr ( "..=" Expr )? BlockExpr
```

For example:

```hop
let items = for name in ["Alice", "Bob"] {
  <li>
    {name}
  </li>
};
let bold = for i in 1..=3 {
  <b>
    {i.to_string()}
  </b>
};
// items: <li>Alice</li><li>Bob</li>
// bold:  <b>1</b><b>2</b><b>3</b>
```

`_` in place of the variable binds nothing.

Looping over anything other than an array or a range of `Int` values is an
error:

```hop
// error: Mismatched type: expected Array[...] got String
for tag in "a, b" {
  <li>
    {tag}
  </li>
}
```

So is a body whose type is not `Html`:

```hop
fn Stars() -> Html {
  for _ in 1..=3 {
    "*" // error: Mismatched type for for body: expected Html got String
  }
}
```

<a id="markup-expressions"></a>

### 3.16 Markup expressions

An `Html` value is a sequence of elements and text. An element has a name,
attributes in the order written, and content, which is itself a sequence of
elements and text. Text is HTML text as it appears in a document, so it can
contain character references. How an `Html` value is represented during
runtime is not specified: it is built by the expressions below, combined by
placing it in the content of another expression or by a
[`for` expression](#for-expressions), and observed only when a page is
[rendered](#rendering). Rendering is deterministic: the same page with the same
arguments renders to the same bytes in every implementation.

Markup is written like HTML, with elements, attributes, text and comments, and
with expressions in braces. A markup expression is a single HTML element,
function element or fragment, and has the type `Html`.

```ebnf
MarkupExpr ::= HtmlElementExpr | FunctionElementExpr | FragmentExpr
```

<a id="html-elements"></a>

#### 3.16.1 HTML element expressions

An HTML element expression `<x …>…</x>` evaluates to the element with that
name, those attributes and that content. An element without content can be
written with a single self-closing tag, `<x/>`, which is the same as `<x></x>`.
A void element such as `<br>` never has content and is written as a single tag,
`<br>` or `<br/>`.

```ebnf
HtmlElementExpr ::= "<" HtmlElementName Attribute* ( "/>" | ">" MarkupNode* "</" HtmlElementName ">" )
                  | "<" VoidElementName Attribute* "/"? ">"
HtmlElementName ::= [a-z] [A-Za-z0-9-]*   /* except a VoidElementName */
VoidElementName ::= "area"
                  | "base"
                  | "br"
                  | "col"
                  | "embed"
                  | "hr"
                  | "img"
                  | "input"
                  | "link"
                  | "meta"
                  | "source"
                  | "track"
                  | "wbr"
```

The name must be that of an HTML or SVG element, such as `div` or `path`, or of
a custom element, which contains `-`, such as `my-widget`. Any other name, such
as `widget`, is an error.

`<html>`, `<head>` and `<body>` cannot be used, since a
[page](#page-declarations) provides them. `<style>` cannot be used either:
styles go in the project stylesheet. A `<script>` must be empty and reference a
file with `src`, as in `<script src="/app.js"></script>`.

An element is an expression: it can be bound with `let` and inserted into other
markup. For example:

```hop
let title = (
  <h2>
    Note
  </h2>
);
let section = (
  <section>
    {title}
  </section>
);
// title:   <h2>Note</h2>
// section: <section><h2>Note</h2></section>
```

<a id="function-elements"></a>

#### 3.16.2 Function element expressions

A function element is written like an [HTML element](#html-elements), with an
`UppercaseIdentifier` as its name. It calls the
[function](#function-declarations) of that name, which must return `Html`. The
attributes and content of the element are the arguments of the call.

```ebnf
FunctionElementExpr ::= "<" UppercaseIdentifier Attribute* ( "/>" | ">" MarkupNode* "</" UppercaseIdentifier ">" )
```

| Written as               | Passes                                         |
| ------------------------ | ---------------------------------------------- |
| `name={e}`               | the value of `e` for the parameter `name`      |
| `name="text"`            | the `String` `"text"` for the parameter `name` |
| `name`                   | `true` for the parameter `name`                |
| content between the tags | the content for the parameter `children: Html` |

For example, with

```hop
fn Badge(
  label: String,
  children: Html,
) -> Html {
  <span>
    {label}
    {children}
  </span>
}
```

```hop
<Badge label="new">
  <b>
    !
  </b>
</Badge>
```

evaluates to `<span>new<b>!</b></span>`.

Every parameter without a default value must be given an argument, so
`<Badge/>` is an error. Every argument must have the type of its parameter, so
`<Badge label={1}/>` is an error. An attribute must name a parameter, unless a
[rest parameter](#rest-parameters) accepts it, so `<Badge label="a" size="x"/>`
is an error.

Content between the tags requires a `children: Html` parameter. With
`fn Label(text: String) -> Html { … }`, `<Label text="a">x</Label>` is an error.
Content cannot be given both between the tags and as a `children` attribute.
Like any parameter, `children` can have a default value, which makes the
content optional.

<a id="fragments"></a>

#### 3.16.3 Fragment expressions

A fragment `<>…</>` groups nodes without an element around them, so that
several nodes can be used where one expression is expected. It evaluates to its
content, and `<></>` to the empty sequence.

```ebnf
FragmentExpr ::= "<>" MarkupNode* "</>"
```

For example:

```hop
let term = "hop";
let definition = "a template language";
let entry = (
  <>
    <dt>
      {term}
    </dt>
    <dd>
      {definition}
    </dd>
  </>
);
// entry: <dt>hop</dt><dd>a template language</dd>
```

<a id="markup-nodes"></a>

#### 3.16.4 Markup nodes

The content of an element or fragment is a sequence of markup nodes. A node is
text, an [interpolation](#interpolation), a comment, or a nested
[markup expression](#markup-expressions).

```ebnf
MarkupNode    ::= MarkupText
                | Interpolation
                | MarkupExpr
                | MarkupComment
MarkupText    ::= [^<{}]+
MarkupComment ::= "<!--" CommentText "-->"   /* CommentText is any text without "-->" */
```

Text evaluates to its characters as written, except that its
[whitespace](#whitespace) is normalized. It is not [escaped](#escaping), so
`&amp;` passes through unchanged. Text cannot contain `<`, `{` or `}`, which
are written `&lt;`, `&lbrace;` and `&rbrace;`. A comment evaluates to nothing.

<a id="interpolation"></a>

#### 3.16.5 Interpolation

An interpolation `{e}` is a [block expression](#block-expressions) whose value
is inserted into the content of an element or fragment.

```ebnf
Interpolation ::= BlockExpr
```

The value must have type `String` or `Html`. An `Html` value is inserted as
the elements and text it consists of. A `String` value is inserted as text,
[escaped](#escaping):

```hop
let text = "<p>hello</p>";
let greeting = (
  <div>
    {text}
  </div>
);
// greeting: <div>&lt;p&gt;hello&lt;/p&gt;</div>
```

`for` and `match` are expressions, not markup, and are written in braces like
any other interpolation. This `match` has type `String`, so its value is
escaped:

```hop
let o = Some("<3");
let display = (
  <p>
    {match o {
      Some(nickname) => nickname,
      None => "anonymous",
    }}
  </p>
);
// display: <p>&lt;3</p>
```

<a id="attributes"></a>

#### 3.16.6 Attributes

An attribute is written in the opening tag of an element, as a name alone, or as
a name with a value in double quotes or in a [block](#block-expressions). A
spread `...rest` adds the attributes collected by a
[rest parameter](#rest-parameters).

```ebnf
Attribute     ::= AttributeName ( "=" ( '"' [^"]* '"' | BlockExpr ) )?
                | "..." LowercaseIdentifier
AttributeName ::= [A-Za-z] [A-Za-z0-9_:.-]*
```

On an [HTML element](#html-elements), the forms evaluate as follows:

| Attribute     | Evaluates to                                                           |
| ------------- | ---------------------------------------------------------------------- |
| `name`        | `name`                                                                 |
| `name="text"` | `name="text"`                                                          |
| `name={e}`    | `name="…"`, with the `String` value of `e` [escaped](#escaping)        |
| `...rest`     | the attributes passed in the [rest parameter](#rest-parameters) `rest` |

`e` must have type `String`. `<div id={1}>` is an error. On a
[function element](#function-elements), `name={e}` is passed as an argument and
can have any type.

An attribute can appear only once per element. Single-quoted values such as
`id='a'` and unquoted values such as `id=a` are errors.

An HTML element accepts the global attributes, its own attributes, and any
attribute starting with `data-` or `aria-`, but not event handler attributes
such as `onclick`. So `<div href="x">` and `<button onclick="go()">` are errors.
SVG and custom elements accept any attribute, including event handlers.

<a id="rest-parameters"></a>

#### 3.16.7 Rest parameters

A rest parameter `...rest`, which must be the last parameter, collects the
attributes a caller passes that are not parameters of the function. The body
must spread it, as `...rest`, exactly once in the opening tag of an element,
where the collected attributes are placed as if written there. For example:

```hop
fn Button(
  kind: String,
  ...rest,
) -> Html {
  <button class={kind} ...rest>
    {kind}
  </button>
}

fn PrimaryButton(...rest) -> Html {
  <Button kind="primary" ...rest/>
}

let button = <Button kind="k" id="x" disabled/>;
let primary = <PrimaryButton type="submit"/>;
// button:  <button class="k" id="x" disabled>k</button>
// primary: <button class="primary" type="submit">primary</button>
```

Exactly once means once in the source text, not once per evaluation: a spread
in each arm of a `match` is an error, while a single spread inside a `for` body
is allowed, and adds the attributes on every iteration.

Which extra attributes the function accepts depends on where the rest parameter
is spread:

- When spread on an HTML element `<x … ...rest>`, the function accepts the
  [attributes `x` accepts](#attributes), except those written on `x`.
- When spread on a function element `<F … ...rest>`, the function accepts the
  parameters and extra attributes of `F`, except those written on `F`.

An attribute written on the element where the rest parameter is spread cannot be
passed through it:

```hop
fn Button(
  kind: String,
  ...rest,
) -> Html {
  <button class={kind} ...rest>
    {kind}
  </button>
}

// error: Function Button does not accept attribute 'class'
<Button kind="k" class="c"/>
```

Rest parameters cannot be spread in a cycle:

```hop
fn A(...rest) -> Html {
  // error: Rest spread of A forms a cycle and never reaches an element
  <B ...rest/>
}

fn B(...rest) -> Html {
  // error: Rest spread of B forms a cycle and never reaches an element
  <A ...rest/>
}
```

<a id="whitespace"></a>

#### 3.16.8 Whitespace

The content of every element and fragment is normalized before it is evaluated:

- Text is trimmed at the start and end of the content, and next to line breaks.
  Whitespace inside a line, and between text and an element or interpolation on
  the same line, is kept as written.
- A line break between two pieces of text becomes a single space, and blank
  lines count as one line break. Any other line break is removed.

In the table, ⏎ marks a line break and `name` is `"Alice"`.

| Markup                                    | Renders as                        |
| ----------------------------------------- | --------------------------------- |
| `<p>   padded   </p>`                     | `<p>padded</p>`                   |
| `<p>⏎  one⏎  two⏎</p>`                    | `<p>one two</p>`                  |
| `<p>one⏎⏎  two</p>`                       | `<p>one two</p>`                  |
| `<p>a    b</p>`                           | `<p>a    b</p>`                   |
| `<p>Hello <b>world</b> again</p>`         | `<p>Hello <b>world</b> again</p>` |
| `<p>⏎  Hello⏎  <b>world</b>⏎  again⏎</p>` | `<p>Hello<b>world</b>again</p>`   |
| `<p>Hi {name}!</p>`                       | `<p>Hi Alice!</p>`                |
| `<p>⏎  Hi⏎  {name}⏎</p>`                  | `<p>HiAlice</p>`                  |
| `<p>⏎  <b>x</b> <i>y</i>⏎</p>`            | `<p><b>x</b> <i>y</i></p>`        |

An interpolation `{" "}` inserts a space that a line break would otherwise
remove.

<a id="escaping"></a>

#### 3.16.9 Escaping

A `String` value inserted into markup, as an [interpolation](#interpolation) or
as an [attribute value](#attributes), is escaped: each character below is
replaced by the character reference next to it, and every other character is
kept as written.

| Character | Replaced by |
| --------- | ----------- |
| `&`       | `&amp;`     |
| `<`       | `&lt;`      |
| `>`       | `&gt;`      |
| `"`       | `&quot;`    |
| `'`       | `&#39;`     |

```hop
let s = "a < b & \"c\" > 'd'";
let p = (
  <p title={s}>
    {s}
  </p>
);
// p: <p title="a &lt; b &amp; &quot;c&quot; &gt; &#39;d&#39;">a &lt; b &amp; &quot;c&quot; &gt; &#39;d&#39;</p>
```

<a id="modules-and-declarations"></a>

## 4 Modules and declarations

A module is a sequence of imports, records, enums, functions and pages. A
declaration marked `pub` can be imported by other modules, and no two
declarations in a module can have the same name. The declarations of a module
can refer to each other in any order. Imports cannot form a cycle: a module
cannot import from a module that imports from it, directly or through other
modules.

```ebnf
Module ::= ( ImportDecl | RecordDecl | EnumDecl | FunctionDecl | PageDecl )*
```

<a id="import-declarations"></a>

### 4.1 Import declarations

An import makes a public declaration of another module available.

```ebnf
ImportDecl    ::= "import" ModuleSegment ( "::" ModuleSegment )* "::" ( UppercaseIdentifier | LowercaseIdentifier )
ModuleSegment ::= [A-Za-z_] [A-Za-z0-9_]*
```

`import a::b::Name` imports `Name` from the module `a::b`, which is the file
`a/b.hop` relative to the project root. The module must exist and declare `Name`
as `pub`. A module cannot re-export what it imports.

<a id="record-declarations"></a>

### 4.2 Record declarations

A record declaration `record R {…}` declares the record type `R`, with the
fields listed between the braces. The fields must have different names, and a
field can refer to the type it belongs to, as in `children: Array[Item]` in a
record `Item`.

```ebnf
RecordDecl ::= "pub"? "record" UppercaseIdentifier "{" ( FieldDecl ( "," FieldDecl )* ","? )? "}"
FieldDecl  ::= LowercaseIdentifier ":" Type
```

<a id="enum-declarations"></a>

### 4.3 Enum declarations

An enum declaration `enum E {…}` declares the enum type `E`, with the variants
listed between the braces. The variants must have different names. A variant can
have fields, declared as in [record declarations](#record-declarations).

```ebnf
EnumDecl ::= "pub"? "enum" UppercaseIdentifier "{" ( Variant ( "," Variant )* ","? )? "}"
Variant  ::= UppercaseIdentifier ( "{" ( FieldDecl ( "," FieldDecl )* ","? )? "}" )?
```

<a id="function-declarations"></a>

### 4.4 Function declarations

A function declaration `fn f(…) -> T { … }` declares the function `f`. A
lowercase function is [called as `f(…)`](#call-expressions), and an uppercase
function returning `Html` is
[called as a function element](#function-elements).

```ebnf
FunctionDecl ::= "pub"? "fn" ( UppercaseIdentifier | LowercaseIdentifier ) "(" ( Param ( "," Param )* ","? )? ")" "->" Type BlockExpr
Param        ::= LowercaseIdentifier ":" Type ( "=" Expr )? | "..." LowercaseIdentifier
```

| Parameter  | Semantics                              |
| ---------- | -------------------------------------- |
| `x: T`     | a parameter of type `T`                |
| `x: T = v` | a parameter with the default value `v` |
| `...x`     | a [rest parameter](#rest-parameters)   |

A default value must be constant: a literal, a numeric literal preceded by `-`,
`<></>`, or an array, record, enum or option built from constants, without a
`...` spread:

```hop
// error: Default values must be constant
fn double(x: Int = 1 + 1) -> Int {
  x * 2
}
```

A function can call itself, for example to build the markup for a tree:

```hop
record Item {
  label: String,
  children: Array[Item],
}

fn Tree(item: Item) -> Html {
  <li>
    {item.label}
    <ul>
      {for child in item.children {
        <Tree item={child}/>
      }}
    </ul>
  </li>
}
```

The compiler does not check that recursion terminates.

A function body must have the declared return type:

```hop
fn answer() -> Int {
  "x" // error: Mismatched type for function body: expected Int got String
}
```

Only an uppercase function that returns `Html` can be used as a function
element. An uppercase function that returns another type cannot be called at
all:

```hop
fn Label() -> String {
  "x"
}

fn Form() -> Html {
  // error: Only a function returning Html can be invoked as a tag
  <Label/>
}
```

<a id="page-declarations"></a>

### 4.5 Page declarations

A page declaration `page P(…) { … }` declares the page `P`. Its parameters are
in scope in `head` and `body`, and [rendering](#rendering) the page produces an
HTML document. The parameters of a page cannot have default values, and a page
has no rest parameter.

```ebnf
PageDecl   ::= "pub"? "page" UppercaseIdentifier ( "(" ( PageParam ( "," PageParam )* ","? )? ")" )? "{" PageMember* "}"
PageParam  ::= LowercaseIdentifier ":" Type
PageMember ::= "fn" ( "head" | "body" ) "(" ")" "->" "Html" BlockExpr
```

A page has exactly one `body` and at most one `head`:

```hop
// error: Expected a 'fn body() -> Html' member
page Home {
  fn head() -> Html {
    <title>
      Home
    </title>
  }
}
```

<a id="rendering"></a>

### 4.6 Rendering

A page is rendered by the host, which supplies its arguments and receives the
resulting document as UTF-8 text. The document is, with nothing between the
parts:

1. `<!doctype html>`
2. `<html><head>`
3. `<meta charset="utf-8">`
4. `<meta content="width=device-width, initial-scale=1" name="viewport">`
5. the rendering of the value of `head`, if the page has one
6. `</head><body>`
7. the rendering of the value of `body`
8. `</body></html>`

The two `<meta>` elements come first, so a page cannot place anything before
the character set declaration. A host can add further elements to the end of
the `<head>`, such as a stylesheet or a script. What it adds is not part of the
language.

An `Html` value renders as the renderings of its elements and text in order.
Text renders as its characters. An element renders as its start tag, its
content and its end tag, except that a [void element](#html-elements) has no
end tag and no content. The start tag is `<`, the name, the attributes and `>`.
Each attribute is a space followed by its name and, if it has a value, `="`,
the value and `"`. The end tag is `</`, the name and `>`.

So `<br/>` renders as `<br>`, `<div/>` renders as `<div></div>`, and
`<input disabled value={v}>` renders as `<input disabled value="…">`, with the
value of `v` [escaped](#escaping).

<a id="reserved-words"></a>

## Appendix: Reserved words

A `LowercaseIdentifier` or `ModuleSegment` cannot be one of:

```
alias        and          as           assert       async        auto
await        break        case         catch        comp         component
const        constructor  continue     default      defer        elif
else         entrypoint   export       extends      final        finally
from         func         get          if           impl         implements
include      interface    internal     is           loop         mod
mut          namespace    new          newtype      nil          not
null         of           or           out          package      priv
private      public       return       self         set          static
struct       super        this         throw        trait        try
undefined    use          val          var          view         void
when         where        while        yield
```

An `UppercaseIdentifier` cannot be one of:

```
Any       Arr       Async     Auto      Box       CSS       Class     Classes
Client    Comp      Computed  Dyn       Dynamic   Enum      Err       Error
Fn        Fragment  Func      Function  Future    HTML      IO        List
Map       Never     Object    Ok        Promise   Rec       Record    Result
Runtime   Safe      Scope     Scoped    Self      Set       Static    Struct
Task      Trusted   Tuple     Type      Union     Unknown   Vec       View
Void
```
