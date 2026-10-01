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

## Lexical structure

A module is a UTF-8 text file. Outside markup and string literals, whitespace
is ignored except as it separates tokens.

A comment starts with `//` and runs to the end of the line. It can appear
wherever whitespace can, except inside markup, where a
[markup comment](#markup-nodes) is written `<!-- … -->`.

```ebnf
Comment ::= "//" [^\n]*
```

<a id="identifiers"></a>

### Identifiers

Identifiers have two forms. A lowercase identifier names a variable, a field, a
function or a macro, and an uppercase identifier names a record, an enum, a
variant, a function or a page.

```ebnf
LowercaseIdentifier ::= [a-z] ( "_"? [a-z0-9] )*
UppercaseIdentifier ::= [A-Z] [A-Za-z0-9]*
```

<a id="keywords"></a>

### Keywords and reserved words

Using any of the words below as an identifier is a compile error.

```
enum    false   fn      for     import  in
let     match   page    pub     record  true

None    Some

Array   Bool    Float   Html    Int     Option  String
```

Further words are reserved for future use. They are listed in
[Appendix: Reserved words](#reserved-words).

<a id="types"></a>

## Types

Every expression has a type, determined at compile time: one of the built-in
types below, or a record or enum type declared in a module. An expression
evaluates to a value of its type.

```ebnf
Type ::= "Bool"
       | "Int"
       | "Float"
       | "String"
       | "Html"
       | "Array" "[" Type "]"
       | "Option" "[" Type "]"
       | "(" ( Type ( "," Type )* ","? )? ")"
       | UppercaseIdentifier
```

An `UppercaseIdentifier` names a [record](#record-declarations) or
[enum](#enum-declarations) type. The built-in types have these values:

| Type          | Values                                  |
| ------------- | --------------------------------------- |
| `Bool`        | `true`, `false`                         |
| `Int`         | 32-bit signed integers                  |
| `Float`       | IEEE 754 binary64, including ±∞ and NaN |
| `String`      | sequences of Unicode scalar values      |
| `Html`        | sequences of HTML elements and text     |
| `Array[T]`    | sequences of `T`                        |
| `Option[T]`   | `None`, `Some(v)`                       |
| `(T1, T2, …)` | `(v1, v2, …)`                           |

<a id="expressions"></a>

## Expressions

An expression computes a value. Evaluation has no side effects and cannot
fail: the value of an expression depends only on the values of its parts, and
the only way an evaluation does not produce a value is that a
[recursive function](#function-declarations) does not terminate. The order in
which the parts of an expression are evaluated is otherwise not observable.

An expression is one of the forms below:

```ebnf
Expr ::= LiteralExpr
       | ArrayExpr
       | TupleExpr
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

### Literal expressions

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

An integer literal that does not fit in an `Int` is a compile error. The one
exception is `2147483648` as the immediate operand of `-`, which lets
`-2147483648` be written:

```hop
0             // 0, of type Int
0.0           // 0.0, of type Float
2147483648    // error: Integer literal is out of range for Int (-2147483648 to 2147483647)
-2147483648   // -2147483648, of type Int
2147483648.0  // 2147483648.0, of type Float
```

A float literal is rounded to a `Float` as in IEEE 754, so a literal that is too
large is ∞.

<a id="array-expressions"></a>

### Array expressions

An array expression `[a, b, …]` evaluates to the array of its elements in
order, and has the type `Array[T]`, where `T` is the type of its elements.
Elements of different types are a compile error.

```ebnf
ArrayExpr ::= "[" ( Expr ( "," Expr )* ","? )? "]"
```

An empty array `[]` does not determine `T`. It takes its type from its
context, such as the annotation in `let tags: Array[String] = [];`. An empty
array with no such context is a compile error:

```hop
let tags = []; // error: Cannot infer type of []
```

<a id="tuple-expressions"></a>

### Tuple expressions

A tuple expression `(a, b, …)` evaluates to the tuple of its elements in order.
Its type is the tuple type of the types of its elements, so `(1, "a")` has the
type `(Int, String)`.

```ebnf
TupleExpr ::= "(" ")"
            | "(" Expr "," ")"
            | "(" Expr ( "," Expr )+ ","? ")"
```

A tuple of one element is written with a trailing comma, since `(a)` is a
[parenthesized expression](#parenthesized-expressions):

```hop
let name = "Alice";

(name, 36)  // ("Alice", 36), of type (String, Int)
(name,)     // ("Alice",), of type (String,)
(name)      // "Alice", of type String
()          // (), of type ()
```

When the context of a tuple expression expects a tuple type with as many
elements, each element takes its context from the element type at its position,
as in `let pair: (Option[String], Array[Int]) = (None, []);`. Without such a
context, an element that cannot determine its own type is a compile error:

```hop
let counts = (1, []); // error: Cannot infer type of []
```

A tuple has no fields, methods or operators. Its elements are read with a
[tuple pattern](#tuple-patterns).

<a id="option-expressions"></a>

### Option expressions

An option expression is `None` or `Some(e)`. `None` evaluates to the option
with no value, and `Some(e)` to the option holding the value of `e`. Both have
the type `Option[T]`, where `T` is the type of `e`.

```ebnf
OptionExpr ::= "None" | "Some" "(" Expr ")"
```

Like an empty array, `None` does not determine `T` and takes its type from
its context, such as the annotation in `let nickname: Option[String] = None;`.
A `None` with no such context is a compile error:

```hop
let nickname = None; // error: Cannot infer type of None
```

<a id="record-expressions"></a>

### Record expressions

A record expression `R {f: e, …}` evaluates to a record of type `R`. Each
entry `f: e` gives the field `f` the value of `e`.

```ebnf
RecordExpr ::= UppercaseIdentifier "{" ( ( FieldValue | Spread ) ( "," ( FieldValue | Spread ) )* ","? )? "}"
FieldValue ::= LowercaseIdentifier ":" Expr
Spread     ::= "..." Expr
```

Leaving a field of `R` without a value, or giving a field a value more than
once, is a compile error.

A spread `...r` copies from `r` the fields that are not written. It is a
compile error if `r` does not have type `R`, or if a record expression has more
than one spread.

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

### Enum expressions

An enum expression `E::V` or `E::V {f: e, …}` evaluates to the variant `V` of
the enum type `E`, and has that type.

```ebnf
EnumExpr ::= UppercaseIdentifier "::" UppercaseIdentifier ( "{" ( FieldValue ( "," FieldValue )* ","? )? "}" )?
```

The fields are written as in [record expressions](#record-expressions), except
that a spread is a compile error.

For example, with `enum Status {Active, Away {since: String}}`, both
`Status::Active` and `Status::Away {since: "Monday"}` are `Status` values.

<a id="variable-reference-expressions"></a>

### Variable reference expressions

A variable reference expression `x` evaluates to the value that `x` is bound
to, by a parameter, a let binding, a `for` or a pattern.

```ebnf
VariableReferenceExpr ::= LowercaseIdentifier
```

A variable that is not in scope is a compile error:

```hop
z // error: Undefined variable: z
```

A binding that reuses a name already in scope is a compile error:

```hop
fn double(x: Int) -> Int {
  let x = x * 2; // error: Variable x is already defined
  x
}
```

<a id="parenthesized-expressions"></a>

### Parenthesized expressions

A parenthesized expression evaluates to the value of the expression inside the
parentheses, and has its type.

```ebnf
ParenExpr ::= "(" Expr ")"
```

Parentheses group an expression to override the
[precedence of operators](#operator-expressions), as in `(a + b) * c`.

<a id="call-expressions"></a>

### Call expressions

A call expression `f(…)` evaluates to the value that the function `f` returns
for its arguments, and has the return type of `f`.

```ebnf
CallExpr  ::= LowercaseIdentifier "(" Arguments? ")"
Arguments ::= Expr ( "," Expr )* ","?
            | LowercaseIdentifier ":" Expr ( "," LowercaseIdentifier ":" Expr )* ","?
```

`f(a, b)` passes its arguments by position, and `f(x: a, y: b)` by name. A call
that mixes the two is a compile error, and so is leaving out a parameter that
has no default value.

Only functions with lowercase names can be called this way. Functions with
uppercase names are called as [function elements](#function-elements).

<a id="macro-expressions"></a>

### Macro expressions

A macro expression `name!(…)` calls one of the three macros [`join!`](#join),
[`format!`](#format) and [`asset!`](#asset). Any other macro name is a compile
error.

```ebnf
MacroExpr ::= LowercaseIdentifier "!" "(" ( Expr ( "," Expr )* ","? )? ")"
```

<a id="join"></a>

#### The join macro

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

#### The format macro

The format macro fills in the placeholders of a template and evaluates to the
resulting `String`. The first argument is the template, and each `{}` in it is
a placeholder. A template that is not a string literal is a compile error.

The other arguments fill in the placeholders in order, one for each `{}`, and
a different number of arguments is a compile error. Each argument has type
`String` or `Int`, and any other type is a compile error. An argument of type
`Int` is converted to a `String` via `to_string()`.

To write a brace in the template, double it: `{{` stands for `{` and `}}` for
`}`. Any other brace in the template is a compile error.

```hop
format!("{} is {} years old", "Alice", 36)  // "Alice is 36 years old"
format!("{{{}}}", "Alice")                  // "{Alice}"
format!("{}" + "!", "Alice")                // error: format! requires a string literal as its first argument
format!("{} and {}", "Alice")               // error: format! expects 2 argument(s) for the format string, got 1
format!("{}", 1.5)                          // error: format! arguments must be String or Int, got Float
```

<a id="asset"></a>

#### The asset macro

The asset macro takes one string literal, the path of a file in the project, and
evaluates to the URL of that file as a `String`. The path starts with `/`, which
stands for the project root. An argument that is not a string literal, a path
that does not start with `/` and a path that does not name a file in the
project are compile errors.

```hop
asset!("/icons/star.svg")       // the URL of icons/star.svg
asset!("/icons/missing.svg")    // error: asset `icons/missing.svg` was not found
asset!("icons/star.svg")        // error: invalid asset! path: path must start with '/'
asset!("/icons/" + "star.svg")  // error: asset! argument must be a string literal
```

<a id="field-access-expressions"></a>

### Field access expressions

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

### Method call expressions

A method call expression `v.m()` calls one of the built-in methods below on
the value `v`, and evaluates to the result in the table. Any other method name
is a compile error.

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

### Operator expressions

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

A binary operator whose operands have different types is a compile error, so
`1 + 1.0` is a compile error unless one side is converted with `to_float()` or
`to_int()`.

The tables below list every combination of operator and type that is allowed,
and any other is a compile error. In particular, comparing an option with `None`
is a compile error. Whether an option is `None` is tested with `is_none()` or a
`match`.

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

### Block expressions

A block expression evaluates its let bindings in order, and each binding is in
scope for the rest of the block. It evaluates to the value of its last
expression, and has its type.

```ebnf
BlockExpr  ::= "{" LetBinding* Expr "}"
LetBinding ::= "let" LowercaseIdentifier ( ":" Type )? "=" Expr ";"
```

A let binding `let x = e;` binds `x` to the value of `e`. With a type
annotation, as in `let x: T = e;`, it is a compile error if `e` does not have
type `T`.

```hop
let val = {
  let name = "Alice";
  let tags: Array[String] = [];
  format!("{} has {} tags", name, tags.len())
};

val // "Alice has 0 tags"
```

<a id="match-expressions"></a>

### Match expressions

A `match` expression compares a value, the subject, with the patterns of its
arms, and evaluates to the value of the first arm whose pattern matches.

```ebnf
MatchExpr       ::= "match" Expr "{" ( MatchArm ( "," MatchArm )* ","? )? "}"
MatchArm        ::= Pattern "=>" Expr
Pattern         ::= WildcardPattern
                  | VariablePattern
                  | BoolPattern
                  | OptionPattern
                  | TuplePattern
                  | RecordPattern
                  | EnumPattern
                  | "(" Pattern ")"
```

A subject that is a [record](#record-expressions) or
[enum expression](#enum-expressions) with fields is a compile error unless it is
wrapped in parentheses, since its `{` would be read as the start of the arms.

The subject has type `Bool`, `Option[T]`, a tuple type, or a record or enum
type, and a subject of any other type is a compile error. So is a pattern that
does not have the type of the subject. A pattern in parentheses, `(p)`, is the
same as `p`. The expressions of all arms have the same type, which is the type
of the `match` expression, and arms of different types are a compile error.

<a id="wildcard-and-variable-patterns"></a>

#### Wildcard and variable patterns

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

A `match` whose only arm matches every value without binding a variable is a
compile error:

```hop
let b = true;
// error: Useless match expression: does not branch or bind any variables
match b {
  _ => "",
}
```

<a id="bool-patterns"></a>

#### Bool patterns

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

#### Option patterns

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

<a id="tuple-patterns"></a>

#### Tuple patterns

A tuple pattern `(p, q, …)` matches a tuple whose elements match the patterns
at the same positions, and binds what its element patterns bind.

```ebnf
TuplePattern ::= "(" ")"
               | "(" Pattern "," ")"
               | "(" Pattern ( "," Pattern )+ ","? ")"
```

As in [tuple expressions](#tuple-expressions), a pattern of one element is
written with a trailing comma, `(p,)`. A `match` on a tuple expression tests
several values at once:

```hop
let signed_in = true;
let nickname = Some("Alice");
match (signed_in, nickname) {
  (true, Some(name)) => name,
  (true, None) => "member",
  (false, _) => "guest",
}
```

A tuple pattern with a different number of patterns than its type has elements
is a compile error:

```hop
let pair = ("Alice", 36);
match pair {
  // error: Mismatched pattern type: expected (String, Int) got (name, _, _)
  (name, _, _) => name,
}
```

<a id="record-patterns"></a>

#### Record patterns

A record pattern `R {f: p, …}` matches a record whose fields match their
patterns, and binds what its field patterns bind. A field pattern `f`
without `: p` is short for `f: f`: it binds the field to a variable of the same
name.

```ebnf
RecordPattern ::= UppercaseIdentifier FieldPatterns
FieldPatterns ::= "{" ( FieldPattern ( "," FieldPattern )* ","? )? "}"
FieldPattern  ::= LowercaseIdentifier ( ":" Pattern )?
```

A record pattern that leaves out a field of its type, or lists a field more
than once, is a compile error. A field pattern `f: _` matches the field without
binding it. With the `User` below, `User {name, age: _}` matches, while
`User {name}` leaves out `age`:

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

#### Enum patterns

An enum pattern `E::V` or `E::V {f: p, …}` matches a value of the variant `V`
whose fields match their patterns, and binds what its field patterns bind.

```ebnf
EnumPattern ::= UppercaseIdentifier "::" UppercaseIdentifier FieldPatterns?
```

The fields are written as in [record patterns](#record-patterns), and leaving
out a field of the variant is a compile error. For example:

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

#### Exhaustiveness

It is a compile error if the arms of a `match` do not together cover every value
of the type of the subject. A wildcard or a variable covers every value.
Coverage is checked recursively:

```hop
let flag = Some(true);
// error: Match expression is missing arms for: Some(false)
match flag {
  Some(true) => "yes",
  None => "unknown",
}
```

<a id="reachability"></a>

#### Reachability

The arms are tried in order. An arm is unreachable, and a compile error, if
every value it matches is matched by an arm before it:

```hop
let b = true;
match b {
  _ => "a",
  // error: Unreachable match arm for pattern 'true'
  true => "b",
}
```

<a id="for-expressions"></a>

### For expressions

A `for` expression evaluates its body, which has type `Html`, once for each
element of an array, and evaluates to the results concatenated in order.

A `for` can also loop over a range `a..=b`: the `Int` values from `a` to `b`
inclusive. The range is empty if `a` is greater than `b`. A range anywhere
else, such as an expression on its own, is a compile error.

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

Looping over anything other than an array or a range of `Int` values is a
compile error:

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
for _ in 1..=3 {
  "*" // error: Mismatched type for for body: expected Html got String
}
```

<a id="markup-expressions"></a>

### Markup expressions

An `Html` value is a sequence of elements and text. An element has a name,
attributes in the order written, and content, which is itself a sequence of
elements and text.

```ebnf
MarkupExpr ::= HtmlElementExpr | FunctionElementExpr | FragmentExpr
```

<a id="html-elements"></a>

#### HTML element expressions

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

The name is that of an HTML or SVG element, such as `div` or `path`, or of a
custom element, which contains `-`, such as `my-widget`. Any other name, such as
`widget`, is a compile error.

Using `<html>`, `<head>` or `<body>` is a compile error, since a
[page](#page-declarations) provides them, and so is using `<style>`: styles go
in the project stylesheet. A `<script>` that has content, or that does not
reference a file with `src`, is a compile error. A script is written as in
`<script src="/app.js"></script>`.

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

#### Function element expressions

A function element is written like an [HTML element](#html-elements), with an
`UppercaseIdentifier` as its name. It calls the
[function](#function-declarations) of that name, and is a compile error if the
function does not return `Html`. The attributes and content of the element are
the arguments of the call.

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

Leaving out a parameter that has no default value is a compile error, as in
`<Badge/>`. So is an argument that does not have the type of its parameter, as
in `<Badge label={1}/>`, and an attribute that names no parameter, unless a
[rest parameter](#rest-parameters) accepts it, as in
`<Badge label="a" size="x"/>`.

Content between the tags requires a `children: Html` parameter. With
`fn Label(text: String) -> Html { … }`, `<Label text="a">x</Label>` is a compile
error. Giving content both between the tags and as a `children` attribute is a
compile error too. Like any parameter, `children` can have a default value,
which makes the content optional.

<a id="fragments"></a>

#### Fragment expressions

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

#### Markup nodes

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

Text evaluates to its characters as written, after its
[whitespace](#whitespace-normalization) is normalized at compile time. It is not
[escaped](#escaping), so `&amp;` passes through unchanged. Text cannot contain
`<`, `{` or `}`, which are written `&lt;`, `&lbrace;` and `&rbrace;`. A comment
evaluates to nothing.

<a id="interpolation"></a>

#### Interpolation

An interpolation `{e}` is a [block expression](#block-expressions) whose value
is inserted into the content of an element or fragment.

```ebnf
Interpolation ::= BlockExpr
```

The block expression has type `String` or `Html`, and any other type is a
compile error. If it has type `Html`, its value is inserted as the elements and
text it consists of. If it has type `String`, its value is inserted as text,
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

#### Attributes

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

An `e` that does not have type `String` is a compile error, as in
`<div id={1}>`. On a [function element](#function-elements), `name={e}` is
passed as an argument and can have any type.

An attribute that appears more than once on an element is a compile error, and
so are single-quoted values such as `id='a'` and unquoted values such as
`id=a`.

An HTML element accepts the global attributes, its own attributes, and any
attribute starting with `data-` or `aria-`. SVG and custom elements accept any
attribute. No element accepts an attribute whose name starts with `on`, in any
mix of upper and lower case, such as the event handler `onclick`. So
`<div href="x">`, `<button onclick="go()">` and `<svg onload="init()">` are
compile errors.

<a id="rest-parameters"></a>

#### Rest parameters

A rest parameter `...rest` is the last parameter, and collects the attributes a
caller passes that are not parameters of the function. The body spreads it, as
`...rest`, in the opening tag of an element, where the collected attributes are
placed as if written there. A rest parameter that is not the last parameter, or
that the body does not spread exactly once, is a compile error. For example:

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
in each arm of a `match` is a compile error, while a single spread inside a
`for` body is allowed, and adds the attributes on every iteration.

Which extra attributes the function accepts depends on where the rest parameter
is spread:

- When spread on an HTML element `<x … ...rest>`, the function accepts the
  [attributes `x` accepts](#attributes), except those written on `x`.
- When spread on a function element `<F … ...rest>`, the function accepts the
  parameters and extra attributes of `F`, except those written on `F`.

Passing an attribute through a rest parameter is a compile error if the
attribute is written on the element where the rest parameter is spread:

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

Spreading rest parameters in a cycle is a compile error:

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

<a id="whitespace-normalization"></a>

#### Whitespace normalization

Whitespace in markup is normalized at compile time. Normalization applies to
the content of each element and fragment as it is written in the source:

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

Normalization does not apply to values. The value of an interpolation is
inserted with its whitespace unchanged, so an interpolation `{" "}` inserts a
space that a line break would otherwise remove:

```hop
let padded = "  Alice  ";
let p = (
  <p>
    {padded}
  </p>
);
// p: <p>  Alice  </p>
```

<a id="escaping"></a>

#### Escaping

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

## Modules and declarations

A module is a sequence of imports, records, enums, functions and pages. A
declaration marked `pub` can be imported by other modules, and two declarations
with the same name in a module are a compile error. The declarations of a
module can refer to each other in any order. Imports that form a cycle, where a
module imports from a module that imports from it, directly or through other
modules, are a compile error.

```ebnf
Module ::= ( ImportDecl | RecordDecl | EnumDecl | FunctionDecl | PageDecl )*
```

<a id="import-declarations"></a>

### Import declarations

An import makes a public declaration of another module available.

```ebnf
ImportDecl    ::= "import" ModuleSegment ( "::" ModuleSegment )* "::" ( UppercaseIdentifier | LowercaseIdentifier )
ModuleSegment ::= [A-Za-z_] [A-Za-z0-9_]*
```

`import a::b::Name` imports `Name` from the module `a::b`, which is the file
`a/b.hop` relative to the project root. Importing from a module that does not
exist, or a `Name` that the module does not declare as `pub`, is a compile
error. A module does not re-export what it imports.

<a id="record-declarations"></a>

### Record declarations

A record declaration `record R {…}` declares the record type `R`, with the
fields listed between the braces. Two fields with the same name are a compile
error, and a field can refer to the type it belongs to, as in `children: Array[Item]` in a
record `Item`.

```ebnf
RecordDecl ::= "pub"? "record" UppercaseIdentifier "{" ( FieldDecl ( "," FieldDecl )* ","? )? "}"
FieldDecl  ::= LowercaseIdentifier ":" Type
```

<a id="enum-declarations"></a>

### Enum declarations

An enum declaration `enum E {…}` declares the enum type `E`, with the variants
listed between the braces. Two variants with the same name are a compile error.
A variant can have fields, declared as in
[record declarations](#record-declarations).

```ebnf
EnumDecl ::= "pub"? "enum" UppercaseIdentifier "{" ( Variant ( "," Variant )* ","? )? "}"
Variant  ::= UppercaseIdentifier ( "{" ( FieldDecl ( "," FieldDecl )* ","? )? "}" )?
```

<a id="function-declarations"></a>

### Function declarations

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

A default value that is not constant is a compile error. A constant is a
literal, a numeric literal preceded by `-`, `<></>`, or an array, tuple, record,
enum or option built from constants, without a `...` spread:

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

An implementation does not check that recursion terminates.

A function body that does not have the declared return type is a compile error:

```hop
fn answer() -> Int {
  "x" // error: Mismatched type for function body: expected Int got String
}
```

Only an uppercase function that returns `Html` can be used as a function
element, and calling an uppercase function that returns another type is a
compile error:

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

### Page declarations

A page declaration `page P(…) { … }` declares the page `P`. Its parameters are
in scope in `head` and `body`, and [rendering](#rendering) the page produces an
HTML document. A default value for a page parameter is a compile error, and so
is a rest parameter.

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

A page parameter whose type is `Html`, or contains `Html` as an element, a
field or a variant field, is a compile error:

```hop
record Post {
  title: String,
  content: Html,
}

// error: Html is not allowed in page parameters
page Show(post: Post) {
  fn body() -> Html {
    post.content
  }
}
```

<a id="rendering"></a>

### Rendering

A page is rendered by the host, which supplies its arguments and receives the
resulting document as UTF-8 text.

The host passes each argument as a value of its own language, which must
represent a value of the type of the parameter, as described in
[Types](#types). Not every host value does: a JavaScript string that contains a
lone surrogate represents no `String`, since a lone surrogate is not a Unicode
scalar value, and a JavaScript number represents an `Int` only if it is an
integer from `-2147483648` to `2147483647`. An implementation need not check
the arguments, and if one represents no value of the type of its parameter, the
behavior of rendering is undefined.

If every argument represents a value of the type of its parameter, rendering is
deterministic: the same page renders to the same bytes for arguments that
represent the same values, whatever the host and the implementation.

The document is, with nothing between the parts:

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

A `LowercaseIdentifier` or `ModuleSegment` that is one of these words is a
compile error:

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

An `UppercaseIdentifier` that is one of these words is a compile error:

```
Any       Arr       Async     Auto      Box       CSS       Class     Classes
Client    Comp      Computed  Dyn       Dynamic   Enum      Err       Error
Fn        Fragment  Func      Function  Future    HTML      IO        List
Map       Never     Object    Ok        Promise   Rec       Record    Result
Runtime   Safe      Scope     Scoped    Self      Set       Static    Struct
Task      Trusted   Tuple     Type      Union     Unknown   Vec       View
Void
```
