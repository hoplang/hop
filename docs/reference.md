# hop language reference

<a id="notation"></a>

## Notation

The grammars use W3C-style EBNF:

```
A ::= …       the rule A
"x", 'x'      the text x
#xN           the character with hexadecimal code point N
A B           A followed by B
A | B         A or B
A?            A or nothing
A*            A repeated zero or more times
A+            A repeated one or more times
( … )         grouping
[a-z], [^"]   a character in, or not in, a set
/* … */       a comment
```

<a id="lexical-structure"></a>

## Lexical structure

A module is a UTF-8 text file. Outside [markup text](#markup-text) and string
literals, whitespace is ignored except as it separates tokens.

<a id="comments"></a>

### Comments

A comment starts with `//` and runs to the end of the line. It can appear
wherever whitespace can, except directly in a tag or in
[markup content](#markup-content). In markup content, a comment is written
`<!-- … -->`. A comment is removed from the source as if it were not written.

```ebnf
Comment       ::= "//" [^#x0A]*              /* #x0A is a line feed */
MarkupComment ::= "<!--" CommentText "-->"   /* CommentText is any text without "-->" */
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

```
Bool          true, false
Int           32-bit signed integers
Float         IEEE 754 binary64, including ±∞ and NaN
String        sequences of Unicode scalar values
Html          sequences of elements and text
Array[T]      sequences of T
Option[T]     None, Some(v)
(T1, T2, …)   (v1, v2, …)
```

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
       | ParenthesizedExpr
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
large evaluates to ∞.

A string literal denotes its characters as written, except that each escape
sequence below stands for the character next to it:

```
\n   line feed
\t   tab
\r   carriage return
\\   backslash
\"   double quote
```

<a id="array-expressions"></a>

### Array expressions

An array expression `[a, b, …]` evaluates to the array of its elements in
order, and has the type `Array[T]`, where `T` is the type of its elements.
Elements of different types are a compile error.

```ebnf
ArrayExpr ::= "[" ( Expr ( "," Expr )* ","? )? "]"
```

An empty array `[]` does not determine `T`. It takes its type from its
context, such as a type annotation. An empty array with no such context is a
compile error:

```hop
let names: Array[String] = []; // []
let tags = [];                 // error: Cannot infer type of []
```

<a id="tuple-expressions"></a>

### Tuple expressions

A tuple expression `(a, b, …)` evaluates to the tuple of its elements in order.
Its type is formed from the types of its elements, so `(1, "a")` has the type
`(Int, String)`.

```ebnf
TupleExpr ::= "(" Expr ( "," Expr )+ ","? ")"
            | "(" Expr "," ")"
            | "(" ")"
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
as with a type annotation. Without such a context, an element that cannot
determine its own type is a compile error:

```hop
let pair: (Option[String], Array[Int]) = (None, []); // (None, [])
let counts = (1, []);                                // error: Cannot infer type of []
```

A tuple has no fields, methods or operators. Its elements are read with a
[tuple pattern](#tuple-patterns).

<a id="option-expressions"></a>

### Option expressions

An option expression is `None` or `Some(e)`. `None` evaluates to the option
with no value, and `Some(e)` to the option holding the value of `e`. Both have
a type `Option[T]`. For `Some(e)`, `T` is the type of `e`.

```ebnf
OptionExpr ::= "None" | "Some" "(" Expr ")"
```

Like an empty array, `None` does not determine `T` and takes its type from
its context, such as a type annotation. A `None` with no such context is a
compile error:

```hop
let alias: Option[String] = None; // None
let nickname = None;              // error: Cannot infer type of None
```

<a id="record-expressions"></a>

### Record expressions

A record expression `R {f: e, …}` evaluates to a record of type `R`. Each
field value `f: e` gives the field `f` the value of `e`.

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

For example:

```hop
enum Status {
  Active,
  Away {since: String},
}

Status::Active                  // of type Status
Status::Away {since: "Monday"}  // of type Status
```

<a id="variable-reference-expressions"></a>

### Variable reference expressions

A variable reference expression `x` evaluates to the value that `x` is bound
to, by a parameter, a let binding, a `for` expression or a pattern.

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
ParenthesizedExpr ::= "(" Expr ")"
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
uppercase names are called by [markup calls](#markup-call-expressions).

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

Accessing a field that the record type does not declare is a compile error:

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

A method call expression `v.m(…)` calls the built-in method `m` on the value
`v` with the given arguments, and evaluates to its result.

```ebnf
MethodCallExpr ::= Expr "." LowercaseIdentifier "(" ( Expr ( "," Expr )* ","? )? ")"
```

The built-in methods are listed below. Any other method name, or a different
number of arguments, is a compile error.

```
Receiver    Method         Result   Semantics
Array[T]    len()          Int      number of elements
Array[T]    is_empty()     Bool     true if the array has no elements
String      is_empty()     Bool     true if the string is ""
Int         to_string()    String   decimal representation, such as -42
Int         to_float()     Float    the same value as a float (exact, since Int is 32-bit)
Float       to_int()       Int      truncates toward zero, saturates at the Int bounds, NaN becomes 0
Option[T]   is_some()      Bool     true if the option is Some(_)
Option[T]   is_none()      Bool     true if the option is None
Option[T]   unwrap_or(d)   T        x if the option is Some(x), otherwise d
```

<a id="operator-expressions"></a>

### Operator expressions

An operator expression combines values with the unary operators `!` and `-`, or
with a binary operator for comparison, arithmetic or logic.

```ebnf
OperatorExpr ::= UnaryExpr | BinaryExpr
UnaryExpr    ::= UnaryOp Expr
BinaryExpr   ::= Expr BinaryOp Expr
UnaryOp      ::= "!" | "-"
BinaryOp     ::= "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "&&" | "||"
```

Operators group by precedence, listed here from highest to lowest:

```
Precedence   Operators           Kind      Associativity
1            .field, .method()   postfix   –
2            !, -                unary     –
3            *                   binary    left
4            +, -                binary    left
5            <, >, <=, >=        binary    left
6            ==, !=              binary    left
7            &&                  binary    left
8            ||                  binary    left
```

A binary operator whose operands have different types is a compile error, so
`1 + 1.0` is a compile error unless one side is converted with `to_float()` or
`to_int()`.

The tables below list every combination of operator and type that is allowed,
and any other is a compile error. In particular, comparing an option with `None`
is a compile error. Whether an option is `None` is tested with `is_none()` or a
`match`.

```
Unary
Operator       Operand    Result   Semantics
!              Bool       Bool     logical not
-              Int        Int      negation, wraps on overflow
               Float      Float    negation

Binary
Operator       Operands   Result   Semantics
==, !=         Bool       Bool     equality
               Int        Bool     equality
               Float      Bool     IEEE 754 equality
               String     Bool     equality
<, >, <=, >=   Int        Bool     numeric ordering
               Float      Bool     IEEE 754 ordering
+              Int        Int      addition, wraps on overflow
               Float      Float    addition
               String     String   concatenation
-              Int        Int      subtraction, wraps on overflow
               Float      Float    subtraction
*              Int        Int      multiplication, wraps on overflow
               Float      Float    multiplication
&&             Bool       Bool     logical and, short-circuiting
||             Bool       Bool     logical or, short-circuiting
```

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
let summary = {
  let name = "Alice";
  let tags: Array[String] = [];
  format!("{} has {} tags", name, tags.len())
};

summary // "Alice has 0 tags"
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

A pattern in parentheses, `(p)`, matches what `p` matches and binds what `p`
binds.

The subject has type `Bool`, `Option[T]`, a tuple type, or a record or enum
type, and a subject of any other type is a compile error. So is a pattern that
does not have the type of the subject. The expressions of all arms have the
same type, which is the type of the `match` expression, and arms of different
types are a compile error.

A subject that is a [record](#record-expressions) or
[enum expression](#enum-expressions) with fields is a compile error unless it is
wrapped in parentheses, since its `{` would be read as the start of the arms.

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
TuplePattern ::= "(" Pattern ( "," Pattern )+ ","? ")"
               | "(" Pattern "," ")"
               | "(" ")"
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
  // error: Pattern does not match type (String, Int)
  (name, _, _) => name,
}
```

<a id="record-patterns"></a>

#### Record patterns

A record pattern `R {f: p, …}` matches a record whose fields match their
patterns, and binds what its field patterns bind. A field pattern `f` without
`: p` is shorthand for `f: f`, which binds the field to a variable of the same
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
// error: Missing pattern(s) Some(false)
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
  // error: Unreachable pattern true
  true => "b",
}
```

<a id="for-expressions"></a>

### For expressions

A `for` expression evaluates its body, which has type `Html`, once for each
element of an array, with the variable bound to that element, and evaluates to
the results concatenated in order.

```ebnf
ForExpr ::= "for" ( LowercaseIdentifier | "_" ) "in" Expr ( "..=" Expr )? BlockExpr
```

A `for` can also loop over a range `a..=b`: the `Int` values from `a` to `b`
inclusive. The range is empty if `a` is greater than `b`. A range anywhere
else, such as an expression on its own, is a compile error.

`_` in place of the variable binds nothing. Looping over anything other than an
array or a range of `Int` values is a compile error. So is a body whose type is
not `Html`.

```hop
for name in ["Alice", "Bob"] { <li>{name}</li> } // <li>Alice</li><li>Bob</li>
for i in 1..=3 { <b>{i.to_string()}</b> }        // <b>1</b><b>2</b><b>3</b>
for tag in "a, b" { <br/> }                      // error: Mismatched type: expected Array[...] got String
for _ in 1..=3 { "*" }                           // error: Mismatched type for for body: expected Html got String
```

<a id="markup-expressions"></a>

### Markup expressions

A markup expression is an element, fragment or markup call expression, and has
type `Html`. An `Html` value is a sequence of elements and text. An element has
a name, attributes in the order written, and content, which is itself a
sequence of elements and text. An attribute has a name and an optional `String`
value.

```ebnf
MarkupExpr ::= ElementExpr | FragmentExpr | MarkupCallExpr
```

An end tag that does not have the same name as its start tag is a compile error.

<a id="element-expressions"></a>

#### Element expressions

An element expression `<x …>…</x>` evaluates to the element with that name,
those attributes and that content. An element without content can be written
with a single self-closing tag, `<x/>`, which is shorthand for `<x></x>`.

```ebnf
ElementExpr ::= "<" ElementName Attribute* ">" MarkupContent "</" ElementName ">"
              | "<" ElementName Attribute* "/>"
ElementName ::= [a-z] [A-Za-z0-9-]*
```

The name is that of an HTML or SVG element, such as `div` or `path`, or of a
custom element, which contains `-`, such as `my-widget`. Any other name is a
compile error.

Using `<html>`, `<head>` or `<body>` is a compile error, since a
[page](#page-declarations) provides them, and so is using `<style>`: styles go
in the project stylesheet. Using `<base>`, `<embed>` or `<object>`, or a
`<script>` with content, is a compile error for [XSS safety](#xss-safety).

A void element is one of `area`, `br`, `col`, `hr`, `img`, `input`, `link`,
`meta`, `source`, `track` and `wbr`, and a void element with content is a
compile error.

```hop
<p class="note">Hello</p> // <p class="note">Hello</p>
<div/>                    // <div></div>
<br/>                     // <br>
<br></br>                 // <br>
<br>text</br>             // error: <br> is a void element and cannot have content
```

<a id="fragment-expressions"></a>

#### Fragment expressions

A fragment expression `<>…</>` evaluates to its content, without an element
around it, and `<></>` evaluates to the empty sequence.

```ebnf
FragmentExpr ::= "<>" MarkupContent "</>"
```

A fragment lets markup content be used where one expression is expected:

```hop
<><i>hello</i> <b>world</b></> // <i>hello</i> <b>world</b>
```

<a id="markup-call-expressions"></a>

#### Markup call expressions

A markup call expression `<F …></F>` evaluates to the value that the
[function](#function-declarations) `F` returns for its arguments. Each
[attribute](#attributes) is the argument for the parameter it names.

```ebnf
MarkupCallExpr ::= "<" UppercaseIdentifier Attribute* ">" MarkupContent "</" UppercaseIdentifier ">"
                 | "<" UppercaseIdentifier Attribute* "/>"
```

Like an element, a markup call without content can be written with a single
self-closing tag, `<F/>`, which is shorthand for `<F></F>`. Content between the
tags is shorthand for a `children` attribute: `<F>…</F>` is the same as `<F
children={<>…</>}/>`. A markup call without content passes no `children`
argument.

The function `F` accepts an attribute for each of its parameters and, if it has
a [rest parameter](#rest-parameters), the attributes the rest parameter accepts.
A markup call is a compile error if `F` does not return `Html`, if it leaves out
a parameter that has no default value, if an argument does not have the type of
its parameter, or if it has an attribute that `F` does not accept.

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

<Badge label="new"><b>!</b></Badge>           // <span>new<b>!</b></span>
<Badge label="new" children={<><b>!</b></>}/> // <span>new<b>!</b></span>
<Badge label="new"></Badge>                   // error: Function Badge requires arguments: children
```

<a id="markup-content"></a>

#### Markup content

The content of a markup expression is a sequence of [markup text](#markup-text),
[markup interpolations](#markup-interpolations) and
[markup expressions](#markup-expressions), between which
[comments](#comments) can appear.

```ebnf
MarkupContent ::= ( MarkupText | MarkupInterpolation | MarkupExpr )*
```

<a id="markup-text"></a>

#### Markup text

Markup text evaluates to its characters as written, after its
[whitespace](#whitespace-normalization) is normalized.

```ebnf
MarkupText ::= [^<{}]+
```

Text is not [escaped](#escaping), so `&amp;` passes through unchanged. Text
cannot contain `<`, `{` or `}`. To display them, write the character references
`&lt;`, `&lbrace;` and `&rbrace;`, which pass through like any other text.

<a id="markup-interpolations"></a>

#### Markup interpolations

A markup interpolation `{e}` is a [block expression](#block-expressions) whose
value is inserted into the content of a markup expression.

```ebnf
MarkupInterpolation ::= BlockExpr
```

The block expression must have type `String` or `Html`. Any other type is a
compile error. If it has type `Html`, its value is inserted as the elements and
text it consists of. If it has type `String`, its value is inserted as text,
[escaped](#escaping):

```hop
let text = "a < b";
let ok = <div>ok</div>;

<div>{text}</div> // <div>a &lt; b</div>
<div>{ok}</div>   // <div><div>ok</div></div>
```

<a id="attributes"></a>

#### Attributes

An attribute is written in the start tag of an element or markup call. It is a
name alone, as in `disabled`, a name with a value, as in `id={e}`, or a spread
`...rest` of a [rest parameter](#rest-parameters). A value is a
[block](#block-expressions) or a [string literal](#literal-expressions), and
`id="main"` is shorthand for `id={"main"}`.

```ebnf
Attribute      ::= AttributeName ( "=" AttributeValue )?
                 | "..." LowercaseIdentifier
AttributeName  ::= [A-Za-z] [A-Za-z0-9_:.-]*
AttributeValue ::= BlockExpr | StringLiteral
```

On an [element](#element-expressions), an attribute renders in the start tag.
Its value must have type `String` and is [escaped](#escaping):

```hop
<div id={1}></div>               // error: Mismatched type for attribute: expected String got Int
<input disabled/>                // <input disabled>
<input pattern="\\d+"/>          // <input pattern="\d+">
<span title="say \"hi\""></span> // <span title="say &quot;hi&quot;"></span>
<abbr title="R&D"></abbr>        // <abbr title="R&amp;D"></abbr>
<abbr title="R&amp;D"></abbr>    // <abbr title="R&amp;amp;D"></abbr>
```

An element defined by HTML, such as `div`, accepts the global attributes of
HTML, the attributes HTML defines for that element, and any attribute whose name
starts with `data-` or `aria-`. Any other attribute, such as `href` on a `div`,
is a compile error. An SVG element, such as `path`, or a custom element accepts
any attribute, except as follows.

For [XSS safety](#xss-safety), no element accepts an attribute whose name
starts with `on`, such as `onclick`. Some attributes, such as the `src` of a
`<script>`, accept only a string literal, written as `src="…"` or `src={"…"}`.

On a [markup call](#markup-call-expressions), a name alone is the argument
`true`.

An attribute written more than once in a start tag, as in
`<div id="a" id="b">`, is a compile error.

<a id="rest-parameters"></a>

#### Rest parameters

A rest parameter `...rest` is the last parameter, and collects the attributes a
caller passes that are not parameters of the function. The body spreads it, as
`...rest`, in the start tag of an element or markup call, where the collected
attributes are placed as if written there. A rest parameter that is not the
last parameter, or that the body does not spread exactly once, is a compile
error. For example:

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

<Button kind="k" id="x" disabled/> // <button class="k" id="x" disabled>k</button>
<PrimaryButton type="submit"/>     // <button class="primary" type="submit">primary</button>
```

Exactly once means once in the source text, not once per evaluation: a spread
in each arm of a `match` is a compile error, while a single spread inside a
`for` body is allowed, and adds the attributes on every iteration.

A rest parameter accepts the attributes that the [element](#attributes) or
[function](#markup-call-expressions) it is spread into accepts, except those
written in the same start tag. For example, `class` is written on the
`<button>` where `Button` spreads `rest`, so `Button` does not accept it:

```hop
fn Button(
  kind: String,
  ...rest,
) -> Html {
  <button class={kind} ...rest>
    {kind}
  </button>
}

<Button kind="k" class="c"/> // error: Function Button does not accept attribute 'class'
```

Spreading rest parameters in a cycle is a compile error:

```hop
fn A(...rest) -> Html {
  <B ...rest/> // error: Rest spread of A forms a cycle and never reaches an element
}

fn B(...rest) -> Html {
  <A ...rest/> // error: Rest spread of B forms a cycle and never reaches an element
}
```

<a id="whitespace-normalization"></a>

#### Whitespace normalization

Whitespace in the content of a markup expression is normalized as it is written
in the source. A run of whitespace that contains a line break becomes a single
space if there is text on both sides of it, and is removed otherwise.
Whitespace at the start and end of the content is also removed. Any other
whitespace is kept as written.

```
-- before
<p>   one   two   </p>
-- after
<p>one   two</p>
--

-- before
<p>
  one
  two

  three
</p>
-- after
<p>one two three</p>
--

-- before
<p>
  Hello <b>world</b>
  again
</p>
-- after
<p>Hello <b>world</b>again</p>
--

-- before
<p>
  Hello
  {" "}
  <b>world</b>
</p>
-- after
<p>Hello{" "}<b>world</b></p>
--
```

<a id="escaping"></a>

#### Escaping

A `String` value inserted into markup, as a
[markup interpolation](#markup-interpolations) or as an
[attribute value](#attributes), is escaped: each character below is replaced by
the character reference next to it, and every other character is kept
unchanged.

```
&   &amp;
<   &lt;
>   &gt;
"   &quot;
```

For example:

```hop
let s = "<b> & \"c\"";

<p>{s}</p> // <p>&lt;b&gt; &amp; &quot;c&quot;</p>
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
error. A field can refer to the type it belongs to, as in
`children: Array[Item]` in a record `Item`.

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
[called by a markup call](#markup-call-expressions).

```ebnf
FunctionDecl ::= "pub"? "fn" ( LowercaseIdentifier | UppercaseIdentifier ) "(" ( Param ( "," Param )* ","? )? ")" "->" Type BlockExpr
Param        ::= LowercaseIdentifier ":" Type ( "=" Expr )?
               | "..." LowercaseIdentifier
```

A parameter has one of these forms:

```
x: T       a parameter of type T
x: T = v   a parameter with the default value v
...x       a rest parameter
```

A function body that does not have the declared return type is a compile error:

```hop
fn answer() -> Int {
  "x" // error: Mismatched type for function body: expected Int got String
}
```

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

Only an uppercase function that returns `Html` can be called by a markup call,
and calling an uppercase function that returns another type is a compile error:

```hop
fn Label() -> String {
  "x"
}

fn Form() -> Html {
  // error: Only a function returning Html can be called by a markup call
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
field or a variant field, is a compile error, for
[XSS safety](#xss-safety).

<a id="rendering"></a>

## Rendering

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

The document is the concatenation of these parts:

1. `<!doctype html>`
2. `<html><head>`
3. `<meta charset="utf-8">`
4. `<meta content="width=device-width, initial-scale=1" name="viewport">`
5. the rendering of the value of `head`, if the page has one
6. `</head><body>`
7. the rendering of the value of `body`
8. `</body></html>`

A host can add further elements to the end of the `<head>`, such as a stylesheet
or a script. What it adds is not part of the language.

An `Html` value renders as the renderings of its elements and text in order.
Text renders as its characters. An element renders as its start tag, its
content and its end tag, except that a [void element](#element-expressions) has
no end tag and no content. The start tag is `<`, the name, the attributes and
`>`. Each attribute is a space followed by its name and, if it has a value,
`="`, the value and `"`. The end tag is `</`, the name and `>`.

For example, with the value of `v` [escaped](#escaping):

```hop
let v = "R&D";

<br/>                        // <br>
<div/>                       // <div></div>
<input disabled value={v}/>  // <input disabled value="R&amp;D">
```

<a id="xss-safety"></a>

## XSS safety

The rendering of a page consists of markup written in the modules of the
project and of `String` values, such as the arguments of the page. Markup text
is not [escaped](#escaping) and renders as written. A `String` value is escaped
wherever it is inserted, as a [markup interpolation](#markup-interpolations) or
as an [attribute value](#attributes), so it renders as text or as the value of
a single attribute, and cannot start or end an element or an attribute.

In the [rendering](#rendering) of a page, `<meta charset="utf-8">` comes before
the value of `head`, so a page cannot place anything before the character set
declaration.

The rules below are compile errors that keep the arguments of a page out of
places where escaping is not enough.

An `Html` value is built only by markup expressions, so a page parameter whose
type is `Html`, or contains `Html` as an element, a field or a variant field,
is a compile error:

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

Using `<base>`, `<embed>` or `<object>` is a compile error, and so is a
`<script>` that has content. A script is written as a reference to a file
instead:

```hop
<script src="/app.js"></script> // <script src="/app.js"></script>
// error: Inline <script> content is not allowed: move the code to a file and reference it with <script src="...">
<script>alert(1)</script>
```

No element accepts an attribute whose name starts with `on`, ignoring case,
such as the event handler `onclick`. This includes SVG and custom elements,
which otherwise accept any attribute. So
`<button onclick="go()">` and `<svg onload="init()">` are compile errors.

The attributes below load a script or a document, or set the value of another
attribute. Their value is a string literal, written as `name="text"` or
`name={"text"}`, and any other expression is a compile error, whether it is
written on the element or passed through a [rest parameter](#rest-parameters).
The names are matched ignoring case.

```
Element        Attributes
animate, set   attributeName, by, from, to, values
iframe         srcdoc
script         src
```

Even a variable bound to a string literal is a compile error:

```hop
let url = "/app.js";
// error: <script> requires a string literal for attribute 'src'
<script src={url}></script>
```

Other attributes accept any `String`, escaped but otherwise unchecked. In
particular, the scheme of a URL is not checked, so an `href`, `src`, `action`
or `formaction` can hold a `javascript:` URL, and a `style` can hold any CSS:

```hop
let url = "javascript:alert(1)";
<a href={url}>Home</a> // <a href="javascript:alert(1)">Home</a>
```

A host that passes such a value to a page is responsible for checking it.

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
