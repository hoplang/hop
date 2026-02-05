# Hop Manual

## Overview

Hop is a programming language that compiles to **TypeScript**, **Go**, **Python**, and **Rust**. It is designed for building HTML-based user interfaces with an HTML-like syntax, components, pattern matching, and type safety.

## Commands

### hop build

Compile a hop project to the target language.

```
hop build [--project PATH] [--no-optimize]
```

**Options:**
- `--project PATH` — Path to project root (defaults to current directory)
- `--no-optimize` — Skip optimization passes

The target language and output path are configured in `hop.toml`. The compiler loads all `.hop` modules, parses and type-checks the code, processes CSS, and writes the output file.

### hop dev

Start a development server with live reload.

```
hop dev [--project PATH] [--port PORT] [--host HOST]
```

**Options:**
- `--project PATH` — Path to project root (defaults to current directory)
- `--port PORT` — Port to serve on (default: 3000)
- `--host HOST` — Host to bind to (default: localhost)

The dev server watches `.hop` files and CSS for changes, automatically recompiles, and reloads the browser via Server-Sent Events. It serves a preview interface at `/` with individual entrypoints at `/preview/{name}`.

### hop fmt

Format hop source files.

```
hop fmt [--project PATH] [FILE]
```

**Options:**
- `--project PATH` — Path to project root (defaults to current directory)
- `FILE` — Specific `.hop` file to format (formats all files if omitted)

Parses and reformats `.hop` files with consistent style. Only overwrites files that changed. Fails atomically if any file has parse errors.

### hop man

Display this manual.

```
hop man
```

## Project Structure

A hop project looks like this:

```
my-app/
├── hop.toml
├── index.hop
├── components/
│   ├── button.hop
│   └── card.hop
└── styles/
    └── input.css
```

### hop.toml

The project configuration file. Example:

```toml
[build]
target = "ts"
output_path = "app.ts"

[css]
mode = "tailwind4"
```

**Fields:**
- **build.target** — Target language: `"ts"`, `"go"`, `"python"`, or `"rust"`
- **build.output_path** — Where the compiled output is written
- **css.mode** — CSS processing mode (e.g. `"tailwind4"`)
- **css.tailwind.input** — Custom Tailwind CSS input file (optional)

### .hop files

Each `.hop` file is a **module**. Modules can contain imports, type definitions (records, enums), components, and entrypoints.

## Language Reference

### Imports

```hop
import components::button::Button
import lucide::ArrowRight
```

Module paths correspond to file paths. `import components::button::Button` imports the `Button` type from `components/button.hop`.

### Types

**Built-in types:** `String`, `Bool`, `Int`, `Float`, `TrustedHTML`, `Array[T]`, `Option[T]`

**Records** — named data structures:

```hop
record User {
  name: String,
  email: String,
}
```

**Enums** — tagged unions:

```hop
enum Device {
  Desktop,
  Mobile,
}

enum Result {
  Ok { value: Int },
  Error { message: String },
}
```

**Option** — represents a value that may or may not be present:

```hop
Some("hello")
None
```

Pattern match to extract the value:

```hop
match option {
  Some(value) => "Got: " + value,
  None => "empty",
}
```

### Methods

Built-in types have methods that can be called with dot syntax. Methods can be chained.

**Array[T]:**
- `.len()` → `Int` — number of elements
- `.is_empty()` → `Bool` — true if the array has no elements

**Int:**
- `.to_string()` → `String` — convert to string
- `.to_float()` → `Float` — convert to float

**Float:**
- `.to_string()` → `String` — convert to string
- `.to_int()` → `Int` — convert to integer (truncates)

```hop
<let {count: Int = items.len()}>
  <p>{count.to_string()} items</p>
</let>
```

### Components

Reusable UI elements with typed parameters and optional defaults:

```hop
<Button {text: String, disabled: Bool = false}>
  <button disabled={disabled}>{text}</button>
</Button>
```

Use components like HTML elements:

```hop
<Button text="Click me"/>
```

**Children** — components can accept nested content via a `children` parameter typed as `TrustedHTML`:

```hop
<Card {title: String, children: TrustedHTML}>
  <div class="card">
    <h2>{title}</h2>
    {children}
  </div>
</Card>
```

Content between the component's tags is passed as `children`:

```hop
<Card title="Welcome">
  <p>This becomes the children content.</p>
</Card>
```

Use `Option[TrustedHTML]` to make children optional:

```hop
<Section {children: Option[TrustedHTML] = None}>
  <div>
    <match {children}>
      <case {Some(c)}>{c}</case>
      <case {None}><p>Default content</p></case>
    </match>
  </div>
</Section>

<Section />
<Section><p>Custom content</p></Section>
```

### Entrypoints

Top-level pages that can be rendered directly. All parameters must have defaults:

```hop
entrypoint Index(
  title: String = "Home",
) {
  <head>
    <title>{title}</title>
  </head>
  <body>
    <h1>{title}</h1>
  </body>
}
```

### HTML

Hop uses HTML-like syntax for rendering:

```hop
<div class="container">
  <h1>Hello, {name}!</h1>
  <a href={"/users/" + id}>Profile</a>
</div>
```

### Control Flow

**Conditional rendering:**

```hop
<if {user.is_logged_in()}>
  <p>Welcome back!</p>
</if>
```

**List rendering:**

```hop
<for {item in items}>
  <div>{item.name}</div>
</for>
```

**Pattern matching in templates:**

```hop
<match {device}>
  <case {Device::Desktop}>
    <div>Desktop layout</div>
  </case>
  <case {Device::Mobile}>
    <div>Mobile layout</div>
  </case>
</match>
```

**Local variables:**

```hop
<let {count: Int = items.len()}>
  <p>Total: {count}</p>
</let>
```

### Expressions

**Match expressions:**

```hop
match result {
  Result::Ok { value: v } => "Got: " + v,
  Result::Error { message: m } => "Error: " + m,
}
```

**Operators:** `+`, `-`, `*`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`, `!`

**join! macro** — concatenate strings with spaces (useful for CSS classes):

```hop
class={join!("btn", "btn-primary")}
```

### Record and Enum Construction

```hop
User { name: "Alice", email: "alice@example.com" }
Device::Desktop
Result::Ok { value: 42 }
```

## Compilation Output

Hop compiles to idiomatic code in the target language. Here's what the generated TypeScript looks like (other targets follow similar patterns).

**Records** compile to classes:

```typescript
export class User {
    constructor(
        public readonly name: string,
        public readonly email: string,
    ) {}
}
```

**Enums** compile to namespaces with tagged unions:

```typescript
export namespace Device {
    export type Device =
        | { readonly tag: "Desktop" }
        | { readonly tag: "Mobile" };

    export function Desktop(): Device { return { tag: "Desktop" }; }
    export function Mobile(): Device { return { tag: "Mobile" }; }
}
```

**Entrypoints** compile to functions returning a string of HTML:

```typescript
export function Index(title: string = "Home"): string {
    let output: string = "";
    output += "<head><title>";
    output += escapeHtml(title);
    output += "</title></head>";
    return output;
}
```

**CSS** is inlined as a `<style>` element in the `<head>` of the output.
