# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

This repository contains the implementation for a HTML-like template language
called `hop`.

## Development Environment

This project is written in rust and the source files are located in `src/`.

This project uses Nix for development environment management. When running
Rust/Cargo commands, always use:

```bash
nix develop --command <cargo command>
```

For example:
- `nix develop --command cargo check`
- `nix develop --command cargo build`
- `nix develop --command cargo test`

## Testing

### Snapshot Testing

Some tests use snapshot testing with the
[expect-test](https://github.com/rust-analyzer/expect-test) library instead of
the txtar format. These are inline tests with `expect!` macros that capture
expected output.

For example, the `dop` expression parser tests in `src/dop/parser.rs` use
snapshot testing.

To update snapshots when the expected output changes, set the `UPDATE_EXPECT`
environment variable:

```bash
nix develop --command env UPDATE_EXPECT=1 cargo test
```

This will automatically update the inline `expect!` blocks with the new actual
output.

## The hop language

Typical hop code looks like this:
```hop
<main-component {items: array[{active: boolean, title: string}]}>
  <for {item in items}>
    <if {item.active}>
      <div>{item.title}</div>
    </if>
  </for>
</main-component>
```

The code above declares a component `main-component` and has a single parameter
`items` that is an array of objects.

The expression language inside the curly braces is internally referred to as
`dop` (short for "data operations"). `dop` expressions may be embedded in
`hop`, but not the other way around.

### Iteration

The `<for>` tag iterates over a `dop` array. The content of the `<for>` tag is
evaluated once for each item of the array. Example:

```
<for {item in items}>
  <div>{item}</div>
</for>
```

Given that items is the array `["foo", "bar"]` the output of evaluating the
code will be two `div` tags each containing the text `foo` and `bar`.

### Conditional rendering

The `<if>` tag renders its content when a `dop` expression evaluates to `true`.
Example:

```
<if {user.name == "admin"}>
  <div>{user.name} is an admin</div>
</if>
```

