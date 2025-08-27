# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This repository contains the implementation for a HTML-like template language called `hop`.

## Development Environment

This project is written in rust and the source files are located in `src/`.

This project uses Nix for development environment management. When running Rust/Cargo commands, always use:

```bash
nix develop --command <cargo command>
```

For example:
- `nix develop --command cargo check`
- `nix develop --command cargo build`
- `nix develop --command cargo test`

## Testing

The project uses a custom format for test cases located in the `test_data/` directory.

Examples:

- `test_data/tokenizer.cases` - Tokenizer tests
- `test_data/parser.cases` - Parser tests  
- `test_data/typechecker.cases` - Typechecker tests

and so on.

### Test File Format

Each test file uses the txtar format with test cases marked by `## BEGIN` and `## END` blocks.

Here's an excerpt from a test file containing two test cases (the test may optionally have a comment above, which is simply ignored when running the test).

```
When a tag is not properly closed the parser outputs an error.

## BEGIN
-- main.hop --
<main-comp>
	<div>
</main-comp>
-- out --
Unclosed <div>
## END

When a void tag is closed with an end tag the parser outputs an error.

## BEGIN
-- main.hop --
<main-comp>
	<hr></hr>
</main-comp>
-- out --
<hr> should not be closed using a closing tag
## END
```

When you add tests to the project you are expected to add them to these files. It is considered good practice to add a comment above each test case.

### Snapshot Testing

Some tests use snapshot testing with the [expect-test](https://github.com/rust-analyzer/expect-test) library instead of the txtar format. These are inline tests with `expect!` macros that capture expected output.

For example, the DOP expression parser tests in `src/dop/parser.rs` use snapshot testing.

To update snapshots when the expected output changes, set the `UPDATE_EXPECT` environment variable:

```bash
nix develop --command env UPDATE_EXPECT=1 cargo test
```

This will automatically update the inline `expect!` blocks with the new actual output.

## The hop language

Typical hop code looks like this:
```hop
<for {item in items}>
  <if {item.active}>
    <div>{item.title}</div>
  </if>
</for>
```

The expression language inside the curly braces is internally referred to as `dop` (short for "data operations"). `dop` expressions may be embedded in `hop`, but not the other way around.

### Iteration

The `<for>` tag iterates over a `dop` array. The content of the `<for>` tag is evaluated once for each item of the array. Example:

```
<for {item in items}>
  <div>{item}</div>
</for>
```

Given that items is the array `["foo", "bar"]` the output of evaluating the code will be two `div` tags each containing the text `foo` and `bar`.

### Conditional rendering

The `<if>` tag renders its content when a `dop` expression evaluates to `true`. Example:

```
<if {user.name == "admin"}>
  <div>{user.name} is an admin</div>
</if>
```

