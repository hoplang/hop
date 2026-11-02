use std::fmt::{self, Write};

/// Writes to the inner writer, indenting every line after the first by two
/// spaces. Nested `Indent`s indent further. Empty lines stay empty.
pub struct Indent<'a> {
    inner: &'a mut dyn Write,
    at_line_start: bool,
}

impl<'a> Indent<'a> {
    pub fn new(inner: &'a mut dyn Write) -> Self {
        Self {
            inner,
            at_line_start: false,
        }
    }
}

impl Write for Indent<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for (i, line) in s.split('\n').enumerate() {
            if i > 0 {
                self.inner.write_char('\n')?;
                self.at_line_start = true;
            }
            if !line.is_empty() {
                if self.at_line_start {
                    self.inner.write_str("  ")?;
                    self.at_line_start = false;
                }
                self.inner.write_str(line)?;
            }
        }
        Ok(())
    }
}

/// The items separated by `sep`.
pub fn join<I>(items: I, sep: &str) -> impl fmt::Display
where
    I: IntoIterator + Clone,
    I::Item: fmt::Display,
{
    fmt::from_fn(move |f| {
        for (i, item) in items.clone().into_iter().enumerate() {
            if i > 0 {
                f.write_str(sep)?;
            }
            write!(f, "{item}")?;
        }
        Ok(())
    })
}

/// The elements of a tuple in parentheses, with a trailing comma when there
/// is exactly one.
pub fn write_tuple<T: fmt::Display>(f: &mut fmt::Formatter<'_>, elements: &[T]) -> fmt::Result {
    let comma = if elements.len() == 1 { "," } else { "" };
    write!(f, "({}{comma})", join(elements, ", "))
}

/// `open`, then each item indented on a line of its own and followed by a
/// comma, then `close` on a line of its own. Without items, `open` and
/// `close` are written together.
pub fn write_lines<I>(
    f: &mut fmt::Formatter<'_>,
    open: impl fmt::Display,
    items: I,
    close: &str,
) -> fmt::Result
where
    I: IntoIterator,
    I::Item: fmt::Display,
{
    write!(f, "{open}")?;
    let mut any = false;
    {
        let mut indented = Indent::new(f);
        for item in items {
            any = true;
            write!(indented, "\n{item},")?;
        }
    }
    if any {
        f.write_char('\n')?;
    }
    f.write_str(close)
}

/// An enum variant as its name, then its fields in braces if it has any.
pub fn write_variant<T: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    name: &str,
    fields: &[T],
) -> fmt::Result {
    if fields.is_empty() {
        return f.write_str(name);
    }
    write!(f, "{name} {{{}}}", join(fields, ", "))
}
