use crate::hop::parsing::parse_expr::Restrictions;
use arbitrary::{Result, Unstructured};
use std::ops::RangeInclusive;

const DEPTH: usize = 3;

const ANYWHERE: Restrictions = Restrictions {
    forbid_record_literals: false,
};
const BEFORE_BRACE: Restrictions = Restrictions {
    forbid_record_literals: true,
};

const VAR_NAMES: &[&str] = &["a", "b", "x", "foo", "foo_bar", "x1"];

const FIELD_NAMES: &[&str] = &["a", "b", "name", "foo_bar", "x1"];

const TYPE_NAMES: &[&str] = &["Foo", "Bar", "Baz1", "Point", "Color"];

const FUNCTION_NAMES: &[&str] = &["f", "foo", "foo_bar", "Card", "Button"];

const STRING_LITERALS: &[&str] = &[
    r#""""#,
    r#""foo""#,
    r#""a b""#,
    r#""a\"b""#,
    r#""back\\slash""#,
    r#""line\nbreak""#,
    r#""cr\rtab\t""#,
    r#""<b>{x}</b>""#,
    "\"raw\nnewline\"",
];

const INT_LITERALS: &[&str] = &["0", "1", "42", "2147483647"];

const FLOAT_LITERALS: &[&str] = &["0.5", "1.0", "3.25", "100.125"];

const BINARY_OPERATORS: &[&str] = &["==", "!=", "<", ">", "<=", ">=", "&&", "||", "+", "-", "*"];

const MARKUP_TEXT: &[&str] = &[
    "hello",
    " a b ",
    "x > y",
    "it's \"q\"",
    "tab\there",
    "&amp;",
    "a-b_c.d/e",
    "/",
    // Inside text `//` is not parsed as a comment
    "// not a comment",
    "  ",
];

const ELEMENT_TAGS: &[&str] = &[
    "div", "span", "p", "a", "ul", "li", "title", "my-el", "colgroup",
];

const VOID_TAGS: &[&str] = &["br", "hr", "img", "input"];

/// The only content a <script> may hold: <style> is rejected outright, and a
/// <script> may only reference an external file.
const RAW_TEXT: &[&str] = &["", " ", "\n", "\n  "];

const ATTRIBUTE_NAMES: &[&str] = &["class", "id", "data-x", "aria:label", "x.y", "on_click"];

const ATTRIBUTE_VALUES: &[&str] = &["", "a", "a b", "{x}", "it's", "<b>"];

const MARKUP_COMMENTS: &[&str] = &["<!-- c -->", "<!---->", "<!-- a-b -- c -->"];

/// Generate a random source that parses without errors.
pub fn random_source(u: &mut Unstructured<'_>) -> Result<String> {
    let mut out = String::new();
    for i in 0..u.int_in_range(1..=3)? {
        if i > 0 {
            out.push('\n');
            ws(u, &mut out)?;
        }
        declaration(u, &mut out)?;
    }
    ws(u, &mut out)?;
    Ok(out)
}

/// Whitespace between two tokens in expression mode, sometimes with a
/// comment.
fn ws(u: &mut Unstructured<'_>, out: &mut String) -> Result<()> {
    out.push_str(if u.int_in_range(0..=7)? == 7 {
        u.choose(&[" // c\n", "\n  // c\n"])?
    } else {
        u.choose(&[" ", "\n", "  "])?
    });
    Ok(())
}

fn list(
    u: &mut Unstructured<'_>,
    count: RangeInclusive<usize>,
    out: &mut String,
    mut item: impl FnMut(&mut Unstructured<'_>, usize, &mut String) -> Result<()>,
) -> Result<()> {
    let n = u.int_in_range(count)?;
    for i in 0..n {
        if i > 0 {
            out.push(',');
            ws(u, out)?;
        }
        item(u, i, out)?;
    }
    if n > 0 && u.arbitrary()? {
        out.push(',');
    }
    Ok(())
}

fn declaration(u: &mut Unstructured<'_>, out: &mut String) -> Result<()> {
    enum P {
        Function,
        Record,
        Enum,
        Page,
        Import,
    }
    let kinds = [P::Function, P::Record, P::Enum, P::Page, P::Import];
    let p = u.choose(&kinds)?;
    if !matches!(p, P::Import) && u.arbitrary()? {
        out.push_str("pub");
        ws(u, out)?;
    }
    match p {
        P::Function => {
            out.push_str("fn");
            ws(u, out)?;
            out.push_str(u.choose(FUNCTION_NAMES)?);
            out.push('(');
            let params = u.int_in_range(0..=3)?;
            let rest = u.arbitrary()?;
            for i in 0..params {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(u.choose(VAR_NAMES)?);
                out.push_str(": ");
                type_(u, 2, out)?;
                if u.arbitrary()? {
                    out.push_str(" = ");
                    expr(u, DEPTH, ANYWHERE, out)?;
                }
            }
            if rest {
                if params > 0 {
                    out.push_str(", ");
                }
                out.push_str("...");
                out.push_str(u.choose(VAR_NAMES)?);
            }
            if (params > 0 || rest) && u.arbitrary()? {
                out.push(',');
            }
            out.push_str(") -> ");
            type_(u, 2, out)?;
            out.push_str(" {");
            ws(u, out)?;
            block_body(u, DEPTH, out)?;
            ws(u, out)?;
            out.push('}');
        }
        P::Record => {
            out.push_str("record");
            ws(u, out)?;
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str(" {");
            ws(u, out)?;
            list(u, 0..=3, out, field_declaration)?;
            ws(u, out)?;
            out.push('}');
        }
        P::Enum => {
            out.push_str("enum");
            ws(u, out)?;
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str(" {");
            ws(u, out)?;
            list(u, 0..=3, out, |u, i, out| {
                out.push_str(u.choose(TYPE_NAMES)?);
                out.push_str(&i.to_string());
                if u.arbitrary()? {
                    out.push_str(" {");
                    ws(u, out)?;
                    list(u, 0..=2, out, field_declaration)?;
                    ws(u, out)?;
                    out.push('}');
                }
                Ok(())
            })?;
            ws(u, out)?;
            out.push('}');
        }
        P::Page => {
            out.push_str("page");
            ws(u, out)?;
            out.push_str(u.choose(TYPE_NAMES)?);
            if u.arbitrary()? {
                out.push('(');
                list(u, 0..=3, out, |u, _, out| {
                    out.push_str(u.choose(VAR_NAMES)?);
                    out.push_str(": ");
                    type_(u, 2, out)
                })?;
                out.push(')');
            }
            out.push_str(" {");
            let members: &[&str] = match (u.arbitrary()?, u.arbitrary()?) {
                (false, _) => &["body"],
                (true, false) => &["body", "head"],
                (true, true) => &["head", "body"],
            };
            for member in members {
                ws(u, out)?;
                out.push_str("fn ");
                out.push_str(member);
                out.push_str("() -> Html {");
                ws(u, out)?;
                block_body(u, DEPTH, out)?;
                ws(u, out)?;
                out.push('}');
            }
            ws(u, out)?;
            out.push('}');
        }
        P::Import => {
            out.push_str("import");
            ws(u, out)?;
            for i in 0..u.int_in_range(2..=3)? {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(u.choose(FUNCTION_NAMES)?);
            }
        }
    }
    Ok(())
}

fn field_declaration(u: &mut Unstructured<'_>, i: usize, out: &mut String) -> Result<()> {
    out.push_str(u.choose(FIELD_NAMES)?);
    out.push_str(&i.to_string());
    out.push_str(": ");
    type_(u, 2, out)
}

fn type_(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    enum P {
        Scalar,
        Named,
        Array,
        Option,
        Tuple,
    }
    let mut kinds = vec![P::Scalar, P::Named];
    if depth > 0 {
        kinds.extend([P::Array, P::Option, P::Tuple]);
    }
    match u.choose(&kinds)? {
        P::Scalar => out.push_str(u.choose(&["Int", "String", "Bool", "Float", "Html"])?),
        P::Named => out.push_str(u.choose(TYPE_NAMES)?),
        P::Array => {
            out.push_str("Array[");
            type_(u, depth - 1, out)?;
            out.push(']');
        }
        P::Option => {
            out.push_str("Option[");
            type_(u, depth - 1, out)?;
            out.push(']');
        }
        P::Tuple => {
            out.push('(');
            list(u, 0..=3, out, |u, _, out| type_(u, depth - 1, out))?;
            out.push(')');
        }
    }
    Ok(())
}

fn block_body(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    for _ in 0..u.int_in_range(0..=2)? {
        out.push_str("let");
        ws(u, out)?;
        out.push_str(u.choose(VAR_NAMES)?);
        if u.arbitrary()? {
            out.push_str(": ");
            type_(u, 2, out)?;
        }
        ws(u, out)?;
        out.push('=');
        ws(u, out)?;
        expr(u, depth, ANYWHERE, out)?;
        out.push(';');
        ws(u, out)?;
    }
    expr(u, depth, ANYWHERE, out)
}

fn expr(
    u: &mut Unstructured<'_>,
    depth: usize,
    restrictions: Restrictions,
    out: &mut String,
) -> Result<()> {
    unary(u, depth, restrictions, out)?;
    if u.int_in_range(0..=7)? == 7 {
        for _ in 0..u.int_in_range(1..=3)? {
            ws(u, out)?;
            out.push_str(u.choose(BINARY_OPERATORS)?);
            ws(u, out)?;
            unary(u, depth, restrictions, out)?;
        }
    }
    Ok(())
}

fn unary(
    u: &mut Unstructured<'_>,
    depth: usize,
    restrictions: Restrictions,
    out: &mut String,
) -> Result<()> {
    for _ in 0..u.int_in_range(0..=2)? {
        out.push_str(u.choose(&["!", "-", "- "])?);
    }
    primary(u, depth, restrictions, out)?;
    if u.int_in_range(0..=3)? == 3 {
        for _ in 0..u.int_in_range(1..=2)? {
            out.push('.');
            out.push_str(u.choose(FIELD_NAMES)?);
            if u.arbitrary()? {
                out.push_str("()");
            }
        }
    }
    Ok(())
}

fn primary(
    u: &mut Unstructured<'_>,
    depth: usize,
    restrictions: Restrictions,
    out: &mut String,
) -> Result<()> {
    enum P {
        Var,
        Str,
        Bool,
        Int,
        Float,
        None,
        EnumUnit,
        Call,
        Macro,
        Array,
        Paren,
        Block,
        Some,
        Match,
        For,
        Markup,
        EnumFields,
        Record,
    }
    let mut kinds = vec![
        P::Var,
        P::Str,
        P::Bool,
        P::Int,
        P::Float,
        P::None,
        P::EnumUnit,
    ];
    if depth > 0 && u.int_in_range(0..=1)? == 1 {
        kinds.extend([
            P::Call,
            P::Macro,
            P::Array,
            P::Paren,
            P::Block,
            P::Some,
            P::Match,
            P::For,
            P::Markup,
        ]);
        if !restrictions.forbid_record_literals {
            kinds.extend([P::EnumFields, P::Record]);
        }
    }
    let inner = ANYWHERE;
    match u.choose(&kinds)? {
        P::Var => out.push_str(u.choose(VAR_NAMES)?),
        P::Str => out.push_str(u.choose(STRING_LITERALS)?),
        P::Bool => out.push_str(u.choose(&["true", "false"])?),
        P::Int => out.push_str(u.choose(INT_LITERALS)?),
        P::Float => out.push_str(u.choose(FLOAT_LITERALS)?),
        P::None => out.push_str("None"),
        P::EnumUnit => {
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str("::");
            out.push_str(u.choose(TYPE_NAMES)?);
        }
        P::Call => {
            out.push_str(u.choose(VAR_NAMES)?);
            out.push('(');
            let named = u.arbitrary()?;
            list(u, 0..=3, out, |u, _, out| {
                if named {
                    out.push_str(u.choose(VAR_NAMES)?);
                    out.push_str(": ");
                }
                expr(u, depth - 1, inner, out)
            })?;
            out.push(')');
        }
        P::Macro => {
            out.push_str(u.choose(VAR_NAMES)?);
            out.push_str("!(");
            list(u, 0..=3, out, |u, _, out| expr(u, depth - 1, inner, out))?;
            out.push(')');
        }
        P::Array => {
            out.push('[');
            list(u, 0..=3, out, |u, _, out| expr(u, depth - 1, inner, out))?;
            out.push(']');
        }
        P::Paren => {
            out.push('(');
            expr(u, depth - 1, inner, out)?;
            out.push(')');
        }
        P::Block => {
            out.push('{');
            ws(u, out)?;
            block_body(u, depth - 1, out)?;
            ws(u, out)?;
            out.push('}');
        }
        P::Some => {
            out.push_str("Some(");
            expr(u, depth - 1, inner, out)?;
            out.push(')');
        }
        P::Match => {
            out.push_str("match");
            ws(u, out)?;
            expr(u, depth - 1, BEFORE_BRACE, out)?;
            out.push_str(" {");
            ws(u, out)?;
            list(u, 0..=3, out, |u, _, out| {
                pattern(u, 2, out)?;
                out.push_str(" => ");
                expr(u, depth - 1, inner, out)
            })?;
            ws(u, out)?;
            out.push('}');
        }
        P::For => {
            out.push_str("for");
            ws(u, out)?;
            out.push_str(if u.arbitrary()? {
                "_"
            } else {
                u.choose(VAR_NAMES)?
            });
            ws(u, out)?;
            out.push_str("in");
            ws(u, out)?;
            expr(u, depth - 1, BEFORE_BRACE, out)?;
            if u.arbitrary()? {
                out.push_str("..=");
                expr(u, depth - 1, BEFORE_BRACE, out)?;
            }
            out.push_str(" {");
            ws(u, out)?;
            block_body(u, depth - 1, out)?;
            ws(u, out)?;
            out.push('}');
        }
        P::Markup => markup(u, depth - 1, out)?,
        P::EnumFields => {
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str("::");
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str(" {");
            list(u, 0..=3, out, |u, _, out| {
                out.push_str(u.choose(FIELD_NAMES)?);
                out.push_str(": ");
                expr(u, depth - 1, inner, out)
            })?;
            out.push('}');
        }
        P::Record => {
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str(" {");
            // At most one spread, anywhere among the fields.
            let spread = if u.arbitrary()? {
                Some(u.int_in_range(0..=3)?)
            } else {
                None
            };
            list(u, 0..=3, out, |u, i, out| {
                if spread == Some(i) {
                    out.push_str("...");
                    return expr(u, depth - 1, inner, out);
                }
                out.push_str(u.choose(FIELD_NAMES)?);
                out.push_str(": ");
                expr(u, depth - 1, inner, out)
            })?;
            out.push('}');
        }
    }
    Ok(())
}

fn pattern(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    enum P {
        Wildcard,
        Binding,
        True,
        False,
        None,
        Variant,
        Some,
        VariantFields,
        Record,
    }
    let mut kinds = vec![
        P::Wildcard,
        P::Binding,
        P::True,
        P::False,
        P::None,
        P::Variant,
    ];
    if depth > 0 {
        kinds.extend([P::Some, P::VariantFields, P::Record]);
    }
    let p = u.choose(&kinds)?;
    match p {
        P::Wildcard => out.push('_'),
        P::Binding => out.push_str(u.choose(VAR_NAMES)?),
        P::True => out.push_str("true"),
        P::False => out.push_str("false"),
        P::None => out.push_str("None"),
        P::Variant => {
            out.push_str(u.choose(TYPE_NAMES)?);
            out.push_str("::");
            out.push_str(u.choose(TYPE_NAMES)?);
        }
        P::Some => {
            out.push_str("Some(");
            pattern(u, depth - 1, out)?;
            out.push(')');
        }
        P::VariantFields | P::Record => {
            out.push_str(u.choose(TYPE_NAMES)?);
            if matches!(p, P::VariantFields) {
                out.push_str("::");
                out.push_str(u.choose(TYPE_NAMES)?);
            }
            out.push_str(" {");
            list(u, 0..=3, out, |u, _, out| {
                out.push_str(u.choose(FIELD_NAMES)?);
                if u.arbitrary()? {
                    out.push_str(": ");
                    pattern(u, depth - 1, out)?;
                }
                Ok(())
            })?;
            out.push('}');
        }
    }
    Ok(())
}

fn markup(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    enum P {
        Void,
        Comment,
        Element,
        Raw,
        Invocation,
        Fragment,
    }
    let kinds = [
        P::Void,
        P::Comment,
        P::Element,
        P::Raw,
        P::Invocation,
        P::Fragment,
    ];
    let p = u.choose(&kinds)?;
    match p {
        P::Void => {
            out.push('<');
            out.push_str(u.choose(VOID_TAGS)?);
            attributes(u, depth, out)?;
            out.push_str(u.choose(&[">", "/>", " />"])?);
        }
        P::Comment => out.push_str(u.choose(MARKUP_COMMENTS)?),
        P::Element | P::Invocation => {
            let tag = if matches!(p, P::Element) {
                u.choose(ELEMENT_TAGS)?
            } else {
                u.choose(&["Card", "Button", "FooBar"])?
            };
            out.push('<');
            out.push_str(tag);
            attributes(u, depth, out)?;
            if u.arbitrary()? {
                out.push_str(u.choose(&["/>", " />"])?);
            } else {
                out.push('>');
                children(u, depth, out)?;
                out.push_str("</");
                out.push_str(tag);
                out.push('>');
            }
        }
        P::Raw => {
            out.push_str("<script");
            attributes(u, depth, out)?;
            out.push('>');
            out.push_str(u.choose(RAW_TEXT)?);
            out.push_str("</script>");
        }
        P::Fragment => {
            out.push_str("<>");
            children(u, depth, out)?;
            out.push_str("</>");
        }
    }
    Ok(())
}

fn attributes(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    enum P {
        KeyOnly,
        String,
        Spread,
        Expression,
    }
    let mut kinds = vec![P::KeyOnly, P::String, P::Spread];
    if depth > 0 {
        kinds.push(P::Expression);
    }
    for i in 0..u.int_in_range(0..=3)? {
        out.push(' ');
        match u.choose(&kinds)? {
            P::Spread => {
                out.push_str("...");
                out.push_str(u.choose(VAR_NAMES)?);
                continue;
            }
            P::KeyOnly => {
                out.push_str(u.choose(ATTRIBUTE_NAMES)?);
                out.push_str(&i.to_string());
            }
            P::String => {
                out.push_str(u.choose(ATTRIBUTE_NAMES)?);
                out.push_str(&i.to_string());
                out.push_str("=\"");
                out.push_str(u.choose(ATTRIBUTE_VALUES)?);
                out.push('"');
            }
            P::Expression => {
                out.push_str(u.choose(ATTRIBUTE_NAMES)?);
                out.push_str(&i.to_string());
                out.push_str("={");
                ws(u, out)?;
                block_body(u, depth - 1, out)?;
                ws(u, out)?;
                out.push('}');
            }
        }
    }
    Ok(())
}

fn children(u: &mut Unstructured<'_>, depth: usize, out: &mut String) -> Result<()> {
    enum P {
        Text,
        Newline,
        Markup,
        Interpolation,
    }
    if depth == 0 {
        return Ok(());
    }
    for _ in 0..u.int_in_range(0..=3)? {
        match u.choose(&[P::Text, P::Newline, P::Markup, P::Interpolation])? {
            P::Text => out.push_str(u.choose(MARKUP_TEXT)?),
            P::Newline => out.push('\n'),
            P::Markup => markup(u, depth - 1, out)?,
            P::Interpolation => {
                out.push('{');
                ws(u, out)?;
                block_body(u, depth - 1, out)?;
                ws(u, out)?;
                out.push('}');
            }
        }
    }
    Ok(())
}
