use crate::document::CheapString;
use crate::hop::typing::Pattern;
use crate::ir::flat_html_element::FlatHtmlElement;
use crate::ir::flat_type::{FlatDataType, FlatType, IrModifier};
use crate::ir::flat_type_declarations::FlatTypeDeclarations;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::function_name::FunctionName;

#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub types: FlatTypeDeclarations,
    pub functions: Vec<FlatFunction>,
    pub entrypoints: Vec<FlatEntrypoint>,
    pub value_ids: ValueIdCounter,
}

#[derive(Debug, Clone)]
pub struct FlatEntrypoint {
    pub name: FunctionName,
    pub parameters: Vec<IrParameter<FlatDataType>>,
    pub body: FlatBlock,
}

#[derive(Debug, Clone)]
pub struct FlatFunction {
    pub function: IrFunction,
    pub modifier: IrModifier,
    pub parameters: Vec<IrParameter<FlatType>>,
    pub return_type: FlatType,
    pub body: FlatBlock,
}

pub type FlatBlock = IrBlock<FlatBinding>;
pub type FlatBinding = IrBinding<FlatType, FlatOp>;
pub type FlatLambda = IrLambda<FlatType, FlatBlock>;

#[derive(Debug, Clone)]
pub enum FlatOp {
    Data(IrDataOp),
    /// Invoke a function.
    Call(IrFunction, Vec<ValueId>),
    /// Match exhaustively.
    Match(FlatMatch),
    /// Bind a cell. Only when its path is new does the block run, and the
    /// cell takes the value it gives. `Cell[T]`.
    Cell(FlatBlock),
    /// Bind an identity, which the node of the element that takes it keeps
    /// across renders. `ElementId`.
    Mint,
    /// Write a cell. `(Cell[T], T) -> Tuple[]`.
    CellWrite(ValueId, ValueId),
    /// Read the value of a cell. `Cell[T] -> T`.
    CellRead(ValueId),
    /// Render text as written. `Html`.
    Text(CheapString),
    /// Html-escape a string. `String -> Html`.
    Escape(ValueId),
    /// Concatenate Html. `(Html, ..., Html) -> Html`.
    Concat(Vec<ValueId>),
    /// Bind a Html element, whose node its identity keeps across renders. A
    /// void element has no children, and every other element has them.
    /// `(ElementId, Html) -> Html`, or `ElementId -> Html` for a void element.
    Element(FlatHtmlElement),
    /// Run a fold-map over an array, where each iteration produces a Html value.
    /// The key, when there is one, gives each element the identity its
    /// cells and elements keep across renders. `Array[T] -> Html`.
    For(FlatFor),
    /// Bind a closure.
    Lambda(FlatLambda),
    /// Invoke a closure.
    Apply(ValueId, Vec<ValueId>),
}

#[derive(Debug, Clone)]
pub struct FlatFor {
    pub var: Option<IrBinder<FlatDataType>>,
    pub array: ValueId,
    pub key: Option<FlatBlock>,
    pub body: FlatBlock,
}

#[derive(Debug, Clone)]
pub struct FlatMatch {
    pub subject: ValueId,
    pub arms: Vec<FlatMatchArm>,
}

#[derive(Debug, Clone)]
pub struct FlatMatchArm {
    pub pattern: FlatPattern,
    pub body: FlatBlock,
}

pub type FlatPattern = Pattern<IrBinder<FlatDataType>>;

impl FlatPattern {
    pub fn for_each_binding(&self, f: &mut impl FnMut(&IrBinder<FlatDataType>)) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => f(binder),
            Pattern::Constructor { args, fields, .. } => {
                for arg in args {
                    arg.for_each_binding(f);
                }
                for field in fields {
                    field.pattern.for_each_binding(f);
                }
            }
        }
    }

    pub fn for_each_binding_mut(&mut self, f: &mut impl FnMut(&mut FlatPattern)) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Binding(_) => f(self),
            Pattern::Constructor { args, fields, .. } => {
                for arg in args {
                    arg.for_each_binding_mut(f);
                }
                for field in fields {
                    field.pattern.for_each_binding_mut(f);
                }
            }
        }
    }
}

impl FlatOp {
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatOp::Data(op) => op.for_each_operand(f),
            FlatOp::Text(_) | FlatOp::Lambda(_) | FlatOp::Cell(_) | FlatOp::Mint => {}
            FlatOp::CellRead(name) | FlatOp::Escape(name) => f(*name),
            FlatOp::CellWrite(first, second) => {
                f(*first);
                f(*second);
            }
            FlatOp::Call(_, names) | FlatOp::Concat(names) => {
                for name in names {
                    f(*name);
                }
            }
            FlatOp::Element(element) => {
                f(element.identity);
                element.for_each_attribute_operand(f);
                if let Some(children) = element.children {
                    f(children);
                }
                if let Some(handler) = element.on_click {
                    f(handler);
                }
            }
            FlatOp::Match(match_) => f(match_.subject),
            FlatOp::For(for_) => f(for_.array),
            FlatOp::Apply(closure, args) => {
                f(*closure);
                for arg in args {
                    f(*arg);
                }
            }
        }
    }

    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            FlatOp::Data(op) => op.for_each_operand_mut(f),
            FlatOp::Text(_) | FlatOp::Lambda(_) | FlatOp::Cell(_) | FlatOp::Mint => {}
            FlatOp::CellRead(name) | FlatOp::Escape(name) => f(name),
            FlatOp::CellWrite(first, second) => {
                f(first);
                f(second);
            }
            FlatOp::Call(_, names) | FlatOp::Concat(names) => {
                for name in names {
                    f(name);
                }
            }
            FlatOp::Element(element) => {
                f(&mut element.identity);
                element.for_each_attribute_operand_mut(f);
                if let Some(children) = &mut element.children {
                    f(children);
                }
                if let Some(handler) = &mut element.on_click {
                    f(handler);
                }
            }
            FlatOp::Match(match_) => f(&mut match_.subject),
            FlatOp::For(for_) => f(&mut for_.array),
            FlatOp::Apply(closure, args) => {
                f(closure);
                for arg in args {
                    f(arg);
                }
            }
        }
    }
}

#[cfg(test)]
mod test_support {
    use super::FlatFor;
    use super::FlatFunction;
    use super::FlatOp;
    use super::FlatPattern;
    use super::FlatProgram;
    use crate::display::{Indent, join, write_tuple};
    use crate::hop::typing::{Constructor, Pattern, PatternField};
    use crate::ir::flat_html_element::FlatAttribute;
    use crate::ir::flat_type::FlatDataType;
    use crate::ir::ir_binder::IrBinder;
    use std::fmt::{self, Write};

    impl fmt::Display for PatternField<IrBinder<FlatDataType>> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.pattern {
                Pattern::Binding(binder) => write!(f, "{}@{binder}", self.name.as_str()),
                pattern => write!(f, "{}: {pattern}", self.name.as_str()),
            }
        }
    }

    impl fmt::Display for FlatPattern {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Pattern::Wildcard => f.write_str("_"),
                Pattern::Binding(binder) => write!(f, "{binder}"),
                Pattern::Constructor {
                    constructor,
                    args,
                    fields,
                } => match constructor {
                    Constructor::Tuple => write_tuple(f, args),
                    Constructor::OptionSome => write!(f, "Some({})", join(args, ", ")),
                    Constructor::Record { .. } => {
                        write!(f, "{constructor} {{{}}}", join(fields, ", "))
                    }
                    Constructor::EnumVariant { .. } if !fields.is_empty() => {
                        write!(f, "{constructor} {{{}}}", join(fields, ", "))
                    }
                    _ => write!(f, "{constructor}"),
                },
            }
        }
    }

    impl fmt::Display for FlatOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                FlatOp::Data(op) => write!(f, "{op}"),
                FlatOp::Call(function, args) => write!(f, "f{}({})", function.id, join(args, ", ")),
                FlatOp::Match(match_) => {
                    write!(f, "match {} {{", match_.subject)?;
                    {
                        let mut arms = Indent::new(f);
                        for arm in &match_.arms {
                            write!(arms, "\n{} => {{", arm.pattern)?;
                            write!(Indent::new(&mut arms), "\n{}", arm.body)?;
                            arms.write_str("\n}")?;
                        }
                    }
                    f.write_str("\n}")
                }
                FlatOp::Cell(initial) => {
                    f.write_str("cell {")?;
                    write!(Indent::new(f), "\n{initial}")?;
                    f.write_str("\n}")
                }
                FlatOp::Mint => f.write_str("mint()"),
                FlatOp::CellWrite(cell, value) => write!(f, "cell_write({cell}, {value})"),
                FlatOp::CellRead(cell) => write!(f, "cell_read({cell})"),
                FlatOp::Text(content) => write!(f, "text({:?})", content.as_str()),
                FlatOp::Concat(parts) => write!(f, "concat({})", join(parts, ", ")),
                FlatOp::Escape(string) => write!(f, "escape({string})"),
                FlatOp::Element(element) => {
                    write!(
                        f,
                        "element({}, {:?}, {{",
                        element.identity,
                        element.element.as_str()
                    )?;
                    for (i, (name, attribute)) in element.attributes.iter().enumerate() {
                        let sep = if i > 0 { ", " } else { "" };
                        write!(f, "{sep}{}", name.as_str())?;
                        match attribute {
                            FlatAttribute::Value(value) | FlatAttribute::Presence(value) => {
                                write!(f, ": {value}")?;
                            }
                            FlatAttribute::Constant(Some(value)) => {
                                write!(f, ": {:?}", value.as_str())?;
                            }
                            FlatAttribute::Constant(None) => {}
                        }
                    }
                    match element.children {
                        Some(children) => write!(f, "}}, {children})")?,
                        None => write!(f, "}})")?,
                    }
                    if let Some(handler) = element.on_click {
                        write!(f, " on_click {handler}")?;
                    }
                    Ok(())
                }
                FlatOp::For(FlatFor {
                    var,
                    array,
                    key,
                    body,
                }) => {
                    match var {
                        Some(binder) => write!(f, "for {binder} in {array}")?,
                        None => write!(f, "for _ in {array}")?,
                    }
                    if let Some(key) = key {
                        f.write_str(" key {")?;
                        write!(Indent::new(f), "\n{key}")?;
                        f.write_str("\n}")?;
                    }
                    f.write_str(" {")?;
                    write!(Indent::new(f), "\n{body}")?;
                    f.write_str("\n}")
                }
                FlatOp::Lambda(lambda) => lambda.write(f, |block, w| write!(w, "\n{block}")),
                FlatOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
            }
        }
    }

    impl fmt::Display for FlatFunction {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}fn {}(", self.modifier, self.function)?;
            for parameter in &self.parameters {
                write!(Indent::new(f), "\n{parameter},")?;
            }
            if !self.parameters.is_empty() {
                f.write_str("\n")?;
            }
            write!(f, ") -> {} {{", self.return_type)?;
            write!(Indent::new(f), "\n{}", self.body)?;
            writeln!(f, "\n}}")
        }
    }

    impl fmt::Display for FlatProgram {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for function in &self.functions {
                write!(f, "{function}")?;
            }
            for (index, entrypoint) in self.entrypoints.iter().enumerate() {
                write!(f, "entrypoint {}@e{}(", entrypoint.name, index)?;
                for parameter in &entrypoint.parameters {
                    write!(Indent::new(f), "\n{parameter},")?;
                }
                if !entrypoint.parameters.is_empty() {
                    f.write_str("\n")?;
                }
                f.write_str(") -> Html {")?;
                write!(Indent::new(f), "\n{}", entrypoint.body)?;
                writeln!(f, "\n}}")?;
            }
            Ok(())
        }
    }
}
