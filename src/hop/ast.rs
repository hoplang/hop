use crate::common::Range;
use crate::dop::{DopExpr, DopType, parser::DopVarName};

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
    pub value_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HopParameter {
    pub var_name: DopVarName,
    pub type_annotation: DopType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HopArgument {
    pub name: String,
    pub name_range: Range,
    pub expression: DopExpr,
}

impl Attribute {
    pub fn new(name: String, value: String, range: Range, value_range: Range) -> Self {
        Attribute {
            name,
            value,
            range,
            value_range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopExprAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
}

impl DopExprAttribute {
    pub fn new(name: String, expression: DopExpr, range: Range) -> Self {
        DopExprAttribute {
            name,
            expression,
            range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoctypeNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentReferenceNode {
    pub component: String,
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub args: Vec<HopArgument>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfNode {
    pub condition: DopExpr,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportNode {
    pub component_attr: Attribute,
    pub from_attr: Attribute,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentDefinitionNode {
    pub name: String,
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub params: Vec<HopParameter>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
    pub preview: Option<Vec<HopNode>>,
    pub entrypoint: bool,
    pub slots: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeHTMLNode {
    pub tag_name: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
    pub set_attributes: Vec<DopExprAttribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorNode {
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotDefinitionNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotReferenceNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderNode {
    pub file_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XExecNode {
    pub cmd_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XRawNode {
    pub trim: bool,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub var_name: DopVarName,
    pub array_expr: DopExpr,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XLoadJsonNode {
    pub file_attr: Attribute,
    pub as_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextExpressionNode {
    pub expression: DopExpr,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HopNode {
    Doctype(DoctypeNode),
    Text(TextNode),
    TextExpression(TextExpressionNode),
    ComponentReference(ComponentReferenceNode),
    SlotDefinition(SlotDefinitionNode),
    SlotReference(SlotReferenceNode),
    If(IfNode),
    For(ForNode),
    NativeHTML(NativeHTMLNode),
    Error(ErrorNode),
    XExec(XExecNode),
    XRaw(XRawNode),
    XLoadJson(XLoadJsonNode),
}
