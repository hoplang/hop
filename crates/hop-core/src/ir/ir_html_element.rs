use indexmap::IndexMap;

use crate::document::CheapString;
use crate::html::HtmlElementKind;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;

#[derive(Debug, Clone)]
pub struct IrHtmlElement<Children> {
    pub element: HtmlElementKind,
    pub attributes: IndexMap<AttributeName, IrAttribute>,
    pub children: Children,
    pub on_click: Option<ValueId>,
}

/// The value of an attribute.
#[derive(Debug, Clone)]
pub enum IrAttribute {
    /// Must name a String.
    Value(ValueId),
    /// Must name a Bool.
    Presence(ValueId),
    /// Rendered as written, the value escaped, or the bare name when there
    /// is none.
    Constant(Option<CheapString>),
}

impl<Children> IrHtmlElement<Children> {
    /// Apply `f` to the name each attribute reads, in order.
    pub fn for_each_attribute_operand(&self, f: &mut impl FnMut(ValueId)) {
        for attribute in self.attributes.values() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(*name),
                IrAttribute::Constant(_) => {}
            }
        }
    }

    /// Apply `f` to the name each attribute reads, to rewrite it. Visits
    /// the same names as `for_each_attribute_operand`.
    pub fn for_each_attribute_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        for attribute in self.attributes.values_mut() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(name),
                IrAttribute::Constant(_) => {}
            }
        }
    }
}
