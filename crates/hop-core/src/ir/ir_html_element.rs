use indexmap::IndexMap;

use crate::document::CheapString;
use crate::html::HtmlElementKind;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;

#[derive(Debug, Clone)]
pub struct IrHtmlElement<Children> {
    pub identity: ValueId,
    pub element: HtmlElementKind,
    pub attributes: IndexMap<AttributeName, IrAttribute>,
    pub children: Children,
    pub on_click: Option<ValueId>,
}

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
    pub fn for_each_attribute_operand(&self, f: &mut impl FnMut(ValueId)) {
        for attribute in self.attributes.values() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(*name),
                IrAttribute::Constant(_) => {}
            }
        }
    }

    pub fn for_each_attribute_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        for attribute in self.attributes.values_mut() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(name),
                IrAttribute::Constant(_) => {}
            }
        }
    }
}
