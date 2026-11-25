use indexmap::IndexMap;

use crate::document::CheapString;
use crate::html::HtmlElementKind;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;

#[derive(Debug, Clone)]
pub struct FlatHtmlElement {
    pub element: HtmlElementKind,
    pub attributes: IndexMap<AttributeName, FlatAttribute>,
    pub children: Option<ValueId>,
    pub on_click: Option<ValueId>,
}

#[derive(Debug, Clone)]
pub enum FlatAttribute {
    /// Must name a String.
    Value(ValueId),
    /// Must name a Bool.
    Presence(ValueId),
    /// Rendered as written, the value escaped, or the bare name when there
    /// is none.
    Constant(Option<CheapString>),
}

impl FlatHtmlElement {
    pub fn for_each_attribute_operand(&self, f: &mut impl FnMut(ValueId)) {
        for attribute in self.attributes.values() {
            match attribute {
                FlatAttribute::Value(name) | FlatAttribute::Presence(name) => f(*name),
                FlatAttribute::Constant(_) => {}
            }
        }
    }

    pub fn for_each_attribute_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        for attribute in self.attributes.values_mut() {
            match attribute {
                FlatAttribute::Value(name) | FlatAttribute::Presence(name) => f(name),
                FlatAttribute::Constant(_) => {}
            }
        }
    }
}
