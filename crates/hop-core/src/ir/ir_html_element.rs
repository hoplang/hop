use indexmap::IndexMap;
use pretty::BoxDoc;

use crate::html::HtmlElementKind;
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;

/// An element with its attributes, by name in the order they render, and
/// its children.
#[derive(Debug, Clone)]
pub struct IrHtmlElement<Children> {
    pub element: HtmlElementKind,
    pub attributes: IndexMap<AttributeName, IrAttribute>,
    pub children: Children,
}

/// The value of an attribute.
#[derive(Debug, Clone)]
pub enum IrAttribute {
    /// Must name a String.
    Value(ValueId),
    /// Must name a Bool.
    Presence(ValueId),
}

impl<Children> IrHtmlElement<Children> {
    /// Apply `f` to the name each attribute reads, in order.
    pub fn for_each_attribute_operand(&self, f: &mut impl FnMut(ValueId)) {
        for attribute in self.attributes.values() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(*name),
            }
        }
    }

    /// Apply `f` to the name each attribute reads, to rewrite it. Visits
    /// the same names as `for_each_attribute_operand`.
    pub fn for_each_attribute_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        for attribute in self.attributes.values_mut() {
            match attribute {
                IrAttribute::Value(name) | IrAttribute::Presence(name) => f(name),
            }
        }
    }

    /// The attributes in braces, each as its name and the name it reads.
    pub fn attributes_to_doc(&self) -> BoxDoc<'_> {
        let attributes = self
            .attributes
            .iter()
            .map(|(name, attribute)| match attribute {
                IrAttribute::Value(value) => format!("{}: {value}", name.as_str()),
                IrAttribute::Presence(present) => format!("{}: {present}", name.as_str()),
            })
            .collect::<Vec<_>>()
            .join(", ");
        BoxDoc::text(format!("{{{attributes}}}"))
    }
}
