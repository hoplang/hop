use std::collections::HashMap;

use super::r#type::{DataType, Type};
use super::type_env::{Name, NameKind};
use crate::definition_link::DefinitionLink;
use crate::document::CheapString;
use crate::hop::parsing::ParsedType;
use crate::hop::typing::type_error::{TypeError, TypeErrorKind};

/// Resolve a parsed Type to a semantic Type.
pub fn resolve_type(
    parsed_type: &ParsedType,
    names: &HashMap<CheapString, Name>,
    definition_links: &mut Vec<DefinitionLink>,
    errors: &mut Vec<TypeError>,
) -> Option<Type> {
    let typ = match parsed_type {
        ParsedType::String { .. } => DataType::String,
        ParsedType::Bool { .. } => DataType::Bool,
        ParsedType::Int { .. } => DataType::Int,
        ParsedType::Float { .. } => DataType::Float,
        ParsedType::Html { .. } => return Some(Type::Html),
        ParsedType::Option { element, .. } => {
            let elem_type = resolve_data_type(element, names, definition_links, errors)?;
            DataType::Option(Box::new(elem_type))
        }
        ParsedType::Array { element, .. } => {
            let elem_type = resolve_data_type(element, names, definition_links, errors)?;
            DataType::Array(Box::new(elem_type))
        }
        ParsedType::Tuple { elements, .. } => {
            let element_types = elements
                .iter()
                .map(|element| resolve_data_type(element, names, definition_links, errors))
                .collect::<Option<Vec<_>>>()?;
            DataType::Tuple(element_types)
        }
        ParsedType::Named { name, range } => match names.get(name.as_str()) {
            Some(Name {
                kind: NameKind::Type(typ),
                definition_range,
                ..
            }) => {
                definition_links.push(DefinitionLink {
                    use_range: range.clone(),
                    definition_range: definition_range.clone(),
                });
                typ.clone()
            }
            Some(Name {
                kind: NameKind::Function,
                ..
            }) => {
                errors.push(TypeError::new(
                    TypeErrorKind::FunctionUsedAsType { name: name.clone() },
                    range.clone(),
                ));
                return None;
            }
            Some(Name {
                kind: NameKind::Page,
                ..
            }) => {
                errors.push(TypeError::new(
                    TypeErrorKind::PageUsedAsType { name: name.clone() },
                    range.clone(),
                ));
                return None;
            }
            None => {
                errors.push(TypeError::new(
                    TypeErrorKind::UndefinedType {
                        type_name: name.clone(),
                    },
                    range.clone(),
                ));
                return None;
            }
        },
    };
    Some(Type::Data(typ))
}

pub fn resolve_data_type(
    parsed_type: &ParsedType,
    names: &HashMap<CheapString, Name>,
    definition_links: &mut Vec<DefinitionLink>,
    errors: &mut Vec<TypeError>,
) -> Option<DataType> {
    match resolve_type(parsed_type, names, definition_links, errors)? {
        Type::Data(data) => Some(data),
        Type::Html => {
            errors.push(TypeError::new(
                TypeErrorKind::HtmlInData,
                parsed_type.range().clone(),
            ));
            None
        }
    }
}
