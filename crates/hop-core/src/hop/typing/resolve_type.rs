use std::collections::HashMap;

use super::r#type::Type;
use super::type_env::{Name, NameKind};
use crate::definition_link::DefinitionLink;
use crate::document::CheapString;
use crate::hop::parsing::ParsedType;
use crate::type_error::{TypeError, TypeErrorKind};

/// Resolve a parsed Type to a semantic Type.
pub fn resolve_type(
    parsed_type: &ParsedType,
    names: &HashMap<CheapString, Name>,
    definition_links: &mut Vec<DefinitionLink>,
    errors: &mut Vec<TypeError>,
) -> Option<Type> {
    let (typ, _) = match parsed_type {
        ParsedType::String { range } => (Type::String, range),
        ParsedType::Bool { range } => (Type::Bool, range),
        ParsedType::Int { range } => (Type::Int, range),
        ParsedType::Float { range } => (Type::Float, range),
        ParsedType::Fragment { range } => (Type::Fragment, range),
        ParsedType::Option { element, range } => {
            let elem_type = resolve_type(element, names, definition_links, errors)?;
            (Type::Option(Box::new(elem_type)), range)
        }
        ParsedType::Array { element, range } => {
            let elem_type = resolve_type(element, names, definition_links, errors)?;
            (Type::Array(Box::new(elem_type)), range)
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
                (typ.clone(), range)
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
    Some(typ)
}
