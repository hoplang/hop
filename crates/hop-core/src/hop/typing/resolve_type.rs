use std::collections::HashMap;

use super::r#type::{DataType, FunctionType, Modifier, Type};
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
        ParsedType::Html { .. } => return Some(Type::HTML),
        ParsedType::Function {
            modifier,
            parameters,
            result,
            ..
        } => {
            let parameters = parameters
                .iter()
                .map(|parameter| resolve_type(parameter, names, definition_links, errors))
                .collect::<Vec<_>>();
            let result_type = resolve_type(result, names, definition_links, errors)?;
            let parameters = parameters.into_iter().collect::<Option<Vec<_>>>()?;
            let function = FunctionType {
                modifier: Modifier::of(modifier.as_ref().map(|(m, _)| *m), &result_type),
                parameters,
                result: Box::new(result_type),
            };
            if let Some(error) = check_function_type(&function) {
                errors.push(TypeError::new(error, parsed_type.range().clone()));
                return None;
            }
            DataType::Function(function)
        }
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

pub fn check_function_type(function: &FunctionType) -> Option<TypeErrorKind> {
    if let Type::Html(_) | Type::Data(DataType::Function(_)) = &*function.result {
        let result = function.result.as_ref().clone();
        match function.modifier {
            Modifier::Mut => return Some(TypeErrorKind::MutFunctionReturnsNonData { result }),
            Modifier::Pure | Modifier::Render | Modifier::Test => {}
        }
    }
    None
}

pub fn resolve_data_type(
    parsed_type: &ParsedType,
    names: &HashMap<CheapString, Name>,
    definition_links: &mut Vec<DefinitionLink>,
    errors: &mut Vec<TypeError>,
) -> Option<DataType> {
    match resolve_type(parsed_type, names, definition_links, errors)? {
        Type::Data(data) => Some(data),
        Type::Html(_) => {
            errors.push(TypeError::new(
                TypeErrorKind::HtmlInsideType,
                parsed_type.range().clone(),
            ));
            None
        }
    }
}
