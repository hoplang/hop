use super::r#type::Type;
use super::type_env::{TypeBinding, TypeEnv};
use crate::definition_link::DefinitionLink;
use crate::hop::parsing::ParsedType;
use crate::type_error::{TypeError, TypeErrorKind};

/// Resolve a parsed Type to a semantic Type.
pub fn resolve_type(
    parsed_type: &ParsedType,
    type_env: &mut TypeEnv,
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
            let elem_type = resolve_type(element, type_env, definition_links, errors)?;
            (Type::Option(Box::new(elem_type)), range)
        }
        ParsedType::Array { element, range } => {
            let elem_type = resolve_type(element, type_env, definition_links, errors)?;
            (Type::Array(Box::new(elem_type)), range)
        }
        ParsedType::Named { name, range } => {
            let Some((binding, def_range)) = type_env.lookup(name) else {
                errors.push(TypeError::new(
                    TypeErrorKind::UndefinedType {
                        type_name: name.clone(),
                    },
                    range.clone(),
                ));
                return None;
            };
            match binding {
                TypeBinding::Type(typ) => {
                    let typ = typ.clone();
                    definition_links.push(DefinitionLink {
                        use_range: range.clone(),
                        definition_range: def_range.clone(),
                    });
                    (typ, range)
                }
                TypeBinding::Component(_) | TypeBinding::Page => {
                    errors.push(TypeError::new(
                        TypeErrorKind::ComponentUsedAsType { name: name.clone() },
                        range.clone(),
                    ));
                    return None;
                }
            }
        }
    };
    Some(typ)
}
