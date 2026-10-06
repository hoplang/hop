use super::r#type::Type;
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::typing::TypedExpr;
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hover_annotation::HoverAnnotation;
use crate::root_relative_file_path::RootRelativeFilePath;
use crate::symbols::var_name::VarName;

pub fn typecheck_join(
    args: &[ParsedExpr],
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let string_type = Type::String;

    let mut typed_args = Vec::with_capacity(args.len());
    for arg in args {
        let Some(typed) = typecheck_expr(
            arg,
            Some(&string_type),
            forwarded_params,
            var_env,
            type_env,
            registry,
            annotations,
            definition_links,
            asset_references,
            errors,
        ) else {
            continue;
        };
        if typed.typ() != Type::String {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: TypeMismatchContext::MacroArgument,
                    expected: Type::String,
                    found: typed.typ(),
                },
                arg.range().clone(),
            ));
            continue;
        }
        typed_args.push(typed);
    }

    if typed_args.len() != args.len() {
        return None;
    }

    let separator = CheapString::new(" ".to_string());
    let mut parts = Vec::with_capacity((typed_args.len() * 2).saturating_sub(1));
    for (index, arg) in typed_args.into_iter().enumerate() {
        if index > 0 {
            parts.push(TypedExpr::StringLiteral {
                value: separator.clone(),
            });
        }
        parts.push(arg);
    }
    Some(TypedExpr::StringConcat { parts })
}

pub fn typecheck_format(
    args: &[ParsedExpr],
    range: &DocumentRange,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let Some((template, template_range)) = args.first().and_then(|arg| match arg {
        ParsedExpr::StringLiteral { value, range } => Some((value, range)),
        _ => None,
    }) else {
        errors.push(TypeError::new(
            TypeErrorKind::FormatMacroNonLiteralTemplate {},
            args.first().map_or(range, ParsedExpr::range).clone(),
        ));
        return None;
    };

    let template = template.cook(&mut |ch, range| {
        errors.push(TypeError::new(
            TypeErrorKind::InvalidEscapeSequence { ch },
            range,
        ));
    })?;

    let mut pieces = Vec::new();
    let mut piece = String::new();
    let mut chars = template.as_str().chars().peekable();
    while let Some(ch) = chars.next() {
        match (ch, chars.peek()) {
            ('{', Some('}')) => {
                chars.next();
                if !piece.is_empty() {
                    pieces.push(Some(CheapString::new(std::mem::take(&mut piece))));
                }
                pieces.push(None);
            }
            ('{', Some('{')) | ('}', Some('}')) => {
                chars.next();
                piece.push(ch);
            }
            ('{' | '}', _) => {
                errors.push(TypeError::new(
                    TypeErrorKind::FormatMacroInvalidPlaceholder {},
                    template_range.clone(),
                ));
                return None;
            }
            _ => piece.push(ch),
        }
    }
    if !piece.is_empty() {
        pieces.push(Some(CheapString::new(piece)));
    }
    let placeholders = pieces.iter().filter(|piece| piece.is_none()).count();

    let value_args = &args[1..];
    let mut typed_args = Vec::with_capacity(value_args.len());
    for arg in value_args {
        let Some(typed) = typecheck_expr(
            arg,
            None,
            forwarded_params,
            var_env,
            type_env,
            registry,
            annotations,
            definition_links,
            asset_references,
            errors,
        ) else {
            continue;
        };
        match typed.typ() {
            Type::String => typed_args.push(typed),
            Type::Int => typed_args.push(TypedExpr::IntToString {
                value: Box::new(typed),
            }),
            found => errors.push(TypeError::new(
                TypeErrorKind::FormatMacroUnsupportedArgument { found },
                arg.range().clone(),
            )),
        }
    }

    if value_args.len() != placeholders {
        errors.push(TypeError::new(
            TypeErrorKind::FormatMacroArity {
                expected: placeholders,
                found: value_args.len(),
            },
            range.clone(),
        ));
        return None;
    }

    if typed_args.len() != value_args.len() {
        return None;
    }

    let mut typed_args = typed_args.into_iter();
    let parts = pieces
        .into_iter()
        .map(|piece| match piece {
            Some(value) => TypedExpr::StringLiteral { value },
            None => typed_args.next().expect("one argument per placeholder"),
        })
        .collect();
    Some(TypedExpr::StringConcat { parts })
}

pub fn typecheck_asset(
    args: &[ParsedExpr],
    range: &DocumentRange,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    if args.len() != 1 {
        errors.push(TypeError::new(
            TypeErrorKind::AssetMacroArity { actual: args.len() },
            range.clone(),
        ));
        return None;
    }
    // Must be a string literal
    let (path, path_range) = match &args[0] {
        ParsedExpr::StringLiteral { value, range } => {
            let path = value.cook(&mut |ch, range| {
                errors.push(TypeError::new(
                    TypeErrorKind::InvalidEscapeSequence { ch },
                    range,
                ));
            })?;
            (path, range.clone())
        }
        other => {
            errors.push(TypeError::new(
                TypeErrorKind::AssetMacroNonLiteralArg {},
                other.range().clone(),
            ));
            return None;
        }
    };
    let asset_path = match RootRelativeFilePath::from_root_anchored(path.as_str()) {
        Ok(asset_path) => asset_path,
        Err(source) => {
            errors.push(TypeError::new(
                TypeErrorKind::InvalidAssetPath { source },
                path_range,
            ));
            return None;
        }
    };

    asset_references.push(AssetReference {
        range: range.clone(),
        path: asset_path.clone(),
    });

    Some(TypedExpr::Asset { path: asset_path })
}
