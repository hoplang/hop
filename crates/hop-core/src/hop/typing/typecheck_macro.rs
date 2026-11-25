use super::r#type::{DataType, FunctionType, Modifier, Type};
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::variable_scope::{Context, Position, VariableScope};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::typing::TypedExpr;
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::type_error::{TypeError, TypeErrorKind, TypeMismatchContext};
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
    let string_type = Type::STRING;

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
        if typed.typ() != Type::STRING {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: TypeMismatchContext::MacroArgument,
                    expected: Type::STRING,
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
            Type::STRING => typed_args.push(typed),
            Type::INT => typed_args.push(TypedExpr::IntToString {
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

/// Typecheck `state!(initial)`, which holds data and is allowed only in
/// what renders. Its type is a tuple of the type of the initial value `T`
/// and the type of the setter, `mut fn(fn(T) -> T) -> Tuple[]`.
pub fn typecheck_state(
    args: &[ParsedExpr],
    subject_range: &DocumentRange,
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
    let [initial] = args else {
        errors.push(TypeError::new(
            TypeErrorKind::StateMacroArity { actual: args.len() },
            range.clone(),
        ));
        return None;
    };
    var_env.require(
        |_| false,
        |position| TypeErrorKind::StateMacroNotAllowed { position },
        subject_range,
        errors,
    );
    let outer = var_env.set_context(Context::Outside(Position::InitialValue));
    let typed_initial = typecheck_expr(
        initial,
        None,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    var_env.set_context(outer);
    let typed_initial = typed_initial?;
    let data = match typed_initial.typ() {
        Type::Data(data) if registry.is_data(&data) => data,
        typ => {
            errors.push(TypeError::new(
                TypeErrorKind::StateMacroNotData { typ },
                initial.range().clone(),
            ));
            return None;
        }
    };
    let setter = DataType::Function(FunctionType {
        modifier: Modifier::Mut,
        parameters: vec![Type::Data(DataType::Function(FunctionType {
            modifier: Modifier::Pure,
            parameters: vec![Type::Data(data.clone())],
            result: Box::new(Type::Data(data.clone())),
        }))],
        result: Box::new(Type::UNIT),
    });
    Some(TypedExpr::State {
        initial: Box::new(typed_initial),
        typ: Type::Data(DataType::Tuple(vec![data, setter])),
    })
}

/// Typecheck `assert!(condition)`, which is allowed only in a test fn. Its
/// condition is a Bool, and its type is `Tuple[]`.
pub fn typecheck_assert(
    args: &[ParsedExpr],
    subject_range: &DocumentRange,
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
    if !var_env.tests() {
        errors.push(TypeError::new(
            TypeErrorKind::AssertMacroNotAllowed {
                name: CheapString::new("assert".to_string()),
            },
            subject_range.clone(),
        ));
    }
    let [condition] = args else {
        errors.push(TypeError::new(
            TypeErrorKind::AssertMacroArity { actual: args.len() },
            range.clone(),
        ));
        return None;
    };
    let typed_condition = typecheck_expr(
        condition,
        Some(&Type::BOOL),
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    )?;
    if typed_condition.typ() != Type::BOOL {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                context: TypeMismatchContext::MacroArgument,
                expected: Type::BOOL,
                found: typed_condition.typ(),
            },
            condition.range().clone(),
        ));
        return None;
    }
    Some(TypedExpr::Assert {
        condition: Box::new(typed_condition),
        range: range.clone(),
    })
}

/// Typecheck `assert_eq!(left, right)`, which is allowed only in a test fn.
/// Its operands have the same type, which is data, and its type is
/// `Tuple[]`.
pub fn typecheck_assert_eq(
    args: &[ParsedExpr],
    subject_range: &DocumentRange,
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
    if !var_env.tests() {
        errors.push(TypeError::new(
            TypeErrorKind::AssertMacroNotAllowed {
                name: CheapString::new("assert_eq".to_string()),
            },
            subject_range.clone(),
        ));
    }
    let [left, right] = args else {
        errors.push(TypeError::new(
            TypeErrorKind::AssertEqMacroArity { actual: args.len() },
            range.clone(),
        ));
        return None;
    };
    let typed_left = typecheck_expr(
        left,
        None,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    let left_type = typed_left.as_ref().map(TypedExpr::typ);
    let typed_right = typecheck_expr(
        right,
        left_type.as_ref(),
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    let (typed_left, typed_right, left_type) = (typed_left?, typed_right?, left_type?);
    if !matches!(&left_type, Type::Data(data) if registry.is_data(data)) {
        errors.push(TypeError::new(
            TypeErrorKind::AssertEqMacroNotComparable { typ: left_type },
            left.range().clone(),
        ));
        return None;
    }
    if typed_right.typ() != left_type {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                context: TypeMismatchContext::MacroArgument,
                expected: left_type,
                found: typed_right.typ(),
            },
            right.range().clone(),
        ));
        return None;
    }
    Some(TypedExpr::AssertEq {
        left: Box::new(typed_left),
        right: Box::new(typed_right),
        range: range.clone(),
    })
}
