use super::type_env::{FunctionSignature, ParamEntry, Tail, TypeEnv};
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typed_expr::TypedExpr;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::typing::type_error::{Target, TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hop::typing::{Type, TypedAttribute, TypedAttrs};
use crate::hop::uncooked_string::UncookedString;
use crate::hover_annotation::HoverAnnotation;
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

/// What a call supplies its arguments to.
pub enum Callee<'a> {
    /// A function, which takes its parameters, and through its rest the
    /// attributes of the element the rest lands on.
    Function(&'a FunctionName),
    /// An element, which is a callee with no parameters whose rest lands on
    /// the element itself, so it takes its attributes as a rest takes them.
    /// Its content is not an argument.
    Element {
        element: &'a HtmlElementKind,
        content: TypedExpr,
    },
}

/// The value supplied for a parameter or an attribute at a call site.
pub enum Argument<'a> {
    /// Written at the call site as an expression, still to be checked
    /// against the parameter.
    Expression(&'a ParsedExpr),
    /// An attribute's quoted text, cooked once its name is accepted.
    Text(&'a UncookedString, DocumentRange),
    /// An attribute written as its name alone, which is a compile error. It is
    /// kept until the parameter or attribute it names is known, so the error
    /// can show a value of the right type. The range is that of the name.
    Bare(DocumentRange),
}

/// An argument and the parameter it names.
pub struct NamedArgument<'a> {
    /// The name as written, which need not be a parameter of the callee.
    pub name: AttributeName,
    /// Where the argument was written, for the errors that concern its name.
    pub range: DocumentRange,
    pub argument: Argument<'a>,
}

/// The arguments of a call, as written.
pub enum CallArguments<'a> {
    Positional(&'a [ParsedExpr]),
    Named {
        arguments: Vec<NamedArgument<'a>>,
        /// The caller's own rest, spread into the call.
        spread: Option<VarName>,
    },
}

/// What an argument is checked against.
enum Receiver<'a> {
    Parameter(&'a ParamEntry),
    /// An attribute of the element the callee's rest lands on.
    Attribute {
        name: AttributeName,
        element: &'a HtmlElementKind,
    },
}

/// Check a call of `callee` and build the typed call, or the typed element
/// when the callee is an element.
///
/// A call expression, a markup call and an element all end up here, the
/// markup call with its attributes as named arguments and its content as
/// `children`, so the three forms are checked by the same code. A named
/// argument that names no parameter is an attribute for the callee's rest,
/// when the element the rest lands on accepts it.
///
/// Arguments are checked in the order they were supplied. Returns None once
/// anything about the call failed, after reporting it.
pub fn typecheck_call(
    callee: Callee<'_>,
    name_range: &DocumentRange,
    call_range: &DocumentRange,
    arguments: CallArguments<'_>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let element_signature;
    let (signature, target) = match &callee {
        Callee::Function(name) => {
            let Some(signature) = type_env.functions.get(name.as_str()) else {
                errors.push(TypeError::new(
                    TypeErrorKind::UndefinedFunction {
                        name: FunctionName::clone(name),
                    },
                    name_range.clone(),
                ));
                return None;
            };
            definition_links.push(DefinitionLink {
                use_range: name_range.clone(),
                definition_range: type_env.names[name.as_str()].definition_range.clone(),
            });
            (signature, Target::Function(FunctionName::clone(name)))
        }
        Callee::Element { element, .. } => {
            element_signature = FunctionSignature {
                params: Vec::new(),
                forwarded: Vec::new(),
                return_type: Type::Html,
                tail: Tail::Html {
                    element: HtmlElementKind::clone(element),
                    reserved: Vec::new(),
                },
                rest_param: None,
            };
            (
                &element_signature,
                Target::Element(HtmlElementKind::clone(element)),
            )
        }
    };
    // A name reaches the parameters the callee's rest carries as well as the
    // declared ones, while a position reaches only the declared ones.
    let params: Vec<&ParamEntry> = signature
        .params
        .iter()
        .chain(&signature.forwarded)
        .collect();

    let mut failed = false;
    let (supplied, rest_spread) = match arguments {
        CallArguments::Positional(values) => {
            let declared = &signature.params;
            let required = declared
                .iter()
                .rposition(|param| param.fallback.is_none())
                .map_or(0, |index| index + 1);
            // Too few arguments leave out parameters, which is reported below
            // as for a call by name, so `F()` fails like `<F/>`.
            if values.len() > declared.len() {
                let Callee::Function(name) = &callee else {
                    unreachable!("an element takes its attributes by name");
                };
                errors.push(TypeError::new(
                    TypeErrorKind::FunctionArgumentCountMismatch {
                        name: FunctionName::clone(name),
                        expected: if required == declared.len() {
                            required.to_string()
                        } else {
                            format!("{required} to {}", declared.len())
                        },
                        found: values.len(),
                    },
                    call_range.clone(),
                ));
                return None;
            }
            (
                declared
                    .iter()
                    .zip(values)
                    .map(|(param, value)| (Receiver::Parameter(param), Argument::Expression(value)))
                    .collect(),
                None,
            )
        }
        CallArguments::Named {
            arguments: named,
            spread,
        } => {
            let mut supplied: Vec<(Receiver<'_>, Argument<'_>)> = Vec::with_capacity(named.len());
            let mut written: Vec<AttributeName> = Vec::with_capacity(named.len());
            for NamedArgument {
                name,
                range,
                argument,
            } in named
            {
                // A name is written once, whether it names a parameter or an
                // attribute for the rest.
                if written.contains(&name) {
                    errors.push(TypeError::new(
                        TypeErrorKind::DuplicateName {
                            target: target.clone(),
                            name,
                        },
                        range,
                    ));
                    failed = true;
                    continue;
                }
                written.push(name.clone());
                // Names are compared ignoring case, here as for duplicates
                // and attributes, so `Title` passes the parameter `title`.
                let param = params
                    .iter()
                    .copied()
                    .find(|param| param.name.as_str().eq_ignore_ascii_case(name.as_str()));
                let receiver = match (param, &signature.tail) {
                    (Some(param), _) => Receiver::Parameter(param),
                    // An argument that names no parameter goes to the rest
                    // when the element the rest lands on accepts it, unless
                    // the site of the spread already writes it.
                    (None, Tail::Html { element, reserved })
                        if element.accepts_attribute(name.as_str())
                            && !reserved.contains(&name) =>
                    {
                        Receiver::Attribute { name, element }
                    }
                    (None, _) => {
                        errors.push(TypeError::new(
                            TypeErrorKind::DoesNotAccept {
                                target: target.clone(),
                                name,
                            },
                            range,
                        ));
                        failed = true;
                        continue;
                    }
                };
                supplied.push((receiver, argument));
            }
            (supplied, spread)
        }
    };

    // A parameter that is not supplied is read from the enclosing function
    // when the caller's rest carries it, since the enclosing signature carries
    // it for exactly this purpose. Otherwise it takes its fallback value, and
    // without one it is missing.
    let mut typed: Vec<(VarName, TypedExpr)> = Vec::with_capacity(params.len());
    let mut missing: Vec<&str> = Vec::new();
    for &param in &params {
        if supplied
            .iter()
            .any(|(receiver, _)| matches!(receiver, Receiver::Parameter(p) if p.name == param.name))
        {
            continue;
        }
        if rest_spread.is_some() && forwarded_params.contains(&param.name) {
            typed.push((
                param.name.clone(),
                TypedExpr::Var {
                    value: param.name.clone(),
                    typ: param.typ.clone(),
                },
            ));
        } else if param.fallback.is_none() {
            missing.push(param.name.as_str());
        }
    }
    if !missing.is_empty() {
        let Callee::Function(name) = &callee else {
            unreachable!("an element has no parameters to leave out");
        };
        errors.push(TypeError::new(
            TypeErrorKind::MissingArguments {
                name: FunctionName::clone(name),
                args: missing.join(", "),
            },
            call_range.clone(),
        ));
        failed = true;
    }

    let mut attributes: Vec<TypedAttribute> = Vec::new();
    for (receiver, argument) in supplied {
        // An attribute is a `Bool` when it is a boolean attribute of its
        // element and a `String` otherwise.
        let expected = match &receiver {
            Receiver::Parameter(param) => param.typ.clone(),
            Receiver::Attribute { name, element } => {
                if element.is_boolean_attribute(name.as_str()) {
                    Type::Bool
                } else {
                    Type::String
                }
            }
        };
        let (value, range) = match argument {
            Argument::Text(text, range) => (
                TypedExpr::StringLiteral {
                    value: text
                        .cook(&mut |ch, range| {
                            errors.push(TypeError::new(
                                TypeErrorKind::InvalidEscapeSequence { ch },
                                range,
                            ));
                        })
                        .unwrap_or_else(|| CheapString::new(String::new())),
                },
                range,
            ),
            // A name alone was supplied, so a parameter is not reported as
            // missing, but it has no value.
            Argument::Bare(range) => {
                errors.push(TypeError::new(
                    TypeErrorKind::AttributeWithoutValue {
                        attr: range.as_str().to_string(),
                        expected,
                    },
                    range,
                ));
                failed = true;
                continue;
            }
            Argument::Expression(expr) => {
                // These attributes load script or documents, or animate other
                // attributes. Escaping does not make a value safe there, so
                // their value must be written in the source.
                if let Receiver::Attribute { name, element } = &receiver {
                    let attr = name.as_str().to_ascii_lowercase();
                    let literal_only = match element {
                        HtmlElementKind::Script => attr == "src",
                        HtmlElementKind::Iframe => attr == "srcdoc",
                        HtmlElementKind::Animate | HtmlElementKind::Set => {
                            matches!(
                                attr.as_str(),
                                "attributename" | "by" | "from" | "to" | "values"
                            )
                        }
                        _ => false,
                    };
                    if literal_only && !matches!(expr, ParsedExpr::StringLiteral { .. }) {
                        errors.push(TypeError::new(
                            TypeErrorKind::AttributeRequiresStringLiteral {
                                element: element.as_str().to_string(),
                                attr: name.to_string(),
                            },
                            expr.range().clone(),
                        ));
                        failed = true;
                    }
                }
                let Some(value) = typecheck_expr(
                    expr,
                    Some(&expected),
                    forwarded_params,
                    var_env,
                    type_env,
                    registry,
                    annotations,
                    definition_links,
                    asset_references,
                    errors,
                ) else {
                    failed = true;
                    continue;
                };
                (value, expr.range().clone())
            }
        };
        let found = value.typ();
        if found != expected {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: match receiver {
                        Receiver::Parameter(_) => TypeMismatchContext::FunctionArgument,
                        Receiver::Attribute { .. } => TypeMismatchContext::Attribute,
                    },
                    expected,
                    found,
                },
                range,
            ));
            failed = true;
            continue;
        }
        match receiver {
            Receiver::Parameter(param) => typed.push((param.name.clone(), value)),
            Receiver::Attribute { name, .. } => attributes.push(TypedAttribute { name, value }),
        }
    }
    if failed {
        return None;
    }

    let attrs = TypedAttrs {
        attributes,
        spread: rest_spread,
    };
    match callee {
        Callee::Function(name) => {
            let args = params
                .iter()
                .filter_map(|param| {
                    let value = match typed.iter().position(|(name, _)| *name == param.name) {
                        Some(index) => typed.swap_remove(index).1,
                        None => param.fallback.clone()?,
                    };
                    Some((param.name.clone(), value))
                })
                .collect();

            let rest = match signature.rest_param.clone() {
                Some(rest_param) => Some((rest_param, attrs)),
                None => {
                    // A spread into a callee that declares no rest is not a
                    // mistake: the spread was carrying typed parameters, and
                    // those are passed explicitly above, so nothing is left
                    // for it to forward.
                    assert!(
                        attrs.attributes.is_empty(),
                        "{} declares no rest, but the call site supplies attributes for one",
                        name.as_str()
                    );
                    None
                }
            };

            Some(TypedExpr::Call {
                function_name: name.clone(),
                module: type_env.names[name.as_str()]
                    .definition_range
                    .document_id()
                    .clone(),
                args,
                rest,
                typ: signature.return_type.clone(),
            })
        }
        Callee::Element { element, content } => Some(TypedExpr::Element {
            element: element.clone(),
            attrs,
            children: Box::new(content),
        }),
    }
}
