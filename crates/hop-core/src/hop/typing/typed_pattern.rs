use pretty::BoxDoc;

use crate::hop::typing::r#type::Type;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// A pattern of a match arm. `B` is what a binding binds, a TypedBinder in
/// the typed AST and an IrBinder in the IR.
#[derive(Debug, Clone)]
pub enum Pattern<B> {
    Wildcard,
    /// Binds the value it matches.
    Binding(B),
    Constructor {
        constructor: Constructor,
        args: Vec<Pattern<B>>,
        fields: Vec<PatternField<B>>,
    },
}

#[derive(Debug, Clone)]
pub struct PatternField<B> {
    pub name: FieldName,
    /// The position of the field in its record or variant declaration.
    pub index: usize,
    pub pattern: Pattern<B>,
}

/// A variable a pattern of the typed AST binds, with the type of the values
/// it takes.
#[derive(Debug, Clone)]
pub struct TypedBinder {
    pub var: VarName,
    pub typ: Type,
}

pub type TypedPattern = Pattern<TypedBinder>;

pub type TypedField = PatternField<TypedBinder>;

/// A constructor pattern (non-wildcard pattern that matches a specific value)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    /// A boolean true pattern
    BoolTrue,
    /// A boolean false pattern
    BoolFalse,
    /// An Option Some pattern, e.g. `Some(_)`
    OptionSome,
    /// An Option None pattern, e.g. `None`
    OptionNone,
    /// An enum variant pattern, e.g. `Color::Red`
    EnumVariant {
        type_name: TypeName,
        variant_name: TypeName,
    },
    /// A record pattern, e.g. `User {name: x, age: y}`
    Record { type_name: TypeName },
    /// A tuple pattern, e.g. `(x, _)`
    Tuple,
}

impl Constructor {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Constructor::EnumVariant {
                type_name,
                variant_name,
            } => BoxDoc::text(type_name.as_str().to_string())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            Constructor::BoolTrue => BoxDoc::text("true"),
            Constructor::BoolFalse => BoxDoc::text("false"),
            Constructor::OptionSome => BoxDoc::text("Some"),
            Constructor::OptionNone => BoxDoc::text("None"),
            Constructor::Record { type_name } => BoxDoc::text(type_name.as_str().to_string()),
            Constructor::Tuple => BoxDoc::nil(),
        }
    }
}

impl TypedPattern {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedPattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let base = constructor.to_doc();
                if matches!(constructor, Constructor::Tuple) {
                    // Tuple pattern: (x, _)
                    BoxDoc::text("(")
                        .append(BoxDoc::intersperse(
                            args.iter().map(|a| a.to_doc()),
                            BoxDoc::text(", "),
                        ))
                        .append(if args.len() == 1 {
                            BoxDoc::text(",")
                        } else {
                            BoxDoc::nil()
                        })
                        .append(BoxDoc::text(")"))
                } else if !fields.is_empty() {
                    // Record pattern: User {name: x, age: y}
                    let fields_doc = BoxDoc::intersperse(
                        fields.iter().map(|field| {
                            if let TypedPattern::Binding(binder) = &field.pattern {
                                if binder.var.as_str() == field.name.as_str() {
                                    return BoxDoc::text(field.name.as_str());
                                }
                            }
                            BoxDoc::text(field.name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(field.pattern.to_doc())
                        }),
                        BoxDoc::text(", "),
                    );
                    base.append(BoxDoc::text(" {"))
                        .append(fields_doc)
                        .append(BoxDoc::text("}"))
                } else if args.is_empty() {
                    base
                } else {
                    // Positional args (Option Some, etc.)
                    let args_doc =
                        BoxDoc::intersperse(args.iter().map(|a| a.to_doc()), BoxDoc::text(", "));
                    base.append(BoxDoc::text("("))
                        .append(args_doc)
                        .append(BoxDoc::text(")"))
                }
            }
            TypedPattern::Wildcard => BoxDoc::text("_"),
            TypedPattern::Binding(binder) => BoxDoc::text(binder.var.as_str()),
        }
    }
}

impl std::fmt::Display for TypedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
