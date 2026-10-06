use pretty::BoxDoc;

use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// A constructor pattern (non-wildcard pattern that matches a specific value)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    /// A boolean true pattern
    BooleanTrue,
    /// A boolean false pattern
    BooleanFalse,
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
            Constructor::BooleanTrue => BoxDoc::text("true"),
            Constructor::BooleanFalse => BoxDoc::text("false"),
            Constructor::OptionSome => BoxDoc::text("Some"),
            Constructor::OptionNone => BoxDoc::text("None"),
            Constructor::Record { type_name } => BoxDoc::text(type_name.as_str().to_string()),
            Constructor::Tuple => BoxDoc::nil(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedMatchPattern {
    Wildcard,
    Binding {
        name: VarName,
    },
    Constructor {
        constructor: Constructor,
        args: Vec<TypedMatchPattern>,
        fields: Vec<TypedField>,
    },
}

impl TypedMatchPattern {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedMatchPattern::Constructor {
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
                            if let TypedMatchPattern::Binding { name: var_name } = &field.pattern {
                                if var_name.as_str() == field.name.as_str() {
                                    return BoxDoc::text(field.name.as_str());
                                }
                            }
                            BoxDoc::text(field.name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(field.pattern.to_doc())
                        }),
                        BoxDoc::text(", "),
                    );
                    base.append(BoxDoc::text("{"))
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
            TypedMatchPattern::Wildcard => BoxDoc::text("_"),
            TypedMatchPattern::Binding { name } => BoxDoc::text(name.as_str()),
        }
    }
}

impl std::fmt::Display for TypedMatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

#[derive(Debug, Clone)]
pub struct TypedField {
    pub name: FieldName,
    pub index: usize,
    pub pattern: TypedMatchPattern,
}
