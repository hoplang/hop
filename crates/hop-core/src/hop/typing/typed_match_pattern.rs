use pretty::BoxDoc;

use crate::hop::parsing::parsed_expr::Constructor;
use crate::symbols::field_name::FieldName;
use crate::symbols::var_name::VarName;

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
