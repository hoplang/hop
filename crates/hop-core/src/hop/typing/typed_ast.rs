use std::fmt::{self, Display};
use std::sync::Arc;

use crate::expr::typing::r#type::EnumVariant;
use crate::expr::{ExamplesAnnotation, Type};
use crate::hop::typing::typed_node::TypedNode;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct TypedAst {
    record_declarations: Vec<TypedRecordDeclaration>,
    enum_declarations: Vec<TypedEnumDeclaration>,
    component_declarations: Vec<TypedComponentDeclaration>,
    view_declarations: Vec<TypedViewDeclaration>,
}

#[derive(Debug, Clone)]
pub struct TypedRecordDeclaration {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Arc<Type>, Option<ExamplesAnnotation>)>,
}

#[derive(Debug, Clone)]
pub struct TypedEnumDeclaration {
    pub name: TypeName,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct TypedComponentDeclaration {
    pub component_name: TypeName,
    pub children: Vec<TypedNode>,
    pub params: Vec<TypedParameter>,
    pub rest_param: Option<VarName>,
    pub is_recursive: bool,
}

#[derive(Debug, Clone)]
pub struct TypedViewDeclaration {
    pub name: TypeName,
    pub children: Vec<TypedNode>,
    pub params: Vec<TypedParameter>,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub var_name: VarName,
    pub var_type: Arc<Type>,
    pub examples: Option<ExamplesAnnotation>,
}

impl TypedAst {
    pub fn new(
        component_declarations: Vec<TypedComponentDeclaration>,
        record_declarations: Vec<TypedRecordDeclaration>,
        enum_declarations: Vec<TypedEnumDeclaration>,
        view_declarations: Vec<TypedViewDeclaration>,
    ) -> Self {
        Self {
            component_declarations,
            record_declarations,
            enum_declarations,
            view_declarations,
        }
    }

    pub fn get_component_declaration(&self, name: &str) -> Option<&TypedComponentDeclaration> {
        self.component_declarations
            .iter()
            .find(|&n| n.component_name.as_str() == name)
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[TypedRecordDeclaration] {
        &self.record_declarations
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[TypedEnumDeclaration] {
        &self.enum_declarations
    }

    /// Returns a reference to all view declarations in the AST.
    pub fn get_view_declarations(&self) -> &[TypedViewDeclaration] {
        &self.view_declarations
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        let mut docs: Vec<BoxDoc<'_>> = Vec::new();

        for record in &self.record_declarations {
            docs.push(record.to_doc());
        }

        for enum_decl in &self.enum_declarations {
            docs.push(enum_decl.to_doc());
        }

        for component in &self.component_declarations {
            docs.push(component.to_doc());
        }

        for view in &self.view_declarations {
            docs.push(view.to_doc());
        }

        if docs.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(docs, BoxDoc::line().append(BoxDoc::line())).append(BoxDoc::line())
        }
    }
}

impl TypedRecordDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("record")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.fields.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.fields.iter().map(|(name, ty, _)| {
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(ty.to_doc())
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl TypedEnumDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("enum")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.variants.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.variants.iter().map(|variant| {
                            if variant.fields.is_empty() {
                                BoxDoc::text(variant.name.as_str())
                            } else {
                                BoxDoc::text(variant.name.as_str())
                                    .append(BoxDoc::text("{"))
                                    .append(BoxDoc::intersperse(
                                        variant.fields.iter().map(|(field_name, field_type, _)| {
                                            BoxDoc::text(field_name.as_str())
                                                .append(BoxDoc::text(": "))
                                                .append(field_type.to_doc())
                                        }),
                                        BoxDoc::text(", "),
                                    ))
                                    .append(BoxDoc::text("}"))
                            }
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl TypedComponentDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = {
            let mut parts: Vec<BoxDoc<'_>> = self
                .params
                .iter()
                .map(|param| {
                    BoxDoc::text(param.var_name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(param.var_type.to_doc())
                })
                .collect();
            if let Some(rest) = &self.rest_param {
                parts.push(BoxDoc::text("...").append(BoxDoc::text(rest.as_str())));
            }
            if parts.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::text("(")
                    .append(BoxDoc::intersperse(parts, BoxDoc::text(", ")))
                    .append(BoxDoc::text(")"))
            }
        };

        let header = BoxDoc::text("component")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(params_doc)
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"));

        if self.children.is_empty() {
            header.append(BoxDoc::text("}"))
        } else {
            header
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            self.children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
        }
    }
}

impl TypedViewDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = if self.params.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(
                self.params.iter().map(|param| {
                    BoxDoc::text(param.var_name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(param.var_type.to_doc())
                }),
                BoxDoc::text(", "),
            )
        };

        let header = BoxDoc::text("view")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"));

        if self.children.is_empty() {
            header.append(BoxDoc::text("}"))
        } else {
            header
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            self.children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
        }
    }
}

impl Display for TypedViewDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for TypedComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for TypedAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
