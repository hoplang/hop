use std::fmt::{self, Display};
use std::sync::Arc;

use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{Type, TypedExpr, VarName};
use crate::hop::semantics::typed_node::TypedNode;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct TypedRecordDeclaration {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Arc<Type>)>,
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
                        self.fields.iter().map(|(name, ty)| {
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

#[derive(Debug, Clone)]
pub struct TypedEnumDeclaration {
    pub name: TypeName,
    /// Variants with their fields: (variant_name, fields)
    pub variants: Vec<(TypeName, Vec<(FieldName, Arc<Type>)>)>,
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
                        self.variants.iter().map(|(name, fields)| {
                            if fields.is_empty() {
                                BoxDoc::text(name.as_str())
                            } else {
                                BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text("{"))
                                    .append(BoxDoc::intersperse(
                                        fields.iter().map(|(field_name, field_type)| {
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

#[derive(Debug, Clone)]
pub struct TypedComponentDeclaration {
    pub component_name: TypeName,
    pub children: Vec<TypedNode>,
    pub params: Vec<(VarName, Arc<Type>, Option<TypedExpr>)>,
}

#[derive(Debug, Clone)]
pub struct TypedEntrypointDeclaration {
    pub name: TypeName,
    pub children: Vec<TypedNode>,
    pub params: Vec<(VarName, Arc<Type>, Option<TypedExpr>)>,
}

impl TypedComponentDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let tag = BoxDoc::text("<").append(BoxDoc::text(self.component_name.as_str()));
        let tag_with_params = if self.params.is_empty() {
            tag
        } else {
            tag.append(BoxDoc::text(" {"))
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            self.params.iter().map(|(name, ty, default)| {
                                let base = BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(ty.to_doc());
                                match default {
                                    Some(expr) => {
                                        base.append(BoxDoc::text(" = ")).append(expr.to_doc())
                                    }
                                    None => base,
                                }
                            }),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .nest(2),
                )
                .append(BoxDoc::line_())
                .append(BoxDoc::text("}"))
                .group()
        };
        let opening = tag_with_params.append(BoxDoc::text(">"));
        if self.children.is_empty() {
            opening
                .append(BoxDoc::text("</"))
                .append(BoxDoc::text(self.component_name.as_str()))
                .append(BoxDoc::text(">"))
        } else {
            opening
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            self.children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("</"))
                .append(BoxDoc::text(self.component_name.as_str()))
                .append(BoxDoc::text(">"))
        }
    }
}

impl TypedEntrypointDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = if self.params.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(
                self.params.iter().map(|(name, ty, default)| {
                    let base = BoxDoc::text(name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(ty.to_doc());
                    match default {
                        Some(expr) => base.append(BoxDoc::text(" = ")).append(expr.to_doc()),
                        None => base,
                    }
                }),
                BoxDoc::text(", "),
            )
        };

        let header = BoxDoc::text("entrypoint")
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

#[derive(Debug, Clone)]
pub struct TypedAst {
    record_declarations: Vec<TypedRecordDeclaration>,
    enum_declarations: Vec<TypedEnumDeclaration>,
    component_declarations: Vec<TypedComponentDeclaration>,
    entrypoint_declarations: Vec<TypedEntrypointDeclaration>,
}

impl TypedAst {
    pub fn new(
        component_declarations: Vec<TypedComponentDeclaration>,
        record_declarations: Vec<TypedRecordDeclaration>,
        enum_declarations: Vec<TypedEnumDeclaration>,
        entrypoint_declarations: Vec<TypedEntrypointDeclaration>,
    ) -> Self {
        Self {
            component_declarations,
            record_declarations,
            enum_declarations,
            entrypoint_declarations,
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

    /// Returns a reference to all entrypoint declarations in the AST.
    pub fn get_entrypoint_declarations(&self) -> &[TypedEntrypointDeclaration] {
        &self.entrypoint_declarations
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

        for entrypoint in &self.entrypoint_declarations {
            docs.push(entrypoint.to_doc());
        }

        if docs.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(docs, BoxDoc::line().append(BoxDoc::line())).append(BoxDoc::line())
        }
    }
}

impl Display for TypedAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
