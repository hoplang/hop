use std::fmt::{self, Display};

use super::inlined_node::InlinedNode;
use crate::hop::typing::typed_ast::TypedParameter;
use crate::symbols::type_name::TypeName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct InlinedViewDeclaration {
    pub name: TypeName,
    pub children: Vec<InlinedNode>,
    pub params: Vec<TypedParameter>,
}

#[derive(Debug, Clone)]
pub struct InlinedComponentDeclaration {
    pub component_name: TypeName,
    pub children: Vec<InlinedNode>,
    pub params: Vec<TypedParameter>,
}

impl InlinedComponentDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let tag = BoxDoc::text("<").append(BoxDoc::text(self.component_name.as_str()));
        let param_docs: Vec<BoxDoc> = self
            .params
            .iter()
            .map(|param| {
                let base = BoxDoc::text(param.var_name.as_str())
                    .append(BoxDoc::text(": "))
                    .append(param.var_type.to_doc());
                match &param.default_value {
                    Some(expr) => base.append(BoxDoc::text(" = ")).append(expr.to_doc()),
                    None => base,
                }
            })
            .collect();
        let tag_with_params = if param_docs.is_empty() {
            tag
        } else {
            tag.append(BoxDoc::text(" {"))
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            param_docs,
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

impl InlinedViewDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = if self.params.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(
                self.params.iter().map(|param| {
                    let base = BoxDoc::text(param.var_name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(param.var_type.to_doc());
                    match &param.default_value {
                        Some(expr) => base.append(BoxDoc::text(" = ")).append(expr.to_doc()),
                        None => base,
                    }
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

impl Display for InlinedViewDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for InlinedComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}
