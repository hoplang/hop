use std::fmt::{self, Display};
use std::sync::Arc;

use crate::hop::typing::r#type::EnumVariant;
use crate::hop::typing::{ExamplesAnnotation, Type, TypedExpr};
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct TypedAst {
    record_declarations: Vec<TypedRecordDeclaration>,
    enum_declarations: Vec<TypedEnumDeclaration>,
    page_declarations: Vec<TypedPageDeclaration>,
    function_declarations: Vec<TypedFunctionDeclaration>,
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
pub struct TypedPageDeclaration {
    pub name: TypeName,
    pub head: TypedExpr,
    pub body: TypedExpr,
    pub params: Vec<TypedParameter>,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub var_name: VarName,
    pub var_type: Arc<Type>,
    pub examples: Option<ExamplesAnnotation>,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionDeclaration {
    pub name: FunctionName,
    pub params: Vec<TypedParameter>,
    pub return_type: Arc<Type>,
    pub body: TypedExpr,
}

impl TypedAst {
    pub fn new(
        record_declarations: Vec<TypedRecordDeclaration>,
        enum_declarations: Vec<TypedEnumDeclaration>,
        page_declarations: Vec<TypedPageDeclaration>,
        function_declarations: Vec<TypedFunctionDeclaration>,
    ) -> Self {
        Self {
            record_declarations,
            enum_declarations,
            page_declarations,
            function_declarations,
        }
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[TypedRecordDeclaration] {
        &self.record_declarations
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[TypedEnumDeclaration] {
        &self.enum_declarations
    }

    /// Returns a reference to all page declarations in the AST (this
    /// includes `view` declarations, which are sugar for `page`).
    pub fn get_page_declarations(&self) -> &[TypedPageDeclaration] {
        &self.page_declarations
    }

    pub fn get_function_declarations(&self) -> &[TypedFunctionDeclaration] {
        &self.function_declarations
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        let mut docs: Vec<BoxDoc<'_>> = Vec::new();

        for record in &self.record_declarations {
            docs.push(record.to_doc());
        }

        for enum_decl in &self.enum_declarations {
            docs.push(enum_decl.to_doc());
        }

        for page in &self.page_declarations {
            docs.push(page.to_doc());
        }

        for function in &self.function_declarations {
            docs.push(function.to_doc());
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

impl TypedPageDeclaration {
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

        let header = BoxDoc::text("page")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"));

        let mut blocks: Vec<BoxDoc<'_>> = Vec::new();
        if !matches!(&self.head, TypedExpr::FragmentConcat { nodes } if nodes.is_empty()) {
            blocks.push(
                BoxDoc::text("head {")
                    .append(
                        BoxDoc::line()
                            .append(self.head.to_doc())
                            .append(BoxDoc::line())
                            .nest(2),
                    )
                    .append(BoxDoc::text("}")),
            );
        }
        blocks.push(
            BoxDoc::text("body {")
                .append(
                    BoxDoc::line()
                        .append(self.body.to_doc())
                        .append(BoxDoc::line())
                        .nest(2),
                )
                .append(BoxDoc::text("}")),
        );

        header
            .append(
                BoxDoc::line()
                    .append(BoxDoc::intersperse(blocks, BoxDoc::line()))
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl TypedFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = BoxDoc::intersperse(
            self.params.iter().map(|param| {
                BoxDoc::text(param.var_name.as_str())
                    .append(BoxDoc::text(": "))
                    .append(param.var_type.to_doc())
            }),
            BoxDoc::text(", "),
        );

        BoxDoc::text("fn")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space())
            .append(BoxDoc::text("->"))
            .append(BoxDoc::space())
            .append(self.return_type.to_doc())
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl Display for TypedFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for TypedPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for TypedAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
