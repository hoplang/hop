use std::fmt::{self, Display};

use crate::examples_annotation::ExamplesAnnotation;
use crate::hop::typing::{Type, TypedExpr};
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct TypedAst {
    page_declarations: Vec<TypedPageDeclaration>,
    function_declarations: Vec<TypedFunctionDeclaration>,
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
    pub var_type: Type,
    pub examples: Option<ExamplesAnnotation>,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionDeclaration {
    pub name: FunctionName,
    pub params: Vec<TypedParameter>,
    pub return_type: Type,
    pub body: TypedExpr,
}

impl TypedAst {
    pub fn new(
        page_declarations: Vec<TypedPageDeclaration>,
        function_declarations: Vec<TypedFunctionDeclaration>,
    ) -> Self {
        Self {
            page_declarations,
            function_declarations,
        }
    }

    pub fn page_declarations(&self) -> &[TypedPageDeclaration] {
        &self.page_declarations
    }

    pub fn function_declarations(&self) -> &[TypedFunctionDeclaration] {
        &self.function_declarations
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        let mut docs: Vec<BoxDoc<'_>> = Vec::new();

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
        if !matches!(&self.head, TypedExpr::HtmlConcat { nodes } if nodes.is_empty()) {
            blocks.push(
                BoxDoc::text("fn head() -> Html {")
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
            BoxDoc::text("fn body() -> Html {")
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
