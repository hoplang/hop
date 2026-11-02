use std::fmt::{self, Display, Write};

use crate::display::{Indent, join};
use crate::hop::typing::{Type, TypedExpr};
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, Clone)]
pub struct TypedModule {
    page_declarations: Vec<TypedPageDeclaration>,
    function_declarations: Vec<TypedFunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct TypedPageDeclaration {
    pub name: TypeName,
    pub head: Option<TypedExpr>,
    pub body: TypedExpr,
    pub params: Vec<TypedParameter>,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub var_name: VarName,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionDeclaration {
    pub name: FunctionName,
    pub params: Vec<TypedParameter>,
    pub rest_param: Option<VarName>,
    pub return_type: Type,
    pub body: TypedExpr,
}

impl TypedModule {
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
}

impl Display for TypedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name.as_str(), self.var_type)
    }
}

impl Display for TypedPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "page {}({}) {{",
            self.name.as_str(),
            join(&self.params, ", ")
        )?;
        {
            let mut blocks = Indent::new(f);
            if let Some(head) = &self.head {
                blocks.write_str("\nfn head() -> Html {")?;
                write!(Indent::new(&mut blocks), "\n{head}")?;
                blocks.write_str("\n}")?;
            }
            blocks.write_str("\nfn body() -> Html {")?;
            write!(Indent::new(&mut blocks), "\n{}", self.body)?;
            blocks.write_str("\n}")?;
        }
        f.write_str("\n}\n")
    }
}

impl Display for TypedFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}({}", self.name.as_str(), join(&self.params, ", "))?;
        if let Some(rest) = &self.rest_param {
            let sep = if self.params.is_empty() { "" } else { ", " };
            write!(f, "{sep}...{}", rest.as_str())?;
        }
        write!(f, ") -> {} {{", self.return_type)?;
        write!(Indent::new(f), "\n{}", self.body)?;
        f.write_str("\n}\n")
    }
}

/// The pages and then the functions, separated by blank lines.
impl Display for TypedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sep = "";
        for page in &self.page_declarations {
            write!(f, "{sep}{page}")?;
            sep = "\n";
        }
        for function in &self.function_declarations {
            write!(f, "{sep}{function}")?;
            sep = "\n";
        }
        Ok(())
    }
}
