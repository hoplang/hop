use crate::display::{Indent, join, write_lines, write_variant};
use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::ParsedType;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::fmt::{self, Display, Write};

#[derive(Debug, Clone)]
pub struct ParsedModule {
    // We use a Vec of enum to store the declarations so that the declaration
    // order is consistent when formatting.
    declarations: Vec<ParsedDeclaration>,
    comments: Vec<DocumentRange>,
}

#[derive(Debug, Clone)]
pub enum ParsedDeclaration {
    Import(ParsedImportDeclaration),
    Record(ParsedRecordDeclaration),
    Enum(ParsedEnumDeclaration),
    Page(Box<ParsedPageDeclaration>),
    Function(Box<ParsedFunctionDeclaration>),
}

/// A function declaration.
///
/// ```text
/// fn add_five(x: Int) -> Int {
///   x + 5
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParsedFunctionDeclaration {
    pub name: FunctionName,
    pub name_range: DocumentRange,
    pub params: Vec<ParsedParameter>,
    pub rest_param: Option<(VarName, DocumentRange)>,
    pub return_type: ParsedType,
    pub body: ParsedExpr,
    pub range: DocumentRange,
    pub pub_range: Option<DocumentRange>,
}

/// A page declaration.
///
/// ```text
/// page Main {
///   fn head() -> Html {
///     <title>My page</title>
///   }
///   fn body() -> Html {
///     <div>
///       Welcome!
///     </div>
///   }
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParsedPageDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub params: Vec<ParsedParameter>,
    pub head: Option<ParsedFunctionDeclaration>,
    pub body: ParsedFunctionDeclaration,
    pub range: DocumentRange,
    pub pub_range: Option<DocumentRange>,
}

impl ParsedPageDeclaration {
    /// The page's members in source order.
    pub fn members(&self) -> impl Iterator<Item = &ParsedFunctionDeclaration> {
        let (first, second) = match &self.head {
            Some(head) if head.range.start() < self.body.range.start() => (head, Some(&self.body)),
            Some(head) => (&self.body, Some(head)),
            None => (&self.body, None),
        };
        std::iter::once(first).chain(second)
    }
}

/// An import declaration.
///
/// ```text
/// import foo::bar::Baz
/// import foo::bar::baz
/// ```
#[derive(Debug, Clone)]
pub struct ParsedImportDeclaration {
    /// The imported name as written.
    pub name: FunctionName,
    /// The range of the imported name in the source (for error reporting)
    pub name_range: DocumentRange,
    /// The full path range for error reporting (covers module::name)
    pub path_range: DocumentRange,
    /// The full range of the import declaration, from `import` keyword to end of name.
    pub import_range: DocumentRange,
    pub module_name: ModuleName,
}

/// A record declaration.
///
/// ```text
/// record User {
///   name: String,
///   email: String,
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParsedRecordDeclaration {
    pub type_name: TypeName,
    pub type_name_range: DocumentRange,
    pub range: DocumentRange,
    pub fields: Vec<ParsedFieldDeclaration>,
    pub pub_range: Option<DocumentRange>,
}

/// An enum declaration.
///
/// ```text
/// enum AuthState {
///   Authenticated { user: User },
///   Unauthenticated,
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParsedEnumDeclaration {
    pub type_name: TypeName,
    pub type_name_range: DocumentRange,
    pub range: DocumentRange,
    pub variants: Vec<ParsedEnumDeclarationVariant>,
    pub pub_range: Option<DocumentRange>,
}

/// A field in a record declaration or an enum variant declaration.
///
/// ```text
/// record User {
///   name: String,
///   ^^^^^^^^^^^^
/// }
///
/// enum AuthState {
///   Authenticated {
///     user: User,
///     ^^^^^^^^^^
///   },
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ParsedFieldDeclaration {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub field_type: ParsedType,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumDeclarationVariant {
    pub name: TypeName,
    pub name_range: DocumentRange,
    /// Optional fields for this variant (empty for unit variants)
    pub fields: Vec<ParsedFieldDeclaration>,
}

#[derive(Debug, Clone)]
pub struct ParsedParameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
    pub fallback_value: Option<ParsedExpr>,
}

impl ParsedModule {
    pub fn new(declarations: Vec<ParsedDeclaration>, comments: Vec<DocumentRange>) -> Self {
        Self {
            declarations,
            comments,
        }
    }

    pub fn comments(&self) -> &[DocumentRange] {
        &self.comments
    }

    /// Returns a reference to all declarations in the module, preserving their original order.
    pub fn declarations(&self) -> &[ParsedDeclaration] {
        &self.declarations
    }

    /// Finds a record declaration by name.
    pub fn find_record_declaration(&self, name: &str) -> Option<&ParsedRecordDeclaration> {
        self.record_declarations()
            .find(|r| r.type_name.as_str() == name)
    }

    /// Returns an iterator over all import declarations in the module.
    pub fn import_declarations(&self) -> impl Iterator<Item = &ParsedImportDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Import(i) => Some(i),
            _ => None,
        })
    }

    /// Returns an iterator over all record declarations in the module.
    pub fn record_declarations(&self) -> impl Iterator<Item = &ParsedRecordDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Record(r) => Some(r),
            _ => None,
        })
    }

    /// Finds an enum declaration by name.
    pub fn find_enum_declaration(&self, name: &str) -> Option<&ParsedEnumDeclaration> {
        self.enum_declarations()
            .find(|e| e.type_name.as_str() == name)
    }

    /// Returns an iterator over all enum declarations in the module.
    pub fn enum_declarations(&self) -> impl Iterator<Item = &ParsedEnumDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Enum(e) => Some(e),
            _ => None,
        })
    }

    pub fn page_declarations(&self) -> impl Iterator<Item = &ParsedPageDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Page(e) => Some(&**e),
            _ => None,
        })
    }

    pub fn function_declarations(&self) -> impl Iterator<Item = &ParsedFunctionDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Function(f) => Some(&**f),
            _ => None,
        })
    }
}

fn pub_prefix(pub_range: &Option<DocumentRange>) -> &'static str {
    if pub_range.is_some() { "pub " } else { "" }
}

impl Display for ParsedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.var_name.as_str();
        match &self.fallback_value {
            Some(fallback) => write!(f, "{name}?: {} = {fallback}", self.var_type),
            None => write!(f, "{name}: {}", self.var_type),
        }
    }
}

impl Display for ParsedDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedDeclaration::Import(import) => write!(f, "{import}"),
            ParsedDeclaration::Record(record) => write!(f, "{record}"),
            ParsedDeclaration::Enum(enumeration) => write!(f, "{enumeration}"),
            ParsedDeclaration::Page(page) => write!(f, "{page}"),
            ParsedDeclaration::Function(function) => write!(f, "{function}"),
        }
    }
}

impl Display for ParsedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sep = "";
        for declaration in &self.declarations {
            writeln!(f, "{sep}{declaration}")?;
            sep = "\n";
        }
        Ok(())
    }
}

impl Display for ParsedImportDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "import {}::{}", self.module_name, self.name.as_str())
    }
}

impl Display for ParsedFieldDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.as_str(), self.field_type)
    }
}

impl Display for ParsedRecordDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (prefix, name) = (pub_prefix(&self.pub_range), self.type_name.as_str());
        write_lines(
            f,
            format_args!("{prefix}record {name} {{"),
            &self.fields,
            "}",
        )
    }
}

impl Display for ParsedEnumDeclarationVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_variant(f, self.name.as_str(), &self.fields)
    }
}

impl Display for ParsedEnumDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (prefix, name) = (pub_prefix(&self.pub_range), self.type_name.as_str());
        write_lines(
            f,
            format_args!("{prefix}enum {name} {{"),
            &self.variants,
            "}",
        )
    }
}

impl Display for ParsedFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}fn {}({}",
            pub_prefix(&self.pub_range),
            self.name.as_str(),
            join(&self.params, ", ")
        )?;
        if let Some((name, _)) = &self.rest_param {
            let sep = if self.params.is_empty() { "" } else { ", " };
            write!(f, "{sep}...{}", name.as_str())?;
        }
        write!(f, ") -> {} {{", self.return_type)?;
        write!(Indent::new(f), "\n{}", self.body)?;
        f.write_str("\n}")
    }
}

impl Display for ParsedPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}page {}({}) {{",
            pub_prefix(&self.pub_range),
            self.name.as_str(),
            join(&self.params, ", ")
        )?;
        {
            let mut members = Indent::new(f);
            for member in self.members() {
                write!(members, "\n{member}")?;
            }
        }
        f.write_str("\n}")
    }
}
