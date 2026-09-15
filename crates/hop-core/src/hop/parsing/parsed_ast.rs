use crate::document::DocumentRange;
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::ParsedType;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct ParsedAst {
    pub document_id: DocumentId,
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
    pub name: TypeName,
    pub name_range: DocumentRange,
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
    pub name: TypeName,
    pub name_range: DocumentRange,
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
    pub examples: Option<ExamplesAnnotation>,
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
    pub default_value: Option<ParsedExpr>,
    pub examples: Option<ExamplesAnnotation>,
    /// The range of the `#[examples(...)]` annotation, present exactly when
    /// `examples` is.
    pub examples_range: Option<DocumentRange>,
}

impl Display for ParsedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl ParsedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let prefix = if let Some(examples) = &self.examples {
            BoxDoc::text(examples.to_annotation_string()).append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        let base = prefix
            .append(BoxDoc::text(self.var_name.as_str()))
            .append(BoxDoc::text(": "))
            .append(self.var_type.to_doc());
        match &self.default_value {
            Some(default) => base.append(BoxDoc::text(" = ")).append(default.to_doc()),
            None => base,
        }
    }
}

impl ParsedDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedDeclaration::Import(import) => import.to_doc(),
            ParsedDeclaration::Record(record) => record.to_doc(),
            ParsedDeclaration::Enum(e) => e.to_doc(),
            ParsedDeclaration::Page(page) => page.to_doc(),
            ParsedDeclaration::Function(function) => function.to_doc(),
        }
    }
}

impl ParsedAst {
    pub fn new(
        document_id: DocumentId,
        declarations: Vec<ParsedDeclaration>,
        comments: Vec<DocumentRange>,
    ) -> Self {
        Self {
            document_id,
            declarations,
            comments,
        }
    }

    pub fn comments(&self) -> &[DocumentRange] {
        &self.comments
    }

    /// Returns a reference to all declarations in the AST, preserving their original order.
    pub fn declarations(&self) -> &[ParsedDeclaration] {
        &self.declarations
    }

    /// Finds a record declaration by name.
    pub fn find_record_declaration(&self, name: &str) -> Option<&ParsedRecordDeclaration> {
        self.record_declarations().find(|r| r.name() == name)
    }

    /// Returns an iterator over all import declarations in the AST.
    pub fn import_declarations(&self) -> impl Iterator<Item = &ParsedImportDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Import(i) => Some(i),
            _ => None,
        })
    }

    /// Returns an iterator over all record declarations in the AST.
    pub fn record_declarations(&self) -> impl Iterator<Item = &ParsedRecordDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Record(r) => Some(r),
            _ => None,
        })
    }

    /// Finds an enum declaration by name.
    pub fn find_enum_declaration(&self, name: &str) -> Option<&ParsedEnumDeclaration> {
        self.enum_declarations().find(|e| e.name() == name)
    }

    /// Returns an iterator over all enum declarations in the AST.
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

    pub fn to_doc(&self) -> BoxDoc<'_> {
        if self.declarations.is_empty() {
            BoxDoc::nil()
        } else {
            let mut doc = BoxDoc::nil();
            let mut prev_was_import = false;
            for (i, decl) in self.declarations.iter().enumerate() {
                if i > 0 {
                    doc = doc.append(BoxDoc::line());
                    let curr_is_import = matches!(decl, ParsedDeclaration::Import(_));
                    if !(prev_was_import && curr_is_import) {
                        doc = doc.append(BoxDoc::line());
                    }
                }
                doc = doc.append(decl.to_doc());
                prev_was_import = matches!(decl, ParsedDeclaration::Import(_));
            }
            doc.append(BoxDoc::line())
        }
    }
}

impl Display for ParsedAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(40))
    }
}

impl ParsedImportDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("import")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.module_name.to_string()))
            .append(BoxDoc::text("::"))
            .append(BoxDoc::text(self.name.as_str()))
    }
}

impl ParsedFieldDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let base = if let Some(examples) = &self.examples {
            BoxDoc::text(examples.to_annotation_string()).append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        base.append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text(": "))
            .append(self.field_type.to_doc())
    }
}

impl ParsedRecordDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        let pub_prefix = if self.pub_range.is_some() {
            BoxDoc::text("pub").append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        pub_prefix
            .append(BoxDoc::text("record"))
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.fields.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.fields.iter().map(|f| f.to_doc()),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl ParsedEnumDeclarationVariant {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        if self.fields.is_empty() {
            BoxDoc::text(self.name.as_str())
        } else {
            BoxDoc::text(self.name.as_str())
                .append(BoxDoc::text(" { "))
                .append(BoxDoc::intersperse(
                    self.fields.iter().map(|field| {
                        BoxDoc::text(field.name.to_string())
                            .append(BoxDoc::text(": "))
                            .append(field.field_type.to_doc())
                    }),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(" }"))
        }
    }
}

impl ParsedEnumDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        let pub_prefix = if self.pub_range.is_some() {
            BoxDoc::text("pub").append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        pub_prefix
            .append(BoxDoc::text("enum"))
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.variants.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.variants.iter().map(|v| v.to_doc()),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl ParsedFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let pub_prefix = if self.pub_range.is_some() {
            BoxDoc::text("pub").append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        let mut params: Vec<BoxDoc<'_>> = self.params.iter().map(|p| p.to_doc()).collect();
        if let Some((name, _)) = &self.rest_param {
            params.push(BoxDoc::text("...").append(BoxDoc::text(name.as_str())));
        }
        pub_prefix
            .append(BoxDoc::text("fn"))
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(BoxDoc::intersperse(params, BoxDoc::text(", ")))
            .append(BoxDoc::text(")"))
            .append(BoxDoc::text(" -> "))
            .append(self.return_type.to_doc())
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(
                BoxDoc::line()
                    .append(self.body.to_doc())
                    .nest(2)
                    .append(BoxDoc::line()),
            )
            .append(BoxDoc::text("}"))
    }
}

impl ParsedPageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let pub_prefix = if self.pub_range.is_some() {
            BoxDoc::text("pub").append(BoxDoc::space())
        } else {
            BoxDoc::nil()
        };
        let header = pub_prefix
            .append(BoxDoc::text("page"))
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(if self.params.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::intersperse(self.params.iter().map(|p| p.to_doc()), BoxDoc::text(", "))
            })
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space());

        let blocks = self.members().map(|member| member.to_doc());
        header
            .append(BoxDoc::text("{"))
            .append(
                BoxDoc::line()
                    .append(BoxDoc::intersperse(blocks, BoxDoc::line()))
                    .nest(2)
                    .append(BoxDoc::line()),
            )
            .append(BoxDoc::text("}"))
    }
}
