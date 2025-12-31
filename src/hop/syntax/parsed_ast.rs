use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::ParsedExpr;
use crate::dop::ParsedType;
use crate::dop::VarName;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

use super::parsed_node::ParsedNode;

/// A ParsedParameter represents a parsed parameter with type annotation
/// and optional default value.
/// E.g. <my-comp {x: String, y: String = "default"}>
///                ^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^
#[derive(Debug, Clone)]
pub struct ParsedParameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
    pub default_value: Option<ParsedExpr>,
}

impl Display for ParsedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl ParsedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let base = BoxDoc::text(self.var_name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.var_type.to_doc());
        match &self.default_value {
            Some(default) => base.append(BoxDoc::text(" = ")).append(default.to_doc()),
            None => base,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedAttributeValue {
    Expressions(Vec<ParsedExpr>),
    String(DocumentRange),
}

impl ParsedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedAttributeValue::Expressions(exprs) => BoxDoc::text("{")
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            exprs.iter().map(|e| e.to_doc().append(BoxDoc::text(","))),
                            BoxDoc::line(),
                        ))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            ParsedAttributeValue::String(range) => BoxDoc::text(format!("\"{}\"", range.as_str())),
        }
    }
}

/// A ParsedAttribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[derive(Debug, Clone)]
pub struct ParsedAttribute {
    pub name: DocumentRange,
    pub value: Option<ParsedAttributeValue>,
}

impl ParsedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(self.name.as_str());
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text("=")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

/// A declaration in a Hop module - can be an import, record, enum, or component.
#[derive(Debug, Clone)]
pub enum ParsedDeclaration {
    Import(ParsedImportDeclaration),
    Record(ParsedRecordDeclaration),
    Enum(ParsedEnumDeclaration),
    Component(ParsedComponentDeclaration),
}

impl ParsedDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedDeclaration::Import(import) => import.to_doc(),
            ParsedDeclaration::Record(record) => record.to_doc(),
            ParsedDeclaration::Enum(e) => e.to_doc(),
            ParsedDeclaration::Component(component) => component.to_doc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedAst {
    pub name: ModuleName,
    declarations: Vec<ParsedDeclaration>,
}

impl ParsedAst {
    pub fn new(name: ModuleName, declarations: Vec<ParsedDeclaration>) -> Self {
        Self { name, declarations }
    }

    /// Returns a reference to all declarations in the AST, preserving their original order.
    pub fn get_declarations(&self) -> &[ParsedDeclaration] {
        &self.declarations
    }

    pub fn get_component_declaration(&self, name: &str) -> Option<&ParsedComponentDeclaration> {
        self.get_component_declarations()
            .find(|n| n.tag_name.as_str() == name)
    }

    /// Finds a record declaration by name.
    pub fn get_record_declaration(&self, name: &str) -> Option<&ParsedRecordDeclaration> {
        self.get_record_declarations().find(|r| r.name() == name)
    }

    /// Returns an iterator over all component declarations in the AST.
    pub fn get_component_declarations(&self) -> impl Iterator<Item = &ParsedComponentDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Component(c) => Some(c),
            _ => None,
        })
    }

    /// Returns an iterator over all import declarations in the AST.
    pub fn get_import_declarations(&self) -> impl Iterator<Item = &ParsedImportDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Import(i) => Some(i),
            _ => None,
        })
    }

    /// Returns an iterator over all record declarations in the AST.
    pub fn get_record_declarations(&self) -> impl Iterator<Item = &ParsedRecordDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Record(r) => Some(r),
            _ => None,
        })
    }

    /// Returns an iterator over all enum declarations in the AST.
    pub fn get_enum_declarations(&self) -> impl Iterator<Item = &ParsedEnumDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Enum(e) => Some(e),
            _ => None,
        })
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &ParsedNode> {
        self.get_component_declarations()
            .flat_map(|n| &n.children)
            .flat_map(|n| n.iter_depth_first())
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
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedImportDeclaration {
    pub type_name: TypeName,
    /// The range of the type name in the source (for error reporting)
    pub type_name_range: DocumentRange,
    /// The full path range for error reporting (covers module::TypeName)
    pub path: DocumentRange,
    pub module_name: ModuleName,
}

impl ParsedImportDeclaration {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_type_name(&self) -> &TypeName {
        &self.type_name
    }
    pub fn type_name_range(&self) -> &DocumentRange {
        &self.type_name_range
    }
    pub fn imports_type(&self, type_name: &str) -> bool {
        self.type_name.as_str() == type_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("import")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.module_name.to_string()))
            .append(BoxDoc::text("::"))
            .append(BoxDoc::text(self.type_name.as_str()))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedRecordDeclarationField {
    pub name: FieldName,
    pub field_type: ParsedType,
}

impl ParsedRecordDeclarationField {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.field_type.to_doc())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedRecordDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<ParsedRecordDeclarationField>,
}

impl ParsedRecordDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

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

#[derive(Debug, Clone)]
pub struct ParsedEnumDeclarationVariant {
    pub name: TypeName,
}

impl ParsedEnumDeclarationVariant {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.name.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedEnumDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub variants: Vec<ParsedEnumDeclarationVariant>,
}

impl ParsedEnumDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

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

#[derive(Debug, Clone)]
pub struct ParsedComponentDeclaration {
    pub component_name: ComponentName,
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<ParsedParameter>, DocumentRange)>,
    pub children: Vec<ParsedNode>,
    pub range: DocumentRange,
}

impl Ranged for ParsedComponentDeclaration {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl ParsedComponentDeclaration {
    pub fn tag_name_ranges(&self) -> impl Iterator<Item = &DocumentRange> {
        self.closing_tag_name.iter().chain(Some(&self.tag_name))
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("<")
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(match &self.params {
                Some((params, _)) if !params.is_empty() => BoxDoc::text(" {")
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                params.iter().map(|p| p.to_doc().append(BoxDoc::text(","))),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}")),
                _ => BoxDoc::nil(),
            })
            .append(BoxDoc::text(">"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|c| c.to_doc()),
                        BoxDoc::line(),
                    ))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("</"))
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(BoxDoc::text(">"))
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;
    use crate::hop::syntax::transform::whitespace_removal::remove_whitespace;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            source.to_string(),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        let ast = remove_whitespace(ast);
        expected.assert_eq(&ast.to_doc().pretty(60).to_string());
    }

    #[test]
    fn import_declaration_to_doc() {
        check(
            indoc! {"
                import foo::Bar
            "},
            expect![[r#"
                import foo::Bar
            "#]],
        );
    }

    #[test]
    fn multiple_import_declarations_to_doc() {
        check(
            indoc! {"
                import foo::Bar
                import baz::Qux
                import components::Button
                record User { name: String }
            "},
            expect![[r#"
                import foo::Bar
                import baz::Qux
                import components::Button

                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn record_declaration_single_field_to_doc() {
        check(
            indoc! {"
                record User { name: String }
            "},
            expect![[r#"
                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn enum_declaration_multiple_variants_to_doc() {
        check(
            indoc! {"
                enum Color { Red, Green, Blue }
            "},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
            "#]],
        );
    }

    #[test]
    fn two_record_declarations_to_doc() {
        check(
            indoc! {"
                record User { name: String, age: Int }
                record Post { title: String, author: User }
            "},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                record Post {
                  title: String,
                  author: User,
                }
            "#]],
        );
    }

    #[test]
    fn component_declaration_to_doc() {
        check(
            indoc! {"
                <Main {name: String, count: Int}>
                  <div>{name}</div>
                </Main>
            "},
            expect![[r#"
                <Main {
                  name: String,
                  count: Int,
                }>
                  <div>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_many_parameters_to_doc() {
        check(
            indoc! {"
                <Main {first_name: String, last_name: String, email: String, age: Int, active: Bool, role: String}></Main>
            "},
            expect![[r#"
                <Main {
                  first_name: String,
                  last_name: String,
                  email: String,
                  age: Int,
                  active: Bool,
                  role: String,
                }></Main>
            "#]],
        );
    }

    #[test]
    fn component_with_match_expression_to_doc() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                <Main {color: Color}>
                  <div class={match color { Color::Red => "red", Color::Green => "green", Color::Blue => "blue" }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {
                  color: Color,
                }>
                  <div class={
                    match color {
                      Color::Red   => "red",
                      Color::Green => "green",
                      Color::Blue  => "blue",
                    },
                  } />
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_text_child_to_doc() {
        check(
            indoc! {"
                <Main>hello</Main>
            "},
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_class_and_expression_to_doc() {
        check(
            indoc! {r#"
                record Character { name: String }
                <Main {character: Character}>
                  <h1 class="text-2xl font-bold">{character.name}</h1>
                </Main>
            "#},
            expect![[r#"
                record Character {
                  name: String,
                }

                <Main {
                  character: Character,
                }>
                  <h1 class="text-2xl font-bold">
                    {character.name}
                  </h1>
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_multiple_class_expressions_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <div class={"foo","bar","baz"}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class={
                    "foo",
                    "bar",
                    "baz",
                  } />
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_multiple_class_expressions_and_child_to_doc() {
        check(
            indoc! {r#"
                <Main {name: String}>
                  <div class={"foo","bar","baz"}>{name}</div>
                </Main>
            "#},
            expect![[r#"
                <Main {
                  name: String,
                }>
                  <div class={
                    "foo",
                    "bar",
                    "baz",
                  }>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_two_match_expressions_to_doc() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                enum Size { Small, Medium, Large }
                <Main {color: Color, size: Size}>
                  <div class={match color { Color::Red => "red", Color::Green => "green", Color::Blue => "blue" }, match size { Size::Small => "sm", Size::Medium => "md", Size::Large => "lg" }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                enum Size {
                  Small,
                  Medium,
                  Large,
                }

                <Main {
                  color: Color,
                  size: Size,
                }>
                  <div class={
                    match color {
                      Color::Red   => "red",
                      Color::Green => "green",
                      Color::Blue  => "blue",
                    },
                    match size {
                      Size::Small  => "sm",
                      Size::Medium => "md",
                      Size::Large  => "lg",
                    },
                  } />
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_equality_condition_to_doc() {
        check(
            indoc! {"
                <Main {a: String, b: String}>
                  <if {a == b}>
                    <div>equal</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {
                  a: String,
                  b: String,
                }>
                  <if {a == b}>
                    <div>
                      equal
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_logical_and_condition_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool, b: Bool}>
                  <if {a && b}>
                    <div>both true</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {
                  a: Bool,
                  b: Bool,
                }>
                  <if {a && b}>
                    <div>
                      both true
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_nested_logical_operators_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool, b: Bool, c: Bool}>
                  <if {a && b || c}>
                    <div>complex</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {
                  a: Bool,
                  b: Bool,
                  c: Bool,
                }>
                  <if {a && b || c}>
                    <div>
                      complex
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_negation_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool}>
                  <if {!a}>
                    <div>not a</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {
                  a: Bool,
                }>
                  <if {!a}>
                    <div>
                      not a
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_negated_equality_to_doc() {
        check(
            indoc! {"
                <Main {a: String, b: String}>
                  <if {!(a == b)}>
                    <div>not equal</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {
                  a: String,
                  b: String,
                }>
                  <if {!(a == b)}>
                    <div>
                      not equal
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_multiline_text() {
        check(
            indoc! {"
                <Main>
                  hello
                  world
                </Main>
            "},
            expect![[r#"
                <Main>
                  hello
                  world
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_nested_html() {
        check(
            indoc! {"
                <Main>
                  <div>
                    content
                  </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    content
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_empty_lines() {
        check(
            indoc! {"
                <Main>

                  hello

                </Main>
            "},
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_components_with_record_attributes_to_doc() {
        check(
            indoc! {r#"
                <IconsPage>
                  <div class="flex">
                    <div class="border-r max-w-80 h-screen">
                      <Sidebar />
                    </div>
                    <div class="flex gap-4 p-8">
                      <IconItem {id: "radix-icons", title: "Radix Icons", img_src: "/img/iphone.jpg", description: "A crisp set of 15×15 icons designed by the @workos team."} />
                    </div>
                  </div>
                </IconsPage>
            "#},
            expect![[r#"
                <IconsPage>
                  <div class="flex">
                    <div class="border-r max-w-80 h-screen">
                      <Sidebar/>
                    </div>
                    <div class="flex gap-4 p-8">
                      <IconItem {
                        id: "radix-icons",
                        title: "Radix Icons",
                        img_src: "/img/iphone.jpg",
                        description: "A crisp set of 15×15 icons designed by the @workos team.",
                      }/>
                    </div>
                  </div>
                </IconsPage>
            "#]],
        );
    }

    #[test]
    fn component_with_string_concatenation_attribute_to_doc() {
        check(
            indoc! {r#"
                record Product { id: String }
                <IconShowPage {product: Product}>
                  <Button {href: "/download/" + product.id}>
                    hello
                  </Button>
                </IconShowPage>
            "#},
            expect![[r#"
                record Product {
                  id: String,
                }

                <IconShowPage {
                  product: Product,
                }>
                  <Button {
                    href: "/download/" + product.id,
                  }>
                    hello
                  </Button>
                </IconShowPage>
            "#]],
        );
    }

    #[test]
    fn component_with_default_string_parameter_to_doc() {
        check(
            indoc! {r#"
                <Greeting {name: String = "World"}>
                  Hello, {name}!
                </Greeting>
            "#},
            expect![[r#"
                <Greeting {
                  name: String = "World",
                }>
                  Hello,
                  {name}
                  !
                </Greeting>
            "#]],
        );
    }

    #[test]
    fn component_with_default_int_parameter_to_doc() {
        check(
            indoc! {"
                <Counter {count: Int = 0}>
                  {count}
                </Counter>
            "},
            expect![[r#"
                <Counter {
                  count: Int = 0,
                }>
                  {count}
                </Counter>
            "#]],
        );
    }

    #[test]
    fn component_with_default_bool_parameter_to_doc() {
        check(
            indoc! {"
                <Toggle {enabled: Bool = true}>
                </Toggle>
            "},
            expect![[r#"
                <Toggle {
                  enabled: Bool = true,
                }></Toggle>
            "#]],
        );
    }

    #[test]
    fn component_with_mixed_required_and_default_parameters_to_doc() {
        check(
            indoc! {r#"
                <UserCard {name: String, role: String = "user", active: Bool = true}>
                  {name}
                </UserCard>
            "#},
            expect![[r#"
                <UserCard {
                  name: String,
                  role: String = "user",
                  active: Bool = true,
                }>
                  {name}
                </UserCard>
            "#]],
        );
    }

    #[test]
    fn component_with_default_array_parameter_to_doc() {
        check(
            indoc! {r#"
                <ItemList {items: Array[String] = ["one", "two"]}>
                </ItemList>
            "#},
            expect![[r#"
                <ItemList {
                  items: Array[String] = ["one", "two"],
                }></ItemList>
            "#]],
        );
    }

    #[test]
    fn component_with_default_empty_array_parameter_to_doc() {
        check(
            indoc! {"
                <ItemList {items: Array[String] = []}>
                </ItemList>
            "},
            expect![[r#"
                <ItemList {
                  items: Array[String] = [],
                }></ItemList>
            "#]],
        );
    }

    #[test]
    fn component_with_default_record_parameter_to_doc() {
        check(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                <Settings {config: Config = Config(debug: false, timeout: 30)}>
                </Settings>
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                <Settings {
                  config: Config = Config(debug: false, timeout: 30),
                }></Settings>
            "#]],
        );
    }

    #[test]
    fn component_with_default_enum_parameter_to_doc() {
        check(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                <Badge {status: Status = Status::Active}>
                </Badge>
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                <Badge {
                  status: Status = Status::Active,
                }></Badge>
            "#]],
        );
    }
}
