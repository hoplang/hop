use super::parsed_ast::ParsedAst;
use super::transform::sort_imports::sort_imports;
use super::transform::whitespace_removal::remove_whitespace;

pub fn format(ast: ParsedAst) -> String {
    let ast = remove_whitespace(ast);
    let ast = sort_imports(ast);
    ast.to_doc().pretty(60).to_string()
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            source.to_string(),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        expected.assert_eq(&super::format(ast));
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
                import baz::Qux
                import components::Button
                import foo::Bar

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
                <Main {name: String, count: Int}>
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

                <Main {color: Color}>
                  <div class={
                    match color {
                      Color::Red   => "red",
                      Color::Green => "green",
                      Color::Blue  => "blue",
                    },
                  }>
                  </div>
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

                <Main {character: Character}>
                  <h1 class="text-2xl font-bold">
                    {character.name}
                  </h1>
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_single_class_expression_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <div class={"p-2"}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class={"p-2"}>
                  </div>
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
                  <div class={"foo", "bar", "baz"}>
                  </div>
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
                <Main {name: String}>
                  <div class={"foo", "bar", "baz"}>
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

                <Main {color: Color, size: Size}>
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
                  }>
                  </div>
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
                <Main {a: String, b: String}>
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
                <Main {a: Bool, b: Bool}>
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
                <Main {a: Bool, b: Bool, c: Bool}>
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
                <Main {a: Bool}>
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
                <Main {a: String, b: String}>
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

                <IconShowPage {product: Product}>
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
                <Greeting {name: String = "World"}>
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
                <Counter {count: Int = 0}>
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
                <Toggle {enabled: Bool = true}></Toggle>
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
                <ItemList {items: Array[String] = []}></ItemList>
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

                <Badge {status: Status = Status::Active}></Badge>
            "#]],
        );
    }

    #[test]
    fn html_with_string_and_expression_attributes_to_doc() {
        check(
            indoc! {r#"
                record Product { img_src: String }
                <ProductImage {product: Product}>
                  <img class="rounded-lg" src={product.img_src}>
                </ProductImage>
            "#},
            expect![[r#"
                record Product {
                  img_src: String,
                }

                <ProductImage {product: Product}>
                  <img class="rounded-lg" src={product.img_src}>
                </ProductImage>
            "#]],
        );
    }

    #[test]
    fn component_with_match_node_to_doc() {
        check(
            indoc! {"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {
                  c: Option[String],
                }>
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                </Main>
            "},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {c: Option[String]}>
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn button_component_with_variant_and_size_enums_to_doc() {
        check(
            indoc! {r#"
                enum ButtonVariant {Default, Destructive, Outline, Secondary, Ghost, Link}

                enum ButtonSize {Default, Sm, Lg, Icon, IconSm, IconLg}

                <Button {variant: ButtonVariant = ButtonVariant::Default, size: ButtonSize = ButtonSize::Default, class: String = "", type: String = "button", aria_label: String = "", children: TrustedHTML}>
                  <button aria-label={aria_label} class={
                    "inline-flex",
                    "items-center",
                    "justify-center",
                    "gap-2",
                    "whitespace-nowrap",
                    "rounded-md",
                    "text-sm",
                    "font-medium",
                    "transition-all",
                    "disabled:pointer-events-none",
                    "disabled:opacity-50",
                    "[&_svg]:pointer-events-none",
                    "[&_svg:not([class*='size-'])]:size-4",
                    "shrink-0",
                    "[&_svg]:shrink-0",
                    "outline-none",
                    "focus-visible:border-ring",
                    "focus-visible:ring-ring/50",
                    "focus-visible:ring-[3px]",
                    "aria-invalid:ring-destructive/20",
                    "dark:aria-invalid:ring-destructive/40",
                    "aria-invalid:border-destructive",
                    match variant {
                      ButtonVariant::Default     => "bg-primary text-primary-foreground hover:bg-primary/90",
                      ButtonVariant::Destructive => "bg-destructive text-white hover:bg-destructive/90 focus-visible:ring-destructive/20 dark:focus-visible:ring-destructive/40 dark:bg-destructive/60",
                      ButtonVariant::Outline     => "border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50",
                      ButtonVariant::Secondary   => "bg-secondary text-secondary-foreground hover:bg-secondary/80",
                      ButtonVariant::Ghost       => "hover:bg-accent hover:text-accent-foreground dark:hover:bg-accent/50",
                      ButtonVariant::Link        => "text-primary underline-offset-4 hover:underline",
                    },
                    match size {
                      ButtonSize::Default => "h-9 px-4 py-2 has-[>svg]:px-3",
                      ButtonSize::Sm      => "h-8 rounded-md gap-1.5 px-3 has-[>svg]:px-2.5",
                      ButtonSize::Lg      => "h-10 rounded-md px-6 has-[>svg]:px-4",
                      ButtonSize::Icon    => "size-9",
                      ButtonSize::IconSm  => "size-8",
                      ButtonSize::IconLg  => "size-10",
                    },
                    class,
                  } data-slot="button" type={type}>
                    {children}
                  </button>
                </Button>
            "#},
            expect![[r#"
                enum ButtonVariant {
                  Default,
                  Destructive,
                  Outline,
                  Secondary,
                  Ghost,
                  Link,
                }

                enum ButtonSize {
                  Default,
                  Sm,
                  Lg,
                  Icon,
                  IconSm,
                  IconLg,
                }

                <Button {
                  variant: ButtonVariant = ButtonVariant::Default,
                  size: ButtonSize = ButtonSize::Default,
                  class: String = "",
                  type: String = "button",
                  aria_label: String = "",
                  children: TrustedHTML,
                }>
                  <button aria-label={aria_label} class={
                    "inline-flex",
                    "items-center",
                    "justify-center",
                    "gap-2",
                    "whitespace-nowrap",
                    "rounded-md",
                    "text-sm",
                    "font-medium",
                    "transition-all",
                    "disabled:pointer-events-none",
                    "disabled:opacity-50",
                    "[&_svg]:pointer-events-none",
                    "[&_svg:not([class*='size-'])]:size-4",
                    "shrink-0",
                    "[&_svg]:shrink-0",
                    "outline-none",
                    "focus-visible:border-ring",
                    "focus-visible:ring-ring/50",
                    "focus-visible:ring-[3px]",
                    "aria-invalid:ring-destructive/20",
                    "dark:aria-invalid:ring-destructive/40",
                    "aria-invalid:border-destructive",
                    match variant {
                      ButtonVariant::Default     => "bg-primary text-primary-foreground hover:bg-primary/90",
                      ButtonVariant::Destructive => "bg-destructive text-white hover:bg-destructive/90 focus-visible:ring-destructive/20 dark:focus-visible:ring-destructive/40 dark:bg-destructive/60",
                      ButtonVariant::Outline     => "border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50",
                      ButtonVariant::Secondary   => "bg-secondary text-secondary-foreground hover:bg-secondary/80",
                      ButtonVariant::Ghost       => "hover:bg-accent hover:text-accent-foreground dark:hover:bg-accent/50",
                      ButtonVariant::Link        => "text-primary underline-offset-4 hover:underline",
                    },
                    match size {
                      ButtonSize::Default => "h-9 px-4 py-2 has-[>svg]:px-3",
                      ButtonSize::Sm      => "h-8 rounded-md gap-1.5 px-3 has-[>svg]:px-2.5",
                      ButtonSize::Lg      => "h-10 rounded-md px-6 has-[>svg]:px-4",
                      ButtonSize::Icon    => "size-9",
                      ButtonSize::IconSm  => "size-8",
                      ButtonSize::IconLg  => "size-10",
                    },
                    class,
                  } data-slot="button" type={type}>
                    {children}
                  </button>
                </Button>
            "#]],
        );
    }
}
