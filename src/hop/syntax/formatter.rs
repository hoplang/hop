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
                  <div
                    class={
                      match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                      }
                    }
                  >
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_empty_parens_omits_parens() {
        // Empty parens in enum patterns should be normalized away
        check(
            indoc! {r#"
                enum Color { Red }
                <Main {color: Color}>
                  <div class={match color { Color::Red() => "red" }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                }

                <Main {color: Color}>
                  <div class={match color {Color::Red => "red"}}>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn enum_declaration_with_empty_parens_omits_parens() {
        // Empty parens in enum declarations should be normalized away
        check(
            indoc! {r#"
                enum Foo { Bar() }
                <Main></Main>
            "#},
            expect![[r#"
                enum Foo {
                  Bar,
                }

                <Main></Main>
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
                      <IconItem id="radix-icons" title="Radix Icons" img_src="/img/iphone.jpg" description="A crisp set of 15x15 icons." />
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
                      <IconItem
                        id="radix-icons"
                        title="Radix Icons"
                        img_src="/img/iphone.jpg"
                        description="A crisp set of 15x15 icons."
                      />
                    </div>
                  </div>
                </IconsPage>
            "#]],
        );
    }

    #[test]
    fn component_with_expression_attribute_to_doc() {
        check(
            indoc! {r#"
                import hop::ui::lucide::ChevronDown

                <NativeSelect>
                  <ChevronDown
                    class={"text-muted-foreground"}
                  />
                </NativeSelect>
            "#},
            expect![[r#"
                import hop::ui::lucide::ChevronDown

                <NativeSelect>
                  <ChevronDown class={"text-muted-foreground"}/>
                </NativeSelect>
            "#]],
        );
    }

    #[test]
    fn component_with_string_concatenation_attribute_to_doc() {
        check(
            indoc! {r#"
                record Product { id: String }
                <IconShowPage {product: Product}>
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                </IconShowPage>
            "#},
            expect![[r#"
                record Product {
                  id: String,
                }

                <IconShowPage {product: Product}>
                  <Button href={"/download/" + product.id}>
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
    fn component_with_classes_macro_in_class_attribute_to_doc() {
        check(
            indoc! {r#"
                <Card {base_class: String, extra_class: String}>
                  <div class={classes!(base_class, extra_class)}></div>
                </Card>
            "#},
            expect![[r#"
                <Card {base_class: String, extra_class: String}>
                  <div class={classes!(base_class, extra_class)}>
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn component_with_classes_macro_multiple_classes_to_doc() {
        check(
            indoc! {r#"
                <Button {size_class: String, variant_class: String, custom_class: String}>
                  <button class={classes!(size_class, variant_class, custom_class)}>Click</button>
                </Button>
            "#},
            expect![[r#"
                <Button {
                  size_class: String,
                  variant_class: String,
                  custom_class: String,
                }>
                  <button
                    class={
                      classes!(size_class, variant_class, custom_class)
                    }
                  >
                    Click
                  </button>
                </Button>
            "#]],
        );
    }

    #[test]
    fn component_with_classes_macro_long_args_breaks_to_multiple_lines() {
        check(
            indoc! {r#"
                <Component {base_styles: String, responsive_styles: String, interactive_styles: String, custom_overrides: String}>
                  <div class={classes!(base_styles, responsive_styles, interactive_styles, custom_overrides)}></div>
                </Component>
            "#},
            expect![[r#"
                <Component {
                  base_styles: String,
                  responsive_styles: String,
                  interactive_styles: String,
                  custom_overrides: String,
                }>
                  <div
                    class={
                      classes!(
                        base_styles,
                        responsive_styles,
                        interactive_styles,
                        custom_overrides,
                      )
                    }
                  >
                  </div>
                </Component>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_spaces_in_string_literals() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("foo bar")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar")}>
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_multiple_spaces_in_string_literals() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("a b c")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("a", "b", "c")}>
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_mixed_variables_and_literals() {
        check(
            indoc! {r#"
                <Card {a: String, b: String, c: String}>
                  <div class={classes!(a, "foo bar", b, "baz qux", c)}></div>
                </Card>
            "#},
            expect![[r#"
                <Card {a: String, b: String, c: String}>
                  <div
                    class={classes!(a, "foo", "bar", b, "baz", "qux", c)}
                  >
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_collapses_multiple_spaces() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("foo    bar")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar")}>
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_trims_and_collapses_whitespace() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("   foo  bar   baz  ")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar", "baz")}>
                  </div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn should_format_deeply_nested_elements() {
        check(
            indoc! {r#"
                <Main>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  content
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <p>
                    <p>
                      <p>
                        <p>
                          <p>
                            <p>
                              <p>
                                <p>
                                  <p>
                                    <p>
                                      <p>
                                        <p>
                                          <p>
                                            <p>
                                              <p>
                                                <p>
                                                  <p>
                                                    <p>
                                                      <p>
                                                        <p>
                                                          <p>
                                                            <p>
                                                              <p>
                                                                <p>
                                                                  <p>
                                                                    <p>
                                                                      <p>
                                                                        <p>
                                                                          <p>
                                                                            <p>
                                                                              <p>
                                                                                <p>
                                                                                  <p>
                                                                                    <p>
                                                                                      <p>
                                                                                        <p>
                                                                                          <p>
                                                                                            <p>
                                                                                              <p>
                                                                                                <p>
                                                                                                  <p>
                                                                                                    <p>
                                                                                                      <p>
                                                                                                        <p>
                                                                                                          <p>
                                                                                                            <p>
                                                                                                              <p>
                                                                                                                <p>
                                                                                                                  <p>
                                                                                                                    <p>
                                                                                                                      content
                                                                                                                    </p>
                                                                                                                  </p>
                                                                                                                </p>
                                                                                                              </p>
                                                                                                            </p>
                                                                                                          </p>
                                                                                                        </p>
                                                                                                      </p>
                                                                                                    </p>
                                                                                                  </p>
                                                                                                </p>
                                                                                              </p>
                                                                                            </p>
                                                                                          </p>
                                                                                        </p>
                                                                                      </p>
                                                                                    </p>
                                                                                  </p>
                                                                                </p>
                                                                              </p>
                                                                            </p>
                                                                          </p>
                                                                        </p>
                                                                      </p>
                                                                    </p>
                                                                  </p>
                                                                </p>
                                                              </p>
                                                            </p>
                                                          </p>
                                                        </p>
                                                      </p>
                                                    </p>
                                                  </p>
                                                </p>
                                              </p>
                                            </p>
                                          </p>
                                        </p>
                                      </p>
                                    </p>
                                  </p>
                                </p>
                              </p>
                            </p>
                          </p>
                        </p>
                      </p>
                    </p>
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_single_string_binding_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {name: String = "World"}>
                    Hello, {name}!
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    Hello,
                    {name}
                    !
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_single_int_binding_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {count: Int = 42}>
                    <span>{count}</span>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {count: Int = 42}>
                    <span>
                      {count}
                    </span>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_trailing_comma_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {name: String = "World",}>
                    {name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    {name}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_multiple_bindings_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    {first} {second}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    {first}
                    {second}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_three_bindings_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>{a} + {b} + {c}</div>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>
                      {a}
                      +
                      {b}
                      +
                      {c}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_expression_value_to_doc() {
        check(
            indoc! {"
                <Main {x: Int, y: Int}>
                  <let {sum: Int = x + y}>
                    <span>{sum}</span>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main {x: Int, y: Int}>
                  <let {sum: Int = x + y}>
                    <span>
                      {sum}
                    </span>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_field_access_value_to_doc() {
        check(
            indoc! {"
                record User { name: String }
                <Main {user: User}>
                  <let {name: String = user.name}>
                    <div>{name}</div>
                  </let>
                </Main>
            "},
            expect![[r#"
                record User {
                  name: String,
                }

                <Main {user: User}>
                  <let {name: String = user.name}>
                    <div>
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_let_tags_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a} {b}
                    </let>
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a}
                      {b}
                    </let>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_inside_if_to_doc() {
        check(
            indoc! {r#"
                <Main {show: Bool}>
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                </Main>
            "#},
            expect![[r#"
                <Main {show: Bool}>
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_inside_for_to_doc() {
        check(
            indoc! {"
                <Main {items: Array[Int]}>
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>{doubled}</span>
                    </let>
                  </for>
                </Main>
            "},
            expect![[r#"
                <Main {items: Array[Int]}>
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>
                        {doubled}
                      </span>
                    </let>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn multiple_sibling_let_tags_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_empty_children_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {x: String = "unused"}></let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {x: String = "unused"}></let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_long_bindings_breaks_to_multiple_lines() {
        check(
            indoc! {r#"
                <Main>
                  <let {first_name: String = "Hello", last_name: String = "World"}>
                    {first_name} {last_name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {
                    first_name: String = "Hello",
                    last_name: String = "World",
                  }>
                    {first_name}
                    {last_name}
                  </let>
                </Main>
            "#]],
        );
    }
}
