use super::Program;
use super::find_node::find_node_at_position;
use crate::document::{DocumentPosition, DocumentRange};
use crate::hop::parsing::ParsedNode;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::type_name::TypeName;

impl Program {
    pub fn rename_locations(&self, position: &DocumentPosition) -> Option<Vec<DocumentRange>> {
        let document_id = position.document_id();
        let ast = self.parsed_asts.get(document_id)?;

        // Check if cursor is on a record declaration name
        for record in ast.record_declarations() {
            if record.type_name_range.contains_position(position) {
                return Some(self.collect_record_rename_locations(&record.type_name, document_id));
            }
        }

        // Check if cursor is on an enum declaration name
        for enum_decl in ast.enum_declarations() {
            if enum_decl.type_name_range.contains_position(position) {
                return Some(self.collect_enum_rename_locations(&enum_decl.type_name, document_id));
            }
        }

        for function in ast.function_declarations() {
            if function.name_range.contains_position(position) {
                return Some(self.collect_function_rename_locations(&function.name_range));
            }
        }

        let node = find_node_at_position(ast, position)?;

        let is_on_tag_name = node.tag_names().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            ParsedNode::FunctionInvocation { .. } => {
                let link = self
                    .definition_links
                    .get(document_id)?
                    .iter()
                    .find(|link| link.use_range.contains_position(position))?;
                Some(self.collect_function_rename_locations(&link.definition_range))
            }
            n @ ParsedNode::HtmlElement { .. } => Some(n.tag_names().cloned().collect()),
            _ => None,
        }
    }

    /// Returns the range and current name of the renameable symbol at the
    /// given position: a function, record or enum name, at its declaration
    /// or at a use.
    pub fn renameable_symbol(
        &self,
        position: &DocumentPosition,
    ) -> Option<(DocumentRange, String)> {
        let ast = self.parsed_asts.get(position.document_id())?;

        let mut declaration_names = ast
            .record_declarations()
            .map(|record| &record.type_name_range)
            .chain(ast.enum_declarations().map(|e| &e.type_name_range))
            .chain(ast.function_declarations().map(|f| &f.name_range));

        let range = match declaration_names.find(|r| r.contains_position(position)) {
            Some(range) => range,
            None => find_node_at_position(ast, position)?
                .tag_names()
                .find(|r| r.contains_position(position))?,
        };

        Some((range.clone(), range.as_str().to_string()))
    }

    /// Collects all locations where a function should be renamed, including:
    /// - The function definition
    /// - All calls and tag invocations of the function (opening and closing
    ///   tags)
    /// - All import statements that import the function
    fn collect_function_rename_locations(
        &self,
        definition_range: &DocumentRange,
    ) -> Vec<DocumentRange> {
        // Collect all use_ranges across all modules whose definition_range
        // matches the function's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| link.use_range.clone())
            .collect()
    }

    /// Collects all locations where a record type should be renamed, including:
    /// - The record declaration
    /// - All type annotations that reference the record
    /// - All import statements that import the record
    fn collect_record_rename_locations(
        &self,
        record_name: &TypeName,
        definition_module: &RootContainedFilePath,
    ) -> Vec<DocumentRange> {
        // Find the definition range (the name_range of the record declaration)
        let definition_range = self
            .parsed_asts
            .get(definition_module)
            .and_then(|module| module.find_record_declaration(record_name.as_str()))
            .map(|decl| &decl.type_name_range);

        let Some(definition_range) = definition_range else {
            return Vec::new();
        };

        // Collect all use_ranges across all modules whose definition_range
        // matches the record's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| link.use_range.clone())
            .collect()
    }

    /// Collects all locations where an enum type should be renamed, including:
    /// - The enum declaration
    /// - All type annotations that reference the enum
    /// - All import statements that import the enum
    fn collect_enum_rename_locations(
        &self,
        enum_name: &TypeName,
        definition_module: &RootContainedFilePath,
    ) -> Vec<DocumentRange> {
        // Find the definition range (the name_range of the enum declaration)
        let definition_range = self
            .parsed_asts
            .get(definition_module)
            .and_then(|module| module.find_enum_declaration(enum_name.as_str()))
            .map(|decl| &decl.type_name_range);

        let Some(definition_range) = definition_range else {
            return Vec::new();
        };

        // Collect all use_ranges across all modules whose definition_range
        // matches the enum's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| link.use_range.clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::{extract_markers_from_archive, program_from_archive};
    use crate::diagnostic::Diagnostic;
    use crate::diagnostic_severity::DiagnosticSeverity;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn check_rename_locations(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let locs = program_from_archive(&archive)
            .rename_locations(&markers[0])
            .expect("Expected locations to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(locs.into_iter().map(|range| Diagnostic {
                message: "Rename".to_string(),
                range,
                severity: DiagnosticSeverity::Error,
            }))
            .render();

        expected.assert_eq(&output);
    }

    fn check_renameable_symbol(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let (range, name) = program_from_archive(&archive)
            .renameable_symbol(&markers[0])
            .expect("Expected symbol to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate([Diagnostic {
                message: name,
                range,
                severity: DiagnosticSeverity::Error,
            }])
            .render();

        expected.assert_eq(&output);
    }

    #[test]
    fn should_find_rename_locations_from_function_invocation() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                pub fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                fn Main() -> Html {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 8)
                1 | pub fn HelloWorld() -> Html {
                  |        ^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | import components::HelloWorld
                  |                    ^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_function_invocation_in_same_module() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                fn Main() -> Html {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 4)
                1 | fn HelloWorld() -> Html {
                  |    ^^^^^^^^^^

                Rename
                  --> main.hop (line 6, col 4)
                6 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_component_definition() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                pub fn HelloWorld() -> Html {
                        ^
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                fn Main() -> Html {
                  <HelloWorld />
                }
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 8)
                1 | pub fn HelloWorld() -> Html {
                  |        ^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | import components::HelloWorld
                  |                    ^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    // Make sure that when we rename a function in a module that has
    // the same name as a module in some other function, the module in
    // the other function is left unchanged.
    #[test]
    fn should_scope_rename_locations_to_component_definition_module() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                fn Main() -> Html {
                   ^
                  <HelloWorld />
                }

                -- main.hop --
                import components::HelloWorld

                fn Main() -> Html {
                  <HelloWorld />
                }
            "#},
            // The result here should not contain rename locations in main.hop.
            expect![[r#"
                Rename
                  --> components.hop (line 5, col 4)
                5 | fn Main() -> Html {
                  |    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_html_opening_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                    <div>
                     ^
                        <span>Content</span>
                    </div>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_nested_html_element() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                  <div>
                    <div>
                     ^
                        <div>Content</div>
                    </div>
                  </div>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 3, col 6)
                3 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 5, col 7)
                5 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_html_closing_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                    <div>
                        <span>Content</span>
                    </div>
                       ^
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_self_closing_html_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                    <br />
                     ^
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <br />
                  |      ^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_record_type() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                record Icon {
                       ^
                  id: String,
                  title: String,
                  img_src: String,
                  description: String,
                }

                fn IconItem(
                  icon: Icon,
                ) -> Html {
                  <a class="flex flex-col gap-2" href={
                    "/icons/" + icon.id,
                  }>
                    <img class="rounded-lg object-cover aspect-3/2" src={
                      icon.img_src,
                    } />
                    <h2 class="font-semibold text-lg">
                      {icon.title}
                    </h2>
                    {icon.description}
                  </a>
                }

                fn IconsPage(
                  icons: Array[Icon],
                ) -> Html {
                  <div class="flex">
                      {for icon in icons {
                        <IconItem icon={icon}/>
                      }}
                  </div>
                }

                fn IconShowPage(
                  icon: Icon,
                ) -> Html {
                  <div class="flex">
                    <div class="flex flex-col gap-4 p-8 mx-auto my-8 w-full max-w-4xl">
                      <h1 class="text-xl font-semibold">
                        {icon.title}
                      </h1>
                      <div>
                        {icon.description}
                      </div>
                      <img class="rounded-lg" src={
                        icon.img_src,
                      } />
                    </div>
                  </div>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 8)
                 1 | record Icon {
                   |        ^^^^

                Rename
                  --> main.hop (line 9, col 9)
                 9 |   icon: Icon,
                   |         ^^^^

                Rename
                  --> main.hop (line 25, col 16)
                25 |   icons: Array[Icon],
                   |                ^^^^

                Rename
                  --> main.hop (line 35, col 9)
                35 |   icon: Icon,
                   |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_record_type_in_match_pattern() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                record User {
                       ^
                  name: String,
                }

                fn Greeting(user: User) -> Html {
                  match user {
                    User {name} => <span>{name}</span>,
                  }
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 8)
                1 | record User {
                  |        ^^^^

                Rename
                  --> main.hop (line 5, col 19)
                5 | fn Greeting(user: User) -> Html {
                  |                   ^^^^

                Rename
                  --> main.hop (line 7, col 5)
                7 |     User {name} => <span>{name}</span>,
                  |     ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_enum_type() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                enum Status {
                     ^
                  Active,
                  Inactive,
                }

                fn UserBadge(status: Status) -> Html {
                  match status {
                    Status::Active => <span>Active</span>,
                    Status::Inactive => <span>Inactive</span>,
                  }
                }

                fn UsersPage(statuses: Array[Status]) -> Html {
                  for status in statuses {
                    <UserBadge status={status}/>
                  }
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 6)
                 1 | enum Status {
                   |      ^^^^^^

                Rename
                  --> main.hop (line 6, col 22)
                 6 | fn UserBadge(status: Status) -> Html {
                   |                      ^^^^^^

                Rename
                  --> main.hop (line 8, col 5)
                 8 |     Status::Active => <span>Active</span>,
                   |     ^^^^^^

                Rename
                  --> main.hop (line 9, col 5)
                 9 |     Status::Inactive => <span>Inactive</span>,
                   |     ^^^^^^

                Rename
                  --> main.hop (line 13, col 30)
                13 | fn UsersPage(statuses: Array[Status]) -> Html {
                   |                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_imported_enum_type() {
        check_rename_locations(
            indoc! {r#"
                -- types.hop --
                pub enum Status {
                         ^
                  Active,
                  Inactive,
                }

                -- main.hop --
                import types::Status

                fn Main(status: Status) -> Html {
                  match status {
                    Status::Active => <span>Active</span>,
                    Status::Inactive => <span>Inactive</span>,
                  }
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 15)
                1 | import types::Status
                  |               ^^^^^^

                Rename
                  --> main.hop (line 3, col 17)
                3 | fn Main(status: Status) -> Html {
                  |                 ^^^^^^

                Rename
                  --> main.hop (line 5, col 5)
                5 |     Status::Active => <span>Active</span>,
                  |     ^^^^^^

                Rename
                  --> main.hop (line 6, col 5)
                6 |     Status::Inactive => <span>Inactive</span>,
                  |     ^^^^^^

                Rename
                  --> types.hop (line 1, col 10)
                1 | pub enum Status {
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_enum_type_in_page() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                enum Device {
                     ^
                  Desktop,
                  Mobile,
                }

                page Preview(
                  iframe_src: String,
                  device: Device,
                ) {
                  fn body() -> Html {
                    <div class={
                      join!(
                        "bg-white",
                        "h-full",
                        "border",
                        "border-neutral-300",
                        "rounded",
                        "overflow-hidden",
                        match device {
                          Device::Mobile => "w-md",
                          _ => "w-full",
                        },
                      )
                    }>
                      <iframe src={iframe_src} class="w-full h-full">
                      </iframe>
                    </div>
                  }
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 6)
                 1 | enum Device {
                   |      ^^^^^^

                Rename
                  --> main.hop (line 8, col 11)
                 8 |   device: Device,
                   |           ^^^^^^

                Rename
                  --> main.hop (line 20, col 11)
                20 |           Device::Mobile => "w-md",
                   |           ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_even_when_there_is_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                    ^
                  <div>
                  <span>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 4)
                1 | fn Main() -> Html {
                  |    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_component_definition() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                   ^
                  <h1>Hello World</h1>
                }
            "#},
            expect![[r#"
                HelloWorld
                  --> main.hop (line 1, col 4)
                1 | fn HelloWorld() -> Html {
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_enum_declaration() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                enum Status { Active, Inactive }
                     ^
                fn Main(status: Status) -> Html {
                  <div>{status}</div>
                }
            "#},
            expect![[r#"
                Status
                  --> main.hop (line 1, col 6)
                1 | enum Status { Active, Inactive }
                  |      ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_html_element() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                    <div>Content</div>
                     ^
                }
            "#},
            expect![[r#"
                div
                  --> main.hop (line 2, col 6)
                2 |     <div>Content</div>
                  |      ^^^
            "#]],
        );
    }
}
