use crate::hop::syntax::parsed_ast::{ParsedAst, ParsedDeclaration};

/// Sorts import declarations alphabetically by module name, then by type name.
///
/// Non-import declarations maintain their relative order and remain after
/// the sorted imports.
pub fn sort_imports(ast: ParsedAst) -> ParsedAst {
    let declarations = ast.get_declarations();

    let mut imports: Vec<_> = declarations
        .iter()
        .filter_map(|d| match d {
            ParsedDeclaration::Import(i) => Some(i.clone()),
            _ => None,
        })
        .collect();

    imports.sort_by(|a, b| {
        a.module_name
            .cmp(&b.module_name)
            .then_with(|| a.type_name.cmp(&b.type_name))
    });

    let non_imports: Vec<_> = declarations
        .iter()
        .filter(|d| !matches!(d, ParsedDeclaration::Import(_)))
        .cloned()
        .collect();

    let sorted_declarations: Vec<ParsedDeclaration> = imports
        .into_iter()
        .map(ParsedDeclaration::Import)
        .chain(non_imports)
        .collect();

    ParsedAst::new(ast.name.clone(), sorted_declarations, ast.comments().to_vec())
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

    use super::sort_imports;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            source.to_string(),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        let ast = remove_whitespace(ast);
        let ast = sort_imports(ast);
        expected.assert_eq(&ast.to_doc().pretty(60).to_string());
    }

    #[test]
    fn sorts_imports_alphabetically_by_module() {
        check(
            indoc! {"
                import zebra::Animal
                import apple::Fruit
                import mango::Tropical
                <Main></Main>
            "},
            expect![[r#"
                import apple::Fruit
                import mango::Tropical
                import zebra::Animal

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn sorts_imports_by_type_name_within_same_module() {
        check(
            indoc! {"
                import components::Zebra
                import components::Apple
                import components::Mango
                <Main></Main>
            "},
            expect![[r#"
                import components::Apple
                import components::Mango
                import components::Zebra

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn sorts_by_module_then_type_name() {
        check(
            indoc! {"
                import beta::Zebra
                import alpha::Banana
                import beta::Aardvark
                import alpha::Apple
                <Main></Main>
            "},
            expect![[r#"
                import alpha::Apple
                import alpha::Banana
                import beta::Aardvark
                import beta::Zebra

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn preserves_non_import_declarations_order() {
        check(
            indoc! {"
                import zebra::Animal
                record Second { b: Int }
                import apple::Fruit
                record First { a: String }
                <Main></Main>
            "},
            expect![[r#"
                import apple::Fruit
                import zebra::Animal

                record Second {
                  b: Int,
                }

                record First {
                  a: String,
                }

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_no_imports() {
        check(
            indoc! {"
                record User { name: String }
                <Main></Main>
            "},
            expect![[r#"
                record User {
                  name: String,
                }

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_only_imports() {
        check(
            indoc! {"
                import zebra::Z
                import apple::A
            "},
            expect![[r#"
                import apple::A
                import zebra::Z
            "#]],
        );
    }

    #[test]
    fn handles_single_import() {
        check(
            indoc! {"
                import components::Button
                <Main></Main>
            "},
            expect![[r#"
                import components::Button

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_nested_module_paths() {
        check(
            indoc! {"
                import ui::components::Button
                import api::handlers::User
                import ui::components::Alert
                <Main></Main>
            "},
            expect![[r#"
                import api::handlers::User
                import ui::components::Alert
                import ui::components::Button

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn moves_imports_before_other_declarations() {
        check(
            indoc! {"
                record User { name: String }
                import components::Button
                enum Status { Active }
                import utils::Helper
                <Main></Main>
            "},
            expect![[r#"
                import components::Button
                import utils::Helper

                record User {
                  name: String,
                }

                enum Status {
                  Active,
                }

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn already_sorted_imports_unchanged() {
        check(
            indoc! {"
                import alpha::First
                import alpha::Second
                import beta::Third
                <Main></Main>
            "},
            expect![[r#"
                import alpha::First
                import alpha::Second
                import beta::Third

                <Main></Main>
            "#]],
        );
    }
}
