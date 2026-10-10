use super::Program;
use super::evaluate_page_error::EvaluatePageError;
use crate::asset_path_rewriter::AssetPathRewriter;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::CheapString;
use crate::ir;
use crate::ir::runtime::EvalError;
use crate::ir::runtime::random::random_value;
use crate::ir::{DocumentShell, TailwindInjection, optimize_flat, pure_to_flat};
use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;
use rand::SeedableRng;
use rand::rngs::Xoshiro256PlusPlus;
use std::collections::HashMap;
use std::sync::Arc;

impl Program {
    /// Evaluate a page given a document and page name.
    fn evaluate_page_with_values(
        &self,
        document_id: &RootContainedFilePath,
        page_name: &TypeName,
        args: HashMap<AttributeName, ir::runtime::value::Value>,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Result<String, EvaluatePageError> {
        // Refuse to evaluate if there are errors in any document
        if self.parse_errors.values().any(|errors| !errors.is_empty()) {
            return Err(EvaluatePageError::ParseErrors);
        }
        if self
            .type_errors
            .values()
            .flatten()
            .any(|error| error.severity() == DiagnosticSeverity::Error)
        {
            return Err(EvaluatePageError::TypeErrors);
        }

        // The page filter compiles only the requested page and what it
        // reaches.
        let (pure_module, pages) = orchestrate_pure(
            self.typed_modules(),
            OrchestrateOptions {
                page_filter: Some((document_id.clone(), page_name.clone())),
                asset_path_rewriter,
            },
        );
        let flat_module = pure_to_flat(pure_module);
        let flat_module = if skip_optimization {
            flat_module
        } else {
            optimize_flat(flat_module)
        };
        let shell = DocumentShell::new(generated_tailwind_css.map(TailwindInjection::Inline), None);
        let rendered = ir::runtime::flat_evaluator::evaluate_page(
            &flat_module,
            &pages,
            page_name,
            args,
            Some(&shell),
        );

        rendered.map_err(|e| match e {
            EvalError::PageNotFound { page } => EvaluatePageError::PageNotFound {
                page: page.to_string(),
                available: self.page_names(),
            },
            EvalError::MissingParameter { param, .. } => EvaluatePageError::MissingParameter {
                page: page_name.to_string(),
                param: param.to_string(),
            },
            EvalError::RecursionLimit { function, limit } => EvaluatePageError::RecursionLimit {
                function: function.name.as_str().to_string(),
                limit,
            },
            EvalError::FunctionNotFound { .. } | EvalError::ArgumentCount { .. } => {
                unreachable!("a call in a typechecked module matches its declaration")
            }
        })
    }

    /// Evaluate a page with randomly generated parameter values derived from `seed`.
    pub fn evaluate_page_with_random_values(
        &self,
        page: &str,
        seed: u64,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Result<String, EvaluatePageError> {
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(seed);

        let page_name = TypeName::new(CheapString::new(page.to_string())).map_err(|e| {
            EvaluatePageError::InvalidPageName {
                page: page.to_string(),
                reason: e.to_string(),
            }
        })?;

        let (document_id, page_decl) = self
            .typed_modules
            .iter()
            .find_map(|(document_id, module)| {
                module
                    .page_declarations()
                    .iter()
                    .find(|ep| ep.name == page_name)
                    .map(|ep| (document_id, ep))
            })
            .ok_or_else(|| EvaluatePageError::PageNotFound {
                page: page.to_string(),
                available: self.page_names(),
            })?;

        let params = page_decl
            .params
            .iter()
            .map(|param| {
                (
                    param.var_name.clone().into(),
                    random_value(
                        &mut rng,
                        &param.var_type,
                        param.examples.as_ref(),
                        &self.type_registry,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        self.evaluate_page_with_values(
            document_id,
            &page_name,
            params,
            generated_tailwind_css,
            skip_optimization,
            asset_path_rewriter,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::program_from_archive;
    use super::*;
    use expect_test::expect;
    use indoc::indoc;
    use txtar::Archive;

    #[test]
    fn should_render_the_document_around_the_head_and_the_body() {
        let program = program_from_archive(&Archive::from(indoc! {r#"
            -- main.hop --
            page Main() {
              fn head() -> Html {
                <title>Hi</title>
              }
              fn body() -> Html {
                <p>Hello</p>
              }
            }
        "#}));

        let main_module = RootContainedFilePath::new("main.hop").unwrap();
        let main = TypeName::parse("Main").unwrap();
        let result = program
            .evaluate_page_with_values(
                &main_module,
                &main,
                HashMap::new(),
                Some(".text-red { color: red; }"),
                false,
                None,
            )
            .expect("Should evaluate successfully");

        expect![[r#"<!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport"><title>Hi</title><style>.text-red { color: red; }</style></head><body><p>Hello</p></body></html>"#]].assert_eq(&result);
    }

    #[test]
    fn should_evaluate_ir_page_with_parameters() {
        let program = program_from_archive(&Archive::from(indoc! {r#"
            -- main.hop --
            page HelloWorld(name: String) {
              fn body() -> Html {
                <h1>Hello {name}!</h1>
              }
            }

            page AnotherComp() {
              fn body() -> Html {
                <p>Static content</p>
              }
            }
        "#}));

        // Test evaluating hello-world page with a name parameter
        let mut args = HashMap::new();
        args.insert(
            AttributeName::parse("name").unwrap(),
            ir::runtime::value::Value::String("Alice".to_string()),
        );

        let main_module = RootContainedFilePath::new("main.hop").unwrap();
        let hello_world = TypeName::parse("HelloWorld").unwrap();
        let result = program
            .evaluate_page_with_values(&main_module, &hello_world, args, None, false, None)
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp page without parameters
        let another_comp = TypeName::parse("AnotherComp").unwrap();
        let result = program
            .evaluate_page_with_values(
                &main_module,
                &another_comp,
                HashMap::new(),
                None,
                false,
                None,
            )
            .expect("Should evaluate successfully");

        assert!(result.contains("<p>Static content</p>"));

        // Test error when page doesn't exist
        let non_existent = TypeName::parse("NonExistent").unwrap();
        let result = program.evaluate_page_with_values(
            &main_module,
            &non_existent,
            HashMap::new(),
            None,
            false,
            None,
        );
        assert!(
            matches!(result, Err(EvaluatePageError::PageNotFound { .. })),
            "Expected PageNotFound error, got: {:?}",
            result
        );
    }
}
