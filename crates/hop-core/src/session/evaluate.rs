use super::Session;
use super::evaluate_page_error::EvaluatePageError;
use crate::asset_path_rewriter::AssetPathRewriter;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::CheapString;
use crate::ir;
use crate::ir::flat_program::FlatProgram;
use crate::ir::ir_page::IrPage;
use crate::ir::runtime::EvalError;
use crate::ir::runtime::random::random_value;
use crate::ir::{DocumentShell, TailwindInjection, optimize_flat};
use crate::orchestrator::{OrchestrateOptions, orchestrate_flat};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;
use rand::SeedableRng;
use rand::rngs::Xoshiro256PlusPlus;
use std::collections::HashMap;
use std::sync::Arc;

impl Session {
    /// Evaluate a page given a document and page name.
    #[cfg(test)]
    fn evaluate_page_with_values(
        &self,
        document_id: &RootContainedFilePath,
        page_name: &TypeName,
        args: HashMap<AttributeName, ir::runtime::value::Value>,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Result<String, EvaluatePageError> {
        self.evaluate_page_with(
            document_id,
            page_name,
            |_, _| args,
            generated_tailwind_css,
            skip_optimization,
            asset_path_rewriter,
        )
    }

    fn evaluate_page_with(
        &self,
        document_id: &RootContainedFilePath,
        page_name: &TypeName,
        args: impl FnOnce(&FlatProgram, &[IrPage]) -> HashMap<AttributeName, ir::runtime::value::Value>,
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
        let (flat_program, pages) = orchestrate_flat(
            self.typed_modules(),
            &self.type_registry,
            OrchestrateOptions {
                page_filter: Some((document_id.clone(), page_name.clone())),
                asset_path_rewriter,
            },
        );
        let flat_program = if skip_optimization {
            flat_program
        } else {
            optimize_flat(flat_program)
        };
        let args = args(&flat_program, &pages);
        let shell = DocumentShell::new(generated_tailwind_css.map(TailwindInjection::Inline), None);
        let rendered = ir::runtime::flat_evaluator::evaluate_page(
            &flat_program,
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
            EvalError::CallDepthLimit { function, limit } => EvaluatePageError::CallDepthLimit {
                function: function.name.as_str().to_string(),
                limit,
            },
            EvalError::ArrayLengthLimit { limit } => EvaluatePageError::ArrayLengthLimit { limit },
            EvalError::StringLengthLimit { limit } => {
                EvaluatePageError::StringLengthLimit { limit }
            }
            EvalError::HtmlSizeLimit { limit } => EvaluatePageError::HtmlSizeLimit { limit },
            EvalError::OpCountLimit { limit } => EvaluatePageError::OpCountLimit { limit },
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

        let args = |program: &FlatProgram, pages: &[IrPage]| {
            let page = pages
                .iter()
                .find(|page| page.name == page_name)
                .expect("the program compiles the page");
            let parameters = program
                .functions
                .iter()
                .find(|decl| *decl.function() == page.body)
                .expect("the program declares the page's body")
                .parameters();
            page_decl
                .params
                .iter()
                .map(|param| {
                    let name: AttributeName = param.var_name.clone().into();
                    let parameter = parameters
                        .iter()
                        .find(|parameter| parameter.name == name)
                        .expect("the body declares every parameter of the page");
                    let value = random_value(
                        &mut rng,
                        &parameter.binder.typ,
                        param.examples.as_ref(),
                        &program.types,
                    );
                    (name, value)
                })
                .collect()
        };

        self.evaluate_page_with(
            document_id,
            &page_name,
            args,
            generated_tailwind_css,
            skip_optimization,
            asset_path_rewriter,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::session_from_archive;
    use super::*;
    use expect_test::expect;
    use indoc::indoc;
    use txtar::Archive;

    #[test]
    fn should_render_the_document_around_the_head_and_the_body() {
        let session = session_from_archive(&Archive::from(indoc! {r#"
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
        let result = session
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
        let session = session_from_archive(&Archive::from(indoc! {r#"
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
            ir::runtime::value::Value::String("Alice".into()),
        );

        let main_module = RootContainedFilePath::new("main.hop").unwrap();
        let hello_world = TypeName::parse("HelloWorld").unwrap();
        let result = session
            .evaluate_page_with_values(&main_module, &hello_world, args, None, false, None)
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp page without parameters
        let another_comp = TypeName::parse("AnotherComp").unwrap();
        let result = session
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
        let result = session.evaluate_page_with_values(
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
