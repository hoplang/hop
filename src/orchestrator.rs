use crate::hop::semantics::typed_ast::TypedAst;
use crate::hop::symbols::module_name::ModuleName;
use crate::inlined::{DoctypeInjector, HtmlStructureInjector, Inliner, MetaInjector, TailwindInjector};
use crate::ir::{optimize, Compiler, IrEnumDeclaration, IrModule, IrRecordDeclaration};
use std::collections::HashMap;

#[derive(Default)]
pub struct OrchestrateOptions {
    pub skip_html_structure: bool,
    pub skip_dev_mode_wrapper: bool,
    pub skip_optimization: bool,
}

pub fn orchestrate(
    typed_asts: &HashMap<ModuleName, TypedAst>,
    generated_tailwind_css: Option<&str>,
    options: OrchestrateOptions,
) -> IrModule {
    // Collect record declarations from all modules
    let mut records: Vec<IrRecordDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_records())
        .map(|record| IrRecordDeclaration {
            name: record.name.to_string(),
            fields: record.fields.clone(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    // Collect enum declarations from all modules
    let mut enums: Vec<IrEnumDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_enums())
        .map(|enum_decl| IrEnumDeclaration {
            name: enum_decl.name.as_str().to_string(),
            variants: enum_decl.variants.clone(),
        })
        .collect();
    enums.sort_by(|a, b| a.name.cmp(&b.name));

    let components = Inliner::inline_ast_entrypoints(typed_asts)
        .into_iter()
        // transform ASTs (skip if options.skip_html_structure is set)
        .map(|e| if options.skip_html_structure { e } else { DoctypeInjector::run(e) })
        .map(|e| if options.skip_html_structure { e } else { HtmlStructureInjector::run(e) })
        .map(|e| if options.skip_html_structure { e } else { MetaInjector::run(e) })
        .map(|e| if options.skip_html_structure { e } else { TailwindInjector::run(e, generated_tailwind_css) })
        // compile to IR
        .map(|e| {
            if options.skip_dev_mode_wrapper {
                Compiler::compile_without_dev_wrapper(e)
            } else {
                Compiler::compile(e)
            }
        })
        .collect();

    let module = IrModule {
        components,
        records,
        enums,
    };

    if options.skip_optimization {
        module
    } else {
        optimize(module)
    }
}
