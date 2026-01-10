use crate::hop::semantics::typed_ast::TypedAst;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::inlined::{DoctypeInjector, HtmlStructureInjector, Inliner, MetaInjector, TailwindInjector};
use crate::ir::{optimize, Compiler, IrEnumDeclaration, IrModule, IrRecordDeclaration};
use anyhow::Result;
use std::collections::HashMap;

pub fn orchestrate(
    typed_asts: HashMap<ModuleName, TypedAst>,
    generated_tailwind_css: Option<&str>,
    pages: &[(ModuleName, ComponentName)],
) -> Result<IrModule> {
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

    let components = Inliner::inline_entrypoints(typed_asts, pages)?
        .into_iter()
        // transform ASTs
        .map(DoctypeInjector::run)
        .map(HtmlStructureInjector::run)
        .map(MetaInjector::run)
        .map(|entrypoint| TailwindInjector::run(entrypoint, generated_tailwind_css))
        // compile to IR
        .map(Compiler::compile)
        .collect();

    let module = IrModule {
        components,
        records,
        enums,
    };

    Ok(optimize(module))
}
