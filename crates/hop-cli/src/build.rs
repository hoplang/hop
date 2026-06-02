use anyhow::Result;
use hop_core::asset_reference;
use hop_core::asset_rewriter::ReplacingAssetRewriter;
use hop_core::document_annotator::DocumentAnnotator;
use hop_core::document_id::DocumentId;
use hop_core::program::Program;
use hop_core::project::Project;
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tailwind_runner::TailwindRunner;

pub struct CompileResult {
    pub output_path: PathBuf,
}

pub fn execute(project: &Project, skip_optimization: bool) -> Result<CompileResult> {
    let config = project.load_config()?;
    let resolved = config.get_resolved_config()?;

    let assets_config = config.assets.as_ref().ok_or_else(|| {
        anyhow::anyhow!(
            "assets.output_dir is required (e.g. `output_dir = \"dist/public\"` in hop.toml)"
        )
    })?;

    // Load program
    let mut program = Program::default();
    for document_id in project.find_hop_modules()? {
        program.update_module(&document_id, project.load_document(&document_id)?);
    }
    for document_id in project.find_css_documents()? {
        program.update_css_document(&document_id, project.load_document(&document_id)?);
    }

    // Print compile errors
    {
        let mut annotator = DocumentAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .with_location();

        for (document_id, errors) in program.get_parse_errors() {
            annotator.annotate(document_id, errors);
        }

        // Don't show type errors if parse errors exist
        if annotator.is_empty() {
            for (document_id, errors) in program.get_type_errors() {
                annotator.annotate(document_id, errors);
            }
        }

        for (document_id, errors) in program.get_css_errors() {
            annotator.annotate(document_id, errors);
        }

        for (document_id, refs) in program.get_asset_references() {
            let errors = asset_reference::validate_asset_existence(refs, project);
            annotator.annotate(document_id, &errors);
        }

        if !annotator.is_empty() {
            return Err(anyhow::anyhow!(
                "Compilation failed:\n{}",
                annotator.render()
            ));
        }
    }

    // Get all asset document ids and compute hashes/filename replacements
    let asset_document_ids: Vec<DocumentId> = program
        .get_asset_references()
        .values()
        .flatten()
        .map(|r| r.document_id.clone())
        .collect();
    let (filenames_with_hashes, filename_replacements) = compute_filename_replacements(
        &asset_document_ids,
        assets_config.production_prefix.clone(),
        project,
    )?;

    let asset_rewriter = Arc::new(ReplacingAssetRewriter::new(filename_replacements));

    let mut css_output = String::new();

    // Run Tailwind on the optimized IR (only classes that survived dead code removal)
    //
    // TODO: Make get_compiled_css_document bundle CSS
    if let Some(input_path) = project.get_css_input_path()? {
        let tailwind_input_document_id = project.path_to_document_id(input_path.as_path())?;
        let compiled_css = program
            .get_compiled_css_document(&tailwind_input_document_id, asset_rewriter.clone())?;
        let tailwind_runner = TailwindRunner::new();
        let sources = program.get_all_hop_sources();
        css_output = tailwind_runner.compile_once(&compiled_css, &sources)?;
    }
    // Hash the rewritten CSS output and compute a href that mirrors how other
    // assets are rewritten (production_prefix + content-hashed filename).
    let css_filename = format!("styles-{:08x}.css", crc32fast::hash(css_output.as_bytes()));
    let css_link_href = match assets_config.production_prefix.as_deref() {
        Some(prefix) => format!("/{}/{}", prefix.trim_matches('/'), css_filename),
        None => format!("/{}", css_filename),
    };

    // Bundle the single JS entrypoint (if configured) with esbuild. The bundled
    // output is hashed and a src is computed the same way as the CSS link
    // (production_prefix + content-hashed filename), then injected as a
    // `<script type="module">` into every view's <head>.
    let js_bundle = match project.get_js_input_path()? {
        Some(input_path) => {
            let bundled = esbuild_runner::bundle_script(&input_path, true)?;
            let js_filename = format!("scripts-{:08x}.js", crc32fast::hash(bundled.as_bytes()));
            let js_src = match assets_config.production_prefix.as_deref() {
                Some(prefix) => format!("/{}/{}", prefix.trim_matches('/'), js_filename),
                None => format!("/{}", js_filename),
            };
            Some((bundled, js_filename, js_src))
        }
        None => None,
    };

    // Compile to IR and inject link to the final CSS file (and script to the JS bundle).
    let generated_code = program.transpile(
        &resolved,
        &css_link_href,
        js_bundle.as_ref().map(|(_, _, src)| src.as_str()),
        skip_optimization,
        Some(asset_rewriter.clone()),
    );

    // Write generated code
    let output_path = project.write_output_path(&generated_code)?;

    // Copy assets with hashed filenames
    copy_assets(
        asset_document_ids,
        project,
        &assets_config.output_dir,
        &filenames_with_hashes,
    )?;

    // Write CSS file
    let css_dest = project
        .get_project_root()
        .join(&assets_config.output_dir)
        .join(&css_filename);
    if let Some(parent) = css_dest.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            anyhow::anyhow!(
                "Failed to create directory {:?} for generated CSS: {}",
                parent,
                err
            )
        })?;
    }
    fs::write(&css_dest, &css_output)
        .map_err(|e| anyhow::anyhow!("Failed to write generated CSS to {:?}: {}", css_dest, e))?;

    // Write JS bundle
    if let Some((bundled, js_filename, _)) = &js_bundle {
        let js_dest = project
            .get_project_root()
            .join(&assets_config.output_dir)
            .join(js_filename);
        if let Some(parent) = js_dest.parent() {
            fs::create_dir_all(parent).map_err(|err| {
                anyhow::anyhow!(
                    "Failed to create directory {:?} for generated JS: {}",
                    parent,
                    err
                )
            })?;
        }
        fs::write(&js_dest, bundled)
            .map_err(|e| anyhow::anyhow!("Failed to write generated JS to {:?}: {}", js_dest, e))?;
    }

    Ok(CompileResult { output_path })
}

/// Insert a content hash into a filename, before the last extension.
fn insert_hash(path: &str, hash: &str) -> String {
    let p = Path::new(path);
    let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
    let new_name = match p.extension().and_then(|e| e.to_str()) {
        Some(ext) => format!("{stem}-{hash}.{ext}"),
        None => format!("{stem}-{hash}"),
    };
    p.with_file_name(new_name).to_string_lossy().into_owned()
}

fn compute_filename_replacements(
    document_ids: &[DocumentId],
    prefix: Option<String>,
    project: &Project,
) -> Result<(HashMap<DocumentId, String>, HashMap<DocumentId, String>)> {
    let document_ids: BTreeSet<DocumentId> = document_ids.iter().cloned().collect();

    let mut filenames_with_hashes = HashMap::new();
    let mut filename_replacements = HashMap::new();
    for document_id in &document_ids {
        let full_path = project.document_id_to_path(document_id);

        let bytes = fs::read(&full_path).map_err(|e| {
            anyhow::anyhow!("Failed to read asset '{}' for hashing: {}", document_id, e)
        })?;
        let filename_for_hash = document_id.to_string();

        let hash = format!("{:08x}", crc32fast::hash(&bytes));
        let hashed_filename = insert_hash(&filename_for_hash, &hash);
        let prefixed_filename = match &prefix {
            Some(p) => format!("/{}/{}", p.trim_matches('/'), hashed_filename),
            None => format!("/{}", hashed_filename),
        };
        filenames_with_hashes.insert(document_id.clone(), hashed_filename);
        filename_replacements.insert(document_id.clone(), prefixed_filename);
    }

    Ok((filenames_with_hashes, filename_replacements))
}

fn copy_assets(
    paths: impl IntoIterator<Item = DocumentId>,
    project: &Project,
    output_dir: &str,
    filenames_with_hashes: &HashMap<DocumentId, String>,
) -> Result<()> {
    let document_ids: BTreeSet<DocumentId> = paths.into_iter().collect();

    let dest_root = project.get_project_root().join(output_dir);

    for document_id in &document_ids {
        let src = project.document_id_to_path(document_id);

        let hashed_filename = filenames_with_hashes
            .get(document_id)
            .unwrap_or_else(|| panic!("no hash computed for asset {}", document_id));
        let dst = dest_root.join(hashed_filename);

        if let Some(parent) = dst.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                anyhow::anyhow!(
                    "Failed to create directory {:?} for asset '{}': {}",
                    parent,
                    document_id,
                    e
                )
            })?;
        }

        fs::copy(&src, &dst).map_err(|e| {
            anyhow::anyhow!("Failed to copy asset '{}' to {:?}: {}", document_id, dst, e)
        })?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use tempfile::TempDir;
    use txtar::{Archive, read_archive_from_dir, write_archive_to_dir};

    fn check(input: &str, expected: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::find_traversing_subdirectories(temp_dir.path()).unwrap();
        execute(&project, false).expect("compilation should succeed");
        let output_archive = read_archive_from_dir(temp_dir.path()).unwrap();
        expected.assert_eq(&output_archive.to_string());
    }

    fn check_error(input: &str, expected: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project_root = archive
            .iter()
            .find(|f| f.name.ends_with("hop.toml"))
            .and_then(|f| Path::new(&f.name).parent().map(|p| p.to_path_buf()))
            .filter(|p| !p.as_os_str().is_empty())
            .map(|p| temp_dir.path().join(p))
            .unwrap_or_else(|| temp_dir.path().to_path_buf());
        let project = Project::from(&project_root).unwrap();

        let err = match execute(&project, false) {
            Ok(_) => panic!("compilation should fail"),
            Err(e) => e,
        };
        expected.assert_eq(&format!("{err}"));
    }

    #[test]
    #[ignore]
    fn deterministic_output_order_across_hop_modules() {
        // Entrypoints are sorted alphabetically by module name.
        // With 8 modules, only 1/40320 chance of accidental success if HashMap iteration leaked.
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "output.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- alpha.hop --
                view AlphaPage() { <div>Alpha</div> }
                -- beta.hop --
                view BetaPage() { <div>Beta</div> }
                -- gamma.hop --
                view GammaPage() { <div>Gamma</div> }
                -- delta.hop --
                view DeltaPage() { <div>Delta</div> }
                -- epsilon.hop --
                view EpsilonPage() { <div>Epsilon</div> }
                -- zeta.hop --
                view ZetaPage() { <div>Zeta</div> }
                -- eta.hop --
                view EtaPage() { <div>Eta</div> }
                -- theta.hop --
                view ThetaPage() { <div>Theta</div> }
            "#},
            expect![[r#"
                -- alpha.hop --
                view AlphaPage() { <div>Alpha</div> }
                -- beta.hop --
                view BetaPage() { <div>Beta</div> }
                -- delta.hop --
                view DeltaPage() { <div>Delta</div> }
                -- dist/public/styles-00000000.css --
                -- epsilon.hop --
                view EpsilonPage() { <div>Epsilon</div> }
                -- eta.hop --
                view EtaPage() { <div>Eta</div> }
                -- gamma.hop --
                view GammaPage() { <div>Gamma</div> }
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "output.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- output.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function AlphaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Alpha</div>";
                    output += "</body></html>";
                    return output;
                }

                export function BetaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Beta</div>";
                    output += "</body></html>";
                    return output;
                }

                export function DeltaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Delta</div>";
                    output += "</body></html>";
                    return output;
                }

                export function EpsilonPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Epsilon";
                    output += "</div></body></html>";
                    return output;
                }

                export function EtaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Eta</div>";
                    output += "</body></html>";
                    return output;
                }

                export function GammaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Gamma</div>";
                    output += "</body></html>";
                    return output;
                }

                export function ThetaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Theta</div>";
                    output += "</body></html>";
                    return output;
                }

                export function ZetaPage(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div>Zeta</div>";
                    output += "</body></html>";
                    return output;
                }
                -- style.css --
                -- theta.hop --
                view ThetaPage() { <div>Theta</div> }
                -- zeta.hop --
                view ZetaPage() { <div>Zeta</div> }
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn copy_assets_to_output_dir() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- logo.svg --
                <svg>logo</svg>
                -- icons/star.svg --
                <svg>star</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"icon\" href=\"/logo-ffe99b60.svg\"";
                    output += "><link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/icons/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- dist/public/icons/star-890d8c02.svg --
                <svg>star</svg>
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- icons/star.svg --
                <svg>star</svg>
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn production_prefix_does_not_affect_output_dir() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><body><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"icon\" href=\"";
                    output += "/static/v1/logo-ffe99b60.svg\"><link rel=\"stylesheet\"";
                    output += " href=\"/static/v1/styles-00000000.css\"></head></body>";
                    output += "</html>";
                    return output;
                }
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn copy_assets_to_output_dir_above_project_root() {
        check(
            indoc! {r#"
                -- hop/style.css --
                -- hop/hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "../assets"
                -- hop/main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- hop/logo.svg --
                <svg>logo</svg>
                -- hop/icons/star.svg --
                <svg>star</svg>
            "#},
            expect![[r#"
                -- assets/icons/star-890d8c02.svg --
                <svg>star</svg>
                -- assets/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- assets/styles-00000000.css --
                -- hop/app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"icon\" href=\"/logo-ffe99b60.svg\"";
                    output += "><link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/icons/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- hop/hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "../assets"
                -- hop/icons/star.svg --
                <svg>star</svg>
                -- hop/logo.svg --
                <svg>logo</svg>
                -- hop/main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- hop/style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn missing_output_dir_is_rejected() {
        check_error(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[
                r#"assets.output_dir is required (e.g. `output_dir = "dist/public"` in hop.toml)"#
            ]],
        )
    }

    #[test]
    #[ignore]
    fn copy_assets_with_content_hash() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- logo.svg --
                <svg>logo</svg>
                -- icons/star.svg --
                <svg>star</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"icon\" href=\"/logo-ffe99b60.svg\"";
                    output += "><link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/icons/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- dist/public/icons/star-890d8c02.svg --
                <svg>star</svg>
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- icons/star.svg --
                <svg>star</svg>
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                view Home() {
                    <head>
                        <link rel="icon" href={asset!("/logo.svg")} />
                    </head>
                    <body>
                        <img src={asset!("/icons/star.svg")} />
                    </body>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn copy_assets_same_content_different_paths() {
        // Two assets with identical content but different paths, same hash suffix, different dirs.
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <img src={asset!("/images/a.svg")} />
                    <img src={asset!("/images/b.svg")} />
                }
                -- images/a.svg --
                <svg>same</svg>
                -- images/b.svg --
                <svg>same</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><img src=\"";
                    output += "/images/a-d8c00d88.svg\"><img src=\"/images/b-d8c00d88.svg\">";
                    output += "</body></html>";
                    return output;
                }
                -- dist/public/images/a-d8c00d88.svg --
                <svg>same</svg>
                -- dist/public/images/b-d8c00d88.svg --
                <svg>same</svg>
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- images/a.svg --
                <svg>same</svg>
                -- images/b.svg --
                <svg>same</svg>
                -- main.hop --
                view Home() {
                    <img src={asset!("/images/a.svg")} />
                    <img src={asset!("/images/b.svg")} />
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn tailwind_css_emitted_as_hashed_link() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"></head><body><div";
                    output += " class=\"text-red-500\">hi</div></body></html>";
                    return output;
                }
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn tailwind_css_link_uses_production_prefix() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                
                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/static/v1/styles-00000000.css\"></head><body><div";
                    output += " class=\"text-red-500\">hi</div></body></html>";
                    return output;
                }
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn js_entrypoint_is_bundled_and_injected() {
        // The single JS entrypoint configured via [js] is bundled with esbuild,
        // emitted as a content-hashed scripts-<hash>.js under the assets output
        // dir, and a <script type="module"> referencing it is injected into the
        // <head> of every view (mirroring how the Tailwind CSS <link> works).
        check(
            indoc! {r#"
                -- app.ts --
                let color: String = "red";
                document.querySelector('body').style.background = color;
                -- style.css --
                -- hop.toml --
                [js]
                bundler = "esbuild"
                input_path = "app.ts"

                [compile]
                target = "rust"
                output_path = "app.rs"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <body>
                      <div class="text-red-500">hi</div>
                    </body>
                }
            "#},
            expect![[r#"
                -- app.rs --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Home {}

                impl View for Home {
                    fn render(self) -> String {
                        let mut output = String::new();
                        output.push_str("<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta");
                        output.push_str(" content=\"width=device-width, initial-scale=1\"");
                        output.push_str(" name=\"viewport\"><link rel=\"stylesheet\"");
                        output.push_str(" href=\"/static/v1/styles-00000000.css\"><script");
                        output.push_str(" type=\"module\" src=\"/static/v1/scripts-27809078.js\">");
                        output.push_str("</script></head><body><div class=\"text-red-500\">hi</div>");
                        output.push_str("</body></html>");
                        output
                    }
                }
                -- app.ts --
                let color: String = "red";
                document.querySelector('body').style.background = color;
                -- dist/public/scripts-27809078.js --
                var e="red";document.querySelector("body").style.background=e;
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [js]
                bundler = "esbuild"
                input_path = "app.ts"

                [compile]
                target = "rust"
                output_path = "app.rs"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <body>
                      <div class="text-red-500">hi</div>
                    </body>
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn js_entrypoint_without_production_prefix() {
        check(
            indoc! {r#"
                -- app.ts --
                let color: String = "red";
                document.querySelector('body').style.background = color;
                -- style.css --
                -- hop.toml --
                [js]
                bundler = "esbuild"
                input_path = "app.ts"

                [compile]
                target = "ts"
                output_path = "out.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
            "#},
            expect![[r#"
                -- app.ts --
                let color: String = "red";
                document.querySelector('body').style.background = color;
                -- dist/public/scripts-27809078.js --
                var e="red";document.querySelector("body").style.background=e;
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [js]
                bundler = "esbuild"
                input_path = "app.ts"

                [compile]
                target = "ts"
                output_path = "out.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <div class="text-red-500">hi</div>
                }
                -- out.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-00000000.css\"><script type=\"module\"";
                    output += " src=\"/scripts-27809078.js\"></script></head><body><div";
                    output += " class=\"text-red-500\">hi</div></body></html>";
                    return output;
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn css_asset_copies_referenced_file_and_rewrites_url() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
                -- fonts/inter.woff2 --
                fake-woff2-bytes
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-ca0dd958.css\"></head><body><div>hi</div>";
                    output += "</body></html>";
                    return output;
                }
                -- dist/public/fonts/inter-1c757f7b.woff2 --
                fake-woff2-bytes
                -- dist/public/styles-ca0dd958.css --
                @font-face{font-family:Inter;src:url(/fonts/inter-1c757f7b.woff2)format("woff2")}
                -- fonts/inter.woff2 --
                fake-woff2-bytes
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn css_asset_rewrites_with_production_prefix() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
                -- fonts/inter.woff2 --
                fake-woff2-bytes
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/static/v1/styles-25fd2426.css\"></head><body><div>hi";
                    output += "</div></body></html>";
                    return output;
                }
                -- dist/public/fonts/inter-1c757f7b.woff2 --
                fake-woff2-bytes
                -- dist/public/styles-25fd2426.css --
                @font-face{font-family:Inter;src:url(/static/v1/fonts/inter-1c757f7b.woff2)format("woff2")}
                -- fonts/inter.woff2 --
                fake-woff2-bytes
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn css_asset_error_missing_file() {
        check_error(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    src: --asset("/fonts/missing.woff2");
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#},
            expect![[r#"
                Compilation failed:
                error: asset `fonts/missing.woff2` does not exist on disk
                  --> input.css (line 2, col 10)
                1 | @font-face {
                2 |     src: --asset("/fonts/missing.woff2");
                  |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn css_asset_error_non_string_argument() {
        check_error(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    src: --asset(var(--x));
                }
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#},
            expect![[r#"
                Compilation failed:
                error: CSS `--asset()` call has a non-string-literal argument: `var(--x)`
                  --> input.css (line 2, col 10)
                1 | @font-face {
                2 |     src: --asset(var(--x));
                  |          ^^^^^^^^^^^^^^^^^
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn css_asset_error_tailwind_expansion() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @import "tailwindcss";
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/styles-dd9995d1.css\"></head><body><div>hi</div>";
                    output += "</body></html>";
                    return output;
                }
                -- dist/public/styles-dd9995d1.css --
                /*! tailwindcss v4.3.0 | MIT License | https://tailwindcss.com */
                @layer theme{:root,:host{--font-sans:ui-sans-serif, system-ui, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji";--font-mono:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;--default-font-family:var(--font-sans);--default-mono-font-family:var(--font-mono)}}@layer base{*,:after,:before,::backdrop{box-sizing:border-box;border:0 solid;margin:0;padding:0}::file-selector-button{box-sizing:border-box;border:0 solid;margin:0;padding:0}html,:host{-webkit-text-size-adjust:100%;tab-size:4;line-height:1.5;font-family:var(--default-font-family,ui-sans-serif, system-ui, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji");font-feature-settings:var(--default-font-feature-settings,normal);font-variation-settings:var(--default-font-variation-settings,normal);-webkit-tap-highlight-color:transparent}hr{height:0;color:inherit;border-top-width:1px}abbr:where([title]){-webkit-text-decoration:underline dotted;text-decoration:underline dotted}h1,h2,h3,h4,h5,h6{font-size:inherit;font-weight:inherit}a{color:inherit;-webkit-text-decoration:inherit;-webkit-text-decoration:inherit;-webkit-text-decoration:inherit;text-decoration:inherit}b,strong{font-weight:bolder}code,kbd,samp,pre{font-family:var(--default-mono-font-family,ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace);font-feature-settings:var(--default-mono-font-feature-settings,normal);font-variation-settings:var(--default-mono-font-variation-settings,normal);font-size:1em}small{font-size:80%}sub,sup{vertical-align:baseline;font-size:75%;line-height:0;position:relative}sub{bottom:-.25em}sup{top:-.5em}table{text-indent:0;border-color:inherit;border-collapse:collapse}:-moz-focusring{outline:auto}progress{vertical-align:baseline}summary{display:list-item}ol,ul,menu{list-style:none}img,svg,video,canvas,audio,iframe,embed,object{vertical-align:middle;display:block}img,video{max-width:100%;height:auto}button,input,select,optgroup,textarea{font:inherit;font-feature-settings:inherit;font-variation-settings:inherit;letter-spacing:inherit;color:inherit;opacity:1;background-color:#0000;border-radius:0}::file-selector-button{font:inherit;font-feature-settings:inherit;font-variation-settings:inherit;letter-spacing:inherit;color:inherit;opacity:1;background-color:#0000;border-radius:0}:where(select:is([multiple],[size])) optgroup{font-weight:bolder}:where(select:is([multiple],[size])) optgroup option{padding-inline-start:20px}::file-selector-button{margin-inline-end:4px}::placeholder{opacity:1}@supports (not ((-webkit-appearance:-apple-pay-button))) or (contain-intrinsic-size:1px){::placeholder{color:currentColor}@supports (color:color-mix(in lab, red, red)){::placeholder{color:color-mix(in oklab, currentcolor 50%, transparent)}}}textarea{resize:vertical}::-webkit-search-decoration{-webkit-appearance:none}::-webkit-date-and-time-value{min-height:1lh;text-align:inherit}::-webkit-datetime-edit{display:inline-flex}::-webkit-datetime-edit-fields-wrapper{padding:0}::-webkit-datetime-edit{padding-block:0}::-webkit-datetime-edit-year-field{padding-block:0}::-webkit-datetime-edit-month-field{padding-block:0}::-webkit-datetime-edit-day-field{padding-block:0}::-webkit-datetime-edit-hour-field{padding-block:0}::-webkit-datetime-edit-minute-field{padding-block:0}::-webkit-datetime-edit-second-field{padding-block:0}::-webkit-datetime-edit-millisecond-field{padding-block:0}::-webkit-datetime-edit-meridiem-field{padding-block:0}::-webkit-calendar-picker-indicator{line-height:1}:-moz-ui-invalid{box-shadow:none}button,input:where([type=button],[type=reset],[type=submit]){appearance:button}::file-selector-button{appearance:button}::-webkit-inner-spin-button{height:auto}::-webkit-outer-spin-button{height:auto}[hidden]:where(:not([hidden=until-found])){display:none!important}}@layer components,utilities;
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @import "tailwindcss";
                -- main.hop --
                view Home() {
                    <div>hi</div>
                }
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn asset_with_production_prefix_logo() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "/assets"
                output_dir = "dist/public"
                -- main.hop --
                view Home() {
                    <img src={asset!("/logo.svg")} />
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta";
                    output += " content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link rel=\"stylesheet\"";
                    output += " href=\"/assets/styles-00000000.css\"></head><body><img src=\"";
                    output += "/assets/logo-ffe99b60.svg\"></body></html>";
                    return output;
                }
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                bundler = "tailwind_4"
                input_path = "style.css"

                [assets]
                production_prefix = "/assets"
                output_dir = "dist/public"
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                view Home() {
                    <img src={asset!("/logo.svg")} />
                }
                -- style.css --
            "#]],
        )
    }
}
