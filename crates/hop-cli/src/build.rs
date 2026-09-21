use anyhow::Result;
use hop_core::{
    AssetReference, AssetRewriter, Diagnostic, DocumentAnnotator, DocumentId, Program, Project,
    Severity,
};
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tailwind_runner::TailwindRunner;

pub struct CompileResult {
    pub output_path: PathBuf,
}

fn annotated_config_error(error: Diagnostic) -> anyhow::Error {
    let mut annotator = DocumentAnnotator::new()
        .with_severity_label()
        .with_lines_before(1)
        .with_location();
    annotator.annotate([error]);
    anyhow::anyhow!("Configuration failed:\n{}", annotator.render())
}

pub fn execute(project: &Project, skip_optimization: bool) -> Result<CompileResult> {
    let config = project.load_config()?;
    let assets_output_dir = config.assets_output_dir().map_err(annotated_config_error)?;
    let production_prefix = config
        .assets_production_prefix()
        .map_err(annotated_config_error)?;

    // Load program
    let mut program = Program::new();
    for document_id in project.documents()? {
        let document = project.load_document(&document_id)?;
        if document_id.extension() == Some("css") {
            program.update_css_document(&document_id, document);
        } else {
            program.update_module(&document_id, document);
        }
    }

    // Print compile errors
    {
        let mut diagnostics = program.diagnostics();

        // An asset referenced via `asset!()` (in hop) or `--asset()` (in CSS)
        // that does not exist on disk.
        for refs in program.asset_references().values() {
            diagnostics.extend(
                refs.iter()
                    .filter(|asset_ref| {
                        !project
                            .document_id_to_path(asset_ref.document_id())
                            .exists()
                    })
                    .map(AssetReference::not_found),
            );
        }

        if diagnostics.iter().any(|d| d.severity() == Severity::Error) {
            let mut annotator = DocumentAnnotator::new()
                .with_severity_label()
                .with_lines_before(1)
                .with_location();
            annotator.annotate(diagnostics);
            return Err(anyhow::anyhow!(
                "Compilation failed:\n{}",
                annotator.render()
            ));
        }
    }

    // Get all asset document ids and compute hashes/filename replacements
    let asset_document_ids: Vec<DocumentId> = program
        .asset_references()
        .values()
        .flatten()
        .map(|r| r.document_id().clone())
        .collect();
    let (filenames_with_hashes, filename_replacements) =
        compute_filename_replacements(&asset_document_ids, production_prefix.clone(), project)?;

    let asset_rewriter: Arc<dyn AssetRewriter> =
        Arc::new(move |document_id: &DocumentId| filename_replacements[document_id].clone());

    let mut css_output = String::new();

    // Run Tailwind on the optimized IR (only classes that survived dead code removal)
    //
    // TODO: Make compiled_css_document bundle CSS
    if let Some(css_input_path) = config.css_input_path().map_err(annotated_config_error)? {
        let input_path = project.project_root().join(css_input_path);
        let tailwind_input_document_id = project.path_to_document_id(input_path.as_path())?;
        let compiled_css = program
            .compiled_css_document(&tailwind_input_document_id, asset_rewriter.clone())
            .ok_or_else(|| {
                anyhow::anyhow!("CSS document '{}' not found", tailwind_input_document_id)
            })?;
        let tailwind_runner = TailwindRunner::new();
        let sources = program.sources();
        css_output = tailwind_runner.compile_once(&compiled_css, &sources)?;
    }
    // Hash the rewritten CSS output and compute a href that mirrors how other
    // assets are rewritten (production_prefix + content-hashed filename).
    let css_filename = format!("styles-{:08x}.css", crc32fast::hash(css_output.as_bytes()));
    let css_link_href = match production_prefix.as_deref() {
        Some(prefix) => format!("/{}/{}", prefix.trim_matches('/'), css_filename),
        None => format!("/{}", css_filename),
    };

    // Bundle the single JS entrypoint (if configured) with esbuild. The bundled
    // output is hashed and a src is computed the same way as the CSS link
    // (production_prefix + content-hashed filename), then injected as a
    // `<script type="module">` into every page's <head>.
    let js_bundle = match config.js_input_path().map_err(annotated_config_error)? {
        Some(js_input_path) => {
            let input_path = project.project_root().join(js_input_path);
            let bundled = esbuild_runner::bundle_script(&input_path, true)?;
            let js_filename = format!("scripts-{:08x}.js", crc32fast::hash(bundled.as_bytes()));
            let js_src = match production_prefix.as_deref() {
                Some(prefix) => format!("/{}/{}", prefix.trim_matches('/'), js_filename),
                None => format!("/{}", js_filename),
            };
            Some((bundled, js_filename, js_src))
        }
        None => None,
    };

    // Compile to IR and inject link to the final CSS file (and script to the JS bundle).
    let generated_code = program.transpile(
        config.target().map_err(annotated_config_error)?,
        &css_link_href,
        js_bundle.as_ref().map(|(_, _, src)| src.as_str()),
        skip_optimization,
        Some(asset_rewriter.clone()),
    );

    // Preserve the file's mtime if the content is unchanged, so downstream
    // build tools (e.g. cargo) don't trigger unnecessary recompiles.
    let output_path = project
        .project_root()
        .join(config.output_path().map_err(annotated_config_error)?);
    if !fs::read(&output_path).is_ok_and(|existing| existing == generated_code.as_bytes()) {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&output_path, &generated_code)?;
    }
    let output_path = output_path.canonicalize()?;

    // Copy assets with hashed filenames
    copy_assets(
        asset_document_ids,
        project,
        &assets_output_dir,
        &filenames_with_hashes,
    )?;

    // Write CSS file
    let css_dest = project
        .project_root()
        .join(&assets_output_dir)
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
            .project_root()
            .join(&assets_output_dir)
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

    let dest_root = project.project_root().join(output_dir);

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
        // Pages are sorted alphabetically by module name.
        // With 8 modules, only 1/40320 chance of accidental success if HashMap iteration leaked.
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "output.ts"
                
                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- alpha.hop --
                page AlphaPage() { fn body() -> Html { <div>Alpha</div> } }
                -- beta.hop --
                page BetaPage() { fn body() -> Html { <div>Beta</div> } }
                -- gamma.hop --
                page GammaPage() { fn body() -> Html { <div>Gamma</div> } }
                -- delta.hop --
                page DeltaPage() { fn body() -> Html { <div>Delta</div> } }
                -- epsilon.hop --
                page EpsilonPage() { fn body() -> Html { <div>Epsilon</div> } }
                -- zeta.hop --
                page ZetaPage() { fn body() -> Html { <div>Zeta</div> } }
                -- eta.hop --
                page EtaPage() { fn body() -> Html { <div>Eta</div> } }
                -- theta.hop --
                page ThetaPage() { fn body() -> Html { <div>Theta</div> } }
            "#},
            expect![[r#"
                -- alpha.hop --
                page AlphaPage() { fn body() -> Html { <div>Alpha</div> } }
                -- beta.hop --
                page BetaPage() { fn body() -> Html { <div>Beta</div> } }
                -- delta.hop --
                page DeltaPage() { fn body() -> Html { <div>Delta</div> } }
                -- dist/public/styles-00000000.css --
                -- epsilon.hop --
                page EpsilonPage() { fn body() -> Html { <div>Epsilon</div> } }
                -- eta.hop --
                page EtaPage() { fn body() -> Html { <div>Eta</div> } }
                -- gamma.hop --
                page GammaPage() { fn body() -> Html { <div>Gamma</div> } }
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "output.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- output.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function AlphaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Alpha</div></body></html>";
                    return output;
                }

                export function BetaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Beta</div></body></html>";
                    return output;
                }

                export function DeltaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Delta</div></body></html>";
                    return output;
                }

                export function EpsilonPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Epsilon</div></body></html>";
                    return output;
                }

                export function EtaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Eta</div></body></html>";
                    return output;
                }

                export function GammaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Gamma</div></body></html>";
                    return output;
                }

                export function ThetaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Theta</div></body></html>";
                    return output;
                }

                export function ZetaPage(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div>Zeta</div></body></html>";
                    return output;
                }
                -- style.css --
                -- theta.hop --
                page ThetaPage() { fn body() -> Html { <div>Theta</div> } }
                -- zeta.hop --
                page ZetaPage() { fn body() -> Html { <div>Zeta</div> } }
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"icon\" href=\"/logo-ffe99b60.svg\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- icons/star.svg --
                <svg>star</svg>
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <></>
                    }
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"icon\" href=\"/static/v1/logo-ffe99b60.svg\"><link";
                    output += " rel=\"stylesheet\" href=\"/static/v1/styles-00000000.css\">";
                    output += "</head><body></body></html>";
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
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <></>
                    }
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
                input_path = "style.css"

                [assets]
                output_dir = "../assets"
                -- hop/main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"icon\" href=\"/logo-ffe99b60.svg\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/icons/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- hop/hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "../assets"
                -- hop/icons/star.svg --
                <svg>star</svg>
                -- hop/logo.svg --
                <svg>logo</svg>
                -- hop/main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {}
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[r#"
                Configuration failed:
                error: missing field `assets`
                  --> hop.toml (line 1, col 1)
                1 | [compile]
                  | ^
            "#]],
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"icon\" href=\"/logo-ffe99b60.svg\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- icons/star.svg --
                <svg>star</svg>
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <link rel="icon" href={asset!("/logo.svg")} />
                    }
                    fn body() -> Html {
                        <img src={asset!("/icons/star.svg")} />
                    }
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <>
                          <img src={asset!("/images/a.svg")} />
                          <img src={asset!("/images/b.svg")} />
                      </>
                  }
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
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/images/a-d8c00d88.svg\">";
                    output += "<img src=\"/images/b-d8c00d88.svg\"></body></html>";
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- images/a.svg --
                <svg>same</svg>
                -- images/b.svg --
                <svg>same</svg>
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <>
                          <img src={asset!("/images/a.svg")} />
                          <img src={asset!("/images/b.svg")} />
                      </>
                  }
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
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div class=\"text-red-500\">hi</div></body></html>";
                    return output;
                }
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
                }
                -- style.css --
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn page_with_explicit_head_and_body_compiles() {
        check(
            indoc! {r#"
                -- style.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <title>My page</title>
                    }
                    fn body() -> Html {
                        <div class="text-red-500">hi</div>
                    }
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><title>My page</title>";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><div class=\"text-red-500\">hi</div></body></html>";
                    return output;
                }
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                    fn head() -> Html {
                        <title>My page</title>
                    }
                    fn body() -> Html {
                        <div class="text-red-500">hi</div>
                    }
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
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link";
                    output += " rel=\"stylesheet\" href=\"/static/v1/styles-00000000.css\">";
                    output += "</head><body><div class=\"text-red-500\">hi</div></body>";
                    output += "</html>";
                    return output;
                }
                -- dist/public/styles-00000000.css --
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
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
        // <head> of every page (mirroring how the Tailwind CSS <link> works).
        check(
            indoc! {r#"
                -- app.ts --
                let color: String = "red";
                document.querySelector('body').style.background = color;
                -- style.css --
                -- hop.toml --
                [js]
                input_path = "app.ts"

                [compile]
                target = "rust"
                output_path = "app.rs"

                [css]
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
                }
            "#},
            expect![[r#"
                -- app.rs --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                pub struct Home {}

                impl View for Home {
                    fn render(self) -> String {
                        let mut output = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        output.push_str("<!doctype html><html><head><meta charset=\"utf-8\">");
                        output.push_str("<meta content=\"width=device-width, initial-scale=1\"");
                        output.push_str(" name=\"viewport\"><link");
                        output.push_str(" rel=\"stylesheet\" href=\"/static/v1/styles-00000000.css\">");
                        output.push_str("<script type=\"module\" src=\"/static/v1/scripts-27809078.js\">");
                        output.push_str("</script></head>");
                        output.push_str("<body><div class=\"text-red-500\">hi</div></body></html>");
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
                input_path = "app.ts"

                [compile]
                target = "rust"
                output_path = "app.rs"

                [css]
                input_path = "style.css"

                [assets]
                production_prefix = "static/v1"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
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
                input_path = "app.ts"

                [compile]
                target = "ts"
                output_path = "out.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
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
                input_path = "app.ts"

                [compile]
                target = "ts"
                output_path = "out.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div class="text-red-500">hi</div>
                  }
                }
                -- out.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\">";
                    output += "<script type=\"module\" src=\"/scripts-27809078.js\"></script>";
                    output += "</head><body><div class=\"text-red-500\">hi</div></body>";
                    output += "</html>";
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
                }
                -- fonts/inter.woff2 --
                fake-woff2-bytes
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-ca0dd958.css\"></head>";
                    output += "<body><div>hi</div></body></html>";
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    font-family: "Inter";
                    src: --asset("/fonts/inter.woff2") format("woff2");
                }
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
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
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
                }
                -- fonts/inter.woff2 --
                fake-woff2-bytes
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\"><link";
                    output += " rel=\"stylesheet\" href=\"/static/v1/styles-25fd2426.css\">";
                    output += "</head><body><div>hi</div></body></html>";
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
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    src: --asset("/fonts/missing.woff2");
                }
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
                }
            "#},
            expect![[r#"
                Compilation failed:
                error: asset `fonts/missing.woff2` was not found
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @font-face {
                    src: --asset(var(--x));
                }
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @import "tailwindcss";
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
                }
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-dd9995d1.css\"></head>";
                    output += "<body><div>hi</div></body></html>";
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
                input_path = "input.css"

                [assets]
                output_dir = "dist/public"
                -- input.css --
                @import "tailwindcss";
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <div>hi</div>
                  }
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
                input_path = "style.css"

                [assets]
                production_prefix = "/assets"
                output_dir = "dist/public"
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <img src={asset!("/logo.svg")} />
                  }
                }
                -- logo.svg --
                <svg>logo</svg>
            "#},
            expect![[r#"
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/assets/styles-00000000.css\">";
                    output += "</head><body><img src=\"/assets/logo-ffe99b60.svg\"></body>";
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
                input_path = "style.css"

                [assets]
                production_prefix = "/assets"
                output_dir = "dist/public"
                -- logo.svg --
                <svg>logo</svg>
                -- main.hop --
                page Home() {
                  fn body() -> Html {
                      <img src={asset!("/logo.svg")} />
                  }
                }
                -- style.css --
            "#]],
        )
    }
}
