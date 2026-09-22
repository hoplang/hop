use crate::project::Project;
use anyhow::Result;
use hop_core::{
    AssetPath, AssetPathRewriter, AssetReference, Diagnostic, DiagnosticSeverity,
    DocumentAnnotator, Program,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};
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
            program.update_hop_document(&document_id, document);
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
                            .root()
                            .asset_path_to_path(asset_ref.path())
                            .is_file()
                    })
                    .map(AssetReference::not_found),
            );
        }

        if diagnostics
            .iter()
            .any(|d| d.severity() == DiagnosticSeverity::Error)
        {
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

    // Collect all referenced assets and compute their hashed output names.
    let asset_paths: BTreeSet<AssetPath> = program
        .asset_references()
        .values()
        .flatten()
        .map(|r| r.path().clone())
        .collect();
    let hashed_filenames = compute_hashed_filenames(&asset_paths, project)?;

    let filename_replacements: HashMap<AssetPath, String> = hashed_filenames
        .iter()
        .map(|(asset_path, filename)| {
            let url = match &production_prefix {
                Some(p) => format!("/{}/{}", p.trim_matches('/'), filename),
                None => format!("/{}", filename),
            };
            (asset_path.clone(), url)
        })
        .collect();
    let asset_path_rewriter: Arc<dyn AssetPathRewriter> =
        Arc::new(move |asset_path: &AssetPath| filename_replacements[asset_path].clone());

    let mut css_output = String::new();

    // Run Tailwind on the optimized IR (only classes that survived dead code removal)
    //
    // TODO: Make compile_css_document bundle CSS
    if let Some(css_input) = config.css_input_path().map_err(annotated_config_error)? {
        let compiled_css = program
            .compile_css_document(&css_input, asset_path_rewriter.clone())
            .ok_or_else(|| anyhow::anyhow!("CSS document '{}' not found", css_input))?;
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
        Some(js_input) => {
            let input_path = project.root().document_id_to_path(&js_input);
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
        Some(asset_path_rewriter.clone()),
    );

    // Preserve the file's mtime if the content is unchanged, so downstream
    // build tools (e.g. cargo) don't trigger unnecessary recompiles.
    let output_path = project
        .root()
        .as_path()
        .join(config.output_path().map_err(annotated_config_error)?);
    if !fs::read(&output_path).is_ok_and(|existing| existing == generated_code.as_bytes()) {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&output_path, &generated_code)?;
    }

    // Copy assets with hashed filenames
    copy_assets(project, &assets_output_dir, &hashed_filenames)?;

    // Write CSS file
    let css_dest = project
        .root()
        .as_path()
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
            .root()
            .as_path()
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

/// Replace every run of characters outside `[A-Za-z0-9._-]` with a single
/// `-`, so the result is safe to use unencoded in a URL and as a filename on
/// any filesystem.
fn sanitize_filename_part(part: &str) -> String {
    let mut out = String::with_capacity(part.len());
    let mut pending_dash = false;
    for c in part.chars() {
        if c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-') {
            if pending_dash && !out.is_empty() {
                out.push('-');
            }
            pending_dash = false;
            out.push(c);
        } else {
            pending_dash = true;
        }
    }
    out
}

/// Derive the output filename for an asset: the sanitized stem, a content
/// hash, and the sanitized extension, e.g. `My Logo.svg` -> `My-Logo-<hash>.svg`.
fn hashed_output_filename(filename: &str, hash: &str) -> String {
    let p = Path::new(filename);
    let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
    let stem = match sanitize_filename_part(stem) {
        s if s.is_empty() => "asset".to_string(),
        s => s,
    };
    let ext = p
        .extension()
        .and_then(|e| e.to_str())
        .map(sanitize_filename_part)
        .filter(|e| !e.is_empty());
    match ext {
        Some(ext) => format!("{stem}-{hash}.{ext}"),
        None => format!("{stem}-{hash}"),
    }
}

/// Compute the output filename for each asset.
///
/// Output names are flat: the source directory structure is dropped and the
/// file name gets a content hash inserted before its extension, so
/// `icons/star.svg` and `../shared/star.svg` both become `star-<hash>.svg`.
/// Characters that are unsafe in URLs or filenames are replaced by `-`.
/// Two assets with the same name and the same content share one output file.
fn compute_hashed_filenames(
    asset_paths: &BTreeSet<AssetPath>,
    project: &Project,
) -> Result<BTreeMap<AssetPath, String>> {
    let mut hashed_filenames = BTreeMap::new();
    for asset_path in asset_paths {
        let full_path = project.root().asset_path_to_path(asset_path);

        let bytes = fs::read(&full_path).map_err(|e| {
            anyhow::anyhow!("Failed to read asset '{}' for hashing: {}", asset_path, e)
        })?;

        let hash = format!("{:08x}", crc32fast::hash(&bytes));
        hashed_filenames.insert(
            asset_path.clone(),
            hashed_output_filename(asset_path.file_name(), &hash),
        );
    }

    Ok(hashed_filenames)
}

fn copy_assets(
    project: &Project,
    output_dir: &str,
    hashed_filenames: &BTreeMap<AssetPath, String>,
) -> Result<()> {
    let dest_root = project.root().as_path().join(output_dir);

    fs::create_dir_all(&dest_root).map_err(|e| {
        anyhow::anyhow!(
            "Failed to create assets output directory {:?}: {}",
            dest_root,
            e
        )
    })?;

    for (asset_path, hashed_filename) in hashed_filenames {
        let src = project.root().asset_path_to_path(asset_path);
        let dst = dest_root.join(hashed_filename);

        fs::copy(&src, &dst).map_err(|e| {
            anyhow::anyhow!("Failed to copy asset '{}' to {:?}: {}", asset_path, dst, e)
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
                    output += "<body><img src=\"/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/star-890d8c02.svg --
                <svg>star</svg>
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
                -- assets/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- assets/star-890d8c02.svg --
                <svg>star</svg>
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
                    output += "<body><img src=\"/star-890d8c02.svg\"></body></html>";
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
                    output += "<body><img src=\"/star-890d8c02.svg\"></body></html>";
                    return output;
                }
                -- dist/public/logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/star-890d8c02.svg --
                <svg>star</svg>
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
                    output += "<body>";
                    output += "<img src=\"/a-d8c00d88.svg\"><img src=\"/b-d8c00d88.svg\">";
                    output += "</body></html>";
                    return output;
                }
                -- dist/public/a-d8c00d88.svg --
                <svg>same</svg>
                -- dist/public/b-d8c00d88.svg --
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
    fn asset_outside_project_root() {
        // `/../` climbs above the project root. The asset is still hashed and
        // copied flat into the output dir.
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
                output_dir = "dist/public"
                -- hop/main.hop --
                page Home() {
                    fn body() -> Html {
                        <img src={asset!("/../shared/logo.svg")} />
                    }
                }
                -- shared/logo.svg --
                <svg>shared</svg>
            "#},
            expect![[r#"
                -- hop/app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-00000000.css\"></head>";
                    output += "<body><img src=\"/logo-87e808bf.svg\"></body></html>";
                    return output;
                }
                -- hop/dist/public/logo-87e808bf.svg --
                <svg>shared</svg>
                -- hop/dist/public/styles-00000000.css --
                -- hop/hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [css]
                input_path = "style.css"

                [assets]
                output_dir = "dist/public"
                -- hop/main.hop --
                page Home() {
                    fn body() -> Html {
                        <img src={asset!("/../shared/logo.svg")} />
                    }
                }
                -- hop/style.css --
                -- shared/logo.svg --
                <svg>shared</svg>
            "#]],
        )
    }

    #[test]
    fn output_filenames_are_sanitized() {
        assert_eq!(
            hashed_output_filename("logo.svg", "abcd1234"),
            "logo-abcd1234.svg"
        );
        assert_eq!(
            hashed_output_filename("My Logo.svg", "abcd1234"),
            "My-Logo-abcd1234.svg"
        );
        assert_eq!(
            hashed_output_filename("Inter Variable (v3).woff2", "abcd1234"),
            "Inter-Variable-v3-abcd1234.woff2"
        );
        assert_eq!(
            hashed_output_filename("ünicode#1.svg", "abcd1234"),
            "nicode-1-abcd1234.svg"
        );
        assert_eq!(
            hashed_output_filename("README", "abcd1234"),
            "README-abcd1234"
        );
        assert_eq!(hashed_output_filename(" ", "abcd1234"), "asset-abcd1234");
        assert_eq!(
            hashed_output_filename(".hidden.svg", "abcd1234"),
            ".hidden-abcd1234.svg"
        );
    }

    #[test]
    #[ignore]
    fn asset_with_spaces_in_name() {
        // Spaces are allowed in the source path. The output filename (and
        // hence the URL) is sanitized so it needs no encoding.
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
                    src: --asset("/fonts/Inter Variable.woff2") format("woff2");
                }
                -- main.hop --
                page Home() {
                    fn body() -> Html {
                        <img src={asset!("/My Logo.svg")} />
                    }
                }
                -- My Logo.svg --
                <svg>logo</svg>
                -- fonts/Inter Variable.woff2 --
                fake-woff2-bytes
            "#},
            expect![[r#"
                -- My Logo.svg --
                <svg>logo</svg>
                -- app.ts --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Home(): string {
                    let output: string = "";
                    output += "<!doctype html><html><head><meta charset=\"utf-8\">";
                    output += "<meta content=\"width=device-width, initial-scale=1\"";
                    output += " name=\"viewport\">";
                    output += "<link rel=\"stylesheet\" href=\"/styles-79463e4b.css\"></head>";
                    output += "<body><img src=\"/My-Logo-ffe99b60.svg\"></body></html>";
                    return output;
                }
                -- dist/public/Inter-Variable-1c757f7b.woff2 --
                fake-woff2-bytes
                -- dist/public/My-Logo-ffe99b60.svg --
                <svg>logo</svg>
                -- dist/public/styles-79463e4b.css --
                @font-face{font-family:Inter;src:url(/Inter-Variable-1c757f7b.woff2)format("woff2")}
                -- fonts/Inter Variable.woff2 --
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
                    src: --asset("/fonts/Inter Variable.woff2") format("woff2");
                }
                -- main.hop --
                page Home() {
                    fn body() -> Html {
                        <img src={asset!("/My Logo.svg")} />
                    }
                }
            "#]],
        )
    }

    #[test]
    #[ignore]
    fn asset_missing_outside_project_root() {
        check_error(
            indoc! {r#"
                -- hop/hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"

                [assets]
                output_dir = "dist/public"
                -- hop/main.hop --
                page Home() {
                    fn body() -> Html {
                        <img src={asset!("/../shared/missing.svg")} />
                    }
                }
            "#},
            expect![[r#"
                Compilation failed:
                error: asset `../shared/missing.svg` was not found
                  --> main.hop (line 3, col 19)
                2 |     fn body() -> Html {
                3 |         <img src={asset!("/../shared/missing.svg")} />
                  |                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                    output += "<link rel=\"stylesheet\" href=\"/styles-779fe409.css\"></head>";
                    output += "<body><div>hi</div></body></html>";
                    return output;
                }
                -- dist/public/inter-1c757f7b.woff2 --
                fake-woff2-bytes
                -- dist/public/styles-779fe409.css --
                @font-face{font-family:Inter;src:url(/inter-1c757f7b.woff2)format("woff2")}
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
                    output += " rel=\"stylesheet\" href=\"/static/v1/styles-19d4bb7c.css\">";
                    output += "</head><body><div>hi</div></body></html>";
                    return output;
                }
                -- dist/public/inter-1c757f7b.woff2 --
                fake-woff2-bytes
                -- dist/public/styles-19d4bb7c.css --
                @font-face{font-family:Inter;src:url(/static/v1/inter-1c757f7b.woff2)format("woff2")}
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
