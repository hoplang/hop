use anyhow::{Context, Result, bail};
use std::{path::Path, process};
use tracing::instrument;

/// Bundle a JS/TS file using esbuild, returning the bundled JS as a string.
///
/// - `path`: Path to the entry file (e.g. `src/app.ts`)
/// - `minify`: If true, pass `--minify` to esbuild (used in production builds)
#[instrument(name = "esbuild::bundle_script", skip(path, minify))]
pub fn bundle_script(path: &Path, minify: bool) -> Result<String> {
    let path_str = path.display().to_string();
    let mut args = vec![path_str.as_str(), "--bundle", "--format=esm"];

    if minify {
        args.push("--minify");
    }

    tracing::info!(target: "exec", cmd = "esbuild", args = ?args);

    let output = process::Command::new("esbuild")
        .args(&args)
        .output()
        .with_context(|| "Failed to run esbuild. Is esbuild installed and on your PATH?")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("esbuild failed:\n{}", stderr);
    }

    let js = String::from_utf8(output.stdout).context("esbuild output is not valid UTF-8")?;

    Ok(js)
}
