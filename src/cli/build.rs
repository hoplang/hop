use crate::filesystem::files::ProjectRoot;
use crate::hop::server::HopMode;
use anyhow::Context;
use rayon::prelude::*;
use std::fs;
use std::path::Path;

use crate::filesystem::files;
use crate::hop::compiler::compile;
use crate::tui::timing;

fn copy_dir_recursive(
    src: &Path,
    dst: &Path,
    file_outputs: &mut Vec<(String, usize)>,
) -> anyhow::Result<()> {
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();
        let file_name = entry.file_name();
        let dst_path = dst.join(&file_name);

        if src_path.is_dir() {
            fs::create_dir_all(&dst_path)?;
            copy_dir_recursive(&src_path, &dst_path, file_outputs)?;
        } else {
            if let Some(parent) = dst_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::copy(&src_path, &dst_path).with_context(|| {
                format!(
                    "Failed to copy file from {} to {}",
                    src_path.display(),
                    dst_path.display()
                )
            })?;

            let file_size = fs::metadata(&dst_path)?.len() as usize;
            file_outputs.push((format!("{}", dst_path.to_string_lossy()), file_size));
        }
    }
    Ok(())
}

pub fn execute(
    root: &ProjectRoot,
    output_dir: &Path,
    script_file: Option<&str>,
    static_dir: Option<&str>,
) -> anyhow::Result<Vec<(String, usize)>> {
    fs::create_dir_all(output_dir)?;

    let mut timer = timing::TimingCollector::new();

    timer.start_phase("loading modules");
    let modules = files::load_all_hop_modules(root)?;

    timer.start_phase("compiling");
    let program = compile(modules, HopMode::Build)
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;

    timer.start_phase("rendering");
    let rendered_files = program
        .get_render_file_paths()
        .into_par_iter()
        .map(|file_path| {
            let content = program
                .render_file(&file_path)
                .map_err(|e| anyhow::anyhow!("Failed to render file '{}': {}", file_path, e))?;
            Ok::<_, anyhow::Error>((file_path, content))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut file_outputs = Vec::new();

    timer.start_phase("writing files");
    for (file_path, content) in rendered_files {
        let output_path = output_dir.join(&file_path);
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(&output_path, &content)
            .with_context(|| format!("Failed to write file {}", output_path.display()))?;

        file_outputs.push((format!("{}", output_path.to_string_lossy()), content.len()));
    }

    if let Some(script_file_name) = script_file {
        let combined_script = program.get_scripts();
        let script_path = output_dir.join(script_file_name);
        fs::write(&script_path, &combined_script)
            .with_context(|| format!("Failed to write script file {}", script_path.display()))?;
        file_outputs.push((
            format!("{}", script_path.to_string_lossy()),
            combined_script.len(),
        ));
    }

    if let Some(static_dir_str) = static_dir {
        timer.start_phase("copying static files");
        let static_path = Path::new(static_dir_str);
        if !static_path.exists() {
            anyhow::bail!("staticdir '{}' does not exist", static_path.display());
        }
        if !static_path.is_dir() {
            anyhow::bail!("staticdir '{}' is not a directory", static_path.display());
        }

        copy_dir_recursive(static_path, output_dir, &mut file_outputs)
            .with_context(|| format!("Failed to copy files from {}", static_path.display()))?;
        timer.end_phase();
    }

    timer.print();

    Ok(file_outputs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::{archive_from_dir, temp_dir_from_archive};
    use indoc::indoc;
    use simple_txtar::Archive;

    /// When the user calls `hop build` with a build.hop file, the rendered content
    /// should be written to the specified output files.
    #[test]
    fn test_build_with_hop_file() {
        use expect_test::expect;

        let archive = Archive::from(indoc! {r#"
            -- src/components.hop --
            <hello-world>
              <h1>Build.hop Test</h1>
              <p>This content comes from a build.hop file.</p>
            </hello-world>
            -- build.hop --
            <import component="hello-world" from="src/components" />

            <render file="foo/bar/index.html">
              <hello-world />
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let result = execute(&root, &dir.join("out"), None, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1);

        let archive_string = archive_from_dir(&dir.join("out")).unwrap().to_string();

        expect![[r#"
            -- foo/bar/index.html --

              <div data-hop-id="src/components/hello-world">
              <h1>Build.hop Test</h1>
              <p>This content comes from a build.hop file.</p>
            </div>
        "#]]
        .assert_eq(&archive_string);
    }

    /// When the user calls `hop build` the global HOP_MODE variable should
    /// be set to 'build'.
    #[test]
    fn test_build_has_hop_mode_build() {
        use expect_test::expect;

        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              mode: {HOP_MODE}
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();
        let output_dir = &dir.join("out");

        let result = execute(&root, output_dir, None, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1);

        let archive_string = archive_from_dir(output_dir).unwrap().to_string();

        expect![[r#"
            -- index.html --

              mode: build
        "#]]
        .assert_eq(&archive_string);
    }

    /// When the user calls `hop build` with a staticdir parameter, files should be copied
    /// from the static directory to the output directory.
    #[test]
    fn test_build_with_staticdir() {
        use expect_test::expect;

        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              Hello from build.hop!
            </render>
            -- static/style.css --
            body { color: red; }
            -- static/script.js --
            console.log('hello world');
            -- static/images/logo.png --
            fake image data
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();
        let output_dir = &dir.join("out");
        let static_dir = dir.join("static");

        let result = execute(&root, output_dir, None, Some(static_dir.to_str().unwrap()));
        assert!(result.is_ok());

        assert_eq!(result.unwrap().len(), 4);

        let archive_string = archive_from_dir(output_dir).unwrap().to_string();

        expect![[r#"
            -- images/logo.png --
            fake image data
            -- index.html --

              Hello from build.hop!
            -- script.js --
            console.log('hello world');
            -- style.css --
            body { color: red; }
        "#]]
        .assert_eq(&archive_string);
    }
}
