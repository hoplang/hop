use anyhow::Context;
use files::ProjectRoot;
use std::fs;
use std::path::Path;

use crate::common::HopMode;
use crate::compiler::compile;
use crate::files;
use crate::timing;

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
    use std::collections::HashMap;

    fs::create_dir_all(output_dir)?;

    let mut timer = timing::TimingCollector::new();

    timer.start_phase("loading modules");
    let modules = files::load_all_hop_modules(root)?;

    timer.start_phase("compiling");
    let program = compile(modules, HopMode::Build)
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;

    timer.start_phase("rendering");
    let mut rendered_files = HashMap::new();
    for file_path in program.get_render_file_paths() {
        let content = program
            .render_file(&file_path)
            .map_err(|e| anyhow::anyhow!("Failed to render file '{}': {}", file_path, e))?;
        rendered_files.insert(file_path, content);
    }

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
        fs::write(&script_path, combined_script)
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
    use simple_txtar::Archive;
    use std::{env, fs};

    fn temp_dir_from_txtar(archive: &str) -> std::io::Result<std::path::PathBuf> {
        let r = rand::random::<u64>();
        let temp_dir = env::temp_dir().join(format!("hop_test_{}", r));
        fs::create_dir_all(&temp_dir)?;
        for file in Archive::from(archive).iter() {
            let file_path = temp_dir.join(&file.name);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&file_path, &file.content)?;
        }
        Ok(temp_dir)
    }

    /// When the user calls `hop build` with a build.hop file, the rendered content
    /// should be written to the specified output files.
    #[test]
    fn test_build_with_hop_file() {
        let dir = temp_dir_from_txtar(
            r#"
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
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let result = execute(&root, &dir.join("out"), None, None);
        assert!(result.is_ok());
        let outputs = result.unwrap();
        assert_eq!(outputs.len(), 1);

        // Check that the output file was created with the correct content
        let output_path = dir.join("out/foo/bar/index.html");
        let content = std::fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("Build.hop Test"));
        assert!(content.contains("This content comes from a build.hop file"));
    }

    /// When the user calls `hop build` the global HOP_MODE variable should
    /// be set to 'build'.
    #[test]
    fn test_build_has_hop_mode_build() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
  mode: {HOP_MODE}
</render>
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let result = execute(&root, &dir.join("out"), None, None);
        assert!(result.is_ok());
        let outputs = result.unwrap();
        assert_eq!(outputs.len(), 1);

        // Check that the output file was created with the correct content
        let output_path = dir.join("out").join("index.html");
        let content = std::fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("mode: build"));
    }

    /// When the user calls `hop build` with a staticdir parameter, files should be copied
    /// from the static directory to the output directory.
    #[test]
    fn test_build_with_staticdir() {
        let dir = temp_dir_from_txtar(
            r#"
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
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();
        let output_dir = dir.join("out");
        let static_dir = dir.join("static");

        let result = execute(&root, &output_dir, None, Some(static_dir.to_str().unwrap()));
        assert!(result.is_ok());
        let outputs = result.unwrap();

        // Should have 4 files: index.html + 3 static files
        assert_eq!(outputs.len(), 4);

        // Check that all files were copied correctly
        let index_path = output_dir.join("index.html");
        let style_path = output_dir.join("style.css");
        let script_path = output_dir.join("script.js");
        let image_path = output_dir.join("images").join("logo.png");

        assert!(index_path.exists());
        assert!(style_path.exists());
        assert!(script_path.exists());
        assert!(image_path.exists());

        // Verify content
        let index_content = std::fs::read_to_string(&index_path).unwrap();
        assert!(index_content.contains("Hello from build.hop!"));

        let style_content = std::fs::read_to_string(&style_path).unwrap();
        assert!(style_content.contains("body { color: red; }"));

        let script_content = std::fs::read_to_string(&script_path).unwrap();
        assert!(script_content.contains("console.log('hello world');"));

        let image_content = std::fs::read_to_string(&image_path).unwrap();
        assert!(image_content.contains("fake image data"));
    }
}
