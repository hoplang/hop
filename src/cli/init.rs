use anyhow::Context;
use std::fs;
use std::path::Path;

const BUILD_HOP_TEMPLATE: &str = r#"<welcome-message>
  <div class="container mx-auto px-4 py-16">
    <h1 class="text-2xl text-gray-800 mb-8 text-center">
      hop
    </h1>
    <div class="bg-white rounded shadow p-6 max-w-sm mx-auto">
	  Edit build.hop to see your changes reflected here
    </div>
  </div>
</welcome-message>

<render file="index.html">
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Welcome to Hop</title>
      <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
    </head>
    <body class="bg-gradient-to-br from-blue-50 to-indigo-100 min-h-screen">
      <welcome-message />
    </body>
  </html>
</render>
"#;

pub fn execute(path: &Path) -> anyhow::Result<()> {
    let build_file = path.join("build.hop");

    if build_file.exists() {
        anyhow::bail!("build.hop already exists in {:?}", path);
    }

    fs::write(&build_file, BUILD_HOP_TEMPLATE)
        .with_context(|| format!("Failed to write build.hop to {:?}", build_file))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    fn create_test_dir() -> std::io::Result<std::path::PathBuf> {
        let r = rand::random::<u64>();
        let temp_dir = env::temp_dir().join(format!("hop_init_test_{}", r));
        fs::create_dir_all(&temp_dir)?;
        Ok(temp_dir)
    }

    #[test]
    fn test_init_creates_build_file() {
        let temp_dir = create_test_dir().unwrap();

        // Run init
        let result = execute(&temp_dir);
        assert!(result.is_ok());

        // Check that build.hop was created
        let build_file = temp_dir.join("build.hop");
        assert!(build_file.exists());

        // Check the content
        let content = fs::read_to_string(&build_file).unwrap();
        assert!(content.contains("<welcome-message>"));
        assert!(content.contains("<render file=\"index.html\">"));
        assert!(content.contains("<!DOCTYPE html>"));
        assert!(content.contains("<html lang=\"en\">"));
        assert!(content.contains("<head>"));
        assert!(content.contains("<title>Welcome to Hop</title>"));
        assert!(content.contains("@tailwindcss/browser"));
        assert!(content.contains("<welcome-message />"));

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_init_fails_if_build_file_exists() {
        let temp_dir = create_test_dir().unwrap();

        // Create an existing build.hop
        let build_file = temp_dir.join("build.hop");
        fs::write(&build_file, "existing content").unwrap();

        // Run init - should fail
        let result = execute(&temp_dir);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("already exists"));

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
