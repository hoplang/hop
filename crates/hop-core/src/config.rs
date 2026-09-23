use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::Document;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_file_path::RootRelativeFilePath;
use crate::root_relative_path::RootRelativePath;
use serde::Deserialize;

/// The parsed contents of a `hop.toml` file.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Config {
    css: Option<CssSection>,
    js: Option<JsSection>,
    assets: Option<AssetsSection>,
    compile: Option<CompileSection>,
}

impl Config {
    /// Parse the contents of a `hop.toml` file.
    pub fn parse(document: &Document) -> Result<Config, Diagnostic> {
        toml::from_str(document.as_str()).map_err(|err| {
            Diagnostic {
                message: err.message().to_string(),
                // A zero-width span is kept as a position marker, toml reports
                // one for "expected X here" and for a missing top-level section.
                range: document.range(err.span().unwrap_or(0..0)),
                severity: DiagnosticSeverity::Error,
            }
        })
    }

    /// Path to the CSS entrypoint.
    pub fn css_input_path(&self) -> Option<&RootContainedFilePath> {
        self.css.as_ref()?.input_path.as_ref()
    }

    /// Path to the JS/TS entrypoint.
    pub fn js_input_path(&self) -> Option<&RootContainedFilePath> {
        self.js.as_ref()?.input_path.as_ref()
    }

    /// Directory to copy all `asset!()` referenced files into during `hop build`.
    pub fn assets_output_dir(&self) -> Option<&RootRelativePath> {
        self.assets.as_ref()?.output_dir.as_ref()
    }

    /// A production prefix that should be prepended to all asset URLs via the
    /// [AssetPathRewriter](crate::AssetPathRewriter) during compilation.
    ///
    /// Leading and trailing slashes are trimmed.
    pub fn assets_production_prefix(&self) -> Option<&str> {
        self.assets.as_ref()?.production_prefix.as_deref()
    }

    /// The language to compile to.
    pub fn compile_target(&self) -> Option<TargetLanguage> {
        self.compile.as_ref()?.target
    }

    /// The path to the compiled output file.
    pub fn compile_output_path(&self) -> Option<&RootRelativeFilePath> {
        self.compile.as_ref()?.output_path.as_ref()
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct CompileSection {
    target: Option<TargetLanguage>,
    #[serde(default, deserialize_with = "deserialize_compile_output_path")]
    output_path: Option<RootRelativeFilePath>,
}

/// The target language for compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum TargetLanguage {
    #[serde(rename = "ts")]
    Typescript,
    #[serde(rename = "rust")]
    Rust,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct CssSection {
    #[serde(default, deserialize_with = "deserialize_css_input_path")]
    input_path: Option<RootContainedFilePath>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct JsSection {
    #[serde(default, deserialize_with = "deserialize_js_input_path")]
    input_path: Option<RootContainedFilePath>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct AssetsSection {
    #[serde(default, deserialize_with = "deserialize_production_prefix")]
    production_prefix: Option<String>,

    #[serde(default, deserialize_with = "deserialize_assets_output_dir")]
    output_dir: Option<RootRelativePath>,
}

fn deserialize_css_input_path<'de, D>(
    deserializer: D,
) -> Result<Option<RootContainedFilePath>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let path = String::deserialize(deserializer)?;
    RootContainedFilePath::new(&path)
        .map(Some)
        .map_err(|err| serde::de::Error::custom(format!("css.input_path: {err}")))
}

fn deserialize_js_input_path<'de, D>(
    deserializer: D,
) -> Result<Option<RootContainedFilePath>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let path = String::deserialize(deserializer)?;
    RootContainedFilePath::new(&path)
        .map(Some)
        .map_err(|err| serde::de::Error::custom(format!("js.input_path: {err}")))
}

fn deserialize_production_prefix<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let production_prefix = String::deserialize(deserializer)?;
    let trimmed = production_prefix.trim_matches('/');
    if trimmed.is_empty() {
        return Err(serde::de::Error::custom(
            "assets.production_prefix must be non-empty (omit the field to leave asset paths untouched)",
        ));
    }
    Ok(Some(trimmed.to_string()))
}

fn deserialize_assets_output_dir<'de, D>(
    deserializer: D,
) -> Result<Option<RootRelativePath>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let path = String::deserialize(deserializer)?;
    RootRelativePath::new(&path)
        .map(Some)
        .map_err(|err| serde::de::Error::custom(format!("assets.output_dir: {err}")))
}

fn deserialize_compile_output_path<'de, D>(
    deserializer: D,
) -> Result<Option<RootRelativeFilePath>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let path = String::deserialize(deserializer)?;
    RootRelativeFilePath::new(&path)
        .map(Some)
        .map_err(|err| serde::de::Error::custom(format!("compile.output_path: {err}")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::expect;
    use indoc::indoc;

    fn parse(toml_str: &str) -> Result<Config, Diagnostic> {
        Config::parse(&Document::new(
            RootContainedFilePath::new("hop.toml").unwrap(),
            toml_str.to_string(),
        ))
    }

    fn config(toml_str: &str) -> Config {
        parse(toml_str).expect("config should parse")
    }

    fn error(toml_str: &str) -> String {
        DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .with_location()
            .annotate([parse(toml_str).expect_err("config should not parse")])
            .render()
    }

    #[test]
    fn accepts_config_with_typescript_target() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#};
        let config = config(toml_str);
        assert_eq!(config.compile_target(), Some(TargetLanguage::Typescript));
        assert_eq!(
            config
                .compile_output_path()
                .map(RootRelativeFilePath::as_str),
            Some("app.ts")
        );
    }

    #[test]
    fn accepts_config_with_css_block() {
        let toml_str = indoc! {r#"
            [css]
            input_path = "styles/input.css"

            [compile]
            target = "ts"
            output_path = "app.ts"
        "#};
        assert_eq!(
            config(toml_str).css_input_path(),
            Some(&RootContainedFilePath::new("styles/input.css").unwrap())
        );
    }

    #[test]
    fn rejects_bundler_key_in_css_section() {
        let toml_str = indoc! {r#"
            [css]
            bundler = "tailwind_4"
            input_path = "styles/input.css"
        "#};
        expect![[r#"
            error: unknown field `bundler`, expected `input_path`
              --> hop.toml (line 2, col 1)
            1 | [css]
            2 | bundler = "tailwind_4"
              | ^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_css_input_path_that_is_not_a_document_id() {
        let toml_str = indoc! {r#"
            [css]
            input_path = "../styles/input.css"
        "#};
        expect![[r#"
            error: css.input_path: path must not point outside the project root
              --> hop.toml (line 2, col 14)
            1 | [css]
            2 | input_path = "../styles/input.css"
              |              ^^^^^^^^^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn normalizes_css_input_path() {
        let toml_str = indoc! {r#"
            [css]
            input_path = "./styles/input.css"
        "#};
        assert_eq!(
            config(toml_str).css_input_path(),
            Some(&RootContainedFilePath::new("styles/input.css").unwrap())
        );
    }

    #[test]
    fn accepts_css_block_without_input_path() {
        let toml_str = indoc! {r#"
            [css]
        "#};
        assert_eq!(config(toml_str).css_input_path(), None);
    }

    #[test]
    fn accepts_css_block_without_compile_section() {
        let toml_str = indoc! {r#"
            [css]
            input_path = "styles/input.css"
        "#};
        assert_eq!(
            config(toml_str).css_input_path(),
            Some(&RootContainedFilePath::new("styles/input.css").unwrap())
        );
    }

    #[test]
    fn accepts_config_with_js_block() {
        let toml_str = indoc! {r#"
            [js]
            input_path = "src/app.ts"

            [compile]
            target = "ts"
            output_path = "app.ts"
        "#};
        assert_eq!(
            config(toml_str).js_input_path(),
            Some(&RootContainedFilePath::new("src/app.ts").unwrap())
        );
    }

    #[test]
    fn rejects_bundler_key_in_js_section() {
        let toml_str = indoc! {r#"
            [js]
            bundler = "esbuild"
            input_path = "src/app.ts"
        "#};
        expect![[r#"
            error: unknown field `bundler`, expected `input_path`
              --> hop.toml (line 2, col 1)
            1 | [js]
            2 | bundler = "esbuild"
              | ^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_absolute_js_input_path() {
        let toml_str = indoc! {r#"
            [js]
            input_path = "/src/app.ts"
        "#};
        expect![[r#"
            error: js.input_path: path must not start with '/'
              --> hop.toml (line 2, col 14)
            1 | [js]
            2 | input_path = "/src/app.ts"
              |              ^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_js_block_without_input_path() {
        let toml_str = indoc! {r#"
            [js]
        "#};
        assert_eq!(config(toml_str).js_input_path(), None);
    }

    #[test]
    fn rejects_config_with_unknown_field() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
            output_path = "app.ts"
            unknown_field = "should error"
        "#};
        expect![[r#"
            error: unknown field `unknown_field`, expected `target` or `output_path`
              --> hop.toml (line 4, col 1)
            3 | output_path = "app.ts"
            4 | unknown_field = "should error"
              | ^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_config_with_invalid_target() {
        let toml_str = indoc! {r#"
            [compile]
            target = "invalid"
            output_path = "app.ts"
        "#};
        expect![[r#"
            error: unknown variant `invalid`, expected `ts` or `rust`
              --> hop.toml (line 2, col 10)
            1 | [compile]
            2 | target = "invalid"
              |          ^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn reports_errors_from_sections_the_caller_never_reads() {
        let toml_str = indoc! {r#"
            [css]
            input_path = "styles/input.css"

            [compile]
            target = "ts"
            output_path = "app.ts"
            unknown_field = "should error"
        "#};
        expect![[r#"
            error: unknown field `unknown_field`, expected `target` or `output_path`
              --> hop.toml (line 7, col 1)
            6 | output_path = "app.ts"
            7 | unknown_field = "should error"
              | ^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_all_valid_target_values() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#};
        assert_eq!(
            config(toml_str).compile_target(),
            Some(TargetLanguage::Typescript)
        );

        let toml_str = indoc! {r#"
            [compile]
            target = "rust"
            output_path = "main.rs"
        "#};
        assert_eq!(
            config(toml_str).compile_target(),
            Some(TargetLanguage::Rust)
        );
    }

    #[test]
    fn rejects_full_target_names() {
        let toml_str = indoc! {r#"
            [compile]
            target = "typescript"
            output_path = "app.ts"
        "#};
        expect![[r#"
            error: unknown variant `typescript`, expected `ts` or `rust`
              --> hop.toml (line 2, col 10)
            1 | [compile]
            2 | target = "typescript"
              |          ^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
        let toml_str = indoc! {r#"
            [compile]
            target = "javascript"
            output_path = "app.js"
        "#};
        expect![[r#"
            error: unknown variant `javascript`, expected `ts` or `rust`
              --> hop.toml (line 2, col 10)
            1 | [compile]
            2 | target = "javascript"
              |          ^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
        let toml_str = indoc! {r#"
            [compile]
            target = "js"
            output_path = "app.js"
        "#};
        expect![[r#"
            error: unknown variant `js`, expected `ts` or `rust`
              --> hop.toml (line 2, col 10)
            1 | [compile]
            2 | target = "js"
              |          ^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_empty_config() {
        let config = config("");
        assert_eq!(config.css_input_path(), None);
        assert_eq!(config.js_input_path(), None);
        assert_eq!(config.assets_output_dir(), None);
        assert_eq!(config.assets_production_prefix(), None);
        assert_eq!(config.compile_target(), None);
        assert_eq!(config.compile_output_path(), None);
    }

    #[test]
    fn rejects_absolute_compile_output_path() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
            output_path = "/etc/hop/app.ts"
        "#};
        expect![[r#"
            error: compile.output_path: path must not start with '/'
              --> hop.toml (line 3, col 15)
            2 | target = "ts"
            3 | output_path = "/etc/hop/app.ts"
              |               ^^^^^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_empty_compile_output_path() {
        let toml_str = indoc! {r#"
            [compile]
            output_path = ""
        "#};
        expect![[r#"
            error: compile.output_path: path cannot be empty
              --> hop.toml (line 2, col 15)
            1 | [compile]
            2 | output_path = ""
              |               ^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_compile_output_path_that_names_no_file() {
        let toml_str = indoc! {r#"
            [compile]
            output_path = "dist/.."
        "#};
        expect![[r#"
            error: compile.output_path: path does not name a file
              --> hop.toml (line 2, col 15)
            1 | [compile]
            2 | output_path = "dist/.."
              |               ^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_compile_output_path_above_project_root() {
        let toml_str = indoc! {r#"
            [compile]
            output_path = "../generated/app.ts"
        "#};
        assert_eq!(
            config(toml_str)
                .compile_output_path()
                .map(RootRelativeFilePath::as_str),
            Some("../generated/app.ts")
        );
    }

    #[test]
    fn accepts_compile_section_without_target() {
        let toml_str = indoc! {r#"
            [compile]
            output_path = "app.ts"
        "#};
        let config = config(toml_str);
        assert_eq!(config.compile_target(), None);
        assert_eq!(
            config
                .compile_output_path()
                .map(RootRelativeFilePath::as_str),
            Some("app.ts")
        );
    }

    #[test]
    fn accepts_compile_section_without_output_path() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
        "#};
        let config = config(toml_str);
        assert_eq!(config.compile_target(), Some(TargetLanguage::Typescript));
        assert_eq!(config.compile_output_path(), None);
    }

    #[test]
    fn rejects_unknown_top_level_sections() {
        let toml_str = indoc! {r#"
            [asset]
            output_dir = "dist/public"
        "#};
        expect![[r#"
            error: unknown field `asset`, expected one of `css`, `js`, `assets`, `compile`
              --> hop.toml (line 1, col 2)
            1 | [asset]
              |  ^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_assets_config_with_production_prefix() {
        let toml_str = indoc! {r#"
            [assets]
            production_prefix = "static/v1"
            output_dir = "dist/public"
        "#};
        assert_eq!(
            config(toml_str).assets_production_prefix(),
            Some("static/v1")
        );
    }

    #[test]
    fn production_prefix_is_stored_without_surrounding_slashes() {
        for written in ["/static/v1", "static/v1/", "/static/v1/", "//static/v1//"] {
            let toml_str = format!("[assets]\nproduction_prefix = {written:?}\n");
            assert_eq!(
                config(&toml_str).assets_production_prefix(),
                Some("static/v1"),
                "for production_prefix = {written:?}"
            );
        }
    }

    #[test]
    fn accepts_assets_section_without_production_prefix() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "dist/public"
        "#};
        assert_eq!(config(toml_str).assets_production_prefix(), None);
    }

    #[test]
    fn rejects_unknown_fields_in_assets_section() {
        let toml_str = indoc! {r#"
            [assets]
            production_prefix = "static/v1"
            output_dir = "dist/public"
            unknown_field = "should error"
        "#};
        expect![[r#"
            error: unknown field `unknown_field`, expected `production_prefix` or `output_dir`
              --> hop.toml (line 4, col 1)
            3 | output_dir = "dist/public"
            4 | unknown_field = "should error"
              | ^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_empty_production_prefix() {
        let toml_str = indoc! {r#"
            [assets]
            production_prefix = ""
            output_dir = "dist/public"
        "#};
        expect![[r#"
            error: assets.production_prefix must be non-empty (omit the field to leave asset paths untouched)
              --> hop.toml (line 2, col 21)
            1 | [assets]
            2 | production_prefix = ""
              |                     ^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_assets_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "dist/public"
        "#};
        assert_eq!(
            config(toml_str)
                .assets_output_dir()
                .map(RootRelativePath::as_str),
            Some("dist/public")
        );
    }

    #[test]
    fn accepts_config_without_assets_section() {
        let toml_str = indoc! {r#"
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#};
        assert_eq!(config(toml_str).assets_output_dir(), None);
    }

    #[test]
    fn accepts_assets_section_without_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            production_prefix = "static/v1"
        "#};
        let config = config(toml_str);
        assert_eq!(config.assets_output_dir(), None);
        assert_eq!(config.assets_production_prefix(), Some("static/v1"));
    }

    #[test]
    fn rejects_absolute_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "/absolute/path"
        "#};
        expect![[r#"
            error: assets.output_dir: path must not start with '/'
              --> hop.toml (line 2, col 14)
            1 | [assets]
            2 | output_dir = "/absolute/path"
              |              ^^^^^^^^^^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_empty_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = ""
        "#};
        expect![[r#"
            error: assets.output_dir: path cannot be empty
              --> hop.toml (line 2, col 14)
            1 | [assets]
            2 | output_dir = ""
              |              ^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn rejects_output_dir_with_trailing_slash() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "dist/"
        "#};
        expect![[r#"
            error: assets.output_dir: path cannot end with '/'
              --> hop.toml (line 2, col 14)
            1 | [assets]
            2 | output_dir = "dist/"
              |              ^^^^^^^
        "#]]
        .assert_eq(&error(toml_str));
    }

    #[test]
    fn accepts_output_dir_above_project_root() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "../assets"
        "#};
        assert_eq!(
            config(toml_str)
                .assets_output_dir()
                .map(RootRelativePath::as_str),
            Some("../assets")
        );
    }

    #[test]
    fn accepts_nested_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "a/b/c"
        "#};
        assert_eq!(
            config(toml_str)
                .assets_output_dir()
                .map(RootRelativePath::as_str),
            Some("a/b/c")
        );
    }

    #[test]
    fn accepts_dot_prefixed_output_dir() {
        let toml_str = indoc! {r#"
            [assets]
            output_dir = "./dist"
        "#};
        assert_eq!(
            config(toml_str)
                .assets_output_dir()
                .map(RootRelativePath::as_str),
            Some("dist")
        );
    }

    #[test]
    fn output_dir_and_production_prefix_can_coexist() {
        let toml_str = indoc! {r#"
            [assets]
            production_prefix = "static/v1"
            output_dir = "dist/public"
        "#};
        let config = config(toml_str);
        assert_eq!(config.assets_production_prefix(), Some("static/v1"));
        assert_eq!(
            config.assets_output_dir().map(RootRelativePath::as_str),
            Some("dist/public")
        );
    }
}
