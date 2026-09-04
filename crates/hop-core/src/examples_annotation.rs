/// Metadata from `#[examples(...)]` annotations on fields/parameters.
#[derive(Debug, Clone, Default)]
pub struct ExamplesAnnotation {
    /// Regex pattern for String fields.
    pub pattern: Option<String>,
    /// Minimum value for Int fields.
    pub min: Option<i32>,
    /// Maximum value for Int fields.
    pub max: Option<i32>,
    /// Minimum length for Array fields.
    pub min_len: Option<i32>,
    /// Maximum length for Array fields.
    pub max_len: Option<i32>,
}

impl ExamplesAnnotation {
    /// Formats the annotation as `#[examples(pattern = "...", min = N, max = N, min_len = N, max_len = N)]`.
    pub fn to_annotation_string(&self) -> String {
        let mut parts = Vec::new();
        if let Some(pattern) = &self.pattern {
            parts.push(format!("pattern = \"{}\"", pattern));
        }
        if let Some(min) = self.min {
            parts.push(format!("min = {}", min));
        }
        if let Some(max) = self.max {
            parts.push(format!("max = {}", max));
        }
        if let Some(min_len) = self.min_len {
            parts.push(format!("min_len = {}", min_len));
        }
        if let Some(max_len) = self.max_len {
            parts.push(format!("max_len = {}", max_len));
        }
        format!("#[examples({})]", parts.join(", "))
    }
}
