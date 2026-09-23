use std::fmt;
use std::sync::Arc;
use thiserror::Error;

use crate::document::CheapString;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::reserved::is_reserved_name;

/// Error type for invalid module IDs
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidModuleNameError {
    #[error("Module name component '{0}' is a reserved word")]
    ReservedComponent(String),
}

/// A type-safe wrapper for module IDs in the hop system.
/// Module IDs represent the path to a module relative to the project root,
/// without the .hop extension. Internally stored with `::` separators.
///
/// Examples: "components::button", "utils", "hop::ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(Arc<[CheapString]>);

impl ModuleName {
    /// Create a new ModuleName from the identifier segments of an import path.
    ///
    /// Every segment is an identifier token, so the only way a module name can
    /// be invalid is by naming a reserved word.
    pub fn new(segments: Vec<CheapString>) -> Result<Self, InvalidModuleNameError> {
        debug_assert!(
            !segments.is_empty(),
            "an import path always has at least one module segment"
        );
        for segment in &segments {
            if is_reserved_name(segment.as_str()) {
                return Err(InvalidModuleNameError::ReservedComponent(
                    segment.as_str().to_string(),
                ));
            }
        }
        Ok(ModuleName(Arc::from(segments)))
    }

    pub fn to_file_path(&self) -> RootContainedFilePath {
        let path = self
            .0
            .iter()
            .map(|segment| segment.as_str())
            .collect::<Vec<_>>()
            .join("/");
        RootContainedFilePath::new(&format!("{path}.hop")).unwrap()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, segment) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str("::")?;
            }
            f.write_str(segment.as_str())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn segments(segments: &[&str]) -> Vec<CheapString> {
        segments
            .iter()
            .map(|segment| CheapString::new(segment.to_string()))
            .collect()
    }

    fn accept(input: &[&str]) {
        assert!(ModuleName::new(segments(input)).is_ok());
    }

    fn reject(input: &[&str], expected: InvalidModuleNameError) {
        assert_eq!(ModuleName::new(segments(input)), Err(expected));
    }

    #[test]
    fn accepts_simple_module_id() {
        accept(&["utils"]);
    }

    #[test]
    fn accepts_module_id_with_path() {
        accept(&["components", "button"]);
        accept(&["hop", "ui"]);
    }

    #[test]
    fn accepts_module_id_with_underscore() {
        accept(&["my_component"]);
    }

    #[test]
    fn accepts_deeply_nested_module_id() {
        accept(&["a", "b", "c", "d"]);
    }

    #[test]
    fn joins_segments_with_separators() {
        assert_eq!(
            ModuleName::new(segments(&["components", "button"]))
                .unwrap()
                .to_string(),
            "components::button"
        );
    }

    #[test]
    fn rejects_module_id_with_reserved_component() {
        reject(
            &["mod"],
            InvalidModuleNameError::ReservedComponent("mod".to_string()),
        );
        reject(
            &["components", "if"],
            InvalidModuleNameError::ReservedComponent("if".to_string()),
        );
    }
}
