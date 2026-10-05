use crate::root_contained_file_path::RootContainedFilePath;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum FormatError {
    #[error("Document '{}' not found", .0.as_str())]
    DocumentNotFound(RootContainedFilePath),

    #[error("Cannot format document '{}': it has parse errors", .0.as_str())]
    HasParseErrors(RootContainedFilePath),
}
