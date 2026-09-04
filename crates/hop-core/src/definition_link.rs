use crate::document::DocumentRange;

/// A link from a use site to its definition site.
#[derive(Debug, Clone)]
pub struct DefinitionLink {
    /// The range where the symbol is used/referenced.
    pub use_range: DocumentRange,
    /// The range where the symbol is defined.
    pub definition_range: DocumentRange,
}
