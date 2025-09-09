use super::Position;

/// Holds source text and precomputed line start offsets for
/// efficient position lookups.
#[derive(Debug, Clone)]
pub struct DocumentInfo {
    /// The source text.
    pub(super) text: String,
    /// Byte offsets where each line starts.
    /// First line always starts at 0.
    pub(super) line_starts: Vec<usize>,
}

impl DocumentInfo {
    pub fn new(text: String) -> Self {
        let mut line_starts = vec![0];
        for (i, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(i + ch.len_utf8());
            }
        }
        Self { text, line_starts }
    }

    /// Convert a byte offset to a UTF-16 position (line, column).
    pub fn offset_to_utf16_position(&self, offset: usize) -> Position {
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        // Calculate UTF-16 column offset from the start of the line
        let line_start_byte = self.line_starts[line_idx];
        let line_text = &self.text[line_start_byte..offset];
        let utf16_column: usize = line_text.chars().map(|ch| ch.len_utf16()).sum();

        Position::Utf16 {
            line: line_idx,
            column: utf16_column,
        }
    }

    /// Convert a byte offset to a UTF-32 position (line, column).
    /// UTF-32 column is the character count from the start of the line.
    pub fn offset_to_utf32_position(&self, offset: usize) -> Position {
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        // Calculate UTF-32 column offset (character count) from the start of the line
        let line_start_byte = self.line_starts[line_idx];
        let line_text = &self.text[line_start_byte..offset];
        let utf32_column = line_text.chars().count();

        Position::Utf32 {
            line: line_idx,
            column: utf32_column,
        }
    }
}
