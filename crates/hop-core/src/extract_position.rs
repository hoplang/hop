use crate::{
    document::{Document, DocumentPosition, PositionEncoding},
    document_id::DocumentId,
};

/// Extracts a single position marked with `^` from the source.
///
/// If a marker is found, returns the cleaned document (without the marker
/// line) and the position on the line above the marker.
///
/// If no marker is found, returns None.
///
/// Panics if multiple position markers are found or if the marker does not
/// point to a valid character on the line above.
pub fn extract_position(
    document_id: DocumentId,
    input: &str,
) -> Option<(Document, DocumentPosition)> {
    let markers = input
        .lines()
        .enumerate()
        .flat_map(|(line, text)| {
            text.char_indices()
                .filter(|(_, ch)| *ch == '^')
                .map(move |(byte, _)| (line, text[..byte].chars().count()))
        })
        .collect::<Vec<_>>();
    assert!(
        markers.len() < 2,
        "Multiple position markers (^) found in source"
    );
    let &(marker_line, column) = markers.first()?;
    assert!(marker_line > 0, "Marker does not point to a valid position");
    let mut output = input
        .lines()
        .filter(|line| !line.contains('^'))
        .collect::<Vec<_>>()
        .join("\n");
    if input.ends_with('\n') {
        output.push('\n');
    }
    let document = Document::new(document_id, output);
    let position = document
        .position(PositionEncoding::Utf32, marker_line - 1, column)
        .expect("Marker does not point to a valid position");
    Some((document, position))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn check_extract_position(input: &str, expected_output: &str, expected: (usize, usize)) {
        let (document, position) =
            extract_position(DocumentId::new("test.hop").unwrap(), input).unwrap();
        assert_eq!(document.as_str(), expected_output);
        assert_eq!((position.line(), position.utf32_column()), expected);
    }

    #[test]
    fn extract_position_start_of_line() {
        check_extract_position(
            indoc! {r#"
                <hello-world>
                ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            indoc! {r#"
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            (0, 0),
        );
    }

    #[test]
    fn extract_position_middle_of_line() {
        check_extract_position(
            indoc! {r#"
                <hello-world>
                        ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            indoc! {r#"
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            (0, 8),
        );
    }

    #[test]
    fn extract_position_end_of_line() {
        check_extract_position(
            indoc! {r#"
                <hello-world>
                            ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            indoc! {r#"
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            (0, 12),
        );
    }

    #[test]
    fn marker_on_last_line() {
        check_extract_position(
            indoc! {r#"
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
                            ^
            "#},
            indoc! {r#"
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            (2, 12),
        );
    }

    #[test]
    fn no_position_returns_none() {
        let input = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert!(extract_position(DocumentId::new("test.hop").unwrap(), input).is_none());
    }

    #[test]
    #[should_panic(expected = "Multiple position markers")]
    fn multiple_positions_panics() {
        let input = indoc! {r#"
            <hello-world>
              ^
              <h1>Hello World</h1>
                     ^
            </hello-world>
        "#};

        let _ = extract_position(DocumentId::new("test.hop").unwrap(), input);
    }

    #[test]
    #[should_panic(expected = "Marker does not point to a valid position")]
    fn marker_on_first_line_panics() {
        let input = indoc! {r#"
            ^
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        let _ = extract_position(DocumentId::new("test.hop").unwrap(), input);
    }

    #[test]
    #[should_panic(expected = "Marker does not point to a valid position")]
    fn marker_in_middle_of_first_line_panics() {
        let input = indoc! {r#"
                 ^
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        let _ = extract_position(DocumentId::new("test.hop").unwrap(), input);
    }

    #[test]
    #[should_panic(expected = "Marker does not point to a valid position")]
    fn marker_past_end_of_line_panics() {
        let input = indoc! {r#"
            <hello-world>
                          ^
              <h1>Hello World</h1>
            </hello-world>
        "#};

        let _ = extract_position(DocumentId::new("test.hop").unwrap(), input);
    }

    #[test]
    fn no_trailing_newline_preserved() {
        // Note: we manually create the string without trailing newline
        // since indoc! always adds one
        check_extract_position(
            "<hello-world>\n        ^\n  <h1>Hello World</h1>\n</hello-world>",
            "<hello-world>\n  <h1>Hello World</h1>\n</hello-world>",
            (0, 8),
        );
    }
}
