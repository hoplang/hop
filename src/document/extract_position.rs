use std::collections::HashSet;

use super::{DocumentPosition, document_cursor::DocumentCursor};

/// Extracts a single position marked with `^` from the source.
///
/// If a marker is found, returns the cleaned source (without marker line)
/// and the position as an UTF32 (line, col) pair.
///
/// If no marker is found, returns None.
///
/// Panics if multiple position markers are found or if marker does not point to a valid character
/// on the above line.
pub fn extract_position(input: &str) -> Option<(String, DocumentPosition)> {
    let markers = DocumentCursor::new(input.to_string())
        .filter(|range| range.ch() == '^')
        .map(|range| {
            // Check if marker is on the first line (line 0)
            if range.start_utf32().line() == 0 {
                panic!("Marker does not point to a valid position");
            }
            // Get position at line above
            DocumentPosition::Utf32 {
                line: range.start_utf32().line() - 1,
                column: range.start_utf32().column(),
            }
        })
        .collect::<Vec<_>>();
    assert!(
        markers.len() < 2,
        "Multiple position markers (^) found in source"
    );
    markers.first().map(|marker| {
        let char_starts = DocumentCursor::new(input.to_string())
            .map(|range| range.start_utf32())
            .collect::<HashSet<_>>();
        assert!(
            char_starts.contains(marker),
            "Marker does not point to a valid position"
        );
        let mut output = input
            .lines()
            .filter(|line| !line.contains('^'))
            .collect::<Vec<_>>()
            .join("\n");
        if input.ends_with('\n') {
            output.push('\n');
        }
        (output, *marker)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn extract_position_start_of_line() {
        let input = indoc! {r#"
            <hello-world>
            ^
              <h1>Hello World</h1>
            </hello-world>
        "#};
        let output = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(
            extract_position(input),
            Some((
                output.to_string(),
                DocumentPosition::Utf32 { line: 0, column: 0 }
            ))
        );
    }

    #[test]
    fn extract_position_middle_of_line() {
        let input = indoc! {r#"
            <hello-world>
                    ^
              <h1>Hello World</h1>
            </hello-world>
        "#};
        let output = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(
            extract_position(input),
            Some((
                output.to_string(),
                DocumentPosition::Utf32 { line: 0, column: 8 }
            ))
        );
    }

    #[test]
    fn extract_position_end_of_line() {
        let input = indoc! {r#"
            <hello-world>
                        ^
              <h1>Hello World</h1>
            </hello-world>
        "#};
        let output = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(
            extract_position(input),
            Some((
                output.to_string(),
                DocumentPosition::Utf32 {
                    line: 0,
                    column: 12
                }
            ))
        );
    }

    #[test]
    fn marker_on_last_line() {
        let input = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
                        ^
        "#};
        let output = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(
            extract_position(input),
            Some((
                output.to_string(),
                DocumentPosition::Utf32 {
                    line: 2,
                    column: 12
                }
            ))
        );
    }

    #[test]
    fn no_position_returns_none() {
        let input = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(extract_position(input), None);
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

        let _ = extract_position(input);
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

        let _ = extract_position(input);
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

        let _ = extract_position(input);
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

        let _ = extract_position(input);
    }

    #[test]
    fn no_trailing_newline_preserved() {
        // Note: we manually create the string without trailing newline
        // since indoc! always adds one
        let input = "<hello-world>\n        ^\n  <h1>Hello World</h1>\n</hello-world>";
        let output = "<hello-world>\n  <h1>Hello World</h1>\n</hello-world>";

        assert_eq!(
            extract_position(input),
            Some((
                output.to_string(),
                DocumentPosition::Utf32 { line: 0, column: 8 }
            ))
        );
    }
}
