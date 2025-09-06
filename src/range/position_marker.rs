use std::collections::HashSet;

use crate::range::Ranged as _;

use super::{Position, string_cursor::StringCursor};

/// Extracts a single position marked with `^` from the source.
///
/// If a marker is found, returns the cleaned source (without marker line) and the position.
/// If no marker is found, returns None.
///
/// Panics if multiple position markers are found or if marker does not point to a valid character
/// on the above line.
pub fn extract_position(input: &str) -> Option<(String, Position)> {
    let markers = StringCursor::new(input)
        .filter(|span| span.ch() == '^')
        .map(|span| Position::new(span.range().start().line() - 1, span.range().start().column()))
        .collect::<Vec<_>>();
    assert!(
        markers.len() < 2,
        "Multiple position markers (^) found in source"
    );
    markers.first().map(|marker| {
        let char_starts = StringCursor::new(input)
            .map(|span| span.range().start())
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
    fn test_extract_position_start_of_line() {
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
            Some((output.to_string(), Position::new(1, 1)))
        );
    }

    #[test]
    fn test_extract_position_middle_of_line() {
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
            Some((output.to_string(), Position::new(1, 9)))
        );
    }

    #[test]
    fn test_extract_position_end_of_line() {
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
            Some((output.to_string(), Position::new(1, 13)))
        );
    }

    #[test]
    fn test_marker_on_last_line() {
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
            Some((output.to_string(), Position::new(3, 13)))
        );
    }

    #[test]
    fn test_no_position_returns_none() {
        let input = indoc! {r#"
            <hello-world>
              <h1>Hello World</h1>
            </hello-world>
        "#};

        assert_eq!(extract_position(input), None);
    }

    #[test]
    #[should_panic(expected = "Multiple position markers")]
    fn test_multiple_positions_panics() {
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
    fn test_marker_on_first_line_panics() {
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
    fn test_marker_in_middle_of_first_line_panics() {
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
    fn test_marker_past_end_of_line_panics() {
        let input = indoc! {r#"
            <hello-world>
                          ^
              <h1>Hello World</h1>
            </hello-world>
        "#};

        let _ = extract_position(input);
    }

    #[test]
    fn test_no_trailing_newline_preserved() {
        // Note: we manually create the string without trailing newline
        // since indoc! always adds one
        let input = "<hello-world>\n        ^\n  <h1>Hello World</h1>\n</hello-world>";
        let output = "<hello-world>\n  <h1>Hello World</h1>\n</hello-world>";

        assert_eq!(
            extract_position(input),
            Some((output.to_string(), Position::new(1, 9)))
        );
    }
}
