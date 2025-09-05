use super::range::Position;

/// Extracts a single position marked with `^` from the source.
///
/// If a marker is found, returns the cleaned source (without marker line) and the position.
/// If no marker is found, returns None.
///
/// Panics if multiple position markers are found or if marker does not point to a valid character
/// on the above line.
pub fn extract_position(input: &str) -> Option<(String, Position)> {
    let mut output = String::new();
    let mut line_iter = input.lines().enumerate().peekable();
    let mut position = None;

    if line_iter.peek().is_some_and(|(_, line)| line.contains('^')) {
        panic!("Cannot place position marker on first line");
    }

    while let Some((i, line)) = line_iter.next() {
        if let Some((_, next_line)) = line_iter.peek() {
            if let Some(col_idx) = next_line.find('^') {
                if col_idx >= line.len() {
                    panic!("Position marker is past the end of the line");
                }

                if position.is_some() {
                    panic!("Multiple position markers (^) found in source");
                }

                position = Some(Position::new(i + 1, col_idx + 1));

                output.push_str(line);
                output.push('\n');
                line_iter.next(); // Skip the marker line
                continue;
            }
        }
        output.push_str(line);
        output.push('\n');
    }

    position.map(|pos| {
        if !input.ends_with('\n') {
            output.pop();
        }
        (output, pos)
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
    #[should_panic(expected = "Cannot place position marker on first line")]
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
    #[should_panic(expected = "Cannot place position marker on first line")]
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
    #[should_panic(expected = "Position marker is past the end of the line")]
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
