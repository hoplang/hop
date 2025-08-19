//! Shared test utilities to avoid duplication across test modules.

/// Parses test cases from txtar format content.
/// 
/// Returns a vector of (test_case_content, start_line_number) tuples.
/// Each test case is delimited by `## BEGIN` and `## END` markers.
/// 
/// # Example
/// 
/// ```
/// use hop_rs::test_utils::parse_test_cases;
/// 
/// let content = r#"
/// Test case 1 description
/// 
/// ## BEGIN
/// -- input.txt --
/// some input
/// -- output.txt --
/// expected output
/// ## END
/// "#;
/// 
/// let cases = parse_test_cases(content);
/// assert_eq!(cases.len(), 1);
/// ```
pub fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
    let mut test_cases = Vec::new();
    let mut current_case = String::new();
    let mut in_case = false;
    let mut case_start_line = 0;

    for (line_num, line) in content.lines().enumerate() {
        let line_number = line_num + 1;

        if line == "## BEGIN" {
            assert!(
                !in_case,
                "Found '## BEGIN' at line {} while already inside a test case",
                line_number
            );
            in_case = true;
            case_start_line = line_number;
            current_case.clear();
        } else if line == "## END" {
            assert!(
                in_case,
                "Found '## END' at line {} without matching '## BEGIN'",
                line_number
            );
            test_cases.push((current_case.clone(), case_start_line));
            in_case = false;
        } else if in_case {
            if !current_case.is_empty() {
                current_case.push('\n');
            }
            current_case.push_str(line);
        }
    }

    assert!(
        !in_case,
        "Reached end of file while inside a test case (missing '## END')"
    );

    test_cases
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_test_cases_single() {
        let content = r#"Some description

## BEGIN
-- input.txt --
hello world
-- output.txt --  
expected
## END
"#;

        let cases = parse_test_cases(content);
        assert_eq!(cases.len(), 1);
        assert_eq!(cases[0].1, 3); // Line number of ## BEGIN
        assert!(cases[0].0.contains("-- input.txt --"));
        assert!(cases[0].0.contains("hello world"));
        assert!(cases[0].0.contains("-- output.txt --"));
        assert!(cases[0].0.contains("expected"));
    }

    #[test]
    fn test_parse_test_cases_multiple() {
        let content = "A basic type can be unified with itself.

## BEGIN
-- in --
(unify string string)
-- out --
## END

Two different basic types can not be unified.

## BEGIN
-- in --
(unify string bool)
-- out --
Can not unify types
## END";

        let cases = parse_test_cases(content);
        assert_eq!(cases.len(), 2);
        assert!(cases[0].0.contains("(unify string string)"));
        assert!(cases[1].0.contains("(unify string bool)"));
    }

    #[test]
    #[should_panic(expected = "Found '## END' at line 2 without matching '## BEGIN'")]
    fn test_parse_test_cases_unmatched_end() {
        let content = r#"some content
## END"#;
        parse_test_cases(content);
    }

    #[test]
    #[should_panic(expected = "Reached end of file while inside a test case")]
    fn test_parse_test_cases_unmatched_begin() {
        let content = r#"## BEGIN
some content"#;
        parse_test_cases(content);
    }
}