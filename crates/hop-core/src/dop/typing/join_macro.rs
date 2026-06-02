use crate::document::CheapString;
use crate::dop::TypedExpr;

/// Build a balanced `StringConcat` tree from `typed_args`, interleaving spaces.
///
/// Splits the list in half recursively, producing a tree of depth O(log N).
pub fn build_balanced_join(mut typed_args: Vec<TypedExpr>) -> TypedExpr {
    match typed_args.len() {
        0 => TypedExpr::StringLiteral {
            value: CheapString::new(String::new()),
        },
        1 => typed_args.pop().unwrap(),
        _ => {
            let mid = typed_args.len() / 2;
            let right_half = typed_args.split_off(mid);
            let left = build_balanced_join(typed_args);
            let right = build_balanced_join(right_half);
            // TODO: Reduce allocations by re-using the same allocation for the
            // space separator for all concatenations?
            TypedExpr::StringConcat {
                left: Box::new(TypedExpr::StringConcat {
                    left: Box::new(left),
                    right: Box::new(TypedExpr::StringLiteral {
                        value: CheapString::new(" ".to_string()),
                    }),
                }),
                right: Box::new(right),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn str_lit(s: &str) -> TypedExpr {
        TypedExpr::StringLiteral {
            value: CheapString::new(s.to_string()),
        }
    }

    /// Evaluate a StringConcat/StringLiteral tree to a plain string.
    fn eval(expr: &TypedExpr) -> String {
        match expr {
            TypedExpr::StringConcat { left, right } => {
                format!("{}{}", eval(left), eval(right))
            }
            TypedExpr::StringLiteral { value } => value.to_string(),
            _ => unreachable!("unexpected variant in join tree"),
        }
    }

    /// Measure the maximum nesting depth of StringConcat nodes.
    fn depth(expr: &TypedExpr) -> usize {
        match expr {
            TypedExpr::StringConcat { left, right } => 1 + depth(left).max(depth(right)),
            TypedExpr::StringLiteral { .. } => 0,
            _ => unreachable!("unexpected variant in join tree"),
        }
    }

    #[test]
    fn empty_returns_empty_string() {
        let tree = build_balanced_join(vec![]);
        assert_eq!(eval(&tree), "");
    }

    #[test]
    fn single_arg_passes_through() {
        let tree = build_balanced_join(vec![str_lit("a")]);
        assert_eq!(eval(&tree), "a");
    }

    #[test]
    fn two_args_joins_with_space() {
        let tree = build_balanced_join(vec![str_lit("hello"), str_lit("world")]);
        assert_eq!(eval(&tree), "hello world");
    }

    #[test]
    fn three_args_joins_with_spaces() {
        let tree = build_balanced_join(vec![str_lit("a"), str_lit("b"), str_lit("c")]);
        assert_eq!(eval(&tree), "a b c");
    }

    #[test]
    fn five_args_produces_correct_result() {
        let tree = build_balanced_join(vec![
            str_lit("one"),
            str_lit("two"),
            str_lit("three"),
            str_lit("four"),
            str_lit("five"),
        ]);
        assert_eq!(eval(&tree), "one two three four five");
    }

    #[test]
    fn tree_is_balanced() {
        let args: Vec<_> = (0..32).map(|i| str_lit(&i.to_string())).collect();
        let tree = build_balanced_join(args);
        let d = depth(&tree);
        assert!(d <= 12, "depth {} should be <= 12 for balanced tree", d);
    }
}
