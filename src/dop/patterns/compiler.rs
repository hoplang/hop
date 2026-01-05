//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from <https://github.com/yorickpeterse/pattern-matching-in-rust/>.
//! Thanks to Yorick Peterse for the original implementation.
use std::collections::{HashMap, HashSet};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::syntax::parsed::{Constructor, ParsedMatchPattern};

use crate::dop::semantics::r#type::Type;
use crate::dop::semantics::type_error::TypeError;

/// A binding introduced by a pattern match (i.e. `name = source_name`).
#[derive(Clone, Debug)]
pub struct Binding {
    /// The name of the variable to bind.
    pub name: String,
    /// The name of the source variable to bind from.
    pub source_name: String,
    /// The type of the binding.
    pub typ: Type,
}

impl Binding {
    pub fn new(name: String, source_name: String, typ: Type) -> Self {
        Self { name, source_name, typ }
    }
}

/// The body of code to evaluate in case of a match.
#[derive(Clone, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    pub bindings: Vec<Binding>,
    /// The branch to run in case of a match.
    pub value: usize,
}

impl Body {
    pub fn new(value: usize) -> Self {
        Self {
            bindings: Vec::new(),
            value,
        }
    }
}

/// A variable used in a match expression.
#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub typ: Type,
}

impl Variable {
    pub fn new(name: String, typ: Type) -> Self {
        Self { name, typ }
    }
}

/// A single case (or row) in a match expression/table.
#[derive(Clone, Debug)]
struct Row {
    columns: Vec<Column>,
    body: Body,
}

impl Row {
    fn new(columns: Vec<Column>, body: Body) -> Self {
        Self { columns, body }
    }

    fn remove_column(&mut self, variable: &Variable) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| c.variable.name == variable.name)
            .map(|idx| self.columns.remove(idx))
    }
}

/// A column in a pattern matching table.
///
/// A column contains a single variable to test, and a pattern to test against
/// that variable. A row may contain multiple columns, though this wouldn't be
/// exposed to the source language.
#[derive(Clone, Debug)]
struct Column {
    variable: Variable,
    pattern: ParsedMatchPattern,
}

impl Column {
    fn new(variable: Variable, pattern: ParsedMatchPattern) -> Self {
        Self { variable, pattern }
    }
}

/// A case in a decision tree to test against a variable.
#[derive(Debug)]
pub struct Case {
    /// The constructor to test against an input variable.
    pub constructor: Constructor,

    /// Variables to introduce to the body of this case.
    pub arguments: Vec<Variable>,

    /// The sub tree of this case.
    pub body: Decision,
}

impl Case {
    fn new(constructor: Constructor, arguments: Vec<Variable>, body: Decision) -> Self {
        Self {
            constructor,
            arguments,
            body,
        }
    }
}

/// A step in the path to a missing pattern, representing a constructor that was matched.
#[derive(Debug, Clone)]
enum MatchedConstructor {
    /// A constructor with positional arguments (e.g., Some(x), None, true, Color::Red)
    Positional {
        var_name: String,
        constructor_name: String,
        arguments: Vec<String>,
    },
    /// A record constructor with named fields (e.g., User(name: x, age: y))
    Record {
        var_name: String,
        constructor_name: String,
        arguments: Vec<String>,
        field_names: Vec<String>,
    },
}

impl MatchedConstructor {
    fn var_name(&self) -> &str {
        match self {
            MatchedConstructor::Positional { var_name, .. } => var_name,
            MatchedConstructor::Record { var_name, .. } => var_name,
        }
    }

    /// Build a pattern string from this constructor and the full path.
    fn pattern_string(&self, all_steps: &[MatchedConstructor]) -> String {
        match self {
            MatchedConstructor::Positional {
                constructor_name,
                arguments,
                ..
            } => {
                if arguments.is_empty() {
                    constructor_name.clone()
                } else {
                    let args = arguments
                        .iter()
                        .map(|arg_var| {
                            all_steps
                                .iter()
                                .find(|s| s.var_name() == arg_var)
                                .map(|s| s.pattern_string(all_steps))
                                .unwrap_or_else(|| "_".to_string())
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}({})", constructor_name, args)
                }
            }
            MatchedConstructor::Record {
                constructor_name,
                arguments,
                field_names,
                ..
            } => {
                let args = arguments
                    .iter()
                    .zip(field_names.iter())
                    .map(|(arg_var, field_name)| {
                        let pattern = all_steps
                            .iter()
                            .find(|s| s.var_name() == arg_var)
                            .map(|s| s.pattern_string(all_steps))
                            .unwrap_or_else(|| "_".to_string());
                        format!("{}: {}", field_name, pattern)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", constructor_name, args)
            }
        }
    }
}

/// Convert a path of matched constructors to a pattern string.
fn path_to_pattern_string(path: &[MatchedConstructor]) -> String {
    path.first()
        .map(|first| first.pattern_string(path))
        .unwrap_or_else(|| "_".to_string())
}

/// A decision tree compiled from a list of match cases.
#[derive(Debug)]
pub enum Decision {
    /// A pattern is matched and the right-hand value is to be returned.
    Success(Body),

    /// Checks if a value is any of the given patterns.
    ///
    /// The values are as follows:
    ///
    /// 1. The variable to test.
    /// 2. The cases to test against this variable.
    Switch(Variable, Vec<Case>),
}

/// The `match` compiler itself.
pub struct Compiler {
    /// A counter used to construct unused variable names.
    var_counter: usize,
    /// The arm indices that are reachable.
    reachable: Vec<usize>,
    /// Missing pattern strings collected during compilation.
    missing_patterns: HashSet<String>,
}

impl Compiler {
    pub fn new(initial_var_counter: usize) -> Self {
        Self {
            var_counter: initial_var_counter,
            reachable: Vec::new(),
            missing_patterns: HashSet::new(),
        }
    }

    /// Returns the index of a constructor within the given type.
    fn constructor_index(&self, cons: &Constructor, typ: &Type) -> usize {
        match cons {
            Constructor::BooleanFalse => 0,
            Constructor::BooleanTrue => 1,
            Constructor::OptionSome => 0,
            Constructor::OptionNone => 1,
            Constructor::EnumVariant { variant_name, .. } => {
                if let Type::Enum { variants, .. } = typ {
                    variants
                        .iter()
                        .position(|v| v.as_str() == variant_name)
                        .expect("unknown variant")
                } else {
                    panic!("type is not an enum")
                }
            }
            // Records have only one constructor, so index is always 0
            Constructor::Record { .. } => 0,
        }
    }

    pub fn compile(
        mut self,
        patterns: &[ParsedMatchPattern],
        subject_name: &str,
        subject_type: &Type,
        subject_range: &DocumentRange,
        match_range: &DocumentRange,
    ) -> Result<Decision, TypeError> {
        // Validate subject type is matchable
        if !matches!(
            subject_type,
            Type::Enum { .. } | Type::Bool | Type::Option(_) | Type::Record { .. }
        ) {
            return Err(TypeError::MatchNotImplementedForType {
                found: subject_type.to_string(),
                range: subject_range.clone(),
            });
        }

        // Check for empty arms
        if patterns.is_empty() {
            return Err(TypeError::MatchNoArms {
                range: match_range.clone(),
            });
        }

        // Validate all patterns against the subject type
        for pattern in patterns {
            Self::validate_pattern(pattern, subject_type)?;
        }

        let subject_var = Variable::new(subject_name.to_string(), subject_type.clone());
        let rows: Vec<Row> = patterns
            .iter()
            .enumerate()
            .map(|(idx, pattern)| {
                Row::new(
                    vec![Column::new(subject_var.clone(), pattern.clone())],
                    Body::new(idx),
                )
            })
            .collect();

        let tree = self.compile_rows(rows, Vec::new());

        // Check for unreachable arms
        let unreachable: Vec<usize> = (0..patterns.len())
            .filter(|i| !self.reachable.contains(i))
            .collect();
        if let Some(&first_unreachable) = unreachable.first() {
            let pattern = &patterns[first_unreachable];
            return Err(TypeError::MatchUnreachableArm {
                variant: pattern.to_string(),
                range: pattern.range().clone(),
            });
        }

        // Check for missing patterns
        if !self.missing_patterns.is_empty() {
            let mut missing: Vec<String> = self.missing_patterns.into_iter().collect();
            missing.sort();
            return Err(TypeError::MatchMissingVariants {
                variants: missing,
                range: match_range.clone(),
            });
        }

        // Tree is guaranteed to be Some if there are no missing patterns
        Ok(tree.expect("tree should be Some when there are no missing patterns"))
    }

    fn compile_rows(
        &mut self,
        mut rows: Vec<Row>,
        path: Vec<MatchedConstructor>,
    ) -> Option<Decision> {
        if rows.is_empty() {
            self.missing_patterns.insert(path_to_pattern_string(&path));
            return None;
        }

        for row in &mut rows {
            // Remove wildcards and move binding patterns into the body
            row.columns.retain(|col| match &col.pattern {
                ParsedMatchPattern::Wildcard { .. } => false,
                ParsedMatchPattern::Binding { name, .. } => {
                    row.body.bindings.push(Binding::new(
                        name.clone(),
                        col.variable.name.clone(),
                        col.variable.typ.clone(),
                    ));
                    false
                }
                ParsedMatchPattern::Constructor { .. } => true,
            });
        }

        // There may be multiple rows, but if the first one has no patterns
        // those extra rows are redundant, as a row without columns/patterns
        // always matches.
        if rows.first().is_some_and(|c| c.columns.is_empty()) {
            let row = rows.remove(0);
            self.reachable.push(row.body.value);
            return Some(Decision::Success(row.body));
        }

        let branch_var = self.find_branch_variable(&rows);

        let mut cases = match &branch_var.typ {
            Type::Bool => {
                vec![
                    (Constructor::BooleanFalse, Vec::new(), Vec::new()),
                    (Constructor::BooleanTrue, Vec::new(), Vec::new()),
                ]
            }
            Type::Option(inner) => {
                vec![
                    (
                        Constructor::OptionSome,
                        vec![self.fresh_var(inner.as_ref().clone())],
                        Vec::new(),
                    ),
                    (Constructor::OptionNone, Vec::new(), Vec::new()),
                ]
            }
            Type::Enum { name, variants, .. } => variants
                .iter()
                .map(|variant| {
                    (
                        Constructor::EnumVariant {
                            enum_name: name.clone(),
                            variant_name: variant.to_string(),
                        },
                        Vec::new(),
                        Vec::new(),
                    )
                })
                .collect(),
            Type::Record { name, fields, .. } => {
                // Records have a single constructor with fresh variables for each field
                let field_vars: Vec<Variable> = fields
                    .iter()
                    .map(|(_, field_type)| self.fresh_var(field_type.clone()))
                    .collect();
                vec![(
                    Constructor::Record {
                        type_name: name.clone(),
                    },
                    field_vars,
                    Vec::new(),
                )]
            }
            Type::String | Type::Int | Type::Float | Type::TrustedHTML | Type::Array(_) => {
                panic!("pattern matching not supported for this type")
            }
        };

        // Compile the cases and sub cases for the constructor located at the
        // column of the branching variable.
        //
        // 1. Take the column we're branching on and remove it from every row.
        // 2. We add additional columns to this row, if the constructor takes any
        //    arguments (which we'll handle in a nested match).
        // 3. We turn the resulting list of rows into a list of cases, then compile
        //    those into decision (sub) trees.
        //
        // If a row didn't include the branching variable, we simply copy that row
        // into the list of rows for every constructor to test.
        //
        // For this to work, the `cases` variable must be prepared such that it has
        // a triple for every constructor we need to handle. For an ADT with 10
        // constructors, that means 10 triples. This is needed so this method can
        // assign the correct sub matches to these constructors.
        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                if let ParsedMatchPattern::Constructor {
                    constructor: cons,
                    args,
                    fields,
                    ..
                } = col.pattern
                {
                    let idx = self.constructor_index(&cons, &branch_var.typ);
                    let mut cols = row.columns;

                    if !fields.is_empty() {
                        // Record pattern: match fields by name
                        if let Type::Record {
                            fields: type_fields,
                            ..
                        } = &branch_var.typ
                        {
                            for (field_name, field_pattern) in fields {
                                // Find the index of this field in the record type
                                let field_idx = type_fields
                                    .iter()
                                    .position(|(name, _)| name == &field_name)
                                    .expect("field not found in record type");
                                let var = &cases[idx].1[field_idx];
                                cols.push(Column::new(var.clone(), field_pattern));
                            }
                        }
                    } else {
                        // Positional args (Option Some, etc.)
                        for (var, pat) in cases[idx].1.iter().zip(args.into_iter()) {
                            cols.push(Column::new(var.clone(), pat));
                        }
                    }

                    cases[idx].2.push(Row::new(cols, row.body));
                }
            } else {
                for (_, _, rows) in &mut cases {
                    rows.push(row.clone());
                }
            }
        }

        // Compile all case bodies, collecting missing patterns along the way
        let compiled_cases: Vec<_> = cases
            .into_iter()
            .map(|(cons, vars, rows)| {
                let constructor_name = cons.to_string();
                let arguments: Vec<String> = vars.iter().map(|v| v.name.clone()).collect();

                let matched = if let Constructor::Record { .. } = &cons {
                    let field_names: Vec<String> =
                        if let Type::Record { fields, .. } = &branch_var.typ {
                            fields.iter().map(|(name, _)| name.to_string()).collect()
                        } else {
                            Vec::new()
                        };
                    MatchedConstructor::Record {
                        var_name: branch_var.name.clone(),
                        constructor_name,
                        arguments,
                        field_names,
                    }
                } else {
                    MatchedConstructor::Positional {
                        var_name: branch_var.name.clone(),
                        constructor_name,
                        arguments,
                    }
                };

                let mut new_path = path.clone();
                new_path.push(matched);

                let body = self.compile_rows(rows, new_path);
                (cons, vars, body)
            })
            .collect();

        // If any case body is None, return None (missing patterns already collected)
        if compiled_cases.iter().any(|(_, _, body)| body.is_none()) {
            return None;
        }

        // All case bodies are Some, build the Switch
        Some(Decision::Switch(
            branch_var.clone(),
            compiled_cases
                .into_iter()
                .map(|(cons, vars, body)| Case::new(cons, vars, body.unwrap()))
                .collect(),
        ))
    }

    /// Given a row, returns the variable in that row that's referred to the
    /// most across all rows.
    fn find_branch_variable(&self, rows: &[Row]) -> Variable {
        let mut counts: HashMap<&str, usize> = HashMap::new();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable.name).or_insert(0_usize) += 1
            }
        }

        rows[0]
            .columns
            .iter()
            .map(|col| col.variable.clone())
            .max_by_key(|var| counts[var.name.as_str()])
            .unwrap()
    }

    /// Returns a new variable to use in the decision tree.
    fn fresh_var(&mut self, typ: Type) -> Variable {
        let name = format!("v{}", self.var_counter);
        self.var_counter += 1;
        Variable::new(name, typ)
    }

    /// Validates that a pattern is compatible with the subject type.
    fn validate_pattern(
        pattern: &ParsedMatchPattern,
        subject_type: &Type,
    ) -> Result<(), TypeError> {
        match pattern {
            ParsedMatchPattern::Wildcard { .. } => Ok(()),
            ParsedMatchPattern::Binding { .. } => Ok(()),
            ParsedMatchPattern::Constructor {
                constructor,
                args,
                fields,
                range,
            } => match (constructor, subject_type) {
                (
                    Constructor::EnumVariant {
                        enum_name: pattern_enum_name,
                        variant_name: pattern_variant_name,
                    },
                    Type::Enum {
                        name: subject_enum_name,
                        variants,
                        ..
                    },
                ) => {
                    if pattern_enum_name != subject_enum_name {
                        return Err(TypeError::MatchPatternEnumMismatch {
                            pattern_enum: pattern_enum_name.to_string(),
                            subject_enum: subject_enum_name.to_string(),
                            range: range.clone(),
                        });
                    }

                    let variant_exists =
                        variants.iter().any(|v| v.as_str() == pattern_variant_name);
                    if !variant_exists {
                        return Err(TypeError::UndefinedEnumVariant {
                            enum_name: pattern_enum_name.to_string(),
                            variant_name: pattern_variant_name.clone(),
                            range: range.clone(),
                        });
                    }

                    Ok(())
                }

                (Constructor::BooleanTrue | Constructor::BooleanFalse, Type::Bool) => Ok(()),

                (Constructor::OptionSome, Type::Option(inner_type)) => {
                    if let Some(inner_pattern) = args.first() {
                        Self::validate_pattern(inner_pattern, inner_type)?;
                    }
                    Ok(())
                }

                (Constructor::OptionNone, Type::Option(_)) => Ok(()),

                (
                    Constructor::Record {
                        type_name: pattern_type_name,
                    },
                    Type::Record {
                        name: subject_type_name,
                        fields: subject_fields,
                        ..
                    },
                ) => {
                    // Check record type matches
                    if pattern_type_name != subject_type_name {
                        return Err(TypeError::MatchPatternRecordMismatch {
                            pattern_record: pattern_type_name.to_string(),
                            subject_record: subject_type_name.to_string(),
                            range: range.clone(),
                        });
                    }

                    // Check all fields are specified (no partial matching)
                    if fields.len() != subject_fields.len() {
                        return Err(TypeError::MatchRecordPatternFieldCount {
                            record_name: pattern_type_name.to_string(),
                            expected: subject_fields.len(),
                            found: fields.len(),
                            range: range.clone(),
                        });
                    }

                    // Validate each field pattern against the field type
                    for (field_name, field_pattern) in fields {
                        let field_type = subject_fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                            .map(|(_, typ)| typ);

                        match field_type {
                            Some(typ) => Self::validate_pattern(field_pattern, typ)?,
                            None => {
                                return Err(TypeError::MatchRecordPatternUnknownField {
                                    field_name: field_name.to_string(),
                                    record_name: pattern_type_name.to_string(),
                                    range: field_pattern.range().clone(),
                                });
                            }
                        }
                    }

                    Ok(())
                }

                _ => {
                    let expected = match subject_type {
                        Type::Enum { .. } => "enum",
                        Type::Bool => "boolean",
                        Type::Option(_) => "option",
                        Type::Record { .. } => "record",
                        _ => {
                            return Err(TypeError::MatchNotImplementedForType {
                                found: subject_type.to_string(),
                                range: range.clone(),
                            });
                        }
                    };
                    Err(TypeError::MatchPatternTypeMismatch {
                        expected: expected.to_string(),
                        found: pattern.to_string(),
                        range: range.clone(),
                    })
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::dop::Parser;
    use crate::dop::symbols::field_name::FieldName;
    use crate::dop::symbols::type_name::TypeName;
    use crate::dop::syntax::parsed::ParsedExpr;
    use crate::hop::symbols::module_name::ModuleName;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(subject_type: Type, expr_str: &str, expected: Expect) {
        let mut parser = Parser::from(expr_str);
        let expr = parser.parse_expr().expect("Failed to parse expression");

        let (subject_name, subject_range, patterns, match_range) = match expr {
            ParsedExpr::Match {
                subject,
                arms,
                range,
                ..
            } => {
                let (name, subject_range) = match subject.as_ref() {
                    ParsedExpr::Var { value, range, .. } => {
                        (value.as_str().to_string(), range.clone())
                    }
                    _ => panic!("Expected variable as match subject"),
                };
                (
                    name,
                    subject_range,
                    arms.into_iter().map(|a| a.pattern).collect::<Vec<_>>(),
                    range,
                )
            }
            _ => panic!("Expected match expression"),
        };

        let result =
            Compiler::new(0).compile(&patterns, &subject_name, &subject_type, &subject_range, &match_range);

        let actual = match result {
            Ok(decision) => format_decision(&decision, 0),
            Err(e) => DocumentAnnotator::new()
                .with_label("error")
                .without_location()
                .without_line_numbers()
                .annotate(None, [e]),
        };

        expected.assert_eq(&actual);
    }

    fn format_decision(decision: &Decision, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        match decision {
            Decision::Success(body) => {
                let mut out = String::new();
                for binding in &body.bindings {
                    out.push_str(&format!("{}let {} = {}\n", pad, binding.name, binding.source_name));
                }
                out.push_str(&format!("{}branch {}\n", pad, body.value));
                out
            }
            Decision::Switch(var, cases) => {
                let mut out = String::new();
                for case in cases {
                    let args = if case.arguments.is_empty() {
                        String::new()
                    } else if let (Constructor::Record { .. }, Type::Record { fields, .. }) =
                        (&case.constructor, &var.typ)
                    {
                        // For records, show field names
                        let named: Vec<_> = fields
                            .iter()
                            .zip(case.arguments.iter())
                            .map(|((field_name, _), v)| format!("{}: {}", field_name, v.name))
                            .collect();
                        format!("({})", named.join(", "))
                    } else {
                        let names: Vec<_> =
                            case.arguments.iter().map(|v| v.name.as_str()).collect();
                        format!("({})", names.join(", "))
                    };
                    out.push_str(&format!(
                        "{}{} is {}{}\n",
                        pad, var.name, case.constructor, args
                    ));
                    out.push_str(&format_decision(&case.body, indent + 1));
                }
                out
            }
        }
    }

    // Bool tests

    #[test]
    fn bool_exhaustive() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                }
            "},
            expect![[r#"
                x is false
                  branch 1
                x is true
                  branch 0
            "#]],
        );
    }

    #[test]
    fn bool_missing_false() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    true => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: false
                match x {
                ^^^^^^^^^
                    true => 0,
                ^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn bool_missing_true() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    false => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: true
                match x {
                ^^^^^^^^^
                    false => 0,
                ^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn bool_unreachable_arm() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                    true => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'true'
                    true => 2,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn bool_wildcard_covers_all() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                branch 0
            "#]],
        );
    }

    #[test]
    fn bool_binding_covers_all() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    b => 0,
                }
            "},
            expect![[r#"
                let b = x
                branch 0
            "#]],
        );
    }

    // Option tests

    #[test]
    fn option_exhaustive() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    Some(val) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                x is Some(v0)
                  let val = v0
                  branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn option_missing_none() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    Some(val) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: None
                match x {
                ^^^^^^^^^
                    Some(val) => 0,
                ^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn option_missing_some() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    None => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(_)
                match x {
                ^^^^^^^^^
                    None => 0,
                ^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn nested_option_exhaustive() {
        check(
            Type::Option(Box::new(Type::Option(Box::new(Type::String)))),
            indoc! {"
                match x {
                    Some(Some(val)) => 0,
                    Some(None) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                x is Some(v0)
                  v0 is Some(v1)
                    let val = v1
                    branch 0
                  v0 is None
                    branch 1
                x is None
                  branch 2
            "#]],
        );
    }

    // Enum tests

    #[test]
    fn enum_exhaustive() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Green => 1,
                    Color::Blue => 2,
                }
            "},
            expect![[r#"
                x is Color::Red
                  branch 0
                x is Color::Green
                  branch 1
                x is Color::Blue
                  branch 2
            "#]],
        );
    }

    #[test]
    fn enum_missing_variant() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Green => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue
                match x {
                ^^^^^^^^^
                    Color::Red => 0,
                ^^^^^^^^^^^^^^^^^^^^
                    Color::Green => 1,
                ^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_wildcard_covers_remaining() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Red => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                x is Color::Red
                  branch 0
                x is Color::Green
                  branch 1
                x is Color::Blue
                  branch 1
            "#]],
        );
    }

    #[test]
    fn enum_unreachable_after_wildcard() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    _ => 0,
                    Color::Red => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'Color::Red'
                    Color::Red => 1,
                    ^^^^^^^^^^
            "#]],
        );
    }

    // Record tests

    #[test]
    fn record_match() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                ],
            },
            indoc! {"
                match x {
                    User(name: n, age: a) => 0,
                }
            "},
            expect![[r#"
                x is User(name: v0, age: v1)
                  let n = v0
                  let a = v1
                  branch 0
            "#]],
        );
    }

    #[test]
    fn record_with_bool_fields_exhaustive() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Foo").unwrap(),
                fields: vec![
                    (FieldName::new("a").unwrap(), Type::Bool),
                    (FieldName::new("b").unwrap(), Type::Bool),
                ],
            },
            indoc! {"
                match x {
                    Foo(a: true, b: true) => 0,
                    Foo(a: true, b: false) => 1,
                    Foo(a: false, b: true) => 2,
                    Foo(a: false, b: false) => 3,
                }
            "},
            expect![[r#"
                x is Foo(a: v0, b: v1)
                  v1 is false
                    v0 is false
                      branch 3
                    v0 is true
                      branch 1
                  v1 is true
                    v0 is false
                      branch 2
                    v0 is true
                      branch 0
            "#]],
        );
    }

    // Additional Bool tests

    #[test]
    fn bool_true_with_wildcard() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    true => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                x is false
                  branch 1
                x is true
                  branch 0
            "#]],
        );
    }

    #[test]
    fn bool_duplicate_false() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                    false => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'false'
                    false => 2,
                    ^^^^^
            "#]],
        );
    }

    #[test]
    fn bool_unreachable_after_wildcard() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    _ => 0,
                    true => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'true'
                    true => 1,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn bool_unreachable_after_binding() {
        check(
            Type::Bool,
            indoc! {"
                match x {
                    b => 0,
                    true => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'true'
                    true => 1,
                    ^^^^
            "#]],
        );
    }

    // Additional Option tests

    #[test]
    fn option_wildcard_covers_all() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                branch 0
            "#]],
        );
    }

    #[test]
    fn option_some_with_wildcard() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    Some(v) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                x is Some(v0)
                  let v = v0
                  branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn option_duplicate_some() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    Some(_) => 0,
                    Some(_) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'Some(_)'
                    Some(_) => 1,
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn option_duplicate_none() {
        check(
            Type::Option(Box::new(Type::String)),
            indoc! {"
                match x {
                    Some(_) => 0,
                    None => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'None'
                    None => 2,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn nested_option_missing_some_none() {
        check(
            Type::Option(Box::new(Type::Option(Box::new(Type::Int)))),
            indoc! {"
                match x {
                    Some(Some(_)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(None)
                match x {
                ^^^^^^^^^
                    Some(Some(_)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^
                    None => 1,
                ^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn nested_option_with_bool_missing() {
        check(
            Type::Option(Box::new(Type::Option(Box::new(Type::Bool)))),
            indoc! {"
                match x {
                    Some(Some(false)) => 0,
                    Some(None) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(Some(true))
                match x {
                ^^^^^^^^^
                    Some(Some(false)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Some(None) => 1,
                ^^^^^^^^^^^^^^^^^^^^
                    None => 2,
                ^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn nested_option_with_bool_exhaustive() {
        check(
            Type::Option(Box::new(Type::Option(Box::new(Type::Bool)))),
            indoc! {"
                match x {
                    Some(Some(true)) => 0,
                    Some(Some(false)) => 1,
                    Some(None) => 2,
                    None => 3,
                }
            "},
            expect![[r#"
                x is Some(v0)
                  v0 is Some(v1)
                    v1 is false
                      branch 1
                    v1 is true
                      branch 0
                  v0 is None
                    branch 2
                x is None
                  branch 3
            "#]],
        );
    }

    // Additional Enum tests

    #[test]
    fn enum_binding_covers_all() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    c => 0,
                }
            "},
            expect![[r#"
                let c = x
                branch 0
            "#]],
        );
    }

    #[test]
    fn enum_duplicate_variant() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Red => 1,
                    Color::Green => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'Color::Red'
                    Color::Red => 1,
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_multiple_wildcards() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    _ => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant '_'
                    _ => 1,
                    ^
            "#]],
        );
    }

    #[test]
    fn enum_missing_multiple_variants() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                    TypeName::new("Blue").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Red => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue, Color::Green
                match x {
                ^^^^^^^^^
                    Color::Red => 0,
                ^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    // Additional Record tests

    #[test]
    fn record_with_wildcard_fields() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                ],
            },
            indoc! {"
                match x {
                    User(name: _, age: _) => 0,
                }
            "},
            expect![[r#"
                x is User(name: v0, age: v1)
                  branch 0
            "#]],
        );
    }

    #[test]
    fn record_with_nested_option() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (
                        FieldName::new("email").unwrap(),
                        Type::Option(Box::new(Type::String)),
                    ),
                ],
            },
            indoc! {"
                match x {
                    User(name: n, email: Some(e)) => 0,
                    User(name: n, email: None) => 1,
                }
            "},
            expect![[r#"
                x is User(name: v0, email: v1)
                  v1 is Some(v2)
                    let n = v0
                    let e = v2
                    branch 0
                  v1 is None
                    let n = v0
                    branch 1
            "#]],
        );
    }

    #[test]
    fn record_with_option_field_missing_none() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (
                        FieldName::new("email").unwrap(),
                        Type::Option(Box::new(Type::String)),
                    ),
                ],
            },
            indoc! {"
                match x {
                    User(name: n, email: Some(e)) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: User(name: _, email: None)
                match x {
                ^^^^^^^^^
                    User(name: n, email: Some(e)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn record_with_bool_fields_missing() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Foo").unwrap(),
                fields: vec![
                    (FieldName::new("a").unwrap(), Type::Bool),
                    (FieldName::new("b").unwrap(), Type::Bool),
                ],
            },
            indoc! {"
                match x {
                    Foo(a: true, b: true) => 0,
                    Foo(a: true, b: false) => 1,
                    Foo(a: false, b: true) => 2,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Foo(a: false, b: false)
                match x {
                ^^^^^^^^^
                    Foo(a: true, b: true) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Foo(a: true, b: false) => 1,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Foo(a: false, b: true) => 2,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn option_of_record_exhaustive() {
        check(
            Type::Option(Box::new(Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                ],
            })),
            indoc! {"
                match x {
                    Some(User(name: n, age: a)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                x is Some(v0)
                  v0 is User(name: v1, age: v2)
                    let n = v1
                    let a = v2
                    branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    // Pattern validation tests

    #[test]
    fn validation_undefined_enum_variant() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Color::Blue => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Variant 'Blue' is not defined in enum 'Color'
                    Color::Blue => 0,
                    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn validation_record_missing_fields() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                ],
            },
            indoc! {"
                match x {
                    User(name: n) => 0,
                }
            "},
            expect![[r#"
                error: Record pattern for 'User' must specify all 2 fields, but only 1 were provided
                    User(name: n) => 0,
                    ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn validation_record_unknown_field() {
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![(FieldName::new("name").unwrap(), Type::String)],
            },
            indoc! {"
                match x {
                    User(email: e) => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'email' in record pattern for 'User'
                    User(email: e) => 0,
                                ^
            "#]],
        );
    }

    #[test]
    fn validation_boolean_pattern_on_enum() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    true => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found true
                    true => 0,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn validation_option_pattern_on_enum() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    TypeName::new("Red").unwrap(),
                    TypeName::new("Green").unwrap(),
                ],
            },
            indoc! {"
                match x {
                    Some(v) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found Some(v)
                    Some(v) => 0,
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn validation_nested_option_wrong_inner_type() {
        check(
            Type::Option(Box::new(Type::Bool)),
            indoc! {"
                match x {
                    Some(Some(v)) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(v)
                    Some(Some(v)) => 0,
                         ^^^^^^^
            "#]],
        );
    }

    // Subject type validation tests

    #[test]
    fn match_not_implemented_for_int() {
        check(
            Type::Int,
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Match is not implemented for type Int
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn match_not_implemented_for_string() {
        check(
            Type::String,
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Match is not implemented for type String
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn match_not_implemented_for_float() {
        check(
            Type::Float,
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Match is not implemented for type Float
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn match_no_arms() {
        check(
            Type::Bool,
            "match x {}",
            expect![[r#"
                error: Match expression must have at least one arm
                match x {}
                ^^^^^^^^^^
            "#]],
        );
    }
}
