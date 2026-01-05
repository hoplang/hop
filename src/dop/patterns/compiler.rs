//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from <https://github.com/yorickpeterse/pattern-matching-in-rust/>.
//! Thanks to Yorick Peterse for the original implementation.
use std::collections::{HashMap, HashSet};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::syntax::parsed::{Constructor, ParsedMatchArm, ParsedMatchPattern};
use crate::environment::Environment;

use crate::dop::semantics::r#type::Type;
use crate::dop::semantics::type_error::TypeError;

/// The body of code to evaluate in case of a match.
#[derive(Clone, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    /// The tuples are in the form `(name, source)` (i.e `x = source`).
    pub bindings: Vec<(String, Variable)>,
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

    /// Returns the index of a constructor.
    fn constructor_index(&self, cons: &Constructor, type_env: &mut Environment<Type>) -> usize {
        match cons {
            Constructor::BooleanFalse => 0,
            Constructor::BooleanTrue => 1,
            Constructor::OptionSome => 0,
            Constructor::OptionNone => 1,
            Constructor::EnumVariant {
                enum_name,
                variant_name,
            } => {
                let typ = type_env.lookup(enum_name.as_str()).expect("unknown enum");
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
        arms: &[ParsedMatchArm],
        subject_var: &Variable,
        match_range: &DocumentRange,
        type_env: &mut Environment<Type>,
    ) -> Result<Decision, TypeError> {
        let rows: Vec<Row> = arms
            .iter()
            .enumerate()
            .map(|(idx, arm)| {
                Row::new(
                    vec![Column::new(subject_var.clone(), arm.pattern.clone())],
                    Body::new(idx),
                )
            })
            .collect();

        let tree = self.compile_rows(rows, type_env, Vec::new());

        // Check for unreachable arms
        let unreachable: Vec<usize> = (0..arms.len())
            .filter(|i| !self.reachable.contains(i))
            .collect();
        if let Some(&first_unreachable) = unreachable.first() {
            let arm = &arms[first_unreachable];
            return Err(TypeError::MatchUnreachableArm {
                variant: arm.pattern.to_string(),
                range: arm.pattern.range().clone(),
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
        type_env: &mut Environment<Type>,
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
                    row.body.bindings.push((name.clone(), col.variable.clone()));
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
                    let idx = self.constructor_index(&cons, type_env);
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

                let body = self.compile_rows(rows, type_env, new_path);
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
}
