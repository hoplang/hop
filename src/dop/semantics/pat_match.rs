#![allow(clippy::new_without_default)]

//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from <https://github.com/yorickpeterse/pattern-matching-in-rust/>.
//! Thanks to Yorick Peterse for the original implementation.
use std::collections::{HashMap, HashSet};

use crate::dop::syntax::parsed::{Constructor, ParsedMatchPattern};
use crate::environment::Environment;

use super::r#type::Type;

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
pub struct Row {
    columns: Vec<Column>,
    body: Body,
}

impl Row {
    pub fn new(columns: Vec<Column>, body: Body) -> Self {
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
pub struct Column {
    variable: Variable,
    pattern: ParsedMatchPattern,
}

impl Column {
    pub fn new(variable: Variable, pattern: ParsedMatchPattern) -> Self {
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

/// A decision tree compiled from a list of match cases.
#[derive(Debug)]
pub enum Decision {
    /// A pattern is matched and the right-hand value is to be returned.
    Success(Body),

    /// A pattern is missing.
    Failure,

    /// Checks if a value is any of the given patterns.
    ///
    /// The values are as follows:
    ///
    /// 1. The variable to test.
    /// 2. The cases to test against this variable.
    Switch(Variable, Vec<Case>),
}

/// A type for storing diagnostics produced by the decision tree compiler.
pub struct Diagnostics {
    /// A flag indicating the match is missing one or more pattern.
    missing: bool,

    /// The right-hand sides that are reachable.
    ///
    /// If a right-hand side isn't in this list it means its pattern is
    /// redundant.
    reachable: Vec<usize>,
}

impl Diagnostics {
    /// Returns true if the match is missing one or more patterns.
    pub fn is_missing(&self) -> bool {
        self.missing
    }

    /// Returns the indices of unreachable (redundant) arms.
    pub fn unreachable(&self, total_arms: usize) -> Vec<usize> {
        (0..total_arms)
            .filter(|i| !self.reachable.contains(i))
            .collect()
    }
}

/// The result of compiling a pattern match expression.
pub struct Match {
    pub tree: Decision,
    pub diagnostics: Diagnostics,
}

/// Information about a single constructor/value (aka term) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
struct Term {
    variable: Variable,
    name: String,
    arguments: Vec<Variable>,
    /// Field names for record constructors (None for non-record constructors)
    field_names: Option<Vec<String>>,
}

impl Term {
    fn new(variable: Variable, name: String, arguments: Vec<Variable>) -> Self {
        Self {
            variable,
            name,
            arguments,
            field_names: None,
        }
    }

    fn new_record(
        variable: Variable,
        name: String,
        arguments: Vec<Variable>,
        field_names: Vec<String>,
    ) -> Self {
        Self {
            variable,
            name,
            arguments,
            field_names: Some(field_names),
        }
    }

    fn pattern_name(&self, terms: &[Term], mapping: &HashMap<&str, usize>) -> String {
        if self.arguments.is_empty() {
            self.name.to_string()
        } else if let Some(field_names) = &self.field_names {
            // Record pattern with named fields: User(name: x, age: y)
            let args = self
                .arguments
                .iter()
                .zip(field_names.iter())
                .map(|(arg, field_name)| {
                    let pattern = mapping
                        .get(arg.name.as_str())
                        .map(|&idx| terms[idx].pattern_name(terms, mapping))
                        .unwrap_or_else(|| "_".to_string());
                    format!("{}: {}", field_name, pattern)
                })
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}({})", self.name, args)
        } else {
            // Positional pattern: Some(x)
            let args = self
                .arguments
                .iter()
                .map(|arg| {
                    mapping
                        .get(arg.name.as_str())
                        .map(|&idx| terms[idx].pattern_name(terms, mapping))
                        .unwrap_or_else(|| "_".to_string())
                })
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}({})", self.name, args)
        }
    }
}

impl Match {
    /// Returns a list of patterns not covered by the match expression.
    pub fn missing_patterns(&self) -> Vec<String> {
        let mut names = HashSet::new();
        let mut steps = Vec::new();

        Self::collect_missing_patterns(&self.tree, &mut steps, &mut names);

        let mut missing: Vec<String> = names.into_iter().collect();

        // Sorting isn't necessary, but it makes it a bit easier to write tests.
        missing.sort();
        missing
    }

    fn collect_missing_patterns(
        node: &Decision,
        terms: &mut Vec<Term>,
        missing: &mut HashSet<String>,
    ) {
        match node {
            Decision::Success(_) => {}
            Decision::Failure => {
                let mut mapping: HashMap<&str, usize> = HashMap::new();

                // At this point the terms stack looks something like this:
                // `[term, term + arguments, term, ...]`. To construct a pattern
                // name from this stack, we first map all variables to their
                // term indexes. This is needed because when a term defines
                // arguments, the terms for those arguments don't necessarily
                // appear in order in the term stack.
                //
                // This mapping is then used when (recursively) generating a
                // pattern name.
                //
                // This approach could probably be done more efficiently, so if
                // you're reading this and happen to know of a way, please
                // submit a merge request :)
                for (index, step) in terms.iter().enumerate() {
                    mapping.insert(&step.variable.name, index);
                }

                let name = terms
                    .first()
                    .map(|term| term.pattern_name(terms, &mapping))
                    .unwrap_or_else(|| "_".to_string());

                missing.insert(name);
            }
            Decision::Switch(var, cases) => {
                for case in cases {
                    match &case.constructor {
                        Constructor::BooleanTrue => {
                            let name = "true".to_string();

                            terms.push(Term::new(var.clone(), name, Vec::new()));
                        }
                        Constructor::BooleanFalse => {
                            let name = "false".to_string();

                            terms.push(Term::new(var.clone(), name, Vec::new()));
                        }
                        Constructor::OptionSome => {
                            let args = case.arguments.clone();
                            terms.push(Term::new(var.clone(), "Some".to_string(), args));
                        }
                        Constructor::OptionNone => {
                            terms.push(Term::new(var.clone(), "None".to_string(), Vec::new()));
                        }
                        Constructor::EnumVariant { variant_name, .. } => {
                            let args = case.arguments.clone();
                            terms.push(Term::new(var.clone(), variant_name.clone(), args));
                        }
                        Constructor::Record { type_name } => {
                            let args = case.arguments.clone();
                            // Extract field names from the variable's type
                            let field_names = if let Type::Record { fields, .. } = &var.typ {
                                fields.iter().map(|(name, _)| name.to_string()).collect()
                            } else {
                                Vec::new()
                            };
                            terms.push(Term::new_record(
                                var.clone(),
                                type_name.as_str().to_string(),
                                args,
                                field_names,
                            ));
                        }
                    }

                    Self::collect_missing_patterns(&case.body, terms, missing);
                    terms.pop();
                }
            }
        }
    }
}

/// The `match` compiler itself.
pub struct Compiler {
    // A counter used to construct unused variable names.
    var_counter: usize,
    // The diagnostics collected during compilation.
    diagnostics: Diagnostics,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            var_counter: 0,
            diagnostics: Diagnostics {
                missing: false,
                reachable: Vec::new(),
            },
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
        rows: Vec<Row>,
        type_env: &mut Environment<Type>,
        var_env: &mut Environment<Type>,
    ) -> Match {
        Match {
            tree: self.compile_rows(rows, type_env, var_env),
            diagnostics: self.diagnostics,
        }
    }

    fn compile_rows(
        &mut self,
        mut rows: Vec<Row>,
        type_env: &mut Environment<Type>,
        var_env: &mut Environment<Type>,
    ) -> Decision {
        if rows.is_empty() {
            self.diagnostics.missing = true;
            return Decision::Failure;
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
            self.diagnostics.reachable.push(row.body.value);
            return Decision::Success(row.body);
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
                        vec![self.fresh_var(inner.as_ref().clone(), var_env)],
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
                    .map(|(_, field_type)| self.fresh_var(field_type.clone(), var_env))
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

        Decision::Switch(
            branch_var,
            cases
                .into_iter()
                .map(|(cons, vars, rows)| {
                    Case::new(cons, vars, self.compile_rows(rows, type_env, var_env))
                })
                .collect(),
        )
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
    fn fresh_var(&mut self, typ: Type, var_env: &mut Environment<Type>) -> Variable {
        let name = format!("v{}", self.var_counter);
        self.var_counter += 1;
        let _ = var_env.push(name.clone(), typ.clone());
        Variable::new(name, typ)
    }
}
