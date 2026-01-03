#![allow(clippy::new_without_default)]

//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from <https://github.com/yorickpeterse/pattern-matching-in-rust/>.
//! Thanks to Yorick Peterse for the original implementation.
use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::dop::symbols::type_name::TypeName;
use crate::dop::syntax::parsed::Constructor;

use super::r#type::Type;

/// The body of code to evaluate in case of a match.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Body {
    /// The branch to run in case of a match.
    value: usize,
}

/// A user defined pattern such as `Some(x)`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    /// A pattern such as `Some(true)`.
    Constructor(Constructor, Vec<Pattern>),
    /// A wildcard pattern, i.e. `_`.
    Wildcard,
}

/// A variable used in a match expression.
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Variable(pub String);

/// A single case (or row) in a match expression/table.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Row {
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
            .position(|c| &c.variable == variable)
            .map(|idx| self.columns.remove(idx))
    }
}

/// A column in a pattern matching table.
///
/// A column contains a single variable to test, and a pattern to test against
/// that variable. A row may contain multiple columns, though this wouldn't be
/// exposed to the source language.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Column {
    variable: Variable,
    pattern: Pattern,
}

impl Column {
    fn new(variable: Variable, pattern: Pattern) -> Self {
        Self { variable, pattern }
    }
}

/// A case in a decision tree to test against a variable.
#[derive(Eq, PartialEq, Debug)]
pub struct Case {
    /// The constructor to test against an input variable.
    constructor: Constructor,

    /// Variables to introduce to the body of this case.
    arguments: Vec<Variable>,

    /// The sub tree of this case.
    body: Decision,
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
#[derive(Eq, PartialEq, Debug)]
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
}

impl Term {
    fn new(variable: Variable, name: String, arguments: Vec<Variable>) -> Self {
        Self {
            variable,
            name,
            arguments,
        }
    }

    fn pattern_name(&self, terms: &[Term], mapping: &HashMap<&Variable, usize>) -> String {
        if self.arguments.is_empty() {
            self.name.to_string()
        } else {
            let args = self
                .arguments
                .iter()
                .map(|arg| {
                    mapping
                        .get(arg)
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
                let mut mapping = HashMap::new();

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
                    mapping.insert(&step.variable, index);
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
    fn constructor_index(
        &self,
        cons: &Constructor,
        type_env: &HashMap<TypeName, Type>,
    ) -> usize {
        match cons {
            Constructor::BooleanFalse => 0,
            Constructor::BooleanTrue => 1,
            Constructor::OptionSome => 0,
            Constructor::OptionNone => 1,
            Constructor::EnumVariant {
                enum_name,
                variant_name,
            } => {
                let typ = type_env.get(enum_name).expect("unknown enum");
                if let Type::Enum { variants, .. } = typ {
                    variants
                        .iter()
                        .position(|v| v.as_str() == variant_name)
                        .expect("unknown variant")
                } else {
                    panic!("type is not an enum")
                }
            }
        }
    }

    pub fn compile(
        mut self,
        rows: Vec<Row>,
        type_env: &HashMap<TypeName, Type>,
        var_env: &mut HashMap<Variable, Type>,
    ) -> Match {
        Match {
            tree: self.compile_rows(rows, type_env, var_env),
            diagnostics: self.diagnostics,
        }
    }

    fn compile_rows(
        &mut self,
        mut rows: Vec<Row>,
        type_env: &HashMap<TypeName, Type>,
        var_env: &mut HashMap<Variable, Type>,
    ) -> Decision {
        if rows.is_empty() {
            self.diagnostics.missing = true;
            return Decision::Failure;
        }

        for row in &mut rows {
            // Remove columns that contain wildcards
            row.columns
                .retain(|col| !matches!(&col.pattern, Pattern::Wildcard));
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

        let mut cases = match var_env.get(&branch_var).expect("unknown variable").clone() {
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
                        vec![self.fresh_var(*inner, var_env)],
                        Vec::new(),
                    ),
                    (Constructor::OptionNone, Vec::new(), Vec::new()),
                ]
            }
            Type::Enum {
                name,
                variants,
                ..
            } => variants
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
            Type::String | Type::Int | Type::Float | Type::TrustedHTML | Type::Array(_) | Type::Record { .. } => {
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
                if let Pattern::Constructor(cons, args) = col.pattern {
                    let idx = self.constructor_index(&cons, type_env);
                    let mut cols = row.columns;

                    for (var, pat) in cases[idx].1.iter().zip(args.into_iter()) {
                        cols.push(Column::new(var.clone(), pat));
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
        let mut counts = HashMap::new();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable).or_insert(0_usize) += 1
            }
        }

        rows[0]
            .columns
            .iter()
            .map(|col| col.variable.clone())
            .max_by_key(|var| counts[var])
            .unwrap()
    }

    /// Returns a new variable to use in the decision tree.
    fn fresh_var(&mut self, typ: Type, var_env: &mut HashMap<Variable, Type>) -> Variable {
        let var = Variable(format!("v{}", self.var_counter));

        self.var_counter += 1;
        var_env.insert(var.clone(), typ);
        var
    }
}

// DISPLAY --------------------------------------------------------------------

impl Decision {
    fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let pad = "  ".repeat(indent);
        match self {
            Decision::Success(body) => write!(f, "{pad}{body}"),
            Decision::Failure => write!(f, "{pad}fail"),
            Decision::Switch(var, cases) => {
                writeln!(f, "{pad}switch {} {{", var.0)?;
                for case in cases {
                    case.fmt_indented(f, indent + 1)?;
                }
                write!(f, "{pad}}}")
            }
        }
    }
}

impl fmt::Display for Decision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indented(f, 0)?;
        writeln!(f)
    }
}

impl Case {
    fn fmt_indented(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let pad = "  ".repeat(indent);
        let args = if self.arguments.is_empty() {
            String::new()
        } else {
            format!(
                "({})",
                self.arguments
                    .iter()
                    .map(|v| v.0.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        writeln!(f, "{pad}{}{args} => {{", self.constructor)?;
        self.body.fmt_indented(f, indent + 1)?;
        writeln!(f)?;
        writeln!(f, "{pad}}}")
    }
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "body({})", self.value)
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Constructor(cons, args) => {
                write!(f, "{cons}")?;
                if !args.is_empty() {
                    let args: Vec<_> = args.iter().map(|a| a.to_string()).collect();
                    write!(f, "({})", args.join(", "))?;
                }
                Ok(())
            }
            Pattern::Wildcard => write!(f, "_"),
        }
    }
}

// TESTS ----------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::symbols::module_name::ModuleName;
    use expect_test::expect;

    fn test_module() -> ModuleName {
        ModuleName::new("test").unwrap()
    }

    fn tt() -> Pattern {
        Pattern::Constructor(Constructor::BooleanTrue, Vec::new())
    }

    fn ff() -> Pattern {
        Pattern::Constructor(Constructor::BooleanFalse, Vec::new())
    }

    fn wild() -> Pattern {
        Pattern::Wildcard
    }

    fn some(inner: Pattern) -> Pattern {
        Pattern::Constructor(Constructor::OptionSome, vec![inner])
    }

    fn none() -> Pattern {
        Pattern::Constructor(Constructor::OptionNone, Vec::new())
    }

    fn variant(enum_name: &str, variant_name: &str, args: Vec<Pattern>) -> Pattern {
        Pattern::Constructor(
            Constructor::EnumVariant {
                enum_name: TypeName::new(enum_name).unwrap(),
                variant_name: variant_name.to_string(),
            },
            args,
        )
    }

    fn new_variable(var_env: &mut HashMap<Variable, Type>, name: &str, typ: Type) -> Variable {
        let var = Variable(name.to_string());
        var_env.insert(var.clone(), typ);
        var
    }

    fn compile(
        compiler: Compiler,
        input: Variable,
        rules: Vec<Pattern>,
        type_env: &HashMap<TypeName, Type>,
        var_env: &mut HashMap<Variable, Type>,
    ) -> Match {
        let rows = rules
            .into_iter()
            .enumerate()
            .map(|(idx, pat)| Row::new(vec![Column::new(input.clone(), pat)], Body { value: idx }))
            .collect();

        compiler.compile(rows, type_env, var_env)
    }

    fn check(
        compiler: Compiler,
        input: Variable,
        rules: Vec<Pattern>,
        type_env: &HashMap<TypeName, Type>,
        var_env: &mut HashMap<Variable, Type>,
        expected: expect_test::Expect,
    ) {
        let mut output = format!("-- input --\nmatch {} {{\n", input.0);
        for (idx, rule) in rules.iter().enumerate() {
            output.push_str(&format!("  {rule} => {idx}\n"));
        }
        output.push_str("}\n-- output --\n");

        let result = compile(compiler, input, rules, type_env, var_env);
        output.push_str(&result.tree.to_string());

        output.push_str("-- reachable --\n");
        let reachable: Vec<_> = result
            .diagnostics
            .reachable
            .iter()
            .map(|i| i.to_string())
            .collect();
        output.push_str(&format!("{}\n", reachable.join(", ")));

        if result.diagnostics.missing {
            output.push_str("-- missing --\n");
            output.push_str(&format!("{}\n", result.missing_patterns().join(", ")));
        }

        expected.assert_eq(&output);
    }

    // ------------------------------------------------------------------------

    #[test]
    fn test_compile_simple_pattern() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![tt(), ff()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  true => 0
                  false => 1
                }
                -- output --
                switch in {
                  false => {
                    body(1)
                  }
                  true => {
                    body(0)
                  }
                }
                -- reachable --
                1, 0
            "#]],
        );
    }

    #[test]
    fn test_compile_nonexhaustive_pattern() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![tt()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  true => 0
                }
                -- output --
                switch in {
                  false => {
                    fail
                  }
                  true => {
                    body(0)
                  }
                }
                -- reachable --
                0
                -- missing --
                false
            "#]],
        );
    }

    #[test]
    fn test_compile_redundant_pattern() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![tt(), tt(), ff()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  true => 0
                  true => 1
                  false => 2
                }
                -- output --
                switch in {
                  false => {
                    body(2)
                  }
                  true => {
                    body(0)
                  }
                }
                -- reachable --
                2, 0
            "#]],
        );
    }

    #[test]
    fn test_compile_wildcard_pattern() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![tt(), wild()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  true => 0
                  _ => 1
                }
                -- output --
                switch in {
                  false => {
                    body(1)
                  }
                  true => {
                    body(0)
                  }
                }
                -- reachable --
                1, 0
            "#]],
        );
    }

    #[test]
    fn test_compile_nonexhaustive_option_type() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Option(Box::new(Type::Bool)));
        check(
            compiler,
            input,
            vec![some(tt())],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(true) => 0
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    switch v0 {
                      false => {
                        fail
                      }
                      true => {
                        body(0)
                      }
                    }
                  }
                  None => {
                    fail
                  }
                }
                -- reachable --
                0
                -- missing --
                None, Some(false)
            "#]],
        );
    }

    #[test]
    fn test_compile_exhaustive_option_type() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Option(Box::new(Type::Bool)));
        check(
            compiler,
            input,
            vec![some(tt()), some(wild()), none()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(true) => 0
                  Some(_)(_) => 1
                  None => 2
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    switch v0 {
                      false => {
                        body(1)
                      }
                      true => {
                        body(0)
                      }
                    }
                  }
                  None => {
                    body(2)
                  }
                }
                -- reachable --
                1, 0, 2
            "#]],
        );
    }

    #[test]
    fn test_compile_redundant_option_type_with_bool() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Option(Box::new(Type::Bool)));
        check(
            compiler,
            input,
            vec![some(tt()), some(tt()), some(wild()), none()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(true) => 0
                  Some(_)(true) => 1
                  Some(_)(_) => 2
                  None => 3
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    switch v0 {
                      false => {
                        body(2)
                      }
                      true => {
                        body(0)
                      }
                    }
                  }
                  None => {
                    body(3)
                  }
                }
                -- reachable --
                2, 0, 3
            "#]],
        );
    }

    #[test]
    fn test_compile_exhaustive_option_type_with_wildcard() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Option(Box::new(Type::Bool)));
        check(
            compiler,
            input,
            vec![some(tt()), wild()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(true) => 0
                  _ => 1
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    switch v0 {
                      false => {
                        body(1)
                      }
                      true => {
                        body(0)
                      }
                    }
                  }
                  None => {
                    body(1)
                  }
                }
                -- reachable --
                1, 0, 1
            "#]],
        );
    }

    #[test]
    fn test_three_variant_enum() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let traffic_light = Type::Enum {
            module: test_module(),
            name: TypeName::new("Light").unwrap(),
            variants: vec![
                TypeName::new("Red").unwrap(),
                TypeName::new("Yellow").unwrap(),
                TypeName::new("Green").unwrap(),
            ],
        };
        let mut type_env = HashMap::new();
        type_env.insert(TypeName::new("Light").unwrap(), traffic_light.clone());
        let input = new_variable(&mut var_env, "in", traffic_light);
        check(
            compiler,
            input,
            vec![
                variant("Light", "Red", Vec::new()),
                variant("Light", "Green", Vec::new()),
            ],
            &type_env,
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Light::Red => 0
                  Light::Green => 1
                }
                -- output --
                switch in {
                  Light::Red => {
                    body(0)
                  }
                  Light::Yellow => {
                    fail
                  }
                  Light::Green => {
                    body(1)
                  }
                }
                -- reachable --
                0, 1
                -- missing --
                Yellow
            "#]],
        );
    }

    #[test]
    fn test_first_match_wins() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![wild(), wild(), tt(), ff()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  _ => 0
                  _ => 1
                  true => 2
                  false => 3
                }
                -- output --
                body(0)
                -- reachable --
                0
            "#]],
        );
    }

    #[test]
    fn test_result_type() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let result_type = Type::Enum {
            module: test_module(),
            name: TypeName::new("Result").unwrap(),
            variants: vec![
                TypeName::new("Ok").unwrap(),
                TypeName::new("Err").unwrap(),
            ],
        };
        let mut type_env = HashMap::new();
        type_env.insert(TypeName::new("Result").unwrap(), result_type.clone());
        let input = new_variable(&mut var_env, "in", result_type);
        check(
            compiler,
            input,
            vec![
                variant("Result", "Ok", Vec::new()),
                variant("Result", "Err", Vec::new()),
            ],
            &type_env,
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Result::Ok => 0
                  Result::Err => 1
                }
                -- output --
                switch in {
                  Result::Ok => {
                    body(0)
                  }
                  Result::Err => {
                    body(1)
                  }
                }
                -- reachable --
                0, 1
            "#]],
        );
    }

    #[test]
    fn test_nested_option() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(
            &mut var_env,
            "in",
            Type::Option(Box::new(Type::Option(Box::new(Type::Bool)))),
        );
        check(
            compiler,
            input,
            vec![some(some(tt())), some(none()), none()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(Some(_)(true)) => 0
                  Some(_)(None) => 1
                  None => 2
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    switch v0 {
                      Some(_)(v1) => {
                        switch v1 {
                          false => {
                            fail
                          }
                          true => {
                            body(0)
                          }
                        }
                      }
                      None => {
                        body(1)
                      }
                    }
                  }
                  None => {
                    body(2)
                  }
                }
                -- reachable --
                0, 1, 2
                -- missing --
                Some(Some(false))
            "#]],
        );
    }

    #[test]
    fn test_all_wildcards() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![wild()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  _ => 0
                }
                -- output --
                body(0)
                -- reachable --
                0
            "#]],
        );
    }

    #[test]
    fn test_wildcard() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Bool);
        check(
            compiler,
            input,
            vec![tt(), wild()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  true => 0
                  _ => 1
                }
                -- output --
                switch in {
                  false => {
                    body(1)
                  }
                  true => {
                    body(0)
                  }
                }
                -- reachable --
                1, 0
            "#]],
        );
    }

    #[test]
    fn test_wildcard_in_constructor() {
        let compiler = Compiler::new();
        let mut var_env = HashMap::new();
        let input = new_variable(&mut var_env, "in", Type::Option(Box::new(Type::Bool)));
        check(
            compiler,
            input,
            vec![some(wild()), none()],
            &HashMap::new(),
            &mut var_env,
            expect![[r#"
                -- input --
                match in {
                  Some(_)(_) => 0
                  None => 1
                }
                -- output --
                switch in {
                  Some(_)(v0) => {
                    body(0)
                  }
                  None => {
                    body(1)
                  }
                }
                -- reachable --
                0, 1
            "#]],
        );
    }
}
