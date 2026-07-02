//! An implementation of the algorithm described at
//! https://julesjacobs.com/notes/patternmatching/patternmatching.pdf.
//!
//! Adapted from <https://github.com/yorickpeterse/pattern-matching-in-rust/>.
//! Thanks to Yorick Peterse for the original implementation.
//!
//! NOTE:
//! The match compiler will always reject useless matching (i.e. when
//! the match does not branch and does not bind any variable). This is
//! necessary to not generate useless variable bindings (which is compile
//! errors in some languages). Make sure that this invariant holds when
//! introducing new match subjects.
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::document::DocumentRange;
use crate::expr::parsing::parsed_expr::Constructor;
use crate::expr::patterns::typed::TypedMatchPattern;

use crate::expr::typing::r#type::{NamedKind, Type};
use crate::expr::typing::type_registry::TypeRegistry;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};
use crate::variable_scope::FreshVarCounter;

/// A binding introduced by a pattern match (i.e. `name = source_name`).
#[derive(Clone, Debug)]
pub struct Binding {
    /// The name of the variable to bind.
    pub name: VarName,
    /// The name of the source variable to bind from.
    pub source_name: VarName,
    /// The type of the binding.
    pub typ: Arc<Type>,
}

impl Binding {
    pub fn new(name: VarName, source_name: VarName, typ: Arc<Type>) -> Self {
        Self {
            name,
            source_name,
            typ,
        }
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
    pub name: VarName,
    pub typ: Arc<Type>,
    /// Whether this variable's pattern introduces no bindings.
    /// When true, the variable should not generate a binding in the output.
    is_free_from_bindings: bool,
}

impl Variable {
    pub fn new(name: VarName, typ: Arc<Type>) -> Self {
        Self {
            name,
            typ,
            is_free_from_bindings: false,
        }
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
    pattern: TypedMatchPattern,
}

impl Column {
    fn new(variable: Variable, pattern: TypedMatchPattern) -> Self {
        Self { variable, pattern }
    }
}

/// A case for boolean pattern matching - no bindings possible.
#[derive(Debug)]
pub struct BoolCase {
    pub body: Decision,
}

/// A case for the Some variant of Option - exactly one potential binding.
#[derive(Debug)]
pub struct OptionSomeCase {
    /// The name to bind the inner value to, or None if wildcard pattern.
    pub bound_name: Option<VarName>,
    pub body: Decision,
}

/// A case for the None variant of Option - no bindings.
#[derive(Debug)]
pub struct OptionNoneCase {
    pub body: Decision,
}

/// A binding for a field in a record or enum variant.
#[derive(Debug, Clone)]
pub struct FieldBinding {
    /// The field name this binding corresponds to.
    pub field_name: FieldName,
    /// The name to bind this field's value to, or None if wildcard pattern.
    pub bound_name: Option<VarName>,
}

/// A case for an enum variant - may have multiple field bindings.
#[derive(Debug)]
pub struct EnumCase {
    pub enum_name: TypeName,
    pub variant_name: TypeName,
    /// Bindings for each field in the variant.
    pub bindings: Vec<FieldBinding>,
    pub body: Decision,
}

/// A case for record destructuring - has bindings for each field.
#[derive(Debug)]
pub struct RecordCase {
    /// Used for debug formatting in tests
    pub _type_name: TypeName,
    /// Bindings for each field in the record.
    pub bindings: Vec<FieldBinding>,
    pub body: Decision,
}

/// A decision tree compiled from a list of match cases.
#[derive(Debug)]
pub enum Decision {
    /// A pattern is matched and the right-hand value is to be returned.
    Success(Body),

    /// Switch on a boolean value.
    SwitchBool {
        variable: Variable,
        true_case: Box<BoolCase>,
        false_case: Box<BoolCase>,
    },

    /// Switch on an Option value.
    SwitchOption {
        variable: Variable,
        some_case: Box<OptionSomeCase>,
        none_case: Box<OptionNoneCase>,
    },

    /// Switch on an enum value.
    SwitchEnum {
        variable: Variable,
        cases: Vec<EnumCase>,
    },

    /// Match a record (single case, destructures fields).
    SwitchRecord {
        variable: Variable,
        case: Box<RecordCase>,
    },
}

/// Information about a matched constructor for a variable.
struct VarInfo {
    /// The constructor name (e.g., "Some", "None", "Color::Red", "User").
    constructor: String,
    /// Constructor arguments: (sub_var_name, optional_field_name).
    /// - `Some(v0)` → `[("v0", None)]`
    /// - `None` → `[]`
    /// - `Foo{a: v0, b: v1}` → `[("v0", Some("a")), ("v1", Some("b"))]`
    args: Vec<(VarName, Option<FieldName>)>,
}

/// Checks if a pattern introduces no bindings and requires no runtime discrimination.
/// This is true for wildcards and for record patterns where all fields are free from bindings
/// (since records have only one constructor).
fn is_free_from_bindings(pattern: &TypedMatchPattern) -> bool {
    match pattern {
        TypedMatchPattern::Wildcard { .. } => true,
        TypedMatchPattern::Binding { .. } => false,
        TypedMatchPattern::Constructor { typ, fields, .. } => {
            // Only records can be free from bindings since they have one constructor
            matches!(
                typ.as_ref(),
                Type::Named {
                    kind: NamedKind::Record,
                    ..
                }
            ) && fields
                .iter()
                .all(|field| is_free_from_bindings(&field.pattern))
        }
    }
}

/// The `match` compiler itself.
pub struct Compiler<'a> {
    /// Counter for generating fresh variable names.
    fresh_vars: &'a mut FreshVarCounter,
    /// Registry used to resolve named type structure.
    registry: &'a TypeRegistry,
    /// The arm indices that are reachable.
    reachable: Vec<usize>,
    /// Missing pattern strings collected during compilation.
    missing_patterns: HashSet<String>,
    /// Maps variable names to their matched constructor info.
    var_info: HashMap<VarName, VarInfo>,
}

impl<'a> Compiler<'a> {
    pub fn new(fresh_vars: &'a mut FreshVarCounter, registry: &'a TypeRegistry) -> Self {
        Self {
            fresh_vars,
            registry,
            reachable: Vec::new(),
            missing_patterns: HashSet::new(),
            var_info: HashMap::new(),
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
                if let Type::Named {
                    module,
                    name,
                    kind: NamedKind::Enum,
                } = typ
                {
                    let variants = self
                        .registry
                        .enum_variants(module, name)
                        .expect("enum type must be registered");
                    variants
                        .iter()
                        .position(|variant| variant.name == *variant_name)
                        .expect("unknown variant")
                } else {
                    panic!("type is not an enum")
                }
            }
            // Records have only one constructor, so index is always 0
            Constructor::Record { .. } => 0,
        }
    }

    /// Compile a collection of patterns into a decision tree.
    pub fn compile(
        mut self,
        patterns: &[TypedMatchPattern],
        subject_type: Arc<Type>,
        subject_range: &DocumentRange,
    ) -> Result<Decision, TypeError> {
        // Check for empty arms
        if patterns.is_empty() {
            return Err(TypeError::new(
                TypeErrorKind::MatchNoArms {},
                subject_range.clone(),
            ));
        }

        let subject_var = self.fresh_var(subject_type);

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

        let tree = self.compile_rows(&subject_var.name, rows);

        // Check for unreachable arms
        let unreachable: Vec<usize> = (0..patterns.len())
            .filter(|i| !self.reachable.contains(i))
            .collect();
        if let Some(&first_unreachable) = unreachable.first() {
            let pattern = &patterns[first_unreachable];
            return Err(TypeError::new(
                TypeErrorKind::MatchUnreachableArm {
                    pattern: Box::new(pattern.clone()),
                },
                pattern.range().clone(),
            ));
        }

        // Check for missing patterns
        if !self.missing_patterns.is_empty() {
            let mut missing: Vec<String> = self.missing_patterns.into_iter().collect();
            missing.sort();
            return Err(TypeError::new(
                TypeErrorKind::MatchMissingVariants { variants: missing },
                subject_range.clone(),
            ));
        }

        // Tree is guaranteed to be Some if there are no missing patterns
        let tree = tree.expect("tree should be Some when there are no missing patterns");

        // Check for useless match (Success with no bindings)
        if let Decision::Success(body) = &tree {
            if body.bindings.is_empty() {
                return Err(TypeError::new(
                    TypeErrorKind::MatchUseless {},
                    subject_range.clone(),
                ));
            }
        }

        Ok(tree)
    }

    fn compile_rows(&mut self, root_var: &VarName, mut rows: Vec<Row>) -> Option<Decision> {
        if rows.is_empty() {
            self.missing_patterns
                .insert(self.build_pattern_for_var(root_var));
            return None;
        }

        for row in &mut rows {
            // Remove wildcards and move binding patterns into the body
            row.columns.retain(|col| match &col.pattern {
                TypedMatchPattern::Wildcard { .. } => false,
                TypedMatchPattern::Binding { name, .. } => {
                    row.body.bindings.push(Binding::new(
                        name.clone(),
                        col.variable.name.clone(),
                        col.variable.typ.clone(),
                    ));
                    false
                }
                TypedMatchPattern::Constructor { .. } => !is_free_from_bindings(&col.pattern),
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

        let mut cases = match branch_var.typ.as_ref() {
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
                        vec![self.fresh_var(inner.clone())],
                        Vec::new(),
                    ),
                    (Constructor::OptionNone, Vec::new(), Vec::new()),
                ]
            }
            Type::Named {
                module,
                name,
                kind: NamedKind::Enum,
            } => {
                let variants = self
                    .registry
                    .enum_variants(module, name)
                    .expect("enum type must be registered");
                variants
                    .iter()
                    .map(|variant| {
                        // Create fresh variables for each field in the variant
                        let field_vars: Vec<Variable> = variant
                            .fields
                            .iter()
                            .map(|(_, field_type, _)| self.fresh_var(field_type.clone()))
                            .collect();
                        (
                            Constructor::EnumVariant {
                                enum_name: name.clone(),
                                variant_name: variant.name.clone(),
                            },
                            field_vars,
                            Vec::new(),
                        )
                    })
                    .collect()
            }
            Type::Named {
                module,
                name,
                kind: NamedKind::Record,
            } => {
                // Records have a single constructor with fresh variables for each field
                let fields = self
                    .registry
                    .record_fields(module, name)
                    .expect("record type must be registered");
                let field_vars: Vec<Variable> = fields
                    .iter()
                    .map(|(_, field_type, _)| self.fresh_var(field_type.clone()))
                    .collect();
                vec![(
                    Constructor::Record {
                        type_name: name.clone(),
                    },
                    field_vars,
                    Vec::new(),
                )]
            }
            Type::String | Type::Int | Type::Float | Type::Fragment | Type::Array(_) => {
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
                if let TypedMatchPattern::Constructor {
                    constructor: cons,
                    args,
                    fields,
                    ..
                } = col.pattern
                {
                    let idx = self.constructor_index(&cons, &branch_var.typ);
                    let mut cols = row.columns;

                    if !fields.is_empty() {
                        // Field patterns: index is resolved on the typed field.
                        for field in fields {
                            let var = &mut cases[idx].1[field.index];
                            if is_free_from_bindings(&field.pattern) {
                                var.is_free_from_bindings = true;
                            }
                            cols.push(Column::new(var.clone(), field.pattern));
                        }
                    } else {
                        // Positional args (Option Some, etc.)
                        for (var, pat) in cases[idx].1.iter_mut().zip(args) {
                            if is_free_from_bindings(&pat) {
                                var.is_free_from_bindings = true;
                            }
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
        let mut compiled_cases = Vec::with_capacity(cases.len());

        for (cons, vars, rows) in cases {
            let args: Vec<(VarName, Option<FieldName>)> = if let Constructor::Record { .. } = &cons
            {
                if let Type::Named {
                    module,
                    name,
                    kind: NamedKind::Record,
                } = branch_var.typ.as_ref()
                {
                    let fields = self
                        .registry
                        .record_fields(module, name)
                        .expect("record type must be registered");
                    vars.iter()
                        .zip(fields.iter())
                        .map(|(v, (field_name, _, _))| (v.name.clone(), Some(field_name.clone())))
                        .collect()
                } else {
                    Vec::new()
                }
            } else if let Constructor::EnumVariant { variant_name, .. } = &cons {
                // For enum variants with fields, include field names
                if let Type::Named {
                    module,
                    name,
                    kind: NamedKind::Enum,
                } = branch_var.typ.as_ref()
                {
                    let variant_fields =
                        self.registry
                            .variant_fields(module, name, variant_name.as_str());
                    if let Some(fields) = variant_fields {
                        vars.iter()
                            .zip(fields.iter())
                            .map(|(v, (field_name, _, _))| {
                                (v.name.clone(), Some(field_name.clone()))
                            })
                            .collect()
                    } else {
                        vars.iter().map(|v| (v.name.clone(), None)).collect()
                    }
                } else {
                    vars.iter().map(|v| (v.name.clone(), None)).collect()
                }
            } else {
                vars.iter().map(|v| (v.name.clone(), None)).collect()
            };
            self.var_info.insert(
                branch_var.name.clone(),
                VarInfo {
                    constructor: cons.to_string(),
                    args,
                },
            );

            let body = self.compile_rows(root_var, rows);
            self.var_info.remove(&branch_var.name);

            compiled_cases.push((cons, vars, body));
        }

        // If any case body is None, return None (missing patterns already collected)
        if compiled_cases.iter().any(|(_, _, body)| body.is_none()) {
            return None;
        }

        // All case bodies are Some, build the appropriate typed Decision variant
        // Clone the type to avoid borrow issues when moving branch_var
        let branch_typ = branch_var.typ.clone();
        match branch_typ.as_ref() {
            Type::Bool => {
                // compiled_cases is ordered: [false, true]
                let mut iter = compiled_cases.into_iter();
                let (_, _, false_body) = iter.next().unwrap();
                let (_, _, true_body) = iter.next().unwrap();
                Some(Decision::SwitchBool {
                    variable: branch_var,
                    false_case: Box::new(BoolCase {
                        body: false_body.unwrap(),
                    }),
                    true_case: Box::new(BoolCase {
                        body: true_body.unwrap(),
                    }),
                })
            }
            Type::Option(_) => {
                // compiled_cases is ordered: [some, none]
                let mut iter = compiled_cases.into_iter();
                let (_, some_vars, some_body) = iter.next().unwrap();
                let (_, _, none_body) = iter.next().unwrap();
                let some_var = some_vars.into_iter().next().unwrap();
                Some(Decision::SwitchOption {
                    variable: branch_var,
                    some_case: Box::new(OptionSomeCase {
                        bound_name: if some_var.is_free_from_bindings {
                            None
                        } else {
                            Some(some_var.name)
                        },
                        body: some_body.unwrap(),
                    }),
                    none_case: Box::new(OptionNoneCase {
                        body: none_body.unwrap(),
                    }),
                })
            }
            Type::Named {
                module,
                name,
                kind: NamedKind::Enum,
            } => {
                let type_variants = self
                    .registry
                    .enum_variants(module, name)
                    .expect("enum type must be registered");
                let cases = compiled_cases
                    .into_iter()
                    .map(|(cons, vars, body)| {
                        let Constructor::EnumVariant { variant_name, .. } = cons else {
                            unreachable!("Expected EnumVariant constructor")
                        };
                        // Get the field names from the type definition
                        let empty_fields = vec![];
                        let variant_fields = type_variants
                            .iter()
                            .find(|variant| variant.name.as_str() == variant_name.as_str())
                            .map(|variant| &variant.fields)
                            .unwrap_or(&empty_fields);
                        // Create FieldBindings with field names
                        let bindings = variant_fields
                            .iter()
                            .zip(vars)
                            .map(|((field_name, _, _), var)| FieldBinding {
                                field_name: field_name.clone(),
                                bound_name: if var.is_free_from_bindings {
                                    None
                                } else {
                                    Some(var.name)
                                },
                            })
                            .collect();
                        EnumCase {
                            enum_name: name.clone(),
                            variant_name,
                            bindings,
                            body: body.unwrap(),
                        }
                    })
                    .collect();
                Some(Decision::SwitchEnum {
                    variable: branch_var,
                    cases,
                })
            }
            Type::Named {
                module,
                name,
                kind: NamedKind::Record,
            } => {
                let type_fields = self
                    .registry
                    .record_fields(module, name)
                    .expect("record type must be registered");
                // Records have exactly one case
                let (_, vars, body) = compiled_cases.into_iter().next().unwrap();

                // Create FieldBindings with field names
                let bindings = type_fields
                    .iter()
                    .zip(vars)
                    .map(|((field_name, _, _), var)| FieldBinding {
                        field_name: field_name.clone(),
                        bound_name: if var.is_free_from_bindings {
                            None
                        } else {
                            Some(var.name)
                        },
                    })
                    .collect();
                Some(Decision::SwitchRecord {
                    variable: branch_var,
                    case: Box::new(RecordCase {
                        _type_name: name.clone(),
                        bindings,
                        body: body.unwrap(),
                    }),
                })
            }
            _ => unreachable!("Unsupported type for pattern matching"),
        }
    }

    /// Given a row, returns the variable in that row that's referred to the
    /// most across all rows.
    fn find_branch_variable(&self, rows: &[Row]) -> Variable {
        let mut counts: HashMap<&VarName, usize> = HashMap::new();
        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable.name).or_insert(0_usize) += 1;
            }
        }
        rows[0]
            .columns
            .iter()
            .map(|col| col.variable.clone())
            .max_by_key(|var| counts[&var.name])
            .unwrap()
    }

    /// Returns a new variable to use in the decision tree.
    fn fresh_var(&mut self, typ: Arc<Type>) -> Variable {
        let name = self.fresh_vars.fresh_var();
        Variable::new(name, typ)
    }

    /// Builds a pattern string for a variable by recursively looking up constructor info.
    ///
    /// This is used to generate human-readable missing pattern messages. Starting from
    /// the root variable, it traverses `var_info` to reconstruct the pattern that would
    /// be needed to make the match exhaustive.
    fn build_pattern_for_var(&self, var_name: &VarName) -> String {
        let Some(info) = self.var_info.get(var_name) else {
            return "_".to_string();
        };
        if info.args.is_empty() {
            return info.constructor.clone();
        }
        let has_named_fields = info.args.iter().any(|(_, f)| f.is_some());
        let args = info
            .args
            .iter()
            .map(|(sub_var, field_name)| {
                let pattern = self.build_pattern_for_var(sub_var);
                match field_name {
                    Some(name) => format!("{}: {}", name, pattern),
                    None => pattern,
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        if has_named_fields {
            format!("{}{{{}}}", info.constructor, args)
        } else {
            format!("{}({})", info.constructor, args)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentCursor;
    use crate::document_annotator::DocumentAnnotator;
    use crate::expr::parse_expr;
    use crate::expr::parsing::parsed_expr::ParsedExpr;
    use crate::expr::patterns::typed::typecheck_pattern;
    use crate::expr::typing::type_registry_builder::TypeRegistryBuilder;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use std::collections::VecDeque;

    fn run_check(types: TypeRegistryBuilder, subject: &str, expr_str: &str) -> (String, bool) {
        let types = types.build();
        let subject_type = types.resolve(subject);
        let cursor = DocumentCursor::new(types.module().clone(), expr_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let expr = parse_expr::parse_expr(&mut iter, &mut comments, &mut errors, &range)
            .expect("Failed to parse expression");

        let (subject_range, patterns) = match expr {
            ParsedExpr::Match { subject, arms, .. } => {
                let ParsedExpr::Var {
                    range: subject_range,
                    ..
                } = *subject
                else {
                    panic!("Expected variable as match subject")
                };
                (
                    subject_range,
                    arms.into_iter().map(|a| a.pattern).collect::<Vec<_>>(),
                )
            }
            _ => panic!("Expected match expression"),
        };

        assert!(
            subject_type.is_matchable(),
            "match is not implemented for subject type {subject_type:?}"
        );
        // Pattern typechecking is covered by the `typed` module tests. Any error
        // here means the test uses a pattern that does not typecheck, so panic
        // rather than exercise the compiler with invalid input.
        let typed_patterns = patterns
            .iter()
            .map(|p| typecheck_pattern(p, subject_type.clone(), types.registry()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap_or_else(|e| panic!("pattern failed to typecheck: {e:?}"));

        let mut fresh_vars = FreshVarCounter::new();
        let result = Compiler::new(&mut fresh_vars, types.registry()).compile(
            &typed_patterns,
            subject_type,
            &subject_range,
        );

        match result {
            Ok(decision) => (format_decision(&decision, 0), true),
            Err(e) => (
                DocumentAnnotator::new()
                    .with_label("error")
                    .without_location()
                    .without_line_numbers()
                    .annotate(types.module(), [e])
                    .render(),
                false,
            ),
        }
    }

    fn accept(types: TypeRegistryBuilder, subject: &str, expr_str: &str, expected: Expect) {
        let (actual, ok) = run_check(types, subject, expr_str);
        if !ok {
            panic!("expected patterns to compile, got error:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    fn reject(types: TypeRegistryBuilder, subject: &str, expr_str: &str, expected: Expect) {
        let (actual, ok) = run_check(types, subject, expr_str);
        if ok {
            panic!("expected a compile error, but patterns compiled to:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    fn format_decision(decision: &Decision, indent: usize) -> String {
        let pad = "  ".repeat(indent);
        match decision {
            Decision::Success(body) => {
                let mut out = String::new();
                for binding in &body.bindings {
                    out.push_str(&format!(
                        "{}let {} = {}\n",
                        pad, binding.name, binding.source_name
                    ));
                }
                out.push_str(&format!("{}branch {}\n", pad, body.value));
                out
            }
            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                let mut out = String::new();
                out.push_str(&format!("{}{} is false\n", pad, variable.name));
                out.push_str(&format_decision(&false_case.body, indent + 1));
                out.push_str(&format!("{}{} is true\n", pad, variable.name));
                out.push_str(&format_decision(&true_case.body, indent + 1));
                out
            }
            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let mut out = String::new();
                let binding_str = some_case
                    .bound_name
                    .as_ref()
                    .map(|v| v.as_str())
                    .unwrap_or("_");
                out.push_str(&format!(
                    "{}{} is Some({})\n",
                    pad, variable.name, binding_str
                ));
                out.push_str(&format_decision(&some_case.body, indent + 1));
                out.push_str(&format!("{}{} is None\n", pad, variable.name));
                out.push_str(&format_decision(&none_case.body, indent + 1));
                out
            }
            Decision::SwitchEnum { variable, cases } => {
                let mut out = String::new();
                for case in cases {
                    let args = if case.bindings.is_empty() {
                        String::new()
                    } else {
                        let named: Vec<_> = case
                            .bindings
                            .iter()
                            .map(|b| {
                                let name = b.bound_name.as_ref().map(|v| v.as_str()).unwrap_or("_");
                                format!("{}: {}", b.field_name, name)
                            })
                            .collect();
                        format!("{{{}}}", named.join(", "))
                    };
                    out.push_str(&format!(
                        "{}{} is {}::{}{}\n",
                        pad, variable.name, case.enum_name, case.variant_name, args
                    ));
                    out.push_str(&format_decision(&case.body, indent + 1));
                }
                out
            }
            Decision::SwitchRecord { variable, case } => {
                let mut out = String::new();
                let args = if case.bindings.is_empty() {
                    String::new()
                } else {
                    let named: Vec<_> = case
                        .bindings
                        .iter()
                        .map(|b| {
                            let name = b.bound_name.as_ref().map(|v| v.as_str()).unwrap_or("_");
                            format!("{}: {}", b.field_name, name)
                        })
                        .collect();
                    format!("{{{}}}", named.join(", "))
                };
                out.push_str(&format!(
                    "{}{} is {}{}\n",
                    pad, variable.name, case._type_name, args
                ));
                out.push_str(&format_decision(&case.body, indent + 1));
                out
            }
        }
    }

    // Bool tests

    #[test]
    fn accepts_bool_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                }
            "},
            expect![[r#"
                v_0 is false
                  branch 1
                v_0 is true
                  branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_bool_missing_false() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    true => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: false
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_bool_missing_true() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    false => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: true
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_bool_unreachable_arm() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                    true => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'true'
                    true => 2,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_bool_wildcard_covers_all() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_bool_binding_covers_all() {
        accept(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    b => 0,
                }
            "},
            expect![[r#"
                let b = v_0
                branch 0
            "#]],
        );
    }

    // Option tests

    #[test]
    fn accepts_option_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    Some(item) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                v_0 is Some(v_1)
                  let item = v_1
                  branch 0
                v_0 is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_option_missing_none() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    Some(item) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: None
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_option_missing_some() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    None => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(_)
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_nested_option_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[Option[String]]",
            indoc! {"
                match x {
                    Some(Some(item)) => 0,
                    Some(None) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                v_0 is Some(v_1)
                  v_1 is Some(v_2)
                    let item = v_2
                    branch 0
                  v_1 is None
                    branch 1
                v_0 is None
                  branch 2
            "#]],
        );
    }

    // Enum tests

    #[test]
    fn accepts_enum_exhaustive() {
        accept(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Green => 1,
                    Color::Blue => 2,
                }
            "},
            expect![[r#"
                v_0 is Color::Red
                  branch 0
                v_0 is Color::Green
                  branch 1
                v_0 is Color::Blue
                  branch 2
            "#]],
        );
    }

    #[test]
    fn rejects_enum_missing_variant() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Green => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_wildcard_covers_remaining() {
        accept(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    Color::Red => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                v_0 is Color::Red
                  branch 0
                v_0 is Color::Green
                  branch 1
                v_0 is Color::Blue
                  branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_enum_unreachable_after_wildcard() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    _ => 0,
                    Color::Red => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'Color::Red'
                    Color::Red => 1,
                    ^^^^^^^^^^
            "#]],
        );
    }

    // Enum with fields tests

    #[test]
    fn accepts_enum_variant_with_fields_exhaustive() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{value: v} => 0,
                    Outcome::Failure{message: m} => 1,
                }
            "},
            expect![[r#"
                v_0 is Outcome::Success{value: v_1}
                  let v = v_1
                  branch 0
                v_0 is Outcome::Failure{message: v_2}
                  let m = v_2
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_mixed_fields_and_unit() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Maybe",
                [("Just", vec![("value", "Int")]), ("Nothing", vec![])],
            ),
            "Maybe",
            indoc! {"
                match x {
                    Maybe::Just{value: v} => 0,
                    Maybe::Nothing => 1,
                }
            "},
            expect![[r#"
                v_0 is Maybe::Just{value: v_1}
                  let v = v_1
                  branch 0
                v_0 is Maybe::Nothing
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_with_wildcard_field() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{value: _} => 0,
                    Outcome::Failure{message: _} => 1,
                }
            "},
            expect![[r#"
                v_0 is Outcome::Success{value: _}
                  branch 0
                v_0 is Outcome::Failure{message: _}
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_enum_three_variants_with_fields() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Status",
                [
                    ("Pending", vec![("since", "Int")]),
                    ("Active", vec![("id", "Int"), ("name", "String")]),
                    ("Inactive", vec![]),
                ],
            ),
            "Status",
            indoc! {"
                match x {
                    Status::Pending{since: s} => 0,
                    Status::Active{id: i, name: n} => 1,
                    Status::Inactive => 2,
                }
            "},
            expect![[r#"
                v_0 is Status::Pending{since: v_1}
                  let s = v_1
                  branch 0
                v_0 is Status::Active{id: v_2, name: v_3}
                  let i = v_2
                  let n = v_3
                  branch 1
                v_0 is Status::Inactive
                  branch 2
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_with_three_fields() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Point3D",
                [("Coords", vec![("x", "Int"), ("y", "Int"), ("z", "Int")])],
            ),
            "Point3D",
            indoc! {"
                match x {
                    Point3D::Coords{x: a, y: b, z: c} => 0,
                }
            "},
            expect![[r#"
                v_0 is Point3D::Coords{x: v_1, y: v_2, z: v_3}
                  let a = v_1
                  let b = v_2
                  let c = v_3
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_all_fields_wildcard() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Point3D",
                [("Coords", vec![("x", "Int"), ("y", "Int"), ("z", "Int")])],
            ),
            "Point3D",
            indoc! {"
                match x {
                    Point3D::Coords{x: _, y: _, z: _} => 0,
                }
            "},
            expect![[r#"
                v_0 is Point3D::Coords{x: _, y: _, z: _}
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_mixed_bindings_and_wildcards() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Point3D",
                [("Coords", vec![("x", "Int"), ("y", "Int"), ("z", "Int")])],
            ),
            "Point3D",
            indoc! {"
                match x {
                    Point3D::Coords{x: a, y: _, z: c} => 0,
                }
            "},
            expect![[r#"
                v_0 is Point3D::Coords{x: v_1, y: _, z: v_3}
                  let a = v_1
                  let c = v_3
                  branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_wildcard_covers_all_variants() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Res",
                [
                    ("Ok", vec![("value", "Int")]),
                    ("Err", vec![("message", "String")]),
                ],
            ),
            "Res",
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_binding_covers_all_variants() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Res",
                [
                    ("Ok", vec![("value", "Int")]),
                    ("Err", vec![("message", "String")]),
                ],
            ),
            "Res",
            indoc! {"
                match x {
                    r => 0,
                }
            "},
            expect![[r#"
                let r = v_0
                branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_partial_coverage_with_wildcard() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Status",
                [
                    ("Pending", vec![("since", "Int")]),
                    ("Active", vec![("id", "Int")]),
                    ("Inactive", vec![]),
                ],
            ),
            "Status",
            indoc! {"
                match x {
                    Status::Pending{since: s} => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                v_0 is Status::Pending{since: v_1}
                  let s = v_1
                  branch 0
                v_0 is Status::Active{id: v_2}
                  branch 1
                v_0 is Status::Inactive
                  branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_missing_variant() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{value: v} => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Outcome::Failure{message: _}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_missing_multiple_variants() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Status",
                [
                    ("Pending", vec![("since", "Int")]),
                    ("Active", vec![("id", "Int")]),
                    ("Inactive", vec![]),
                ],
            ),
            "Status",
            indoc! {"
                match x {
                    Status::Pending{since: _} => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Status::Active{id: _}, Status::Inactive
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_duplicate_pattern() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{value: v} => 0,
                    Outcome::Success{value: w} => 1,
                    Outcome::Failure{message: _} => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'Outcome::Success{value: w}'
                    Outcome::Success{value: w} => 1,
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_unreachable_after_wildcard() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    _ => 0,
                    Outcome::Success{value: v} => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'Outcome::Success{value: v}'
                    Outcome::Success{value: v} => 1,
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_nested_option_field() {
        accept(
            TypeRegistryBuilder::new()
                .enum_("Container", [("Wrapped", vec![("inner", "Option[Int]")])]),
            "Container",
            indoc! {"
                match x {
                    Container::Wrapped{inner: Some(v)} => 0,
                    Container::Wrapped{inner: None} => 1,
                }
            "},
            expect![[r#"
                v_0 is Container::Wrapped{inner: v_1}
                  v_1 is Some(v_2)
                    let v = v_2
                    branch 0
                  v_1 is None
                    branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_nested_option_field_missing_none() {
        reject(
            TypeRegistryBuilder::new()
                .enum_("Container", [("Wrapped", vec![("inner", "Option[Int]")])]),
            "Container",
            indoc! {"
                match x {
                    Container::Wrapped{inner: Some(v)} => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Container::Wrapped{inner: None}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_nested_bool_field() {
        accept(
            TypeRegistryBuilder::new().enum_("Flag", [("Active", vec![("enabled", "Bool")])]),
            "Flag",
            indoc! {"
                match x {
                    Flag::Active{enabled: true} => 0,
                    Flag::Active{enabled: false} => 1,
                }
            "},
            expect![[r#"
                v_0 is Flag::Active{enabled: v_1}
                  v_1 is false
                    branch 1
                  v_1 is true
                    branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_nested_bool_field_missing_case() {
        reject(
            TypeRegistryBuilder::new().enum_("Flag", [("Active", vec![("enabled", "Bool")])]),
            "Flag",
            indoc! {"
                match x {
                    Flag::Active{enabled: true} => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Flag::Active{enabled: false}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_four_fields() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Rectangle",
                [(
                    "Bounds",
                    vec![
                        ("x", "Int"),
                        ("y", "Int"),
                        ("width", "Int"),
                        ("height", "Int"),
                    ],
                )],
            ),
            "Rectangle",
            indoc! {"
                match x {
                    Rectangle::Bounds{x: a, y: b, width: w, height: h} => 0,
                }
            "},
            expect![[r#"
                v_0 is Rectangle::Bounds{x: v_1, y: v_2, width: v_3, height: v_4}
                  let a = v_1
                  let b = v_2
                  let w = v_3
                  let h = v_4
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_four_variants_mixed() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Event",
                [
                    ("Click", vec![("x", "Int"), ("y", "Int")]),
                    ("KeyPress", vec![("key", "String")]),
                    ("Focus", vec![]),
                    ("Blur", vec![]),
                ],
            ),
            "Event",
            indoc! {"
                match x {
                    Event::Click{x: a, y: b} => 0,
                    Event::KeyPress{key: k} => 1,
                    Event::Focus => 2,
                    Event::Blur => 3,
                }
            "},
            expect![[r#"
                v_0 is Event::Click{x: v_1, y: v_2}
                  let a = v_1
                  let b = v_2
                  branch 0
                v_0 is Event::KeyPress{key: v_3}
                  let k = v_3
                  branch 1
                v_0 is Event::Focus
                  branch 2
                v_0 is Event::Blur
                  branch 3
            "#]],
        );
    }

    // Record tests

    #[test]
    fn accepts_record_match() {
        accept(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User{name: n, age: a} => 0,
                }
            "},
            expect![[r#"
                v_0 is User{name: v_1, age: v_2}
                  let n = v_1
                  let a = v_2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_record_with_bool_fields_exhaustive() {
        accept(
            TypeRegistryBuilder::new().record("Foo", [("a", "Bool"), ("b", "Bool")]),
            "Foo",
            indoc! {"
                match x {
                    Foo{a: true, b: true} => 0,
                    Foo{a: true, b: false} => 1,
                    Foo{a: false, b: true} => 2,
                    Foo{a: false, b: false} => 3,
                }
            "},
            expect![[r#"
                v_0 is Foo{a: v_1, b: v_2}
                  v_2 is false
                    v_1 is false
                      branch 3
                    v_1 is true
                      branch 1
                  v_2 is true
                    v_1 is false
                      branch 2
                    v_1 is true
                      branch 0
            "#]],
        );
    }

    // Additional Bool tests

    #[test]
    fn accepts_bool_true_with_wildcard() {
        accept(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    true => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                v_0 is false
                  branch 1
                v_0 is true
                  branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_bool_duplicate_false() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    true => 0,
                    false => 1,
                    false => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'false'
                    false => 2,
                    ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_bool_unreachable_after_wildcard() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    _ => 0,
                    true => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'true'
                    true => 1,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_bool_unreachable_after_binding() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            indoc! {"
                match x {
                    b => 0,
                    true => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'true'
                    true => 1,
                    ^^^^
            "#]],
        );
    }

    // Additional Option tests

    #[test]
    fn rejects_option_wildcard_covers_all() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_option_some_with_wildcard() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    Some(v) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                v_0 is Some(v_1)
                  let v = v_1
                  branch 0
                v_0 is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_option_duplicate_some() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    Some(_) => 0,
                    Some(_) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'Some(_)'
                    Some(_) => 1,
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_duplicate_none() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[String]",
            indoc! {"
                match x {
                    Some(_) => 0,
                    None => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'None'
                    None => 2,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_nested_option_missing_some_none() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[Option[Int]]",
            indoc! {"
                match x {
                    Some(Some(_)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(None)
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_nested_option_with_bool_missing() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[Option[Bool]]",
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
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_nested_option_with_bool_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[Option[Bool]]",
            indoc! {"
                match x {
                    Some(Some(true)) => 0,
                    Some(Some(false)) => 1,
                    Some(None) => 2,
                    None => 3,
                }
            "},
            expect![[r#"
                v_0 is Some(v_1)
                  v_1 is Some(v_2)
                    v_2 is false
                      branch 1
                    v_2 is true
                      branch 0
                  v_1 is None
                    branch 2
                v_0 is None
                  branch 3
            "#]],
        );
    }

    #[test]
    fn rejects_nested_option_with_bool_missing_multiple() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[Option[Bool]]",
            indoc! {"
                match x {
                    Some(None) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(Some(_))
                match x {
                      ^
            "#]],
        );
    }

    // Additional Enum tests

    #[test]
    fn accepts_enum_binding_covers_all() {
        accept(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    c => 0,
                }
            "},
            expect![[r#"
                let c = v_0
                branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_enum_duplicate_variant() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            indoc! {"
                match x {
                    Color::Red => 0,
                    Color::Red => 1,
                    Color::Green => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern 'Color::Red'
                    Color::Red => 1,
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_multiple_wildcards() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            indoc! {"
                match x {
                    _ => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern '_'
                    _ => 1,
                    ^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_missing_multiple_variants() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green", "Blue"]),
            "Color",
            indoc! {"
                match x {
                    Color::Red => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue, Color::Green
                match x {
                      ^
            "#]],
        );
    }

    // Additional Record tests

    #[test]
    fn rejects_record_with_wildcard_fields() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User{name: _, age: _} => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_record_with_nested_option() {
        accept(
            TypeRegistryBuilder::new()
                .record("User", [("name", "String"), ("email", "Option[String]")]),
            "User",
            indoc! {"
                match x {
                    User{name: n, email: Some(e)} => 0,
                    User{name: n, email: None} => 1,
                }
            "},
            expect![[r#"
                v_0 is User{name: v_1, email: v_2}
                  v_2 is Some(v_3)
                    let n = v_1
                    let e = v_3
                    branch 0
                  v_2 is None
                    let n = v_1
                    branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_option_field_missing_none() {
        reject(
            TypeRegistryBuilder::new()
                .record("User", [("name", "String"), ("email", "Option[String]")]),
            "User",
            indoc! {"
                match x {
                    User{name: n, email: Some(e)} => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: User{name: _, email: None}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_bool_fields_missing() {
        reject(
            TypeRegistryBuilder::new().record("Foo", [("a", "Bool"), ("b", "Bool")]),
            "Foo",
            indoc! {"
                match x {
                    Foo{a: true, b: true} => 0,
                    Foo{a: true, b: false} => 1,
                    Foo{a: false, b: true} => 2,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Foo{a: false, b: false}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_bool_fields_missing_multiple() {
        reject(
            TypeRegistryBuilder::new().record("Foo", [("a", "Bool"), ("b", "Bool")]),
            "Foo",
            indoc! {"
                match x {
                    Foo{a: true, b: true} => 0,
                    Foo{a: false, b: false} => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Foo{a: false, b: true}, Foo{a: true, b: false}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_option_of_record_exhaustive() {
        accept(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "Option[User]",
            indoc! {"
                match x {
                    Some(User{name: n, age: a}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                v_0 is Some(v_1)
                  v_1 is User{name: v_2, age: v_3}
                    let n = v_2
                    let a = v_3
                    branch 0
                v_0 is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_option_of_record_with_wildcard_fields() {
        accept(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "Option[User]",
            indoc! {"
                match x {
                    Some(User{name: _, age: _}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                v_0 is Some(_)
                  branch 0
                v_0 is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_option_of_nested_records_with_wildcard_fields() {
        // Role is a nested record inside User
        accept(
            TypeRegistryBuilder::new()
                .record("Role", [("title", "String"), ("salary", "Int")])
                .record("User", [("role", "Role"), ("created_at", "Int")]),
            "Option[User]",
            indoc! {"
                match x {
                    Some(User{role: Role{title: _, salary: _}, created_at: _}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                v_0 is Some(_)
                  branch 0
                v_0 is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_all_effectively_wildcard_fields() {
        // All fields are effectively wildcards (one literal, one nested record)
        reject(
            TypeRegistryBuilder::new()
                .record("Address", [("street", "String"), ("city", "String")])
                .record("User", [("name", "String"), ("address", "Address")]),
            "User",
            indoc! {"
                match x {
                    User{name: _, address: Address{street: _, city: _}} => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_nested_wildcard_fields_followed_by_wildcard() {
        // First arm has all wildcard fields (effectively a wildcard), second arm is unreachable
        reject(
            TypeRegistryBuilder::new()
                .record("Role", [("title", "String"), ("salary", "Int")])
                .record("User", [("role", "Role"), ("created_at", "Int")]),
            "User",
            indoc! {"
                match x {
                    User{role: Role{title: _, salary: _}, created_at: _} => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for pattern '_'
                    _ => 1,
                    ^
            "#]],
        );
    }

    #[test]
    fn accepts_record_with_binding_and_nested_wildcard_record() {
        // User has a binding for `name` but `address` is an effectively-wildcard record
        accept(
            TypeRegistryBuilder::new()
                .record("Address", [("street", "String"), ("city", "String")])
                .record("User", [("name", "String"), ("address", "Address")]),
            "User",
            indoc! {"
                match x {
                    User{name: n, address: Address{street: _, city: _}} => 0,
                }
            "},
            expect![[r#"
                v_0 is User{name: v_1, address: _}
                  let n = v_1
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_with_binding_and_nested_wildcard_record() {
        // Outcome::Success has a binding for `value` but `metadata` is an effectively-wildcard record
        accept(
            TypeRegistryBuilder::new()
                .record("Metadata", [("created", "Int"), ("updated", "Int")])
                .enum_(
                    "Outcome",
                    [
                        (
                            "Success",
                            vec![("value", "String"), ("metadata", "Metadata")],
                        ),
                        ("Failure", vec![("message", "String")]),
                    ],
                ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{value: v, metadata: Metadata{created: _, updated: _}} => 0,
                    Outcome::Failure{message: _} => 1,
                }
            "},
            expect![[r#"
                v_0 is Outcome::Success{value: v_1, metadata: _}
                  let v = v_1
                  branch 0
                v_0 is Outcome::Failure{message: _}
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_three_level_nested_records_with_middle_binding() {
        // Outer -> Middle (has binding) -> Inner (all wildcards)
        accept(
            TypeRegistryBuilder::new()
                .record("Inner", [("x", "Int"), ("y", "Int")])
                .record("Middle", [("name", "String"), ("inner", "Inner")])
                .record("Outer", [("middle", "Middle")]),
            "Outer",
            indoc! {"
                match x {
                    Outer{middle: Middle{name: n, inner: Inner{x: _, y: _}}} => 0,
                }
            "},
            expect![[r#"
                v_0 is Outer{middle: v_1}
                  v_1 is Middle{name: v_2, inner: _}
                    let n = v_2
                    branch 0
            "#]],
        );
    }

    // Pattern validation tests

    #[test]
    fn rejects_match_no_arms() {
        reject(
            TypeRegistryBuilder::new(),
            "Bool",
            "match x {}",
            expect![[r#"
                error: Match expression must have at least one arm
                match x {}
                      ^
            "#]],
        );
    }
}
