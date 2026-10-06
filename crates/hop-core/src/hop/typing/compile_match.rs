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

use crate::hop::typing::Constructor;
use crate::hop::typing::TypeErrorKind;
use crate::hop::typing::r#type::Type;
use crate::hop::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::hop::typing::typed_match_pattern::{TypedField, TypedMatchPattern};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// A variable the decision tree introduces, numbered from 0 within one match.
/// Case variable 0 is the subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CaseVar(pub usize);

impl std::fmt::Display for CaseVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// A binding introduced by a pattern match (i.e. `name = source`).
#[derive(Clone, Debug)]
pub struct Binding {
    /// The name of the variable to bind.
    pub name: VarName,
    /// The case variable to bind from.
    pub source: CaseVar,
}

/// The body of code to evaluate in case of a match.
#[derive(Clone, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    pub bindings: Vec<Binding>,
    /// The branch to run in case of a match.
    pub value: usize,
}

/// A variable used in a match expression.
#[derive(Clone, Debug)]
pub struct Variable {
    pub id: CaseVar,
    pub typ: Type,
}

/// A single case (or row) in a match expression/table.
#[derive(Clone, Debug)]
struct Row {
    columns: Vec<Column>,
    body: Body,
}

impl Row {
    fn remove_column(&mut self, variable: &Variable) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| c.variable.id == variable.id)
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

/// A case for boolean pattern matching - no bindings possible.
#[derive(Clone, Debug)]
pub struct BoolCase {
    pub body: Decision,
}

/// A case for the Some variant of Option - exactly one potential binding.
#[derive(Clone, Debug)]
pub struct OptionSomeCase {
    /// The case variable holding the inner value.
    pub var: Variable,
    pub body: Decision,
}

/// A case for the None variant of Option - no bindings.
#[derive(Clone, Debug)]
pub struct OptionNoneCase {
    pub body: Decision,
}

/// A case for an enum variant - may have multiple field bindings.
#[derive(Clone, Debug)]
pub struct EnumCase {
    pub type_name: TypeName,
    pub variant_name: TypeName,
    /// Bindings for each field in the variant.
    pub bindings: Vec<FieldBinding>,
    pub body: Decision,
}

/// A case for record destructuring - has bindings for each field.
#[derive(Clone, Debug)]
pub struct RecordCase {
    /// Used for debug formatting in tests
    pub _type_name: TypeName,
    /// Bindings for each field in the record.
    pub bindings: Vec<FieldBinding>,
    pub body: Decision,
}

/// A case for tuple destructuring - has a case variable for each element.
#[derive(Clone, Debug)]
pub struct TupleCase {
    /// The case variables holding the tuple's elements, in order.
    pub elements: Vec<Variable>,
    pub body: Decision,
}

/// A binding for a field in a record or enum variant.
#[derive(Debug, Clone)]
pub struct FieldBinding {
    /// The field name this binding corresponds to.
    pub field_name: FieldName,
    /// The case variable holding this field's value.
    pub var: Variable,
}

/// A decision tree compiled from a list of match cases.
#[derive(Clone, Debug)]
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

    /// Match a tuple (single case, destructures elements).
    SwitchTuple {
        variable: Variable,
        case: Box<TupleCase>,
    },
}

impl Decision {
    /// Collect every case variable the tree reads, as the subject of a
    /// switch or as the source of a binding.
    pub fn collect_used(&self, used: &mut HashSet<CaseVar>) {
        match self {
            Decision::Success(body) => {
                used.extend(body.bindings.iter().map(|binding| binding.source));
            }
            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                used.insert(variable.id);
                true_case.body.collect_used(used);
                false_case.body.collect_used(used);
            }
            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                used.insert(variable.id);
                some_case.body.collect_used(used);
                none_case.body.collect_used(used);
            }
            Decision::SwitchEnum { variable, cases } => {
                used.insert(variable.id);
                for case in cases {
                    case.body.collect_used(used);
                }
            }
            Decision::SwitchRecord { variable, case } => {
                used.insert(variable.id);
                case.body.collect_used(used);
            }
            Decision::SwitchTuple { variable, case } => {
                used.insert(variable.id);
                case.body.collect_used(used);
            }
        }
    }
}

/// Information about a matched constructor for a variable.
struct VarInfo {
    /// The constructor (e.g., `Some`, `None`, `Color::Red`, `User`).
    constructor: Constructor,
    /// Constructor arguments: (optional_field_name, sub_var).
    /// ```text
    /// Some(#1)             [(None, #1)]
    /// None                 []
    /// Foo{a: #1, b: #2}    [(Some("a"), #1), (Some("b"), #2)]
    /// (#1, #2)             [(None, #1), (None, #2)]
    /// ```
    args: Vec<(Option<FieldName>, Variable)>,
}

/// Checks if a pattern introduces no bindings and requires no runtime discrimination.
/// This is true for wildcards and for record and tuple patterns where all fields or
/// elements are free from bindings (since records and tuples have only one constructor).
fn is_free_from_bindings(pattern: &TypedMatchPattern) -> bool {
    match pattern {
        TypedMatchPattern::Wildcard => true,
        TypedMatchPattern::Binding { .. } => false,
        TypedMatchPattern::Constructor {
            constructor,
            args,
            fields,
        } => {
            // Only records and tuples can be free from bindings since they have one constructor
            matches!(constructor, Constructor::Record { .. } | Constructor::Tuple)
                && args.iter().all(is_free_from_bindings)
                && fields
                    .iter()
                    .all(|field| is_free_from_bindings(&field.pattern))
        }
    }
}

/// Returns the index of a constructor within the given type.
fn constructor_index(cons: &Constructor, resolved: ResolvedType<'_>) -> usize {
    match cons {
        Constructor::BooleanFalse => 0,
        Constructor::BooleanTrue => 1,
        Constructor::OptionSome => 0,
        Constructor::OptionNone => 1,
        Constructor::EnumVariant { variant_name, .. } => {
            let ResolvedType::Enum { variants, .. } = resolved else {
                panic!("type is not an enum")
            };
            variants
                .iter()
                .position(|variant| variant.name == *variant_name)
                .expect("unknown variant")
        }
        // Records and tuples have only one constructor, so index is always 0
        Constructor::Record { .. } | Constructor::Tuple => 0,
    }
}

/// Why a match does not compile.
#[derive(Debug)]
pub struct MatchError {
    pub kind: Box<TypeErrorKind>,
    /// The part of the match the error is about.
    pub site: MatchErrorSite,
}

#[derive(Debug)]
pub enum MatchErrorSite {
    /// The match as a whole, reported at its subject.
    Subject,
    /// The pattern at this index.
    Pattern(usize),
}

/// Compile a collection of patterns into a decision tree.
pub fn compile_match(
    registry: &TypeRegistry,
    patterns: &[TypedMatchPattern],
    subject_type: Type,
) -> Result<Decision, MatchError> {
    if patterns.is_empty() {
        return Err(MatchError {
            kind: Box::new(TypeErrorKind::MatchNoArms {}),
            site: MatchErrorSite::Subject,
        });
    }

    let subject_var = Variable {
        id: CaseVar(0),
        typ: subject_type,
    };
    let mut next_case = 1;

    let rows: Vec<Row> = patterns
        .iter()
        .enumerate()
        .map(|(idx, pattern)| Row {
            columns: vec![Column {
                variable: subject_var.clone(),
                pattern: pattern.clone(),
            }],
            body: Body {
                bindings: Vec::new(),
                value: idx,
            },
        })
        .collect();

    let mut reachable = Vec::new();
    let mut missing_patterns = Vec::new();
    let mut var_info = HashMap::new();

    let tree = compile_rows(
        &mut next_case,
        registry,
        &mut reachable,
        &mut missing_patterns,
        &mut var_info,
        rows,
    );

    if let Some(index) = (0..patterns.len()).find(|i| !reachable.contains(i)) {
        return Err(MatchError {
            kind: Box::new(TypeErrorKind::MatchUnreachablePattern {
                pattern: Box::new(patterns[index].clone()),
            }),
            site: MatchErrorSite::Pattern(index),
        });
    }

    if !missing_patterns.is_empty() {
        let mut missing: Vec<String> = missing_patterns
            .iter()
            .map(|pattern| pattern.to_string())
            .collect();
        missing.sort();
        missing.dedup();
        return Err(MatchError {
            kind: Box::new(TypeErrorKind::MatchMissingPattern { patterns: missing }),
            site: MatchErrorSite::Subject,
        });
    }

    let tree = tree.expect("tree should be Some when there are no missing patterns");

    if let Decision::Success(body) = &tree
        && body.bindings.is_empty()
    {
        return Err(MatchError {
            kind: Box::new(TypeErrorKind::MatchUseless {}),
            site: MatchErrorSite::Subject,
        });
    }

    Ok(tree)
}

fn compile_rows(
    next_case: &mut usize,
    registry: &TypeRegistry,
    reachable: &mut Vec<usize>,
    missing_patterns: &mut Vec<TypedMatchPattern>,
    var_info: &mut HashMap<CaseVar, VarInfo>,
    mut rows: Vec<Row>,
) -> Option<Decision> {
    if rows.is_empty() {
        missing_patterns.push(witness_for_var(var_info, CaseVar(0)));
        return None;
    }

    for row in &mut rows {
        // Remove wildcards and move binding patterns into the body
        row.columns.retain(|col| match &col.pattern {
            TypedMatchPattern::Wildcard => false,
            TypedMatchPattern::Binding { name } => {
                row.body.bindings.push(Binding {
                    name: name.clone(),
                    source: col.variable.id,
                });
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
        reachable.push(row.body.value);
        return Some(Decision::Success(row.body));
    }

    let branch_var = find_branch_variable(&rows);
    // Resolve a clone of the type, since branch_var moves into the decision.
    let branch_typ = branch_var.typ.clone();
    let resolved = registry
        .resolve(&branch_typ)
        .expect("named type must be registered");

    let mut cases = match resolved {
        ResolvedType::Bool => {
            vec![
                (Constructor::BooleanFalse, Vec::new(), Vec::new()),
                (Constructor::BooleanTrue, Vec::new(), Vec::new()),
            ]
        }
        ResolvedType::Option(inner) => {
            vec![
                (
                    Constructor::OptionSome,
                    vec![(None, fresh_var(next_case, inner.clone()))],
                    Vec::new(),
                ),
                (Constructor::OptionNone, Vec::new(), Vec::new()),
            ]
        }
        ResolvedType::Enum { name, variants, .. } => variants
            .iter()
            .map(|variant| {
                // Create fresh variables for each field in the variant
                let field_vars = variant
                    .fields
                    .iter()
                    .map(|field| {
                        (
                            Some(field.name.clone()),
                            fresh_var(next_case, field.typ.clone()),
                        )
                    })
                    .collect();
                (
                    Constructor::EnumVariant {
                        type_name: name.clone(),
                        variant_name: variant.name.clone(),
                    },
                    field_vars,
                    Vec::new(),
                )
            })
            .collect(),
        ResolvedType::Record { name, fields, .. } => {
            // Records have a single constructor with fresh variables for each field
            let field_vars = fields
                .iter()
                .map(|field| {
                    (
                        Some(field.name.clone()),
                        fresh_var(next_case, field.typ.clone()),
                    )
                })
                .collect();
            vec![(
                Constructor::Record {
                    type_name: name.clone(),
                },
                field_vars,
                Vec::new(),
            )]
        }
        ResolvedType::Tuple(elements) => {
            // Tuples have a single constructor with fresh variables for each element
            let element_vars = elements
                .iter()
                .map(|element| (None, fresh_var(next_case, element.clone())))
                .collect();
            vec![(Constructor::Tuple, element_vars, Vec::new())]
        }
        ResolvedType::String
        | ResolvedType::Int
        | ResolvedType::Float
        | ResolvedType::Html
        | ResolvedType::Array(_) => {
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
    // constructors, that means 10 triples. This is needed so this function can
    // assign the correct sub matches to these constructors.
    for mut row in rows {
        if let Some(col) = row.remove_column(&branch_var) {
            if let TypedMatchPattern::Constructor {
                constructor: cons,
                args,
                fields,
            } = col.pattern
            {
                let idx = constructor_index(&cons, resolved);
                let mut cols = row.columns;

                if !fields.is_empty() {
                    // Field patterns: index is resolved on the typed field.
                    for field in fields {
                        cols.push(Column {
                            variable: cases[idx].1[field.index].1.clone(),
                            pattern: field.pattern,
                        });
                    }
                } else {
                    // Positional args (Option Some, etc.)
                    for ((_, var), pat) in cases[idx].1.iter().zip(args) {
                        cols.push(Column {
                            variable: var.clone(),
                            pattern: pat,
                        });
                    }
                }

                cases[idx].2.push(Row {
                    columns: cols,
                    body: row.body,
                });
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
        var_info.insert(
            branch_var.id,
            VarInfo {
                constructor: cons.clone(),
                args: vars.clone(),
            },
        );

        let body = compile_rows(
            next_case,
            registry,
            reachable,
            missing_patterns,
            var_info,
            rows,
        );
        var_info.remove(&branch_var.id);

        compiled_cases.push((cons, vars, body));
    }

    // If any case body is None, return None (missing patterns already collected)
    if compiled_cases.iter().any(|(_, _, body)| body.is_none()) {
        return None;
    }

    // All case bodies are Some, build the appropriate typed Decision variant
    match resolved {
        ResolvedType::Bool => {
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
        ResolvedType::Option(_) => {
            // compiled_cases is ordered: [some, none]
            let mut iter = compiled_cases.into_iter();
            let (_, some_vars, some_body) = iter.next().unwrap();
            let (_, _, none_body) = iter.next().unwrap();
            let (_, some_var) = some_vars.into_iter().next().unwrap();
            Some(Decision::SwitchOption {
                variable: branch_var,
                some_case: Box::new(OptionSomeCase {
                    var: some_var,
                    body: some_body.unwrap(),
                }),
                none_case: Box::new(OptionNoneCase {
                    body: none_body.unwrap(),
                }),
            })
        }
        ResolvedType::Enum { name, .. } => {
            let cases = compiled_cases
                .into_iter()
                .map(|(cons, vars, body)| {
                    let Constructor::EnumVariant { variant_name, .. } = cons else {
                        unreachable!("Expected EnumVariant constructor")
                    };
                    let bindings = vars
                        .into_iter()
                        .map(|(field_name, var)| FieldBinding {
                            field_name: field_name.expect("enum variant fields are named"),
                            var,
                        })
                        .collect();
                    EnumCase {
                        type_name: name.clone(),
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
        ResolvedType::Record { name, .. } => {
            // Records have exactly one case
            let (_, vars, body) = compiled_cases.into_iter().next().unwrap();

            let bindings = vars
                .into_iter()
                .map(|(field_name, var)| FieldBinding {
                    field_name: field_name.expect("record fields are named"),
                    var,
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
        ResolvedType::Tuple(_) => {
            // Tuples have exactly one case
            let (_, vars, body) = compiled_cases.into_iter().next().unwrap();

            Some(Decision::SwitchTuple {
                variable: branch_var,
                case: Box::new(TupleCase {
                    elements: vars.into_iter().map(|(_, var)| var).collect(),
                    body: body.unwrap(),
                }),
            })
        }
        _ => unreachable!("Unsupported type for pattern matching"),
    }
}

/// Given a row, returns the variable in that row that's referred to the
/// most across all rows.
fn find_branch_variable(rows: &[Row]) -> Variable {
    let mut counts: HashMap<CaseVar, usize> = HashMap::new();
    for row in rows {
        for col in &row.columns {
            *counts.entry(col.variable.id).or_insert(0_usize) += 1;
        }
    }
    rows[0]
        .columns
        .iter()
        .map(|col| col.variable.clone())
        .max_by_key(|var| counts[&var.id])
        .unwrap()
}

/// Returns a new case variable to use in the decision tree.
fn fresh_var(next_case: &mut usize, typ: Type) -> Variable {
    let id = CaseVar(*next_case);
    *next_case += 1;
    Variable { id, typ }
}

/// Builds the witness for a variable by recursively looking up constructor info.
///
/// Starting from the root variable, it traverses `var_info` to reconstruct the
/// pattern that would be needed to make the match exhaustive. A variable that
/// no switch has tested is a wildcard.
fn witness_for_var(var_info: &HashMap<CaseVar, VarInfo>, var: CaseVar) -> TypedMatchPattern {
    let Some(info) = var_info.get(&var) else {
        return TypedMatchPattern::Wildcard;
    };
    let mut args = Vec::new();
    let mut fields = Vec::new();
    for (index, (field_name, sub_var)) in info.args.iter().enumerate() {
        let pattern = witness_for_var(var_info, sub_var.id);
        match field_name {
            Some(name) => fields.push(TypedField {
                name: name.clone(),
                index,
                pattern,
            }),
            None => args.push(pattern),
        }
    }
    TypedMatchPattern::Constructor {
        constructor: info.constructor.clone(),
        args,
        fields,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentCursor;
    use crate::document_annotator::DocumentAnnotator;
    use crate::hop::parsing::ParsedExpr;
    use crate::hop::parsing::parse_expr;
    use crate::hop::typing::TypeError;
    use crate::hop::typing::type_registry_builder::TypeRegistryBuilder;
    use crate::hop::typing::typecheck_pattern::typecheck_pattern;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn run_check(types: TypeRegistryBuilder, subject: &str, expr_str: &str) -> (String, bool) {
        let types = types.build();
        let subject_type = types.resolve(subject);
        let mut iter = DocumentCursor::new(types.module().clone(), expr_str.to_string());
        let mut comments = Vec::new();
        let mut errors = Vec::new();
        let expr =
            parse_expr(&mut iter, &mut comments, &mut errors).expect("Failed to parse expression");

        let (subject_range, patterns) = match expr {
            ParsedExpr::Match { subject, arms, .. } => {
                let ParsedExpr::VariableReference {
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
        // Pattern typechecking is covered by the `typecheck_pattern` tests. Any error
        // here means the test uses a pattern that does not typecheck, so panic
        // rather than exercise the compiler with invalid input.
        let mut type_errors = Vec::new();
        let typed_patterns = patterns
            .iter()
            .map(|p| {
                typecheck_pattern(
                    p,
                    &subject_type,
                    &types.type_env(),
                    types.registry(),
                    &mut Vec::new(),
                    &mut Vec::new(),
                    &mut type_errors,
                )
            })
            .collect::<Option<Vec<_>>>()
            .unwrap_or_else(|| panic!("pattern failed to typecheck: {type_errors:?}"));

        match compile_match(types.registry(), &typed_patterns, subject_type) {
            Ok(decision) => (format_decision(&decision, 0), true),
            Err(error) => {
                let range = match error.site {
                    MatchErrorSite::Subject => &subject_range,
                    MatchErrorSite::Pattern(index) => patterns[index].range(),
                };
                let type_error = TypeError::new(*error.kind, range.clone());
                (
                    DocumentAnnotator::new()
                        .with_severity_label()
                        .without_location()
                        .without_line_numbers()
                        .annotate([type_error.to_diagnostic()])
                        .render(),
                    false,
                )
            }
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
                        pad, binding.name, binding.source
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
                out.push_str(&format!("{}{} is false\n", pad, variable.id));
                out.push_str(&format_decision(&false_case.body, indent + 1));
                out.push_str(&format!("{}{} is true\n", pad, variable.id));
                out.push_str(&format_decision(&true_case.body, indent + 1));
                out
            }
            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let mut out = String::new();
                out.push_str(&format!(
                    "{}{} is Some({})\n",
                    pad, variable.id, some_case.var.id
                ));
                out.push_str(&format_decision(&some_case.body, indent + 1));
                out.push_str(&format!("{}{} is None\n", pad, variable.id));
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
                            .map(|b| format!("{}: {}", b.field_name, b.var.id))
                            .collect();
                        format!(" {{{}}}", named.join(", "))
                    };
                    out.push_str(&format!(
                        "{}{} is {}::{}{}\n",
                        pad, variable.id, case.type_name, case.variant_name, args
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
                        .map(|b| format!("{}: {}", b.field_name, b.var.id))
                        .collect();
                    format!(" {{{}}}", named.join(", "))
                };
                out.push_str(&format!(
                    "{}{} is {}{}\n",
                    pad, variable.id, case._type_name, args
                ));
                out.push_str(&format_decision(&case.body, indent + 1));
                out
            }
            Decision::SwitchTuple { variable, case } => {
                let mut out = String::new();
                let elements: Vec<_> = case.elements.iter().map(|e| e.id.to_string()).collect();
                let trailing_comma = if elements.len() == 1 { "," } else { "" };
                out.push_str(&format!(
                    "{}{} is ({}{})\n",
                    pad,
                    variable.id,
                    elements.join(", "),
                    trailing_comma
                ));
                out.push_str(&format_decision(&case.body, indent + 1));
                out
            }
        }
    }

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
                #0 is false
                  branch 1
                #0 is true
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
                error: Missing pattern(s) false
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
                error: Missing pattern(s) true
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
                error: Unreachable pattern true
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
                let b = #0
                branch 0
            "#]],
        );
    }

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
                #0 is Some(#1)
                  let item = #1
                  branch 0
                #0 is None
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
                error: Missing pattern(s) None
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
                error: Missing pattern(s) Some(_)
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
                #0 is Some(#1)
                  #1 is Some(#2)
                    let item = #2
                    branch 0
                  #1 is None
                    branch 1
                #0 is None
                  branch 2
            "#]],
        );
    }

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
                #0 is Color::Red
                  branch 0
                #0 is Color::Green
                  branch 1
                #0 is Color::Blue
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
                error: Missing pattern(s) Color::Blue
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
                #0 is Color::Red
                  branch 0
                #0 is Color::Green
                  branch 1
                #0 is Color::Blue
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
                error: Unreachable pattern Color::Red
                    Color::Red => 1,
                    ^^^^^^^^^^
            "#]],
        );
    }

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
                    Outcome::Success {value: v} => 0,
                    Outcome::Failure {message: m} => 1,
                }
            "},
            expect![[r#"
                #0 is Outcome::Success {value: #1}
                  let v = #1
                  branch 0
                #0 is Outcome::Failure {message: #2}
                  let m = #2
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
                    Maybe::Just {value: v} => 0,
                    Maybe::Nothing => 1,
                }
            "},
            expect![[r#"
                #0 is Maybe::Just {value: #1}
                  let v = #1
                  branch 0
                #0 is Maybe::Nothing
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
                    Outcome::Success {value: _} => 0,
                    Outcome::Failure {message: _} => 1,
                }
            "},
            expect![[r#"
                #0 is Outcome::Success {value: #1}
                  branch 0
                #0 is Outcome::Failure {message: #2}
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
                    Status::Pending {since: s} => 0,
                    Status::Active {id: i, name: n} => 1,
                    Status::Inactive => 2,
                }
            "},
            expect![[r#"
                #0 is Status::Pending {since: #1}
                  let s = #1
                  branch 0
                #0 is Status::Active {id: #2, name: #3}
                  let i = #2
                  let n = #3
                  branch 1
                #0 is Status::Inactive
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
                    Point3D::Coords {x: a, y: b, z: c} => 0,
                }
            "},
            expect![[r#"
                #0 is Point3D::Coords {x: #1, y: #2, z: #3}
                  let a = #1
                  let b = #2
                  let c = #3
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
                    Point3D::Coords {x: _, y: _, z: _} => 0,
                }
            "},
            expect![[r#"
                #0 is Point3D::Coords {x: #1, y: #2, z: #3}
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
                    Point3D::Coords {x: a, y: _, z: c} => 0,
                }
            "},
            expect![[r#"
                #0 is Point3D::Coords {x: #1, y: #2, z: #3}
                  let a = #1
                  let c = #3
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
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
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
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Res",
            indoc! {"
                match x {
                    r => 0,
                }
            "},
            expect![[r#"
                let r = #0
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
                    Status::Pending {since: s} => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                #0 is Status::Pending {since: #1}
                  let s = #1
                  branch 0
                #0 is Status::Active {id: #2}
                  branch 1
                #0 is Status::Inactive
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
                    Outcome::Success {value: v} => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Outcome::Failure {message: _}
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
                    Status::Pending {since: _} => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Status::Active {id: _}, Status::Inactive
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
                    Outcome::Success {value: v} => 0,
                    Outcome::Success {value: w} => 1,
                    Outcome::Failure {message: _} => 2,
                }
            "},
            expect![[r#"
                error: Unreachable pattern Outcome::Success {value: w}
                    Outcome::Success {value: w} => 1,
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                    Outcome::Success {value: v} => 1,
                }
            "},
            expect![[r#"
                error: Unreachable pattern Outcome::Success {value: v}
                    Outcome::Success {value: v} => 1,
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                    Container::Wrapped {inner: Some(v)} => 0,
                    Container::Wrapped {inner: None} => 1,
                }
            "},
            expect![[r#"
                #0 is Container::Wrapped {inner: #1}
                  #1 is Some(#2)
                    let v = #2
                    branch 0
                  #1 is None
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
                    Container::Wrapped {inner: Some(v)} => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Container::Wrapped {inner: None}
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
                    Flag::Active {enabled: true} => 0,
                    Flag::Active {enabled: false} => 1,
                }
            "},
            expect![[r#"
                #0 is Flag::Active {enabled: #1}
                  #1 is false
                    branch 1
                  #1 is true
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
                    Flag::Active {enabled: true} => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Flag::Active {enabled: false}
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
                    Rectangle::Bounds {x: a, y: b, width: w, height: h} => 0,
                }
            "},
            expect![[r#"
                #0 is Rectangle::Bounds {x: #1, y: #2, width: #3, height: #4}
                  let a = #1
                  let b = #2
                  let w = #3
                  let h = #4
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
                    Event::Click {x: a, y: b} => 0,
                    Event::KeyPress {key: k} => 1,
                    Event::Focus => 2,
                    Event::Blur => 3,
                }
            "},
            expect![[r#"
                #0 is Event::Click {x: #1, y: #2}
                  let a = #1
                  let b = #2
                  branch 0
                #0 is Event::KeyPress {key: #3}
                  let k = #3
                  branch 1
                #0 is Event::Focus
                  branch 2
                #0 is Event::Blur
                  branch 3
            "#]],
        );
    }

    #[test]
    fn accepts_record_match() {
        accept(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User {name: n, age: a} => 0,
                }
            "},
            expect![[r#"
                #0 is User {name: #1, age: #2}
                  let n = #1
                  let a = #2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_record_match_with_fields_out_of_order() {
        accept(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User {age: a, name: n} => 0,
                }
            "},
            expect![[r#"
                #0 is User {name: #1, age: #2}
                  let a = #2
                  let n = #1
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_match_with_fields_out_of_order() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Shape",
                [
                    ("Point", vec![("x", "Int"), ("visible", "Bool")]),
                    ("Empty", vec![]),
                ],
            ),
            "Shape",
            indoc! {"
                match x {
                    Shape::Point {visible: true, x: a} => 0,
                    Shape::Point {visible: false, x: _} => 1,
                    Shape::Empty => 2,
                }
            "},
            expect![[r#"
                #0 is Shape::Point {x: #1, visible: #2}
                  #2 is false
                    branch 1
                  #2 is true
                    let a = #1
                    branch 0
                #0 is Shape::Empty
                  branch 2
            "#]],
        );
    }

    #[test]
    fn rejects_unreachable_record_pattern_with_fields_out_of_order() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User {name: n, age: a} => 0,
                    User {age: a, name: n} => 1,
                }
            "},
            expect![[r#"
                error: Unreachable pattern User {age: a, name: n}
                    User {age: a, name: n} => 1,
                    ^^^^^^^^^^^^^^^^^^^^^^
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
                    Foo {a: true, b: true} => 0,
                    Foo {a: true, b: false} => 1,
                    Foo {a: false, b: true} => 2,
                    Foo {a: false, b: false} => 3,
                }
            "},
            expect![[r#"
                #0 is Foo {a: #1, b: #2}
                  #2 is false
                    #1 is false
                      branch 3
                    #1 is true
                      branch 1
                  #2 is true
                    #1 is false
                      branch 2
                    #1 is true
                      branch 0
            "#]],
        );
    }

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
                #0 is false
                  branch 1
                #0 is true
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
                error: Unreachable pattern false
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
                error: Unreachable pattern true
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
                error: Unreachable pattern true
                    true => 1,
                    ^^^^
            "#]],
        );
    }

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
                #0 is Some(#1)
                  let v = #1
                  branch 0
                #0 is None
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
                error: Unreachable pattern Some(_)
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
                error: Unreachable pattern None
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
                error: Missing pattern(s) Some(None)
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
                error: Missing pattern(s) Some(Some(true))
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
                #0 is Some(#1)
                  #1 is Some(#2)
                    #2 is false
                      branch 1
                    #2 is true
                      branch 0
                  #1 is None
                    branch 2
                #0 is None
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
                error: Missing pattern(s) Some(Some(_))
                match x {
                      ^
            "#]],
        );
    }

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
                let c = #0
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
                error: Unreachable pattern Color::Red
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
                error: Unreachable pattern _
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
                error: Missing pattern(s) Color::Blue, Color::Green
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_wildcard_fields() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User {name: _, age: _} => 0,
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
                    User {name: n, email: Some(e)} => 0,
                    User {name: n, email: None} => 1,
                }
            "},
            expect![[r#"
                #0 is User {name: #1, email: #2}
                  #2 is Some(#3)
                    let n = #1
                    let e = #3
                    branch 0
                  #2 is None
                    let n = #1
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
                    User {name: n, email: Some(e)} => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) User {name: _, email: None}
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
                    Foo {a: true, b: true} => 0,
                    Foo {a: true, b: false} => 1,
                    Foo {a: false, b: true} => 2,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Foo {a: false, b: false}
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
                    Foo {a: true, b: true} => 0,
                    Foo {a: false, b: false} => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Foo {a: false, b: true}, Foo {a: true, b: false}
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
                    Some(User {name: n, age: a}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                #0 is Some(#1)
                  #1 is User {name: #2, age: #3}
                    let n = #2
                    let a = #3
                    branch 0
                #0 is None
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
                    Some(User {name: _, age: _}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                #0 is Some(#1)
                  branch 0
                #0 is None
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
                    Some(User {role: Role {title: _, salary: _}, created_at: _}) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                #0 is Some(#1)
                  branch 0
                #0 is None
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
                    User {name: _, address: Address {street: _, city: _}} => 0,
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
                    User {role: Role {title: _, salary: _}, created_at: _} => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Unreachable pattern _
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
                    User {name: n, address: Address {street: _, city: _}} => 0,
                }
            "},
            expect![[r#"
                #0 is User {name: #1, address: #2}
                  let n = #1
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
                    Outcome::Success {value: v, metadata: Metadata {created: _, updated: _}} => 0,
                    Outcome::Failure {message: _} => 1,
                }
            "},
            expect![[r#"
                #0 is Outcome::Success {value: #1, metadata: #2}
                  let v = #1
                  branch 0
                #0 is Outcome::Failure {message: #3}
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
                    Outer {middle: Middle {name: n, inner: Inner {x: _, y: _}}} => 0,
                }
            "},
            expect![[r#"
                #0 is Outer {middle: #1}
                  #1 is Middle {name: #2, inner: #3}
                    let n = #2
                    branch 0
            "#]],
        );
    }

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

    #[test]
    fn accepts_recursive_enum_exhaustive() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "IntList",
                [
                    ("Cons", vec![("head", "Int"), ("tail", "IntList")]),
                    ("Nil", vec![]),
                ],
            ),
            "IntList",
            indoc! {"
                match x {
                    IntList::Cons {head: h, tail: t} => 0,
                    IntList::Nil => 1,
                }
            "},
            expect![[r#"
                #0 is IntList::Cons {head: #1, tail: #2}
                  let h = #1
                  let t = #2
                  branch 0
                #0 is IntList::Nil
                  branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_recursive_enum_nested_patterns() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "IntList",
                [
                    ("Cons", vec![("head", "Int"), ("tail", "IntList")]),
                    ("Nil", vec![]),
                ],
            ),
            "IntList",
            indoc! {"
                match x {
                    IntList::Cons {head: h, tail: IntList::Nil} => 0,
                    IntList::Cons {head: h, tail: IntList::Cons {head: h2, tail: rest}} => 1,
                    IntList::Nil => 2,
                }
            "},
            expect![[r#"
                #0 is IntList::Cons {head: #1, tail: #2}
                  #2 is IntList::Cons {head: #3, tail: #4}
                    let h = #1
                    let h2 = #3
                    let rest = #4
                    branch 1
                  #2 is IntList::Nil
                    let h = #1
                    branch 0
                #0 is IntList::Nil
                  branch 2
            "#]],
        );
    }

    #[test]
    fn rejects_recursive_enum_missing_nested_case() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "IntList",
                [
                    ("Cons", vec![("head", "Int"), ("tail", "IntList")]),
                    ("Nil", vec![]),
                ],
            ),
            "IntList",
            indoc! {"
                match x {
                    IntList::Cons {head: h, tail: IntList::Nil} => 0,
                    IntList::Nil => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) IntList::Cons {head: _, tail: IntList::Cons {head: _, tail: _}}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_recursive_record_through_option() {
        accept(
            TypeRegistryBuilder::new().record("Node", [("value", "Int"), ("next", "Option[Node]")]),
            "Node",
            indoc! {"
                match x {
                    Node {value: v, next: Some(n)} => 0,
                    Node {value: v, next: None} => 1,
                }
            "},
            expect![[r#"
                #0 is Node {value: #1, next: #2}
                  #2 is Some(#3)
                    let v = #1
                    let n = #3
                    branch 0
                  #2 is None
                    let v = #1
                    branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_recursive_record_missing_nested_case() {
        reject(
            TypeRegistryBuilder::new().record("Node", [("value", "Int"), ("next", "Option[Node]")]),
            "Node",
            indoc! {"
                match x {
                    Node {value: v, next: Some(Node {value: v2, next: None})} => 0,
                    Node {value: v, next: None} => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Node {value: _, next: Some(Node {value: _, next: Some(_)})}
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_option_test_and_wildcard_in_some() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[Bool]",
            indoc! {"
                match x {
                    Some(true) => 0,
                    Some(_) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                #0 is Some(#1)
                  #1 is false
                    branch 1
                  #1 is true
                    branch 0
                #0 is None
                  branch 2
            "#]],
        );
    }

    #[test]
    fn accepts_record_test_and_wildcard_in_field() {
        accept(
            TypeRegistryBuilder::new().record("Foo", [("a", "Bool"), ("b", "Option[String]")]),
            "Foo",
            indoc! {"
                match x {
                    Foo {a: true, b: Some(n)} => 0,
                    Foo {a: true, b: None} => 1,
                    Foo {a: false, b: _} => 2,
                }
            "},
            expect![[r#"
                #0 is Foo {a: #1, b: #2}
                  #1 is false
                    branch 2
                  #1 is true
                    #2 is Some(#3)
                      let n = #3
                      branch 0
                    #2 is None
                      branch 1
            "#]],
        );
    }

    #[test]
    fn accepts_record_binding_and_wildcard_in_field() {
        accept(
            TypeRegistryBuilder::new().record("Foo", [("a", "Bool"), ("b", "String")]),
            "Foo",
            indoc! {"
                match x {
                    Foo {a: true, b: _} => 0,
                    Foo {a: false, b: n} => 1,
                }
            "},
            expect![[r#"
                #0 is Foo {a: #1, b: #2}
                  #1 is false
                    let n = #2
                    branch 1
                  #1 is true
                    branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_test_and_wildcard_in_field() {
        accept(
            TypeRegistryBuilder::new().enum_(
                "Status",
                [("Active", vec![("admin", "Bool")]), ("Inactive", vec![])],
            ),
            "Status",
            indoc! {"
                match x {
                    Status::Active {admin: true} => 0,
                    Status::Active {admin: _} => 1,
                    Status::Inactive => 2,
                }
            "},
            expect![[r#"
                #0 is Status::Active {admin: #1}
                  #1 is false
                    branch 1
                  #1 is true
                    branch 0
                #0 is Status::Inactive
                  branch 2
            "#]],
        );
    }

    #[test]
    fn accepts_tuple_of_bools_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            indoc! {"
                match x {
                    (true, true) => 0,
                    (true, false) => 1,
                    (false, _) => 2,
                }
            "},
            expect![[r#"
                #0 is (#1, #2)
                  #1 is false
                    branch 2
                  #1 is true
                    #2 is false
                      branch 1
                    #2 is true
                      branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_of_bools_missing_case() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            indoc! {"
                match x {
                    (true, _) => 0,
                    (false, true) => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) (false, false)
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_tuple_with_binding_and_wildcard() {
        accept(
            TypeRegistryBuilder::new(),
            "(Bool, String, Int)",
            indoc! {"
                match x {
                    (true, s, _) => 0,
                    (false, _, n) => 1,
                }
            "},
            expect![[r#"
                #0 is (#1, #2, #3)
                  #1 is false
                    let n = #3
                    branch 1
                  #1 is true
                    let s = #2
                    branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_tuple_with_only_bindings() {
        accept(
            TypeRegistryBuilder::new(),
            "(String, Int)",
            indoc! {"
                match x {
                    (s, n) => 0,
                }
            "},
            expect![[r#"
                #0 is (#1, #2)
                  let s = #1
                  let n = #2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_with_wildcard_elements() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            indoc! {"
                match x {
                    (_, _) => 0,
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
    fn rejects_empty_tuple_pattern() {
        reject(
            TypeRegistryBuilder::new(),
            "()",
            indoc! {"
                match x {
                    () => 0,
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
    fn rejects_tuple_unreachable_arm() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            indoc! {"
                match x {
                    (a, _) => 0,
                    (true, _) => 1,
                }
            "},
            expect![[r#"
                error: Unreachable pattern (true, _)
                    (true, _) => 1,
                    ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_one_tuple() {
        accept(
            TypeRegistryBuilder::new(),
            "(Bool,)",
            indoc! {"
                match x {
                    (true,) => 0,
                    (false,) => 1,
                }
            "},
            expect![[r#"
                #0 is (#1,)
                  #1 is false
                    branch 1
                  #1 is true
                    branch 0
            "#]],
        );
    }

    #[test]
    fn rejects_one_tuple_missing_case() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool,)",
            indoc! {"
                match x {
                    (true,) => 0,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) (false,)
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_nested_tuple() {
        accept(
            TypeRegistryBuilder::new(),
            "((Bool, Int), Option[String])",
            indoc! {"
                match x {
                    ((true, n), Some(s)) => 0,
                    ((false, _), Some(_)) => 1,
                    (_, None) => 2,
                }
            "},
            expect![[r#"
                #0 is (#1, #2)
                  #2 is Some(#3)
                    #1 is (#4, #5)
                      #4 is false
                        branch 1
                      #4 is true
                        let s = #3
                        let n = #5
                        branch 0
                  #2 is None
                    branch 2
            "#]],
        );
    }

    #[test]
    fn rejects_nested_tuple_missing_case() {
        reject(
            TypeRegistryBuilder::new(),
            "((Bool, Bool), Bool)",
            indoc! {"
                match x {
                    ((true, _), _) => 0,
                    ((false, true), true) => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) ((false, false), _), ((false, true), false)
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_nested_tuple_with_wildcard_elements_followed_by_wildcard() {
        reject(
            TypeRegistryBuilder::new(),
            "((Bool, Bool), Bool)",
            indoc! {"
                match x {
                    ((_, _), _) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Unreachable pattern _
                    _ => 1,
                    ^
            "#]],
        );
    }

    #[test]
    fn accepts_tuple_with_binding_and_nested_wildcard_tuple() {
        accept(
            TypeRegistryBuilder::new(),
            "((Bool, Bool), String)",
            indoc! {"
                match x {
                    ((_, _), s) => 0,
                }
            "},
            expect![[r#"
                #0 is (#1, #2)
                  let s = #2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_option_of_tuple_exhaustive() {
        accept(
            TypeRegistryBuilder::new(),
            "Option[(Bool, Int)]",
            indoc! {"
                match x {
                    Some((true, n)) => 0,
                    Some((false, _)) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                #0 is Some(#1)
                  #1 is (#2, #3)
                    #2 is false
                      branch 1
                    #2 is true
                      let n = #3
                      branch 0
                #0 is None
                  branch 2
            "#]],
        );
    }

    #[test]
    fn rejects_option_of_tuple_missing_case() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[(Bool, Int)]",
            indoc! {"
                match x {
                    Some((true, n)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) Some((false, _))
                match x {
                      ^
            "#]],
        );
    }

    #[test]
    fn accepts_record_with_tuple_field() {
        accept(
            TypeRegistryBuilder::new().record("Point", [("xy", "(Int, Bool)")]),
            "Point",
            indoc! {"
                match x {
                    Point {xy: (n, true)} => 0,
                    Point {xy: (_, false)} => 1,
                }
            "},
            expect![[r#"
                #0 is Point {xy: #1}
                  #1 is (#2, #3)
                    #3 is false
                      branch 1
                    #3 is true
                      let n = #2
                      branch 0
            "#]],
        );
    }

    #[test]
    fn accepts_tuple_of_enums() {
        accept(
            TypeRegistryBuilder::new().enum_("Color", [("Red", vec![]), ("Green", vec![])]),
            "(Color, Color)",
            indoc! {"
                match x {
                    (Color::Red, Color::Red) => 0,
                    (Color::Green, Color::Green) => 1,
                    (_, _) => 2,
                }
            "},
            expect![[r#"
                #0 is (#1, #2)
                  #2 is Color::Red
                    #1 is Color::Red
                      branch 0
                    #1 is Color::Green
                      branch 2
                  #2 is Color::Green
                    #1 is Color::Red
                      branch 2
                    #1 is Color::Green
                      branch 1
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_of_enums_missing_case() {
        reject(
            TypeRegistryBuilder::new().enum_("Color", [("Red", vec![]), ("Green", vec![])]),
            "(Color, Color)",
            indoc! {"
                match x {
                    (Color::Red, _) => 0,
                    (_, Color::Red) => 1,
                }
            "},
            expect![[r#"
                error: Missing pattern(s) (Color::Green, Color::Green)
                match x {
                      ^
            "#]],
        );
    }
}
