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

use crate::document::{CheapString, DocumentRange, Ranged};
use crate::dop::syntax::parsed::{Constructor, ParsedMatchPattern};

use crate::dop::semantics::r#type::Type;
use crate::dop::semantics::type_error::TypeError;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;

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
    pub name: String,
    pub typ: Type,
    /// Whether this variable's pattern introduces no bindings.
    /// When true, the variable should not generate a binding in the output.
    is_free_from_bindings: bool,
}

impl Variable {
    pub fn new(name: String, typ: Type) -> Self {
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
    pattern: ParsedMatchPattern,
}

impl Column {
    fn new(variable: Variable, pattern: ParsedMatchPattern) -> Self {
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
    pub bound_name: Option<String>,
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
    pub bound_name: Option<String>,
    /// The type of the field.
    pub typ: Type,
}

/// A case for an enum variant - may have multiple field bindings.
#[derive(Debug)]
pub struct EnumCase {
    pub enum_name: TypeName,
    pub variant_name: CheapString,
    /// Bindings for each field in the variant.
    pub bindings: Vec<FieldBinding>,
    pub body: Decision,
}

/// A case for record destructuring - has bindings for each field.
#[derive(Debug)]
pub struct RecordCase {
    pub type_name: TypeName,
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
    /// - `Foo(a: v0, b: v1)` → `[("v0", Some("a")), ("v1", Some("b"))]`
    args: Vec<(String, Option<String>)>,
}

/// Checks if a pattern introduces no bindings and requires no runtime discrimination.
/// This is true for wildcards and for record patterns where all fields are free from bindings
/// (since records have only one constructor).
fn is_free_from_bindings(pattern: &ParsedMatchPattern, typ: &Type) -> bool {
    match pattern {
        ParsedMatchPattern::Wildcard { .. } => true,
        ParsedMatchPattern::Binding { .. } => false,
        ParsedMatchPattern::Constructor { fields, .. } => {
            // Only records can be free from bindings since they have one constructor
            if let Type::Record {
                fields: type_fields,
                ..
            } = typ
            {
                fields.iter().all(|(field_name, _, field_pattern)| {
                    type_fields
                        .iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, field_type)| is_free_from_bindings(field_pattern, field_type))
                        .unwrap_or(false)
                })
            } else {
                false
            }
        }
    }
}

/// The `match` compiler itself.
pub struct Compiler<'a> {
    /// Environment for generating fresh variable names.
    env: &'a mut crate::environment::Environment<Type>,
    /// The arm indices that are reachable.
    reachable: Vec<usize>,
    /// Missing pattern strings collected during compilation.
    missing_patterns: HashSet<String>,
    /// The root variable being matched.
    root_var: String,
    /// Maps variable names to their matched constructor info.
    var_info: HashMap<String, VarInfo>,
}

impl<'a> Compiler<'a> {
    pub fn new(env: &'a mut crate::environment::Environment<Type>) -> Self {
        Self {
            env,
            reachable: Vec::new(),
            missing_patterns: HashSet::new(),
            root_var: String::new(),
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
                if let Type::Enum { variants, .. } = typ {
                    variants
                        .iter()
                        .position(|(v, _)| v.as_str() == variant_name)
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
        self.root_var = subject_var.name.clone();

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

        let tree = self.compile_rows(rows);

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
        let tree = tree.expect("tree should be Some when there are no missing patterns");

        // Check for useless match (Success with no bindings)
        if let Decision::Success(body) = &tree {
            if body.bindings.is_empty() {
                return Err(TypeError::MatchUseless {
                    range: match_range.clone(),
                });
            }
        }

        Ok(tree)
    }

    fn compile_rows(&mut self, mut rows: Vec<Row>) -> Option<Decision> {
        if rows.is_empty() {
            self.missing_patterns
                .insert(self.build_pattern_for_var(&self.root_var));
            return None;
        }

        for row in &mut rows {
            // Remove wildcards and move binding patterns into the body
            row.columns.retain(|col| match &col.pattern {
                ParsedMatchPattern::Wildcard { .. } => false,
                ParsedMatchPattern::Binding { name, .. } => {
                    row.body.bindings.push(Binding::new(
                        name.to_string(),
                        col.variable.name.clone(),
                        col.variable.typ.clone(),
                    ));
                    false
                }
                ParsedMatchPattern::Constructor { .. } => {
                    !is_free_from_bindings(&col.pattern, &col.variable.typ)
                }
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
                .map(|(variant_name, fields)| {
                    // Create fresh variables for each field in the variant
                    let field_vars: Vec<Variable> = fields
                        .iter()
                        .map(|(_, field_type)| self.fresh_var(field_type.clone()))
                        .collect();
                    (
                        Constructor::EnumVariant {
                            enum_name: name.clone(),
                            variant_name: CheapString::new(variant_name.to_string()),
                        },
                        field_vars,
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
            Type::String
            | Type::Int
            | Type::Float
            | Type::TrustedHTML
            | Type::Array(_)
            | Type::Component { .. } => {
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
                        // Field patterns: match fields by name for records and enum variants
                        if let Type::Record {
                            fields: type_fields,
                            ..
                        } = &branch_var.typ
                        {
                            for (field_name, _, field_pattern) in fields {
                                // Find the index of this field in the record type
                                let field_idx = type_fields
                                    .iter()
                                    .position(|(name, _)| name == &field_name)
                                    .expect("field not found in record type");
                                let var = &mut cases[idx].1[field_idx];
                                if is_free_from_bindings(&field_pattern, &var.typ) {
                                    var.is_free_from_bindings = true;
                                }
                                cols.push(Column::new(var.clone(), field_pattern));
                            }
                        } else if let Type::Enum { variants, .. } = &branch_var.typ {
                            // Get the variant fields for the matched constructor
                            if let Constructor::EnumVariant { variant_name, .. } = &cons {
                                let variant_fields = variants
                                    .iter()
                                    .find(|(v, _)| v.as_str() == variant_name)
                                    .map(|(_, f)| f)
                                    .expect("variant not found in enum type");
                                for (field_name, _, field_pattern) in fields {
                                    // Find the index of this field in the variant
                                    let field_idx = variant_fields
                                        .iter()
                                        .position(|(name, _)| name == &field_name)
                                        .expect("field not found in variant");
                                    let var = &mut cases[idx].1[field_idx];
                                    if is_free_from_bindings(&field_pattern, &var.typ) {
                                        var.is_free_from_bindings = true;
                                    }
                                    cols.push(Column::new(var.clone(), field_pattern));
                                }
                            }
                        }
                    } else {
                        // Positional args (Option Some, etc.)
                        for (var, pat) in cases[idx].1.iter_mut().zip(args.into_iter()) {
                            if is_free_from_bindings(&pat, &var.typ) {
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
            let args = if let Constructor::Record { .. } = &cons {
                if let Type::Record { fields, .. } = &branch_var.typ {
                    vars.iter()
                        .zip(fields.iter())
                        .map(|(v, (field_name, _))| (v.name.clone(), Some(field_name.to_string())))
                        .collect()
                } else {
                    Vec::new()
                }
            } else if let Constructor::EnumVariant { variant_name, .. } = &cons {
                // For enum variants with fields, include field names
                if let Type::Enum { variants, .. } = &branch_var.typ {
                    let variant_fields = variants
                        .iter()
                        .find(|(v, _)| v.as_str() == variant_name)
                        .map(|(_, f)| f);
                    if let Some(fields) = variant_fields {
                        vars.iter()
                            .zip(fields.iter())
                            .map(|(v, (field_name, _))| {
                                (v.name.clone(), Some(field_name.to_string()))
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

            let body = self.compile_rows(rows);
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
        match branch_typ {
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
            Type::Enum {
                name,
                variants: type_variants,
                ..
            } => {
                let cases = compiled_cases
                    .into_iter()
                    .map(|(cons, vars, body)| {
                        let variant_name = match cons {
                            Constructor::EnumVariant { variant_name, .. } => variant_name,
                            _ => unreachable!("Expected EnumVariant constructor"),
                        };
                        // Get the field names from the type definition
                        let empty_fields = vec![];
                        let variant_fields = type_variants
                            .iter()
                            .find(|(v, _)| v.as_str() == variant_name.as_str())
                            .map(|(_, fields)| fields)
                            .unwrap_or(&empty_fields);
                        // Create FieldBindings with field names
                        let bindings = variant_fields
                            .iter()
                            .zip(vars)
                            .map(|((field_name, _), var)| FieldBinding {
                                field_name: field_name.clone(),
                                bound_name: if var.is_free_from_bindings {
                                    None
                                } else {
                                    Some(var.name)
                                },
                                typ: var.typ,
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
            Type::Record {
                name,
                fields: type_fields,
                ..
            } => {
                // Records have exactly one case
                let (_, vars, body) = compiled_cases.into_iter().next().unwrap();

                // Create FieldBindings with field names
                let bindings = type_fields
                    .iter()
                    .zip(vars)
                    .map(|((field_name, _), var)| FieldBinding {
                        field_name: field_name.clone(),
                        bound_name: if var.is_free_from_bindings {
                            None
                        } else {
                            Some(var.name)
                        },
                        typ: var.typ,
                    })
                    .collect();
                Some(Decision::SwitchRecord {
                    variable: branch_var,
                    case: Box::new(RecordCase {
                        type_name: name,
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
        let name = self.env.fresh_var();
        Variable::new(name, typ)
    }

    /// Builds a pattern string for a variable by recursively looking up constructor info.
    ///
    /// This is used to generate human-readable missing pattern messages. Starting from
    /// the root variable, it traverses `var_info` to reconstruct the pattern that would
    /// be needed to make the match exhaustive.
    fn build_pattern_for_var(&self, var_name: &str) -> String {
        let Some(info) = self.var_info.get(var_name) else {
            return "_".to_string();
        };
        if info.args.is_empty() {
            return info.constructor.clone();
        }
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

        format!("{}({})", info.constructor, args)
    }

    /// Validates that a pattern is compatible with a given type.
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
                constructor_range,
                range,
            } => match (constructor, subject_type) {
                (Constructor::BooleanTrue | Constructor::BooleanFalse, Type::Bool) => Ok(()),

                (Constructor::OptionSome, Type::Option(inner_type)) => {
                    if let Some(inner_pattern) = args.first() {
                        Self::validate_pattern(inner_pattern, inner_type)?;
                    }
                    Ok(())
                }

                (Constructor::OptionNone, Type::Option(_)) => Ok(()),

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

                    let variant_fields = variants
                        .iter()
                        .find(|(v, _)| v.as_str() == pattern_variant_name)
                        .map(|(_, f)| f);

                    let variant_fields = match variant_fields {
                        Some(f) => f,
                        None => {
                            return Err(TypeError::UndefinedEnumVariant {
                                enum_name: pattern_enum_name.to_string(),
                                variant_name: pattern_variant_name.to_string(),
                                range: range.clone(),
                            });
                        }
                    };

                    // Validate each field pattern (also catches unknown fields on unit variants)
                    for (field_name, field_name_range, field_pattern) in fields {
                        let field_type = variant_fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                            .map(|(_, typ)| typ);

                        match field_type {
                            Some(typ) => Self::validate_pattern(field_pattern, typ)?,
                            None => {
                                return Err(TypeError::EnumVariantUnknownField {
                                    enum_name: pattern_enum_name.to_string(),
                                    variant_name: pattern_variant_name.to_string(),
                                    field_name: field_name.to_string(),
                                    range: field_name_range.clone(),
                                });
                            }
                        }
                    }

                    // Check all fields are specified (no partial matching)
                    if fields.len() < variant_fields.len() {
                        let pattern_field_names: Vec<_> =
                            fields.iter().map(|(name, _, _)| name).collect();
                        let missing_fields: Vec<String> = variant_fields
                            .iter()
                            .filter(|(name, _)| !pattern_field_names.contains(&name))
                            .map(|(name, _)| name.to_string())
                            .collect();
                        return Err(TypeError::EnumVariantMissingFields {
                            enum_name: pattern_enum_name.to_string(),
                            variant_name: pattern_variant_name.to_string(),
                            missing_fields,
                            range: constructor_range.clone(),
                        });
                    }

                    Ok(())
                }

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

                    // Validate each field pattern (also catches unknown fields)
                    for (field_name, field_name_range, field_pattern) in fields {
                        let field_type = subject_fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                            .map(|(_, typ)| typ);

                        match field_type {
                            Some(typ) => Self::validate_pattern(field_pattern, typ)?,
                            None => {
                                return Err(TypeError::RecordUnknownField {
                                    field_name: field_name.to_string(),
                                    record_name: pattern_type_name.to_string(),
                                    range: field_name_range.clone(),
                                });
                            }
                        }
                    }

                    // Check all fields are specified (no partial matching)
                    if fields.len() < subject_fields.len() {
                        let pattern_field_names: Vec<_> =
                            fields.iter().map(|(name, _, _)| name).collect();
                        let missing_fields: Vec<String> = subject_fields
                            .iter()
                            .filter(|(name, _)| !pattern_field_names.contains(&name))
                            .map(|(name, _)| name.to_string())
                            .collect();
                        return Err(TypeError::RecordMissingFields {
                            record_name: pattern_type_name.to_string(),
                            missing_fields,
                            range: constructor_range.clone(),
                        });
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
    use crate::document::DocumentCursor;
    use crate::dop::parser;
    use crate::dop::symbols::field_name::FieldName;
    use crate::dop::symbols::type_name::TypeName;
    use crate::dop::syntax::parsed::ParsedExpr;
    use crate::hop::symbols::module_name::ModuleName;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use std::collections::VecDeque;

    fn check(subject_type: Type, expr_str: &str, expected: Expect) {
        let cursor = DocumentCursor::new(expr_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let expr = parser::parse_expr(&mut iter, &mut comments, &range)
            .expect("Failed to parse expression");

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

        let mut env = crate::environment::Environment::<Type>::new();
        let result = Compiler::new(&mut env).compile(
            &patterns,
            &subject_name,
            &subject_type,
            &subject_range,
            &match_range,
        );

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
                let binding_str = some_case.bound_name.as_deref().unwrap_or("_");
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
                                let name = b.bound_name.as_deref().unwrap_or("_");
                                format!("{}: {}", b.field_name, name)
                            })
                            .collect();
                        format!("({})", named.join(", "))
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
                            let name = b.bound_name.as_deref().unwrap_or("_");
                            format!("{}: {}", b.field_name, name)
                        })
                        .collect();
                    format!("({})", named.join(", "))
                };
                out.push_str(&format!(
                    "{}{} is {}{}\n",
                    pad, variable.name, case.type_name, args
                ));
                out.push_str(&format_decision(&case.body, indent + 1));
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
                error: Useless match expression: does not branch or bind any variables
                match x {
                ^^^^^^^^^
                    _ => 0,
                ^^^^^^^^^^^
                }
                ^
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
                x is Some(v_0)
                  let val = v_0
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
                x is Some(v_0)
                  v_0 is Some(v_1)
                    let val = v_1
                    branch 0
                  v_0 is None
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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

    // Enum with fields tests

    #[test]
    fn enum_variant_with_fields_exhaustive() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(value: v) => 0,
                    Result::Err(message: m) => 1,
                }
            "},
            expect![[r#"
                x is Result::Ok(value: v_0)
                  let v = v_0
                  branch 0
                x is Result::Err(message: v_1)
                  let m = v_1
                  branch 1
            "#]],
        );
    }

    #[test]
    fn enum_variant_mixed_fields_and_unit() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Maybe").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Just").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (TypeName::new("Nothing").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Maybe::Just(value: v) => 0,
                    Maybe::Nothing => 1,
                }
            "},
            expect![[r#"
                x is Maybe::Just(value: v_0)
                  let v = v_0
                  branch 0
                x is Maybe::Nothing
                  branch 1
            "#]],
        );
    }

    #[test]
    fn enum_variant_with_wildcard_field() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(value: _) => 0,
                    Result::Err(message: _) => 1,
                }
            "},
            expect![[r#"
                x is Result::Ok(value: _)
                  branch 0
                x is Result::Err(message: _)
                  branch 1
            "#]],
        );
    }

    #[test]
    fn enum_three_variants_with_fields() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Status").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Pending").unwrap(),
                        vec![(FieldName::new("since").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Active").unwrap(),
                        vec![
                            (FieldName::new("id").unwrap(), Type::Int),
                            (FieldName::new("name").unwrap(), Type::String),
                        ],
                    ),
                    (TypeName::new("Inactive").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Status::Pending(since: s) => 0,
                    Status::Active(id: i, name: n) => 1,
                    Status::Inactive => 2,
                }
            "},
            expect![[r#"
                x is Status::Pending(since: v_0)
                  let s = v_0
                  branch 0
                x is Status::Active(id: v_1, name: v_2)
                  let i = v_1
                  let n = v_2
                  branch 1
                x is Status::Inactive
                  branch 2
            "#]],
        );
    }

    #[test]
    fn enum_variant_with_three_fields() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point3D").unwrap(),
                variants: vec![(
                    TypeName::new("Coords").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                        (FieldName::new("z").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point3D::Coords(x: a, y: b, z: c) => 0,
                }
            "},
            expect![[r#"
                x is Point3D::Coords(x: v_0, y: v_1, z: v_2)
                  let a = v_0
                  let b = v_1
                  let c = v_2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_all_fields_wildcard() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point3D").unwrap(),
                variants: vec![(
                    TypeName::new("Coords").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                        (FieldName::new("z").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point3D::Coords(x: _, y: _, z: _) => 0,
                }
            "},
            expect![[r#"
                x is Point3D::Coords(x: _, y: _, z: _)
                  branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_mixed_bindings_and_wildcards() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point3D").unwrap(),
                variants: vec![(
                    TypeName::new("Coords").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                        (FieldName::new("z").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point3D::Coords(x: a, y: _, z: c) => 0,
                }
            "},
            expect![[r#"
                x is Point3D::Coords(x: v_0, y: _, z: v_2)
                  let a = v_0
                  let c = v_2
                  branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_wildcard_covers_all_variants() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    _ => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                ^^^^^^^^^
                    _ => 0,
                ^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_variant_binding_covers_all_variants() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    r => 0,
                }
            "},
            expect![[r#"
                let r = x
                branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_partial_coverage_with_wildcard() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Status").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Pending").unwrap(),
                        vec![(FieldName::new("since").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Active").unwrap(),
                        vec![(FieldName::new("id").unwrap(), Type::Int)],
                    ),
                    (TypeName::new("Inactive").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Status::Pending(since: s) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                x is Status::Pending(since: v_0)
                  let s = v_0
                  branch 0
                x is Status::Active(id: v_1)
                  branch 1
                x is Status::Inactive
                  branch 1
            "#]],
        );
    }

    #[test]
    fn enum_variant_missing_variant_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(value: v) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Result::Err(message: _)
                match x {
                ^^^^^^^^^
                    Result::Ok(value: v) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_variant_missing_multiple_variants_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Status").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Pending").unwrap(),
                        vec![(FieldName::new("since").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Active").unwrap(),
                        vec![(FieldName::new("id").unwrap(), Type::Int)],
                    ),
                    (TypeName::new("Inactive").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Status::Pending(since: _) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Status::Active(id: _), Status::Inactive
                match x {
                ^^^^^^^^^
                    Status::Pending(since: _) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_variant_duplicate_pattern_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(value: v) => 0,
                    Result::Ok(value: w) => 1,
                    Result::Err(message: _) => 2,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'Result::Ok(value: w)'
                    Result::Ok(value: w) => 1,
                    ^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_unreachable_after_wildcard_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    _ => 0,
                    Result::Ok(value: v) => 1,
                }
            "},
            expect![[r#"
                error: Unreachable match arm for variant 'Result::Ok(value: v)'
                    Result::Ok(value: v) => 1,
                    ^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_unknown_field_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(unknown: v) => 0,
                    Result::Err(message: _) => 1,
                }
            "},
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Result::Ok'
                    Result::Ok(unknown: v) => 0,
                               ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_unknown_field_after_valid_field_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point").unwrap(),
                variants: vec![(
                    TypeName::new("XY").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point::XY(x: a, unknown: b) => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Point::XY'
                    Point::XY(x: a, unknown: b) => 0,
                                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_two_unknown_fields_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point").unwrap(),
                variants: vec![(
                    TypeName::new("XY").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point::XY(foo: a, bar: b) => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'foo' in enum variant 'Point::XY'
                    Point::XY(foo: a, bar: b) => 0,
                              ^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_missing_field_in_pattern_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point").unwrap(),
                variants: vec![(
                    TypeName::new("XY").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point::XY(x: a) => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: y
                    Point::XY(x: a) => 0,
                    ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_missing_two_fields_in_pattern_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point").unwrap(),
                variants: vec![(
                    TypeName::new("XYZ").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                        (FieldName::new("z").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point::XYZ(x: a) => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XYZ' is missing fields: y, z
                    Point::XYZ(x: a) => 0,
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_no_parens_when_fields_expected_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Point").unwrap(),
                variants: vec![(
                    TypeName::new("XY").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Point::XY => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: x, y
                    Point::XY => 0,
                    ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_fields_provided_to_unit_variant_error() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Maybe").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Just").unwrap(),
                        vec![(FieldName::new("value").unwrap(), Type::Int)],
                    ),
                    (TypeName::new("Nothing").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Maybe::Just(value: v) => 0,
                    Maybe::Nothing(value: v) => 1,
                }
            "},
            expect![[r#"
                error: Unknown field 'value' in enum variant 'Maybe::Nothing'
                    Maybe::Nothing(value: v) => 1,
                                   ^^^^^
            "#]],
        );
    }

    #[test]
    fn enum_variant_nested_option_field() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Container").unwrap(),
                variants: vec![(
                    TypeName::new("Wrapped").unwrap(),
                    vec![(
                        FieldName::new("inner").unwrap(),
                        Type::Option(Box::new(Type::Int)),
                    )],
                )],
            },
            indoc! {"
                match x {
                    Container::Wrapped(inner: Some(v)) => 0,
                    Container::Wrapped(inner: None) => 1,
                }
            "},
            expect![[r#"
                x is Container::Wrapped(inner: v_0)
                  v_0 is Some(v_1)
                    let v = v_1
                    branch 0
                  v_0 is None
                    branch 1
            "#]],
        );
    }

    #[test]
    fn enum_variant_nested_option_field_missing_none() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Container").unwrap(),
                variants: vec![(
                    TypeName::new("Wrapped").unwrap(),
                    vec![(
                        FieldName::new("inner").unwrap(),
                        Type::Option(Box::new(Type::Int)),
                    )],
                )],
            },
            indoc! {"
                match x {
                    Container::Wrapped(inner: Some(v)) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Container::Wrapped(inner: None)
                match x {
                ^^^^^^^^^
                    Container::Wrapped(inner: Some(v)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_variant_nested_bool_field() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Flag").unwrap(),
                variants: vec![(
                    TypeName::new("Set").unwrap(),
                    vec![(FieldName::new("enabled").unwrap(), Type::Bool)],
                )],
            },
            indoc! {"
                match x {
                    Flag::Set(enabled: true) => 0,
                    Flag::Set(enabled: false) => 1,
                }
            "},
            expect![[r#"
                x is Flag::Set(enabled: v_0)
                  v_0 is false
                    branch 1
                  v_0 is true
                    branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_nested_bool_field_missing_case() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Flag").unwrap(),
                variants: vec![(
                    TypeName::new("Set").unwrap(),
                    vec![(FieldName::new("enabled").unwrap(), Type::Bool)],
                )],
            },
            indoc! {"
                match x {
                    Flag::Set(enabled: true) => 0,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Flag::Set(enabled: false)
                match x {
                ^^^^^^^^^
                    Flag::Set(enabled: true) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn enum_variant_four_fields() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Rectangle").unwrap(),
                variants: vec![(
                    TypeName::new("Bounds").unwrap(),
                    vec![
                        (FieldName::new("x").unwrap(), Type::Int),
                        (FieldName::new("y").unwrap(), Type::Int),
                        (FieldName::new("width").unwrap(), Type::Int),
                        (FieldName::new("height").unwrap(), Type::Int),
                    ],
                )],
            },
            indoc! {"
                match x {
                    Rectangle::Bounds(x: a, y: b, width: w, height: h) => 0,
                }
            "},
            expect![[r#"
                x is Rectangle::Bounds(x: v_0, y: v_1, width: v_2, height: v_3)
                  let a = v_0
                  let b = v_1
                  let w = v_2
                  let h = v_3
                  branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_four_variants_mixed() {
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Event").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Click").unwrap(),
                        vec![
                            (FieldName::new("x").unwrap(), Type::Int),
                            (FieldName::new("y").unwrap(), Type::Int),
                        ],
                    ),
                    (
                        TypeName::new("KeyPress").unwrap(),
                        vec![(FieldName::new("key").unwrap(), Type::String)],
                    ),
                    (TypeName::new("Focus").unwrap(), vec![]),
                    (TypeName::new("Blur").unwrap(), vec![]),
                ],
            },
            indoc! {"
                match x {
                    Event::Click(x: a, y: b) => 0,
                    Event::KeyPress(key: k) => 1,
                    Event::Focus => 2,
                    Event::Blur => 3,
                }
            "},
            expect![[r#"
                x is Event::Click(x: v_0, y: v_1)
                  let a = v_0
                  let b = v_1
                  branch 0
                x is Event::KeyPress(key: v_2)
                  let k = v_2
                  branch 1
                x is Event::Focus
                  branch 2
                x is Event::Blur
                  branch 3
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
                x is User(name: v_0, age: v_1)
                  let n = v_0
                  let a = v_1
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
                x is Foo(a: v_0, b: v_1)
                  v_1 is false
                    v_0 is false
                      branch 3
                    v_0 is true
                      branch 1
                  v_1 is true
                    v_0 is false
                      branch 2
                    v_0 is true
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
                error: Useless match expression: does not branch or bind any variables
                match x {
                ^^^^^^^^^
                    _ => 0,
                ^^^^^^^^^^^
                }
                ^
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
                x is Some(v_0)
                  let v = v_0
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
                x is Some(v_0)
                  v_0 is Some(v_1)
                    v_1 is false
                      branch 1
                    v_1 is true
                      branch 0
                  v_0 is None
                    branch 2
                x is None
                  branch 3
            "#]],
        );
    }

    #[test]
    fn nested_option_with_bool_missing_multiple() {
        check(
            Type::Option(Box::new(Type::Option(Box::new(Type::Bool)))),
            indoc! {"
                match x {
                    Some(None) => 1,
                    None => 2,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Some(Some(_))
                match x {
                ^^^^^^^^^
                    Some(None) => 1,
                ^^^^^^^^^^^^^^^^^^^^
                    None => 2,
                ^^^^^^^^^^^^^^
                }
                ^
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
                    (TypeName::new("Blue").unwrap(), vec![]),
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
                error: Useless match expression: does not branch or bind any variables
                match x {
                ^^^^^^^^^
                    User(name: _, age: _) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
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
                x is User(name: v_0, email: v_1)
                  v_1 is Some(v_2)
                    let n = v_0
                    let e = v_2
                    branch 0
                  v_1 is None
                    let n = v_0
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
    fn record_with_bool_fields_missing_multiple() {
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
                    Foo(a: false, b: false) => 1,
                }
            "},
            expect![[r#"
                error: Match expression is missing arms for: Foo(a: false, b: true), Foo(a: true, b: false)
                match x {
                ^^^^^^^^^
                    Foo(a: true, b: true) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Foo(a: false, b: false) => 1,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                x is Some(v_0)
                  v_0 is User(name: v_1, age: v_2)
                    let n = v_1
                    let a = v_2
                    branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn option_of_record_with_wildcard_fields() {
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
                    Some(User(name: _, age: _)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                x is Some(_)
                  branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn option_of_nested_records_with_wildcard_fields() {
        // Role is a nested record inside User
        let role_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Role").unwrap(),
            fields: vec![
                (FieldName::new("title").unwrap(), Type::String),
                (FieldName::new("salary").unwrap(), Type::Int),
            ],
        };
        check(
            Type::Option(Box::new(Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("role").unwrap(), role_type),
                    (FieldName::new("created_at").unwrap(), Type::Int),
                ],
            })),
            indoc! {"
                match x {
                    Some(User(role: Role(title: _, salary: _), created_at: _)) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                x is Some(_)
                  branch 0
                x is None
                  branch 1
            "#]],
        );
    }

    #[test]
    fn record_with_all_effectively_wildcard_fields() {
        // All fields are effectively wildcards (one literal, one nested record)
        let address_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Address").unwrap(),
            fields: vec![
                (FieldName::new("street").unwrap(), Type::String),
                (FieldName::new("city").unwrap(), Type::String),
            ],
        };
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("address").unwrap(), address_type),
                ],
            },
            indoc! {"
                match x {
                    User(name: _, address: Address(street: _, city: _)) => 0,
                }
            "},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match x {
                ^^^^^^^^^
                    User(name: _, address: Address(street: _, city: _)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn record_with_nested_wildcard_fields_followed_by_wildcard() {
        // First arm has all wildcard fields (effectively a wildcard), second arm is unreachable
        let role_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Role").unwrap(),
            fields: vec![
                (FieldName::new("title").unwrap(), Type::String),
                (FieldName::new("salary").unwrap(), Type::Int),
            ],
        };
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("role").unwrap(), role_type),
                    (FieldName::new("created_at").unwrap(), Type::Int),
                ],
            },
            indoc! {"
                match x {
                    User(role: Role(title: _, salary: _), created_at: _) => 0,
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
    fn record_with_binding_and_nested_wildcard_record() {
        // User has a binding for `name` but `address` is an effectively-wildcard record
        let address_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Address").unwrap(),
            fields: vec![
                (FieldName::new("street").unwrap(), Type::String),
                (FieldName::new("city").unwrap(), Type::String),
            ],
        };
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("User").unwrap(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("address").unwrap(), address_type),
                ],
            },
            indoc! {"
                match x {
                    User(name: n, address: Address(street: _, city: _)) => 0,
                }
            "},
            expect![[r#"
                x is User(name: v_0, address: _)
                  let n = v_0
                  branch 0
            "#]],
        );
    }

    #[test]
    fn enum_variant_with_binding_and_nested_wildcard_record() {
        // Result::Ok has a binding for `value` but `metadata` is an effectively-wildcard record
        let metadata_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Metadata").unwrap(),
            fields: vec![
                (FieldName::new("created").unwrap(), Type::Int),
                (FieldName::new("updated").unwrap(), Type::Int),
            ],
        };
        check(
            Type::Enum {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Result").unwrap(),
                variants: vec![
                    (
                        TypeName::new("Ok").unwrap(),
                        vec![
                            (FieldName::new("value").unwrap(), Type::String),
                            (FieldName::new("metadata").unwrap(), metadata_type),
                        ],
                    ),
                    (
                        TypeName::new("Err").unwrap(),
                        vec![(FieldName::new("message").unwrap(), Type::String)],
                    ),
                ],
            },
            indoc! {"
                match x {
                    Result::Ok(value: v, metadata: Metadata(created: _, updated: _)) => 0,
                    Result::Err(message: _) => 1,
                }
            "},
            expect![[r#"
                x is Result::Ok(value: v_0, metadata: _)
                  let v = v_0
                  branch 0
                x is Result::Err(message: _)
                  branch 1
            "#]],
        );
    }

    #[test]
    fn three_level_nested_records_with_middle_binding() {
        // Outer -> Middle (has binding) -> Inner (all wildcards)
        let inner_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Inner").unwrap(),
            fields: vec![
                (FieldName::new("x").unwrap(), Type::Int),
                (FieldName::new("y").unwrap(), Type::Int),
            ],
        };
        let middle_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Middle").unwrap(),
            fields: vec![
                (FieldName::new("name").unwrap(), Type::String),
                (FieldName::new("inner").unwrap(), inner_type),
            ],
        };
        check(
            Type::Record {
                module: ModuleName::new("test").unwrap(),
                name: TypeName::new("Outer").unwrap(),
                fields: vec![(FieldName::new("middle").unwrap(), middle_type)],
            },
            indoc! {"
                match x {
                    Outer(middle: Middle(name: n, inner: Inner(x: _, y: _))) => 0,
                }
            "},
            expect![[r#"
                x is Outer(middle: v_0)
                  v_0 is Middle(name: v_1, inner: _)
                    let n = v_1
                    branch 0
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
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
                error: Record 'User' is missing fields: age
                    User(name: n) => 0,
                    ^^^^
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
                error: Unknown field 'email' in record 'User'
                    User(email: e) => 0,
                         ^^^^^
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
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
                    (TypeName::new("Red").unwrap(), vec![]),
                    (TypeName::new("Green").unwrap(), vec![]),
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
