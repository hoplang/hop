use crate::document_id::DocumentId;
use crate::expr::Type;
use crate::expr::typing::r#type::{EnumVariant, ExamplesAnnotation};
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::ir::semantics::evaluator::Value;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use rand::{Rng, RngExt};

/// Recursion depth at which generation stops making random choices and
/// steers toward the shortest terminating value instead.
const MAX_DEPTH: usize = 8;

/// Generate a random string matching the given regex pattern.
fn random_string_from_pattern(rng: &mut impl Rng, pattern: &str) -> Value {
    match rand_regex::Regex::compile(pattern, 100) {
        Ok(regex) => Value::String(rng.sample::<String, _>(&regex)),
        // Fall back to word list if the regex is invalid (should be caught by type checker)
        Err(_) => random_word(rng),
    }
}

fn random_word(rng: &mut impl Rng) -> Value {
    const WORDS: &[&str] = &["foo", "bar", "baz"];
    Value::String(WORDS[rng.random_range(0..WORDS.len())].to_string())
}

/// Whether a value of the given type can be constructed without expanding
/// any named type in `visiting`. Restores `visiting` before returning.
fn can_construct(
    ty: &Type,
    registry: &TypeRegistry,
    visiting: &mut Vec<(DocumentId, TypeName)>,
) -> bool {
    if let Type::Named { module, name } = ty {
        if visiting.iter().any(|(m, n)| m == module && n == name) {
            return false;
        }
        visiting.push((module.clone(), name.clone()));
    }
    let ok = match registry.resolve(ty).expect("named type must be registered") {
        ResolvedType::String
        | ResolvedType::Bool
        | ResolvedType::Int
        | ResolvedType::Float
        | ResolvedType::Fragment
        // Arrays and options can always terminate as empty and None
        | ResolvedType::Array(_)
        | ResolvedType::Option(_) => true,
        ResolvedType::Record { fields, .. } => fields
            .iter()
            .all(|(_, ty, _)| can_construct(ty, registry, visiting)),
        ResolvedType::Enum { variants, .. } => variants.iter().any(|v| {
            v.fields
                .iter()
                .all(|(_, ty, _)| can_construct(ty, registry, visiting))
        }),
    };
    if matches!(ty, Type::Named { .. }) {
        visiting.pop();
    }
    ok
}

/// Generates a random [`Value`] matching the given [`Type`].
///
/// Recursively generates values for nested types (arrays, options, records, enums).
/// An optional `examples` annotation can customize generation for String and Int types.
///
/// Recursive types are supported. Beyond a fixed depth, options become None,
/// arrays become empty and enums only pick variants that can finish without
/// re-entering a type already being expanded.
///
/// Panics if the type has no finite values at all, e.g. a record whose
/// fields require the record itself.
pub fn random_value(
    rng: &mut impl Rng,
    ty: &Type,
    examples: Option<&ExamplesAnnotation>,
    registry: &TypeRegistry,
) -> Value {
    random_value_at_depth(rng, ty, examples, registry, 0, &mut Vec::new())
}

fn random_value_at_depth(
    rng: &mut impl Rng,
    ty: &Type,
    examples: Option<&ExamplesAnnotation>,
    registry: &TypeRegistry,
    depth: usize,
    visiting: &mut Vec<(DocumentId, TypeName)>,
) -> Value {
    if depth == MAX_DEPTH && !can_construct(ty, registry, &mut Vec::new()) {
        panic!("cannot generate a value for infinitely recursive type {ty}");
    }
    if depth >= MAX_DEPTH {
        if let Type::Named { module, name } = ty {
            visiting.push((module.clone(), name.clone()));
        }
    }
    let value = generate(rng, ty, examples, registry, depth, visiting);
    if depth >= MAX_DEPTH && matches!(ty, Type::Named { .. }) {
        visiting.pop();
    }
    value
}

fn generate(
    rng: &mut impl Rng,
    ty: &Type,
    examples: Option<&ExamplesAnnotation>,
    registry: &TypeRegistry,
    depth: usize,
    visiting: &mut Vec<(DocumentId, TypeName)>,
) -> Value {
    match registry.resolve(ty).expect("named type must be registered") {
        ResolvedType::String => match examples.and_then(|e| e.pattern.as_deref()) {
            Some(p) => random_string_from_pattern(rng, p),
            None => random_word(rng),
        },
        ResolvedType::Bool => Value::Bool(rng.random_bool(0.5)),
        ResolvedType::Int => {
            let min = examples.and_then(|e| e.min).unwrap_or(0);
            let max = examples.and_then(|e| e.max).unwrap_or(100);
            Value::Int(rng.random_range(min..=max))
        }
        ResolvedType::Float => Value::Float(rng.random_range(0.0..100.0)),
        ResolvedType::Fragment => Value::String("<span>sample</span>".to_string()),
        ResolvedType::Array(inner) => {
            let len = if depth >= MAX_DEPTH {
                0
            } else {
                let min = examples.and_then(|e| e.min_len).unwrap_or(0).max(0) as usize;
                let max = examples.and_then(|e| e.max_len).unwrap_or(5).max(0) as usize;
                rng.random_range(min..=max)
            };
            Value::Array(
                (0..len)
                    .map(|_| random_value_at_depth(rng, inner, None, registry, depth + 1, visiting))
                    .collect(),
            )
        }
        ResolvedType::Option(inner) => {
            if depth < MAX_DEPTH && rng.random_bool(0.5) {
                Value::Some(Box::new(random_value_at_depth(
                    rng,
                    inner,
                    None,
                    registry,
                    depth + 1,
                    visiting,
                )))
            } else {
                Value::None
            }
        }
        ResolvedType::Record { fields, .. } => {
            let map = fields
                .iter()
                .map(|(name, ty, examples)| {
                    (
                        name.clone(),
                        random_value_at_depth(
                            rng,
                            ty,
                            examples.as_ref(),
                            registry,
                            depth + 1,
                            visiting,
                        ),
                    )
                })
                .collect();
            Value::Record(map)
        }
        ResolvedType::Enum { variants, .. } => {
            let variant = if depth >= MAX_DEPTH {
                // Nonempty since the type passed can_construct when it
                // crossed MAX_DEPTH or was chosen by an enclosing filter
                let candidates: Vec<&EnumVariant> = variants
                    .iter()
                    .filter(|v| {
                        v.fields
                            .iter()
                            .all(|(_, ty, _)| can_construct(ty, registry, visiting))
                    })
                    .collect();
                candidates[rng.random_range(0..candidates.len())]
            } else {
                &variants[rng.random_range(0..variants.len())]
            };
            let fields = variant
                .fields
                .iter()
                .map(|(name, ty, examples): &(_, _, _)| {
                    (
                        FieldName::new(name.as_str()).unwrap(),
                        random_value_at_depth(
                            rng,
                            ty,
                            examples.as_ref(),
                            registry,
                            depth + 1,
                            visiting,
                        ),
                    )
                })
                .collect();
            Value::Enum {
                variant_name: variant.name.clone(),
                fields,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::typing::type_registry_builder::TypeRegistryBuilder;
    use rand::SeedableRng;
    use rand::rngs::StdRng;
    use std::sync::Arc;

    #[test]
    fn recursive_enum_generation_terminates() {
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Tree",
                [
                    (
                        "Node",
                        vec![("left", "Tree"), ("middle", "Tree"), ("right", "Tree")],
                    ),
                    ("Leaf", vec![]),
                ],
            )
            .build();
        let ty = types.named("Tree");
        for seed in 0..50 {
            let mut rng = StdRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, None, types.registry());
        }
    }

    #[test]
    fn recursive_record_through_option_terminates() {
        let types = TypeRegistryBuilder::new()
            .record("Node", [("value", "Int"), ("next", "Option[Node]")])
            .build();
        let ty = types.named("Node");
        for seed in 0..50 {
            let mut rng = StdRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, None, types.registry());
        }
    }

    #[test]
    fn mutually_recursive_enums_terminate() {
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Expr",
                [
                    ("Lit", vec![("value", "Int")]),
                    ("Group", vec![("items", "ExprList")]),
                ],
            )
            .enum_(
                "ExprList",
                [
                    ("Cons", vec![("head", "Expr"), ("tail", "ExprList")]),
                    ("Nil", vec![]),
                ],
            )
            .build();
        let ty = types.named("Expr");
        for seed in 0..50 {
            let mut rng = StdRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, None, types.registry());
        }
    }

    #[test]
    #[should_panic(expected = "cannot generate a value")]
    fn uninhabited_recursive_record_panics() {
        let types = TypeRegistryBuilder::new()
            .record("Node", [("value", "Int"), ("next", "Node")])
            .build();
        let ty = types.named("Node");
        let mut rng = StdRng::seed_from_u64(0);
        random_value(&mut rng, &ty, None, types.registry());
    }

    #[test]
    fn array_length_respects_min_len_and_max_len() {
        let ty = Type::Array(Arc::new(Type::Int));
        let examples = ExamplesAnnotation {
            min_len: Some(3),
            max_len: Some(4),
            ..Default::default()
        };
        let registry = TypeRegistry::default();
        for seed in 0..50 {
            let mut rng = StdRng::seed_from_u64(seed);
            let value = random_value(&mut rng, &ty, Some(&examples), &registry);
            let Value::Array(items) = value else {
                panic!("expected Value::Array");
            };
            assert!(
                (3..=4).contains(&items.len()),
                "length {} out of [3, 4] for seed {}",
                items.len(),
                seed
            );
        }
    }

    #[test]
    fn array_length_defaults_to_zero_through_five() {
        let ty = Type::Array(Arc::new(Type::Int));
        let registry = TypeRegistry::default();
        for seed in 0..50 {
            let mut rng = StdRng::seed_from_u64(seed);
            let value = random_value(&mut rng, &ty, None, &registry);
            let Value::Array(items) = value else {
                panic!("expected Value::Array");
            };
            assert!(items.len() <= 5);
        }
    }
}
