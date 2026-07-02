use crate::expr::Type;
use crate::expr::typing::r#type::ExamplesAnnotation;
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::ir::semantics::evaluator::Value;
use crate::symbols::field_name::FieldName;
use rand::{Rng, RngExt};

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

/// Generates a random [`Value`] matching the given [`Type`].
///
/// Recursively generates values for nested types (arrays, options, records, enums).
/// An optional `examples` annotation can customize generation for String and Int types.
pub fn random_value(
    rng: &mut impl Rng,
    ty: &Type,
    examples: Option<&ExamplesAnnotation>,
    registry: &TypeRegistry,
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
            let min = examples.and_then(|e| e.min_len).unwrap_or(0).max(0) as usize;
            let max = examples.and_then(|e| e.max_len).unwrap_or(5).max(0) as usize;
            let len = rng.random_range(min..=max);
            Value::Array(
                (0..len)
                    .map(|_| random_value(rng, inner, None, registry))
                    .collect(),
            )
        }
        ResolvedType::Option(inner) => {
            if rng.random_bool(0.5) {
                Value::Some(Box::new(random_value(rng, inner, None, registry)))
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
                        random_value(rng, ty, examples.as_ref(), registry),
                    )
                })
                .collect();
            Value::Record(map)
        }
        ResolvedType::Enum { variants, .. } => {
            let idx = rng.random_range(0..variants.len());
            let variant = &variants[idx];
            let fields = variant
                .fields
                .iter()
                .map(|(name, ty, examples): &(_, _, _)| {
                    (
                        FieldName::new(name.as_str()).unwrap(),
                        random_value(rng, ty, examples.as_ref(), registry),
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
    use rand::SeedableRng;
    use rand::rngs::StdRng;
    use std::sync::Arc;

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
