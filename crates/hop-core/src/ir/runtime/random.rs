use super::bounded_vec::BoundedVec;
use super::html_node::{Html, HtmlNode};
use super::value::{MAX_ARRAY_LENGTH, Value};
use crate::html::HtmlElementKind;
use crate::ir::ir_type::{IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrType};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations, IrVariant};
use crate::symbols::type_name::TypeName;
use rand::{Rng, RngExt};
use std::rc::Rc;

/// Recursion depth at which generation stops making random choices and
/// steers toward the shortest terminating value instead.
const MAX_DEPTH: usize = 8;

fn random_word(rng: &mut impl Rng) -> Value<'static> {
    const WORDS: &[&str] = &["foo", "bar", "baz"];
    Value::String(WORDS[rng.random_range(0..WORDS.len())].into())
}

/// Whether a value of the given type can be constructed without expanding
/// any named type in `visiting`. Restores `visiting` before returning.
fn can_construct(
    ty: &IrDataType,
    types: &IrTypeDeclarations,
    visiting: &mut Vec<TypeName>,
) -> bool {
    if let Some(name) = named(ty) {
        if visiting.contains(name) {
            return false;
        }
        visiting.push(name.clone());
    }
    let ok = match ty {
        IrDataType::String
        | IrDataType::Bool
        | IrDataType::Int
        | IrDataType::Float
        // Arrays and options can always terminate as empty and None
        | IrDataType::Array(_)
        | IrDataType::Option(_) => true,
        IrDataType::Tuple(elements) => elements
            .iter()
            .all(|element| can_construct(element, types, visiting)),
        IrDataType::Record(name) => record_fields(types, name)
            .iter()
            .all(|field| can_construct(&field.typ, types, visiting)),
        IrDataType::Enum(name) => enum_variants(types, name).iter().any(|v| {
            v.fields
                .iter()
                .all(|field| can_construct(&field.typ, types, visiting))
        }),
    };
    if named(ty).is_some() {
        visiting.pop();
    }
    ok
}

fn named(ty: &IrDataType) -> Option<&TypeName> {
    match ty {
        IrDataType::Record(name) | IrDataType::Enum(name) => Some(name),
        _ => None,
    }
}

fn record_fields<'a>(types: &'a IrTypeDeclarations, name: &TypeName) -> &'a [IrField] {
    &types.record(name).expect("record must be declared").fields
}

fn enum_variants<'a>(types: &'a IrTypeDeclarations, name: &TypeName) -> &'a [IrVariant] {
    &types
        .enumeration(name)
        .expect("enum must be declared")
        .variants
}

/// Generates a random [`Value`] matching the given [`IrType`].
///
/// Recursively generates values for nested types (arrays, options, records, enums).
///
/// Recursive types are supported. Beyond a fixed depth, options become None,
/// arrays become empty and enums only pick variants that can finish without
/// re-entering a type already being expanded.
///
/// Panics if the type has no finite values at all, e.g. a record whose
/// fields require the record itself, or if it is the type of a closure,
/// which no entrypoint takes.
pub fn random_value(rng: &mut impl Rng, ty: &IrType, types: &IrTypeDeclarations) -> Value<'static> {
    match ty {
        IrType::Data(IrFunctionDataType::Data(data)) => {
            random_value_at_depth(rng, data, types, 0, &mut Vec::new())
        }
        IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => Value::Html(
            Html::new(HtmlNode::Element {
                element: HtmlElementKind::Span,
                attributes: Vec::new(),
                children: Html::new(HtmlNode::Text("sample".to_string())).unwrap(),
            })
            .unwrap(),
        ),
        IrType::Data(IrFunctionDataType::Function(_))
        | IrType::Html(IrFunctionHtmlType::Function(_)) => {
            panic!("an entrypoint takes no closure")
        }
    }
}

fn random_value_at_depth(
    rng: &mut impl Rng,
    ty: &IrDataType,
    types: &IrTypeDeclarations,
    depth: usize,
    visiting: &mut Vec<TypeName>,
) -> Value<'static> {
    if depth == MAX_DEPTH && !can_construct(ty, types, &mut Vec::new()) {
        panic!("cannot generate a value for infinitely recursive type {ty}");
    }
    if depth >= MAX_DEPTH
        && let Some(name) = named(ty)
    {
        visiting.push(name.clone());
    }
    let value = generate(rng, ty, types, depth, visiting);
    if depth >= MAX_DEPTH && named(ty).is_some() {
        visiting.pop();
    }
    value
}

fn generate(
    rng: &mut impl Rng,
    ty: &IrDataType,
    types: &IrTypeDeclarations,
    depth: usize,
    visiting: &mut Vec<TypeName>,
) -> Value<'static> {
    match ty {
        IrDataType::String => random_word(rng),
        IrDataType::Bool => Value::Bool(rng.random_bool(0.5)),
        IrDataType::Int => Value::Int(rng.random_range(0..=100)),
        IrDataType::Float => Value::Float(rng.random_range(0.0..100.0)),
        IrDataType::Array(inner) => {
            let len = if depth >= MAX_DEPTH {
                0
            } else {
                rng.random_range(0..=MAX_ARRAY_LENGTH.min(5))
            };
            let elements =
                (0..len).map(|_| random_value_at_depth(rng, inner, types, depth + 1, visiting));
            Value::Array(Rc::new(
                BoundedVec::new(elements).expect("the length is within the limit"),
            ))
        }
        IrDataType::Option(inner) => {
            if depth < MAX_DEPTH && rng.random_bool(0.5) {
                Value::Option(Some(Rc::new(random_value_at_depth(
                    rng,
                    inner,
                    types,
                    depth + 1,
                    visiting,
                ))))
            } else {
                Value::Option(None)
            }
        }
        IrDataType::Tuple(elements) => Value::Tuple(
            elements
                .iter()
                .map(|element| random_value_at_depth(rng, element, types, depth + 1, visiting))
                .collect(),
        ),
        IrDataType::Record(name) => {
            let map = record_fields(types, name)
                .iter()
                .map(|field| {
                    (
                        field.name.clone(),
                        random_value_at_depth(rng, &field.typ, types, depth + 1, visiting),
                    )
                })
                .collect();
            Value::Record(map)
        }
        IrDataType::Enum(name) => {
            let variants = enum_variants(types, name);
            let variant = if depth >= MAX_DEPTH {
                // Nonempty since the type passed can_construct when it
                // crossed MAX_DEPTH or was chosen by an enclosing filter
                let candidates: Vec<&IrVariant> = variants
                    .iter()
                    .filter(|v| {
                        v.fields
                            .iter()
                            .all(|field| can_construct(&field.typ, types, visiting))
                    })
                    .collect();
                candidates[rng.random_range(0..candidates.len())]
            } else {
                &variants[rng.random_range(0..variants.len())]
            };
            let fields = variant
                .fields
                .iter()
                .map(|field| {
                    (
                        field.name.clone(),
                        random_value_at_depth(rng, &field.typ, types, depth + 1, visiting),
                    )
                })
                .collect();
            Value::Enum(variant.name.clone(), fields)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::Ty;
    use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
    use rand::SeedableRng;
    use rand::rngs::SmallRng;

    #[test]
    fn recursive_enum_generation_terminates() {
        let types = IrTypeDeclarationsBuilder::new()
            .enum_(
                "Tree",
                [
                    (
                        "Node",
                        vec![
                            ("left", Ty::named("Tree")),
                            ("middle", Ty::named("Tree")),
                            ("right", Ty::named("Tree")),
                        ],
                    ),
                    ("Leaf", vec![]),
                ],
            )
            .build();
        let ty = types.ty(Ty::named("Tree"));
        for seed in 0..50 {
            let mut rng = SmallRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, &types);
        }
    }

    #[test]
    fn recursive_record_through_option_terminates() {
        let types = IrTypeDeclarationsBuilder::new()
            .record(
                "Node",
                [("value", Ty::Int), ("next", Ty::option(Ty::named("Node")))],
            )
            .build();
        let ty = types.ty(Ty::named("Node"));
        for seed in 0..50 {
            let mut rng = SmallRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, &types);
        }
    }

    #[test]
    fn mutually_recursive_enums_terminate() {
        let types = IrTypeDeclarationsBuilder::new()
            .enum_(
                "Expr",
                [
                    ("Lit", vec![("value", Ty::Int)]),
                    ("Group", vec![("items", Ty::named("ExprList"))]),
                ],
            )
            .enum_(
                "ExprList",
                [
                    (
                        "Cons",
                        vec![("head", Ty::named("Expr")), ("tail", Ty::named("ExprList"))],
                    ),
                    ("Nil", vec![]),
                ],
            )
            .build();
        let ty = types.ty(Ty::named("Expr"));
        for seed in 0..50 {
            let mut rng = SmallRng::seed_from_u64(seed);
            random_value(&mut rng, &ty, &types);
        }
    }

    #[test]
    #[should_panic(expected = "cannot generate a value")]
    fn uninhabited_recursive_record_panics() {
        let types = IrTypeDeclarationsBuilder::new()
            .record("Node", [("value", Ty::Int), ("next", Ty::named("Node"))])
            .build();
        let ty = types.ty(Ty::named("Node"));
        let mut rng = SmallRng::seed_from_u64(0);
        random_value(&mut rng, &ty, &types);
    }

    #[test]
    fn array_length_defaults_to_zero_through_five() {
        let ty = IrType::array(IrDataType::Int);
        let types = IrTypeDeclarations::default();
        for seed in 0..50 {
            let mut rng = SmallRng::seed_from_u64(seed);
            let value = random_value(&mut rng, &ty, &types);
            let Value::Array(items) = value else {
                panic!("expected Value::Array");
            };
            assert!(items.len() <= 5);
        }
    }
}
