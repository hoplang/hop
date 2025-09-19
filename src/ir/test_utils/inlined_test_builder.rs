use crate::document::document_cursor::StringSpan;
use crate::dop::expr::{Expr, TypedExpr};
use crate::dop::{Type, VarName};
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntryPoint, InlinedNode, InlinedParameter,
};
use std::cell::RefCell;
use std::collections::BTreeMap;

pub struct InlinedTestBuilder {
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<InlinedParameter>,
}

impl InlinedTestBuilder {
    pub fn new(params: Vec<(String, Type)>) -> Self {
        let initial_vars = params.clone();

        Self {
            var_stack: RefCell::new(initial_vars.clone()),
            params: params
                .into_iter()
                .map(|(name, typ)| InlinedParameter {
                    var_name: VarName::try_from(name).unwrap(),
                    var_type: typ,
                })
                .collect(),
        }
    }

    pub fn build(&self, tag_name: &str, children: Vec<InlinedNode>) -> InlinedEntryPoint {
        InlinedEntryPoint {
            tag_name: StringSpan::new(tag_name.to_string()),
            params: self.params.clone(),
            children,
        }
    }

    // Expression builders for creating TypedExpr
    pub fn str_expr(&self, s: &str) -> TypedExpr {
        Expr::StringLiteral {
            value: s.to_string(),
            annotation: Type::String,
        }
    }

    pub fn num_expr(&self, n: f64) -> TypedExpr {
        Expr::NumberLiteral {
            value: serde_json::Number::from_f64(n).unwrap_or_else(|| serde_json::Number::from(0)),
            annotation: Type::Number,
        }
    }

    pub fn bool_expr(&self, b: bool) -> TypedExpr {
        Expr::BooleanLiteral {
            value: b,
            annotation: Type::Bool,
        }
    }

    pub fn var_expr(&self, name: &str) -> TypedExpr {
        let typ = self
            .var_stack
            .borrow()
            .iter()
            .rev()
            .find(|(var_name, _)| var_name == name)
            .map(|(_, typ)| typ.clone())
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' not found in scope. Available variables: {:?}",
                    name,
                    self.var_stack
                        .borrow()
                        .iter()
                        .map(|(n, _)| n.as_str())
                        .collect::<Vec<_>>()
                )
            });

        Expr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            annotation: typ,
        }
    }

    pub fn array_expr(&self, elements: Vec<TypedExpr>) -> TypedExpr {
        let element_type = elements
            .first()
            .map(|first| Box::new(first.annotation().clone()));

        Expr::ArrayLiteral {
            elements,
            annotation: Type::Array(element_type),
        }
    }

    pub fn object_expr(&self, props: Vec<(&str, TypedExpr)>) -> TypedExpr {
        let mut type_map = BTreeMap::new();
        for (key, expr) in &props {
            type_map.insert(key.to_string(), expr.annotation().clone());
        }

        Expr::ObjectLiteral {
            properties: props.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            annotation: Type::Object(type_map),
        }
    }

    pub fn prop_access_expr(&self, object: TypedExpr, property: &str) -> TypedExpr {
        let property_type = match object.annotation() {
            Type::Object(type_map) => type_map
                .get(property)
                .cloned()
                .unwrap_or_else(|| panic!("Property '{}' not found in object type", property)),
            _ => panic!("Cannot access property '{}' on non-object type", property),
        };

        Expr::PropertyAccess {
            object: Box::new(object),
            property: property.to_string(),
            annotation: property_type,
        }
    }

    // Node builders
    pub fn text(&self, s: &str) -> InlinedNode {
        InlinedNode::Text {
            value: StringSpan::new(s.to_string()),
        }
    }

    pub fn text_expr(&self, expr: TypedExpr) -> InlinedNode {
        assert_eq!(*expr.annotation(), Type::String, "{}", expr);
        InlinedNode::TextExpression { expression: expr }
    }

    pub fn if_node(&self, cond: TypedExpr, children: Vec<InlinedNode>) -> InlinedNode {
        assert_eq!(*cond.annotation(), Type::Bool, "{}", cond);
        InlinedNode::If {
            condition: cond,
            children,
        }
    }

    pub fn for_node<F>(&self, var: &str, array: TypedExpr, body_fn: F) -> InlinedNode
    where
        F: FnOnce(&Self) -> Vec<InlinedNode>,
    {
        let element_type = match array.annotation() {
            Type::Array(Some(elem_type)) => *elem_type.clone(),
            Type::Array(None) => panic!("Cannot iterate over array with unknown element type"),
            _ => panic!("Cannot iterate over non-array type"),
        };

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        let children = body_fn(self);

        self.var_stack.borrow_mut().pop();

        InlinedNode::For {
            var_name: VarName::try_from(var.to_string()).unwrap(),
            array_expr: array,
            children,
        }
    }

    pub fn let_node<F>(&self, var: &str, value: TypedExpr, body_fn: F) -> InlinedNode
    where
        F: FnOnce(&Self) -> Vec<InlinedNode>,
    {
        let value_type = value.annotation().clone();

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), value_type));

        let children = body_fn(self);

        self.var_stack.borrow_mut().pop();

        InlinedNode::Let {
            var: VarName::try_from(var.to_string()).unwrap(),
            value,
            children,
        }
    }

    pub fn doctype(&self, value: &str) -> InlinedNode {
        InlinedNode::Doctype {
            value: StringSpan::new(value.to_string()),
        }
    }

    pub fn html(
        &self,
        tag_name: &str,
        attributes: Vec<(&str, InlinedAttribute)>,
        children: Vec<InlinedNode>,
    ) -> InlinedNode {
        let attr_map = attributes
            .into_iter()
            .map(|(k, mut v)| {
                v.name = k.to_string(); // Set the attribute name
                (k.to_string(), v)
            })
            .collect();

        InlinedNode::Html {
            tag_name: StringSpan::new(tag_name.to_string()),
            attributes: attr_map,
            children,
        }
    }

    // Attribute builders
    pub fn attr_str(&self, value: &str) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: Some(InlinedAttributeValue::String(value.to_string())),
        }
    }

    pub fn attr_expr(&self, expr: TypedExpr) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: Some(InlinedAttributeValue::Expression(expr)),
        }
    }

    pub fn attr_bool(&self) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: None,
        }
    }
}

/// Ergonomic function for building inlined entrypoints in tests
///
/// This combines the creation of InlinedTestBuilder and building of the entrypoint into a single call.
///
/// # Arguments
/// * `tag_name` - The tag name for the entrypoint
/// * `params` - Vector of parameter (name, type) tuples
/// * `children_fn` - Closure that takes an InlinedTestBuilder and returns a vector of InlinedNodes
///
/// # Example
/// ```rust
/// let entrypoint = build_inlined("test", vec![("x".to_string(), Type::String)], |t| {
///     vec![t.text_expr(t.var_expr("x"))]
/// });
/// ```
pub fn build_inlined<F>(
    tag_name: &str,
    params: Vec<(String, Type)>,
    children_fn: F,
) -> InlinedEntryPoint
where
    F: FnOnce(&InlinedTestBuilder) -> Vec<InlinedNode>,
{
    let builder = InlinedTestBuilder::new(params);
    let children = children_fn(&builder);
    builder.build(tag_name, children)
}
