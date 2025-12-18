use crate::document::document_cursor::StringSpan;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::VarName;
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
    InlinedParameter,
};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use std::cell::RefCell;

pub fn build_inlined_auto<F>(
    tag_name: &str,
    params: Vec<(&str, Type)>,
    children_fn: F,
) -> InlinedComponentDeclaration
where
    F: FnOnce(&mut InlinedAutoBuilder),
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    let inner_builder = InlinedTestBuilder::new(params_owned);
    let mut builder = InlinedAutoBuilder::new(inner_builder);
    children_fn(&mut builder);
    builder.build(tag_name)
}

pub struct InlinedTestBuilder {
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<InlinedParameter>,
}

#[allow(dead_code)]
impl InlinedTestBuilder {
    fn new(params: Vec<(String, Type)>) -> Self {
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

    fn build(
        &self,
        component_name: &str,
        children: Vec<InlinedNode>,
    ) -> InlinedComponentDeclaration {
        InlinedComponentDeclaration {
            module_name: ModuleName::new("test").unwrap(),
            component_name: ComponentName::new(component_name.to_string()).unwrap(),
            params: self.params.clone(),
            children,
        }
    }

    // Expression builders for creating SimpleTypedExpr
    pub fn str_expr(&self, s: &str) -> TypedExpr {
        TypedExpr::StringLiteral {
            value: s.to_string(),
        }
    }

    pub fn num_expr(&self, n: f64) -> TypedExpr {
        TypedExpr::FloatLiteral { value: n }
    }

    pub fn bool_expr(&self, b: bool) -> TypedExpr {
        TypedExpr::BooleanLiteral { value: b }
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

        TypedExpr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            kind: typ,
        }
    }

    pub fn array_expr(&self, elements: Vec<TypedExpr>) -> TypedExpr {
        let element_type = elements
            .first()
            .map(|first| Box::new(first.as_type().clone()))
            .expect("Cannot create empty array literal in test builder");

        TypedExpr::ArrayLiteral {
            elements,
            kind: Type::Array(element_type),
        }
    }

    // Node builders
    pub fn text(&self, s: &str) -> InlinedNode {
        InlinedNode::Text {
            value: StringSpan::new(s.to_string()),
        }
    }

    pub fn text_expr(&self, expr: TypedExpr) -> InlinedNode {
        assert_eq!(*expr.as_type(), Type::String, "{}", expr);
        InlinedNode::TextExpression { expression: expr }
    }

    pub fn if_node(&self, cond: TypedExpr, children: Vec<InlinedNode>) -> InlinedNode {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        InlinedNode::If {
            condition: cond,
            children,
        }
    }

    pub fn for_node<F>(&self, var: &str, array: TypedExpr, body_fn: F) -> InlinedNode
    where
        F: FnOnce(&Self) -> Vec<InlinedNode>,
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
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
        let value_type = value.as_type().clone();

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

    // Convenience methods for common HTML elements
    pub fn div(
        &self,
        attributes: Vec<(&str, InlinedAttribute)>,
        children: Vec<InlinedNode>,
    ) -> InlinedNode {
        self.html("div", attributes, children)
    }

    pub fn ul(
        &self,
        attributes: Vec<(&str, InlinedAttribute)>,
        children: Vec<InlinedNode>,
    ) -> InlinedNode {
        self.html("ul", attributes, children)
    }

    pub fn li(
        &self,
        attributes: Vec<(&str, InlinedAttribute)>,
        children: Vec<InlinedNode>,
    ) -> InlinedNode {
        self.html("li", attributes, children)
    }

    // Attribute builders
    pub fn attr_str(&self, value: &str) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: Some(InlinedAttributeValue::String(value.to_string())),
        }
    }

    pub fn attr_exprs(&self, exprs: Vec<TypedExpr>) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: Some(InlinedAttributeValue::Expressions(exprs)),
        }
    }

    pub fn attr_bool(&self) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(), // Will be set when used in html()
            value: None,
        }
    }
}

pub struct InlinedAutoBuilder {
    inner: InlinedTestBuilder,
    children: Vec<InlinedNode>,
}

#[allow(dead_code)]
impl InlinedAutoBuilder {
    fn new(inner: InlinedTestBuilder) -> Self {
        Self {
            inner,
            children: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            inner: InlinedTestBuilder {
                var_stack: self.inner.var_stack.clone(),
                params: self.inner.params.clone(),
            },
            children: Vec::new(),
        }
    }

    fn build(self, tag_name: &str) -> InlinedComponentDeclaration {
        self.inner.build(tag_name, self.children)
    }

    // Node methods that auto-collect
    pub fn text(&mut self, s: &str) {
        self.children.push(self.inner.text(s));
    }

    pub fn text_expr(&mut self, expr: TypedExpr) {
        self.children.push(self.inner.text_expr(expr));
    }

    pub fn if_node<F>(&mut self, cond: TypedExpr, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut inner_builder = self.new_scoped();
        children_fn(&mut inner_builder);
        let children = inner_builder.children;
        self.children.push(self.inner.if_node(cond, children));
    }

    pub fn for_node<F>(&mut self, var: &str, array: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Extract element type from array
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        // Push the loop variable onto the stack
        self.inner
            .var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        // Create scoped builder and evaluate the body
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        // Pop the loop variable from the stack
        self.inner.var_stack.borrow_mut().pop();

        self.children
            .push(self.inner.for_node(var, array, |_| children));
    }

    pub fn let_node<F>(&mut self, var: &str, value: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Get the type from the value expression
        let value_type = value.as_type().clone();

        // Push the variable onto the stack
        self.inner
            .var_stack
            .borrow_mut()
            .push((var.to_string(), value_type));

        // Create scoped builder and evaluate the body
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        // Pop the variable from the stack
        self.inner.var_stack.borrow_mut().pop();

        self.children
            .push(self.inner.let_node(var, value, |_| children));
    }

    pub fn doctype(&mut self, value: &str) {
        self.children.push(self.inner.doctype(value));
    }

    pub fn html<F>(
        &mut self,
        tag_name: &str,
        attributes: Vec<(&str, InlinedAttribute)>,
        children_fn: F,
    ) where
        F: FnOnce(&mut Self),
    {
        let mut inner_builder = self.new_scoped();
        children_fn(&mut inner_builder);
        let children = inner_builder.children;
        self.children
            .push(self.inner.html(tag_name, attributes, children));
    }

    // Convenience methods for common HTML elements
    pub fn div<F>(&mut self, attributes: Vec<(&str, InlinedAttribute)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("div", attributes, children_fn);
    }

    pub fn ul<F>(&mut self, attributes: Vec<(&str, InlinedAttribute)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("ul", attributes, children_fn);
    }

    pub fn li<F>(&mut self, attributes: Vec<(&str, InlinedAttribute)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("li", attributes, children_fn);
    }

    // Expression methods - delegate to inner builder
    pub fn str_expr(&self, s: &str) -> TypedExpr {
        self.inner.str_expr(s)
    }

    pub fn num_expr(&self, n: f64) -> TypedExpr {
        self.inner.num_expr(n)
    }

    pub fn bool_expr(&self, b: bool) -> TypedExpr {
        self.inner.bool_expr(b)
    }

    pub fn var_expr(&self, name: &str) -> TypedExpr {
        self.inner.var_expr(name)
    }

    pub fn array_expr(&self, elements: Vec<TypedExpr>) -> TypedExpr {
        self.inner.array_expr(elements)
    }

    // Attribute methods - delegate to inner builder
    pub fn attr_str(&self, value: &str) -> InlinedAttribute {
        self.inner.attr_str(value)
    }

    pub fn attr_exprs(&self, exprs: Vec<TypedExpr>) -> InlinedAttribute {
        self.inner.attr_exprs(exprs)
    }

    pub fn attr_bool(&self) -> InlinedAttribute {
        self.inner.attr_bool()
    }
}
