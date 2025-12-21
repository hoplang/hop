use super::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
    InlinedParameter,
};
use crate::document::document_cursor::StringSpan;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::VarName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use std::cell::RefCell;

pub fn build_inlined<F>(
    tag_name: &str,
    params: Vec<(&str, Type)>,
    children_fn: F,
) -> InlinedComponentDeclaration
where
    F: FnOnce(&mut InlinedBuilder),
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    let mut builder = InlinedBuilder::new(params_owned);
    children_fn(&mut builder);
    builder.build(tag_name)
}

pub struct InlinedBuilder {
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<InlinedParameter>,
    children: Vec<InlinedNode>,
}

impl InlinedBuilder {
    fn new(params: Vec<(String, Type)>) -> Self {
        let initial_vars = params.clone();

        Self {
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(name, typ)| InlinedParameter {
                    var_name: VarName::try_from(name).unwrap(),
                    var_type: typ,
                })
                .collect(),
            children: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            var_stack: self.var_stack.clone(),
            params: self.params.clone(),
            children: Vec::new(),
        }
    }

    fn build(self, component_name: &str) -> InlinedComponentDeclaration {
        InlinedComponentDeclaration {
            module_name: ModuleName::new("test").unwrap(),
            component_name: ComponentName::new(component_name.to_string()).unwrap(),
            params: self.params,
            children: self.children,
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

        TypedExpr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            kind: typ,
        }
    }

    pub fn text(&mut self, s: &str) {
        self.children.push(InlinedNode::Text {
            value: StringSpan::new(s.to_string()),
        });
    }

    pub fn text_expr(&mut self, expr: TypedExpr) {
        assert_eq!(*expr.as_type(), Type::String, "{}", expr);
        self.children
            .push(InlinedNode::TextExpression { expression: expr });
    }

    pub fn if_node<F>(&mut self, cond: TypedExpr, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        let mut inner_builder = self.new_scoped();
        children_fn(&mut inner_builder);
        self.children.push(InlinedNode::If {
            condition: cond,
            children: inner_builder.children,
        });
    }

    pub fn for_node<F>(&mut self, var: &str, array: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        self.var_stack.borrow_mut().pop();

        self.children.push(InlinedNode::For {
            var_name: VarName::try_from(var.to_string()).unwrap(),
            array_expr: array,
            children,
        });
    }

    pub fn doctype(&mut self, value: &str) {
        self.children.push(InlinedNode::Doctype {
            value: StringSpan::new(value.to_string()),
        });
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

        let attr_map = attributes
            .into_iter()
            .map(|(k, mut v)| {
                v.name = k.to_string();
                (k.to_string(), v)
            })
            .collect();

        self.children.push(InlinedNode::Html {
            tag_name: StringSpan::new(tag_name.to_string()),
            attributes: attr_map,
            children: inner_builder.children,
        });
    }

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

    pub fn attr_str(&self, value: &str) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(),
            value: Some(InlinedAttributeValue::String(value.to_string())),
        }
    }

    pub fn attr_exprs(&self, exprs: Vec<TypedExpr>) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(),
            value: Some(InlinedAttributeValue::Expressions(exprs)),
        }
    }
}
