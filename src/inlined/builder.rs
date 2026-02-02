use std::cell::RefCell;
use std::sync::Arc;

use super::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypointDeclaration, InlinedNode,
    InlinedParameter,
};
use crate::document::CheapString;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::VarName;
use crate::hop::semantics::typed_node::TypedLoopSource;
use crate::hop::symbols::component_name::ComponentName;

/// Helper function to extract (VarName, Arc<Type>) from a TypedExpr.
/// Panics if the expression is not a variable reference.
fn extract_var_subject(expr: &TypedExpr) -> (VarName, Arc<Type>) {
    match expr {
        TypedExpr::Var { value, kind, .. } => (value.clone(), kind.clone()),
        _ => panic!("Match subject must be a variable reference, got {:?}", expr),
    }
}

pub fn build_inlined_no_params<F>(tag_name: &str, children_fn: F) -> InlinedEntrypointDeclaration
where
    F: FnOnce(&mut InlinedBuilder),
{
    let mut builder = InlinedBuilder::new(vec![]);
    children_fn(&mut builder);
    builder.build(tag_name)
}

pub fn build_inlined<F, P, T>(
    tag_name: &str,
    params: P,
    children_fn: F,
) -> InlinedEntrypointDeclaration
where
    F: FnOnce(&mut InlinedBuilder),
    P: IntoIterator<Item = (&'static str, T)>,
    T: Into<Arc<Type>>,
{
    let params_owned: Vec<(String, Arc<Type>)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v.into()))
        .collect();
    let mut builder = InlinedBuilder::new(params_owned);
    children_fn(&mut builder);
    builder.build(tag_name)
}

pub struct InlinedBuilder {
    var_stack: RefCell<Vec<(String, Arc<Type>)>>,
    params: Vec<InlinedParameter>,
    children: Vec<Arc<InlinedNode>>,
}

impl InlinedBuilder {
    fn new(params: Vec<(String, Arc<Type>)>) -> Self {
        let initial_vars = params.clone();

        Self {
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(name, typ)| InlinedParameter {
                    var_name: VarName::try_from(name).unwrap(),
                    var_type: typ,
                    default_value: None,
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

    fn build(self, component_name: &str) -> InlinedEntrypointDeclaration {
        InlinedEntrypointDeclaration {
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
        self.children.push(Arc::new(InlinedNode::Text {
            value: CheapString::new(s.to_string()),
        }));
    }

    pub fn text_expr(&mut self, expr: TypedExpr) {
        assert_eq!(*expr.as_type(), Type::String, "{}", expr);
        self.children
            .push(Arc::new(InlinedNode::TextExpression { expression: expr }));
    }

    pub fn if_node<F>(&mut self, cond: TypedExpr, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        let mut inner_builder = self.new_scoped();
        children_fn(&mut inner_builder);
        self.children.push(Arc::new(InlinedNode::If {
            condition: cond,
            children: inner_builder.children,
        }));
    }

    pub fn for_node<F>(&mut self, var: &str, array: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => elem_type.clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        self.var_stack.borrow_mut().pop();

        self.children.push(Arc::new(InlinedNode::For {
            var_name: Some(VarName::try_from(var.to_string()).unwrap()),
            source: TypedLoopSource::Array(array),
            children,
        }));
    }

    pub fn doctype(&mut self, value: &str) {
        self.children.push(Arc::new(InlinedNode::Doctype {
            value: CheapString::new(value.to_string()),
        }));
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

        let attrs: Vec<InlinedAttribute> = attributes
            .into_iter()
            .map(|(k, mut v)| {
                v.name = CheapString::new(k.to_string());
                v
            })
            .collect();

        self.children.push(Arc::new(InlinedNode::Html {
            tag_name: CheapString::new(tag_name.to_string()),
            attributes: attrs,
            children: inner_builder.children,
        }));
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
            name: CheapString::new(String::new()),
            value: Some(InlinedAttributeValue::String(CheapString::new(
                value.to_string(),
            ))),
        }
    }

    pub fn attr_expr(&self, expr: TypedExpr) -> InlinedAttribute {
        InlinedAttribute {
            name: CheapString::new(String::new()),
            value: Some(InlinedAttributeValue::Expression(expr)),
        }
    }

    pub fn bool_match_node<FTrue, FFalse>(
        &mut self,
        subject: TypedExpr,
        true_children_fn: FTrue,
        false_children_fn: FFalse,
    ) where
        FTrue: FnOnce(&mut Self),
        FFalse: FnOnce(&mut Self),
    {
        use crate::dop::patterns::Match;

        let mut true_builder = self.new_scoped();
        true_children_fn(&mut true_builder);
        let mut false_builder = self.new_scoped();
        false_children_fn(&mut false_builder);

        self.children.push(Arc::new(InlinedNode::Match {
            match_: Match::Bool {
                subject: extract_var_subject(&subject),
                true_body: Box::new(true_builder.children),
                false_body: Box::new(false_builder.children),
            },
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(component: InlinedEntrypointDeclaration, expected: Expect) {
        expected.assert_eq(&component.to_string());
    }

    #[test]
    fn simple_text() {
        check(
            build_inlined_no_params("Hello", |b| {
                b.text("Hello, World!");
            }),
            expect![[r#"
                entrypoint Hello() {
                  Hello, World!
                }
            "#]],
        );
    }

    #[test]
    fn html_with_attributes() {
        check(
            build_inlined_no_params("Card", |b| {
                b.div(vec![("class", b.attr_str("container"))], |b| {
                    b.text("Content");
                });
            }),
            expect![[r#"
                entrypoint Card() {
                  <div class="container">
                    Content
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn component_with_params() {
        check(
            build_inlined("Greeting", [("name", Type::String)], |b| {
                b.text("Hello, ");
                b.text_expr(b.var_expr("name"));
            }),
            expect![[r#"
                entrypoint Greeting(name: String) {
                  Hello, 
                  {name}
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_scoped_variable() {
        check(
            build_inlined(
                "List",
                [("items", Type::Array(Arc::new(Type::String)))],
                |b| {
                    b.ul(vec![], |b| {
                        b.for_node("item", b.var_expr("items"), |b| {
                            b.li(vec![], |b| {
                                b.text_expr(b.var_expr("item"));
                            });
                        });
                    });
                },
            ),
            expect![[r#"
                entrypoint List(items: Array[String]) {
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                }
            "#]],
        );
    }

    #[test]
    fn if_conditional() {
        check(
            build_inlined("Toggle", [("visible", Type::Bool)], |b| {
                b.if_node(b.var_expr("visible"), |b| {
                    b.div(vec![], |b| {
                        b.text("Shown");
                    });
                });
            }),
            expect![[r#"
                entrypoint Toggle(visible: Bool) {
                  <if {visible}>
                    <div>
                      Shown
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    #[should_panic(expected = "Variable 'missing' not found in scope")]
    fn panics_on_undefined_variable() {
        build_inlined_no_params("Bad", |b| {
            b.text_expr(b.var_expr("missing"));
        });
    }

    #[test]
    #[should_panic(expected = "Variable 'item' not found in scope")]
    fn loop_variable_not_accessible_outside_loop() {
        build_inlined(
            "Bad",
            [("items", Type::Array(Arc::new(Type::String)))],
            |b| {
                b.for_node("item", b.var_expr("items"), |_| {});
                // item should not be accessible here
                b.text_expr(b.var_expr("item"));
            },
        );
    }
}
