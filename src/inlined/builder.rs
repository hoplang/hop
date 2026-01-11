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

/// Helper function to extract (VarName, Type) from a TypedExpr.
/// Panics if the expression is not a variable reference.
fn extract_var_subject(expr: &TypedExpr) -> (VarName, Type) {
    match expr {
        TypedExpr::Var { value, kind, .. } => (value.clone(), kind.clone()),
        _ => panic!("Match subject must be a variable reference, got {:?}", expr),
    }
}

pub fn build_inlined<F, P>(tag_name: &str, params: P, children_fn: F) -> InlinedComponentDeclaration
where
    F: FnOnce(&mut InlinedBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
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

    pub fn attr_expr(&self, expr: TypedExpr) -> InlinedAttribute {
        InlinedAttribute {
            name: String::new(),
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

        self.children.push(InlinedNode::Match {
            match_: Match::Bool {
                subject: extract_var_subject(&subject),
                true_body: Box::new(true_builder.children),
                false_body: Box::new(false_builder.children),
            },
        });
    }

    pub fn let_node<F>(&mut self, var: &str, value: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let var_type = value.as_type().clone();
        self.var_stack
            .borrow_mut()
            .push((var.to_string(), var_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        self.var_stack.borrow_mut().pop();

        self.children.push(InlinedNode::Let {
            var: VarName::try_from(var.to_string()).unwrap(),
            value,
            children,
        });
    }

    pub fn bool_match_expr(
        &self,
        subject: TypedExpr,
        true_value: &str,
        false_value: &str,
    ) -> TypedExpr {
        use crate::dop::patterns::Match;

        TypedExpr::Match {
            match_: Match::Bool {
                subject: extract_var_subject(&subject),
                true_body: Box::new(TypedExpr::StringLiteral {
                    value: true_value.to_string(),
                }),
                false_body: Box::new(TypedExpr::StringLiteral {
                    value: false_value.to_string(),
                }),
            },
            kind: Type::String,
        }
    }

    pub fn string_lit(&self, value: &str) -> TypedExpr {
        TypedExpr::StringLiteral {
            value: value.to_string(),
        }
    }

    pub fn bool_lit(&self, value: bool) -> TypedExpr {
        TypedExpr::BooleanLiteral { value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(component: InlinedComponentDeclaration, expected: Expect) {
        expected.assert_eq(&component.to_string());
    }

    #[test]
    fn simple_text() {
        check(
            build_inlined("Hello", [], |b| {
                b.text("Hello, World!");
            }),
            expect![[r#"
                <Hello>
                  Hello, World!
                </Hello>
            "#]],
        );
    }

    #[test]
    fn html_with_attributes() {
        check(
            build_inlined("Card", [], |b| {
                b.div(vec![("class", b.attr_str("container"))], |b| {
                    b.text("Content");
                });
            }),
            expect![[r#"
                <Card>
                  <div class="container">
                    Content
                  </div>
                </Card>
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
                <Greeting {name: String}>
                  Hello, 
                  {name}
                </Greeting>
            "#]],
        );
    }

    #[test]
    fn for_loop_with_scoped_variable() {
        check(
            build_inlined(
                "List",
                [("items", Type::Array(Box::new(Type::String)))],
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
                <List {items: Array[String]}>
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                </List>
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
                <Toggle {visible: Bool}>
                  <if {visible}>
                    <div>
                      Shown
                    </div>
                  </if>
                </Toggle>
            "#]],
        );
    }

    #[test]
    #[should_panic(expected = "Variable 'missing' not found in scope")]
    fn panics_on_undefined_variable() {
        build_inlined("Bad", [], |b| {
            b.text_expr(b.var_expr("missing"));
        });
    }

    #[test]
    #[should_panic(expected = "Variable 'item' not found in scope")]
    fn loop_variable_not_accessible_outside_loop() {
        build_inlined(
            "Bad",
            [("items", Type::Array(Box::new(Type::String)))],
            |b| {
                b.for_node("item", b.var_expr("items"), |_| {});
                // item should not be accessible here
                b.text_expr(b.var_expr("item"));
            },
        );
    }
}
