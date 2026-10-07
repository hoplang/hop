use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::document::CheapString;
use crate::hop::typing::Type;
use crate::hop::typing::TypedExpr;
use crate::hop::typing::compile_match::compile_match;
use crate::hop::typing::type_registry::ResolvedType;
use crate::hop::typing::type_registry_builder::{TestTypes, TypeRegistryBuilder};
use crate::hop::typing::typed_module::{TypedPageDeclaration, TypedParameter};
use crate::hop::typing::typed_pattern::Constructor;
use crate::hop::typing::typed_pattern::TypedPattern;
use crate::hop::typing::{TypedAttribute, TypedAttrs, TypedLoopSource, TypedRecordUpdateField};
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

pub fn build_page_no_params<F>(page_name: &str, children_fn: F) -> TypedPageDeclaration
where
    F: FnOnce(&mut TypedAstBuilder),
{
    let mut builder = TypedAstBuilder::new(TypeRegistryBuilder::new().build(), vec![]);
    children_fn(&mut builder);
    builder.build(page_name)
}

pub fn build_page<F, P, T>(page_name: &str, params: P, children_fn: F) -> TypedPageDeclaration
where
    F: FnOnce(&mut TypedAstBuilder),
    P: IntoIterator<Item = (&'static str, T)>,
    T: Into<Type>,
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v.into()))
        .collect();
    let mut builder = TypedAstBuilder::new(TypeRegistryBuilder::new().build(), params_owned);
    children_fn(&mut builder);
    builder.build(page_name)
}

pub fn build_page_with_types<'a, F>(
    types: TypeRegistryBuilder,
    page_name: &str,
    params: impl IntoIterator<Item = (&'a str, &'a str)>,
    children_fn: F,
) -> TypedPageDeclaration
where
    F: FnOnce(&mut TypedAstBuilder),
{
    let types = types.build();
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(name, typ)| (name.to_string(), types.resolve(typ)))
        .collect();
    let mut builder = TypedAstBuilder::new(types, params_owned);
    children_fn(&mut builder);
    builder.build(page_name)
}

pub struct TypedAstBuilder {
    types: Rc<TestTypes>,
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<TypedParameter>,
    children: Vec<TypedExpr>,
}

impl TypedAstBuilder {
    fn new(types: TestTypes, params: Vec<(String, Type)>) -> Self {
        let initial_vars = params.clone();

        Self {
            types: Rc::new(types),
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(name, typ)| TypedParameter {
                    var_name: VarName::try_from(name).unwrap(),
                    var_type: typ,
                    examples: None,
                })
                .collect(),
            children: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            types: self.types.clone(),
            var_stack: self.var_stack.clone(),
            params: self.params.clone(),
            children: Vec::new(),
        }
    }

    fn build(self, page_name: &str) -> TypedPageDeclaration {
        TypedPageDeclaration {
            name: TypeName::parse(page_name).unwrap(),
            head: TypedExpr::HtmlConcat { parts: Vec::new() },
            body: TypedExpr::HtmlConcat {
                parts: self.children,
            },
            params: self.params,
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
            typ,
        }
    }

    pub fn string_literal(&self, value: &str) -> TypedExpr {
        TypedExpr::StringLiteral {
            value: CheapString::new(value.to_string()),
        }
    }

    pub fn int_literal(&self, value: i32) -> TypedExpr {
        TypedExpr::IntLiteral { value }
    }

    pub fn field_access(&self, record: TypedExpr, field: &str) -> TypedExpr {
        let record_type = record.typ();
        let Some(ResolvedType::Record {
            name: record_name,
            fields,
            ..
        }) = self.types.registry().resolve(&record_type)
        else {
            panic!("Cannot access field '{field}' on non-record type {record_type}");
        };
        let typ = fields
            .iter()
            .find(|f| f.name.as_str() == field)
            .map(|f| f.typ.clone())
            .unwrap_or_else(|| panic!("Field '{field}' not found in record '{record_name}'"));
        TypedExpr::FieldAccess {
            record: Box::new(record),
            field: FieldName::parse(field).unwrap(),
            typ,
        }
    }

    /// Build a record expression that supplies `fields` and reads the others
    /// from `base`, e.g. User {...user, name: "Jane"}.
    pub fn record_update(&self, base: TypedExpr, fields: Vec<(&str, TypedExpr)>) -> TypedExpr {
        let typ = base.typ();
        let Some(ResolvedType::Record {
            name: record_name,
            fields: record_fields,
            ..
        }) = self.types.registry().resolve(&typ)
        else {
            panic!("Cannot update non-record type {typ}");
        };
        let mut explicit: HashMap<&str, TypedExpr> = fields.into_iter().collect();
        let all_fields = record_fields
            .iter()
            .map(|record_field| {
                let field = match explicit.remove(record_field.name.as_str()) {
                    Some(value) => {
                        assert_eq!(
                            value.typ(),
                            record_field.typ,
                            "Field '{}' of record '{record_name}' has mismatched type, got: {value}",
                            record_field.name
                        );
                        TypedRecordUpdateField::Explicit(value)
                    }
                    None => TypedRecordUpdateField::FromBase(record_field.typ.clone()),
                };
                (record_field.name.clone(), field)
            })
            .collect::<Vec<_>>();
        assert!(
            explicit.is_empty(),
            "Fields {:?} not found in record '{record_name}'",
            explicit.keys().collect::<Vec<_>>()
        );
        TypedExpr::RecordUpdate {
            type_name: record_name.clone(),
            base: Box::new(base),
            fields: all_fields,
            typ,
        }
    }

    pub fn text(&mut self, s: &str) {
        self.children.push(TypedExpr::HtmlRaw {
            value: CheapString::new(s.to_string()),
        });
    }

    pub fn text_expr(&mut self, expr: TypedExpr) {
        assert_eq!(expr.typ(), Type::String, "{}", expr);
        self.children.push(TypedExpr::HtmlEscape {
            expr: Box::new(expr),
        });
    }

    pub fn if_html<F>(&mut self, cond: TypedExpr, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        assert_eq!(cond.typ(), Type::Bool, "{}", cond);
        self.bool_match_html(cond, children_fn, |_| {});
    }

    pub fn for_html<F>(&mut self, var: &str, array: TypedExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let element_type = match array.typ() {
            Type::Array(elem_type) => elem_type.as_ref().clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let children = inner_builder.children;

        self.var_stack.borrow_mut().pop();

        self.children.push(TypedExpr::For {
            var_name: Some(VarName::try_from(var.to_string()).unwrap()),
            source: Box::new(TypedLoopSource::Array(array)),
            body: Box::new(TypedExpr::HtmlConcat { parts: children }),
            typ: Type::Html,
        });
    }

    pub fn html<F>(&mut self, tag_name: &str, attributes: Vec<(&str, TypedExpr)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut inner_builder = self.new_scoped();
        children_fn(&mut inner_builder);

        let attrs: Vec<TypedAttribute> = attributes
            .into_iter()
            .map(|(name, value)| TypedAttribute {
                name: AttributeName::new(CheapString::new(name.to_string()))
                    .expect("builder html() called with an invalid attribute name"),
                value,
            })
            .collect();

        self.children.push(TypedExpr::Element {
            element: HtmlElementKind::parse(tag_name)
                .expect("builder html() called with an unrecognized tag name"),
            attrs: TypedAttrs {
                attributes: attrs,
                spread: None,
            },
            children: Box::new(TypedExpr::HtmlConcat {
                parts: inner_builder.children,
            }),
        });
    }

    pub fn div<F>(&mut self, attributes: Vec<(&str, TypedExpr)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("div", attributes, children_fn);
    }

    pub fn ul<F>(&mut self, attributes: Vec<(&str, TypedExpr)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("ul", attributes, children_fn);
    }

    pub fn li<F>(&mut self, attributes: Vec<(&str, TypedExpr)>, children_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        self.html("li", attributes, children_fn);
    }

    pub fn bool_match_html<FTrue, FFalse>(
        &mut self,
        subject: TypedExpr,
        true_children_fn: FTrue,
        false_children_fn: FFalse,
    ) where
        FTrue: FnOnce(&mut Self),
        FFalse: FnOnce(&mut Self),
    {
        let mut true_builder = self.new_scoped();
        true_children_fn(&mut true_builder);
        let mut false_builder = self.new_scoped();
        false_children_fn(&mut false_builder);

        let patterns: Vec<TypedPattern> = [Constructor::BoolTrue, Constructor::BoolFalse]
            .into_iter()
            .map(|constructor| TypedPattern::Constructor {
                constructor,
                args: Vec::new(),
                fields: Vec::new(),
            })
            .collect();
        let decision = compile_match(self.types.registry(), &patterns, Type::Bool)
            .expect("a match on true and false is exhaustive");
        let bodies = [
            TypedExpr::HtmlConcat {
                parts: true_builder.children,
            },
            TypedExpr::HtmlConcat {
                parts: false_builder.children,
            },
        ];

        self.children.push(TypedExpr::Match {
            subject: Box::new(subject),
            arms: patterns.into_iter().zip(bodies).collect(),
            decision,
            typ: Type::Html,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(page: TypedPageDeclaration, expected: Expect) {
        expected.assert_eq(&format!("{}\n", page.to_doc().pretty(60)));
    }

    #[test]
    fn simple_text() {
        check(
            build_page_no_params("Hello", |b| {
                b.text("Hello, World!");
            }),
            expect![[r#"
                page Hello() {
                  fn body() -> Html {
                    concat(raw("Hello, World!"))
                  }
                }
            "#]],
        );
    }

    #[test]
    fn html_with_attributes() {
        check(
            build_page_no_params("Card", |b| {
                b.div(vec![("class", b.string_literal("container"))], |b| {
                    b.text("Content");
                });
            }),
            expect![[r#"
                page Card() {
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "div",
                        attrs: [class: escape("container")],
                        children: concat(raw("Content")),
                      ),
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn page_with_params() {
        check(
            build_page("Greeting", [("name", Type::String)], |b| {
                b.text("Hello, ");
                b.text_expr(b.var_expr("name"));
            }),
            expect![[r#"
                page Greeting(name: String) {
                  fn body() -> Html {
                    concat(raw("Hello, "), escape(name))
                  }
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_scoped_variable() {
        check(
            build_page(
                "ItemList",
                [("items", Type::Array(Box::new(Type::String)))],
                |b| {
                    b.ul(vec![], |b| {
                        b.for_html("item", b.var_expr("items"), |b| {
                            b.li(vec![], |b| {
                                b.text_expr(b.var_expr("item"));
                            });
                        });
                    });
                },
            ),
            expect![[r#"
                page ItemList(items: Array[String]) {
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "ul",
                        attrs: [],
                        children: concat(
                          for item in items {
                            concat(
                              html(
                                tag: "li",
                                attrs: [],
                                children: concat(escape(item)),
                              ),
                            )
                          },
                        ),
                      ),
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn if_conditional() {
        check(
            build_page("Toggle", [("visible", Type::Bool)], |b| {
                b.if_html(b.var_expr("visible"), |b| {
                    b.div(vec![], |b| {
                        b.text("Shown");
                    });
                });
            }),
            expect![[r#"
                page Toggle(visible: Bool) {
                  fn body() -> Html {
                    concat(
                      match visible {
                        true => concat(
                          html(
                            tag: "div",
                            attrs: [],
                            children: concat(raw("Shown")),
                          ),
                        ),
                        false => concat(),
                      },
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    #[should_panic(expected = "Variable 'missing' not found in scope")]
    fn panics_on_undefined_variable() {
        build_page_no_params("Bad", |b| {
            b.text_expr(b.var_expr("missing"));
        });
    }

    #[test]
    #[should_panic(expected = "Variable 'item' not found in scope")]
    fn loop_variable_not_accessible_outside_loop() {
        build_page(
            "Bad",
            [("items", Type::Array(Box::new(Type::String)))],
            |b| {
                b.for_html("item", b.var_expr("items"), |_| {});
                // item should not be accessible here
                b.text_expr(b.var_expr("item"));
            },
        );
    }
}
