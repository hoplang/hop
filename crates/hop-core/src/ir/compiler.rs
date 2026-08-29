use std::borrow::Cow;
use std::sync::Arc;

use crate::asset_rewriter::AssetRewriter;
use crate::document::CheapString;
use crate::document_id::DocumentId;
use crate::expr::Type;
use crate::expr::TypedExpr;
use crate::expr::patterns::{EnumMatchArm, Match};
use crate::hop::assembly::AssembledPageDeclaration;
use crate::hop::typing::typed_ast::{
    TypedComponentDeclaration, TypedEnumDeclaration, TypedFunctionDeclaration,
    TypedRecordDeclaration,
};
use crate::hop::typing::typed_node::{
    TypedArgumentValue, TypedAttribute, TypedAttributeValue, TypedLoopSource, TypedNode,
};
use crate::ir::expr_id::ExprId;
use crate::ir::expr_id::ExprIdCounter;
use crate::ir::ir_var::IrVar;
use crate::ir::pure_module::PureForSource;
use crate::ir::var_id::VarId;
use crate::ir::var_id::VarIdCounter;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::HashMap;

use super::pure_module::{
    PureArgument, PureExpr, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use super::writer_module::{WriterEnumDeclaration, WriterParameter, WriterRecordDeclaration};

pub fn compile(
    pages: Vec<AssembledPageDeclaration>,
    components: &[(DocumentId, &TypedComponentDeclaration)],
    source_functions: &[&TypedFunctionDeclaration],
    records: &[&TypedRecordDeclaration],
    enums: &[&TypedEnumDeclaration],
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
) -> PureModule {
    let mut expr_ids = ExprIdCounter::new();
    let mut var_ids = VarIdCounter::new();
    // A component's rest parameter is passed like any other argument, so a
    // call site needs the name the callee gave it.
    let rest_params: HashMap<(DocumentId, TypeName), VarName> = components
        .iter()
        .filter_map(|(module, decl)| {
            decl.rest_param
                .as_ref()
                .map(|rest| ((module.clone(), decl.component_name.clone()), rest.clone()))
        })
        .collect();
    let mut compiler = Compiler::new(&mut expr_ids, &mut var_ids, asset_rewriter, rest_params);

    let pages = pages
        .into_iter()
        .map(|page| compiler.compile_page_decl(page))
        .collect();
    let mut functions: Vec<PureFunctionDeclaration> = components
        .iter()
        .map(|(_, decl)| compiler.compile_component_decl(decl))
        .collect();
    functions.extend(
        source_functions
            .iter()
            .map(|decl| compiler.compile_function_decl(decl)),
    );

    // Records and enums carry no code, so they are converted as-is. Both are
    // sorted by name since callers collect them from an unordered set of
    // modules and the IR must be deterministic.
    let mut records: Vec<WriterRecordDeclaration> = records
        .iter()
        .map(|record| WriterRecordDeclaration {
            name: record.name.clone(),
            fields: record.fields.clone(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    let mut enums: Vec<WriterEnumDeclaration> = enums
        .iter()
        .map(|enum_decl| WriterEnumDeclaration {
            name: enum_decl.name.clone(),
            variants: enum_decl.variants.clone(),
        })
        .collect();
    enums.sort_by(|a, b| a.name.cmp(&b.name));

    PureModule {
        pages,
        functions,
        records,
        enums,
        expr_ids,
        var_ids,
    }
}

struct Compiler<'a> {
    expr_id_counter: &'a mut ExprIdCounter,
    var_id_counter: &'a mut VarIdCounter,
    scopes: Vec<Vec<(VarName, VarId)>>,
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    /// The name each rest-carrying component gave its rest parameter.
    rest_params: HashMap<(DocumentId, TypeName), VarName>,
}

impl<'a> Compiler<'a> {
    fn new(
        expr_id_counter: &'a mut ExprIdCounter,
        var_id_counter: &'a mut VarIdCounter,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
        rest_params: HashMap<(DocumentId, TypeName), VarName>,
    ) -> Self {
        Compiler {
            expr_id_counter,
            var_id_counter,
            scopes: vec![Vec::new()],
            asset_rewriter,
            rest_params,
        }
    }

    fn compile_component_decl(
        &mut self,
        decl: &TypedComponentDeclaration,
    ) -> PureFunctionDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len());
        for param in &decl.params {
            parameters.push(WriterParameter {
                var: self.bind(&param.var_name),
                name: param.var_name.clone(),
                typ: param.var_type.clone(),
            });
        }

        // The rest is an ordinary parameter holding pre-rendered attribute
        // text.
        if let Some(rest) = &decl.rest_param {
            parameters.push(WriterParameter {
                var: self.bind(rest),
                name: rest.clone(),
                typ: Arc::new(Type::Fragment),
            });
        }

        let declaration = PureFunctionDeclaration {
            name: decl.component_name.clone().into(),
            parameters,
            return_type: Arc::new(Type::Fragment),
            body: self.compile_nodes(&decl.children),
        };
        self.pop_scope();
        declaration
    }

    fn compile_function_decl(
        &mut self,
        decl: &TypedFunctionDeclaration,
    ) -> PureFunctionDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len());
        for param in &decl.params {
            parameters.push(WriterParameter {
                var: self.bind(&param.var_name),
                name: param.var_name.clone(),
                typ: param.var_type.clone(),
            });
        }

        let declaration = PureFunctionDeclaration {
            name: decl.name.clone().into(),
            parameters,
            return_type: decl.return_type.clone(),
            body: self.compile_expr(&decl.body),
        };
        self.pop_scope();
        declaration
    }

    fn compile_page_decl(&mut self, page: AssembledPageDeclaration) -> PurePageDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(page.params.len());
        for param in page.params {
            parameters.push(WriterParameter {
                var: self.bind(&param.var_name),
                name: param.var_name,
                typ: param.var_type,
            });
        }

        let declaration = PurePageDeclaration {
            name: page.name,
            parameters,
            body: self.compile_nodes(&page.children),
        };
        self.pop_scope();
        declaration
    }

    fn next_var_id(&mut self) -> VarId {
        self.var_id_counter.next()
    }

    fn next_expr_id(&mut self) -> ExprId {
        self.expr_id_counter.next()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope stack should not be empty");
    }

    fn bind(&mut self, name: &VarName) -> IrVar {
        let id = self.next_var_id();
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
            .push((name.clone(), id));
        IrVar::new(id)
    }

    fn resolve(&mut self, name: &VarName) -> IrVar {
        for scope in self.scopes.iter().rev() {
            if let Some((_, id)) = scope.iter().rev().find(|(n, _)| n == name) {
                return IrVar::new(*id);
            }
        }
        panic!("undefined variable: {name}");
    }

    fn compile_nodes(&mut self, nodes: &[TypedNode]) -> PureExpr {
        let mut parts = Vec::new();
        for node in nodes {
            self.compile_node(node, &mut parts);
        }
        PureExpr::FragmentConcat {
            parts,
            id: self.next_expr_id(),
        }
    }

    fn compile_node(&mut self, node: &TypedNode, output: &mut Vec<PureExpr>) {
        match node {
            TypedNode::Text { value } => {
                output.push(PureExpr::FragmentRaw {
                    content: value.to_string(),
                    id: self.next_expr_id(),
                });
            }

            TypedNode::TextExpression { expression } => {
                if matches!(expression.as_type(), Type::Fragment) {
                    output.push(self.compile_expr(expression));
                } else {
                    output.push(PureExpr::FragmentEscape {
                        expr: Box::new(self.compile_expr(expression)),
                        id: self.next_expr_id(),
                    });
                }
            }

            TypedNode::Html {
                element,
                attributes,
                rest_spread,
                children,
            } => {
                output.push(PureExpr::FragmentRaw {
                    content: format!("<{}", element.as_str()),
                    id: self.next_expr_id(),
                });
                for attr in attributes {
                    self.compile_attribute_opt(attr, output);
                }
                // The spread lands after the element's own attributes,
                // whatever position `...rest` was written in..
                if let Some(rest) = rest_spread {
                    let var = self.resolve(rest);
                    output.push(PureExpr::VariableReference {
                        value: var,
                        kind: Arc::new(Type::Fragment),
                        id: self.next_expr_id(),
                    });
                }
                output.push(PureExpr::FragmentRaw {
                    content: ">".to_string(),
                    id: self.next_expr_id(),
                });
                if !element.is_void() {
                    for child in children {
                        self.compile_node(child, output);
                    }
                    output.push(PureExpr::FragmentRaw {
                        content: format!("</{}>", element.as_str()),
                        id: self.next_expr_id(),
                    });
                }
            }

            TypedNode::If {
                condition,
                children,
                ..
            } => {
                output.push(PureExpr::Match {
                    match_: Match::Bool {
                        subject: Box::new(self.compile_expr(condition)),
                        true_body: Box::new(self.compile_nodes(children)),
                        false_body: Box::new(PureExpr::FragmentConcat {
                            parts: Vec::new(),
                            id: self.next_expr_id(),
                        }),
                    },
                    kind: Arc::new(Type::Fragment),
                    id: self.next_expr_id(),
                });
            }

            TypedNode::For {
                var_name,
                source,
                children,
                ..
            } => {
                let pure_source = match source {
                    TypedLoopSource::Array(array_expr) => {
                        PureForSource::Array(self.compile_expr(array_expr))
                    }
                    TypedLoopSource::RangeInclusive { start, end } => {
                        PureForSource::RangeInclusive {
                            start: self.compile_expr(start),
                            end: self.compile_expr(end),
                        }
                    }
                };
                self.push_scope();
                let var = var_name.as_ref().map(|name| self.bind(name));
                let body = self.compile_nodes(children);
                self.pop_scope();
                output.push(PureExpr::FragmentFor {
                    var,
                    source: Box::new(pure_source),
                    body: Box::new(body),
                    id: self.next_expr_id(),
                });
            }

            TypedNode::Let {
                var,
                value,
                children,
            } => {
                let value = self.compile_expr(value);
                self.push_scope();
                let pure_var = self.bind(var);
                let body = self.compile_nodes(children);
                self.pop_scope();
                output.push(PureExpr::Let {
                    var: pure_var,
                    value: Box::new(value),
                    body: Box::new(body),
                    kind: Arc::new(Type::Fragment),
                    id: self.next_expr_id(),
                });
            }

            TypedNode::Match { match_ } => {
                let compiled_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(self.compile_expr(subject)),
                        true_body: Box::new(self.compile_nodes(true_body)),
                        false_body: Box::new(self.compile_nodes(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let subject = Box::new(self.compile_expr(subject));
                        self.push_scope();
                        let binding = some_arm_binding.as_ref().map(|name| self.bind(name));
                        let some_body = self.compile_nodes(some_arm_body);
                        self.pop_scope();
                        let none_body = self.compile_nodes(none_arm_body);
                        Match::Option {
                            subject,
                            some_arm_binding: binding,
                            some_arm_body: Box::new(some_body),
                            none_arm_body: Box::new(none_body),
                        }
                    }
                    Match::Enum { subject, arms } => {
                        let subject = Box::new(self.compile_expr(subject));
                        let arms = arms
                            .iter()
                            .map(|arm| {
                                self.push_scope();
                                let bindings = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, name)| (field.clone(), self.bind(name)))
                                    .collect();
                                let body = self.compile_nodes(&arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings,
                                    body,
                                }
                            })
                            .collect();
                        Match::Enum { subject, arms }
                    }
                };
                output.push(PureExpr::Match {
                    match_: compiled_match,
                    kind: Arc::new(Type::Fragment),
                    id: self.next_expr_id(),
                });
            }

            TypedNode::ComponentInvocation {
                component_name,
                component_module,
                args,
                extra_attributes,
                rest_spread,
            } => {
                let mut compiled_args: Vec<PureArgument> = args
                    .iter()
                    .map(|arg| PureArgument {
                        name: arg.name.clone(),
                        expr: self.compile_argument_value(&arg.value),
                    })
                    .collect();

                let callee = (component_module.clone(), component_name.clone());
                if let Some(rest_param) = self.rest_params.get(&callee).cloned() {
                    compiled_args.push(PureArgument {
                        name: rest_param,
                        expr: self.compile_rest(extra_attributes, rest_spread.as_ref()),
                    });
                } else {
                    // A spread into a callee that declares no rest is not a
                    // mistake: the spread was carrying typed parameters, and
                    // those are passed explicitly above, so nothing is left
                    // for it to forward.
                    assert!(
                        extra_attributes.is_empty(),
                        "<{}> declares no rest, but the call site supplies attributes for one",
                        component_name.as_str()
                    );
                }

                output.push(PureExpr::FunctionCall {
                    function_name: component_name.clone().into(),
                    args: compiled_args,
                    kind: Arc::new(Type::Fragment),
                    id: self.next_expr_id(),
                });
            }
        }
    }

    /// Render the attributes a call site passes on to the callee's rest into
    /// the text they will occupy inside an open tag: ` name="value"`, escaped
    /// exactly as if written on the element directly.
    fn compile_rest(
        &mut self,
        extra_attributes: &[TypedAttribute],
        rest_spread: Option<&VarName>,
    ) -> PureExpr {
        let mut parts = Vec::new();
        for attr in extra_attributes {
            self.compile_attribute_opt(attr, &mut parts);
        }
        if let Some(rest) = rest_spread {
            let var = self.resolve(rest);
            parts.push(PureExpr::VariableReference {
                value: var,
                kind: Arc::new(Type::Fragment),
                id: self.next_expr_id(),
            });
        }
        PureExpr::FragmentConcat {
            parts,
            id: self.next_expr_id(),
        }
    }

    fn compile_argument_value(&mut self, value: &TypedArgumentValue) -> PureExpr {
        match value {
            TypedArgumentValue::Expr(expr) => self.compile_expr(expr),
            TypedArgumentValue::Fragment(nodes) => self.compile_nodes(nodes),
        }
    }

    fn compile_attribute_opt(&mut self, attr: &TypedAttribute, output: &mut Vec<PureExpr>) {
        match &attr.value {
            Some(value) => self.compile_attribute(&attr.name, value, output),
            // A valueless attribute is its own text.
            None => output.push(PureExpr::FragmentRaw {
                content: format!(" {}", attr.name.as_str()),
                id: self.next_expr_id(),
            }),
        }
    }

    /// Helper to compile an attribute to PureIR fragment parts
    fn compile_attribute(
        &mut self,
        name: &CheapString,
        value: &TypedAttributeValue,
        output: &mut Vec<PureExpr>,
    ) {
        match value {
            TypedAttributeValue::String(s) => {
                output.push(PureExpr::FragmentRaw {
                    content: format!(" {}=\"{}\"", name.as_str(), s.as_str()),
                    id: self.next_expr_id(),
                });
            }
            TypedAttributeValue::Expression(expr) => {
                assert!(
                    expr.as_type() == &Type::String,
                    "attribute `{}` holds {}, but attribute values must be String",
                    name.as_str(),
                    expr.as_type()
                );
                // String attributes: output attribute="value"
                output.push(PureExpr::FragmentRaw {
                    content: format!(" {}=\"", name.as_str()),
                    id: self.next_expr_id(),
                });
                output.push(PureExpr::FragmentEscape {
                    expr: Box::new(self.compile_expr(expr)),
                    id: self.next_expr_id(),
                });
                output.push(PureExpr::FragmentRaw {
                    content: "\"".to_string(),
                    id: self.next_expr_id(),
                });
            }
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> PureExpr {
        let expr_id = self.next_expr_id();

        match expr {
            TypedExpr::Var { value, kind, .. } => PureExpr::VariableReference {
                value: self.resolve(value),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::FieldAccess {
                record: object,
                field,
                kind,
                ..
            } => PureExpr::FieldAccess {
                record: Box::new(self.compile_expr(object)),
                field: field.clone(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::BooleanNegation { operand, .. } => PureExpr::BooleanNegation {
                operand: Box::new(self.compile_expr(operand)),
                id: expr_id,
            },
            TypedExpr::NumericNegation {
                operand,
                operand_type,
            } => PureExpr::NumericNegation {
                operand: Box::new(self.compile_expr(operand)),
                operand_type: operand_type.clone(),
                id: expr_id,
            },
            TypedExpr::ArrayLiteral { elements, kind, .. } => PureExpr::ArrayLiteral {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::RecordLiteral {
                record_name,
                fields,
                kind,
                ..
            } => PureExpr::RecordLiteral {
                record_name: record_name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.compile_expr(v)))
                    .collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::StringLiteral { value, .. } => PureExpr::StringLiteral {
                value: match process_escape_sequences(value.as_str()) {
                    Cow::Borrowed(_) => value.clone(),
                    Cow::Owned(unescaped) => CheapString::new(unescaped),
                },
                id: expr_id,
            },
            TypedExpr::Asset { path } => {
                let rewritten = match &self.asset_rewriter {
                    Some(rewriter) => {
                        rewriter.rewrite(&DocumentId::new(path.trim_start_matches('/')).unwrap())
                    }
                    None => path.to_string(),
                };
                PureExpr::StringLiteral {
                    value: CheapString::new(process_escape_sequences(&rewritten).into_owned()),
                    id: expr_id,
                }
            }
            TypedExpr::BooleanLiteral { value, .. } => PureExpr::BooleanLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::FloatLiteral { value, .. } => PureExpr::FloatLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::IntLiteral { value, .. } => PureExpr::IntLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::StringConcat { parts, .. } => PureExpr::StringConcat {
                parts: parts.iter().map(|part| self.compile_expr(part)).collect(),
                id: expr_id,
            },
            TypedExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Equals {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NotEquals {
                left,
                right,
                operand_types,
                ..
            } => {
                // Desugar NotEquals into BooleanNegation(Equals(...))
                let equals_id = self.next_expr_id();
                PureExpr::BooleanNegation {
                    operand: Box::new(PureExpr::Equals {
                        left: Box::new(self.compile_expr(left)),
                        right: Box::new(self.compile_expr(right)),
                        operand_types: operand_types.clone(),
                        id: equals_id,
                    }),
                    id: expr_id,
                }
            }
            TypedExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::LessThan {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            // Convert a > b to b < a
            TypedExpr::GreaterThan {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::LessThan {
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::LessThanOrEqual {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            // Convert a >= b to b <= a
            TypedExpr::GreaterThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::LessThanOrEqual {
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::BooleanLogicalAnd { left, right, .. } => PureExpr::BooleanLogicalAnd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::BooleanLogicalOr { left, right, .. } => PureExpr::BooleanLogicalOr {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::NumericAdd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::NumericSubtract {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::NumericMultiply {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                kind,
            } => PureExpr::EnumLiteral {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.compile_expr(field_expr))
                    })
                    .collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::Match { match_, kind } => {
                let compiled_match = match match_ {
                    Match::Enum { subject, arms } => {
                        let subject = Box::new(self.compile_expr(subject));
                        let arms = arms
                            .iter()
                            .map(|arm| {
                                self.push_scope();
                                let bindings = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, name)| (field.clone(), self.bind(name)))
                                    .collect();
                                let body = self.compile_expr(&arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings,
                                    body,
                                }
                            })
                            .collect();
                        Match::Enum { subject, arms }
                    }
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(self.compile_expr(subject)),
                        true_body: Box::new(self.compile_expr(true_body)),
                        false_body: Box::new(self.compile_expr(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let subject = Box::new(self.compile_expr(subject));
                        self.push_scope();
                        let binding = some_arm_binding.as_ref().map(|name| self.bind(name));
                        let some_body = self.compile_expr(some_arm_body);
                        self.pop_scope();
                        let none_body = self.compile_expr(none_arm_body);
                        Match::Option {
                            subject,
                            some_arm_binding: binding,
                            some_arm_body: Box::new(some_body),
                            none_arm_body: Box::new(none_body),
                        }
                    }
                };
                PureExpr::Match {
                    match_: compiled_match,
                    kind: kind.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::OptionLiteral { value, kind } => PureExpr::OptionLiteral {
                value: value.as_ref().map(|v| Box::new(self.compile_expr(v))),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::FragmentEmpty => PureExpr::FragmentConcat {
                parts: Vec::new(),
                id: expr_id,
            },
            TypedExpr::FunctionCall {
                function_name,
                args,
                kind,
            } => PureExpr::FunctionCall {
                function_name: function_name.clone().into(),
                args: args
                    .iter()
                    .map(|(name, value)| PureArgument {
                        name: name.clone(),
                        expr: self.compile_expr(value),
                    })
                    .collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::Let {
                var,
                value,
                body,
                kind,
            } => {
                let value = Box::new(self.compile_expr(value));
                self.push_scope();
                let ir_var = self.bind(var);
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                PureExpr::Let {
                    var: ir_var,
                    value,
                    body,
                    kind: kind.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::ArrayLength { array } => PureExpr::ArrayLength {
                array: Box::new(self.compile_expr(array)),
                id: expr_id,
            },
            TypedExpr::ArrayIsEmpty { array } => PureExpr::ArrayIsEmpty {
                array: Box::new(self.compile_expr(array)),
                id: expr_id,
            },
            TypedExpr::StringIsEmpty { string } => PureExpr::StringIsEmpty {
                string: Box::new(self.compile_expr(string)),
                id: expr_id,
            },
            TypedExpr::OptionIsSome { option } => PureExpr::OptionIsSome {
                option: Box::new(self.compile_expr(option)),
                id: expr_id,
            },
            TypedExpr::OptionIsNone { option } => PureExpr::OptionIsNone {
                option: Box::new(self.compile_expr(option)),
                id: expr_id,
            },
            TypedExpr::IntToString { value } => PureExpr::IntToString {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
            TypedExpr::FloatToInt { value } => PureExpr::FloatToInt {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
            TypedExpr::IntToFloat { value } => PureExpr::IntToFloat {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
        }
    }
}

/// Processes escape sequences in a string, converting raw escape sequences
/// like `\n` to their actual character values.
///
/// Supported escape sequences:
/// - `\n` → newline
/// - `\t` → tab
/// - `\r` → carriage return
/// - `\\` → backslash
/// - `\"` → double quote
fn process_escape_sequences(s: &str) -> Cow<'_, str> {
    // Without a backslash there is nothing to unescape, so the caller can keep
    // whatever allocation it already has instead of copying the text out.
    if !s.contains('\\') {
        return Cow::Borrowed(s);
    }

    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(other) => {
                    // Invalid escape sequence - keep as-is
                    // (tokenizer already reported the error)
                    result.push('\\');
                    result.push(other);
                }
                None => {
                    // Trailing backslash - keep as-is
                    result.push('\\');
                }
            }
        } else {
            result.push(ch);
        }
    }

    Cow::Owned(result)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::hop::typing::typed_ast_builder::{build_page, build_page_no_params};
    use expect_test::{Expect, expect};

    fn check(page: AssembledPageDeclaration, expected: Expect) {
        let before = page.to_string();
        let mut expr_ids = ExprIdCounter::new();
        let mut var_ids = VarIdCounter::new();
        let compiled_view = Compiler::new(&mut expr_ids, &mut var_ids, None, HashMap::new())
            .compile_page_decl(page);
        let after = compiled_view.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_compile_simple_text() {
        check(
            build_page_no_params("MainComp", |t| {
                t.text("Hello World");
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  Hello World
                }

                -- after --
                page MainComp() {
                  concat(raw("Hello World"))
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_text_expression() {
        check(
            build_page("MainComp", [("name", Type::String)], |t| {
                t.text("Hello ");
                t.text_expr(t.var_expr("name"));
            }),
            expect![[r#"
                -- before --
                page MainComp(name: String) {
                  Hello 
                  {name}
                }

                -- after --
                page MainComp(name@v0: String) {
                  concat(raw("Hello "), escape(v0))
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_html_element() {
        check(
            build_page_no_params("MainComp", |t| {
                t.div(vec![], |t| {
                    t.text("Content");
                });
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  <div>
                    Content
                  </div>
                }

                -- after --
                page MainComp() {
                  concat(
                    raw("<div"),
                    raw(">"),
                    raw("Content"),
                    raw("</div>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_if_node() {
        check(
            build_page("MainComp", [("show", Type::Bool)], |t| {
                t.if_node(t.var_expr("show"), |t| {
                    t.div(vec![], |t| {
                        t.text("Visible");
                    });
                });
            }),
            expect![[r#"
                -- before --
                page MainComp(show: Bool) {
                  <if {show}>
                    <div>
                      Visible
                    </div>
                  </if>
                }

                -- after --
                page MainComp(show@v0: Bool) {
                  concat(
                    match v0 {
                      true => {
                        concat(
                          raw("<div"),
                          raw(">"),
                          raw("Visible"),
                          raw("</div>"),
                        )
                      }
                      false => { concat() }
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_for_node() {
        check(
            build_page(
                "MainComp",
                vec![("items", Type::Array(Arc::new(Type::String)))],
                |t| {
                    t.ul(vec![], |t| {
                        t.for_node("item", t.var_expr("items"), |t| {
                            t.li(vec![], |t| {
                                t.text_expr(t.var_expr("item"));
                            });
                        });
                    });
                },
            ),
            expect![[r#"
                -- before --
                page MainComp(items: Array[String]) {
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                }

                -- after --
                page MainComp(items@v0: Array[String]) {
                  concat(
                    raw("<ul"),
                    raw(">"),
                    for v1 in v0 {
                      concat(raw("<li"), raw(">"), escape(v1), raw("</li>"))
                    },
                    raw("</ul>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_static_attributes() {
        check(
            build_page_no_params("MainComp", |t| {
                t.div(
                    vec![("class", t.attr_str("base")), ("id", t.attr_str("test"))],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  <div class="base" id="test">
                    Content
                  </div>
                }

                -- after --
                page MainComp() {
                  concat(
                    raw("<div"),
                    raw(" class=\"base\""),
                    raw(" id=\"test\""),
                    raw(">"),
                    raw("Content"),
                    raw("</div>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_dynamic_attributes() {
        check(
            build_page("MainComp", [("cls", Type::String)], |t| {
                t.div(
                    vec![
                        ("class", t.attr_str("base")),
                        ("data-value", t.attr_expr(t.var_expr("cls"))),
                    ],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                page MainComp(cls: String) {
                  <div class="base" data-value={cls}>
                    Content
                  </div>
                }

                -- after --
                page MainComp(cls@v0: String) {
                  concat(
                    raw("<div"),
                    raw(" class=\"base\""),
                    raw(" data-value=\""),
                    escape(v0),
                    raw("\""),
                    raw(">"),
                    raw("Content"),
                    raw("</div>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_development_mode_bootstrap() {
        check(
            build_page(
                "TestComp",
                vec![("name", Type::String), ("count", Type::String)],
                |t| {
                    t.div(vec![], |t| {
                        t.text("Hello ");
                        t.text_expr(t.var_expr("name"));
                        t.text(", count: ");
                        t.text_expr(t.var_expr("count"));
                    });
                },
            ),
            expect![[r#"
                -- before --
                page TestComp(name: String, count: String) {
                  <div>
                    Hello 
                    {name}
                    , count: 
                    {count}
                  </div>
                }

                -- after --
                page TestComp(name@v0: String, count@v1: String) {
                  concat(
                    raw("<div"),
                    raw(">"),
                    raw("Hello "),
                    escape(v0),
                    raw(", count: "),
                    escape(v1),
                    raw("</div>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_bool_match_node() {
        check(
            build_page("TestComp", vec![("flag", Type::Bool)], |t| {
                t.bool_match_node(
                    t.var_expr("flag"),
                    |t| {
                        t.text("yes");
                    },
                    |t| {
                        t.text("no");
                    },
                );
            }),
            expect![[r#"
                -- before --
                page TestComp(flag: Bool) {
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
                }

                -- after --
                page TestComp(flag@v0: Bool) {
                  concat(
                    match v0 {
                      true => { concat(raw("yes")) }
                      false => { concat(raw("no")) }
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_inline_script() {
        check(
            build_page_no_params("MainComp", |t| {
                t.html("script", vec![], |t| {
                    t.text("alert(\"hi\")");
                });
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  <script>
                    alert("hi")
                  </script>
                }

                -- after --
                page MainComp() {
                  concat(
                    raw("<script"),
                    raw(">"),
                    raw("alert(\"hi\")"),
                    raw("</script>"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_void_element() {
        check(
            build_page_no_params("MainComp", |t| {
                t.html("br", vec![], |_| {});
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  <br></br>
                }

                -- after --
                page MainComp() {
                  concat(raw("<br"), raw(">"))
                }
            "#]],
        );
    }
}
