use std::sync::Arc;

use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::CheapString;
use crate::hop::typing::{
    CaseVar, Decision, Type, TypedAttribute, TypedExpr, TypedFunctionDeclaration, TypedLoopSource,
    TypedPageDeclaration, TypedParameter, TypedPattern, TypedRecordUpdateField,
};
use crate::ir::binder_id::BinderId;
use crate::ir::binder_id::BinderIdCounter;
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::pure_module::PureForSource;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::{HashMap, HashSet};

use super::ir_parameter::{IrParameter, PageParameter};
use super::pure_module::{
    PureAttribute, PureExpr, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};

/// Compile the pages and the functions they reach.
///
/// A rest parameter is resolved at compile time. A call supplies attributes
/// to the rest of its callee, and the callee is compiled once for each
/// distinct list of attributes it is called with, with a parameter for each
/// of them in place of the rest. So a function with a rest is compiled once
/// per such list, a function without one at most once, and a function no
/// page reaches not at all.
pub fn typed_to_pure(
    pages: Vec<TypedPageDeclaration>,
    source_functions: &[(&RootContainedFilePath, &TypedFunctionDeclaration)],
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
) -> PureModule {
    let mut binder_ids = BinderIdCounter::new();
    let mut function_ids = FunctionIdCounter::new();

    // Each declaration with its position, since the module keeps the
    // functions in declaration order.
    let source: HashMap<(RootContainedFilePath, FunctionName), (usize, &TypedFunctionDeclaration)> =
        source_functions
            .iter()
            .enumerate()
            .map(|(index, (module, decl))| (((*module).clone(), decl.name.clone()), (index, *decl)))
            .collect();

    let mut compiler = Compiler::new(&mut binder_ids, &mut function_ids, asset_path_rewriter);

    let pages: Vec<CompiledPage> = pages
        .into_iter()
        .map(|page| compiler.compile_page_decl(page))
        .collect();

    // Compiling a body requests the specializations it calls, so this runs
    // until every request is compiled.
    let mut functions: Vec<(usize, PureFunctionDeclaration)> = Vec::new();
    let mut next = 0;
    while let Some(request) = compiler.specializations.get(next).cloned() {
        let (index, decl) = source[&(request.module, request.name)];
        let function = compiler.compile_function_decl(decl, request.shape, request.function);
        functions.push((index, function));
        next += 1;
    }
    functions.sort_by_key(|(index, _)| *index);
    let mut functions: Vec<PureFunctionDeclaration> =
        functions.into_iter().map(|(_, decl)| decl).collect();

    // The functions of the pages come last, numbered after every function
    // a page reaches.
    let pages = pages
        .into_iter()
        .map(|page| page.declare(&mut function_ids, &mut functions))
        .collect();

    PureModule {
        pages,
        functions,
        binder_ids,
    }
}

/// A function compiled for one list of attributes supplied to its rest.
#[derive(Clone)]
struct Specialization {
    module: RootContainedFilePath,
    name: FunctionName,
    /// The attributes supplied to the rest, with their types, in the order
    /// they render: those written at the call, then those the call forwards
    /// from the rest of the calling function.
    shape: Vec<(AttributeName, Type)>,
    function: IrFunction,
}

/// A page whose head and body are compiled but not yet declared as
/// functions: the parameters and the body of each.
struct CompiledPage {
    name: TypeName,
    parameters: Vec<PageParameter>,
    head: Option<(Vec<IrParameter>, PureExpr)>,
    body: (Vec<IrParameter>, PureExpr),
}

impl CompiledPage {
    /// Declare the head and the body as functions, appended to `functions`,
    /// and point the page at them.
    fn declare(
        self,
        function_ids: &mut FunctionIdCounter,
        functions: &mut Vec<PureFunctionDeclaration>,
    ) -> PurePageDeclaration {
        let mut declare = |function: IrFunction, (parameters, body)| {
            functions.push(PureFunctionDeclaration {
                function: function.clone(),
                parameters,
                return_type: Type::Html,
                body,
            });
            function
        };
        let head = self
            .head
            .map(|head| declare(IrFunction::page_head(function_ids.next()), head));
        let body = declare(IrFunction::page_body(function_ids.next()), self.body);
        PurePageDeclaration {
            name: self.name,
            parameters: self.parameters,
            head,
            body,
        }
    }
}

struct Compiler<'a> {
    binder_id_counter: &'a mut BinderIdCounter,
    function_id_counter: &'a mut FunctionIdCounter,
    /// The specializations calls have requested so far, in request order.
    specializations: Vec<Specialization>,
    scopes: Vec<Vec<(VarName, BinderId)>>,
    /// The parameters of the function being compiled, those it declares and
    /// those its rest adds from a function it is spread into. A forwarded
    /// parameter reads these, so a binding in the body that reuses the name
    /// does not capture it.
    params: HashMap<VarName, BinderId>,
    /// The attributes the specialization being compiled receives through its
    /// rest, each with the parameter that holds it, in the order they
    /// render. The spread reads these. They are not in scope by name.
    rest: Vec<(AttributeName, Type, BinderId)>,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
}

impl<'a> Compiler<'a> {
    fn new(
        binder_id_counter: &'a mut BinderIdCounter,
        function_id_counter: &'a mut FunctionIdCounter,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Self {
        Compiler {
            binder_id_counter,
            function_id_counter,
            specializations: Vec::new(),
            scopes: vec![Vec::new()],
            params: HashMap::new(),
            rest: Vec::new(),
            asset_path_rewriter,
        }
    }

    fn compile_function_decl(
        &mut self,
        decl: &TypedFunctionDeclaration,
        shape: Vec<(AttributeName, Type)>,
        function: IrFunction,
    ) -> PureFunctionDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len() + shape.len());
        for param in &decl.params {
            let var = self.bind(&param.var_name);
            self.params.insert(param.var_name.clone(), var);
            parameters.push(IrParameter {
                var,
                name: param.var_name.clone().into(),
                typ: param.var_type.clone(),
            });
        }
        // Each attribute the specialization receives through its rest is a
        // parameter of its own, after the declared ones and in the order of
        // the shape, as a call passes them.
        for (name, typ) in shape {
            let var = self.next_binder_id();
            self.rest.push((name.clone(), typ.clone(), var));
            parameters.push(IrParameter { var, name, typ });
        }

        let declaration = PureFunctionDeclaration {
            function,
            parameters,
            return_type: decl.return_type.clone(),
            body: self.compile_expr(&decl.body),
        };
        self.pop_scope();
        self.params.clear();
        self.rest.clear();
        declaration
    }

    fn compile_page_decl(&mut self, page: TypedPageDeclaration) -> CompiledPage {
        let parameters = page
            .params
            .iter()
            .map(|param| PageParameter {
                name: param.var_name.clone().into(),
                typ: param.var_type.clone(),
            })
            .collect();
        CompiledPage {
            name: page.name,
            parameters,
            head: page
                .head
                .as_ref()
                .map(|head| self.compile_page_member(&page.params, head)),
            body: self.compile_page_member(&page.params, &page.body),
        }
    }

    /// Compile the head or the body of a page as the body of a function
    /// that declares the page's parameters, each with a binder of its own.
    fn compile_page_member(
        &mut self,
        params: &[TypedParameter],
        expr: &TypedExpr,
    ) -> (Vec<IrParameter>, PureExpr) {
        self.push_scope();
        let parameters = params
            .iter()
            .map(|param| IrParameter {
                var: self.bind(&param.var_name),
                name: param.var_name.clone().into(),
                typ: param.var_type.clone(),
            })
            .collect();
        let body = self.compile_expr(expr);
        self.pop_scope();
        (parameters, body)
    }

    fn next_binder_id(&mut self) -> BinderId {
        self.binder_id_counter.next()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope stack should not be empty");
    }

    fn bind(&mut self, name: &VarName) -> BinderId {
        let id = self.next_binder_id();
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
            .push((name.clone(), id));
        id
    }

    fn resolve(&mut self, name: &VarName) -> BinderId {
        for scope in self.scopes.iter().rev() {
            if let Some((_, id)) = scope.iter().rev().find(|(n, _)| n == name) {
                return *id;
            }
        }
        panic!("undefined variable: {name}");
    }

    /// Compile the decision tree of a match. Only the case variables in
    /// `used` are bound, and `case_vars` maps every case variable bound so far
    /// to the IR variable that holds it.
    fn compile_decision(
        &mut self,
        decision: &Decision,
        arms: &[(TypedPattern, TypedExpr)],
        typ: &Type,
        used: &HashSet<CaseVar>,
        case_vars: &mut HashMap<CaseVar, BinderId>,
    ) -> PureExpr {
        match decision {
            Decision::Success(body) => {
                // The arm's pattern variables name the case variables they
                // read from, so the body refers to those directly.
                self.push_scope();
                for binding in &body.bindings {
                    self.scopes
                        .last_mut()
                        .expect("scope stack should not be empty")
                        .push((binding.name.clone(), case_vars[&binding.source]));
                }
                let result = self.compile_expr(&arms[body.value].1);
                self.pop_scope();
                result
            }
            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                });
                let true_body = self.compile_decision(&true_case.body, arms, typ, used, case_vars);
                let false_body =
                    self.compile_decision(&false_case.body, arms, typ, used, case_vars);
                PureExpr::Match {
                    match_: Match::Bool {
                        subject,
                        true_body: Box::new(true_body),
                        false_body: Box::new(false_body),
                    },
                    typ: typ.clone(),
                }
            }
            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                });
                let binding = if used.contains(&some_case.var.id) {
                    let var = self.next_binder_id();
                    case_vars.insert(some_case.var.id, var);
                    Some(IrBinder {
                        var,
                        typ: some_case.var.typ.clone(),
                    })
                } else {
                    None
                };
                let some_body = self.compile_decision(&some_case.body, arms, typ, used, case_vars);
                let none_body = self.compile_decision(&none_case.body, arms, typ, used, case_vars);
                PureExpr::Match {
                    match_: Match::Option {
                        subject,
                        some_arm_binding: binding,
                        some_arm_body: Box::new(some_body),
                        none_arm_body: Box::new(none_body),
                    },
                    typ: typ.clone(),
                }
            }
            Decision::SwitchEnum { variable, cases } => {
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                });
                let enum_arms = cases
                    .iter()
                    .map(|case| {
                        let bindings = case
                            .bindings
                            .iter()
                            .filter_map(|binding| {
                                if !used.contains(&binding.var.id) {
                                    return None;
                                }
                                let var = self.next_binder_id();
                                case_vars.insert(binding.var.id, var);
                                Some((
                                    binding.field_name.clone(),
                                    IrBinder {
                                        var,
                                        typ: binding.var.typ.clone(),
                                    },
                                ))
                            })
                            .collect();
                        EnumMatchArm {
                            pattern: EnumPattern::Variant {
                                type_name: case.type_name.clone(),
                                variant_name: case.variant_name.clone(),
                            },
                            bindings,
                            body: self.compile_decision(&case.body, arms, typ, used, case_vars),
                        }
                    })
                    .collect();
                PureExpr::Match {
                    match_: Match::Enum {
                        subject,
                        arms: enum_arms,
                    },
                    typ: typ.clone(),
                }
            }
            Decision::SwitchRecord { variable, case } => {
                // Bind each field a pattern reads to its case variable, the
                // first field outermost.
                let mut lets = Vec::new();
                for binding in &case.bindings {
                    if !used.contains(&binding.var.id) {
                        continue;
                    }
                    let value = PureExpr::FieldAccess {
                        record: Box::new(PureExpr::VariableReference {
                            value: case_vars[&variable.id],
                            typ: variable.typ.clone(),
                        }),
                        field: binding.field_name.clone(),
                        typ: binding.var.typ.clone(),
                    };
                    let var = self.next_binder_id();
                    case_vars.insert(binding.var.id, var);
                    let binder = IrBinder {
                        var,
                        typ: binding.var.typ.clone(),
                    };
                    lets.push((binder, value));
                }
                let mut result = self.compile_decision(&case.body, arms, typ, used, case_vars);
                for (var, value) in lets.into_iter().rev() {
                    result = PureExpr::Let {
                        var,
                        value: Box::new(value),
                        body: Box::new(result),
                        typ: typ.clone(),
                    };
                }
                result
            }
            Decision::SwitchTuple { variable, case } => {
                // Bind each element a pattern reads to its case variable, the
                // first element outermost.
                let mut lets = Vec::new();
                for (index, element) in case.elements.iter().enumerate() {
                    if !used.contains(&element.id) {
                        continue;
                    }
                    let value = PureExpr::TupleIndex {
                        tuple: Box::new(PureExpr::VariableReference {
                            value: case_vars[&variable.id],
                            typ: variable.typ.clone(),
                        }),
                        index,
                        typ: element.typ.clone(),
                    };
                    let var = self.next_binder_id();
                    case_vars.insert(element.id, var);
                    let binder = IrBinder {
                        var,
                        typ: element.typ.clone(),
                    };
                    lets.push((binder, value));
                }
                let mut result = self.compile_decision(&case.body, arms, typ, used, case_vars);
                for (var, value) in lets.into_iter().rev() {
                    result = PureExpr::Let {
                        var,
                        value: Box::new(value),
                        body: Box::new(result),
                        typ: typ.clone(),
                    };
                }
                result
            }
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> PureExpr {
        match expr {
            TypedExpr::Var { value, typ, .. } => PureExpr::VariableReference {
                value: self.resolve(value),
                typ: typ.clone(),
            },
            TypedExpr::ForwardedParam { value, typ } => PureExpr::VariableReference {
                value: self.params[value],
                typ: typ.clone(),
            },
            TypedExpr::FieldAccess {
                record: object,
                field,
                typ,
                ..
            } => PureExpr::FieldAccess {
                record: Box::new(self.compile_expr(object)),
                field: field.clone(),
                typ: typ.clone(),
            },
            TypedExpr::BoolNegation { operand, .. } => PureExpr::Unary {
                op: IrUnaryOp::BoolNegation,
                operand: Box::new(self.compile_expr(operand)),
            },
            TypedExpr::NumericNegation {
                operand,
                operand_type,
            } => PureExpr::Unary {
                op: IrUnaryOp::NumericNegation(operand_type.clone()),
                operand: Box::new(self.compile_expr(operand)),
            },
            TypedExpr::Array { elements, typ, .. } => PureExpr::Array {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                typ: typ.clone(),
            },
            TypedExpr::Tuple { elements, typ } => PureExpr::Tuple {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                typ: typ.clone(),
            },
            TypedExpr::Record {
                type_name,
                fields,
                typ,
                ..
            } => PureExpr::Record {
                type_name: type_name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.compile_expr(v)))
                    .collect(),
                typ: typ.clone(),
            },
            TypedExpr::RecordUpdate {
                type_name,
                base,
                fields,
                typ,
            } => {
                let value = Box::new(self.compile_expr(base));
                let base_var = self.next_binder_id();
                let literal = PureExpr::Record {
                    type_name: type_name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, field)| {
                            let value = match field {
                                TypedRecordUpdateField::Explicit(value) => self.compile_expr(value),
                                TypedRecordUpdateField::FromBase(field_typ) => {
                                    PureExpr::FieldAccess {
                                        record: Box::new(PureExpr::VariableReference {
                                            value: base_var,
                                            typ: typ.clone(),
                                        }),
                                        field: name.clone(),
                                        typ: field_typ.clone(),
                                    }
                                }
                            };
                            (name.clone(), value)
                        })
                        .collect(),
                    typ: typ.clone(),
                };
                PureExpr::Let {
                    var: IrBinder {
                        var: base_var,
                        typ: typ.clone(),
                    },
                    value,
                    body: Box::new(literal),
                    typ: typ.clone(),
                }
            }
            TypedExpr::StringLiteral { value, .. } => PureExpr::StringLiteral {
                value: value.clone(),
            },
            TypedExpr::Asset { path } => {
                let value = match &self.asset_path_rewriter {
                    Some(rewriter) => rewriter.rewrite(path),
                    None => format!("/{}", path.as_str()),
                };
                PureExpr::StringLiteral {
                    value: CheapString::new(value),
                }
            }
            TypedExpr::BoolLiteral { value, .. } => PureExpr::BoolLiteral { value: *value },
            TypedExpr::FloatLiteral { value, .. } => PureExpr::FloatLiteral { value: *value },
            TypedExpr::IntLiteral { value, .. } => PureExpr::IntLiteral { value: *value },
            TypedExpr::StringConcat { parts, .. } => PureExpr::StringConcat {
                parts: parts.iter().map(|part| self.compile_expr(part)).collect(),
            },
            TypedExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::Equals(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::NotEquals {
                left,
                right,
                operand_types,
                ..
            } => {
                // Desugar NotEquals into BoolNegation(Equals(...))
                PureExpr::Unary {
                    op: IrUnaryOp::BoolNegation,
                    operand: Box::new(PureExpr::Binary {
                        op: IrBinaryOp::Equals(operand_types.clone()),
                        left: Box::new(self.compile_expr(left)),
                        right: Box::new(self.compile_expr(right)),
                    }),
                }
            }
            TypedExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::LessThan(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            // Convert a > b to b < a
            TypedExpr::GreaterThan {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::LessThan(operand_types.clone()),
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
            },
            TypedExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::LessThanOrEqual(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            // Convert a >= b to b <= a
            TypedExpr::GreaterThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::LessThanOrEqual(operand_types.clone()),
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
            },
            TypedExpr::BoolLogicalAnd { left, right, .. } => PureExpr::BoolLogicalAnd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::BoolLogicalOr { left, right, .. } => PureExpr::BoolLogicalOr {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::NumericAdd(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::NumericSubtract(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => PureExpr::Binary {
                op: IrBinaryOp::NumericMultiply(operand_types.clone()),
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
            },
            TypedExpr::Enum {
                type_name,
                variant_name,
                fields,
                typ,
            } => PureExpr::Enum {
                type_name: type_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.compile_expr(field_expr))
                    })
                    .collect(),
                typ: typ.clone(),
            },
            TypedExpr::Match {
                subject,
                arms,
                decision,
                typ,
            } => {
                let mut used = HashSet::new();
                decision.collect_used(&mut used);
                let value = Box::new(self.compile_expr(subject));
                let var = self.next_binder_id();
                let mut case_vars = HashMap::from([(CaseVar(0), var)]);
                let body = self.compile_decision(decision, arms, typ, &used, &mut case_vars);
                PureExpr::Let {
                    var: IrBinder {
                        var,
                        typ: value.typ(),
                    },
                    value,
                    body: Box::new(body),
                    typ: typ.clone(),
                }
            }
            TypedExpr::Option { value, typ } => PureExpr::Option {
                value: value.as_ref().map(|v| Box::new(self.compile_expr(v))),
                typ: typ.clone(),
            },
            TypedExpr::HtmlConcat { parts } => {
                let mut compiled = Vec::with_capacity(parts.len());
                for part in parts {
                    assert_eq!(
                        part.typ(),
                        Type::Html,
                        "HtmlConcat must hold Html, but holds {part}"
                    );
                    compiled.push(self.compile_expr(part));
                }
                PureExpr::HtmlConcat { parts: compiled }
            }
            TypedExpr::HtmlText { value } => PureExpr::HtmlText {
                content: value.clone(),
            },
            TypedExpr::HtmlEscape { expr } => {
                assert_eq!(
                    expr.typ(),
                    Type::String,
                    "HtmlEscape must hold a String, but holds {expr}"
                );
                PureExpr::HtmlEscape {
                    expr: Box::new(self.compile_expr(expr)),
                }
            }
            TypedExpr::Element {
                element,
                attrs,
                children,
            } => {
                let mut attributes = Vec::with_capacity(attrs.attributes.len() + self.rest.len());
                for attr in &attrs.attributes {
                    attributes.push(match attr {
                        TypedAttribute::Value { name, value } => PureAttribute::Value {
                            name: name.clone(),
                            value: self.compile_expr(value),
                        },
                        TypedAttribute::Presence { name, present } => PureAttribute::Presence {
                            name: name.clone(),
                            present: self.compile_expr(present),
                        },
                    });
                }
                // The spread places the attributes the rest receives after
                // those written on the element. A Bool is a boolean
                // attribute, any other type a value.
                if attrs.spread.is_some() {
                    for (name, typ, var) in self.rest.clone() {
                        let value = PureExpr::VariableReference {
                            value: var,
                            typ: typ.clone(),
                        };
                        attributes.push(match typ {
                            Type::Bool => PureAttribute::Presence {
                                name,
                                present: value,
                            },
                            _ => PureAttribute::Value { name, value },
                        });
                    }
                }
                PureExpr::HtmlElement {
                    element: element.clone(),
                    attributes,
                    children: Box::new(self.compile_expr(children)),
                }
            }
            TypedExpr::Call {
                function_name,
                module,
                args,
                rest,
                typ,
            } => {
                // The typechecker orders the arguments as the callee
                // declares its parameters.
                let mut compiled_args: Vec<PureExpr> = args
                    .iter()
                    .map(|(_, value)| self.compile_expr(value))
                    .collect();
                // The attributes supplied to the callee's rest select the
                // specialization, and are passed to the parameters it has
                // for them, in the order of its shape: those written at the
                // call first, then those the spread forwards from the rest
                // of the calling function.
                let mut shape = Vec::new();
                if let Some((_, attrs)) = rest {
                    for attr in &attrs.attributes {
                        let (name, typ, value) = match attr {
                            TypedAttribute::Value { name, value } => {
                                (name, Type::String, self.compile_expr(value))
                            }
                            TypedAttribute::Presence { name, present } => {
                                (name, Type::Bool, self.compile_expr(present))
                            }
                        };
                        compiled_args.push(value);
                        shape.push((name.clone(), typ));
                    }
                    if attrs.spread.is_some() {
                        for (name, typ, var) in self.rest.clone() {
                            compiled_args.push(PureExpr::VariableReference {
                                value: var,
                                typ: typ.clone(),
                            });
                            shape.push((name, typ));
                        }
                    }
                }
                // An attribute renders as the caller spelled it, so shapes
                // compare by spelling, not as attribute names compare.
                let existing = self.specializations.iter().find(|s| {
                    s.module == *module
                        && s.name == *function_name
                        && s.shape.len() == shape.len()
                        && s.shape.iter().zip(&shape).all(|((a, a_typ), (b, b_typ))| {
                            a.as_str() == b.as_str() && a_typ == b_typ
                        })
                });
                let function = match existing {
                    Some(existing) => existing.function.clone(),
                    None => {
                        let function =
                            IrFunction::new(self.function_id_counter.next(), function_name.clone());
                        self.specializations.push(Specialization {
                            module: module.clone(),
                            name: function_name.clone(),
                            shape,
                            function: function.clone(),
                        });
                        function
                    }
                };
                PureExpr::Call {
                    function,
                    args: compiled_args,
                    typ: typ.clone(),
                }
            }
            TypedExpr::Let {
                var,
                value,
                body,
                typ,
            } => {
                let value = Box::new(self.compile_expr(value));
                self.push_scope();
                let ir_var = IrBinder {
                    var: self.bind(var),
                    typ: value.typ(),
                };
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                PureExpr::Let {
                    var: ir_var,
                    value,
                    body,
                    typ: typ.clone(),
                }
            }
            TypedExpr::For {
                var_name,
                source,
                body,
                typ,
            } => {
                assert_eq!(
                    *typ,
                    Type::Html,
                    "For must fold into Html, but folds into {typ}"
                );
                let pure_source = match &**source {
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
                let element_type = match &pure_source {
                    PureForSource::Array(array) => {
                        let Type::Array(element_type) = array.typ() else {
                            unreachable!("a loop over an array has an Array source");
                        };
                        *element_type
                    }
                    PureForSource::RangeInclusive { .. } => Type::Int,
                };
                self.push_scope();
                let var = var_name.as_ref().map(|name| IrBinder {
                    var: self.bind(name),
                    typ: element_type,
                });
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                PureExpr::HtmlFor {
                    var,
                    source: Box::new(pure_source),
                    body,
                }
            }
            TypedExpr::ArrayLength { array } => PureExpr::Unary {
                op: IrUnaryOp::ArrayLength,
                operand: Box::new(self.compile_expr(array)),
            },
            TypedExpr::ArrayIsEmpty { array } => PureExpr::Unary {
                op: IrUnaryOp::ArrayIsEmpty,
                operand: Box::new(self.compile_expr(array)),
            },
            TypedExpr::StringIsEmpty { string } => PureExpr::Unary {
                op: IrUnaryOp::StringIsEmpty,
                operand: Box::new(self.compile_expr(string)),
            },
            TypedExpr::OptionIsSome { option } => PureExpr::Unary {
                op: IrUnaryOp::OptionIsSome,
                operand: Box::new(self.compile_expr(option)),
            },
            TypedExpr::OptionIsNone { option } => PureExpr::Unary {
                op: IrUnaryOp::OptionIsNone,
                operand: Box::new(self.compile_expr(option)),
            },
            TypedExpr::OptionUnwrapOr {
                option,
                default,
                typ,
            } => {
                let subject = Box::new(self.compile_expr(option));
                let binding = self.next_binder_id();
                PureExpr::Match {
                    match_: Match::Option {
                        subject,
                        some_arm_binding: Some(IrBinder {
                            var: binding,
                            typ: typ.clone(),
                        }),
                        some_arm_body: Box::new(PureExpr::VariableReference {
                            value: binding,
                            typ: typ.clone(),
                        }),
                        none_arm_body: Box::new(self.compile_expr(default)),
                    },
                    typ: typ.clone(),
                }
            }
            TypedExpr::IntToString { value } => PureExpr::Unary {
                op: IrUnaryOp::IntToString,
                operand: Box::new(self.compile_expr(value)),
            },
            TypedExpr::FloatToInt { value } => PureExpr::Unary {
                op: IrUnaryOp::FloatToInt,
                operand: Box::new(self.compile_expr(value)),
            },
            TypedExpr::IntToFloat { value } => PureExpr::Unary {
                op: IrUnaryOp::IntToFloat,
                operand: Box::new(self.compile_expr(value)),
            },
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::document::Document;
    use crate::hop::typing::{
        TypeRegistryBuilder, TypedAttrs, build_page, build_page_no_params, build_page_with_types,
    };
    use crate::html::HtmlElementKind;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(page: TypedPageDeclaration, expected: Expect) {
        let before = page.to_string();
        let mut binder_ids = BinderIdCounter::new();
        let mut function_ids = FunctionIdCounter::new();
        let compiled_page =
            Compiler::new(&mut binder_ids, &mut function_ids, None).compile_page_decl(page);
        let mut functions = Vec::new();
        let page = compiled_page.declare(&mut function_ids, &mut functions);
        let after = PureModule {
            pages: vec![page],
            functions,
            binder_ids,
        }
        .to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_compile_the_head_and_the_body() {
        let mut page = build_page_no_params("MainComp", |t| {
            t.text("Hello World");
        });
        page.head = Some(TypedExpr::HtmlConcat {
            parts: vec![TypedExpr::Element {
                element: HtmlElementKind::Title,
                attrs: TypedAttrs {
                    attributes: vec![],
                    spread: None,
                },
                children: Box::new(TypedExpr::HtmlConcat {
                    parts: vec![TypedExpr::HtmlText {
                        value: CheapString::new("Hi".to_string()),
                    }],
                }),
            }],
        });
        check(
            page,
            expect![[r#"
                -- before --
                page MainComp() {
                  fn head() -> Html {
                    concat(
                      html(
                        tag: "title",
                        attrs: [],
                        children: concat(text("Hi")),
                      ),
                    )
                  }
                  fn body() -> Html {
                    concat(text("Hello World"))
                  }
                }

                -- after --
                fn head@f0() -> Html {
                  concat(html("title", {}, concat(text("Hi"))))
                }
                fn body@f1() -> Html {
                  concat(text("Hello World"))
                }
                page MainComp() {
                  head@f0()
                  body@f1()
                }
            "#]],
        );
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
                  fn body() -> Html {
                    concat(text("Hello World"))
                  }
                }

                -- after --
                fn body@f0() -> Html {
                  concat(text("Hello World"))
                }
                page MainComp() {
                  body@f0()
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
                  fn body() -> Html {
                    concat(text("Hello "), escape(name))
                  }
                }

                -- after --
                fn body@f0(name@b0: String) -> Html {
                  concat(text("Hello "), escape(b0))
                }
                page MainComp(name: String) {
                  body@f0(name)
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
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "div",
                        attrs: [],
                        children: concat(text("Content")),
                      ),
                    )
                  }
                }

                -- after --
                fn body@f0() -> Html {
                  concat(html("div", {}, concat(text("Content"))))
                }
                page MainComp() {
                  body@f0()
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_if_html() {
        check(
            build_page("MainComp", [("show", Type::Bool)], |t| {
                t.if_html(t.var_expr("show"), |t| {
                    t.div(vec![], |t| {
                        t.text("Visible");
                    });
                });
            }),
            expect![[r#"
                -- before --
                page MainComp(show: Bool) {
                  fn body() -> Html {
                    concat(
                      match show {
                        true => concat(
                          html(
                            tag: "div",
                            attrs: [],
                            children: concat(text("Visible")),
                          ),
                        ),
                        false => concat(),
                      },
                    )
                  }
                }

                -- after --
                fn body@f0(show@b0: Bool) -> Html {
                  concat(
                    let b1: Bool = b0 in {
                      match b1 {
                        true => {
                          concat(html("div", {}, concat(text("Visible"))))
                        }
                        false => {
                          concat()
                        }
                      }
                    },
                  )
                }
                page MainComp(show: Bool) {
                  body@f0(show)
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_for_html() {
        check(
            build_page(
                "MainComp",
                vec![("items", Type::Array(Box::new(Type::String)))],
                |t| {
                    t.ul(vec![], |t| {
                        t.for_html("item", t.var_expr("items"), |t| {
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

                -- after --
                fn body@f0(items@b0: Array[String]) -> Html {
                  concat(
                    html(
                      "ul",
                      {},
                      concat(
                        for b1: String in b0 {
                          concat(html("li", {}, concat(escape(b1))))
                        },
                      ),
                    ),
                  )
                }
                page MainComp(items: Array[String]) {
                  body@f0(items)
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_static_attributes() {
        check(
            build_page_no_params("MainComp", |t| {
                t.div(
                    vec![
                        ("class", t.string_literal("base")),
                        ("id", t.string_literal("test")),
                    ],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                page MainComp() {
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "div",
                        attrs: [class: escape("base"), id: escape("test")],
                        children: concat(text("Content")),
                      ),
                    )
                  }
                }

                -- after --
                fn body@f0() -> Html {
                  concat(
                    html(
                      "div",
                      {class: "base", id: "test"},
                      concat(text("Content")),
                    ),
                  )
                }
                page MainComp() {
                  body@f0()
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
                        ("class", t.string_literal("base")),
                        ("data-value", t.var_expr("cls")),
                    ],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                page MainComp(cls: String) {
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "div",
                        attrs: [
                          class: escape("base"),
                          data-value: escape(cls),
                        ],
                        children: concat(text("Content")),
                      ),
                    )
                  }
                }

                -- after --
                fn body@f0(cls@b0: String) -> Html {
                  concat(
                    html(
                      "div",
                      {class: "base", data-value: b0},
                      concat(text("Content")),
                    ),
                  )
                }
                page MainComp(cls: String) {
                  body@f0(cls)
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
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "div",
                        attrs: [],
                        children: concat(
                          text("Hello "),
                          escape(name),
                          text(", count: "),
                          escape(count),
                        ),
                      ),
                    )
                  }
                }

                -- after --
                fn body@f0(name@b0: String, count@b1: String) -> Html {
                  concat(
                    html(
                      "div",
                      {},
                      concat(
                        text("Hello "),
                        escape(b0),
                        text(", count: "),
                        escape(b1),
                      ),
                    ),
                  )
                }
                page TestComp(name: String, count: String) {
                  body@f0(name, count)
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_bool_match_html() {
        check(
            build_page("TestComp", vec![("flag", Type::Bool)], |t| {
                t.bool_match_html(
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
                  fn body() -> Html {
                    concat(
                      match flag {
                        true => concat(text("yes")),
                        false => concat(text("no")),
                      },
                    )
                  }
                }

                -- after --
                fn body@f0(flag@b0: Bool) -> Html {
                  concat(
                    let b1: Bool = b0 in {
                      match b1 {
                        true => {
                          concat(text("yes"))
                        }
                        false => {
                          concat(text("no"))
                        }
                      }
                    },
                  )
                }
                page TestComp(flag: Bool) {
                  body@f0(flag)
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
                  fn body() -> Html {
                    concat(
                      html(
                        tag: "script",
                        attrs: [],
                        children: concat(text("alert(\"hi\")")),
                      ),
                    )
                  }
                }

                -- after --
                fn body@f0() -> Html {
                  concat(html("script", {}, concat(text("alert(\"hi\")"))))
                }
                page MainComp() {
                  body@f0()
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
                  fn body() -> Html {
                    concat(html(tag: "br", attrs: []))
                  }
                }

                -- after --
                fn body@f0() -> Html {
                  concat(html("br", {}))
                }
                page MainComp() {
                  body@f0()
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_record_update_of_variable() {
        check(
            build_page_with_types(
                TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
                "MainComp",
                [("user", "User")],
                |t| {
                    let updated = t.record_update(
                        t.var_expr("user"),
                        vec![("name", t.string_literal("Jane"))],
                    );
                    t.text_expr(t.field_access(updated, "name"));
                },
            ),
            expect![[r#"
                -- before --
                page MainComp(user: User) {
                  fn body() -> Html {
                    concat(escape(User {...user, name: "Jane"}.name))
                  }
                }

                -- after --
                fn body@f0(user@b0: User) -> Html {
                  concat(
                    escape(let b1: User = b0 in {
                      User {name: "Jane", age: b1.age}
                    }.name),
                  )
                }
                page MainComp(user: User) {
                  body@f0(user)
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_record_update_of_expression() {
        check(
            build_page_with_types(
                TypeRegistryBuilder::new()
                    .record("State", [("query", "String"), ("num", "Int")])
                    .record("App", [("state", "State")]),
                "MainComp",
                [("app", "App")],
                |t| {
                    let next = t.record_update(
                        t.field_access(t.var_expr("app"), "state"),
                        vec![("num", t.int_literal(1))],
                    );
                    t.text_expr(t.field_access(next, "query"));
                },
            ),
            expect![[r#"
                -- before --
                page MainComp(app: App) {
                  fn body() -> Html {
                    concat(escape(State {...app.state, num: 1}.query))
                  }
                }

                -- after --
                fn body@f0(app@b0: App) -> Html {
                  concat(
                    escape(let b1: State = b0.state in {
                      State {query: b1.query, num: 1}
                    }.query),
                  )
                }
                page MainComp(app: App) {
                  body@f0(app)
                }
            "#]],
        );
    }

    /// Compile a module written in source, without optimization.
    fn check_source(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let module = orchestrate_pure(
            program.typed_modules(),
            OrchestrateOptions {
                ..Default::default()
            },
        );
        expected.assert_eq(&module.to_string());
    }

    #[test]
    fn should_specialize_a_function_for_each_attribute_list_it_is_called_with() {
        check_source(
            indoc! {r#"
                fn Button(...rest) -> Html {
                  <button ...rest>
                    Go
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Button id="a"/>
                      <Button class="b" disabled={true}/>
                      <Button id="c"/>
                    </>
                  }
                }
            "#},
            expect![[r#"
                fn Button@f0(id@b0: String) -> Html {
                  html("button", {id: b0}, concat(text("Go")))
                }
                fn Button@f1(class@b1: String, disabled@b2: Bool) -> Html {
                  html(
                    "button",
                    {class: b1, disabled: b2},
                    concat(text("Go")),
                  )
                }
                fn body@f2() -> Html {
                  concat(
                    call Button@f0("a"),
                    call Button@f1("b", true),
                    call Button@f0("c"),
                  )
                }
                page Test() {
                  body@f2()
                }
            "#]],
        );
    }

    #[test]
    fn should_forward_a_rest_through_a_function_into_an_element() {
        check_source(
            indoc! {r#"
                fn Card(
                  title: String,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {title}
                  </div>
                }

                fn Panel(...rest) -> Html {
                  <Card id="panel" ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Panel title="Hi" class="wide"/>
                  }
                }
            "#},
            expect![[r#"
                fn Card@f1(
                  title@b2: String,
                  id@b3: String,
                  class@b4: String,
                ) -> Html {
                  html("div", {id: b3, class: b4}, concat(escape(b2)))
                }
                fn Panel@f0(title@b0: String, class@b1: String) -> Html {
                  call Card@f1(b0, "panel", b1)
                }
                fn body@f2() -> Html {
                  call Panel@f0("Hi", "wide")
                }
                page Test() {
                  body@f2()
                }
            "#]],
        );
    }

    #[test]
    fn should_specialize_a_recursive_function_with_a_rest() {
        check_source(
            indoc! {r#"
                fn Nest(
                  depth: Int,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {match depth > 0 {
                      true => <Nest depth={depth - 1} class="inner"/>,
                      false => <></>,
                    }}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Nest depth={2} id="outer"/>
                  }
                }
            "#},
            expect![[r#"
                fn Nest@f0(depth@b0: Int, id@b1: String) -> Html {
                  html(
                    "div",
                    {id: b1},
                    concat(
                      let b2: Bool = (0 < b0) in {
                        match b2 {
                          true => {
                            call Nest@f1((b0 - 1), "inner")
                          }
                          false => {
                            concat()
                          }
                        }
                      },
                    ),
                  )
                }
                fn Nest@f1(depth@b3: Int, class@b4: String) -> Html {
                  html(
                    "div",
                    {class: b4},
                    concat(
                      let b5: Bool = (0 < b3) in {
                        match b5 {
                          true => {
                            call Nest@f1((b3 - 1), "inner")
                          }
                          false => {
                            concat()
                          }
                        }
                      },
                    ),
                  )
                }
                fn body@f2() -> Html {
                  call Nest@f0(2, "outer")
                }
                page Test() {
                  body@f2()
                }
            "#]],
        );
    }

    #[test]
    fn should_name_a_parameter_after_the_attribute_it_receives() {
        check_source(
            indoc! {r#"
                fn Button(...rest) -> Html {
                  <button ...rest>
                    Go
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button data-x="1" aria-label="go"/>
                  }
                }
            "#},
            expect![[r#"
                fn Button@f0(
                  data-x@b0: String,
                  aria-label@b1: String,
                ) -> Html {
                  html(
                    "button",
                    {data-x: b0, aria-label: b1},
                    concat(text("Go")),
                  )
                }
                fn body@f1() -> Html {
                  call Button@f0("1", "go")
                }
                page Test() {
                  body@f1()
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_only_the_functions_the_pages_reach() {
        check_source(
            indoc! {r#"
                fn Used() -> Html {
                  <p>used</p>
                }

                fn Unused() -> Html {
                  <p>unused</p>
                }

                fn Uncalled(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Used/>
                  }
                }
            "#},
            expect![[r#"
                fn Used@f0() -> Html {
                  html("p", {}, concat(text("used")))
                }
                fn body@f1() -> Html {
                  call Used@f0()
                }
                page Test() {
                  body@f1()
                }
            "#]],
        );
    }
}
