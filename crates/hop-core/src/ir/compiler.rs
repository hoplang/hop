use std::sync::Arc;

use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::CheapString;
use crate::hop::typing::{
    CaseVar, Decision, Type, TypedAttribute, TypedExpr, TypedFunctionDeclaration, TypedLoopSource,
    TypedPageDeclaration, TypedPattern, TypedRecordUpdateField,
};
use crate::ir::expr_id::{ExprId, ExprIdCounter};
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_var::IrVar;
use crate::ir::pure_module::PureForSource;
use crate::ir::var_id::VarId;
use crate::ir::var_id::VarIdCounter;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;
use std::collections::{HashMap, HashSet};

use super::pure_module::{
    PureArgument, PureExpr, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use super::writer_module::WriterParameter;

/// Compile the pages and the functions they reach.
///
/// A rest parameter is resolved at compile time. A call supplies attributes
/// to the rest of its callee, and the callee is compiled once for each
/// distinct list of attributes it is called with, with a parameter for each
/// of them in place of the rest. So a function with a rest is compiled once
/// per such list, a function without one at most once, and a function no
/// page reaches not at all.
pub fn compile(
    pages: Vec<TypedPageDeclaration>,
    source_functions: &[(&RootContainedFilePath, &TypedFunctionDeclaration)],
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
) -> PureModule {
    let mut expr_ids = ExprIdCounter::new();
    let mut var_ids = VarIdCounter::new();
    let mut function_ids = FunctionIdCounter::new();

    // Each declaration with its position, since the module keeps the
    // functions in declaration order.
    let source: HashMap<(RootContainedFilePath, FunctionName), (usize, &TypedFunctionDeclaration)> =
        source_functions
            .iter()
            .enumerate()
            .map(|(index, (module, decl))| (((*module).clone(), decl.name.clone()), (index, *decl)))
            .collect();

    let mut compiler = Compiler::new(
        &mut expr_ids,
        &mut var_ids,
        &mut function_ids,
        asset_path_rewriter,
    );

    let pages = pages
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
    let functions = functions.into_iter().map(|(_, decl)| decl).collect();

    PureModule {
        pages,
        functions,
        expr_ids,
        var_ids,
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

struct Compiler<'a> {
    expr_id_counter: &'a mut ExprIdCounter,
    var_id_counter: &'a mut VarIdCounter,
    function_id_counter: &'a mut FunctionIdCounter,
    /// The specializations calls have requested so far, in request order.
    specializations: Vec<Specialization>,
    scopes: Vec<Vec<(VarName, VarId)>>,
    /// The parameters of the function being compiled, those it declares and
    /// those its rest adds from a function it is spread into. A forwarded
    /// parameter reads these, so a binding in the body that reuses the name
    /// does not capture it.
    params: HashMap<VarName, IrVar>,
    /// The attributes the specialization being compiled receives through its
    /// rest, each with the parameter that holds it, in the order they
    /// render. The spread reads these. They are not in scope by name.
    rest: Vec<(AttributeName, Type, IrVar)>,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
}

impl<'a> Compiler<'a> {
    fn new(
        expr_id_counter: &'a mut ExprIdCounter,
        var_id_counter: &'a mut VarIdCounter,
        function_id_counter: &'a mut FunctionIdCounter,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Self {
        Compiler {
            expr_id_counter,
            var_id_counter,
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
            parameters.push(WriterParameter {
                var,
                name: param.var_name.clone().into(),
                typ: param.var_type.clone(),
            });
        }
        // Each attribute the specialization receives through its rest is a
        // parameter of its own.
        for (name, typ) in shape {
            let var = IrVar::new(self.next_var_id());
            self.rest.push((name.clone(), typ.clone(), var));
            parameters.push(WriterParameter { var, name, typ });
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

    fn compile_page_decl(&mut self, page: TypedPageDeclaration) -> PurePageDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(page.params.len());
        for param in page.params {
            parameters.push(WriterParameter {
                var: self.bind(&param.var_name),
                name: param.var_name.into(),
                typ: param.var_type,
            });
        }

        let declaration = PurePageDeclaration {
            name: page.name,
            parameters,
            head: self.compile_expr(&page.head),
            body: self.compile_expr(&page.body),
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

    /// Compile the decision tree of a match. Only the case variables in
    /// `used` are bound, and `case_vars` maps every case variable bound so far
    /// to the IR variable that holds it.
    fn compile_decision(
        &mut self,
        decision: &Decision,
        arms: &[(TypedPattern, TypedExpr)],
        typ: &Type,
        used: &HashSet<CaseVar>,
        case_vars: &mut HashMap<CaseVar, IrVar>,
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
                        .push((binding.name.clone(), case_vars[&binding.source].id));
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
                let id = self.next_expr_id();
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                    id: self.next_expr_id(),
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
                    id,
                }
            }
            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let id = self.next_expr_id();
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                    id: self.next_expr_id(),
                });
                let binding = if used.contains(&some_case.var.id) {
                    let var = IrVar::new(self.next_var_id());
                    case_vars.insert(some_case.var.id, var);
                    Some(var)
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
                    id,
                }
            }
            Decision::SwitchEnum { variable, cases } => {
                let id = self.next_expr_id();
                let subject = Box::new(PureExpr::VariableReference {
                    value: case_vars[&variable.id],
                    typ: variable.typ.clone(),
                    id: self.next_expr_id(),
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
                                let var = IrVar::new(self.next_var_id());
                                case_vars.insert(binding.var.id, var);
                                Some((binding.field_name.clone(), var))
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
                    id,
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
                    let id = self.next_expr_id();
                    let value = PureExpr::FieldAccess {
                        id: self.next_expr_id(),
                        record: Box::new(PureExpr::VariableReference {
                            value: case_vars[&variable.id],
                            typ: variable.typ.clone(),
                            id: self.next_expr_id(),
                        }),
                        field: binding.field_name.clone(),
                        typ: binding.var.typ.clone(),
                    };
                    let var = IrVar::new(self.next_var_id());
                    case_vars.insert(binding.var.id, var);
                    lets.push((id, var, value));
                }
                let mut result = self.compile_decision(&case.body, arms, typ, used, case_vars);
                for (id, var, value) in lets.into_iter().rev() {
                    result = PureExpr::Let {
                        var,
                        value: Box::new(value),
                        body: Box::new(result),
                        typ: typ.clone(),
                        id,
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
                    let id = self.next_expr_id();
                    let value = PureExpr::TupleIndex {
                        id: self.next_expr_id(),
                        tuple: Box::new(PureExpr::VariableReference {
                            value: case_vars[&variable.id],
                            typ: variable.typ.clone(),
                            id: self.next_expr_id(),
                        }),
                        index,
                        typ: element.typ.clone(),
                    };
                    let var = IrVar::new(self.next_var_id());
                    case_vars.insert(element.id, var);
                    lets.push((id, var, value));
                }
                let mut result = self.compile_decision(&case.body, arms, typ, used, case_vars);
                for (id, var, value) in lets.into_iter().rev() {
                    result = PureExpr::Let {
                        var,
                        value: Box::new(value),
                        body: Box::new(result),
                        typ: typ.clone(),
                        id,
                    };
                }
                result
            }
        }
    }

    /// Append the parts that render one attribute of an element. A Bool
    /// value is a boolean attribute, present without a value when it is true
    /// and absent when it is false. Any other value is a String, escaped
    /// between quotes.
    fn compile_attribute(
        &mut self,
        name: &AttributeName,
        typ: &Type,
        value: PureExpr,
        output: &mut Vec<PureExpr>,
    ) {
        match typ {
            Type::Bool => {
                let true_body = Box::new(PureExpr::HtmlRaw {
                    content: format!(" {}", name.as_str()),
                    id: self.next_expr_id(),
                });
                let false_body = Box::new(PureExpr::HtmlConcat {
                    parts: Vec::new(),
                    id: self.next_expr_id(),
                });
                output.push(PureExpr::Match {
                    match_: Match::Bool {
                        subject: Box::new(value),
                        true_body,
                        false_body,
                    },
                    typ: Type::Html,
                    id: self.next_expr_id(),
                });
            }
            _ => {
                output.push(PureExpr::HtmlRaw {
                    content: format!(" {}=\"", name.as_str()),
                    id: self.next_expr_id(),
                });
                output.push(PureExpr::HtmlEscape {
                    expr: Box::new(value),
                    id: self.next_expr_id(),
                });
                output.push(PureExpr::HtmlRaw {
                    content: "\"".to_string(),
                    id: self.next_expr_id(),
                });
            }
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> PureExpr {
        let expr_id = self.next_expr_id();

        match expr {
            TypedExpr::Var { value, typ, .. } => PureExpr::VariableReference {
                value: self.resolve(value),
                typ: typ.clone(),
                id: expr_id,
            },
            TypedExpr::ForwardedParam { value, typ } => PureExpr::VariableReference {
                value: self.params[value],
                typ: typ.clone(),
                id: expr_id,
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
                id: expr_id,
            },
            TypedExpr::BoolNegation { operand, .. } => PureExpr::BoolNegation {
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
            TypedExpr::Array { elements, typ, .. } => PureExpr::Array {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                typ: typ.clone(),
                id: expr_id,
            },
            TypedExpr::Tuple { elements, typ } => PureExpr::Tuple {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                typ: typ.clone(),
                id: expr_id,
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
                id: expr_id,
            },
            TypedExpr::RecordUpdate {
                type_name,
                base,
                fields,
                typ,
            } => {
                let value = Box::new(self.compile_expr(base));
                let base_var = IrVar::new(self.next_var_id());
                let literal_id = self.next_expr_id();
                let literal = PureExpr::Record {
                    type_name: type_name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, field)| {
                            let value = match field {
                                TypedRecordUpdateField::Explicit(value) => self.compile_expr(value),
                                TypedRecordUpdateField::FromBase(field_typ) => {
                                    PureExpr::FieldAccess {
                                        id: self.next_expr_id(),
                                        record: Box::new(PureExpr::VariableReference {
                                            value: base_var,
                                            typ: typ.clone(),
                                            id: self.next_expr_id(),
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
                    id: literal_id,
                };
                PureExpr::Let {
                    var: base_var,
                    value,
                    body: Box::new(literal),
                    typ: typ.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::StringLiteral { value, .. } => PureExpr::StringLiteral {
                value: value.clone(),
                id: expr_id,
            },
            TypedExpr::Asset { path } => {
                let value = match &self.asset_path_rewriter {
                    Some(rewriter) => rewriter.rewrite(path),
                    None => format!("/{}", path.as_str()),
                };
                PureExpr::StringLiteral {
                    value: CheapString::new(value),
                    id: expr_id,
                }
            }
            TypedExpr::BoolLiteral { value, .. } => PureExpr::BoolLiteral {
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
                // Desugar NotEquals into BoolNegation(Equals(...))
                let equals_id = self.next_expr_id();
                PureExpr::BoolNegation {
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
            TypedExpr::BoolLogicalAnd { left, right, .. } => PureExpr::BoolLogicalAnd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::BoolLogicalOr { left, right, .. } => PureExpr::BoolLogicalOr {
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
                id: expr_id,
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
                let var = IrVar::new(self.next_var_id());
                let mut case_vars = HashMap::from([(CaseVar(0), var)]);
                let body = self.compile_decision(decision, arms, typ, &used, &mut case_vars);
                PureExpr::Let {
                    var,
                    value,
                    body: Box::new(body),
                    typ: typ.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::Option { value, typ } => PureExpr::Option {
                value: value.as_ref().map(|v| Box::new(self.compile_expr(v))),
                typ: typ.clone(),
                id: expr_id,
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
                PureExpr::HtmlConcat {
                    parts: compiled,
                    id: expr_id,
                }
            }
            TypedExpr::HtmlText { value } => PureExpr::HtmlRaw {
                content: value.to_string(),
                id: expr_id,
            },
            TypedExpr::HtmlEscape { expr } => {
                assert_eq!(
                    expr.typ(),
                    Type::String,
                    "HtmlEscape must hold a String, but holds {expr}"
                );
                PureExpr::HtmlEscape {
                    expr: Box::new(self.compile_expr(expr)),
                    id: expr_id,
                }
            }
            TypedExpr::Element {
                element,
                attrs,
                children,
            } => {
                let mut parts = vec![PureExpr::HtmlRaw {
                    content: format!("<{}", element.as_str()),
                    id: self.next_expr_id(),
                }];
                for attr in &attrs.attributes {
                    let (name, typ, value) = match attr {
                        TypedAttribute::Value { name, value } => {
                            (name, Type::String, self.compile_expr(value))
                        }
                        TypedAttribute::Presence { name, present } => {
                            (name, Type::Bool, self.compile_expr(present))
                        }
                    };
                    self.compile_attribute(name, &typ, value, &mut parts);
                }
                // The spread places the attributes the rest receives after
                // those written on the element.
                if attrs.spread.is_some() {
                    for (name, typ, var) in self.rest.clone() {
                        let value = PureExpr::VariableReference {
                            value: var,
                            typ: typ.clone(),
                            id: self.next_expr_id(),
                        };
                        self.compile_attribute(&name, &typ, value, &mut parts);
                    }
                }
                parts.push(PureExpr::HtmlRaw {
                    content: ">".to_string(),
                    id: self.next_expr_id(),
                });
                if !element.is_void() {
                    parts.push(self.compile_expr(children));
                    parts.push(PureExpr::HtmlRaw {
                        content: format!("</{}>", element.as_str()),
                        id: self.next_expr_id(),
                    });
                }
                PureExpr::HtmlConcat { parts, id: expr_id }
            }
            TypedExpr::Call {
                function_name,
                module,
                args,
                rest,
                typ,
            } => {
                let mut compiled_args: Vec<PureArgument> = args
                    .iter()
                    .map(|(name, value)| PureArgument {
                        name: name.clone().into(),
                        expr: self.compile_expr(value),
                    })
                    .collect();
                // The attributes supplied to the callee's rest select the
                // specialization, and are passed to the parameters it has
                // for them: those written at the call first, then those the
                // spread forwards from the rest of the calling function.
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
                        compiled_args.push(PureArgument {
                            name: name.clone(),
                            expr: value,
                        });
                        shape.push((name.clone(), typ));
                    }
                    if attrs.spread.is_some() {
                        for (name, typ, var) in self.rest.clone() {
                            compiled_args.push(PureArgument {
                                name: name.clone(),
                                expr: PureExpr::VariableReference {
                                    value: var,
                                    typ: typ.clone(),
                                    id: self.next_expr_id(),
                                },
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
                    id: expr_id,
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
                let ir_var = self.bind(var);
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                PureExpr::Let {
                    var: ir_var,
                    value,
                    body,
                    typ: typ.clone(),
                    id: expr_id,
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
                self.push_scope();
                let var = var_name.as_ref().map(|name| self.bind(name));
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                PureExpr::HtmlFor {
                    var,
                    source: Box::new(pure_source),
                    body,
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
            TypedExpr::OptionUnwrapOr {
                option,
                default,
                typ,
            } => {
                let subject = Box::new(self.compile_expr(option));
                let binding = IrVar::new(self.next_var_id());
                let reference_id = self.next_expr_id();
                PureExpr::Match {
                    match_: Match::Option {
                        subject,
                        some_arm_binding: Some(binding),
                        some_arm_body: Box::new(PureExpr::VariableReference {
                            value: binding,
                            typ: typ.clone(),
                            id: reference_id,
                        }),
                        none_arm_body: Box::new(self.compile_expr(default)),
                    },
                    typ: typ.clone(),
                    id: expr_id,
                }
            }
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
        let mut expr_ids = ExprIdCounter::new();
        let mut var_ids = VarIdCounter::new();
        let mut function_ids = FunctionIdCounter::new();
        let compiled_page = Compiler::new(&mut expr_ids, &mut var_ids, &mut function_ids, None)
            .compile_page_decl(page);
        let after = compiled_page.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_compile_the_head_and_the_body() {
        let mut page = build_page_no_params("MainComp", |t| {
            t.text("Hello World");
        });
        page.head = TypedExpr::HtmlConcat {
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
        };
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
                page MainComp() {
                  fn head() -> Html {
                    concat(
                      concat(
                        raw("<title"),
                        raw(">"),
                        concat(raw("Hi")),
                        raw("</title>"),
                      ),
                    )
                  }
                  fn body() -> Html {
                    concat(raw("Hello World"))
                  }
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
                  fn body() -> Html {
                    concat(text("Hello "), escape(name))
                  }
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
                page MainComp() {
                  concat(
                    concat(
                      raw("<div"),
                      raw(">"),
                      concat(raw("Content")),
                      raw("</div>"),
                    ),
                  )
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
                page MainComp(show@v0: Bool) {
                  concat(
                    let v1 = v0 in {
                      match v1 {
                        true => {
                          concat(
                            concat(
                              raw("<div"),
                              raw(">"),
                              concat(raw("Visible")),
                              raw("</div>"),
                            ),
                          )
                        }
                        false => { concat() }
                      }
                    },
                  )
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
                page MainComp(items@v0: Array[String]) {
                  concat(
                    concat(
                      raw("<ul"),
                      raw(">"),
                      concat(
                        for v1 in v0 {
                          concat(
                            concat(
                              raw("<li"),
                              raw(">"),
                              concat(escape(v1)),
                              raw("</li>"),
                            ),
                          )
                        },
                      ),
                      raw("</ul>"),
                    ),
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
                page MainComp() {
                  concat(
                    concat(
                      raw("<div"),
                      raw(" class=\""),
                      escape("base"),
                      raw("\""),
                      raw(" id=\""),
                      escape("test"),
                      raw("\""),
                      raw(">"),
                      concat(raw("Content")),
                      raw("</div>"),
                    ),
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
                page MainComp(cls@v0: String) {
                  concat(
                    concat(
                      raw("<div"),
                      raw(" class=\""),
                      escape("base"),
                      raw("\""),
                      raw(" data-value=\""),
                      escape(v0),
                      raw("\""),
                      raw(">"),
                      concat(raw("Content")),
                      raw("</div>"),
                    ),
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
                page TestComp(name@v0: String, count@v1: String) {
                  concat(
                    concat(
                      raw("<div"),
                      raw(">"),
                      concat(
                        raw("Hello "),
                        escape(v0),
                        raw(", count: "),
                        escape(v1),
                      ),
                      raw("</div>"),
                    ),
                  )
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
                page TestComp(flag@v0: Bool) {
                  concat(
                    let v1 = v0 in {
                      match v1 {
                        true => { concat(raw("yes")) }
                        false => { concat(raw("no")) }
                      }
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
                page MainComp() {
                  concat(
                    concat(
                      raw("<script"),
                      raw(">"),
                      concat(raw("alert(\"hi\")")),
                      raw("</script>"),
                    ),
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
                  fn body() -> Html {
                    concat(html(tag: "br", attrs: []))
                  }
                }

                -- after --
                page MainComp() {
                  concat(concat(raw("<br"), raw(">")))
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
                page MainComp(user@v0: User) {
                  concat(
                    escape(let v1 = v0 in {
                      User {name: "Jane", age: v1.age}
                    }.name),
                  )
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
                page MainComp(app@v0: App) {
                  concat(
                    escape(let v1 = v0.state in {
                      State {query: v1.query, num: 1}
                    }.query),
                  )
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
                skip_optimization: true,
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
                fn Button@f0(id@v0: String) -> Html {
                  concat(
                    raw("<button"),
                    raw(" id=\""),
                    escape(v0),
                    raw("\""),
                    raw(">"),
                    concat(raw("Go")),
                    raw("</button>"),
                  )
                }
                fn Button@f1(class@v1: String, disabled@v2: Bool) -> Html {
                  concat(
                    raw("<button"),
                    raw(" class=\""),
                    escape(v1),
                    raw("\""),
                    match v2 {
                      true => { raw(" disabled") }
                      false => { concat() }
                    },
                    raw(">"),
                    concat(raw("Go")),
                    raw("</button>"),
                  )
                }
                page Test() {
                  concat(
                    call Button@f0(id = "a"),
                    call Button@f1(class = "b", disabled = true),
                    call Button@f0(id = "c"),
                  )
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
                  title@v2: String,
                  id@v3: String,
                  class@v4: String,
                ) -> Html {
                  concat(
                    raw("<div"),
                    raw(" id=\""),
                    escape(v3),
                    raw("\""),
                    raw(" class=\""),
                    escape(v4),
                    raw("\""),
                    raw(">"),
                    concat(escape(v2)),
                    raw("</div>"),
                  )
                }
                fn Panel@f0(title@v0: String, class@v1: String) -> Html {
                  call Card@f1(title = v0, id = "panel", class = v1)
                }
                page Test() {
                  call Panel@f0(title = "Hi", class = "wide")
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
                fn Nest@f0(depth@v0: Int, id@v1: String) -> Html {
                  concat(
                    raw("<div"),
                    raw(" id=\""),
                    escape(v1),
                    raw("\""),
                    raw(">"),
                    concat(
                      let v2 = (0 < v0) in {
                        match v2 {
                          true => {
                            call Nest@f1(depth = (v0 - 1), class = "inner")
                          }
                          false => { concat() }
                        }
                      },
                    ),
                    raw("</div>"),
                  )
                }
                fn Nest@f1(depth@v3: Int, class@v4: String) -> Html {
                  concat(
                    raw("<div"),
                    raw(" class=\""),
                    escape(v4),
                    raw("\""),
                    raw(">"),
                    concat(
                      let v5 = (0 < v3) in {
                        match v5 {
                          true => {
                            call Nest@f1(depth = (v3 - 1), class = "inner")
                          }
                          false => { concat() }
                        }
                      },
                    ),
                    raw("</div>"),
                  )
                }
                page Test() {
                  call Nest@f0(depth = 2, id = "outer")
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
                  data-x@v0: String,
                  aria-label@v1: String,
                ) -> Html {
                  concat(
                    raw("<button"),
                    raw(" data-x=\""),
                    escape(v0),
                    raw("\""),
                    raw(" aria-label=\""),
                    escape(v1),
                    raw("\""),
                    raw(">"),
                    concat(raw("Go")),
                    raw("</button>"),
                  )
                }
                page Test() {
                  call Button@f0(data-x = "1", aria-label = "go")
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
                  concat(
                    raw("<p"),
                    raw(">"),
                    concat(raw("used")),
                    raw("</p>"),
                  )
                }
                page Test() {
                  call Used@f0()
                }
            "#]],
        );
    }
}
