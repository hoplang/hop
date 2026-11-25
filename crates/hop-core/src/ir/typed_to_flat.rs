use indexmap::IndexMap;
use std::sync::Arc;

use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::CheapString;
use crate::hop::typing::{
    Constructor, DataType, Pattern, PatternField, RecordField, ResolvedType, Type, TypeRegistry,
    TypedAttribute, TypedExpr, TypedFunctionDeclaration, TypedLoopSource, TypedPageDeclaration,
    TypedParameter, TypedPattern, TypedRecordUpdateField, TypedStatement,
};
use crate::ir::assertion_id::AssertionId;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_html_element::{FlatAttribute, FlatHtmlElement};
use crate::ir::flat_program::{
    FlatBinding, FlatBlock, FlatEntrypoint, FlatFor, FlatFunction, FlatLambda, FlatMatch,
    FlatMatchArm, FlatOp, FlatPattern, FlatProgram,
};
use crate::ir::flat_type::{FlatDataType, FlatSignature, FlatType, IrModifier};
use crate::ir::flat_type_declarations::{
    FlatEnum, FlatField, FlatRecord, FlatTypeDeclarations, FlatVariant,
};
use crate::ir::flat_typecheck;
use crate::ir::function_id::{FunctionId, FunctionIdCounter};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::lambda_id::LambdaIdCounter;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::HashMap;

use super::ir_page::IrPage;
use super::ir_parameter::IrParameter;
use super::ir_test::{IrAssertion, IrTest, IrTests};

/// Compile the pages, the tests and the functions they reach.
///
/// Flat has no pages. A page compiles to an entrypoint for its head, if
/// it has one, and one for its body, and the pages that point at them are
/// returned beside the program.
///
/// Flat has no tests either. A test is a function without parameters that
/// tests, and the tests that point at the functions of `tests` are returned
/// beside the program. An assertion compiles to an AssertEq that points at
/// where it is written, which is returned with the tests. An `assert!`
/// compiles to an AssertEq of its condition and true.
///
/// Each expression that computes a value becomes one binding with a fresh
/// name, in evaluation order. A let binds nothing, its variable is the name
/// of its value. Any other variable, a parameter, a loop variable or a
/// binding of a pattern, is bound to a fresh name of its own. A reference to
/// a variable is its name.
///
/// A rest parameter is resolved at compile time. A call supplies attributes
/// to the rest of its callee, and the callee is compiled once for each
/// distinct list of attributes it is called with, with a parameter for each
/// of them in place of the rest. So a function with a rest is compiled once
/// per such list, a function without one at most once, and a function no
/// page reaches not at all.
///
/// A lambda compiles to a declaration of its own, which reads each name
/// bound outside its body through a capture, and to a Lambda that binds a
/// closure of it over those names. An application of a variable compiles to
/// an Apply of its name.
///
/// Html is never part of another type, so no value depends on Html. It is
/// compiled all the same wherever it is written, and what computes a value
/// only passes it on.
pub fn typed_to_flat(
    pages: Vec<TypedPageDeclaration>,
    tests: &[(&RootContainedFilePath, &FunctionName)],
    source_functions: &[(&RootContainedFilePath, &TypedFunctionDeclaration)],
    registry: &TypeRegistry,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
) -> (FlatProgram, Vec<IrPage>, IrTests) {
    let mut value_ids = ValueIdCounter::new();
    let mut function_ids = FunctionIdCounter::new();

    // Each declaration with its position, since the program keeps the
    // functions in declaration order.
    let source: HashMap<(RootContainedFilePath, FunctionName), (usize, &TypedFunctionDeclaration)> =
        source_functions
            .iter()
            .enumerate()
            .map(|(index, (module, decl))| (((*module).clone(), decl.name.clone()), (index, *decl)))
            .collect();

    let mut compiler = Compiler::new(
        &mut value_ids,
        &mut function_ids,
        registry,
        asset_path_rewriter,
    );

    let pages: Vec<CompiledPage> = pages
        .into_iter()
        .map(|page| compiler.compile_page_decl(page))
        .collect();

    let tests: Vec<IrTest> = tests
        .iter()
        .map(|&(module, name)| {
            let function = compiler.function_id_counter.next();
            compiler.specializations.push(Specialization {
                module: module.clone(),
                name: name.clone(),
                shape: Vec::new(),
                id: function,
            });
            IrTest {
                module: module.clone(),
                name: name.clone(),
                function,
            }
        })
        .collect();

    // Compiling a body requests the specializations it calls, so this runs
    // until every request is compiled.
    let mut functions: Vec<(usize, FlatFunction)> = Vec::new();
    let mut next = 0;
    while let Some(request) = compiler.specializations.get(next).cloned() {
        let (index, decl) = source[&(request.module, request.name.clone())];
        let function = IrFunction::new(request.id, request.name);
        let function = compiler.compile_function_decl(decl, request.shape, function);
        functions.push((index, function));
        next += 1;
    }
    functions.sort_by_key(|(index, _)| *index);
    let functions: Vec<FlatFunction> = functions.into_iter().map(|(_, decl)| decl).collect();

    let mut entrypoints = Vec::new();
    let pages = pages
        .into_iter()
        .map(|page| page.declare(&mut entrypoints))
        .collect();

    let tests = IrTests {
        tests,
        assertions: compiler.assertions,
    };
    let program = FlatProgram {
        types: lower_type_declarations(registry),
        functions,
        lambdas: compiler.lambdas,
        entrypoints,
        value_ids,
    };
    if let Err(error) = flat_typecheck::typecheck(&program) {
        panic!("typed_to_flat produced an ill-typed program: {error:?}");
    }
    (program, pages, tests)
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
    id: FunctionId,
}

/// A page whose head and body are compiled but not yet declared as
/// functions: the parameters and the body of each.
struct CompiledPage {
    name: TypeName,
    head: Option<(Vec<IrParameter<FlatDataType>>, FlatBlock)>,
    body: (Vec<IrParameter<FlatDataType>>, FlatBlock),
}

impl CompiledPage {
    /// Declare the head and the body as entrypoints and point the page at them.
    fn declare(self, entrypoints: &mut Vec<FlatEntrypoint>) -> IrPage {
        let mut declare = |name: &str, (parameters, body)| {
            let id = EntrypointId::new(entrypoints.len());
            entrypoints.push(FlatEntrypoint {
                name: FunctionName::new(CheapString::new(name.to_string()))
                    .expect("a page member name is valid"),
                parameters,
                body,
            });
            id
        };
        let head = self.head.map(|head| declare("head", head));
        let body = declare("body", self.body);
        IrPage {
            name: self.name,
            head,
            body,
        }
    }
}

struct Compiler<'a> {
    value_id_counter: &'a mut ValueIdCounter,
    function_id_counter: &'a mut FunctionIdCounter,
    registry: &'a TypeRegistry,
    /// The specializations calls have requested so far, in request order.
    specializations: Vec<Specialization>,
    /// The name each variable in scope stands for.
    scopes: Vec<Vec<(VarName, ValueId)>>,
    /// The parameters of the function being compiled, those it declares and
    /// those its rest adds from a function it is spread into. A forwarded
    /// parameter reads these, so a binding in the body that reuses the name
    /// does not capture it.
    params: HashMap<VarName, ValueId>,
    /// The attributes the specialization being compiled receives through its
    /// rest, each with the parameter that holds it, in the order they
    /// render. The spread reads these. They are not in scope by name.
    rest: Vec<(AttributeName, Type, ValueId)>,
    /// The bindings of the block being compiled, in evaluation order.
    bindings: Vec<FlatBinding>,
    context: IrModifier,
    lambda_id_counter: LambdaIdCounter,
    lambdas: Vec<FlatLambda>,
    types: HashMap<ValueId, FlatType>,
    /// The assertions compiled so far, by the id of each.
    assertions: Vec<IrAssertion>,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
}

impl<'a> Compiler<'a> {
    fn new(
        value_id_counter: &'a mut ValueIdCounter,
        function_id_counter: &'a mut FunctionIdCounter,
        registry: &'a TypeRegistry,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Self {
        Compiler {
            value_id_counter,
            function_id_counter,
            registry,
            specializations: Vec::new(),
            scopes: vec![Vec::new()],
            params: HashMap::new(),
            rest: Vec::new(),
            bindings: Vec::new(),
            context: IrModifier::Pure,
            lambda_id_counter: LambdaIdCounter::new(),
            lambdas: Vec::new(),
            types: HashMap::new(),
            assertions: Vec::new(),
            asset_path_rewriter,
        }
    }

    fn compile_function_decl(
        &mut self,
        decl: &TypedFunctionDeclaration,
        shape: Vec<(AttributeName, Type)>,
        function: IrFunction,
    ) -> FlatFunction {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len() + shape.len());
        for param in &decl.params {
            let var = self.bind(&param.var_name);
            self.params.insert(param.var_name.clone(), var);
            let typ = lower_type(self.registry, &param.var_type);
            self.types.insert(var, typ.clone());
            parameters.push(IrParameter {
                name: param.var_name.clone().into(),
                binder: IrBinder { var, typ },
            });
        }
        // Each attribute the specialization receives through its rest is a
        // parameter of its own, after the declared ones and in the order of
        // the shape, as a call passes them.
        for (name, typ) in shape {
            let var = self.value_id_counter.next();
            let binder = IrBinder {
                var,
                typ: lower_type(self.registry, &typ),
            };
            self.types.insert(var, binder.typ.clone());
            self.rest.push((name.clone(), typ, var));
            parameters.push(IrParameter { name, binder });
        }

        let modifier = IrModifier::from(decl.modifier);
        let outer = std::mem::replace(&mut self.context, modifier);
        let body = match &decl.return_type {
            Type::Html(_) => self.compile_html_block(&decl.body),
            Type::Data(_) => self.compile_value_block(&decl.body),
        };
        self.context = outer;
        let return_type = lower_type(self.registry, &decl.return_type);
        let declaration = FlatFunction {
            function,
            modifier,
            parameters,
            return_type,
            body,
        };
        self.pop_scope();
        self.params.clear();
        self.rest.clear();
        declaration
    }

    fn compile_page_decl(&mut self, page: TypedPageDeclaration) -> CompiledPage {
        CompiledPage {
            name: page.name,
            head: page
                .head
                .as_ref()
                .map(|head| self.compile_page_member(&page.params, head)),
            body: self.compile_page_member(&page.params, &page.body),
        }
    }

    /// Compile the head or the body of a page as an entrypoint
    /// that declares the page's parameters, each with a name of its own.
    fn compile_page_member(
        &mut self,
        params: &[TypedParameter],
        expr: &TypedExpr,
    ) -> (Vec<IrParameter<FlatDataType>>, FlatBlock) {
        self.push_scope();
        let parameters = params
            .iter()
            .map(|param| {
                let var = self.bind(&param.var_name);
                let typ = match &param.var_type {
                    Type::Data(typ) => lower_data_type(self.registry, typ),
                    _ => panic!("a page parameter must have a data type"),
                };
                self.types.insert(var, FlatType::Data(typ.clone()));
                IrParameter {
                    name: param.var_name.clone().into(),
                    binder: IrBinder { var, typ },
                }
            })
            .collect();
        let outer = std::mem::replace(&mut self.context, IrModifier::Render);
        let body = self.compile_html_block(expr);
        self.context = outer;
        self.pop_scope();
        (parameters, body)
    }

    /// Compile `expr`, which computes a value, as a block of its own,
    /// holding the bindings of its parts outside any nested block.
    fn compile_value_block(&mut self, expr: &TypedExpr) -> FlatBlock {
        self.data_block(|cx| cx.compile_expr(expr))
    }

    /// Compile `expr`, which renders Html, as a block of its own, holding
    /// the bindings of its parts outside any nested block.
    fn compile_html_block(&mut self, expr: &TypedExpr) -> FlatBlock {
        self.html_block(|cx| cx.compile_expr(expr))
    }

    /// The block of the bindings `f` pushes, whose result is the value `f`
    /// returns.
    fn data_block(&mut self, f: impl FnOnce(&mut Self) -> ValueId) -> FlatBlock {
        let render = self.context == IrModifier::Render;
        let outer = std::mem::take(&mut self.bindings);
        let result = f(self);
        let bindings = std::mem::replace(&mut self.bindings, outer)
            .into_iter()
            .inspect(|binding| {
                let cell = matches!(binding.op, FlatOp::Cell(_));
                assert!(
                    render || !cell,
                    "a block that computes a value outside what renders binds no cell"
                );
            })
            .collect();
        IrBlock { bindings, result }
    }

    /// The block of the bindings `f` pushes, whose result is the Html `f`
    /// returns.
    fn html_block(&mut self, f: impl FnOnce(&mut Self) -> ValueId) -> FlatBlock {
        let outer = std::mem::take(&mut self.bindings);
        let result = f(self);
        IrBlock {
            bindings: std::mem::replace(&mut self.bindings, outer),
            result,
        }
    }

    /// Bind `op` to a fresh name of type `typ`, which is a value, in the
    /// block being compiled, and return the name.
    fn push(&mut self, typ: Type, op: FlatOp) -> ValueId {
        let typ = lower_type(self.registry, &typ);
        assert!(
            matches!(typ, FlatType::Data(_)),
            "an op of a value produces a value"
        );
        let name = self.value_id_counter.next();
        self.types.insert(name, typ.clone());
        self.bindings.push(IrBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    /// Bind `op` to a fresh name of type Html in the block being compiled,
    /// and return the name.
    fn push_html(&mut self, op: FlatOp) -> ValueId {
        self.bind_html(FlatType::HTML, op)
    }

    /// Bind `op` to a fresh name of type `typ` in the block being compiled,
    /// and return the name.
    fn bind_html(&mut self, typ: FlatType, op: FlatOp) -> ValueId {
        let name = self.value_id_counter.next();
        self.types.insert(name, typ.clone());
        self.bindings.push(IrBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    /// Compile the arms of a match, each body as a block of `compile_block`
    /// with the names its pattern binds in scope.
    fn compile_arms(
        &mut self,
        arms: &[(TypedPattern, TypedExpr)],
        compile_block: impl Fn(&mut Self, &TypedExpr) -> FlatBlock,
    ) -> Vec<FlatMatchArm> {
        arms.iter()
            .map(|(pattern, body)| {
                self.push_scope();
                let pattern = self.compile_pattern(pattern);
                let body = compile_block(self, body);
                self.pop_scope();
                FlatMatchArm { pattern, body }
            })
            .collect()
    }

    fn close_lambda(
        &mut self,
        modifier: IrModifier,
        parameters: Vec<IrBinder<FlatType>>,
        return_type: FlatType,
        body: FlatBlock,
    ) -> FlatOp {
        let id = self.lambda_id_counter.next();
        let (lambda, captured) =
            FlatLambda::closing(id, modifier, parameters, return_type, body, |name| {
                let var = self.value_id_counter.next();
                let typ = self.types[&name].clone();
                self.types.insert(var, typ.clone());
                IrBinder { var, typ }
            });
        self.lambdas.push(lambda);
        FlatOp::Lambda(id, captured)
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope stack should not be empty");
    }

    fn bind(&mut self, name: &VarName) -> ValueId {
        let id = self.value_id_counter.next();
        self.name(name, id);
        id
    }

    fn name(&mut self, name: &VarName, id: ValueId) {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
            .push((name.clone(), id));
    }

    /// Name the variables of a let pattern, which matches every value of
    /// `typ`: each names the element or field it reads out of `value`.
    fn destructure(&mut self, pattern: &TypedPattern, value: ValueId, typ: &DataType) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => self.name(&binder.var, value),
            Pattern::Constructor {
                constructor: Constructor::Tuple,
                args,
                ..
            } => {
                let DataType::Tuple(elements) = typ else {
                    unreachable!("a tuple pattern matches a tuple, not {typ}");
                };
                for (index, (arg, element)) in args.iter().zip(elements).enumerate() {
                    if let Pattern::Wildcard = arg {
                        continue;
                    }
                    let op = FlatOp::Data(IrDataOp::TupleIndex(value, index));
                    let part = self.push(Type::Data(element.clone()), op);
                    self.destructure(arg, part, element);
                }
            }
            Pattern::Constructor {
                constructor: Constructor::Record { .. },
                fields,
                ..
            } => {
                let registry = self.registry;
                let Some(ResolvedType::Record {
                    fields: declared, ..
                }) = registry.resolve(typ)
                else {
                    unreachable!("a record pattern matches a record, not {typ}");
                };
                for field in fields {
                    if let Pattern::Wildcard = field.pattern {
                        continue;
                    }
                    let field_type = &declared[field.index].typ;
                    let op = FlatOp::Data(IrDataOp::FieldAccess(value, field.name.clone()));
                    let part = self.push(Type::Data(field_type.clone()), op);
                    self.destructure(&field.pattern, part, field_type);
                }
            }
            Pattern::Constructor { .. } => unreachable!("a let pattern matches every value"),
        }
    }

    fn resolve(&mut self, name: &VarName) -> ValueId {
        for scope in self.scopes.iter().rev() {
            if let Some((_, id)) = scope.iter().rev().find(|(n, _)| n == name) {
                return *id;
            }
        }
        panic!("undefined variable: {name}");
    }

    /// Bind the variables of a pattern in the current scope, and return the
    /// pattern with each binding binding the name of its variable.
    fn compile_pattern(&mut self, pattern: &TypedPattern) -> FlatPattern {
        match pattern {
            Pattern::Wildcard => Pattern::Wildcard,
            Pattern::Binding(binder) => {
                let Type::Data(typ) = binder.typ.clone() else {
                    unreachable!("a pattern binds data");
                };
                let var = self.bind(&binder.var);
                let typ = lower_data_type(self.registry, &typ);
                self.types.insert(var, FlatType::Data(typ.clone()));
                Pattern::Binding(IrBinder { var, typ })
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => Pattern::Constructor {
                constructor: constructor.clone(),
                args: args.iter().map(|arg| self.compile_pattern(arg)).collect(),
                fields: fields
                    .iter()
                    .map(|field| PatternField {
                        name: field.name.clone(),
                        index: field.index,
                        pattern: self.compile_pattern(&field.pattern),
                    })
                    .collect(),
            },
        }
    }

    /// Push the bindings of `expr` to the block being compiled, and return
    /// the name of its value or its Html.
    fn compile_expr(&mut self, expr: &TypedExpr) -> ValueId {
        let typ = expr.typ();
        let op = match expr {
            TypedExpr::Var { value, .. } => return self.resolve(value),
            TypedExpr::ForwardedParam { value, .. } => return self.params[value],
            TypedExpr::FieldAccess { record, field, .. } => FlatOp::Data(IrDataOp::FieldAccess(
                self.compile_expr(record),
                field.clone(),
            )),
            TypedExpr::BoolNegation { operand, .. } => {
                FlatOp::Data(IrDataOp::BoolNegation(self.compile_expr(operand)))
            }
            TypedExpr::NumericNegation {
                operand,
                operand_type,
            } => FlatOp::Data(IrDataOp::NumericNegation(
                *operand_type,
                self.compile_expr(operand),
            )),
            TypedExpr::Array { elements, .. } => FlatOp::Data(IrDataOp::Array(
                elements.iter().map(|e| self.compile_expr(e)).collect(),
            )),
            TypedExpr::Tuple { elements, .. } => FlatOp::Data(IrDataOp::Tuple(
                elements.iter().map(|e| self.compile_expr(e)).collect(),
            )),
            TypedExpr::Record { fields, .. } => FlatOp::Data(IrDataOp::Record(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.compile_expr(v)))
                    .collect(),
            )),
            TypedExpr::RecordUpdate { base, fields, .. } => {
                let base = self.compile_expr(base);
                FlatOp::Data(IrDataOp::Record(
                    fields
                        .iter()
                        .map(|(name, field)| {
                            let value = match field {
                                TypedRecordUpdateField::Explicit(value) => self.compile_expr(value),
                                TypedRecordUpdateField::FromBase(field_typ) => self.push(
                                    field_typ.clone(),
                                    FlatOp::Data(IrDataOp::FieldAccess(base, name.clone())),
                                ),
                            };
                            (name.clone(), value)
                        })
                        .collect(),
                ))
            }
            TypedExpr::StringLiteral { value, .. } => {
                FlatOp::Data(IrDataOp::StringLiteral(value.clone()))
            }
            TypedExpr::Asset { path } => {
                let value = match &self.asset_path_rewriter {
                    Some(rewriter) => rewriter.rewrite(path),
                    None => format!("/{}", path.as_str()),
                };
                FlatOp::Data(IrDataOp::StringLiteral(CheapString::new(value)))
            }
            TypedExpr::BoolLiteral { value, .. } => FlatOp::Data(IrDataOp::BoolLiteral(*value)),
            TypedExpr::FloatLiteral { value, .. } => FlatOp::Data(IrDataOp::FloatLiteral(*value)),
            TypedExpr::IntLiteral { value, .. } => FlatOp::Data(IrDataOp::IntLiteral(*value)),
            TypedExpr::StringConcat { parts, .. } => FlatOp::Data(IrDataOp::StringConcat(
                parts.iter().map(|part| self.compile_expr(part)).collect(),
            )),
            TypedExpr::Equals {
                left,
                right,
                operand_type,
            } => FlatOp::Data(IrDataOp::Equals(
                *operand_type,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::NotEquals {
                left,
                right,
                operand_type,
            } => {
                // Desugar NotEquals into BoolNegation(Equals(...))
                let equals = FlatOp::Data(IrDataOp::Equals(
                    *operand_type,
                    self.compile_expr(left),
                    self.compile_expr(right),
                ));
                FlatOp::Data(IrDataOp::BoolNegation(self.push(Type::BOOL, equals)))
            }
            TypedExpr::LessThan {
                left,
                right,
                operand_type,
            } => FlatOp::Data(IrDataOp::LessThan(
                *operand_type,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            // Convert a > b to b < a
            TypedExpr::GreaterThan {
                left,
                right,
                operand_type,
            } => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                FlatOp::Data(IrDataOp::LessThan(*operand_type, right, left))
            }
            TypedExpr::LessThanOrEqual {
                left,
                right,
                operand_type,
            } => FlatOp::Data(IrDataOp::LessThanOrEqual(
                *operand_type,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            // Convert a >= b to b <= a
            TypedExpr::GreaterThanOrEqual {
                left,
                right,
                operand_type,
            } => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                FlatOp::Data(IrDataOp::LessThanOrEqual(*operand_type, right, left))
            }
            // The right operand runs only when the left one is true, so it
            // becomes the true arm. The false arm is false.
            TypedExpr::BoolLogicalAnd { left, right, .. } => FlatOp::Match(FlatMatch {
                subject: self.compile_expr(left),
                arms: vec![
                    FlatMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolTrue,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.compile_value_block(right),
                    },
                    FlatMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolFalse,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.data_block(|cx| {
                            cx.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(false)))
                        }),
                    },
                ],
            }),
            // The right operand runs only when the left one is false, so it
            // becomes the false arm. The true arm is true.
            TypedExpr::BoolLogicalOr { left, right, .. } => FlatOp::Match(FlatMatch {
                subject: self.compile_expr(left),
                arms: vec![
                    FlatMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolTrue,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.data_block(|cx| {
                            cx.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(true)))
                        }),
                    },
                    FlatMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolFalse,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.compile_value_block(right),
                    },
                ],
            }),
            TypedExpr::NumericAdd {
                left,
                right,
                operand_types,
            } => FlatOp::Data(IrDataOp::NumericAdd(
                *operand_types,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::NumericSubtract {
                left,
                right,
                operand_types,
            } => FlatOp::Data(IrDataOp::NumericSubtract(
                *operand_types,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::NumericMultiply {
                left,
                right,
                operand_types,
            } => FlatOp::Data(IrDataOp::NumericMultiply(
                *operand_types,
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::Enum {
                variant_name,
                fields,
                ..
            } => FlatOp::Data(IrDataOp::Enum(
                variant_name.clone(),
                fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.compile_expr(field_expr))
                    })
                    .collect(),
            )),
            TypedExpr::Match { subject, arms, .. } => {
                let subject = self.compile_expr(subject);
                if typ == Type::HTML {
                    let arms = self.compile_arms(arms, Self::compile_html_block);
                    return self.push_html(FlatOp::Match(FlatMatch { subject, arms }));
                }
                let arms = self.compile_arms(arms, Self::compile_value_block);
                FlatOp::Match(FlatMatch { subject, arms })
            }
            TypedExpr::Option { value, .. } => FlatOp::Data(match value {
                Some(value) => IrDataOp::OptionSome(self.compile_expr(value)),
                None => IrDataOp::OptionNone,
            }),
            TypedExpr::HtmlConcat { parts } => {
                let mut compiled = Vec::with_capacity(parts.len());
                for part in parts {
                    assert_eq!(
                        part.typ(),
                        Type::HTML,
                        "HtmlConcat must hold Html, but holds {part}"
                    );
                    compiled.push(self.compile_expr(part));
                }
                return self.push_html(FlatOp::HtmlConcat(compiled));
            }
            TypedExpr::HtmlText { value } => {
                return self.push_html(FlatOp::HtmlText(value.clone()));
            }
            TypedExpr::HtmlEscape { expr } => {
                assert_eq!(
                    expr.typ(),
                    Type::STRING,
                    "HtmlEscape must hold a String, but holds {expr}"
                );
                let string = self.compile_expr(expr);
                return self.push_html(FlatOp::HtmlEscape(string));
            }
            TypedExpr::Element {
                element,
                attrs,
                children,
                on_click,
            } => {
                let mut attributes =
                    IndexMap::with_capacity(attrs.attributes.len() + self.rest.len());
                for (name, attr) in &attrs.attributes {
                    let attribute = match attr {
                        TypedAttribute::Value(value) => {
                            FlatAttribute::Value(self.compile_expr(value))
                        }
                        TypedAttribute::Presence(present) => {
                            FlatAttribute::Presence(self.compile_expr(present))
                        }
                    };
                    attributes.insert(name.clone(), attribute);
                }
                // The spread places the attributes the rest receives after
                // those written on the element. A Bool is a boolean
                // attribute, any other type a value. The typechecker rejects
                // a forwarded attribute the element also writes, so every
                // name is new.
                if attrs.spread.is_some() {
                    for (name, typ, value) in self.rest.clone() {
                        let attribute = match typ {
                            Type::BOOL => FlatAttribute::Presence(value),
                            _ => FlatAttribute::Value(value),
                        };
                        let previous = attributes.insert(name, attribute);
                        assert!(previous.is_none(), "an attribute is set twice");
                    }
                }
                let on_click = on_click.as_ref().map(|handler| self.compile_expr(handler));
                let children = if element.is_void() {
                    None
                } else {
                    Some(self.compile_expr(children))
                };
                return self.push_html(FlatOp::HtmlElement(FlatHtmlElement {
                    element: element.clone(),
                    attributes,
                    children,
                    on_click,
                }));
            }
            TypedExpr::Call {
                function_name,
                module,
                args,
                rest,
                ..
            } => {
                // The typechecker orders the arguments as the callee
                // declares its parameters.
                let mut compiled_args: Vec<ValueId> = args
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
                    for (name, attr) in &attrs.attributes {
                        let (typ, value) = match attr {
                            TypedAttribute::Value(value) => {
                                (Type::STRING, self.compile_expr(value))
                            }
                            TypedAttribute::Presence(present) => {
                                (Type::BOOL, self.compile_expr(present))
                            }
                        };
                        compiled_args.push(value);
                        shape.push((name.clone(), typ));
                    }
                    if attrs.spread.is_some() {
                        for (name, typ, var) in self.rest.clone() {
                            compiled_args.push(var);
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
                let id = match existing {
                    Some(existing) => existing.id,
                    None => {
                        let id = self.function_id_counter.next();
                        self.specializations.push(Specialization {
                            module: module.clone(),
                            name: function_name.clone(),
                            shape,
                            id,
                        });
                        id
                    }
                };
                if typ == Type::HTML {
                    return self.push_html(FlatOp::Call(id, compiled_args));
                }
                FlatOp::Call(id, compiled_args)
            }
            // A let binds nothing. Its variable is the name of its value, in
            // a scope that the block's lets share and that holds the tail,
            // and the variables of a pattern are names of the elements and
            // fields it reads out of the value.
            TypedExpr::Block { statements, tail } => {
                self.push_scope();
                for statement in statements {
                    let binding = match statement {
                        TypedStatement::Let(binding) => binding,
                        TypedStatement::Expr(expr) => {
                            self.compile_expr(expr);
                            continue;
                        }
                    };
                    let value = self.compile_expr(&binding.value);
                    match &binding.pattern {
                        Pattern::Binding(binder) => self.name(&binder.var, value),
                        pattern => {
                            let Type::Data(data) = binding.value.typ() else {
                                unreachable!("a let pattern other than a name takes data apart");
                            };
                            self.destructure(pattern, value, &data);
                        }
                    }
                }
                let tail = self.compile_expr(tail);
                self.pop_scope();
                return tail;
            }
            TypedExpr::For {
                var_name,
                source,
                key,
                body,
                ..
            } => {
                assert_eq!(
                    typ,
                    Type::HTML,
                    "For must fold into Html, but folds into {typ}"
                );
                let array = match &**source {
                    TypedLoopSource::Array(array_expr) => self.compile_expr(array_expr),
                    TypedLoopSource::RangeInclusive { start, end } => {
                        let op = FlatOp::Data(IrDataOp::ArrayFromRange(
                            self.compile_expr(start),
                            self.compile_expr(end),
                        ));
                        self.push(Type::array(DataType::Int), op)
                    }
                };
                let element_type = match &**source {
                    TypedLoopSource::Array(array_expr) => {
                        let Type::Data(DataType::Array(element_type)) = array_expr.typ() else {
                            unreachable!("a loop over an array has an Array source");
                        };
                        lower_data_type(self.registry, &element_type)
                    }
                    TypedLoopSource::RangeInclusive { .. } => FlatDataType::Int,
                };
                self.push_scope();
                let var = var_name.as_ref().map(|name| IrBinder {
                    var: self.bind(name),
                    typ: element_type,
                });
                if let Some(binder) = &var {
                    self.types
                        .insert(binder.var, FlatType::Data(binder.typ.clone()));
                }
                let key = key.as_ref().map(|key| self.compile_value_block(key));
                let body = self.compile_html_block(body);
                self.pop_scope();
                return self.push_html(FlatOp::HtmlFor(FlatFor {
                    var,
                    array,
                    key,
                    body,
                }));
            }
            // A state binds a cell, whose initial value is compiled as a
            // block of its own. Its value is a tuple of a read of the cell
            // and a mut fn that updates the cell with the function it takes.
            TypedExpr::Assert { condition, range } => {
                let condition = self.compile_expr(condition);
                let expected = self.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(true)));
                let assertion = AssertionId::new(self.assertions.len());
                self.assertions.push(IrAssertion {
                    range: range.clone(),
                    reports_operands: false,
                });
                FlatOp::AssertEq(assertion, condition, expected)
            }
            TypedExpr::AssertEq { left, right, range } => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                let assertion = AssertionId::new(self.assertions.len());
                self.assertions.push(IrAssertion {
                    range: range.clone(),
                    reports_operands: true,
                });
                FlatOp::AssertEq(assertion, left, right)
            }
            TypedExpr::State { initial, .. } => FlatOp::Cell(self.compile_value_block(initial)),
            TypedExpr::Lambda { params, body, .. } => {
                let Type::Data(DataType::Function(function)) = &typ else {
                    unreachable!("a lambda has a function type");
                };
                let outer =
                    std::mem::replace(&mut self.context, IrModifier::from(function.modifier));
                self.push_scope();
                let parameters: Vec<IrBinder<FlatType>> = params
                    .iter()
                    .map(|(name, typ)| IrBinder {
                        var: match name {
                            Some(name) => self.bind(name),
                            None => self.value_id_counter.next(),
                        },
                        typ: lower_type(self.registry, typ),
                    })
                    .collect();
                for parameter in &parameters {
                    self.types.insert(parameter.var, parameter.typ.clone());
                }
                let body = if typ.renders_html() {
                    self.compile_html_block(body)
                } else {
                    self.compile_value_block(body)
                };
                self.pop_scope();
                self.context = outer;
                self.close_lambda(
                    IrModifier::from(function.modifier),
                    parameters,
                    lower_type(self.registry, &function.result),
                    body,
                )
            }
            TypedExpr::Apply { callee, args, .. } => {
                let closure = self.resolve(callee);
                let args = args.iter().map(|arg| self.compile_expr(arg)).collect();
                if typ == Type::HTML {
                    return self.push_html(FlatOp::Apply(closure, args));
                }
                FlatOp::Apply(closure, args)
            }
            TypedExpr::ArrayLength { array } => {
                FlatOp::Data(IrDataOp::ArrayLength(self.compile_expr(array)))
            }
            TypedExpr::ArrayIsEmpty { array } => {
                FlatOp::Data(IrDataOp::ArrayIsEmpty(self.compile_expr(array)))
            }
            TypedExpr::ArrayGet { array, index, .. } => {
                let array = self.compile_expr(array);
                FlatOp::Data(IrDataOp::ArrayGet(array, self.compile_expr(index)))
            }
            TypedExpr::ArrayEnumerate { array, .. } => {
                FlatOp::Data(IrDataOp::ArrayEnumerate(self.compile_expr(array)))
            }
            TypedExpr::StringIsEmpty { string } => {
                FlatOp::Data(IrDataOp::StringIsEmpty(self.compile_expr(string)))
            }
            TypedExpr::OptionIsSome { option } => {
                FlatOp::Data(IrDataOp::OptionIsSome(self.compile_expr(option)))
            }
            TypedExpr::OptionIsNone { option } => {
                FlatOp::Data(IrDataOp::OptionIsNone(self.compile_expr(option)))
            }
            TypedExpr::OptionUnwrapOr {
                option, default, ..
            } => {
                let Type::Data(element_type) = typ.clone() else {
                    unreachable!("an Option holds data");
                };
                let subject = self.compile_expr(option);
                let binding = self.value_id_counter.next();
                self.types.insert(
                    binding,
                    FlatType::Data(lower_data_type(self.registry, &element_type)),
                );
                FlatOp::Match(FlatMatch {
                    subject,
                    arms: vec![
                        FlatMatchArm {
                            pattern: Pattern::Constructor {
                                constructor: Constructor::OptionSome,
                                args: vec![Pattern::Binding(IrBinder {
                                    var: binding,
                                    typ: lower_data_type(self.registry, &element_type),
                                })],
                                fields: Vec::new(),
                            },
                            body: self.data_block(|_| binding),
                        },
                        FlatMatchArm {
                            pattern: Pattern::Constructor {
                                constructor: Constructor::OptionNone,
                                args: Vec::new(),
                                fields: Vec::new(),
                            },
                            body: self.compile_value_block(default),
                        },
                    ],
                })
            }
            TypedExpr::IntToString { value } => {
                FlatOp::Data(IrDataOp::IntToString(self.compile_expr(value)))
            }
            TypedExpr::FloatToInt { value } => {
                FlatOp::Data(IrDataOp::FloatToInt(self.compile_expr(value)))
            }
            TypedExpr::IntToFloat { value } => {
                FlatOp::Data(IrDataOp::IntToFloat(self.compile_expr(value)))
            }
        };
        self.push(typ, op)
    }
}

fn lower_type_declarations(registry: &TypeRegistry) -> FlatTypeDeclarations {
    let lower_fields = |fields: &[RecordField]| {
        fields
            .iter()
            .map(|field| FlatField {
                name: field.name.clone(),
                typ: lower_data_type(registry, &field.typ),
            })
            .collect()
    };
    FlatTypeDeclarations {
        records: registry
            .records()
            .map(|(name, fields)| FlatRecord {
                name: name.clone(),
                fields: lower_fields(fields),
            })
            .collect(),
        enums: registry
            .enums()
            .map(|(name, variants)| FlatEnum {
                name: name.clone(),
                variants: variants
                    .iter()
                    .map(|variant| FlatVariant {
                        name: variant.name.clone(),
                        fields: lower_fields(&variant.fields),
                    })
                    .collect(),
            })
            .collect(),
    }
}

fn lower_type(registry: &TypeRegistry, typ: &Type) -> FlatType {
    match typ {
        Type::Data(data) => FlatType::Data(lower_data_type(registry, data)),
        Type::Html(_) => FlatType::Html,
    }
}

fn lower_data_type(registry: &TypeRegistry, typ: &DataType) -> FlatDataType {
    match typ {
        DataType::String => FlatDataType::String,
        DataType::Bool => FlatDataType::Bool,
        DataType::Int => FlatDataType::Int,
        DataType::Float => FlatDataType::Float,
        DataType::Array(element) => {
            FlatDataType::Array(Box::new(lower_data_type(registry, element)))
        }
        DataType::Option(element) => {
            FlatDataType::Option(Box::new(lower_data_type(registry, element)))
        }
        DataType::Tuple(elements) => FlatDataType::Tuple(
            elements
                .iter()
                .map(|element| lower_data_type(registry, element))
                .collect(),
        ),
        DataType::Named { name, .. } => match registry.resolve(typ) {
            Some(ResolvedType::Record { .. }) => FlatDataType::Record(name.clone()),
            Some(ResolvedType::Enum { .. }) => FlatDataType::Enum(name.clone()),
            _ => panic!("type `{name}` must be declared"),
        },
        DataType::Function(function) => FlatDataType::Function(FlatSignature {
            modifier: IrModifier::from(function.modifier),
            parameters: (function.parameters.iter())
                .map(|parameter| lower_type(registry, parameter))
                .collect(),
            result: Box::new(lower_type(registry, &function.result)),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::session::Session;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    /// Compile a program written in source, without optimization.
    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut session = Session::new();
        session.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, pages) = session.compile_flat(None, None);
        let pages: String = pages.iter().map(|page| format!("{page}\n")).collect();
        expected.assert_eq(&format!("{flat}{pages}"));
    }

    #[test]
    fn should_compile_the_head_and_the_body() {
        check(
            indoc! {r#"
                page Test() {
                  fn head() -> Html {
                    <title>Hi</title>
                  }
                  fn body() -> Html {
                    <>Hello World</>
                  }
                }
            "#},
            expect![[r#"
                entrypoint head@e0() -> Html {
                  let v0: Html = text("Hi")
                  let v1: Html = concat(v0)
                  let v2: Html = element("title", {}, v1)
                  v2
                }
                entrypoint body@e1() -> Html {
                  let v3: Html = text("Hello World")
                  let v4: Html = concat(v3)
                  v4
                }
                page Test: e0, e1
            "#]],
        );
    }

    #[test]
    fn should_compile_text_expression() {
        check(
            indoc! {r#"
                page Test(name: String) {
                  fn body() -> Html {
                    <>Hello {name}</>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  name@v0: String,
                ) -> Html {
                  let v1: Html = text("Hello ")
                  let v2: Html = escape(v0)
                  let v3: Html = concat(v1, v2)
                  v3
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_html_element() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div>Content</div>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0() -> Html {
                  let v0: Html = text("Content")
                  let v1: Html = concat(v0)
                  let v2: Html = element("div", {}, v1)
                  v2
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_for_html() {
        check(
            indoc! {r#"
                page Test(items: Array[String]) {
                  fn body() -> Html {
                    <ul>
                      {for item in items {
                        <li>{item}</li>
                      }}
                    </ul>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  items@v0: Array[String],
                ) -> Html {
                  let v5: Html = for v1: String in v0 {
                    let v2: Html = escape(v1)
                    let v3: Html = concat(v2)
                    let v4: Html = element("li", {}, v3)
                    v4
                  }
                  let v6: Html = concat(v5)
                  let v7: Html = element("ul", {}, v6)
                  v7
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_a_let_in_a_loop_body_or_an_arm_in_that_block() {
        check(
            indoc! {r#"
                page Test(items: Array[String], name: Option[String]) {
                  fn body() -> Html {
                    <ul>
                      {for item in items {
                        let label = item;
                        <li>{label}</li>
                      }}
                      {match name {
                        Some(n) => { let greeting = n; <b>{greeting}</b> },
                        None => <i>anonymous</i>,
                      }}
                    </ul>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  items@v0: Array[String],
                  name@v1: Option[String],
                ) -> Html {
                  let v6: Html = for v2: String in v0 {
                    let v3: Html = escape(v2)
                    let v4: Html = concat(v3)
                    let v5: Html = element("li", {}, v4)
                    v5
                  }
                  let v14: Html = match v1 {
                    Some(v7: String) => {
                      let v8: Html = escape(v7)
                      let v9: Html = concat(v8)
                      let v10: Html = element("b", {}, v9)
                      v10
                    }
                    None => {
                      let v11: Html = text("anonymous")
                      let v12: Html = concat(v11)
                      let v13: Html = element("i", {}, v12)
                      v13
                    }
                  }
                  let v15: Html = concat(v6, v14)
                  let v16: Html = element("ul", {}, v15)
                  v16
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_static_attributes() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div class="base" id="test">Content</div>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0() -> Html {
                  let v0: String = "base"
                  let v1: String = "test"
                  let v2: Html = text("Content")
                  let v3: Html = concat(v2)
                  let v4: Html = element("div", {class: v0, id: v1}, v3)
                  v4
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_dynamic_attributes() {
        check(
            indoc! {r#"
                page Test(cls: String) {
                  fn body() -> Html {
                    <div class="base" data-value={cls}>Content</div>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  cls@v0: String,
                ) -> Html {
                  let v1: String = "base"
                  let v2: Html = text("Content")
                  let v3: Html = concat(v2)
                  let v4: Html = element("div", {class: v1, data-value: v0}, v3)
                  v4
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_bool_match_html() {
        check(
            indoc! {r#"
                page Test(flag: Bool) {
                  fn body() -> Html {
                    match flag {
                      true => <>yes</>,
                      false => <>no</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  flag@v0: Bool,
                ) -> Html {
                  let v5: Html = match v0 {
                    true => {
                      let v1: Html = text("yes")
                      let v2: Html = concat(v1)
                      v2
                    }
                    false => {
                      let v3: Html = text("no")
                      let v4: Html = concat(v3)
                      v4
                    }
                  }
                  v5
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_the_short_circuit_operators_to_bool_matches() {
        check(
            indoc! {r#"
                page Test(a: Bool, b: Bool) {
                  fn body() -> Html {
                    <>
                      {match a && !b {
                        true => <>and</>,
                        false => <></>,
                      }}
                      {match a || !b {
                        true => <>or</>,
                        false => <></>,
                      }}
                    </>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  a@v0: Bool,
                  b@v1: Bool,
                ) -> Html {
                  let v4: Bool = match v0 {
                    true => {
                      let v2: Bool = bool_not(v1)
                      v2
                    }
                    false => {
                      let v3: Bool = false
                      v3
                    }
                  }
                  let v8: Html = match v4 {
                    true => {
                      let v5: Html = text("and")
                      let v6: Html = concat(v5)
                      v6
                    }
                    false => {
                      let v7: Html = concat()
                      v7
                    }
                  }
                  let v11: Bool = match v0 {
                    true => {
                      let v9: Bool = true
                      v9
                    }
                    false => {
                      let v10: Bool = bool_not(v1)
                      v10
                    }
                  }
                  let v15: Html = match v11 {
                    true => {
                      let v12: Html = text("or")
                      let v13: Html = concat(v12)
                      v13
                    }
                    false => {
                      let v14: Html = concat()
                      v14
                    }
                  }
                  let v16: Html = concat(v8, v15)
                  v16
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_the_arms_of_a_match_as_written() {
        check(
            indoc! {r#"
                enum Status {
                  Active,
                  Away {since: String},
                }

                page Test(signed_in: Bool, status: Status) {
                  fn body() -> Html {
                    match (signed_in, status) {
                      (true, Status::Away {since}) => <p>{since}</p>,
                      (true, _) => <p>here</p>,
                      (false, _) => <p>guest</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  signed_in@v0: Bool,
                  status@v1: Status,
                ) -> Html {
                  let v2: Tuple[Bool, Status] = (v0, v1)
                  let v13: Html = match v2 {
                    (true, Status::Away {since@v3: String}) => {
                      let v4: Html = escape(v3)
                      let v5: Html = concat(v4)
                      let v6: Html = element("p", {}, v5)
                      v6
                    }
                    (true, _) => {
                      let v7: Html = text("here")
                      let v8: Html = concat(v7)
                      let v9: Html = element("p", {}, v8)
                      v9
                    }
                    (false, _) => {
                      let v10: Html = text("guest")
                      let v11: Html = concat(v10)
                      let v12: Html = element("p", {}, v11)
                      v12
                    }
                  }
                  v13
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_void_element() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <br/>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0() -> Html {
                  let v0: Html = element("br", {})
                  v0
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_record_update_of_variable() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  age: Int,
                }

                page Test(user: User) {
                  fn body() -> Html {
                    <>{(User {...user, name: "Jane"}).name}</>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  user@v0: User,
                ) -> Html {
                  let v1: String = "Jane"
                  let v2: Int = v0.age
                  let v3: User = {name: v1, age: v2}
                  let v4: String = v3.name
                  let v5: Html = escape(v4)
                  let v6: Html = concat(v5)
                  v6
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_record_update_of_expression() {
        check(
            indoc! {r#"
                record State {
                  query: String,
                  num: Int,
                }

                record App {
                  state: State,
                }

                page Test(app: App) {
                  fn body() -> Html {
                    <>{(State {...app.state, num: 1}).query}</>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  app@v0: App,
                ) -> Html {
                  let v1: State = v0.state
                  let v2: String = v1.query
                  let v3: Int = 1
                  let v4: State = {query: v2, num: v3}
                  let v5: String = v4.query
                  let v6: Html = escape(v5)
                  let v7: Html = concat(v6)
                  v7
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_specialize_a_function_for_each_attribute_list_it_is_called_with() {
        check(
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
                render fn Button@f0(
                  id@v8: String,
                ) -> Html {
                  let v9: Html = text("Go")
                  let v10: Html = concat(v9)
                  let v11: Html = element("button", {id: v8}, v10)
                  v11
                }
                render fn Button@f1(
                  class@v12: String,
                  disabled@v13: Bool,
                ) -> Html {
                  let v14: Html = text("Go")
                  let v15: Html = concat(v14)
                  let v16: Html = element("button", {class: v12, disabled: v13}, v15)
                  v16
                }
                entrypoint body@e0() -> Html {
                  let v0: String = "a"
                  let v1: Html = f0(v0)
                  let v2: String = "b"
                  let v3: Bool = true
                  let v4: Html = f1(v2, v3)
                  let v5: String = "c"
                  let v6: Html = f0(v5)
                  let v7: Html = concat(v1, v4, v6)
                  v7
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_forward_a_rest_through_a_function_into_an_element() {
        check(
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
                render fn Card@f1(
                  title@v7: String,
                  id@v8: String,
                  class@v9: String,
                ) -> Html {
                  let v10: Html = escape(v7)
                  let v11: Html = concat(v10)
                  let v12: Html = element("div", {id: v8, class: v9}, v11)
                  v12
                }
                render fn Panel@f0(
                  title@v3: String,
                  class@v4: String,
                ) -> Html {
                  let v5: String = "panel"
                  let v6: Html = f1(v3, v5, v4)
                  v6
                }
                entrypoint body@e0() -> Html {
                  let v0: String = "Hi"
                  let v1: String = "wide"
                  let v2: Html = f0(v0, v1)
                  v2
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_specialize_a_recursive_function_with_a_rest() {
        check(
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
                render fn Nest@f0(
                  depth@v3: Int,
                  id@v4: String,
                ) -> Html {
                  let v5: Int = 0
                  let v6: Bool = int_lt(v5, v3)
                  let v12: Html = match v6 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = int_sub(v3, v7)
                      let v9: String = "inner"
                      let v10: Html = f1(v8, v9)
                      v10
                    }
                    false => {
                      let v11: Html = concat()
                      v11
                    }
                  }
                  let v13: Html = concat(v12)
                  let v14: Html = element("div", {id: v4}, v13)
                  v14
                }
                render fn Nest@f1(
                  depth@v15: Int,
                  class@v16: String,
                ) -> Html {
                  let v17: Int = 0
                  let v18: Bool = int_lt(v17, v15)
                  let v24: Html = match v18 {
                    true => {
                      let v19: Int = 1
                      let v20: Int = int_sub(v15, v19)
                      let v21: String = "inner"
                      let v22: Html = f1(v20, v21)
                      v22
                    }
                    false => {
                      let v23: Html = concat()
                      v23
                    }
                  }
                  let v25: Html = concat(v24)
                  let v26: Html = element("div", {class: v16}, v25)
                  v26
                }
                entrypoint body@e0() -> Html {
                  let v0: Int = 2
                  let v1: String = "outer"
                  let v2: Html = f0(v0, v1)
                  v2
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_name_a_parameter_after_the_attribute_it_receives() {
        check(
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
                render fn Button@f0(
                  data-x@v3: String,
                  aria-label@v4: String,
                ) -> Html {
                  let v5: Html = text("Go")
                  let v6: Html = concat(v5)
                  let v7: Html = element("button", {data-x: v3, aria-label: v4}, v6)
                  v7
                }
                entrypoint body@e0() -> Html {
                  let v0: String = "1"
                  let v1: String = "go"
                  let v2: Html = f0(v0, v1)
                  v2
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_only_the_functions_the_pages_reach() {
        check(
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
                render fn Used@f0() -> Html {
                  let v1: Html = text("used")
                  let v2: Html = concat(v1)
                  let v3: Html = element("p", {}, v2)
                  v3
                }
                entrypoint body@e0() -> Html {
                  let v0: Html = f0()
                  v0
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_the_html_of_a_function_that_returns_a_value() {
        check(
            indoc! {r#"
                fn width(icon: Html, n: Int) -> Int {
                  match n <= 0 {
                    true => 0,
                    false => width(icon, n - 1) + 2,
                  }
                }

                render fn label(icon: Html, name: String) -> String {
                  let badge = <b>{icon}</b>;
                  let n = width(badge, 3);
                  format!("{} {}", name, n.to_string())
                }

                page Test(name: String) {
                  fn body() -> Html {
                    <p>{label(<i>*</i>, name)}</p>
                  }
                }
            "#},
            expect![[r#"
                render fn label@f0(
                  icon@v8: Html,
                  name@v9: String,
                ) -> String {
                  let v10: Html = concat(v8)
                  let v11: Html = element("b", {}, v10)
                  let v12: Int = 3
                  let v13: Int = f1(v11, v12)
                  let v14: String = " "
                  let v15: String = int_to_string(v13)
                  let v16: String = string_concat(v9, v14, v15)
                  v16
                }
                fn width@f1(
                  icon@v17: Html,
                  n@v18: Int,
                ) -> Int {
                  let v19: Int = 0
                  let v20: Bool = int_lt_eq(v18, v19)
                  let v27: Int = match v20 {
                    true => {
                      let v21: Int = 0
                      v21
                    }
                    false => {
                      let v22: Int = 1
                      let v23: Int = int_sub(v18, v22)
                      let v24: Int = f1(v17, v23)
                      let v25: Int = 2
                      let v26: Int = int_add(v24, v25)
                      v26
                    }
                  }
                  v27
                }
                entrypoint body@e0(
                  name@v0: String,
                ) -> Html {
                  let v1: Html = text("*")
                  let v2: Html = concat(v1)
                  let v3: Html = element("i", {}, v2)
                  let v4: String = f0(v3, v0)
                  let v5: Html = escape(v4)
                  let v6: Html = concat(v5)
                  let v7: Html = element("p", {}, v6)
                  v7
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_lambdas_and_their_applications() {
        check(
            indoc! {r#"
                fn Rows(labels: Array[String], row: fn(String) -> Html) -> Html {
                  <ul>
                    {for label in labels {
                      row(label)
                    }}
                  </ul>
                }

                page Test(labels: Array[String], scale: Int) {
                  fn body() -> Html {
                    let times = fn(n: Int) { n * scale };
                    <Rows labels={labels} row={fn(label) {
                      <li>{label}{times(2).to_string()}</li>
                    }}/>
                  }
                }
            "#},
            expect![[r#"
                render fn Rows@f0(
                  labels@v17: Array[String],
                  row@v18: render (String) -> Html,
                ) -> Html {
                  let v21: Html = for v19: String in v17 {
                    let v20: Html = v18(v19)
                    v20
                  }
                  let v22: Html = concat(v21)
                  let v23: Html = element("ul", {}, v22)
                  v23
                }
                lambda l0[
                  v4: Int,
                ](
                  v2: Int,
                ) -> Int {
                  let v3: Int = int_mul(v2, v4)
                  v3
                }
                render lambda l1[
                  v14: (Int) -> Int,
                ](
                  v6: String,
                ) -> Html {
                  let v7: Html = escape(v6)
                  let v8: Int = 2
                  let v9: Int = v14(v8)
                  let v10: String = int_to_string(v9)
                  let v11: Html = escape(v10)
                  let v12: Html = concat(v7, v11)
                  let v13: Html = element("li", {}, v12)
                  v13
                }
                entrypoint body@e0(
                  labels@v0: Array[String],
                  scale@v1: Int,
                ) -> Html {
                  let v5: (Int) -> Int = l0[v1]
                  let v15: render (String) -> Html = l1[v5]
                  let v16: Html = f0(v0, v15)
                  v16
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_a_lambda_that_captures_a_parameter_of_the_lambda_around_it() {
        check(
            indoc! {r#"
                page Test {
                  fn body() -> Html {
                    let add = fn(a: Int) {
                      fn(b: Int) { a + b }
                    };
                    let add_one = add(1);
                    <div>{add_one(2).to_string()}</div>
                  }
                }
            "#},
            expect![[r#"
                lambda l0[
                  v3: Int,
                ](
                  v1: Int,
                ) -> Int {
                  let v2: Int = int_add(v3, v1)
                  v2
                }
                lambda l1[](
                  v0: Int,
                ) -> (Int) -> Int {
                  let v4: (Int) -> Int = l0[v0]
                  v4
                }
                entrypoint body@e0() -> Html {
                  let v5: (Int) -> (Int) -> Int = l1[]
                  let v6: Int = 1
                  let v7: (Int) -> Int = v5(v6)
                  let v8: Int = 2
                  let v9: Int = v7(v8)
                  let v10: String = int_to_string(v9)
                  let v11: Html = escape(v10)
                  let v12: Html = concat(v11)
                  let v13: Html = element("div", {}, v12)
                  v13
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_the_html_lambdas_of_a_function_that_returns_a_value() {
        check(
            indoc! {r#"
                fn width(icon: fn() -> Html, scale: fn(Int) -> Int, n: Int) -> Int {
                  let wrap = fn(content: Html) { <b>{content}{icon()}</b> };
                  match n <= 0 {
                    true => scale(2),
                    false => width(fn() { wrap(<i>*</i>) }, scale, n - 1),
                  }
                }

                page Test(n: Int) {
                  fn body() -> Html {
                    <p>{width(fn() { <i>*</i> }, fn(m) { m + n }, 3).to_string()}</p>
                  }
                }
            "#},
            expect![[r#"
                fn width@f0(
                  icon@v15: render () -> Html,
                  scale@v16: (Int) -> Int,
                  n@v17: Int,
                ) -> Int {
                  let v23: render (Html) -> Html = l2[v15]
                  let v24: Int = 0
                  let v25: Bool = int_lt_eq(v17, v24)
                  let v37: Int = match v25 {
                    true => {
                      let v26: Int = 2
                      let v27: Int = v16(v26)
                      v27
                    }
                    false => {
                      let v33: render () -> Html = l3[v23]
                      let v34: Int = 1
                      let v35: Int = int_sub(v17, v34)
                      let v36: Int = f0(v33, v16, v35)
                      v36
                    }
                  }
                  v37
                }
                render lambda l0[]() -> Html {
                  let v1: Html = text("*")
                  let v2: Html = concat(v1)
                  let v3: Html = element("i", {}, v2)
                  v3
                }
                lambda l1[
                  v7: Int,
                ](
                  v5: Int,
                ) -> Int {
                  let v6: Int = int_add(v5, v7)
                  v6
                }
                render lambda l2[
                  v22: render () -> Html,
                ](
                  v18: Html,
                ) -> Html {
                  let v19: Html = v22()
                  let v20: Html = concat(v18, v19)
                  let v21: Html = element("b", {}, v20)
                  v21
                }
                render lambda l3[
                  v32: render (Html) -> Html,
                ]() -> Html {
                  let v28: Html = text("*")
                  let v29: Html = concat(v28)
                  let v30: Html = element("i", {}, v29)
                  let v31: Html = v32(v30)
                  v31
                }
                entrypoint body@e0(
                  n@v0: Int,
                ) -> Html {
                  let v4: render () -> Html = l0[]
                  let v8: (Int) -> Int = l1[v0]
                  let v9: Int = 3
                  let v10: Int = f0(v4, v8, v9)
                  let v11: String = int_to_string(v10)
                  let v12: Html = escape(v11)
                  let v13: Html = concat(v12)
                  let v14: Html = element("p", {}, v13)
                  v14
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_the_html_of_a_block_that_produces_an_html_lambda_for_a_value() {
        check(
            indoc! {r#"
                fn weigh(icon: Html, n: Int) -> Int {
                  match n <= 0 {
                    true => 0,
                    false => weigh(icon, n - 1) + 2,
                  }
                }

                fn count(mark: fn() -> Html, n: Int) -> Int {
                  match n <= 0 {
                    true => 0,
                    false => count(mark, n - 1) + 1,
                  }
                }

                render fn label(star: Html, name: String) -> String {
                  let mark = {
                    let badge = <b>{star}</b>;
                    let weight = weigh(badge, 1);
                    fn() { <i>{weight.to_string()}</i> }
                  };
                  format!("{} {}", name, count(mark, 3).to_string())
                }

                page Test(name: String) {
                  fn body() -> Html {
                    <p>{label(<i>*</i>, name)}</p>
                  }
                }
            "#},
            expect![[r#"
                fn count@f2(
                  mark@v36: render () -> Html,
                  n@v37: Int,
                ) -> Int {
                  let v38: Int = 0
                  let v39: Bool = int_lt_eq(v37, v38)
                  let v46: Int = match v39 {
                    true => {
                      let v40: Int = 0
                      v40
                    }
                    false => {
                      let v41: Int = 1
                      let v42: Int = int_sub(v37, v41)
                      let v43: Int = f2(v36, v42)
                      let v44: Int = 1
                      let v45: Int = int_add(v43, v44)
                      v45
                    }
                  }
                  v46
                }
                render fn label@f0(
                  star@v8: Html,
                  name@v9: String,
                ) -> String {
                  let v10: Html = concat(v8)
                  let v11: Html = element("b", {}, v10)
                  let v12: Int = 1
                  let v13: Int = f1(v11, v12)
                  let v19: render () -> Html = l0[v13]
                  let v20: String = " "
                  let v21: Int = 3
                  let v22: Int = f2(v19, v21)
                  let v23: String = int_to_string(v22)
                  let v24: String = string_concat(v9, v20, v23)
                  v24
                }
                fn weigh@f1(
                  icon@v25: Html,
                  n@v26: Int,
                ) -> Int {
                  let v27: Int = 0
                  let v28: Bool = int_lt_eq(v26, v27)
                  let v35: Int = match v28 {
                    true => {
                      let v29: Int = 0
                      v29
                    }
                    false => {
                      let v30: Int = 1
                      let v31: Int = int_sub(v26, v30)
                      let v32: Int = f1(v25, v31)
                      let v33: Int = 2
                      let v34: Int = int_add(v32, v33)
                      v34
                    }
                  }
                  v35
                }
                render lambda l0[
                  v18: Int,
                ]() -> Html {
                  let v14: String = int_to_string(v18)
                  let v15: Html = escape(v14)
                  let v16: Html = concat(v15)
                  let v17: Html = element("i", {}, v16)
                  v17
                }
                entrypoint body@e0(
                  name@v0: String,
                ) -> Html {
                  let v1: Html = text("*")
                  let v2: Html = concat(v1)
                  let v3: Html = element("i", {}, v2)
                  let v4: String = f0(v3, v0)
                  let v5: Html = escape(v4)
                  let v6: Html = concat(v5)
                  let v7: Html = element("p", {}, v6)
                  v7
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_keep_the_html_of_an_arm_that_computes_a_value() {
        check(
            indoc! {r#"
                fn width(icon: Html, n: Int) -> Int {
                  match n <= 0 {
                    true => 0,
                    false => width(icon, n - 1) + 2,
                  }
                }

                page Test(flag: Bool) {
                  fn body() -> Html {
                    <p>
                      {match flag {
                        true => {
                          let star = <i>*</i>;
                          width(star, 1).to_string()
                        },
                        false => "none",
                      }}
                    </p>
                  }
                }
            "#},
            expect![[r#"
                fn width@f0(
                  icon@v12: Html,
                  n@v13: Int,
                ) -> Int {
                  let v14: Int = 0
                  let v15: Bool = int_lt_eq(v13, v14)
                  let v22: Int = match v15 {
                    true => {
                      let v16: Int = 0
                      v16
                    }
                    false => {
                      let v17: Int = 1
                      let v18: Int = int_sub(v13, v17)
                      let v19: Int = f0(v12, v18)
                      let v20: Int = 2
                      let v21: Int = int_add(v19, v20)
                      v21
                    }
                  }
                  v22
                }
                entrypoint body@e0(
                  flag@v0: Bool,
                ) -> Html {
                  let v8: String = match v0 {
                    true => {
                      let v1: Html = text("*")
                      let v2: Html = concat(v1)
                      let v3: Html = element("i", {}, v2)
                      let v4: Int = 1
                      let v5: Int = f0(v3, v4)
                      let v6: String = int_to_string(v5)
                      v6
                    }
                    false => {
                      let v7: String = "none"
                      v7
                    }
                  }
                  let v9: Html = escape(v8)
                  let v10: Html = concat(v9)
                  let v11: Html = element("p", {}, v10)
                  v11
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_forward_children_through_a_rest_and_read_html_once_in_each_arm() {
        check(
            indoc! {r#"
                fn Callout(open: Bool, children: Html) -> Html {
                  match open {
                    true => <aside>{children}</aside>,
                    false => <details>{children}</details>,
                  }
                }

                fn Tip(...rest) -> Html {
                  <Callout ...rest/>
                }

                page Test(open: Bool) {
                  fn body() -> Html {
                    <Tip open={open}>
                      <b>x</b>
                    </Tip>
                  }
                }
            "#},
            expect![[r#"
                render fn Callout@f1(
                  open@v9: Bool,
                  children@v10: Html,
                ) -> Html {
                  let v15: Html = match v9 {
                    true => {
                      let v11: Html = concat(v10)
                      let v12: Html = element("aside", {}, v11)
                      v12
                    }
                    false => {
                      let v13: Html = concat(v10)
                      let v14: Html = element("details", {}, v13)
                      v14
                    }
                  }
                  v15
                }
                render fn Tip@f0(
                  open@v6: Bool,
                  children@v7: Html,
                ) -> Html {
                  let v8: Html = f1(v6, v7)
                  v8
                }
                entrypoint body@e0(
                  open@v0: Bool,
                ) -> Html {
                  let v1: Html = text("x")
                  let v2: Html = concat(v1)
                  let v3: Html = element("b", {}, v2)
                  let v4: Html = concat(v3)
                  let v5: Html = f0(v0, v4)
                  v5
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_cells_clicks_and_keys() {
        check(
            indoc! {r#"
                record Item {
                  id: Int,
                  label: String,
                }
                fn Counter(label: String) -> Html {
                  let (count, set_count) = state!(0);
                  <button on:click={mut fn() { set_count(fn(old) { old + 1 }) }}>
                    {label} {count.to_string()}
                  </button>
                }
                page Test() {
                  fn body() -> Html {
                    let items = [Item {id: 1, label: "a"}, Item {id: 2, label: "b"}];
                    <ul>
                      {for item in items key item.id {
                        <li><Counter label={item.label}/></li>
                      }}
                    </ul>
                  }
                }
            "#},
            expect![[r#"
                render fn Counter@f0(
                  label@v16: String,
                ) -> Html {
                  let v18: Tuple[Int, mut ((Int) -> Int) -> Tuple[]] = cell {
                    let v17: Int = 0
                    v17
                  }
                  let v19: Int = tuple_index(v18, 0)
                  let v20: mut ((Int) -> Int) -> Tuple[] = tuple_index(v18, 1)
                  let v27: mut () -> Tuple[] = l1[v20]
                  let v28: Html = escape(v16)
                  let v29: Html = text(" ")
                  let v30: String = int_to_string(v19)
                  let v31: Html = escape(v30)
                  let v32: Html = concat(v28, v29, v31)
                  let v33: Html = element("button", {}, v32) on_click v27
                  v33
                }
                lambda l0[](
                  v21: Int,
                ) -> Int {
                  let v22: Int = 1
                  let v23: Int = int_add(v21, v22)
                  v23
                }
                mut lambda l1[
                  v26: mut ((Int) -> Int) -> Tuple[],
                ]() -> Tuple[] {
                  let v24: (Int) -> Int = l0[]
                  let v25: Tuple[] = v26(v24)
                  v25
                }
                entrypoint body@e0() -> Html {
                  let v0: Int = 1
                  let v1: String = "a"
                  let v2: Item = {id: v0, label: v1}
                  let v3: Int = 2
                  let v4: String = "b"
                  let v5: Item = {id: v3, label: v4}
                  let v6: Array[Item] = [v2, v5]
                  let v13: Html = for v7: Item in v6 key {
                    let v8: Int = v7.id
                    v8
                  } {
                    let v9: String = v7.label
                    let v10: Html = f0(v9)
                    let v11: Html = concat(v10)
                    let v12: Html = element("li", {}, v11)
                    v12
                  }
                  let v14: Html = concat(v13)
                  let v15: Html = element("ul", {}, v14)
                  v15
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_pass_a_handler_to_an_element_through_a_parameter() {
        check(
            indoc! {r#"
                fn Button(on_press: mut fn() -> Tuple[], children: Html) -> Html {
                  <button on:click={on_press}>{children}</button>
                }
                page Test() {
                  fn body() -> Html {
                    let (on, set_on) = state!(false);
                    <Button on_press={mut fn() { set_on(fn(old) { !old }) }}>
                      {match on {true => "on", false => "off"}}
                    </Button>
                  }
                }
            "#},
            expect![[r#"
                render fn Button@f0(
                  on_press@v16: mut () -> Tuple[],
                  children@v17: Html,
                ) -> Html {
                  let v18: Html = concat(v17)
                  let v19: Html = element("button", {}, v18) on_click v16
                  v19
                }
                lambda l0[](
                  v4: Bool,
                ) -> Bool {
                  let v5: Bool = bool_not(v4)
                  v5
                }
                mut lambda l1[
                  v8: mut ((Bool) -> Bool) -> Tuple[],
                ]() -> Tuple[] {
                  let v6: (Bool) -> Bool = l0[]
                  let v7: Tuple[] = v8(v6)
                  v7
                }
                entrypoint body@e0() -> Html {
                  let v1: Tuple[Bool, mut ((Bool) -> Bool) -> Tuple[]] = cell {
                    let v0: Bool = false
                    v0
                  }
                  let v2: Bool = tuple_index(v1, 0)
                  let v3: mut ((Bool) -> Bool) -> Tuple[] = tuple_index(v1, 1)
                  let v9: mut () -> Tuple[] = l1[v3]
                  let v12: String = match v2 {
                    true => {
                      let v10: String = "on"
                      v10
                    }
                    false => {
                      let v11: String = "off"
                      v11
                    }
                  }
                  let v13: Html = escape(v12)
                  let v14: Html = concat(v13)
                  let v15: Html = f0(v9, v14)
                  v15
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_a_let_pattern_to_reads_of_the_elements_and_fields_it_names() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                page Test(user: User) {
                  fn body() -> Html {
                    let (count, _) = state!(0);
                    let ((n, _), User {name: _, age: a}) = ((count, true), user);
                    <p>{n.to_string()}{a.to_string()}</p>
                  }
                }
            "#},
            expect![[r#"
                entrypoint body@e0(
                  user@v0: User,
                ) -> Html {
                  let v2: Tuple[Int, mut ((Int) -> Int) -> Tuple[]] = cell {
                    let v1: Int = 0
                    v1
                  }
                  let v3: Int = tuple_index(v2, 0)
                  let v4: Bool = true
                  let v5: Tuple[Int, Bool] = (v3, v4)
                  let v6: Tuple[Tuple[Int, Bool], User] = (v5, v0)
                  let v7: Tuple[Int, Bool] = tuple_index(v6, 0)
                  let v8: Int = tuple_index(v7, 0)
                  let v9: User = tuple_index(v6, 1)
                  let v10: Int = v9.age
                  let v11: String = int_to_string(v8)
                  let v12: Html = escape(v11)
                  let v13: String = int_to_string(v10)
                  let v14: Html = escape(v13)
                  let v15: Html = concat(v12, v14)
                  let v16: Html = element("p", {}, v15)
                  v16
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_a_cell_in_a_block_that_returns_a_lambda_that_renders_html() {
        check(
            indoc! {r#"
                page Test {
                  fn body() -> Html {
                    let row = {
                      let (shown, set_shown) = state!(false);
                      fn(label: String) {
                        <li on:click={mut fn() { set_shown(fn(_) { !shown }) }}>{label}</li>
                      }
                    };
                    <ul>{row("a")}{row("b")}</ul>
                  }
                }
            "#},
            expect![[r#"
                lambda l0[
                  v7: Bool,
                ](
                  v5: Bool,
                ) -> Bool {
                  let v6: Bool = bool_not(v7)
                  v6
                }
                mut lambda l1[
                  v10: Bool,
                  v11: mut ((Bool) -> Bool) -> Tuple[],
                ]() -> Tuple[] {
                  let v8: (Bool) -> Bool = l0[v10]
                  let v9: Tuple[] = v11(v8)
                  v9
                }
                render lambda l2[
                  v16: Bool,
                  v17: mut ((Bool) -> Bool) -> Tuple[],
                ](
                  v4: String,
                ) -> Html {
                  let v12: mut () -> Tuple[] = l1[v16, v17]
                  let v13: Html = escape(v4)
                  let v14: Html = concat(v13)
                  let v15: Html = element("li", {}, v14) on_click v12
                  v15
                }
                entrypoint body@e0() -> Html {
                  let v1: Tuple[Bool, mut ((Bool) -> Bool) -> Tuple[]] = cell {
                    let v0: Bool = false
                    v0
                  }
                  let v2: Bool = tuple_index(v1, 0)
                  let v3: mut ((Bool) -> Bool) -> Tuple[] = tuple_index(v1, 1)
                  let v18: render (String) -> Html = l2[v2, v3]
                  let v19: String = "a"
                  let v20: Html = v18(v19)
                  let v21: String = "b"
                  let v22: Html = v18(v21)
                  let v23: Html = concat(v20, v22)
                  let v24: Html = element("ul", {}, v23)
                  v24
                }
                page Test: e0
            "#]],
        );
    }

    #[test]
    fn should_compile_mut_functions_assignments_and_reads_of_cells_in_what_mutates() {
        check(
            indoc! {r#"
                mut fn twice(press: mut fn() -> Tuple[]) -> Tuple[] {
                  press();
                  press()
                }
                page Test() {
                  fn body() -> Html {
                    let (count, set_count) = state!(0);
                    let rendered = count;
                    <button on:click={mut fn() {
                      twice(mut fn() { set_count(fn(old) { old + rendered }) });
                      match count > 9 {true => set_count(fn(_) { 0 }), false => ()}
                    }}>{count.to_string()}</button>
                  }
                }
            "#},
            expect![[r#"
                mut fn twice@f0(
                  press@v28: mut () -> Tuple[],
                ) -> Tuple[] {
                  let v29: Tuple[] = v28()
                  let v30: Tuple[] = v28()
                  v30
                }
                lambda l0[
                  v6: Int,
                ](
                  v4: Int,
                ) -> Int {
                  let v5: Int = int_add(v4, v6)
                  v5
                }
                mut lambda l1[
                  v9: Int,
                  v10: mut ((Int) -> Int) -> Tuple[],
                ]() -> Tuple[] {
                  let v7: (Int) -> Int = l0[v9]
                  let v8: Tuple[] = v10(v7)
                  v8
                }
                lambda l2[](
                  v15: Int,
                ) -> Int {
                  let v16: Int = 0
                  v16
                }
                mut lambda l3[
                  v21: Int,
                  v22: mut ((Int) -> Int) -> Tuple[],
                ]() -> Tuple[] {
                  let v11: mut () -> Tuple[] = l1[v21, v22]
                  let v12: Tuple[] = f0(v11)
                  let v13: Int = 9
                  let v14: Bool = int_lt(v13, v21)
                  let v20: Tuple[] = match v14 {
                    true => {
                      let v17: (Int) -> Int = l2[]
                      let v18: Tuple[] = v22(v17)
                      v18
                    }
                    false => {
                      let v19: Tuple[] = ()
                      v19
                    }
                  }
                  v20
                }
                entrypoint body@e0() -> Html {
                  let v1: Tuple[Int, mut ((Int) -> Int) -> Tuple[]] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v2: Int = tuple_index(v1, 0)
                  let v3: mut ((Int) -> Int) -> Tuple[] = tuple_index(v1, 1)
                  let v23: mut () -> Tuple[] = l3[v2, v3]
                  let v24: String = int_to_string(v2)
                  let v25: Html = escape(v24)
                  let v26: Html = concat(v25)
                  let v27: Html = element("button", {}, v26) on_click v23
                  v27
                }
                page Test: e0
            "#]],
        );
    }
}
