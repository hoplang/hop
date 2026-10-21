use indexmap::IndexMap;
use std::sync::Arc;

use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::CheapString;
use crate::hop::typing::{
    Constructor, DataType, Pattern, PatternField, Type, TypedAttribute, TypedExpr,
    TypedFunctionDeclaration, TypedLoopSource, TypedPageDeclaration, TypedParameter, TypedPattern,
    TypedRecordUpdateField,
};
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatFunctionDeclaration, FlatModule, FlatOp};
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::HashMap;

use super::ir_page::IrPage;
use super::ir_parameter::IrParameter;

/// Compile the pages and the functions they reach.
///
/// Flat has no pages. A page compiles to an entrypoint function for its head, if
/// it has one, and one for its body, and the pages that point at them are
/// returned beside the module.
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
pub fn typed_to_flat(
    pages: Vec<TypedPageDeclaration>,
    source_functions: &[(&RootContainedFilePath, &TypedFunctionDeclaration)],
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
) -> (FlatModule, Vec<IrPage>) {
    let mut value_ids = ValueIdCounter::new();
    let mut function_ids = FunctionIdCounter::new();

    // Each declaration with its position, since the module keeps the
    // functions in declaration order.
    let source: HashMap<(RootContainedFilePath, FunctionName), (usize, &TypedFunctionDeclaration)> =
        source_functions
            .iter()
            .enumerate()
            .map(|(index, (module, decl))| (((*module).clone(), decl.name.clone()), (index, *decl)))
            .collect();

    let mut compiler = Compiler::new(&mut value_ids, &mut function_ids, asset_path_rewriter);

    let pages: Vec<CompiledPage> = pages
        .into_iter()
        .map(|page| compiler.compile_page_decl(page))
        .collect();

    // Compiling a body requests the specializations it calls, so this runs
    // until every request is compiled.
    let mut functions: Vec<(usize, FlatFunctionDeclaration)> = Vec::new();
    let mut next = 0;
    while let Some(request) = compiler.specializations.get(next).cloned() {
        let (index, decl) = source[&(request.module, request.name)];
        let function = compiler.compile_function_decl(decl, request.shape, request.function);
        functions.push((index, function));
        next += 1;
    }
    functions.sort_by_key(|(index, _)| *index);
    let mut functions: Vec<FlatFunctionDeclaration> =
        functions.into_iter().map(|(_, decl)| decl).collect();

    // The functions of the pages come last, numbered after every function
    // a page reaches.
    let pages = pages
        .into_iter()
        .map(|page| page.declare(&mut function_ids, &mut functions))
        .collect();

    (
        FlatModule {
            functions,
            value_ids,
        },
        pages,
    )
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
    head: Option<(Vec<IrParameter>, FlatBlock)>,
    body: (Vec<IrParameter>, FlatBlock),
}

impl CompiledPage {
    /// Declare the head and the body as entrypoint functions, appended to
    /// `functions`, and point the page at them.
    fn declare(
        self,
        function_ids: &mut FunctionIdCounter,
        functions: &mut Vec<FlatFunctionDeclaration>,
    ) -> IrPage {
        let mut declare = |function: IrFunction, (parameters, body)| {
            functions.push(FlatFunctionDeclaration {
                function: function.clone(),
                entrypoint: true,
                parameters,
                return_type: Type::Html,
                body,
            });
            function
        };
        let head = self
            .head
            .map(|head| declare(page_function(function_ids, "head"), head));
        let body = declare(page_function(function_ids, "body"), self.body);
        IrPage {
            name: self.name,
            head,
            body,
        }
    }
}

/// A function that renders a page member, named after the member.
fn page_function(function_ids: &mut FunctionIdCounter, member: &str) -> IrFunction {
    let name = FunctionName::new(CheapString::new(member.to_string()))
        .expect("a page member name is a valid function name");
    IrFunction::new(function_ids.next(), name)
}

struct Compiler<'a> {
    value_id_counter: &'a mut ValueIdCounter,
    function_id_counter: &'a mut FunctionIdCounter,
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
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
}

impl<'a> Compiler<'a> {
    fn new(
        value_id_counter: &'a mut ValueIdCounter,
        function_id_counter: &'a mut FunctionIdCounter,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> Self {
        Compiler {
            value_id_counter,
            function_id_counter,
            specializations: Vec::new(),
            scopes: vec![Vec::new()],
            params: HashMap::new(),
            rest: Vec::new(),
            bindings: Vec::new(),
            asset_path_rewriter,
        }
    }

    fn compile_function_decl(
        &mut self,
        decl: &TypedFunctionDeclaration,
        shape: Vec<(AttributeName, Type)>,
        function: IrFunction,
    ) -> FlatFunctionDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len() + shape.len());
        for param in &decl.params {
            let var = self.bind(&param.var_name);
            self.params.insert(param.var_name.clone(), var);
            parameters.push(IrParameter {
                name: param.var_name.clone().into(),
                binder: IrBinder {
                    var,
                    typ: param.var_type.clone(),
                },
            });
        }
        // Each attribute the specialization receives through its rest is a
        // parameter of its own, after the declared ones and in the order of
        // the shape, as a call passes them.
        for (name, typ) in shape {
            let var = self.value_id_counter.next();
            self.rest.push((name.clone(), typ.clone(), var));
            parameters.push(IrParameter {
                name,
                binder: IrBinder { var, typ },
            });
        }

        let declaration = FlatFunctionDeclaration {
            function,
            entrypoint: false,
            parameters,
            return_type: decl.return_type.clone(),
            body: self.compile_block(&decl.body),
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

    /// Compile the head or the body of a page as the body of a function
    /// that declares the page's parameters, each with a name of its own.
    fn compile_page_member(
        &mut self,
        params: &[TypedParameter],
        expr: &TypedExpr,
    ) -> (Vec<IrParameter>, FlatBlock) {
        self.push_scope();
        let parameters = params
            .iter()
            .map(|param| IrParameter {
                name: param.var_name.clone().into(),
                binder: IrBinder {
                    var: self.bind(&param.var_name),
                    typ: param.var_type.clone(),
                },
            })
            .collect();
        let body = self.compile_block(expr);
        self.pop_scope();
        (parameters, body)
    }

    /// Compile `expr` as a block of its own, holding the bindings of its
    /// parts outside any nested block.
    fn compile_block(&mut self, expr: &TypedExpr) -> FlatBlock {
        self.block(|cx| cx.compile_expr(expr))
    }

    /// The block of the bindings `f` pushes, whose result is the name `f`
    /// returns.
    fn block(&mut self, f: impl FnOnce(&mut Self) -> ValueId) -> FlatBlock {
        let outer = std::mem::take(&mut self.bindings);
        let result = f(self);
        FlatBlock {
            bindings: std::mem::replace(&mut self.bindings, outer),
            result,
        }
    }

    /// Bind `op` to a fresh name in the block being compiled, and return
    /// the name.
    fn push(&mut self, typ: Type, op: FlatOp) -> ValueId {
        let name = self.value_id_counter.next();
        self.bindings.push(FlatBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope stack should not be empty");
    }

    fn bind(&mut self, name: &VarName) -> ValueId {
        let id = self.value_id_counter.next();
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
            .push((name.clone(), id));
        id
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
    fn compile_pattern(&mut self, pattern: &TypedPattern) -> IrPattern {
        match pattern {
            Pattern::Wildcard => Pattern::Wildcard,
            Pattern::Binding(binder) => Pattern::Binding(IrBinder {
                var: self.bind(&binder.var),
                typ: binder.typ.clone(),
            }),
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
    /// the name of its value.
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
            TypedExpr::NumericNegation { operand, .. } => {
                FlatOp::Data(IrDataOp::NumericNegation(self.compile_expr(operand)))
            }
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
            TypedExpr::Equals { left, right } => FlatOp::Data(IrDataOp::Equals(
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::NotEquals { left, right } => {
                // Desugar NotEquals into BoolNegation(Equals(...))
                let equals = FlatOp::Data(IrDataOp::Equals(
                    self.compile_expr(left),
                    self.compile_expr(right),
                ));
                FlatOp::Data(IrDataOp::BoolNegation(self.push(Type::BOOL, equals)))
            }
            TypedExpr::LessThan { left, right } => FlatOp::Data(IrDataOp::LessThan(
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            // Convert a > b to b < a
            TypedExpr::GreaterThan { left, right } => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                FlatOp::Data(IrDataOp::LessThan(right, left))
            }
            TypedExpr::LessThanOrEqual { left, right } => FlatOp::Data(IrDataOp::LessThanOrEqual(
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            // Convert a >= b to b <= a
            TypedExpr::GreaterThanOrEqual { left, right } => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                FlatOp::Data(IrDataOp::LessThanOrEqual(right, left))
            }
            // The right operand runs only when the left one is true, so it
            // becomes the true arm. The false arm is false.
            TypedExpr::BoolLogicalAnd { left, right, .. } => FlatOp::Match(IrMatch {
                subject: self.compile_expr(left),
                arms: vec![
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolTrue,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.compile_block(right),
                    },
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolFalse,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.block(|cx| {
                            cx.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(false)))
                        }),
                    },
                ],
            }),
            // The right operand runs only when the left one is false, so it
            // becomes the false arm. The true arm is true.
            TypedExpr::BoolLogicalOr { left, right, .. } => FlatOp::Match(IrMatch {
                subject: self.compile_expr(left),
                arms: vec![
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolTrue,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.block(|cx| {
                            cx.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(true)))
                        }),
                    },
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::BoolFalse,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: self.compile_block(right),
                    },
                ],
            }),
            TypedExpr::NumericAdd { left, right, .. } => FlatOp::Data(IrDataOp::NumericAdd(
                self.compile_expr(left),
                self.compile_expr(right),
            )),
            TypedExpr::NumericSubtract { left, right, .. } => FlatOp::Data(
                IrDataOp::NumericSubtract(self.compile_expr(left), self.compile_expr(right)),
            ),
            TypedExpr::NumericMultiply { left, right, .. } => FlatOp::Data(
                IrDataOp::NumericMultiply(self.compile_expr(left), self.compile_expr(right)),
            ),
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
                let arms = arms
                    .iter()
                    .map(|(pattern, body)| {
                        self.push_scope();
                        let pattern = self.compile_pattern(pattern);
                        let body = self.compile_block(body);
                        self.pop_scope();
                        IrMatchArm { pattern, body }
                    })
                    .collect();
                FlatOp::Match(IrMatch { subject, arms })
            }
            TypedExpr::Option { value, .. } => FlatOp::Data(IrDataOp::Option(
                value.as_ref().map(|v| self.compile_expr(v)),
            )),
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
                FlatOp::HtmlConcat(compiled)
            }
            TypedExpr::HtmlText { value } => FlatOp::HtmlText(value.clone()),
            TypedExpr::HtmlEscape { expr } => {
                assert_eq!(
                    expr.typ(),
                    Type::STRING,
                    "HtmlEscape must hold a String, but holds {expr}"
                );
                FlatOp::HtmlEscape(self.compile_expr(expr))
            }
            TypedExpr::Element {
                element,
                attrs,
                children,
            } => {
                let mut attributes =
                    IndexMap::with_capacity(attrs.attributes.len() + self.rest.len());
                for (name, attr) in &attrs.attributes {
                    let attribute = match attr {
                        TypedAttribute::Value(value) => {
                            IrAttribute::Value(self.compile_expr(value))
                        }
                        TypedAttribute::Presence(present) => {
                            IrAttribute::Presence(self.compile_expr(present))
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
                            Type::BOOL => IrAttribute::Presence(value),
                            _ => IrAttribute::Value(value),
                        };
                        let previous = attributes.insert(name, attribute);
                        assert!(previous.is_none(), "an attribute is set twice");
                    }
                }
                FlatOp::HtmlElement(IrHtmlElement {
                    element: element.clone(),
                    attributes,
                    children: self.compile_expr(children),
                })
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
                FlatOp::Call(function, compiled_args)
            }
            // A let binds nothing. Its variable is the name of its value,
            // in a scope of its own that holds the body.
            TypedExpr::Let {
                var, value, body, ..
            } => {
                let value = self.compile_expr(value);
                self.scopes.push(vec![(var.clone(), value)]);
                let body = self.compile_expr(body);
                self.pop_scope();
                return body;
            }
            TypedExpr::For {
                var_name,
                source,
                body,
                ..
            } => {
                assert_eq!(
                    typ,
                    Type::Html,
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
                        Type::Data(*element_type)
                    }
                    TypedLoopSource::RangeInclusive { .. } => Type::INT,
                };
                self.push_scope();
                let var = var_name.as_ref().map(|name| IrBinder {
                    var: self.bind(name),
                    typ: element_type,
                });
                let body = self.compile_block(body);
                self.pop_scope();
                FlatOp::HtmlFor(IrFor { var, array, body })
            }
            TypedExpr::ArrayLength { array } => {
                FlatOp::Data(IrDataOp::ArrayLength(self.compile_expr(array)))
            }
            TypedExpr::ArrayIsEmpty { array } => {
                FlatOp::Data(IrDataOp::ArrayIsEmpty(self.compile_expr(array)))
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
                let subject = self.compile_expr(option);
                let binding = self.value_id_counter.next();
                FlatOp::Match(IrMatch {
                    subject,
                    arms: vec![
                        IrMatchArm {
                            pattern: Pattern::Constructor {
                                constructor: Constructor::OptionSome,
                                args: vec![Pattern::Binding(IrBinder {
                                    var: binding,
                                    typ: typ.clone(),
                                })],
                                fields: Vec::new(),
                            },
                            body: self.block(|_| binding),
                        },
                        IrMatchArm {
                            pattern: Pattern::Constructor {
                                constructor: Constructor::OptionNone,
                                args: Vec::new(),
                                fields: Vec::new(),
                            },
                            body: self.compile_block(default),
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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::document::Document;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_flat};
    use crate::program::Program;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    /// Compile a module written in source, without optimization.
    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (module, pages) = orchestrate_flat(
            program.typed_modules(),
            OrchestrateOptions {
                ..Default::default()
            },
        );
        let pages: String = pages.iter().map(|page| format!("{page}\n")).collect();
        expected.assert_eq(&format!("{module}{pages}"));
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
                entrypoint fn head@f0() -> Html {
                  let v0: Html = text("Hi")
                  let v1: Html = concat(v0)
                  let v2: Html = element("title", {}, v1)
                  v2
                }
                entrypoint fn body@f1() -> Html {
                  let v3: Html = text("Hello World")
                  let v4: Html = concat(v3)
                  v4
                }
                page Test: head@f0, body@f1
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
                entrypoint fn body@f0(name@v0: String) -> Html {
                  let v1: Html = text("Hello ")
                  let v2: Html = escape(v0)
                  let v3: Html = concat(v1, v2)
                  v3
                }
                page Test: body@f0
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
                entrypoint fn body@f0() -> Html {
                  let v0: Html = text("Content")
                  let v1: Html = concat(v0)
                  let v2: Html = element("div", {}, v1)
                  v2
                }
                page Test: body@f0
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
                entrypoint fn body@f0(items@v0: Array[String]) -> Html {
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
                page Test: body@f0
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
                entrypoint fn body@f0(items@v0: Array[String], name@v1: Option[String]) -> Html {
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
                page Test: body@f0
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
                entrypoint fn body@f0() -> Html {
                  let v0: String = "base"
                  let v1: String = "test"
                  let v2: Html = text("Content")
                  let v3: Html = concat(v2)
                  let v4: Html = element("div", {class: v0, id: v1}, v3)
                  v4
                }
                page Test: body@f0
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
                entrypoint fn body@f0(cls@v0: String) -> Html {
                  let v1: String = "base"
                  let v2: Html = text("Content")
                  let v3: Html = concat(v2)
                  let v4: Html = element("div", {class: v1, data-value: v0}, v3)
                  v4
                }
                page Test: body@f0
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
                entrypoint fn body@f0(flag@v0: Bool) -> Html {
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
                page Test: body@f0
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
                entrypoint fn body@f0(a@v0: Bool, b@v1: Bool) -> Html {
                  let v4: Bool = match v0 {
                    true => {
                      let v2: Bool = !v1
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
                      let v10: Bool = !v1
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
                page Test: body@f0
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
                entrypoint fn body@f0(signed_in@v0: Bool, status@v1: Status) -> Html {
                  let v2: (Bool, Status) = (v0, v1)
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
                page Test: body@f0
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
                entrypoint fn body@f0() -> Html {
                  let v0: Html = concat()
                  let v1: Html = element("br", {}, v0)
                  v1
                }
                page Test: body@f0
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
                entrypoint fn body@f0(user@v0: User) -> Html {
                  let v1: String = "Jane"
                  let v2: Int = v0.age
                  let v3: User = {name: v1, age: v2}
                  let v4: String = v3.name
                  let v5: Html = escape(v4)
                  let v6: Html = concat(v5)
                  v6
                }
                page Test: body@f0
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
                entrypoint fn body@f0(app@v0: App) -> Html {
                  let v1: State = v0.state
                  let v2: String = v1.query
                  let v3: Int = 1
                  let v4: State = {query: v2, num: v3}
                  let v5: String = v4.query
                  let v6: Html = escape(v5)
                  let v7: Html = concat(v6)
                  v7
                }
                page Test: body@f0
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
                fn Button@f0(id@v8: String) -> Html {
                  let v9: Html = text("Go")
                  let v10: Html = concat(v9)
                  let v11: Html = element("button", {id: v8}, v10)
                  v11
                }
                fn Button@f1(class@v12: String, disabled@v13: Bool) -> Html {
                  let v14: Html = text("Go")
                  let v15: Html = concat(v14)
                  let v16: Html = element("button", {class: v12, disabled: v13}, v15)
                  v16
                }
                entrypoint fn body@f2() -> Html {
                  let v0: String = "a"
                  let v1: Html = Button@f0(v0)
                  let v2: String = "b"
                  let v3: Bool = true
                  let v4: Html = Button@f1(v2, v3)
                  let v5: String = "c"
                  let v6: Html = Button@f0(v5)
                  let v7: Html = concat(v1, v4, v6)
                  v7
                }
                page Test: body@f2
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
                fn Card@f1(title@v7: String, id@v8: String, class@v9: String) -> Html {
                  let v10: Html = escape(v7)
                  let v11: Html = concat(v10)
                  let v12: Html = element("div", {id: v8, class: v9}, v11)
                  v12
                }
                fn Panel@f0(title@v3: String, class@v4: String) -> Html {
                  let v5: String = "panel"
                  let v6: Html = Card@f1(v3, v5, v4)
                  v6
                }
                entrypoint fn body@f2() -> Html {
                  let v0: String = "Hi"
                  let v1: String = "wide"
                  let v2: Html = Panel@f0(v0, v1)
                  v2
                }
                page Test: body@f2
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
                fn Nest@f0(depth@v3: Int, id@v4: String) -> Html {
                  let v5: Int = 0
                  let v6: Bool = v5 < v3
                  let v12: Html = match v6 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = v3 - v7
                      let v9: String = "inner"
                      let v10: Html = Nest@f1(v8, v9)
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
                fn Nest@f1(depth@v15: Int, class@v16: String) -> Html {
                  let v17: Int = 0
                  let v18: Bool = v17 < v15
                  let v24: Html = match v18 {
                    true => {
                      let v19: Int = 1
                      let v20: Int = v15 - v19
                      let v21: String = "inner"
                      let v22: Html = Nest@f1(v20, v21)
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
                entrypoint fn body@f2() -> Html {
                  let v0: Int = 2
                  let v1: String = "outer"
                  let v2: Html = Nest@f0(v0, v1)
                  v2
                }
                page Test: body@f2
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
                fn Button@f0(data-x@v3: String, aria-label@v4: String) -> Html {
                  let v5: Html = text("Go")
                  let v6: Html = concat(v5)
                  let v7: Html = element("button", {data-x: v3, aria-label: v4}, v6)
                  v7
                }
                entrypoint fn body@f1() -> Html {
                  let v0: String = "1"
                  let v1: String = "go"
                  let v2: Html = Button@f0(v0, v1)
                  v2
                }
                page Test: body@f1
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
                fn Used@f0() -> Html {
                  let v1: Html = text("used")
                  let v2: Html = concat(v1)
                  let v3: Html = element("p", {}, v2)
                  v3
                }
                entrypoint fn body@f1() -> Html {
                  let v0: Html = Used@f0()
                  v0
                }
                page Test: body@f1
            "#]],
        );
    }
}
