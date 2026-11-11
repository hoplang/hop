use std::collections::{HashMap, HashSet};

use crate::hop::typing::{
    CaseVar, Decision, FieldBinding, Pattern, Variable, compile_decision_tree,
};
use crate::html::write_escaped_html;
use crate::ir::document_shell::DocumentShell;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_program::{
    FlatEntrypoint, FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram,
    FlatPureBlock, FlatPureOp,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::ir_page::IrPage;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_switch::{EnumMatchArm, EnumPattern, IrSwitch};
use crate::ir::ir_type::{
    IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrResultType, IrSignature,
    IrType,
};
use crate::ir::ir_type_declarations::IrTypeDeclarations;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::ir::writer_program::{
    WriterBlock, WriterFunctionBody, WriterOp, WriterPageDeclaration, WriterProgram, WriterStmt,
};
use crate::symbols::type_name::TypeName;

/// Lower a Flat program to the Writer.
///
/// Values stay lets, in their order. Html is written where it is read, as a
/// part of a concat, the children of an element, the result of a block or
/// the argument of a call for an Html parameter. Html is read at most once
/// on each path and never from a loop body it is bound outside of, so Html
/// read once in each arm of a match is written in each, and Html that
/// nothing reads is not written at all. An Html parameter takes a closure
/// without parameters that writes the Html, and the function applies it
/// where it reads the parameter, which it does at most once on each path
/// and never in a loop, so the Html is never held. The Html a call passes
/// for an Html parameter becomes such a closure, except Html that only
/// applies one, which passes that closure on. A string constant or concat
/// that only escapes and attribute values read is escaped now, part by
/// part.
///
/// Html is never part of another type and a function that returns a value
/// has no Html parameter, so no value reads Html. Panics if one does all the
/// same.
///
/// A click never happens on a page rendered to a string, so the clicks are
/// left out.
///
/// Every closure is a lambda or a parameter, since no match, call or
/// application produces one. Such a closure outlives everything that reads
/// it, so a backend can have every closure borrow the names it captures. A
/// lambda becomes a let at the start of its block, with the body a function
/// that writes or returns would have. Its Html parameters take closures, as
/// those of a function do.
///
/// A match compiles to a decision tree. A switch of the tree becomes a
/// Writer switch on the part of the subject it tests, a record or tuple the
/// tree takes apart becomes field accesses and tuple indexes, and a leaf
/// becomes the arm it selects, reading the parts its pattern binds. A record
/// or tuple the program constructs is taken apart by reading the operands of
/// its constructor, which is not bound at all when only matches read it and
/// none of their arms binds it as a whole.
///
/// An arm that several leaves select is lowered once for each of them, and
/// Html written in several arms is lowered once for each. Every copy after
/// the first binds fresh names, so that the program binds every name once.
///
/// The Flat IR has no pages, so the pages come beside the program, each
/// pointing at the entrypoints that render it. Their bodies are written into
/// the page. With a shell, a page writes a whole document, the
/// shell around its head and its body. Without one, a page writes its head
/// and its body alone.
pub fn flat_to_writer(
    program: FlatProgram,
    pages: &[IrPage],
    shell: Option<&DocumentShell>,
) -> WriterProgram {
    let FlatProgram {
        types,
        functions,
        entrypoints,
        value_ids,
    } = program;
    let mut lowerer = Lowerer {
        types: &types,
        value_ids,
        bindings: HashMap::new(),
        html: HashMap::new(),
        html_parameters: HashSet::new(),
        binder_types: HashMap::new(),
        readers: HashMap::new(),
        aliases: HashMap::new(),
        bound: HashSet::new(),
    };
    for function in &functions {
        for param in &function.parameters() {
            lowerer.analyze_parameter(&param.binder);
        }
        match function {
            FlatFunction::Pure(function) => lowerer.analyze_pure_block(&function.body),
            FlatFunction::Html(function) => lowerer.analyze_html_block(&function.body),
        }
    }
    for entrypoint in entrypoints.iter() {
        for param in &entrypoint.parameters {
            lowerer.analyze_parameter(&param.widen().binder);
        }
        lowerer.analyze_html_block(&entrypoint.body);
    }
    let mut page_entrypoints: HashMap<EntrypointId, _> = entrypoints.iter_enumerated().collect();
    let pages = pages
        .iter()
        .map(|page| {
            let mut take = |id: &EntrypointId| {
                page_entrypoints
                    .remove(id)
                    .expect("each page points at entrypoints of its own")
            };
            let head = page.head.as_ref().map(&mut take);
            let body = take(&page.body);
            lowerer.lower_page(page.name.clone(), head, body, shell)
        })
        .collect();
    let functions = functions
        .iter()
        .map(|decl| lowerer.lower_function(decl))
        .collect();
    WriterProgram {
        types,
        pages,
        functions,
    }
}

/// The longest a constant write grows by merging with the write before it,
/// which keeps the string literals in generated code short.
const WRITE_LIMIT: usize = 60;

/// Append a constant write, merged into the write before it while the
/// combined length stays below the limit.
fn write(out: &mut Vec<WriterStmt>, content: &str) {
    if let Some(WriterStmt::Write(previous)) = out.last_mut() {
        if previous.len() + content.len() < WRITE_LIMIT {
            previous.push_str(content);
            return;
        }
    }
    out.push(WriterStmt::Write(content.to_string()));
}

/// The Writer type of a parameter, where Html is a closure without
/// parameters that writes it, in the signatures of closures as well, an
/// effect is the unit tuple and a cell is its value.
fn parameter_type(typ: &IrType) -> IrType {
    match typ {
        IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => IrType::html_function([]),
        IrType::Html(IrFunctionHtmlType::Function(signature)) => {
            IrType::html_function(signature.parameters.iter().map(parameter_type))
        }
        IrType::Data(data) => IrType::Data(value_type(data)),
    }
}

fn value_type(typ: &IrFunctionDataType) -> IrFunctionDataType {
    match typ {
        IrFunctionDataType::Data(_) => typ.clone(),
        IrFunctionDataType::Effect(_) => IrFunctionDataType::Data(IrDataType::UNIT),
        IrFunctionDataType::Cell(data) => IrFunctionDataType::Data(data.clone()),
        IrFunctionDataType::Function(signature) => IrFunctionDataType::Function(IrSignature {
            parameters: signature.parameters.iter().map(value_type).collect(),
            result: IrResultType::Data(result_type(&signature.result)),
        }),
    }
}

fn result_type(typ: &IrResultType) -> IrDataType {
    match typ {
        IrResultType::Data(data) => data.clone(),
        IrResultType::Effect(_) => IrDataType::UNIT,
    }
}

/// What reads a name.
#[derive(Clone, Copy)]
enum Reader {
    /// An escape or an attribute value. A constant String read here is
    /// escaped now.
    Escaped,
    /// A part of the named String concat. Folded along with the concat.
    ConcatPart(ValueId),
    /// The subject of a match that no arm binds as a whole. A tuple or
    /// record constructed here is read through its operands, if at all.
    TakenApart,
    /// Anything else.
    Value,
}

fn html_bindings(
    block: &FlatHtmlBlock,
) -> impl Iterator<Item = &IrBinding<IrFunctionHtmlType, FlatHtmlOp>> {
    block.bindings.iter().filter_map(|binding| match binding {
        FlatHtmlBinding::Pure(_) | FlatHtmlBinding::Cell(..) | FlatHtmlBinding::Track(..) => None,
        FlatHtmlBinding::Html(binding) => Some(binding),
    })
}

struct Lowerer<'a> {
    types: &'a IrTypeDeclarations,
    value_ids: ValueIdCounter,
    /// Every Pure binding of the program, by name and by the fresh name of
    /// each copy of it. Names are unique, so one map serves every block.
    bindings: HashMap<ValueId, &'a IrBinding<IrFunctionDataType, FlatPureOp>>,
    /// Every Html binding of the program, by name, to write it where it is
    /// read.
    html: HashMap<ValueId, &'a IrBinding<IrFunctionHtmlType, FlatHtmlOp>>,
    /// The Html parameters of every function and lambda, which take a
    /// closure that writes the Html. Html is never part of another type, so
    /// these are the only names of Html that no binding binds.
    html_parameters: HashSet<ValueId>,
    /// The type of each parameter, loop variable, binding of a pattern of
    /// data, cell and track, none of which is in `bindings`.
    binder_types: HashMap<ValueId, IrDataType>,
    /// What reads each name, once for each read.
    readers: HashMap<ValueId, Vec<Reader>>,
    /// The name a name is read through, in place of its own: the result of
    /// a match, the fresh name of a copy, the part of the subject a binding
    /// of a pattern binds in the leaf lowered last, and the parameter of the
    /// page's body for a parameter of its head.
    aliases: HashMap<ValueId, ValueId>,
    /// The names of the lets and loops lowered so far, so that a copy binds
    /// fresh ones.
    bound: HashSet<ValueId>,
}

impl<'a> Lowerer<'a> {
    fn read(&mut self, name: ValueId, reader: Reader) {
        self.readers.entry(name).or_default().push(reader);
    }

    /// Whether the name is Html.
    fn is_html(&self, name: ValueId) -> bool {
        self.html_parameters.contains(&name)
            || (self.html.get(&name))
                .is_some_and(|binding| binding.binder.typ == IrFunctionHtmlType::Html(IrHtmlType))
    }

    // An Html parameter takes a closure, and the type of a parameter of data
    // is what a match on it needs. An effect is the unit tuple.
    fn analyze_parameter(&mut self, binder: &IrBinder<IrType>) {
        match &binder.typ {
            IrType::Data(IrFunctionDataType::Data(typ)) => {
                self.binder_types.insert(binder.var, typ.clone());
            }
            IrType::Data(IrFunctionDataType::Effect(_)) => {
                self.binder_types.insert(binder.var, IrDataType::UNIT);
            }
            IrType::Data(IrFunctionDataType::Cell(typ)) => {
                self.binder_types.insert(binder.var, typ.clone());
            }
            IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                self.html_parameters.insert(binder.var);
            }
            IrType::Data(IrFunctionDataType::Function(_))
            | IrType::Html(IrFunctionHtmlType::Function(_)) => {}
        }
    }

    fn read_args(&mut self, args: &[ValueId]) {
        for arg in args {
            if !self.is_html(*arg) {
                self.read(*arg, Reader::Value);
            }
        }
    }

    /// Record that a match reads its subject, as a whole or taken apart.
    fn read_subject<B>(&mut self, match_: &IrMatch<B>) {
        let bound_whole = match_
            .arms
            .iter()
            .any(|arm| matches!(arm.pattern, Pattern::Binding(_)));
        let reader = if bound_whole {
            Reader::Value
        } else {
            Reader::TakenApart
        };
        self.read(match_.subject, reader);
    }

    /// Record the binding and what it reads. Panics if it reads Html.
    fn analyze_pure_binding(&mut self, binding: &'a IrBinding<IrFunctionDataType, FlatPureOp>) {
        binding.op.for_each_operand(&mut |operand| {
            assert!(
                !self.is_html(operand),
                "{} reads the Html of {operand} as a value",
                binding.binder.var
            );
        });
        self.bindings.insert(binding.binder.var, binding);
        match &binding.op {
            FlatPureOp::Data(IrDataOp::StringConcat(parts)) => {
                for part in parts {
                    self.read(*part, Reader::ConcatPart(binding.binder.var));
                }
            }
            FlatPureOp::Data(op) => {
                op.for_each_operand(&mut |operand| self.read(operand, Reader::Value));
            }
            FlatPureOp::Call(_, args) => {
                for arg in args {
                    self.read(*arg, Reader::Value);
                }
            }
            FlatPureOp::Match(match_) => {
                self.read_subject(match_);
                for arm in &match_.arms {
                    self.analyze_pattern(&arm.pattern);
                    self.analyze_pure_block(&arm.body);
                }
            }
            // Rendered on the empty state, an effect performs nothing, so
            // it reads nothing.
            FlatPureOp::CellWrite(..)
            | FlatPureOp::CellRead(_)
            | FlatPureOp::EffectPure(_)
            | FlatPureOp::EffectBind(..) => {}
            FlatPureOp::Lambda(lambda) => {
                for param in &lambda.parameters {
                    self.analyze_parameter(&IrBinder {
                        var: param.var,
                        typ: IrType::Data(param.typ.clone()),
                    });
                }
                self.analyze_pure_block(&lambda.body);
            }
            FlatPureOp::Apply(closure, args) => {
                self.read(*closure, Reader::Value);
                self.read_args(args);
            }
        }
    }

    fn analyze_pure_block(&mut self, block: &'a FlatPureBlock) {
        for binding in &block.bindings {
            self.analyze_pure_binding(binding);
        }
        self.read(block.result, Reader::Value);
    }

    /// Record the bindings of the block and what they read. Its Html is
    /// recorded first, so that a value that reads it is found.
    fn analyze_html_block(&mut self, block: &'a FlatHtmlBlock) {
        for binding in html_bindings(block) {
            self.html.insert(binding.binder.var, binding);
        }
        for binding in &block.bindings {
            match binding {
                FlatHtmlBinding::Pure(binding) => self.analyze_pure_binding(binding),
                FlatHtmlBinding::Cell(binder, initial) => {
                    let IrFunctionDataType::Cell(typ) = &binder.typ else {
                        unreachable!("a cell binding binds a cell");
                    };
                    self.binder_types.insert(binder.var, typ.clone());
                    self.read(*initial, Reader::Value);
                }
                FlatHtmlBinding::Track(binder, cell) => {
                    self.binder_types.insert(binder.var, binder.typ.clone());
                    self.read(*cell, Reader::Value);
                }
                FlatHtmlBinding::Html(_) => {}
            }
        }
        for binding in html_bindings(block) {
            match &binding.op {
                FlatHtmlOp::Text(_) | FlatHtmlOp::Concat(_) => {}
                FlatHtmlOp::Escape(string) => self.read(*string, Reader::Escaped),
                FlatHtmlOp::Element(IrHtmlElement { attributes, .. }) => {
                    for attribute in attributes.values() {
                        match attribute {
                            IrAttribute::Value(value) => {
                                self.read(*value, Reader::Escaped);
                            }
                            IrAttribute::Presence(present) => {
                                self.read(*present, Reader::Value);
                            }
                            IrAttribute::Constant(_) => {}
                        }
                    }
                }
                FlatHtmlOp::Match(match_) => {
                    self.read_subject(match_);
                    for arm in &match_.arms {
                        self.analyze_pattern(&arm.pattern);
                        self.analyze_html_block(&arm.body);
                    }
                }
                FlatHtmlOp::For(FlatFor {
                    var, array, body, ..
                }) => {
                    if let Some(binder) = var {
                        self.analyze_binder(binder);
                    }
                    self.read(*array, Reader::Value);
                    self.analyze_html_block(body);
                }
                FlatHtmlOp::Call(_, args) => self.read_args(args),
                FlatHtmlOp::Lambda(lambda) => {
                    for param in &lambda.parameters {
                        self.analyze_parameter(param);
                    }
                    self.analyze_html_block(&lambda.body);
                }
                FlatHtmlOp::Apply(closure, args) => {
                    self.read(*closure, Reader::Value);
                    self.read_args(args);
                }
            }
        }
    }

    /// Record the types of the names a pattern binds.
    fn analyze_pattern(&mut self, pattern: &IrPattern) {
        pattern.for_each_binding(&mut |binder| self.analyze_binder(binder));
    }

    /// Record the type of a name a loop or a pattern binds.
    fn analyze_binder(&mut self, binder: &IrBinder<IrDataType>) {
        self.binder_types.insert(binder.var, binder.typ.clone());
    }

    /// Whether the binding is written or folded where it is read, rather
    /// than bound to its name: a string constant or concat that only
    /// escapes, attribute values and folded concats read, or a tuple or
    /// record that only matches read, none of them binding it as a whole.
    fn folded(&self, name: ValueId) -> bool {
        let (Some(readers), Some(binding)) = (self.readers.get(&name), self.bindings.get(&name))
        else {
            return false;
        };
        match binding.op {
            FlatPureOp::Data(IrDataOp::StringLiteral(_) | IrDataOp::StringConcat(_)) => {
                readers.iter().all(|reader| match reader {
                    Reader::Escaped => true,
                    Reader::ConcatPart(concat) => self.folded(*concat),
                    Reader::TakenApart | Reader::Value => false,
                })
            }
            FlatPureOp::Data(IrDataOp::Tuple(_) | IrDataOp::Record(..)) => readers
                .iter()
                .all(|reader| matches!(reader, Reader::TakenApart)),
            _ => false,
        }
    }

    /// The Writer name for reading a name.
    fn name(&self, name: ValueId) -> ValueId {
        self.aliases.get(&name).copied().unwrap_or(name)
    }

    /// The name a let or a loop binds in place of `name`: `name` itself the
    /// first time, and after that a fresh one, which the copy reads in its
    /// place and which names the binding of `name`, if it has one.
    fn bind(&mut self, name: ValueId) -> ValueId {
        if self.bound.insert(name) {
            return name;
        }
        let fresh = self.value_ids.next();
        if let Some(binding) = self.bindings.get(&name).copied() {
            self.bindings.insert(fresh, binding);
        }
        self.aliases.insert(name, fresh);
        fresh
    }

    /// Lower a page from the entrypoints it points at. The page takes the
    /// body's parameters, and the head reads them in place of its own.
    fn lower_page(
        &mut self,
        name: TypeName,
        head: Option<&'a FlatEntrypoint>,
        body: &'a FlatEntrypoint,
        shell: Option<&DocumentShell>,
    ) -> WriterPageDeclaration {
        let head = head.map(|head| {
            for (from, to) in head.parameters.iter().zip(&body.parameters) {
                self.aliases.insert(from.binder.var, to.binder.var);
            }
            &head.body
        });
        let mut statements = Vec::new();
        match shell {
            Some(shell) => {
                statements.push(WriterStmt::Write(shell.before_head.to_string()));
                if let Some(head) = head {
                    self.write_block(head, &mut statements);
                }
                statements.push(WriterStmt::Write(shell.after_head.clone()));
                self.write_block(&body.body, &mut statements);
                statements.push(WriterStmt::Write(shell.after_body.to_string()));
            }
            None => {
                if let Some(head) = head {
                    self.write_block(head, &mut statements);
                }
                self.write_block(&body.body, &mut statements);
            }
        }
        WriterPageDeclaration {
            name,
            parameters: body.parameters.iter().map(|param| param.widen()).collect(),
            body: statements,
        }
    }

    /// Lower a function declaration, choosing the calling convention from
    /// its kind. Html compiles to destination passing, a value to an
    /// ordinary value returning function.
    fn lower_function(
        &mut self,
        decl: &'a FlatFunction,
    ) -> IrFunctionDeclaration<IrType, IrType, WriterFunctionBody> {
        match decl {
            FlatFunction::Html(function) => {
                let mut statements = Vec::new();
                self.write_block(&function.body, &mut statements);
                IrFunctionDeclaration {
                    function: function.function.clone(),
                    parameters: (function.parameters.iter())
                        .map(|param| IrParameter {
                            name: param.name.clone(),
                            binder: IrBinder {
                                var: param.binder.var,
                                typ: parameter_type(&param.binder.typ),
                            },
                        })
                        .collect(),
                    return_type: IrType::HTML,
                    body: WriterFunctionBody::Writes(statements),
                }
            }
            FlatFunction::Pure(function) => {
                let mut bindings = Vec::new();
                self.lower_pure_bindings(&function.body.bindings, &mut bindings);
                IrFunctionDeclaration {
                    function: function.function.clone(),
                    parameters: (decl.parameters().into_iter())
                        .map(|param| IrParameter {
                            name: param.name,
                            binder: IrBinder {
                                var: param.binder.var,
                                typ: parameter_type(&param.binder.typ),
                            },
                        })
                        .collect(),
                    return_type: IrType::Data(IrFunctionDataType::Data(result_type(
                        &function.return_type,
                    ))),
                    body: WriterFunctionBody::Returns(WriterBlock {
                        bindings,
                        result: self.name(function.body.result),
                    }),
                }
            }
        }
    }

    /// Lower the bindings to lets, folding the results of matches into their
    /// readers and leaving out the bindings folded where they are read.
    fn lower_pure_bindings(
        &mut self,
        bindings: &'a [IrBinding<IrFunctionDataType, FlatPureOp>],
        out: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) {
        for binding in bindings {
            self.lower_pure_binding(binding, out);
        }
    }

    fn lower_pure_binding(
        &mut self,
        binding: &'a IrBinding<IrFunctionDataType, FlatPureOp>,
        out: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) {
        if self.folded(binding.binder.var) {
            return;
        }
        let op = match &binding.op {
            FlatPureOp::Match(match_) => {
                let typ = value_type(&binding.binder.typ);
                let alias = self.lower_data_match(match_, &typ, out);
                self.aliases.insert(binding.binder.var, alias);
                return;
            }
            FlatPureOp::CellWrite(..)
            | FlatPureOp::CellRead(_)
            | FlatPureOp::EffectPure(_)
            | FlatPureOp::EffectBind(..) => WriterOp::Data(IrDataOp::Tuple(Vec::new())),
            FlatPureOp::Call(function, args) => WriterOp::Call(
                function.clone(),
                args.iter().map(|arg| self.name(*arg)).collect(),
            ),
            FlatPureOp::Data(op) => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |name| *name = self.name(*name));
                WriterOp::Data(op)
            }
            FlatPureOp::Lambda(lambda) => {
                let parameters = self.bind_parameters(&lambda.parameters);
                let mut bindings = Vec::new();
                self.lower_pure_bindings(&lambda.body.bindings, &mut bindings);
                let body = WriterFunctionBody::Returns(WriterBlock {
                    bindings,
                    result: self.name(lambda.body.result),
                });
                WriterOp::Lambda(IrLambda {
                    parameters: (parameters.into_iter())
                        .map(|param| IrBinder {
                            var: param.var,
                            typ: IrType::Data(value_type(&param.typ)),
                        })
                        .collect(),
                    body,
                })
            }
            FlatPureOp::Apply(closure, args) => WriterOp::Apply(
                self.name(*closure),
                args.iter().map(|arg| self.name(*arg)).collect(),
            ),
        };
        out.push(IrBinding {
            binder: IrBinder {
                var: self.bind(binding.binder.var),
                typ: IrType::Data(value_type(&binding.binder.typ)),
            },
            op,
        });
    }

    // The parameters of a lambda, binding fresh names in a copy.
    fn bind_parameters<T: Clone>(&mut self, parameters: &[IrBinder<T>]) -> Vec<IrBinder<T>> {
        (parameters.iter())
            .map(|param| IrBinder {
                var: self.bind(param.var),
                typ: param.typ.clone(),
            })
            .collect()
    }

    /// The names a call passes. The Html for an Html parameter becomes a
    /// closure that writes it, bound by a let appended to `lets`, except Html
    /// that only applies a closure without parameters, which passes that
    /// closure on.
    fn lower_args(
        &mut self,
        args: &[ValueId],
        lets: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) -> Vec<ValueId> {
        let mut names = Vec::new();
        for arg in args {
            if !self.is_html(*arg) {
                names.push(self.name(*arg));
                continue;
            }
            let mut body = Vec::new();
            self.write_html(*arg, &mut body);
            let name = match body.as_slice() {
                [WriterStmt::WriteApply(closure, args)] if args.is_empty() => *closure,
                _ => {
                    let name = self.value_ids.next();
                    lets.push(IrBinding {
                        binder: IrBinder {
                            var: name,
                            typ: IrType::html_function([]),
                        },
                        op: WriterOp::Lambda(IrLambda {
                            parameters: Vec::new(),
                            body: WriterFunctionBody::Writes(body),
                        }),
                    });
                    name
                }
            };
            names.push(name);
        }
        names
    }

    /// The decision tree of a match, the case variables its leaves use and
    /// the parts of the subject bound before the tree, which is the subject
    /// itself.
    fn compile_match<B>(
        &self,
        match_: &IrMatch<B>,
    ) -> (
        Decision<IrBinder<IrDataType>, IrDataType>,
        HashSet<CaseVar>,
        HashMap<CaseVar, ValueId>,
    ) {
        let patterns: Vec<_> = match_.arms.iter().map(|arm| arm.pattern.clone()).collect();
        let subject_type = match self.bindings.get(&match_.subject) {
            Some(binding) => {
                let IrFunctionDataType::Data(typ) = &binding.binder.typ else {
                    unreachable!("a match subject is data");
                };
                typ.clone()
            }
            None => self.binder_types[&match_.subject].clone(),
        };
        let decision = compile_decision_tree(self.types, &patterns, subject_type);
        let mut used = HashSet::new();
        decision.collect_used(&mut used);
        let parts = HashMap::from([(CaseVar(0), self.name(match_.subject))]);
        (decision, used, parts)
    }

    /// Bind the fields of a record that a switch of the tree takes apart
    /// and that are in `used`, with the lets that read them appended to
    /// `lets`. A record the program constructs is taken apart through the
    /// operands of its constructor.
    fn take_apart_record(
        &mut self,
        record: ValueId,
        fields: &[FieldBinding<IrDataType>],
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        lets: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) {
        let constructed = match self.bindings.get(&record).copied() {
            Some(IrBinding {
                op: FlatPureOp::Data(IrDataOp::Record(operands)),
                ..
            }) => Some(operands),
            _ => None,
        };
        for field in fields {
            if !used.contains(&field.var.id) {
                continue;
            }
            let part = match constructed {
                Some(constructed) => {
                    let (_, value) = constructed
                        .iter()
                        .find(|(name, _)| *name == field.field_name)
                        .expect("a record has the fields of its type");
                    self.name(*value)
                }
                None => {
                    let name = self.value_ids.next();
                    lets.push(IrBinding {
                        binder: IrBinder {
                            var: name,
                            typ: IrType::Data(IrFunctionDataType::Data(field.var.typ.clone())),
                        },
                        op: WriterOp::Data(IrDataOp::FieldAccess(record, field.field_name.clone())),
                    });
                    name
                }
            };
            parts.insert(field.var.id, part);
        }
    }

    /// Bind the elements of a tuple that a switch of the tree takes apart
    /// and that are in `used`, with the lets that read them appended to
    /// `lets`. A tuple the program constructs is taken apart through the
    /// operands of its constructor.
    fn take_apart_tuple(
        &mut self,
        tuple: ValueId,
        elements: &[Variable<IrDataType>],
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        lets: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) {
        let constructed = match self.bindings.get(&tuple).copied() {
            Some(IrBinding {
                op: FlatPureOp::Data(IrDataOp::Tuple(operands)),
                ..
            }) => Some(operands),
            _ => None,
        };
        for (index, element) in elements.iter().enumerate() {
            if !used.contains(&element.id) {
                continue;
            }
            let part = match constructed {
                Some(constructed) => self.name(constructed[index]),
                None => {
                    let name = self.value_ids.next();
                    lets.push(IrBinding {
                        binder: IrBinder {
                            var: name,
                            typ: IrType::Data(IrFunctionDataType::Data(element.typ.clone())),
                        },
                        op: WriterOp::TupleIndex(tuple, index),
                    });
                    name
                }
            };
            parts.insert(element.id, part);
        }
    }

    /// The name of the result of a value match of type `typ`, with the lets
    /// that compute it appended to `lets`.
    fn lower_data_match(
        &mut self,
        match_: &'a IrMatch<FlatPureBlock>,
        typ: &IrFunctionDataType,
        lets: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) -> ValueId {
        let (decision, used, mut parts) = self.compile_match(match_);
        self.lower_data_decision(&decision, &match_.arms, typ, &used, &mut parts, lets)
    }

    /// The name of the value a decision tree of a match of type `typ`
    /// selects from the arms, with the lets that compute it appended to
    /// `lets`. `parts` holds the name of the part of the subject each case
    /// variable bound so far stands for. A case variable that is not in
    /// `used` is not bound.
    fn lower_data_decision(
        &mut self,
        decision: &Decision<IrBinder<IrDataType>, IrDataType>,
        arms: &'a [IrMatchArm<FlatPureBlock>],
        typ: &IrFunctionDataType,
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        lets: &mut Vec<IrBinding<IrType, WriterOp>>,
    ) -> ValueId {
        let switch = match decision {
            Decision::Success(body) => {
                for binding in &body.bindings {
                    self.aliases
                        .insert(binding.binder.var, parts[&binding.source]);
                }
                let block = &arms[body.value].body;
                self.lower_pure_bindings(&block.bindings, lets);
                return self.name(block.result);
            }

            Decision::SwitchRecord { variable, case } => {
                let record = parts[&variable.id];
                self.take_apart_record(record, &case.bindings, used, parts, lets);
                return self.lower_data_decision(&case.body, arms, typ, used, parts, lets);
            }

            Decision::SwitchTuple { variable, case } => {
                let tuple = parts[&variable.id];
                self.take_apart_tuple(tuple, &case.elements, used, parts, lets);
                return self.lower_data_decision(&case.body, arms, typ, used, parts, lets);
            }

            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                let mut true_lets = Vec::new();
                let true_result = self.lower_data_decision(
                    &true_case.body,
                    arms,
                    typ,
                    used,
                    parts,
                    &mut true_lets,
                );
                let mut false_lets = Vec::new();
                let false_result = self.lower_data_decision(
                    &false_case.body,
                    arms,
                    typ,
                    used,
                    parts,
                    &mut false_lets,
                );
                IrSwitch::Bool {
                    subject: parts[&variable.id],
                    true_body: WriterBlock {
                        bindings: true_lets,
                        result: true_result,
                    },
                    false_body: WriterBlock {
                        bindings: false_lets,
                        result: false_result,
                    },
                }
            }

            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let some_arm_binding =
                    self.bind_part(some_case.var.id, &some_case.var.typ, used, parts);
                let mut some_lets = Vec::new();
                let some_result = self.lower_data_decision(
                    &some_case.body,
                    arms,
                    typ,
                    used,
                    parts,
                    &mut some_lets,
                );
                let mut none_lets = Vec::new();
                let none_result = self.lower_data_decision(
                    &none_case.body,
                    arms,
                    typ,
                    used,
                    parts,
                    &mut none_lets,
                );
                IrSwitch::Option {
                    subject: parts[&variable.id],
                    some_arm_binding,
                    some_arm_body: WriterBlock {
                        bindings: some_lets,
                        result: some_result,
                    },
                    none_arm_body: WriterBlock {
                        bindings: none_lets,
                        result: none_result,
                    },
                }
            }

            Decision::SwitchEnum { variable, cases } => {
                let mut enum_arms = Vec::new();
                for case in cases {
                    let mut bindings = Vec::new();
                    for binding in &case.bindings {
                        if let Some(binder) =
                            self.bind_part(binding.var.id, &binding.var.typ, used, parts)
                        {
                            bindings.push((binding.field_name.clone(), binder));
                        }
                    }
                    let mut case_lets = Vec::new();
                    let result = self.lower_data_decision(
                        &case.body,
                        arms,
                        typ,
                        used,
                        parts,
                        &mut case_lets,
                    );
                    enum_arms.push(EnumMatchArm {
                        pattern: EnumPattern::Variant {
                            type_name: case.type_name.clone(),
                            variant_name: case.variant_name.clone(),
                        },
                        bindings,
                        body: WriterBlock {
                            bindings: case_lets,
                            result,
                        },
                    });
                }
                IrSwitch::Enum {
                    subject: parts[&variable.id],
                    arms: enum_arms,
                }
            }
        };
        let name = self.value_ids.next();
        lets.push(IrBinding {
            binder: IrBinder {
                var: name,
                typ: IrType::Data(typ.clone()),
            },
            op: WriterOp::Match(switch),
        });
        name
    }

    /// The variable for a part of the subject that an arm of a switch holds.
    /// A used part is bound to a fresh name, which becomes the name of
    /// the part.
    fn bind_part(
        &mut self,
        part: CaseVar,
        typ: &IrDataType,
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
    ) -> Option<IrBinder<IrDataType>> {
        if !used.contains(&part) {
            return None;
        }
        let binder = self.value_ids.next();
        parts.insert(part, binder);
        Some(IrBinder {
            var: binder,
            typ: typ.clone(),
        })
    }

    /// Write a block: the lets of its values, then those of its closures,
    /// then its result.
    fn write_block(&mut self, block: &'a FlatHtmlBlock, out: &mut Vec<WriterStmt>) {
        let mut lets = Vec::new();
        for binding in &block.bindings {
            match binding {
                FlatHtmlBinding::Pure(binding) => {
                    self.lower_pure_binding(binding, &mut lets);
                }
                FlatHtmlBinding::Cell(binder, initial) => {
                    let initial = self.name(*initial);
                    self.aliases.insert(binder.var, initial);
                }
                FlatHtmlBinding::Track(binder, cell) => {
                    let cell = self.name(*cell);
                    self.aliases.insert(binder.var, cell);
                }
                FlatHtmlBinding::Html(_) => {}
            }
        }
        out.extend(lets.into_iter().map(WriterStmt::Let));
        for binding in html_bindings(block) {
            let FlatHtmlOp::Lambda(lambda) = &binding.op else {
                continue;
            };
            let parameters = (self.bind_parameters(&lambda.parameters).into_iter())
                .map(|param| IrBinder {
                    var: param.var,
                    typ: parameter_type(&param.typ),
                })
                .collect();
            let mut statements = Vec::new();
            self.write_block(&lambda.body, &mut statements);
            out.push(WriterStmt::Let(IrBinding {
                binder: IrBinder {
                    var: self.bind(binding.binder.var),
                    typ: parameter_type(&IrType::Html(binding.binder.typ.clone())),
                },
                op: WriterOp::Lambda(IrLambda {
                    parameters,
                    body: WriterFunctionBody::Writes(statements),
                }),
            }));
        }
        self.write_html(block.result, out);
    }

    /// Write a String name escaped. A folded constant is escaped now, and a
    /// folded concat part by part, so only what varies is escaped when the
    /// page renders.
    fn write_escaped(&mut self, name: ValueId, out: &mut Vec<WriterStmt>) {
        if !self.folded(name) {
            out.push(WriterStmt::WriteString(self.name(name)));
            return;
        }
        match &self.bindings[&name].op {
            FlatPureOp::Data(IrDataOp::StringLiteral(value)) => {
                let mut content = String::new();
                write_escaped_html(value.as_str(), &mut content);
                write(out, &content);
            }
            FlatPureOp::Data(IrDataOp::StringConcat(parts)) => {
                for part in parts {
                    self.write_escaped(*part, out);
                }
            }
            op => unreachable!("a folded string is a constant or a concat, not {op:?}"),
        }
    }

    /// Write the Html that `html` names: an application of the closure an
    /// Html parameter takes, or the writes of the op of its binding.
    fn write_html(&mut self, html: ValueId, out: &mut Vec<WriterStmt>) {
        if self.html_parameters.contains(&html) {
            out.push(WriterStmt::WriteApply(self.name(html), Vec::new()));
            return;
        }
        let binding = *self
            .html
            .get(&html)
            .unwrap_or_else(|| panic!("{html} is read as Html but is not Html"));
        match &binding.op {
            FlatHtmlOp::Text(text) => write(out, text.as_str()),

            FlatHtmlOp::Escape(string) => self.write_escaped(*string, out),

            FlatHtmlOp::Concat(parts) => {
                for part in parts {
                    self.write_html(*part, out);
                }
            }

            FlatHtmlOp::Element(element) => self.write_element(element, out),

            FlatHtmlOp::Match(match_) => {
                let (decision, used, mut parts) = self.compile_match(match_);
                self.write_decision(&decision, &match_.arms, &used, &mut parts, out);
            }

            FlatHtmlOp::For(FlatFor {
                var,
                array,
                body,
                key: _,
            }) => {
                let array = self.name(*array);
                let var = var.as_ref().map(|var| IrBinder {
                    var: self.bind(var.var),
                    typ: var.typ.clone(),
                });
                let mut statements = Vec::new();
                self.write_block(body, &mut statements);
                out.push(WriterStmt::HtmlFor(IrFor {
                    var,
                    array,
                    body: statements,
                }));
            }

            FlatHtmlOp::Call(function, args) => {
                let mut lets = Vec::new();
                let args = self.lower_args(args, &mut lets);
                out.extend(lets.into_iter().map(WriterStmt::Let));
                out.push(WriterStmt::WriteFunction(function.clone(), args));
            }

            FlatHtmlOp::Apply(closure, args) => {
                let mut lets = Vec::new();
                let args = self.lower_args(args, &mut lets);
                out.extend(lets.into_iter().map(WriterStmt::Let));
                out.push(WriterStmt::WriteApply(self.name(*closure), args));
            }

            FlatHtmlOp::Lambda(_) => unreachable!("a closure is bound, not written"),
        }
    }

    fn write_element(&mut self, element: &'a IrHtmlElement<ValueId>, out: &mut Vec<WriterStmt>) {
        let IrHtmlElement {
            element,
            attributes,
            children,
            on_click: _,
        } = element;
        // The element's writes merge among themselves first, so a constant
        // write grows across an element boundary only where the whole
        // element fits.
        let mut unit = Vec::new();
        write(&mut unit, &format!("<{}", element.as_str()));
        for (name, attribute) in attributes {
            match attribute {
                IrAttribute::Value(value) => {
                    write(&mut unit, &format!(" {}=\"", name.as_str()));
                    self.write_escaped(*value, &mut unit);
                    write(&mut unit, "\"");
                }
                IrAttribute::Presence(present) => {
                    let mut true_body = Vec::new();
                    write(&mut true_body, &format!(" {}", name.as_str()));
                    unit.push(WriterStmt::Match(IrSwitch::Bool {
                        subject: self.name(*present),
                        true_body,
                        false_body: Vec::new(),
                    }));
                }
                IrAttribute::Constant(Some(value)) => {
                    let mut escaped = String::new();
                    write_escaped_html(value.as_str(), &mut escaped);
                    write(&mut unit, &format!(" {}=\"{escaped}\"", name.as_str()));
                }
                IrAttribute::Constant(None) => write(&mut unit, &format!(" {}", name.as_str())),
            }
        }
        write(&mut unit, ">");
        if !element.is_void() {
            self.write_html(*children, &mut unit);
            write(&mut unit, &format!("</{}>", element.as_str()));
        }
        for statement in unit {
            match statement {
                WriterStmt::Write(content) => write(out, &content),
                statement => out.push(statement),
            }
        }
    }

    /// Write the Html a decision tree selects from the arms. `parts` holds
    /// the name of the part of the subject each case variable bound so far
    /// stands for. A case variable that is not in `used` is not bound.
    fn write_decision(
        &mut self,
        decision: &Decision<IrBinder<IrDataType>, IrDataType>,
        arms: &'a [IrMatchArm<FlatHtmlBlock>],
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        out: &mut Vec<WriterStmt>,
    ) {
        let switch = match decision {
            Decision::Success(body) => {
                for binding in &body.bindings {
                    self.aliases
                        .insert(binding.binder.var, parts[&binding.source]);
                }
                self.write_block(&arms[body.value].body, out);
                return;
            }

            Decision::SwitchRecord { variable, case } => {
                let record = parts[&variable.id];
                let mut lets = Vec::new();
                self.take_apart_record(record, &case.bindings, used, parts, &mut lets);
                out.extend(lets.into_iter().map(WriterStmt::Let));
                return self.write_decision(&case.body, arms, used, parts, out);
            }

            Decision::SwitchTuple { variable, case } => {
                let tuple = parts[&variable.id];
                let mut lets = Vec::new();
                self.take_apart_tuple(tuple, &case.elements, used, parts, &mut lets);
                out.extend(lets.into_iter().map(WriterStmt::Let));
                return self.write_decision(&case.body, arms, used, parts, out);
            }

            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                let mut true_body = Vec::new();
                self.write_decision(&true_case.body, arms, used, parts, &mut true_body);
                let mut false_body = Vec::new();
                self.write_decision(&false_case.body, arms, used, parts, &mut false_body);
                IrSwitch::Bool {
                    subject: parts[&variable.id],
                    true_body,
                    false_body,
                }
            }

            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let some_arm_binding =
                    self.bind_part(some_case.var.id, &some_case.var.typ, used, parts);
                let mut some_arm_body = Vec::new();
                self.write_decision(&some_case.body, arms, used, parts, &mut some_arm_body);
                let mut none_arm_body = Vec::new();
                self.write_decision(&none_case.body, arms, used, parts, &mut none_arm_body);
                IrSwitch::Option {
                    subject: parts[&variable.id],
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                }
            }

            Decision::SwitchEnum { variable, cases } => {
                let mut enum_arms = Vec::new();
                for case in cases {
                    let mut bindings = Vec::new();
                    for binding in &case.bindings {
                        if let Some(binder) =
                            self.bind_part(binding.var.id, &binding.var.typ, used, parts)
                        {
                            bindings.push((binding.field_name.clone(), binder));
                        }
                    }
                    let mut body = Vec::new();
                    self.write_decision(&case.body, arms, used, parts, &mut body);
                    enum_arms.push(EnumMatchArm {
                        pattern: EnumPattern::Variant {
                            type_name: case.type_name.clone(),
                            variant_name: case.variant_name.clone(),
                        },
                        bindings,
                        body,
                    });
                }
                IrSwitch::Enum {
                    subject: parts[&variable.id],
                    arms: enum_arms,
                }
            }
        };
        out.push(WriterStmt::Match(switch));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::hop::typing::{ComparableType, NumericType, Ty};
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::random_program;
    use crate::ir::ir_parameter::IrParameter;
    use crate::ir::ir_type::IrType;
    use crate::ir::optimize_flat;
    use crate::root_contained_file_path::RootContainedFilePath;
    use crate::session::Session;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    /// Prints the optimized program as Flat and as the Writer IR that Flat
    /// lowers to.
    fn check(source: &str, shell: Option<&DocumentShell>, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut session = Session::new();
        session.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, pages) = session.compile_flat(None, None);
        let flat = optimize_flat(flat);
        let before = flat.to_string();
        let writer = flat_to_writer(flat, &pages, shell).to_string();
        expected.assert_eq(&format!("-- flat --\n{before}\n-- writer --\n{writer}"));
    }

    /// Prints the optimized program as Flat and as the Writer IR that Flat
    /// lowers to, with a page for each entrypoint.
    fn check_program(program: FlatProgram, expected: Expect) {
        let pages = IrPage::for_entrypoints(&program);
        let flat = optimize_flat(program);
        let before = flat.to_string();
        let writer = flat_to_writer(flat, &pages, None).to_string();
        expected.assert_eq(&format!("-- flat --\n{before}\n-- writer --\n{writer}"));
    }

    #[test]
    fn leaves_out_the_clicks_and_renders_each_cell_as_its_initial_value() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Buttons",
                    [
                        ("n", Ty::Int),
                        ("on_press", Ty::function([], Ty::effect(Ty::tuple([])))),
                    ],
                    Ty::Html,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.html_concat([]));
                            m.arm(Pat::False, |t, _| {
                                let label = t.escape(t.int_to_string(t.param("n")));
                                let button =
                                    t.element_on_click("button", [], t.param("on_press"), label);
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Buttons", [rest, t.param("on_press")]);
                                t.html_concat([button, rest])
                            });
                        })
                    },
                )
                .entrypoint("Test", [], |t| {
                    let count = t.cell(t.int(0));
                    let n = t.track(count);
                    let bump = t.event_handler(|t| {
                        t.cell_write(count, t.add(NumericType::Int, n, t.int(1)))
                    });
                    let buttons = t.call("Buttons", [t.int(2), bump]);
                    let shown = t.escape(t.int_to_string(n));
                    t.html_concat([shown, buttons])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                fn Buttons@f0(n@v0: Int, on_press@v1: () -> Effect[()]) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  let v12: Html = match v3 {
                    true => {
                      let v4: Html = concat()
                      v4
                    }
                    false => {
                      let v5: String = v0.to_string()
                      let v6: Html = escape(v5)
                      let v7: Html = element("button", {}, v6) on_click v1
                      let v8: Int = 1
                      let v9: Int = sub[Int](v0, v8)
                      let v10: Html = Buttons@f0(v9, v1)
                      let v11: Html = concat(v7, v10)
                      v11
                    }
                  }
                  v12
                }
                entrypoint Test@e0() -> Html {
                  let v13: Int = 0
                  let v14: Cell[Int] = cell(v13)
                  let v15: Int = track(v14)
                  let v19: () -> Effect[()] = fn() {
                    let v16: Int = 1
                    let v17: Int = add[Int](v15, v16)
                    let v18: Effect[()] = v14 := v17
                    v18
                  }
                  let v20: Int = 2
                  let v21: Html = Buttons@f0(v20, v19)
                  let v22: String = v15.to_string()
                  let v23: Html = escape(v22)
                  let v24: Html = concat(v23, v21)
                  v24
                }

                -- writer --
                fn Buttons@f0(n@v0: Int, on_press@v1: () -> ()) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  match v3 {
                    true => {
                    }
                    false => {
                      let v5: String = v0.to_string()
                      let v8: Int = 1
                      let v9: Int = sub[Int](v0, v8)
                      write("<button>")
                      write_string(v5)
                      write("</button>")
                      write_function Buttons@f0(v9, v1)
                    }
                  }
                }
                page Test() {
                  let v13: Int = 0
                  let v19: () -> () = fn() {
                    let v16: Int = 1
                    let v17: Int = add[Int](v13, v16)
                    let v18: () = ()
                    v18
                  }
                  let v20: Int = 2
                  let v22: String = v13.to_string()
                  write_string(v22)
                  write_function Buttons@f0(v20, v19)
                }
            "#]],
        );
    }

    #[test]
    fn binds_a_closure_a_recursive_function_takes_and_writes_each_application() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Countdown",
                    [("n", Ty::Int), ("row", Ty::function([Ty::Int], Ty::Html))],
                    Ty::Html,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.html_concat([]));
                            m.arm(Pat::False, |t, _| {
                                let row = t.apply(t.param("row"), [t.param("n")]);
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Countdown", [rest, t.param("row")]);
                                t.html_concat([row, rest])
                            });
                        })
                    },
                )
                .entrypoint("Test", [("prefix", Ty::String)], |t| {
                    let prefix = t.param("prefix");
                    let row = t.lambda([IrType::INT], |t, params| {
                        let text = t.string_concat([prefix, t.int_to_string(params[0])]);
                        t.element("li", [], t.escape(text))
                    });
                    t.element("ul", [], t.call("Countdown", [t.int(3), row]))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                fn Countdown@f0(n@v0: Int, row@v1: (Int) -> Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  let v10: Html = match v3 {
                    true => {
                      let v4: Html = concat()
                      v4
                    }
                    false => {
                      let v5: Html = v1(v0)
                      let v6: Int = 1
                      let v7: Int = sub[Int](v0, v6)
                      let v8: Html = Countdown@f0(v7, v1)
                      let v9: Html = concat(v5, v8)
                      v9
                    }
                  }
                  v10
                }
                entrypoint Test@e0(prefix@v11: String) -> Html {
                  let v17: (Int) -> Html = fn(v12: Int) {
                    let v13: String = v12.to_string()
                    let v14: String = string_concat(v11, v13)
                    let v15: Html = escape(v14)
                    let v16: Html = element("li", {}, v15)
                    v16
                  }
                  let v18: Int = 3
                  let v19: Html = Countdown@f0(v18, v17)
                  let v20: Html = element("ul", {}, v19)
                  v20
                }

                -- writer --
                fn Countdown@f0(n@v0: Int, row@v1: (Int) -> Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  match v3 {
                    true => {
                    }
                    false => {
                      let v6: Int = 1
                      let v7: Int = sub[Int](v0, v6)
                      write_apply v1(v0)
                      write_function Countdown@f0(v7, v1)
                    }
                  }
                }
                page Test(prefix@v11: String) {
                  let v18: Int = 3
                  let v17: (Int) -> Html = fn(v12: Int) {
                    let v13: String = v12.to_string()
                    write("<li>")
                    write_string(v11)
                    write_string(v13)
                    write("</li>")
                  }
                  write("<ul>")
                  write_function Countdown@f0(v18, v17)
                  write("</ul>")
                }
            "#]],
        );
    }

    #[test]
    fn applies_a_closure_that_matches_on_a_name_it_captures() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Sum",
                    [("n", Ty::Int), ("scale", Ty::function([Ty::Int], Ty::Int))],
                    Ty::Int,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.int(0));
                            m.arm(Pat::False, |t, _| {
                                let scaled = t.apply(t.param("scale"), [t.param("n")]);
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Sum", [rest, t.param("scale")]);
                                t.add(NumericType::Int, scaled, rest)
                            });
                        })
                    },
                )
                .entrypoint("Test", [("double", Ty::Bool)], |t| {
                    let scale = t.lambda([IrType::INT], |t, params| {
                        t.match_(t.param("double"), |m| {
                            m.arm(Pat::True, |t, _| {
                                t.mul(NumericType::Int, params[0], t.int(2))
                            });
                            m.arm(Pat::False, |_, _| params[0]);
                        })
                    });
                    let sum = t.call("Sum", [t.int(3), scale]);
                    t.escape(t.int_to_string(sum))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                fn Sum@f0(n@v0: Int, scale@v1: (Int) -> Int) -> Int {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  let v10: Int = match v3 {
                    true => {
                      let v4: Int = 0
                      v4
                    }
                    false => {
                      let v5: Int = v1(v0)
                      let v6: Int = 1
                      let v7: Int = sub[Int](v0, v6)
                      let v8: Int = Sum@f0(v7, v1)
                      let v9: Int = add[Int](v5, v8)
                      v9
                    }
                  }
                  v10
                }
                entrypoint Test@e0(double@v11: Bool) -> Html {
                  let v16: (Int) -> Int = fn(v12: Int) {
                    let v15: Int = match v11 {
                      true => {
                        let v13: Int = 2
                        let v14: Int = mul[Int](v12, v13)
                        v14
                      }
                      false => {
                        v12
                      }
                    }
                    v15
                  }
                  let v17: Int = 3
                  let v18: Int = Sum@f0(v17, v16)
                  let v19: String = v18.to_string()
                  let v20: Html = escape(v19)
                  v20
                }

                -- writer --
                fn Sum@f0(n@v0: Int, scale@v1: (Int) -> Int) -> Int {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  let v22: Int = match v3 {
                    true => {
                      let v4: Int = 0
                      v4
                    }
                    false => {
                      let v5: Int = v1(v0)
                      let v6: Int = 1
                      let v7: Int = sub[Int](v0, v6)
                      let v8: Int = Sum@f0(v7, v1)
                      let v9: Int = add[Int](v5, v8)
                      v9
                    }
                  }
                  v22
                }
                page Test(double@v11: Bool) {
                  let v16: (Int) -> Int = fn(v12: Int) {
                    let v21: Int = match v11 {
                      true => {
                        let v13: Int = 2
                        let v14: Int = mul[Int](v12, v13)
                        v14
                      }
                      false => {
                        v12
                      }
                    }
                    v21
                  }
                  let v17: Int = 3
                  let v18: Int = Sum@f0(v17, v16)
                  let v19: String = v18.to_string()
                  write_string(v19)
                }
            "#]],
        );
    }

    #[test]
    fn passes_the_html_a_closure_takes_as_a_closure() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Wrap",
                    [("n", Ty::Int), ("wrap", Ty::function([Ty::Html], Ty::Html))],
                    Ty::Html,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.text("x"));
                            m.arm(Pat::False, |t, _| {
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let inner = t.call("Wrap", [rest, t.param("wrap")]);
                                t.apply(t.param("wrap"), [inner])
                            });
                        })
                    },
                )
                .entrypoint("Test", [], |t| {
                    let wrap = t.lambda([IrType::HTML], |t, params| t.element("b", [], params[0]));
                    t.call("Wrap", [t.int(2), wrap])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                fn Wrap@f0(n@v0: Int, wrap@v1: (Html) -> Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  let v9: Html = match v3 {
                    true => {
                      let v4: Html = text("x")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = sub[Int](v0, v5)
                      let v7: Html = Wrap@f0(v6, v1)
                      let v8: Html = v1(v7)
                      v8
                    }
                  }
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v12: (Html) -> Html = fn(v10: Html) {
                    let v11: Html = element("b", {}, v10)
                    v11
                  }
                  let v13: Int = 2
                  let v14: Html = Wrap@f0(v13, v12)
                  v14
                }

                -- writer --
                fn Wrap@f0(n@v0: Int, wrap@v1: (() -> Html) -> Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = lt_eq[Int](v0, v2)
                  match v3 {
                    true => {
                      write("x")
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = sub[Int](v0, v5)
                      let v15: () -> Html = fn() {
                        write_function Wrap@f0(v6, v1)
                      }
                      write_apply v1(v15)
                    }
                  }
                }
                page Test() {
                  let v13: Int = 2
                  let v12: (() -> Html) -> Html = fn(v10: () -> Html) {
                    write("<b>")
                    write_apply v10()
                    write("</b>")
                  }
                  write_function Wrap@f0(v13, v12)
                }
            "#]],
        );
    }

    #[test]
    fn writes_the_head_and_the_body_into_the_shell() {
        check(
            indoc! {r#"
                page Test(title: String) {
                  fn head() -> Html {
                    <title>{title}</title>
                  }
                  fn body() -> Html {
                    <h1>{title}</h1>
                  }
                }
            "#},
            Some(&DocumentShell::new(None, None)),
            expect![[r#"
                -- flat --
                entrypoint head@e0(title@v0: String) -> Html {
                  let v1: Html = escape(v0)
                  let v3: Html = element("title", {}, v1)
                  v3
                }
                entrypoint body@e1(title@v4: String) -> Html {
                  let v5: Html = escape(v4)
                  let v7: Html = element("h1", {}, v5)
                  v7
                }

                -- writer --
                page Test(title@v4: String) {
                  write("<!doctype html><html><head><meta charset=\"utf-8\"><meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">")
                  write("<title>")
                  write_string(v4)
                  write("</title>")
                  write("</head><body><h1>")
                  write_string(v4)
                  write("</h1>")
                  write("</body></html>")
                }
            "#]],
        );
    }

    #[test]
    fn writes_constant_attributes_with_their_element_and_switches_on_a_dynamic_presence() {
        check(
            indoc! {r#"
                page Test(id: String, flag: Bool) {
                  fn body() -> Html {
                    <div class="a<b" id={id}>
                      <input disabled={true} required={false} checked={flag}/>
                    </div>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(id@v0: String, flag@v1: Bool) -> Html {
                  let v5: Html = concat()
                  let v6: Html = element("input", {disabled, checked: v1}, v5)
                  let v8: Html = element("div", {class: "a<b", id: v0}, v6)
                  v8
                }

                -- writer --
                page Test(id@v0: String, flag@v1: Bool) {
                  write("<div class=\"a&lt;b\" id=\"")
                  write_string(v0)
                  write("\"><input disabled")
                  match v1 {
                    true => {
                      write(" checked")
                    }
                    false => {
                    }
                  }
                  write("></div>")
                }
            "#]],
        );
    }

    #[test]
    fn writes_a_loop_apart_from_the_writes_around_it() {
        check(
            indoc! {r#"
                page Test(items: Array[String]) {
                  fn body() -> Html {
                    <ul>
                      before
                      {for item in items { <li>{item}</li> }}
                      after
                    </ul>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(items@v0: Array[String]) -> Html {
                  let v1: Html = text("before")
                  let v6: Html = for v2: String in v0 {
                    let v3: Html = escape(v2)
                    let v5: Html = element("li", {}, v3)
                    v5
                  }
                  let v7: Html = text("after")
                  let v8: Html = concat(v1, v6, v7)
                  let v9: Html = element("ul", {}, v8)
                  v9
                }

                -- writer --
                page Test(items@v0: Array[String]) {
                  write("<ul>before")
                  for v2: String in v0 {
                    write("<li>")
                    write_string(v2)
                    write("</li>")
                  }
                  write("after</ul>")
                }
            "#]],
        );
    }

    #[test]
    fn writes_the_elements_of_a_fragment_in_order() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <>
                      <p>Hello</p>
                      <p>World</p>
                    </>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0() -> Html {
                  let v0: Html = text("Hello")
                  let v2: Html = element("p", {}, v0)
                  let v3: Html = text("World")
                  let v5: Html = element("p", {}, v3)
                  let v6: Html = concat(v2, v5)
                  v6
                }

                -- writer --
                page Test() {
                  write("<p>Hello</p><p>World</p>")
                }
            "#]],
        );
    }

    #[test]
    fn merges_adjacent_writes_while_they_stay_below_the_limit() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <p>
                      aaaaaaaaaaaaaaaaaaaaaaaaa
                      <b>bbbbbbbbbbbbbbbbbbbbbbbbb</b>
                      ccccccccccccccccccccccccc
                    </p>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0() -> Html {
                  let v0: Html = text("aaaaaaaaaaaaaaaaaaaaaaaaa")
                  let v1: Html = text("bbbbbbbbbbbbbbbbbbbbbbbbb")
                  let v3: Html = element("b", {}, v1)
                  let v4: Html = text("ccccccccccccccccccccccccc")
                  let v5: Html = concat(v0, v3, v4)
                  let v6: Html = element("p", {}, v5)
                  v6
                }

                -- writer --
                page Test() {
                  write("<p>aaaaaaaaaaaaaaaaaaaaaaaaa")
                  write("<b>bbbbbbbbbbbbbbbbbbbbbbbbb</b>ccccccccccccccccccccccccc")
                  write("</p>")
                }
            "#]],
        );
    }

    #[test]
    fn escapes_the_constant_parts_of_a_concat_now() {
        check(
            indoc! {r#"
                page Test(name: String) {
                  fn body() -> Html {
                    <p title={"a<" + name + ">b"}></p>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(name@v0: String) -> Html {
                  let v1: String = "a<"
                  let v3: String = ">b"
                  let v4: String = string_concat(v1, v0, v3)
                  let v5: Html = concat()
                  let v6: Html = element("p", {title: v4}, v5)
                  v6
                }

                -- writer --
                page Test(name@v0: String) {
                  write("<p title=\"a&lt;")
                  write_string(v0)
                  write("&gt;b\"></p>")
                }
            "#]],
        );
    }

    #[test]
    fn switches_on_each_part_of_a_tuple_and_writes_an_arm_at_every_leaf_that_selects_it() {
        check(
            indoc! {r#"
                page Test(signed_in: Bool, nickname: Option[String]) {
                  fn body() -> Html {
                    match (signed_in, nickname) {
                      (true, Some(name)) => <p class="member">{name}</p>,
                      _ => <p>guest</p>,
                    }
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(signed_in@v0: Bool, nickname@v1: Option[String]) -> Html {
                  let v2: (Bool, Option[String]) = (v0, v1)
                  let v11: Html = match v2 {
                    (true, Some(v3: String)) => {
                      let v5: Html = escape(v3)
                      let v7: Html = element("p", {class: "member"}, v5)
                      v7
                    }
                    _ => {
                      let v8: Html = text("guest")
                      let v10: Html = element("p", {}, v8)
                      v10
                    }
                  }
                  v11
                }

                -- writer --
                page Test(signed_in@v0: Bool, nickname@v1: Option[String]) {
                  match v1 {
                    Some(v12: String) => {
                      match v0 {
                        true => {
                          write("<p class=\"member\">")
                          write_string(v12)
                          write("</p>")
                        }
                        false => {
                          write("<p>guest</p>")
                        }
                      }
                    }
                    None => {
                      write("<p>guest</p>")
                    }
                  }
                }
            "#]],
        );
    }

    /// Each entrypoint is lowered as a page. The lowering also finds no
    /// Html held in a value, whether the program is optimized or not.
    #[test]
    fn fuzz_random_programs_lower_to_writer_binding_every_name_once() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let pages = IrPage::for_entrypoints(&program);
            for flat in [program.clone(), optimize_flat(program)] {
                let writer = flat_to_writer(flat, &pages, None);
                assert_eq!(name_bound_twice(&writer), None, "bound twice in\n{writer}");
            }
            Ok(())
        });
    }

    /// A name the program binds more than once, if there is one.
    fn name_bound_twice(program: &WriterProgram) -> Option<ValueId> {
        let mut names = BoundNames::default();
        for page in &program.pages {
            names.parameters(&page.parameters);
            names.stmts(&page.body);
        }
        for function in &program.functions {
            names.parameters(&function.parameters);
            match &function.body {
                WriterFunctionBody::Writes(stmts) => names.stmts(stmts),
                WriterFunctionBody::Returns(block) => names.value_block(block),
            }
        }
        names.twice
    }

    /// The names bound so far, and the first of them bound twice.
    #[derive(Default)]
    struct BoundNames {
        bound: HashSet<ValueId>,
        twice: Option<ValueId>,
    }

    impl BoundNames {
        fn bind(&mut self, name: ValueId) {
            if !self.bound.insert(name) && self.twice.is_none() {
                self.twice = Some(name);
            }
        }

        fn binder(&mut self, binder: &IrBinder<IrDataType>) {
            self.bind(binder.var);
        }

        fn parameters(&mut self, parameters: &[IrParameter<IrType>]) {
            for parameter in parameters {
                self.bind(parameter.binder.var);
            }
        }

        fn stmts(&mut self, stmts: &[WriterStmt]) {
            for stmt in stmts {
                match stmt {
                    WriterStmt::Let(let_) => self.let_(let_),
                    WriterStmt::Write(_)
                    | WriterStmt::WriteString(_)
                    | WriterStmt::WriteFunction(..)
                    | WriterStmt::WriteApply(..) => {}
                    WriterStmt::HtmlFor(IrFor { var, body, .. }) => {
                        if let Some(var) = var {
                            self.binder(var);
                        }
                        self.stmts(body);
                    }
                    WriterStmt::Match(switch) => {
                        self.switch(switch, |names, body| names.stmts(body));
                    }
                }
            }
        }

        fn value_block(&mut self, block: &WriterBlock) {
            for let_ in &block.bindings {
                self.let_(let_);
            }
        }

        fn let_(&mut self, let_: &IrBinding<IrType, WriterOp>) {
            self.bind(let_.binder.var);
            match &let_.op {
                WriterOp::Data(_)
                | WriterOp::TupleIndex(..)
                | WriterOp::Call(..)
                | WriterOp::Apply(..) => {}
                WriterOp::Match(switch) => self.switch(switch, Self::value_block),
                WriterOp::Lambda(lambda) => {
                    for param in &lambda.parameters {
                        self.bind(param.var);
                    }
                    match &lambda.body {
                        WriterFunctionBody::Writes(stmts) => self.stmts(stmts),
                        WriterFunctionBody::Returns(block) => self.value_block(block),
                    }
                }
            }
        }

        fn switch<Body>(&mut self, switch: &IrSwitch<Body>, body: fn(&mut Self, &Body)) {
            match switch {
                IrSwitch::Enum { arms, .. } => {
                    for arm in arms {
                        for (_, binder) in &arm.bindings {
                            self.binder(binder);
                        }
                        body(self, &arm.body);
                    }
                }
                IrSwitch::Bool {
                    true_body,
                    false_body,
                    ..
                } => {
                    body(self, true_body);
                    body(self, false_body);
                }
                IrSwitch::Option {
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                    ..
                } => {
                    if let Some(binder) = some_arm_binding {
                        self.binder(binder);
                    }
                    body(self, some_arm_body);
                    body(self, none_arm_body);
                }
            }
        }
    }

    #[test]
    fn binds_fresh_names_in_each_copy_of_an_arm_that_several_leaves_select() {
        check(
            indoc! {r#"
                page Test(signed_in: Bool, nickname: Option[String], count: Int) {
                  fn body() -> Html {
                    <div>
                      <p>
                        {match (signed_in, nickname) {
                          (true, Some(name)) => name,
                          _ => count.to_string(),
                        }}
                      </p>
                      {match (signed_in, nickname) {
                        (true, Some(name)) => <p>{name}</p>,
                        _ => <p>{count.to_string()}{for i in 1..=count { <b>{i.to_string()}</b> }}</p>,
                      }}
                    </div>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(signed_in@v0: Bool, nickname@v1: Option[String], count@v2: Int) -> Html {
                  let v3: (Bool, Option[String]) = (v0, v1)
                  let v6: String = match v3 {
                    (true, Some(v4: String)) => {
                      v4
                    }
                    _ => {
                      let v5: String = v2.to_string()
                      v5
                    }
                  }
                  let v7: Html = escape(v6)
                  let v9: Html = element("p", {}, v7)
                  let v10: (Bool, Option[String]) = (v0, v1)
                  let v27: Html = match v10 {
                    (true, Some(v11: String)) => {
                      let v12: Html = escape(v11)
                      let v14: Html = element("p", {}, v12)
                      v14
                    }
                    _ => {
                      let v15: String = v2.to_string()
                      let v16: Html = escape(v15)
                      let v17: Int = 1
                      let v18: Array[Int] = v17..=v2
                      let v24: Html = for v19: Int in v18 {
                        let v20: String = v19.to_string()
                        let v21: Html = escape(v20)
                        let v23: Html = element("b", {}, v21)
                        v23
                      }
                      let v25: Html = concat(v16, v24)
                      let v26: Html = element("p", {}, v25)
                      v26
                    }
                  }
                  let v28: Html = concat(v9, v27)
                  let v29: Html = element("div", {}, v28)
                  v29
                }

                -- writer --
                page Test(signed_in@v0: Bool, nickname@v1: Option[String], count@v2: Int) {
                  let v33: String = match v1 {
                    Some(v30: String) => {
                      let v31: String = match v0 {
                        true => {
                          v30
                        }
                        false => {
                          let v5: String = v2.to_string()
                          v5
                        }
                      }
                      v31
                    }
                    None => {
                      let v32: String = v2.to_string()
                      v32
                    }
                  }
                  write("<div><p>")
                  write_string(v33)
                  write("</p>")
                  match v1 {
                    Some(v34: String) => {
                      match v0 {
                        true => {
                          write("<p>")
                          write_string(v34)
                          write("</p>")
                        }
                        false => {
                          let v15: String = v2.to_string()
                          let v17: Int = 1
                          let v18: Array[Int] = v17..=v2
                          write("<p>")
                          write_string(v15)
                          for v19: Int in v18 {
                            let v20: String = v19.to_string()
                            write("<b>")
                            write_string(v20)
                            write("</b>")
                          }
                          write("</p>")
                        }
                      }
                    }
                    None => {
                      let v35: String = v2.to_string()
                      let v36: Int = 1
                      let v37: Array[Int] = v36..=v2
                      write("<p>")
                      write_string(v35)
                      for v38: Int in v37 {
                        let v39: String = v38.to_string()
                        write("<b>")
                        write_string(v39)
                        write("</b>")
                      }
                      write("</p>")
                    }
                  }
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn binds_the_fields_of_a_variant_and_takes_apart_a_record() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  admin: Bool,
                }

                enum Session {
                  Guest,
                  SignedIn {user: User},
                }

                page Test(session: Session) {
                  fn body() -> Html {
                    match session {
                      Session::SignedIn {user: User {name, admin: true}} => <b>{name}</b>,
                      Session::SignedIn {user: User {name, admin: false}} => <i>{name}</i>,
                      Session::Guest => <p>guest</p>,
                    }
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(session@v0: Session) -> Html {
                  let v12: Html = match v0 {
                    Session::SignedIn {user: User {name@v1: String, admin: true}} => {
                      let v2: Html = escape(v1)
                      let v4: Html = element("b", {}, v2)
                      v4
                    }
                    Session::SignedIn {user: User {name@v5: String, admin: false}} => {
                      let v6: Html = escape(v5)
                      let v8: Html = element("i", {}, v6)
                      v8
                    }
                    Session::Guest => {
                      let v9: Html = text("guest")
                      let v11: Html = element("p", {}, v9)
                      v11
                    }
                  }
                  v12
                }

                -- writer --
                page Test(session@v0: Session) {
                  match v0 {
                    Session::Guest => {
                      write("<p>guest</p>")
                    }
                    Session::SignedIn {user@v13: User} => {
                      let v14: String = v13.name
                      let v15: Bool = v13.admin
                      match v15 {
                        true => {
                          write("<b>")
                          write_string(v14)
                          write("</b>")
                        }
                        false => {
                          write("<i>")
                          write_string(v14)
                          write("</i>")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn compiles_a_value_match_to_switches_that_produce_the_value() {
        check(
            indoc! {r#"
                page Test(count: Option[Int], exact: Bool) {
                  fn body() -> Html {
                    <p>
                      {match (count, exact) {
                        (Some(n), true) => n.to_string(),
                        (Some(_), false) => "some",
                        (None, _) => "none",
                      }}
                    </p>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                entrypoint body@e0(count@v0: Option[Int], exact@v1: Bool) -> Html {
                  let v2: (Option[Int], Bool) = (v0, v1)
                  let v7: String = match v2 {
                    (Some(v3: Int), true) => {
                      let v4: String = v3.to_string()
                      v4
                    }
                    (Some(_), false) => {
                      let v5: String = "some"
                      v5
                    }
                    (None, _) => {
                      let v6: String = "none"
                      v6
                    }
                  }
                  let v8: Html = escape(v7)
                  let v10: Html = element("p", {}, v8)
                  v10
                }

                -- writer --
                page Test(count@v0: Option[Int], exact@v1: Bool) {
                  let v13: String = match v0 {
                    Some(v11: Int) => {
                      let v12: String = match v1 {
                        true => {
                          let v4: String = v11.to_string()
                          v4
                        }
                        false => {
                          let v5: String = "some"
                          v5
                        }
                      }
                      v12
                    }
                    None => {
                      let v6: String = "none"
                      v6
                    }
                  }
                  write("<p>")
                  write_string(v13)
                  write("</p>")
                }
            "#]],
        );
    }

    #[test]
    fn passes_markup_to_a_recursive_function_as_a_closure_and_passes_it_on() {
        check(
            indoc! {r#"
                fn Nest(depth: Int, children: Html) -> Html {
                  match 0 < depth {
                    true => <div><Nest depth={depth - 1}>{children}</Nest></div>,
                    false => children,
                  }
                }

                page Test(name: String) {
                  fn body() -> Html {
                    <Nest depth={2}><b>{name}</b></Nest>
                  }
                }
            "#},
            None,
            expect![[r#"
                -- flat --
                fn Nest@f0(depth@v7: Int, children@v8: Html) -> Html {
                  let v9: Int = 0
                  let v10: Bool = lt[Int](v9, v7)
                  let v17: Html = match v10 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = sub[Int](v7, v11)
                      let v14: Html = Nest@f0(v12, v8)
                      let v16: Html = element("div", {}, v14)
                      v16
                    }
                    false => {
                      v8
                    }
                  }
                  v17
                }
                entrypoint body@e0(name@v0: String) -> Html {
                  let v1: Int = 2
                  let v2: Html = escape(v0)
                  let v4: Html = element("b", {}, v2)
                  let v6: Html = Nest@f0(v1, v4)
                  v6
                }

                -- writer --
                fn Nest@f0(depth@v7: Int, children@v8: () -> Html) -> Html {
                  let v9: Int = 0
                  let v10: Bool = lt[Int](v9, v7)
                  match v10 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = sub[Int](v7, v11)
                      write("<div>")
                      write_function Nest@f0(v12, v8)
                      write("</div>")
                    }
                    false => {
                      write_apply v8()
                    }
                  }
                }
                page Test(name@v0: String) {
                  let v1: Int = 2
                  let v18: () -> Html = fn() {
                    write("<b>")
                    write_string(v0)
                    write("</b>")
                  }
                  write_function Nest@f0(v1, v18)
                }
            "#]],
        );
    }

    #[test]
    fn writes_html_read_in_two_arms_once_in_each() {
        check_program(
            FlatProgramBuilder::new()
                .entrypoint(
                    "Greeting",
                    [("name", Ty::option(Ty::String)), ("formal", Ty::Bool)],
                    |t| {
                        let greeting = t.match_(t.param("name"), |m| {
                            m.arm(Pat::some(Pat::bind("n")), |t, arm| {
                                t.element("b", [], t.escape(arm.binding("n")))
                            });
                            m.arm(Pat::None, |t, _| t.text("anonymous"));
                        });
                        t.match_(t.param("formal"), |m| {
                            m.arm(Pat::True, |t, _| t.element("p", [], greeting));
                            m.arm(Pat::False, |t, _| t.element("i", [], greeting));
                        })
                    },
                )
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                entrypoint Greeting@e0(name@v0: Option[String], formal@v1: Bool) -> Html {
                  let v6: Html = match v0 {
                    Some(v2: String) => {
                      let v3: Html = escape(v2)
                      let v4: Html = element("b", {}, v3)
                      v4
                    }
                    None => {
                      let v5: Html = text("anonymous")
                      v5
                    }
                  }
                  let v9: Html = match v1 {
                    true => {
                      let v7: Html = element("p", {}, v6)
                      v7
                    }
                    false => {
                      let v8: Html = element("i", {}, v6)
                      v8
                    }
                  }
                  v9
                }

                -- writer --
                page Greeting(name@v0: Option[String], formal@v1: Bool) {
                  match v1 {
                    true => {
                      write("<p>")
                      match v0 {
                        Some(v10: String) => {
                          write("<b>")
                          write_string(v10)
                          write("</b>")
                        }
                        None => {
                          write("anonymous")
                        }
                      }
                      write("</p>")
                    }
                    false => {
                      write("<i>")
                      match v0 {
                        Some(v11: String) => {
                          write("<b>")
                          write_string(v11)
                          write("</b>")
                        }
                        None => {
                          write("anonymous")
                        }
                      }
                      write("</i>")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn keeps_a_value_outside_the_loop_that_writes_it_and_drops_html_nothing_reads() {
        check_program(
            FlatProgramBuilder::new()
                .entrypoint(
                    "Items",
                    [("items", Ty::array(Ty::String)), ("n", Ty::Int)],
                    |t| {
                        let _unused = t.text("never");
                        let data_x =
                            t.int_to_string(t.add(NumericType::Int, t.param("n"), t.int(1)));
                        t.element(
                            "ul",
                            [],
                            t.html_for(t.param("items"), |t, _| {
                                t.element("li", [t.attr_value("data-x", data_x)], t.text("x"))
                            }),
                        )
                    },
                )
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                entrypoint Items@e0(items@v0: Array[String], n@v1: Int) -> Html {
                  let v3: Int = 1
                  let v4: Int = add[Int](v1, v3)
                  let v5: String = v4.to_string()
                  let v9: Html = for _ in v0 {
                    let v7: Html = text("x")
                    let v8: Html = element("li", {data-x: v5}, v7)
                    v8
                  }
                  let v10: Html = element("ul", {}, v9)
                  v10
                }

                -- writer --
                page Items(items@v0: Array[String], n@v1: Int) {
                  let v3: Int = 1
                  let v4: Int = add[Int](v1, v3)
                  let v5: String = v4.to_string()
                  write("<ul>")
                  for _ in v0 {
                    write("<li data-x=\"")
                    write_string(v5)
                    write("\">x</li>")
                  }
                  write("</ul>")
                }
            "#]],
        );
    }

    #[test]
    fn writes_the_children_of_inlined_calls_in_place() {
        check_program(
            FlatProgramBuilder::new()
                .function("Badge", [("label", Ty::String)], Ty::Html, |t| {
                    t.element("span", [], t.escape(t.param("label")))
                })
                .function("Wrap", [("content", Ty::Html)], Ty::Html, |t| {
                    t.element("div", [], t.param("content"))
                })
                .entrypoint("Test", [("label", Ty::String)], |t| {
                    t.call(
                        "Wrap",
                        [t.html_concat(vec![
                            t.call("Badge", vec![t.param("label")]),
                            t.element("b", vec![], t.html_concat(vec![])),
                        ])],
                    )
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                entrypoint Test@e0(label@v5: String) -> Html {
                  let v11: Html = escape(v5)
                  let v12: Html = element("span", {}, v11)
                  let v7: Html = concat()
                  let v8: Html = element("b", {}, v7)
                  let v9: Html = concat(v12, v8)
                  let v13: Html = element("div", {}, v9)
                  v13
                }

                -- writer --
                page Test(label@v5: String) {
                  write("<div><span>")
                  write_string(v5)
                  write("</span><b></b></div>")
                }
            "#]],
        );
    }

    #[test]
    fn renders_a_cell_a_function_takes_as_its_initial_value() {
        check_program(
            FlatProgramBuilder::new()
                .function("Show", [("count", Ty::cell(Ty::Int))], Ty::Html, |t| {
                    let shown = t.track(t.param("count"));
                    let bump = t.event_handler(|t| {
                        t.cell_write(t.param("count"), t.add(NumericType::Int, shown, t.int(1)))
                    });
                    t.element_on_click("button", [], bump, t.escape(t.int_to_string(shown)))
                })
                .entrypoint("Test", [], |t| {
                    let count = t.cell(t.int(7));
                    t.call("Show", [count])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- flat --
                entrypoint Test@e0() -> Html {
                  let v9: Int = 7
                  let v10: Cell[Int] = cell(v9)
                  let v12: Int = track(v10)
                  let v16: () -> Effect[()] = fn() {
                    let v13: Int = 1
                    let v14: Int = add[Int](v12, v13)
                    let v15: Effect[()] = v10 := v14
                    v15
                  }
                  let v17: String = v12.to_string()
                  let v18: Html = escape(v17)
                  let v19: Html = element("button", {}, v18) on_click v16
                  v19
                }

                -- writer --
                page Test() {
                  let v9: Int = 7
                  let v16: () -> () = fn() {
                    let v13: Int = 1
                    let v14: Int = add[Int](v9, v13)
                    let v15: () = ()
                    v15
                  }
                  let v17: String = v9.to_string()
                  write("<button>")
                  write_string(v17)
                  write("</button>")
                }
            "#]],
        );
    }
}
