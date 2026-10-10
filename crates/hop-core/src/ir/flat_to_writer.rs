use std::collections::HashMap;

use crate::hop::typing::Type;
use crate::html::write_escaped_html;
use crate::ir::binder_id::BinderId;
use crate::ir::document_shell::DocumentShell;
use crate::ir::flat_module::{
    FlatAttribute, FlatBinding, FlatBlock, FlatForSource, FlatFunctionDeclaration, FlatModule,
    FlatOp, FlatPageDeclaration,
};
use crate::ir::ir_match::{EnumMatchArm, Match};
use crate::ir::var_id::VarId;
use crate::ir::writer_module::{
    WriterForSource, WriterFunctionBody, WriterFunctionDeclaration, WriterLet, WriterModule,
    WriterName, WriterOp, WriterPageDeclaration, WriterStmt, WriterValueBlock,
};

/// Lower a Flat module to the Writer.
///
/// Values stay lets, in their order, except a Read, which is folded into
/// its readers so they read the binder. Html is written. An Html binding that
/// is read once, as a part of a concat, the children of an element or the
/// result of a block, and not from inside a loop it sits outside of, is
/// not bound at all but written where it is read. Every other Html binding
/// renders into a buffer of its own as an HtmlLiteral. A constant string
/// written escaped is escaped now and a concat part by part, and a
/// constant presence settles whether its attribute renders, when the
/// constant has no other reader.
///
/// With a shell, a page writes a whole document, the shell around its head
/// and its body. Without one, a page writes its head and its body alone.
pub fn flat_to_writer(module: FlatModule, shell: Option<&DocumentShell>) -> WriterModule {
    WriterModule {
        pages: module
            .pages
            .into_iter()
            .map(|page| lower_page(page, shell))
            .collect(),
        functions: module.functions.into_iter().map(lower_function).collect(),
    }
}

fn lower_page(decl: FlatPageDeclaration, shell: Option<&DocumentShell>) -> WriterPageDeclaration {
    let mut body = Vec::new();
    match shell {
        Some(shell) => {
            body.push(WriterStmt::Write(shell.before_head.to_string()));
            if let Some(head) = decl.head {
                lower_body(head, &mut body);
            }
            body.push(WriterStmt::Write(shell.after_head.clone()));
            lower_body(decl.body, &mut body);
            body.push(WriterStmt::Write(shell.after_body.to_string()));
        }
        None => {
            if let Some(head) = decl.head {
                lower_body(head, &mut body);
            }
            lower_body(decl.body, &mut body);
        }
    }
    WriterPageDeclaration {
        name: decl.name,
        parameters: decl.parameters,
        body,
    }
}

/// Lower a function declaration, choosing the calling convention from its
/// return type. Html compiles to destination passing, everything else
/// compiles as an ordinary value returning function.
fn lower_function(decl: FlatFunctionDeclaration) -> WriterFunctionDeclaration {
    let body = if matches!(decl.return_type, Type::Html) {
        let mut statements = Vec::new();
        lower_body(decl.body, &mut statements);
        WriterFunctionBody::Writes(statements)
    } else {
        let mut lowerer = Lowerer::new(&decl.body, false);
        let block = lowerer.lower_value_block(decl.body);
        debug_assert!(
            lowerer.pending.is_empty(),
            "every held back binding was read"
        );
        WriterFunctionBody::Returns(block)
    };
    WriterFunctionDeclaration {
        function: decl.function,
        parameters: decl.parameters,
        return_type: decl.return_type,
        body,
    }
}

/// Lower an Html block in output position.
fn lower_body(block: FlatBlock, out: &mut Vec<WriterStmt>) {
    let mut lowerer = Lowerer::new(&block, true);
    lowerer.lower_output_block(block, out);
    debug_assert!(
        lowerer.pending.is_empty(),
        "every held back binding was read"
    );
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

/// What reads a name, for a name read once.
#[derive(Clone, Copy)]
enum Reader {
    /// A part of a concat, the children of an element or the result of a
    /// block in output position. Html read here is written.
    Output,
    /// An escape or an attribute value. A constant String read here is
    /// escaped now.
    Escaped,
    /// A presence. A constant Bool read here settles the attribute.
    Presence,
    /// A part of the named String concat. Folded along with the concat.
    ConcatPart(VarId),
    /// Anything else.
    Value,
}

struct Use {
    count: usize,
    reader: Reader,
    /// The loop depth of the reader.
    depth: usize,
}

#[derive(Clone, Copy)]
enum Kind {
    /// An op that writes: text, escape, element, concat, loop, and a match
    /// or call of type Html.
    Html,
    StringLiteral,
    StringConcat,
    BoolLiteral,
    Other,
}

struct Def {
    /// The loop depth of the binding.
    depth: usize,
    kind: Kind,
}

struct Lowerer {
    uses: HashMap<VarId, Use>,
    defs: HashMap<VarId, Def>,
    /// The ops of the bindings written or folded where they are read, held
    /// until that read.
    pending: HashMap<VarId, FlatOp>,
    /// The binder each Read binding reads. A Read is not lowered, and what
    /// reads its binding reads the binder instead.
    reads: HashMap<VarId, BinderId>,
}

impl Lowerer {
    /// Analyze a body before lowering it. `output` says whether the body's
    /// result is written.
    fn new(block: &FlatBlock, output: bool) -> Self {
        let mut lowerer = Lowerer {
            uses: HashMap::new(),
            defs: HashMap::new(),
            pending: HashMap::new(),
            reads: HashMap::new(),
        };
        lowerer.analyze(block, 0, output);
        lowerer
    }

    /// The Writer name for reading a binding: its binder when it is a Read.
    fn name(&self, name: VarId) -> WriterName {
        match self.reads.get(&name) {
            Some(binder) => WriterName::Binder(*binder),
            None => WriterName::Binding(name),
        }
    }

    fn analyze(&mut self, block: &FlatBlock, depth: usize, output: bool) {
        for binding in &block.bindings {
            let kind = match &binding.op {
                FlatOp::HtmlText(_)
                | FlatOp::HtmlEscape(_)
                | FlatOp::HtmlElement { .. }
                | FlatOp::HtmlConcat(_)
                | FlatOp::HtmlFor { .. } => Kind::Html,
                FlatOp::Match(_) | FlatOp::Call { .. } if matches!(binding.typ, Type::Html) => {
                    Kind::Html
                }
                FlatOp::StringLiteral(_) => Kind::StringLiteral,
                FlatOp::StringConcat(_) => Kind::StringConcat,
                FlatOp::BoolLiteral(_) => Kind::BoolLiteral,
                _ => Kind::Other,
            };
            self.defs.insert(binding.name, Def { depth, kind });
            match &binding.op {
                FlatOp::HtmlConcat(parts) => {
                    for part in parts {
                        self.read(*part, Reader::Output, depth);
                    }
                }
                FlatOp::HtmlElement {
                    attributes,
                    children,
                    ..
                } => {
                    for attribute in attributes {
                        match attribute {
                            FlatAttribute::Value { value, .. } => {
                                self.read(*value, Reader::Escaped, depth);
                            }
                            FlatAttribute::Presence { present, .. } => {
                                self.read(*present, Reader::Presence, depth);
                            }
                        }
                    }
                    self.read(*children, Reader::Output, depth);
                }
                FlatOp::HtmlEscape(string) => self.read(*string, Reader::Escaped, depth),
                FlatOp::StringConcat(parts) => {
                    for part in parts {
                        self.read(*part, Reader::ConcatPart(binding.name), depth);
                    }
                }
                FlatOp::Match(match_) => {
                    let output = matches!(binding.typ, Type::Html);
                    match match_ {
                        Match::Bool {
                            subject,
                            true_body,
                            false_body,
                        } => {
                            self.read(**subject, Reader::Value, depth);
                            self.analyze(true_body, depth, output);
                            self.analyze(false_body, depth, output);
                        }
                        Match::Option {
                            subject,
                            some_arm_body,
                            none_arm_body,
                            ..
                        } => {
                            self.read(**subject, Reader::Value, depth);
                            self.analyze(some_arm_body, depth, output);
                            self.analyze(none_arm_body, depth, output);
                        }
                        Match::Enum { subject, arms } => {
                            self.read(**subject, Reader::Value, depth);
                            for arm in arms {
                                self.analyze(&arm.body, depth, output);
                            }
                        }
                    }
                }
                FlatOp::HtmlFor { source, body, .. } => {
                    match source {
                        FlatForSource::Array(array) => self.read(*array, Reader::Value, depth),
                        FlatForSource::RangeInclusive { start, end } => {
                            self.read(*start, Reader::Value, depth);
                            self.read(*end, Reader::Value, depth);
                        }
                    }
                    self.analyze(body, depth + 1, true);
                }
                op => op.for_each_operand(&mut |operand| self.read(operand, Reader::Value, depth)),
            }
        }
        let reader = if output {
            Reader::Output
        } else {
            Reader::Value
        };
        self.read(block.result, reader, depth);
    }

    fn read(&mut self, name: VarId, reader: Reader, depth: usize) {
        let use_ = self.uses.entry(name).or_insert(Use {
            count: 0,
            reader,
            depth,
        });
        use_.count += 1;
        use_.reader = reader;
        use_.depth = depth;
    }

    /// Whether the binding is written or folded where it is read, rather
    /// than bound to its name.
    fn held_back(&self, name: VarId) -> bool {
        let Some(use_) = self.uses.get(&name) else {
            return false;
        };
        if use_.count != 1 {
            return false;
        }
        let def = &self.defs[&name];
        match (def.kind, use_.reader) {
            (Kind::Html, Reader::Output) => use_.depth == def.depth,
            (Kind::StringLiteral | Kind::StringConcat, Reader::Escaped) => true,
            (Kind::StringLiteral | Kind::StringConcat, Reader::ConcatPart(concat)) => {
                self.held_back(concat)
            }
            (Kind::BoolLiteral, Reader::Presence) => true,
            _ => false,
        }
    }

    fn lower_output_block(&mut self, block: FlatBlock, out: &mut Vec<WriterStmt>) {
        let lets = self.lower_bindings(block.bindings);
        out.extend(lets.into_iter().map(WriterStmt::Let));
        self.lower_html(block.result, out);
    }

    fn lower_value_block(&mut self, block: FlatBlock) -> WriterValueBlock {
        let lets = self.lower_bindings(block.bindings);
        WriterValueBlock {
            lets,
            result: self.name(block.result),
        }
    }

    /// Lower the bindings to lets, folding the Reads into their readers and
    /// holding back the ones written or folded where they are read.
    fn lower_bindings(&mut self, bindings: Vec<FlatBinding>) -> Vec<WriterLet> {
        let mut lets = Vec::new();
        for binding in bindings {
            if let FlatOp::Read(binder) = binding.op {
                self.reads.insert(binding.name, binder);
                continue;
            }
            if self.held_back(binding.name) {
                self.pending.insert(binding.name, binding.op);
                continue;
            }
            let FlatBinding { name, typ, op } = binding;
            let op = match op {
                op @ (FlatOp::HtmlText(_)
                | FlatOp::HtmlEscape(_)
                | FlatOp::HtmlElement { .. }
                | FlatOp::HtmlConcat(_)
                | FlatOp::HtmlFor { .. }) => {
                    let mut body = Vec::new();
                    self.lower_html_op(op, &mut body);
                    WriterOp::HtmlLiteral(body)
                }
                op @ (FlatOp::Match(_) | FlatOp::Call { .. }) if matches!(typ, Type::Html) => {
                    let mut body = Vec::new();
                    self.lower_html_op(op, &mut body);
                    WriterOp::HtmlLiteral(body)
                }
                FlatOp::Match(match_) => WriterOp::Match(match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(self.name(*subject)),
                        true_body: Box::new(self.lower_value_block(*true_body)),
                        false_body: Box::new(self.lower_value_block(*false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject: Box::new(self.name(*subject)),
                        some_arm_binding,
                        some_arm_body: Box::new(self.lower_value_block(*some_arm_body)),
                        none_arm_body: Box::new(self.lower_value_block(*none_arm_body)),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: Box::new(self.name(*subject)),
                        arms: arms
                            .into_iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern,
                                bindings: arm.bindings,
                                body: self.lower_value_block(arm.body),
                            })
                            .collect(),
                    },
                }),
                FlatOp::Read(_) => unreachable!("a Read is folded into its readers"),
                FlatOp::Call { function, args } => WriterOp::Call {
                    function,
                    args: args.into_iter().map(|arg| self.name(arg)).collect(),
                },
                FlatOp::StringLiteral(value) => WriterOp::StringLiteral(value),
                FlatOp::IntLiteral(value) => WriterOp::IntLiteral(value),
                FlatOp::FloatLiteral(value) => WriterOp::FloatLiteral(value),
                FlatOp::BoolLiteral(value) => WriterOp::BoolLiteral(value),
                FlatOp::FieldAccess { record, field } => WriterOp::FieldAccess {
                    record: self.name(record),
                    field,
                },
                FlatOp::TupleIndex { tuple, index } => WriterOp::TupleIndex {
                    tuple: self.name(tuple),
                    index,
                },
                FlatOp::Array(elements) => WriterOp::Array(
                    elements
                        .into_iter()
                        .map(|element| self.name(element))
                        .collect(),
                ),
                FlatOp::Tuple(elements) => WriterOp::Tuple(
                    elements
                        .into_iter()
                        .map(|element| self.name(element))
                        .collect(),
                ),
                FlatOp::Record { fields } => WriterOp::Record {
                    fields: fields
                        .into_iter()
                        .map(|(field, value)| (field, self.name(value)))
                        .collect(),
                },
                FlatOp::Enum {
                    variant_name,
                    fields,
                } => WriterOp::Enum {
                    variant_name,
                    fields: fields
                        .into_iter()
                        .map(|(field, value)| (field, self.name(value)))
                        .collect(),
                },
                FlatOp::Option(value) => WriterOp::Option(value.map(|value| self.name(value))),
                FlatOp::StringConcat(parts) => {
                    WriterOp::StringConcat(parts.into_iter().map(|part| self.name(part)).collect())
                }
                FlatOp::Binary { op, left, right } => WriterOp::Binary {
                    op,
                    left: self.name(left),
                    right: self.name(right),
                },
                FlatOp::Unary { op, operand } => WriterOp::Unary {
                    op,
                    operand: self.name(operand),
                },
            };
            lets.push(WriterLet { name, typ, op });
        }
        lets
    }

    /// Write an Html name: the writes of its op when it was held back, the
    /// value it was bound to otherwise.
    fn lower_html(&mut self, name: VarId, out: &mut Vec<WriterStmt>) {
        match self.pending.remove(&name) {
            Some(op) => self.lower_html_op(op, out),
            None => out.push(WriterStmt::WriteHtml(self.name(name))),
        }
    }

    /// Write a String name escaped. A constant held back is escaped now,
    /// and a concat held back part by part, so only what varies is escaped
    /// when the page renders.
    fn lower_escaped(&mut self, name: VarId, out: &mut Vec<WriterStmt>) {
        match self.pending.remove(&name) {
            Some(FlatOp::StringLiteral(value)) => {
                let mut content = String::new();
                write_escaped_html(value.as_str(), &mut content);
                write(out, &content);
            }
            Some(FlatOp::StringConcat(parts)) => {
                for part in parts {
                    self.lower_escaped(part, out);
                }
            }
            Some(op) => unreachable!("a held back string is a constant or a concat, not {op:?}"),
            None => out.push(WriterStmt::WriteString(self.name(name))),
        }
    }

    /// Write the Html an op produces.
    fn lower_html_op(&mut self, op: FlatOp, out: &mut Vec<WriterStmt>) {
        match op {
            FlatOp::HtmlText(content) => write(out, content.as_str()),

            FlatOp::HtmlEscape(string) => self.lower_escaped(string, out),

            FlatOp::HtmlElement {
                element,
                attributes,
                children,
            } => {
                // The element's writes merge among themselves first, so a
                // constant write grows across an element boundary only where
                // the whole element fits.
                let mut unit = Vec::new();
                write(&mut unit, &format!("<{}", element.as_str()));
                for attribute in attributes {
                    match attribute {
                        FlatAttribute::Value { name, value } => {
                            write(&mut unit, &format!(" {}=\"", name.as_str()));
                            self.lower_escaped(value, &mut unit);
                            write(&mut unit, "\"");
                        }
                        // A constant condition settles now whether the
                        // attribute renders.
                        FlatAttribute::Presence { name, present } => {
                            match self.pending.remove(&present) {
                                Some(FlatOp::BoolLiteral(true)) => {
                                    write(&mut unit, &format!(" {}", name.as_str()));
                                }
                                Some(FlatOp::BoolLiteral(false)) => {}
                                Some(op) => {
                                    unreachable!("a held back presence is a constant, not {op:?}")
                                }
                                None => {
                                    let mut true_body = Vec::new();
                                    write(&mut true_body, &format!(" {}", name.as_str()));
                                    unit.push(WriterStmt::Match(Match::Bool {
                                        subject: Box::new(self.name(present)),
                                        true_body: Box::new(true_body),
                                        false_body: Box::new(Vec::new()),
                                    }));
                                }
                            }
                        }
                    }
                }
                write(&mut unit, ">");
                if element.is_void() {
                    // A void element renders without its children, which
                    // are empty.
                    self.pending.remove(&children);
                } else {
                    self.lower_html(children, &mut unit);
                    write(&mut unit, &format!("</{}>", element.as_str()));
                }
                for statement in unit {
                    match statement {
                        WriterStmt::Write(content) => write(out, &content),
                        statement => out.push(statement),
                    }
                }
            }

            FlatOp::HtmlConcat(parts) => {
                for part in parts {
                    self.lower_html(part, out);
                }
            }

            FlatOp::HtmlFor { var, source, body } => {
                let source = match source {
                    FlatForSource::Array(array) => WriterForSource::Array(self.name(array)),
                    FlatForSource::RangeInclusive { start, end } => {
                        WriterForSource::RangeInclusive {
                            start: self.name(start),
                            end: self.name(end),
                        }
                    }
                };
                let mut statements = Vec::new();
                self.lower_output_block(body, &mut statements);
                out.push(WriterStmt::For {
                    var,
                    source,
                    body: statements,
                });
            }

            FlatOp::Match(match_) => out.push(WriterStmt::Match(match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => {
                    let mut true_statements = Vec::new();
                    self.lower_output_block(*true_body, &mut true_statements);
                    let mut false_statements = Vec::new();
                    self.lower_output_block(*false_body, &mut false_statements);
                    Match::Bool {
                        subject: Box::new(self.name(*subject)),
                        true_body: Box::new(true_statements),
                        false_body: Box::new(false_statements),
                    }
                }
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let mut some_statements = Vec::new();
                    self.lower_output_block(*some_arm_body, &mut some_statements);
                    let mut none_statements = Vec::new();
                    self.lower_output_block(*none_arm_body, &mut none_statements);
                    Match::Option {
                        subject: Box::new(self.name(*subject)),
                        some_arm_binding,
                        some_arm_body: Box::new(some_statements),
                        none_arm_body: Box::new(none_statements),
                    }
                }
                Match::Enum { subject, arms } => Match::Enum {
                    subject: Box::new(self.name(*subject)),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            let mut statements = Vec::new();
                            self.lower_output_block(arm.body, &mut statements);
                            EnumMatchArm {
                                pattern: arm.pattern,
                                bindings: arm.bindings,
                                body: statements,
                            }
                        })
                        .collect(),
                },
            })),

            FlatOp::Call { function, args } => out.push(WriterStmt::WriteFunction {
                function,
                args: args.into_iter().map(|arg| self.name(arg)).collect(),
            }),

            op => unreachable!("{op:?} produces no Html"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::{ComparableType, EquatableType, NumericType};
    use crate::ir::flat_optimizer::optimize_flat;
    use crate::ir::ir_binary_op::IrBinaryOp;
    use crate::ir::ir_unary_op::IrUnaryOp;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::pure_to_flat::pure_to_flat;

    use expect_test::{Expect, expect};

    /// The type of a name in scope.
    fn type_of(names: &[(WriterName, Type)], name: WriterName) -> &Type {
        let Some((_, typ)) = names.iter().find(|(bound, _)| *bound == name) else {
            panic!("{name} is not in scope");
        };
        typ
    }

    /// Asserts that every name a statement reads was bound before it in
    /// its block or an enclosing one, that the types a value writes agree
    /// with the names it reads and with its own let, and that a loop
    /// variable or option binding has the type of the elements of its
    /// source or subject.
    fn check_stmts(stmts: &[WriterStmt], names: &mut Vec<(WriterName, Type)>) {
        let names_len = names.len();
        for stmt in stmts {
            let scope_len = names.len();
            match stmt {
                WriterStmt::Let(let_) => {
                    if let Some(expected) = check_op(&let_.op, names) {
                        assert_eq!(let_.typ, expected, "{} has the wrong type", let_.name);
                    }
                    names.push((WriterName::Binding(let_.name), let_.typ.clone()));
                }
                WriterStmt::Write(_) => {}
                WriterStmt::WriteString(name) => assert_eq!(type_of(names, *name), &Type::String),
                WriterStmt::WriteHtml(name) => assert_eq!(type_of(names, *name), &Type::Html),
                WriterStmt::WriteFunction { args, .. } => {
                    for arg in args {
                        type_of(names, *arg);
                    }
                }
                WriterStmt::For { var, source, body } => {
                    let element_type = match source {
                        WriterForSource::Array(array) => match type_of(names, *array) {
                            Type::Array(element_type) => (**element_type).clone(),
                            typ => panic!("a loop over {array} of type {typ}"),
                        },
                        WriterForSource::RangeInclusive { start, end } => {
                            assert_eq!(type_of(names, *start), &Type::Int);
                            assert_eq!(type_of(names, *end), &Type::Int);
                            Type::Int
                        }
                    };
                    if let Some(binder) = var {
                        assert_eq!(
                            binder.typ, element_type,
                            "{} has the wrong type",
                            binder.var
                        );
                        names.push((WriterName::Binder(binder.var), binder.typ.clone()));
                    }
                    check_stmts(body, names);
                    names.truncate(scope_len);
                }
                WriterStmt::Match(Match::Bool {
                    subject,
                    true_body,
                    false_body,
                }) => {
                    assert_eq!(type_of(names, **subject), &Type::Bool);
                    check_stmts(true_body, names);
                    check_stmts(false_body, names);
                }
                WriterStmt::Match(Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                }) => {
                    let Type::Option(inner) = type_of(names, **subject).clone() else {
                        panic!("a match on {subject}, which is not an Option");
                    };
                    if let Some(binder) = some_arm_binding {
                        assert_eq!(binder.typ, *inner, "{} has the wrong type", binder.var);
                        names.push((WriterName::Binder(binder.var), binder.typ.clone()));
                    }
                    check_stmts(some_arm_body, names);
                    names.truncate(scope_len);
                    check_stmts(none_arm_body, names);
                }
                WriterStmt::Match(Match::Enum { subject, arms }) => {
                    type_of(names, **subject);
                    for arm in arms {
                        for (_, binder) in &arm.bindings {
                            names.push((WriterName::Binder(binder.var), binder.typ.clone()));
                        }
                        check_stmts(&arm.body, names);
                        names.truncate(scope_len);
                    }
                }
            }
        }
        names.truncate(names_len);
    }

    /// Checks the lets and returns the type of the result.
    fn check_value_block(block: &WriterValueBlock, names: &mut Vec<(WriterName, Type)>) -> Type {
        let names_len = names.len();
        for let_ in &block.lets {
            if let Some(expected) = check_op(&let_.op, names) {
                assert_eq!(let_.typ, expected, "{} has the wrong type", let_.name);
            }
            names.push((WriterName::Binding(let_.name), let_.typ.clone()));
        }
        let result = type_of(names, block.result).clone();
        names.truncate(names_len);
        result
    }

    /// Checks the op and returns the type it must have, when the op
    /// determines it.
    fn check_op(op: &WriterOp, names: &mut Vec<(WriterName, Type)>) -> Option<Type> {
        let names_len = names.len();
        op.for_each_operand(&mut |operand| {
            type_of(names, operand);
        });
        match op {
            WriterOp::Binary {
                op: IrBinaryOp::NumericAdd(operand_types),
                left,
                right,
            }
            | WriterOp::Binary {
                op: IrBinaryOp::NumericSubtract(operand_types),
                left,
                right,
            }
            | WriterOp::Binary {
                op: IrBinaryOp::NumericMultiply(operand_types),
                left,
                right,
            } => {
                let typ = match operand_types {
                    NumericType::Int => Type::Int,
                    NumericType::Float => Type::Float,
                };
                assert_eq!(type_of(names, *left), &typ);
                assert_eq!(type_of(names, *right), &typ);
                Some(typ)
            }
            WriterOp::Unary {
                op: IrUnaryOp::NumericNegation(operand_type),
                operand,
            } => {
                let typ = match operand_type {
                    NumericType::Int => Type::Int,
                    NumericType::Float => Type::Float,
                };
                assert_eq!(type_of(names, *operand), &typ);
                Some(typ)
            }
            WriterOp::Binary {
                op: IrBinaryOp::Equals(operand_types),
                left,
                right,
            } => {
                let typ = match operand_types {
                    EquatableType::String => Type::String,
                    EquatableType::Bool => Type::Bool,
                    EquatableType::Int => Type::Int,
                    EquatableType::Float => Type::Float,
                };
                assert_eq!(type_of(names, *left), &typ);
                assert_eq!(type_of(names, *right), &typ);
                Some(Type::Bool)
            }
            WriterOp::Binary {
                op: IrBinaryOp::LessThan(operand_types),
                left,
                right,
            }
            | WriterOp::Binary {
                op: IrBinaryOp::LessThanOrEqual(operand_types),
                left,
                right,
            } => {
                let typ = match operand_types {
                    ComparableType::Int => Type::Int,
                    ComparableType::Float => Type::Float,
                };
                assert_eq!(type_of(names, *left), &typ);
                assert_eq!(type_of(names, *right), &typ);
                Some(Type::Bool)
            }
            WriterOp::HtmlLiteral(body) => {
                check_stmts(body, names);
                Some(Type::Html)
            }
            WriterOp::Match(Match::Bool {
                subject,
                true_body,
                false_body,
            }) => {
                assert_eq!(type_of(names, **subject), &Type::Bool);
                let true_type = check_value_block(true_body, names);
                let false_type = check_value_block(false_body, names);
                assert_eq!(true_type, false_type);
                Some(true_type)
            }
            WriterOp::Match(Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            }) => {
                let Type::Option(inner) = type_of(names, **subject).clone() else {
                    panic!("a match on {subject}, which is not an Option");
                };
                if let Some(binder) = some_arm_binding {
                    assert_eq!(binder.typ, *inner, "{} has the wrong type", binder.var);
                    names.push((WriterName::Binder(binder.var), binder.typ.clone()));
                }
                let some_type = check_value_block(some_arm_body, names);
                names.truncate(names_len);
                let none_type = check_value_block(none_arm_body, names);
                assert_eq!(some_type, none_type);
                Some(some_type)
            }
            WriterOp::Match(Match::Enum { arms, .. }) => {
                let mut arm_type = None;
                for arm in arms {
                    for (_, binder) in &arm.bindings {
                        names.push((WriterName::Binder(binder.var), binder.typ.clone()));
                    }
                    let typ = check_value_block(&arm.body, names);
                    names.truncate(names_len);
                    if let Some(arm_type) = &arm_type {
                        assert_eq!(arm_type, &typ);
                    }
                    arm_type = Some(typ);
                }
                arm_type
            }
            _ => None,
        }
    }

    #[test]
    fn fuzz_random_modules_lower_to_well_scoped_statements() {
        arbtest::arbtest(|u| {
            let (module, _) = random_module(u);
            let module = flat_to_writer(optimize_flat(pure_to_flat(module)), None);
            for page in &module.pages {
                let mut names: Vec<(WriterName, Type)> = page
                    .parameters
                    .iter()
                    .map(|param| (WriterName::Binder(param.var), param.typ.clone()))
                    .collect();
                check_stmts(&page.body, &mut names);
            }
            for function in &module.functions {
                let mut names: Vec<(WriterName, Type)> = function
                    .parameters
                    .iter()
                    .map(|param| (WriterName::Binder(param.var), param.typ.clone()))
                    .collect();
                match &function.body {
                    WriterFunctionBody::Writes(statements) => {
                        check_stmts(statements, &mut names);
                    }
                    WriterFunctionBody::Returns(block) => {
                        let result = check_value_block(block, &mut names);
                        assert_eq!(result, function.return_type);
                    }
                }
            }
            Ok(())
        });
    }

    /// Prints the module through the Writer lowering and through the Flat IR and
    /// the Writer lowering, so the two can be compared side by side.
    fn check(build: impl Fn() -> PureModule, shell: Option<&DocumentShell>, expected: Expect) {
        let module = build();
        let pure = module.to_string();
        let writer = flat_to_writer(optimize_flat(pure_to_flat(module)), shell).to_string();
        expected.assert_eq(&format!("-- pure --\n{pure}\n-- writer --\n{writer}"));
    }

    #[test]
    fn writes_the_shell_around_the_page() {
        check(
            || {
                PureModuleBuilder::new()
                    .page_no_params("Main", |t| t.element("p", vec![], vec![t.text("Hello")]))
                    .build()
            },
            Some(&DocumentShell::new(None, Some("/scripts-deadbeef.js"))),
            expect![[r#"
                -- pure --
                page Main() {
                  html("p", {}, concat(text("Hello")))
                }

                -- writer --
                page Main() {
                  write("<!doctype html><html><head><meta charset=\"utf-8\"><meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">")
                  write("<script type=\"module\" src=\"/scripts-deadbeef.js\"></script></head><body>")
                  write("<p>Hello</p>")
                  write("</body></html>")
                }
            "#]],
        );
    }

    #[test]
    fn should_write_an_element_with_constant_attributes_as_one_write() {
        check(
            || {
                PureModuleBuilder::new()
                    .page_no_params("Test", |t| {
                        t.element(
                            "div",
                            vec![t.attr("class", t.str("base")), t.attr("id", t.str("a<b"))],
                            vec![t.text("Content")],
                        )
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test() {
                  html(
                    "div",
                    {class: "base", id: "a<b"},
                    concat(text("Content")),
                  )
                }

                -- writer --
                page Test() {
                  write("<div class=\"base\" id=\"a&lt;b\">Content</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_a_dynamic_attribute_value_when_rendering() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("cls", "String")], |t| {
                        t.element("div", vec![t.attr("data-value", t.var("cls"))], vec![])
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(cls@b0: String) {
                  html("div", {data-value: b0}, concat())
                }

                -- writer --
                page Test(cls@b0: String) {
                  write("<div data-value=\"")
                  write_string(b0)
                  write("\"></div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_settle_a_constant_presence_and_match_on_a_dynamic_one() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("flag", "Bool")], |t| {
                        t.element(
                            "input",
                            vec![
                                t.presence("disabled", t.bool(true)),
                                t.presence("hidden", t.bool(false)),
                                t.presence("checked", t.var("flag")),
                            ],
                            vec![],
                        )
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(flag@b0: Bool) {
                  html(
                    "input",
                    {disabled: true, hidden: false, checked: b0},
                  )
                }

                -- writer --
                page Test(flag@b0: Bool) {
                  write("<input disabled")
                  match b0 {
                    true => {
                      write(" checked")
                    }
                    false => {
                    }
                  }
                  write(">")
                }
            "#]],
        );
    }

    #[test]
    fn should_nest_elements() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("name", "String")], |t| {
                        t.element(
                            "div",
                            vec![],
                            vec![t.element("p", vec![], vec![t.escape(t.var("name"))])],
                        )
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(name@b0: String) {
                  html("div", {}, concat(html("p", {}, concat(escape(b0)))))
                }

                -- writer --
                page Test(name@b0: String) {
                  write("<div><p>")
                  write_string(b0)
                  write("</p></div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_the_constant_parts_of_a_concat_when_lowering() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("name", "String")], |t| {
                        t.escape(t.string_concat(vec![t.str("a<"), t.var("name"), t.str(">b")]))
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(name@b0: String) {
                  escape(concat("a<", b0, ">b"))
                }

                -- writer --
                page Test(name@b0: String) {
                  write("a&lt;")
                  write_string(b0)
                  write("&gt;b")
                }
            "#]],
        );
    }

    #[test]
    fn should_merge_adjacent_writes_while_they_stay_below_the_limit() {
        check(
            || {
                PureModuleBuilder::new()
                    .page_no_params("Test", |t| {
                        t.concat(vec![
                            t.text("aaaaaaaaaaaaaaaaaaaaaaaaa"),
                            t.text("bbbbbbbbbbbbbbbbbbbbbbbbb"),
                            t.text("ccccccccccccccccccccccccc"),
                            t.text("ddddddddddddddddddddddddd"),
                        ])
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test() {
                  concat(
                    text("aaaaaaaaaaaaaaaaaaaaaaaaa"),
                    text("bbbbbbbbbbbbbbbbbbbbbbbbb"),
                    text("ccccccccccccccccccccccccc"),
                    text("ddddddddddddddddddddddddd"),
                  )
                }

                -- writer --
                page Test() {
                  write("aaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbb")
                  write("cccccccccccccccccccccccccddddddddddddddddddddddddd")
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_the_writes_of_a_loop_body_apart_from_those_around_it() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("items", "Array[String]")], |t| {
                        t.concat(vec![
                            t.text("before "),
                            t.html_for(Some("item"), t.var("items"), |t| {
                                t.element("li", vec![], vec![t.escape(t.var("item"))])
                            }),
                            t.text(" after"),
                        ])
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(items@b0: Array[String]) {
                  concat(
                    text("before "),
                    for b1: String in b0 {
                      html("li", {}, concat(escape(b1)))
                    },
                    text(" after"),
                  )
                }

                -- writer --
                page Test(items@b0: Array[String]) {
                  write("before ")
                  for b1: String in b0 {
                    write("<li>")
                    write_string(b1)
                    write("</li>")
                  }
                  write(" after")
                }
            "#]],
        );
    }

    #[test]
    fn should_render_an_html_value_read_twice_once() {
        check(
            || {
                PureModuleBuilder::new()
                    .page_no_params("Test", |t| {
                        t.let_expr("x", t.element("b", vec![], vec![t.text("hi")]), |t| {
                            t.concat(vec![t.var("x"), t.var("x")])
                        })
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test() {
                  let b0: Html = html("b", {}, concat(text("hi"))) in {
                    concat(b0, b0)
                  }
                }

                -- writer --
                page Test() {
                  let v2: Html = html {
                    write("<b>hi</b>")
                  }
                  write_html(v2)
                  write_html(v2)
                }
            "#]],
        );
    }

    #[test]
    fn should_render_an_html_value_outside_the_loop_that_reads_it() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("items", "Array[String]")], |t| {
                        t.let_expr("x", t.element("b", vec![], vec![t.text("hi")]), |t| {
                            t.html_for(None, t.var("items"), |t| t.var("x"))
                        })
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(items@b0: Array[String]) {
                  let b1: Html = html("b", {}, concat(text("hi"))) in {
                    for _ in b0 { b1 }
                  }
                }

                -- writer --
                page Test(items@b0: Array[String]) {
                  let v2: Html = html {
                    write("<b>hi</b>")
                  }
                  for _ in b0 {
                    write_html(v2)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_pass_html_to_a_function_as_a_literal_and_write_the_call() {
        check(
            || {
                PureModuleBuilder::new()
                    .function("wrap", [("inner", "Html")], "Html", |t| {
                        t.element("div", vec![], vec![t.var("inner")])
                    })
                    .page_no_params("Test", |t| {
                        t.call(
                            "wrap",
                            vec![("inner", t.element("b", vec![], vec![t.text("hi")]))],
                        )
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                fn wrap@f0(inner@b0: Html) -> Html {
                  html("div", {}, concat(b0))
                }
                page Test() {
                  call wrap@f0(html("b", {}, concat(text("hi"))))
                }

                -- writer --
                page Test() {
                  write("<div><b>hi</b></div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_return_lets_and_a_result_from_a_value_function() {
        check(
            || {
                PureModuleBuilder::new()
                    .function("square_next", [("x", "Int")], "Int", |t| {
                        t.let_expr("y", t.add(t.var("x"), t.int(1)), |t| {
                            t.mul(t.var("y"), t.var("y"))
                        })
                    })
                    .page_no_params("Test", |t| {
                        t.escape(t.int_to_string(t.call("square_next", vec![("x", t.int(2))])))
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                fn square_next@f0(x@b0: Int) -> Int {
                  let b1: Int = (b0 + 1) in { (b1 * b1) }
                }
                page Test() {
                  escape(call square_next@f0(2).to_string())
                }

                -- writer --
                page Test() {
                  write("9")
                }
            "#]],
        );
    }

    #[test]
    fn should_render_an_html_match_held_in_a_record_into_a_literal() {
        check(
            || {
                PureModuleBuilder::new()
                    .record("Card", [("body", "Html")])
                    .page("Test", [("flag", "Bool")], |t| {
                        t.let_expr(
                            "card",
                            t.record(
                                "Card",
                                vec![(
                                    "body",
                                    t.bool_match_expr(t.var("flag"), t.text("a"), t.text("b")),
                                )],
                            ),
                            |t| t.field_access(t.var("card"), "body"),
                        )
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(flag@b0: Bool) {
                  let b1: Card = Card {
                    body: match b0 {
                      true => {
                        text("a")
                      }
                      false => {
                        text("b")
                      }
                    },
                  } in {
                    b1.body
                  }
                }

                -- writer --
                page Test(flag@b0: Bool) {
                  match b0 {
                    true => {
                      write("a")
                    }
                    false => {
                      write("b")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_match_on_a_value_with_lets_in_its_arms() {
        check(
            || {
                PureModuleBuilder::new()
                    .page("Test", [("flag", "Bool"), ("n", "Int")], |t| {
                        t.escape(t.int_to_string(t.bool_match_expr(
                            t.var("flag"),
                            t.add(t.var("n"), t.int(1)),
                            t.int(0),
                        )))
                    })
                    .build()
            },
            None,
            expect![[r#"
                -- pure --
                page Test(flag@b0: Bool, n@b1: Int) {
                  escape(match b0 {
                    true => {
                      (b1 + 1)
                    }
                    false => {
                      0
                    }
                  }.to_string())
                }

                -- writer --
                page Test(flag@b0: Bool, n@b1: Int) {
                  let v5: Int = match b0 {
                    true => {
                      let v2: Int = 1
                      let v3: Int = b1 + v2
                      v3
                    }
                    false => {
                      let v4: Int = 0
                      v4
                    }
                  }
                  let v6: String = v5.to_string()
                  write_string(v6)
                }
            "#]],
        );
    }
}
