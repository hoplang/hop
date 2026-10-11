use std::collections::{HashMap, HashSet};

use crate::hop::typing::{
    CaseVar, Constructor, Decision, Pattern, PatternField, Type, TypeRegistry,
    compile_decision_tree,
};
use crate::ir::binder_id::{BinderId, BinderIdCounter};
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatFunctionDeclaration, FlatModule, FlatOp};
use crate::ir::flat_transform::eliminate_dead_bindings::eliminate_dead_bindings;
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// A pass that lowers every match to switches, matches whose arms each
/// test one bool, option or enum constructor of the subject and bind or
/// ignore what it holds.
///
/// The patterns of a match compile to a decision tree. A switch of the tree
/// becomes a switch on the part of the subject it tests, a record or tuple
/// the tree takes apart becomes field accesses and tuple indexes, and a
/// leaf becomes a copy of the arm it selects, with fresh names, that reads
/// the parts its pattern binds. An arm that several leaves select is copied
/// once for each of them.
///
/// A record or tuple that a binding constructs is taken apart by reading
/// the constructor's operands, which can leave the constructor unread, so
/// the bindings nothing reads are dropped at the end.
pub fn lower_matches(module: FlatModule, registry: &TypeRegistry) -> FlatModule {
    let mut lowering = Lowering {
        registry,
        value_ids: module.value_ids,
        binder_ids: module.binder_ids,
        types: HashMap::new(),
        constructors: HashMap::new(),
        renames: HashMap::new(),
    };
    let functions = module
        .functions
        .into_iter()
        .map(|function| FlatFunctionDeclaration {
            function: function.function,
            entry: function.entry,
            parameters: function.parameters,
            return_type: function.return_type,
            body: eliminate_dead_bindings(lowering.lower_block(function.body)),
        })
        .collect();
    FlatModule {
        functions,
        value_ids: lowering.value_ids,
        binder_ids: lowering.binder_ids,
    }
}

struct Lowering<'a> {
    registry: &'a TypeRegistry,
    value_ids: ValueIdCounter,
    binder_ids: BinderIdCounter,
    /// The type of every name bound so far. Names are unique, so one map
    /// serves every body.
    types: HashMap<ValueId, Type>,
    /// The record and tuple constructors among the bindings so far.
    constructors: HashMap<ValueId, FlatOp>,
    /// The result each lowered match is read through.
    renames: HashMap<ValueId, ValueId>,
}

impl Lowering<'_> {
    fn resolve(&self, name: ValueId) -> ValueId {
        self.renames.get(&name).copied().unwrap_or(name)
    }

    fn lower_block(&mut self, block: FlatBlock) -> FlatBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.lower_binding(binding, &mut bindings);
        }
        FlatBlock {
            bindings,
            result: self.resolve(block.result),
        }
    }

    fn lower_binding(&mut self, binding: FlatBinding, out: &mut Vec<FlatBinding>) {
        let FlatBinding { name, typ, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        let op = match op {
            FlatOp::Match(IrMatch { subject, arms }) => {
                let (patterns, bodies): (Vec<_>, Vec<_>) = arms
                    .into_iter()
                    .map(|arm| (arm.pattern, self.lower_block(arm.body)))
                    .unzip();
                let decision =
                    compile_decision_tree(self.registry, &patterns, self.types[&subject].clone());
                let mut used = HashSet::new();
                decision.collect_used(&mut used);
                let mut parts = HashMap::from([(CaseVar(0), subject)]);
                let result = self.lower_decision(&decision, &bodies, &typ, &used, &mut parts, out);
                self.types.insert(result, typ);
                self.renames.insert(name, result);
                return;
            }
            FlatOp::HtmlFor { var, source, body } => FlatOp::HtmlFor {
                var,
                source,
                body: self.lower_block(body),
            },
            op @ (FlatOp::Record { .. } | FlatOp::Tuple(_)) => {
                self.constructors.insert(name, op.clone());
                op
            }
            op => op,
        };
        self.types.insert(name, typ.clone());
        out.push(FlatBinding { name, typ, op });
    }

    /// Lower a decision tree of a match of type `typ` whose arms are
    /// `bodies`, appending its bindings to `out`, and return the name of
    /// its result. `parts` holds the name of the part of the subject each
    /// case variable bound so far stands for. A case variable that is not
    /// in `used` is not bound.
    fn lower_decision(
        &mut self,
        decision: &Decision<BinderId>,
        bodies: &[FlatBlock],
        typ: &Type,
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        out: &mut Vec<FlatBinding>,
    ) -> ValueId {
        let (subject, arms) = match decision {
            Decision::Success(body) => {
                let arguments = body
                    .bindings
                    .iter()
                    .map(|binding| (binding.name, parts[&binding.source]))
                    .collect();
                let copy = Freshener::new(&mut self.value_ids, &mut self.binder_ids, arguments)
                    .freshen_block(&bodies[body.value]);
                out.extend(copy.bindings);
                return copy.result;
            }

            Decision::SwitchRecord { variable, case } => {
                let record = parts[&variable.id];
                for binding in &case.bindings {
                    if !used.contains(&binding.var.id) {
                        continue;
                    }
                    let part = match self.constructors.get(&record) {
                        Some(FlatOp::Record { fields }) => fields
                            .iter()
                            .find(|(field, _)| *field == binding.field_name)
                            .map(|(_, value)| *value)
                            .unwrap_or_else(|| {
                                panic!("record has no field {}", binding.field_name.as_str())
                            }),
                        _ => self.bind(
                            binding.var.typ.clone(),
                            FlatOp::FieldAccess {
                                record,
                                field: binding.field_name.clone(),
                            },
                            out,
                        ),
                    };
                    parts.insert(binding.var.id, part);
                }
                return self.lower_decision(&case.body, bodies, typ, used, parts, out);
            }

            Decision::SwitchTuple { variable, case } => {
                let tuple = parts[&variable.id];
                for (index, element) in case.elements.iter().enumerate() {
                    if !used.contains(&element.id) {
                        continue;
                    }
                    let part = match self.constructors.get(&tuple) {
                        Some(FlatOp::Tuple(elements)) => elements[index],
                        _ => self.bind(
                            element.typ.clone(),
                            FlatOp::TupleIndex { tuple, index },
                            out,
                        ),
                    };
                    parts.insert(element.id, part);
                }
                return self.lower_decision(&case.body, bodies, typ, used, parts, out);
            }

            Decision::SwitchBool {
                variable,
                true_case,
                false_case,
            } => {
                let mut arms = Vec::new();
                for (constructor, body) in [
                    (Constructor::BoolTrue, &true_case.body),
                    (Constructor::BoolFalse, &false_case.body),
                ] {
                    let mut bindings = Vec::new();
                    let result = self.lower_decision(body, bodies, typ, used, parts, &mut bindings);
                    arms.push(IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: FlatBlock { bindings, result },
                    });
                }
                (parts[&variable.id], arms)
            }

            Decision::SwitchOption {
                variable,
                some_case,
                none_case,
            } => {
                let mut some_bindings = Vec::new();
                let inner = self.bind_part(
                    some_case.var.id,
                    &some_case.var.typ,
                    used,
                    parts,
                    &mut some_bindings,
                );
                let some_result = self.lower_decision(
                    &some_case.body,
                    bodies,
                    typ,
                    used,
                    parts,
                    &mut some_bindings,
                );
                let mut none_bindings = Vec::new();
                let none_result = self.lower_decision(
                    &none_case.body,
                    bodies,
                    typ,
                    used,
                    parts,
                    &mut none_bindings,
                );
                let arms = vec![
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::OptionSome,
                            args: vec![inner],
                            fields: Vec::new(),
                        },
                        body: FlatBlock {
                            bindings: some_bindings,
                            result: some_result,
                        },
                    },
                    IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::OptionNone,
                            args: Vec::new(),
                            fields: Vec::new(),
                        },
                        body: FlatBlock {
                            bindings: none_bindings,
                            result: none_result,
                        },
                    },
                ];
                (parts[&variable.id], arms)
            }

            Decision::SwitchEnum { variable, cases } => {
                let mut arms = Vec::new();
                for case in cases {
                    let mut bindings = Vec::new();
                    let fields = case
                        .bindings
                        .iter()
                        .enumerate()
                        .map(|(index, binding)| PatternField {
                            name: binding.field_name.clone(),
                            index,
                            pattern: self.bind_part(
                                binding.var.id,
                                &binding.var.typ,
                                used,
                                parts,
                                &mut bindings,
                            ),
                        })
                        .collect();
                    let result =
                        self.lower_decision(&case.body, bodies, typ, used, parts, &mut bindings);
                    arms.push(IrMatchArm {
                        pattern: Pattern::Constructor {
                            constructor: Constructor::EnumVariant {
                                type_name: case.type_name.clone(),
                                variant_name: case.variant_name.clone(),
                            },
                            args: Vec::new(),
                            fields,
                        },
                        body: FlatBlock { bindings, result },
                    });
                }
                (parts[&variable.id], arms)
            }
        };
        self.bind(typ.clone(), FlatOp::Match(IrMatch { subject, arms }), out)
    }

    /// The pattern for a part of the subject that an arm of a switch
    /// holds. A used part is bound to a fresh binder, which a Read at the
    /// start of the arm's bindings makes the name of the part.
    fn bind_part(
        &mut self,
        part: CaseVar,
        typ: &Type,
        used: &HashSet<CaseVar>,
        parts: &mut HashMap<CaseVar, ValueId>,
        bindings: &mut Vec<FlatBinding>,
    ) -> IrPattern {
        if !used.contains(&part) {
            return Pattern::Wildcard;
        }
        let binder = self.binder_ids.next();
        let read = self.bind(typ.clone(), FlatOp::Read(binder), bindings);
        parts.insert(part, read);
        Pattern::Binding {
            name: binder,
            typ: typ.clone(),
        }
    }

    /// Append a binding of the op to a fresh name, and return the name.
    fn bind(&mut self, typ: Type, op: FlatOp, out: &mut Vec<FlatBinding>) -> ValueId {
        let name = self.value_ids.next();
        self.types.insert(name, typ.clone());
        out.push(FlatBinding { name, typ, op });
        name
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::ir::flat_optimizer::optimize_flat;
    use crate::ir::pure_module_generator::{random_entry_args, random_module};
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entry;
    use crate::ir::runtime::value::Value;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_lowering_matches() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entry_args = random_entry_args(&module, &mut rng, &registry);
            let module = pure_to_flat(module);
            let module = if u.arbitrary()? {
                optimize_flat(module)
            } else {
                module
            };

            let before_module = module.to_string();
            let before: Vec<Result<String, EvalError>> = entry_args
                .iter()
                .map(|(function, args)| {
                    evaluate_entry(&module, function, args.clone()).map(Value::into_markup)
                })
                .collect();

            let module = lower_matches(module, &registry);

            for ((function, args), before) in entry_args.iter().zip(before) {
                // An entry that hit the call depth limit has no output to
                // preserve, and dropping the bindings nothing reads may drop
                // the diverging call.
                let Ok(before) = before else {
                    continue;
                };
                let after = evaluate_entry(&module, function, args.clone())
                    .unwrap()
                    .into_markup();
                assert_eq!(
                    before, after,
                    "entry {function}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    /// Prints the Flat IR of the source before and after its matches are
    /// lowered.
    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (pure, _) = orchestrate_pure(program.typed_modules(), OrchestrateOptions::default());
        let module = pure_to_flat(pure);
        let before = module.to_string();
        let after = lower_matches(module, program.type_registry()).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn copies_an_arm_once_for_each_leaf_that_selects_it() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                page Test(color: Color) {
                  fn body() -> Html {
                    match color {
                      Color::Red => <p>stop</p>,
                      _ => <input/>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entry fn body@f0(color@b0: Color) -> Html {
                  let v0: Color = b0
                  let v6: Html = match v0 {
                    Color::Red => {
                      let v1: Html = text("stop")
                      let v2: Html = concat(v1)
                      let v3: Html = html("p", {}, v2)
                      v3
                    }
                    _ => {
                      let v4: Html = concat()
                      let v5: Html = html("input", {}, v4)
                      v5
                    }
                  }
                  v6
                }

                -- after --
                entry fn body@f0(color@b0: Color) -> Html {
                  let v0: Color = b0
                  let v14: Html = match v0 {
                    Color::Red => {
                      let v7: Html = text("stop")
                      let v8: Html = concat(v7)
                      let v9: Html = html("p", {}, v8)
                      v9
                    }
                    Color::Green => {
                      let v10: Html = concat()
                      let v11: Html = html("input", {}, v10)
                      v11
                    }
                    Color::Blue => {
                      let v12: Html = concat()
                      let v13: Html = html("input", {}, v12)
                      v13
                    }
                  }
                  v14
                }
            "#]],
        );
    }

    #[test]
    fn takes_apart_a_tuple_the_block_constructs_through_its_operands() {
        check(
            indoc! {r#"
                page Test(signed_in: Bool, nickname: Option[String]) {
                  fn body() -> Html {
                    match (signed_in, nickname) {
                      (true, Some(name)) => <p>{name}</p>,
                      (true, None) => <p>member</p>,
                      (false, _) => <p>guest</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entry fn body@f0(signed_in@b0: Bool, nickname@b1: Option[String]) -> Html {
                  let v0: Bool = b0
                  let v1: Option[String] = b1
                  let v2: (Bool, Option[String]) = (v0, v1)
                  let v13: Html = match v2 {
                    (true, Some(b2: String)) => {
                      let v3: String = b2
                      let v4: Html = escape(v3)
                      let v5: Html = concat(v4)
                      let v6: Html = html("p", {}, v5)
                      v6
                    }
                    (true, None) => {
                      let v7: Html = text("member")
                      let v8: Html = concat(v7)
                      let v9: Html = html("p", {}, v8)
                      v9
                    }
                    (false, _) => {
                      let v10: Html = text("guest")
                      let v11: Html = concat(v10)
                      let v12: Html = html("p", {}, v11)
                      v12
                    }
                  }
                  v13
                }

                -- after --
                entry fn body@f0(signed_in@b0: Bool, nickname@b1: Option[String]) -> Html {
                  let v0: Bool = b0
                  let v1: Option[String] = b1
                  let v25: Html = match v0 {
                    true => {
                      let v21: Html = match v1 {
                        Some(b3: String) => {
                          let v14: String = b3
                          let v15: Html = escape(v14)
                          let v16: Html = concat(v15)
                          let v17: Html = html("p", {}, v16)
                          v17
                        }
                        None => {
                          let v18: Html = text("member")
                          let v19: Html = concat(v18)
                          let v20: Html = html("p", {}, v19)
                          v20
                        }
                      }
                      v21
                    }
                    false => {
                      let v22: Html = text("guest")
                      let v23: Html = concat(v22)
                      let v24: Html = html("p", {}, v23)
                      v24
                    }
                  }
                  v25
                }
            "#]],
        );
    }

    #[test]
    fn reads_the_fields_of_a_record_it_does_not_construct() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  admin: Bool,
                }

                page Test(user: User) {
                  fn body() -> Html {
                    match user {
                      User {name, admin: true} => <b>{name}</b>,
                      User {name, admin: false} => <p>{name}</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entry fn body@f0(user@b0: User) -> Html {
                  let v0: User = b0
                  let v9: Html = match v0 {
                    User {name@b1: String, admin: true} => {
                      let v1: String = b1
                      let v2: Html = escape(v1)
                      let v3: Html = concat(v2)
                      let v4: Html = html("b", {}, v3)
                      v4
                    }
                    User {name@b2: String, admin: false} => {
                      let v5: String = b2
                      let v6: Html = escape(v5)
                      let v7: Html = concat(v6)
                      let v8: Html = html("p", {}, v7)
                      v8
                    }
                  }
                  v9
                }

                -- after --
                entry fn body@f0(user@b0: User) -> Html {
                  let v0: User = b0
                  let v10: String = v0.name
                  let v11: Bool = v0.admin
                  let v18: Html = match v11 {
                    true => {
                      let v12: Html = escape(v10)
                      let v13: Html = concat(v12)
                      let v14: Html = html("b", {}, v13)
                      v14
                    }
                    false => {
                      let v15: Html = escape(v10)
                      let v16: Html = concat(v15)
                      let v17: Html = html("p", {}, v16)
                      v17
                    }
                  }
                  v18
                }
            "#]],
        );
    }

    #[test]
    fn reads_a_part_a_switch_binds_at_the_start_of_its_arm() {
        check(
            indoc! {r#"
                page Test(nested: Option[Option[String]]) {
                  fn body() -> Html {
                    match nested {
                      Some(Some(text)) => <p>{text}</p>,
                      Some(None) => <p>empty</p>,
                      None => <p>missing</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entry fn body@f0(nested@b0: Option[Option[String]]) -> Html {
                  let v0: Option[Option[String]] = b0
                  let v11: Html = match v0 {
                    Some(Some(b1: String)) => {
                      let v1: String = b1
                      let v2: Html = escape(v1)
                      let v3: Html = concat(v2)
                      let v4: Html = html("p", {}, v3)
                      v4
                    }
                    Some(None) => {
                      let v5: Html = text("empty")
                      let v6: Html = concat(v5)
                      let v7: Html = html("p", {}, v6)
                      v7
                    }
                    None => {
                      let v8: Html = text("missing")
                      let v9: Html = concat(v8)
                      let v10: Html = html("p", {}, v9)
                      v10
                    }
                  }
                  v11
                }

                -- after --
                entry fn body@f0(nested@b0: Option[Option[String]]) -> Html {
                  let v0: Option[Option[String]] = b0
                  let v24: Html = match v0 {
                    Some(b2: Option[String]) => {
                      let v12: Option[String] = b2
                      let v20: Html = match v12 {
                        Some(b3: String) => {
                          let v13: String = b3
                          let v14: Html = escape(v13)
                          let v15: Html = concat(v14)
                          let v16: Html = html("p", {}, v15)
                          v16
                        }
                        None => {
                          let v17: Html = text("empty")
                          let v18: Html = concat(v17)
                          let v19: Html = html("p", {}, v18)
                          v19
                        }
                      }
                      v20
                    }
                    None => {
                      let v21: Html = text("missing")
                      let v22: Html = concat(v21)
                      let v23: Html = html("p", {}, v22)
                      v23
                    }
                  }
                  v24
                }
            "#]],
        );
    }

    #[test]
    fn runs_the_arm_of_a_match_that_binds_without_testing() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  admin: Bool,
                }

                page Test(user: User) {
                  fn body() -> Html {
                    match user {
                      User {name, admin: _} => <p>{name}</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entry fn body@f0(user@b0: User) -> Html {
                  let v0: User = b0
                  let v5: Html = match v0 {
                    User {name@b1: String, admin: _} => {
                      let v1: String = b1
                      let v2: Html = escape(v1)
                      let v3: Html = concat(v2)
                      let v4: Html = html("p", {}, v3)
                      v4
                    }
                  }
                  v5
                }

                -- after --
                entry fn body@f0(user@b0: User) -> Html {
                  let v0: User = b0
                  let v6: String = v0.name
                  let v7: Html = escape(v6)
                  let v8: Html = concat(v7)
                  let v9: Html = html("p", {}, v8)
                  v9
                }
            "#]],
        );
    }
}
