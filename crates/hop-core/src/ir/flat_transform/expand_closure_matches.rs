use crate::ir::flat_program::{
    FlatDataBinding, FlatDataBlock, FlatDataFunction, FlatDataOp, FlatFunctionDeclaration,
    FlatHtmlBinding, FlatHtmlBlock, FlatHtmlFunction, FlatHtmlOp, FlatProgram,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::ir_type::{IrHtmlType, IrValueType};
use crate::ir::value_id::{ValueId, ValueIdCounter};

// A pass that turns a match whose arms produce closures into one lambda,
// whose body runs the match and applies the closure of the arm it takes,
// so that every closure left is a lambda or a parameter, as `flat_to_writer`
// needs, which explains why. An arm then runs on each application instead
// of once, which costs time but changes no result, since every op is pure.
//
// The arms move into the lambda, so they keep their names. A closure reads
// no Html bound outside it, so the plain Html an arm of such a match binds
// is never read, and goes away rather than end up in the lambda.
pub fn expand_closure_matches(program: FlatProgram) -> FlatProgram {
    let FlatProgram {
        types,
        functions,
        mut value_ids,
    } = program;
    let mut expander = Expander {
        value_ids: &mut value_ids,
    };
    let functions = functions
        .into_iter()
        .map(|function| match function {
            FlatFunctionDeclaration::Data(function) => {
                FlatFunctionDeclaration::Data(FlatDataFunction {
                    body: expander.data_block(function.body),
                    ..function
                })
            }
            FlatFunctionDeclaration::Html(function) => {
                FlatFunctionDeclaration::Html(FlatHtmlFunction {
                    body: expander.html_block(function.body),
                    ..function
                })
            }
        })
        .collect();
    FlatProgram {
        types,
        functions,
        value_ids,
    }
}

struct Expander<'a> {
    value_ids: &'a mut ValueIdCounter,
}

impl Expander<'_> {
    fn data_block(&mut self, block: FlatDataBlock) -> FlatDataBlock {
        FlatDataBlock {
            bindings: (block.bindings.into_iter())
                .map(|binding| self.data_binding(binding))
                .collect(),
            result: block.result,
        }
    }

    fn html_block(&mut self, block: FlatHtmlBlock) -> FlatHtmlBlock {
        FlatHtmlBlock {
            bindings: (block.bindings.into_iter())
                .map(|binding| self.data_binding(binding))
                .collect(),
            html: (block.html.into_iter())
                .map(|binding| self.html_binding(binding))
                .collect(),
            result: block.result,
        }
    }

    fn match_<B>(
        &mut self,
        match_: IrMatch<B>,
        expand_block: impl Fn(&mut Self, B) -> B,
    ) -> IrMatch<B> {
        IrMatch {
            subject: match_.subject,
            arms: (match_.arms.into_iter())
                .map(|arm| IrMatchArm {
                    pattern: arm.pattern,
                    body: expand_block(self, arm.body),
                })
                .collect(),
        }
    }

    fn parameters<T: Clone>(&mut self, types: &[T]) -> Vec<IrBinder<T>> {
        (types.iter())
            .map(|typ| IrBinder {
                var: self.value_ids.next(),
                typ: typ.clone(),
            })
            .collect()
    }

    fn data_binding(&mut self, binding: FlatDataBinding) -> FlatDataBinding {
        let FlatDataBinding { binder, op } = binding;
        let op = match op {
            FlatDataOp::Match(match_) => {
                let match_ = self.match_(match_, Self::data_block);
                match &binder.typ {
                    IrValueType::Function(signature) => {
                        let parameters = self.parameters(&signature.parameters);
                        let args: Vec<ValueId> = parameters.iter().map(|param| param.var).collect();
                        let typ = IrValueType::Data(signature.result.clone());
                        let arms = (match_.arms.into_iter())
                            .map(|arm| {
                                let mut body = arm.body;
                                let applied = self.value_ids.next();
                                body.bindings.push(FlatDataBinding {
                                    binder: IrBinder {
                                        var: applied,
                                        typ: typ.clone(),
                                    },
                                    op: FlatDataOp::Apply(body.result, args.clone()),
                                });
                                body.result = applied;
                                IrMatchArm {
                                    pattern: arm.pattern,
                                    body,
                                }
                            })
                            .collect();
                        let result = self.value_ids.next();
                        let body = FlatDataBlock {
                            bindings: vec![FlatDataBinding {
                                binder: IrBinder { var: result, typ },
                                op: FlatDataOp::Match(IrMatch {
                                    subject: match_.subject,
                                    arms,
                                }),
                            }],
                            result,
                        };
                        FlatDataOp::Lambda(IrLambda { parameters, body })
                    }
                    IrValueType::Data(_) => FlatDataOp::Match(match_),
                }
            }
            FlatDataOp::Lambda(IrLambda { parameters, body }) => FlatDataOp::Lambda(IrLambda {
                parameters,
                body: self.data_block(body),
            }),
            op @ (FlatDataOp::Data(_) | FlatDataOp::Call(..) | FlatDataOp::Apply(..)) => op,
        };
        FlatDataBinding { binder, op }
    }

    fn html_binding(&mut self, binding: FlatHtmlBinding) -> FlatHtmlBinding {
        let FlatHtmlBinding { binder, op } = binding;
        let op = match op {
            FlatHtmlOp::Match(match_) => {
                let match_ = self.match_(match_, Self::html_block);
                match &binder.typ {
                    IrHtmlType::Function(signature) => {
                        let parameters = self.parameters(&signature.parameters);
                        let args: Vec<ValueId> = parameters.iter().map(|param| param.var).collect();
                        let arms = (match_.arms.into_iter())
                            .map(|arm| {
                                let mut body = arm.body;
                                body.html
                                    .retain(|binding| binding.binder.typ != IrHtmlType::Html);
                                let applied = self.value_ids.next();
                                body.html.push(FlatHtmlBinding {
                                    binder: IrBinder {
                                        var: applied,
                                        typ: IrHtmlType::Html,
                                    },
                                    op: FlatHtmlOp::Apply(body.result, args.clone()),
                                });
                                body.result = applied;
                                IrMatchArm {
                                    pattern: arm.pattern,
                                    body,
                                }
                            })
                            .collect();
                        let result = self.value_ids.next();
                        let body = FlatHtmlBlock {
                            bindings: Vec::new(),
                            html: vec![FlatHtmlBinding {
                                binder: IrBinder {
                                    var: result,
                                    typ: IrHtmlType::Html,
                                },
                                op: FlatHtmlOp::Match(IrMatch {
                                    subject: match_.subject,
                                    arms,
                                }),
                            }],
                            result,
                        };
                        FlatHtmlOp::Lambda(IrLambda { parameters, body })
                    }
                    IrHtmlType::Html => FlatHtmlOp::Match(match_),
                }
            }
            FlatHtmlOp::For(IrFor { var, array, body }) => FlatHtmlOp::For(IrFor {
                var,
                array,
                body: self.html_block(body),
            }),
            FlatHtmlOp::Lambda(IrLambda { parameters, body }) => FlatHtmlOp::Lambda(IrLambda {
                parameters,
                body: self.html_block(body),
            }),
            op @ (FlatHtmlOp::Call(..)
            | FlatHtmlOp::Apply(..)
            | FlatHtmlOp::Text(_)
            | FlatHtmlOp::Escape(_)
            | FlatHtmlOp::Concat(_)
            | FlatHtmlOp::Element(_)) => op,
        };
        FlatHtmlBinding { binder, op }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::Ty;
    use crate::ir::flat_lint;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::ir_type::IrType;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    // The markup an entrypoint renders, or None if it reaches a limit.
    fn markup(result: Result<Value<'_>, EvalError>) -> Option<String> {
        match result.map(Value::into_markup) {
            Ok(output) => Some(output),
            Err(
                EvalError::CallDepthLimit { .. }
                | EvalError::OpCountLimit { .. }
                | EvalError::HtmlSizeLimit { .. }
                | EvalError::StringLengthLimit { .. },
            ) => None,
            Err(error) => panic!("{error}"),
        }
    }

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_expansion() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entrypoint_args = random_entrypoint_args(&program, &mut rng);

            let before_program = program.clone();
            let before: Vec<Option<String>> = (entrypoint_args.iter())
                .map(|(function, args)| {
                    markup(evaluate_entrypoint(&program, function, args.clone()))
                })
                .collect();

            let program = expand_closure_matches(program);
            flat_lint::assert_well_typed(&program);

            for ((function, args), before) in entrypoint_args.iter().zip(&before) {
                // An arm runs on each application after the expansion, so an
                // entrypoint can reach a limit it did not reach before.
                let after = markup(evaluate_entrypoint(&program, function, args.clone()));
                let (Some(before), Some(after)) = (before, after) else {
                    continue;
                };
                assert_eq!(
                    before, &after,
                    "entrypoint {function}\n-- before --\n{before_program}\n-- after --\n{program}"
                );
            }
            Ok(())
        });
    }

    fn check(program: FlatProgram, expected: Expect) {
        let before = program.to_string();
        let program = expand_closure_matches(program);
        flat_lint::assert_well_typed(&program);
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{program}"));
    }

    #[test]
    fn should_move_a_match_of_closures_that_compute_values_into_one_lambda() {
        check(
            FlatProgramBuilder::new()
                .entrypoint(
                    "Test",
                    [("double", Ty::Bool), ("n", Ty::Int)],
                    Ty::Html,
                    |t| {
                        let scale = t.match_(t.param("double"), |m| {
                            m.arm(Pat::True, |t, _| {
                                let two = t.int(2);
                                t.lambda([IrType::INT], |t, params| t.mul(params[0], two))
                            });
                            m.arm(Pat::False, |t, _| {
                                t.lambda([IrType::INT], |_, params| params[0])
                            });
                        });
                        let scaled = t.apply(scale, [t.param("n")]);
                        t.escape(t.int_to_string(scaled))
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(double@v0: Bool, n@v1: Int) -> Html {
                  let v8: (Int) -> Int = match v0 {
                    true => {
                      let v2: Int = 2
                      let v5: (Int) -> Int = fn(v3: Int) {
                        let v4: Int = v3 * v2
                        v4
                      }
                      v5
                    }
                    false => {
                      let v7: (Int) -> Int = fn(v6: Int) {
                        v6
                      }
                      v7
                    }
                  }
                  let v9: Int = v8(v1)
                  let v10: String = v9.to_string()
                  let v11: Html = escape(v10)
                  v11
                }

                -- after --
                entrypoint fn Test@f0(double@v0: Bool, n@v1: Int) -> Html {
                  let v8: (Int) -> Int = fn(v12: Int) {
                    let v15: Int = match v0 {
                      true => {
                        let v2: Int = 2
                        let v5: (Int) -> Int = fn(v3: Int) {
                          let v4: Int = v3 * v2
                          v4
                        }
                        let v13: Int = v5(v12)
                        v13
                      }
                      false => {
                        let v7: (Int) -> Int = fn(v6: Int) {
                          v6
                        }
                        let v14: Int = v7(v12)
                        v14
                      }
                    }
                    v15
                  }
                  let v9: Int = v8(v1)
                  let v10: String = v9.to_string()
                  let v11: Html = escape(v10)
                  v11
                }
            "#]],
        );
    }

    #[test]
    fn should_move_a_match_of_closures_that_render_html_into_one_lambda_without_its_html() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("bold", Ty::Bool)], Ty::Html, |t| {
                    let show = t.match_(t.param("bold"), |m| {
                        m.arm(Pat::True, |t, _| {
                            t.text("never read");
                            t.lambda([IrType::STRING], |t, params| {
                                t.element("b", [], t.escape(params[0]))
                            })
                        });
                        m.arm(Pat::False, |t, _| {
                            t.lambda([IrType::STRING], |t, params| t.escape(params[0]))
                        });
                    });
                    t.apply(show, [t.str("x")])
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(bold@v0: Bool) -> Html {
                  let v10: String = "x"
                  let v9: (String) -> Html = match v0 {
                    true => {
                      let v1: Html = text("never read")
                      let v5: (String) -> Html = fn(v2: String) {
                        let v3: Html = escape(v2)
                        let v4: Html = element("b", {}, v3)
                        v4
                      }
                      v5
                    }
                    false => {
                      let v8: (String) -> Html = fn(v6: String) {
                        let v7: Html = escape(v6)
                        v7
                      }
                      v8
                    }
                  }
                  let v11: Html = v9(v10)
                  v11
                }

                -- after --
                entrypoint fn Test@f0(bold@v0: Bool) -> Html {
                  let v10: String = "x"
                  let v9: (String) -> Html = fn(v12: String) {
                    let v15: Html = match v0 {
                      true => {
                        let v5: (String) -> Html = fn(v2: String) {
                          let v3: Html = escape(v2)
                          let v4: Html = element("b", {}, v3)
                          v4
                        }
                        let v13: Html = v5(v12)
                        v13
                      }
                      false => {
                        let v8: (String) -> Html = fn(v6: String) {
                          let v7: Html = escape(v6)
                          v7
                        }
                        let v14: Html = v8(v12)
                        v14
                      }
                    }
                    v15
                  }
                  let v11: Html = v9(v10)
                  v11
                }
            "#]],
        );
    }
}
