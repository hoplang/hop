use crate::document::Document;
use crate::ir::flat_program::FlatProgram;
use crate::ir::ir_page::IrPage;
use crate::ir::optimize_flat;
use crate::ir::runtime::page_runtime::PageRuntime;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::session::Session;
use expect_test::{Expect, expect};
use indoc::indoc;
use std::collections::HashMap;

enum Event {
    Click(usize),
}

fn check(source: &str, events: &[Event], expected: Expect) {
    let document_id = RootContainedFilePath::new("main.hop").unwrap();
    let mut session = Session::new();
    session.update_hop_document(
        &document_id,
        Document::new(document_id.clone(), source.to_string()),
    );
    let diagnostics = session.diagnostics();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let (program, pages) = session.compile_flat(None, None);
    let page = pages
        .iter()
        .find(|page| page.name.as_str() == "Test")
        .expect("the source declares a page Test");
    let out = run(&program, page, events);
    let optimized = optimize_flat(program);
    assert_eq!(
        run(&optimized, page, events),
        out,
        "the optimized program renders differently"
    );
    expected.assert_eq(&out);
}

fn run(program: &FlatProgram, page: &IrPage, events: &[Event]) -> String {
    let mut runtime = PageRuntime::open(program, page.body, HashMap::new()).unwrap();
    let mut out = format!("-- render --\n{runtime}");
    for event in events {
        match *event {
            Event::Click(label) => {
                let shown = (runtime.shown().iter())
                    .find(|shown| shown.label == label)
                    .unwrap_or_else(|| panic!("e{label} is not shown"));
                out.push_str(&format!("-- click {}@e{label} --\n", shown.tag));
                if let Err(error) = runtime.click(label) {
                    out.push_str(&format!("error: {error}\n"));
                    continue;
                }
            }
        }
        out.push_str(&runtime.to_string());
    }
    out
}

#[test]
fn the_state_of_a_loop_without_a_key_follows_the_index() {
    check(
        indoc! {r#"
            fn Counter(label: String) -> Html {
              let (count, set_count) = state!(0);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };
              
              <button on:click={incr_count}>
                {label}:{count.to_string()}
              </button>
            }
            
            page Test {
              fn body() -> Html {
                let (reversed, set_reversed) = state!(false);
                let reverse = mut fn() {
                  set_reversed(fn(curr) { !curr })
                };
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                
                <>
                  <b on:click={reverse}>flip</b>
                  {for id in ids {
                    <Counter label={id.to_string()}/>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <button@e2>1:0</button>
            <button@e3>2:0</button>
            -- click button@e2 --
            <b@e1>flip</b>
            <button@e2>1:1</button>
            <button@e3>2:0</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e2>2:1</button>
            <button@e3>1:0</button>
        "#]],
    );
}

#[test]
fn state_and_an_element_of_a_call_in_an_arm_start_over_when_the_match_takes_the_arm_again() {
    check(
        indoc! {r#"
            fn Counter() -> Html {
              let (count, set_count) = state!(0);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };
              
              <button on:click={incr_count}>
                {count.to_string()}
              </button>
            }
            
            page Test {
              fn body() -> Html {
                let (open, set_open) = state!(true);
                let toggle_open = mut fn() {
                  set_open(fn(curr) { !curr })
                };
                
                <>
                  <b on:click={toggle_open}>toggle</b>
                  {match open {
                    true => <Counter/>,
                    false => <i>closed</i>,
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(2),
            Event::Click(1),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <b@e1>toggle</b>
            <button@e2>0</button>
            -- click button@e2 --
            <b@e1>toggle</b>
            <button@e2>1</button>
            -- click button@e2 --
            <b@e1>toggle</b>
            <button@e2>2</button>
            -- click b@e1 --
            <b@e1>toggle</b>
            <i@e3>closed</i>
            -- click b@e1 --
            <b@e1>toggle</b>
            <button@e4>0</button>
        "#]],
    );
}

#[test]
fn an_element_bound_outside_a_match_keeps_its_identity_in_each_arm() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (on, set_on) = state!(true);
                let toggle_on = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                let field = <input/>;
                
                <>
                  <button on:click={toggle_on}>toggle</button>
                  {match on {
                    true => <div>{field}</div>,
                    false => <p>{field}</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>toggle</button>
            <div@e2>
              <input@e3>
            </div>
            -- click button@e1 --
            <button@e1>toggle</button>
            <p@e4>
              <input@e3>
            </p>
            -- click button@e1 --
            <button@e1>toggle</button>
            <div@e5>
              <input@e3>
            </div>
        "#]],
    );
}

#[test]
fn a_click_runs_the_handlers_of_the_elements_around_it_innermost_first() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (clicked, set_clicked) = state!("none");
                let (inner, set_inner) = state!(0);
                let div_click = mut fn() {
                  set_clicked(fn(_) { "div" })
                };
                let btn_click = mut fn() {
                  set_clicked(fn(_) { "button" });
                  set_inner(fn(curr) { curr + 1 })
                };
                
                <div on:click={div_click}>
                  <button on:click={btn_click}>
                    {clicked} {inner.to_string()}
                  </button>
                </div>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <div@e1>
              <button@e2>none 0</button>
            </div>
            -- click button@e2 --
            <div@e1>
              <button@e2>div 1</button>
            </div>
            -- click div@e1 --
            <div@e1>
              <button@e2>div 1</button>
            </div>
        "#]],
    );
}

#[test]
fn an_element_a_match_hides_keeps_its_identity_while_it_is_evaluated() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (shown, set_shown) = state!(true);
                let toggle_shown = mut fn() {
                  set_shown(fn(curr) { !curr })
                };
                let field = <input/>;
                
                <>
                  <button on:click={toggle_shown}>toggle</button>
                  {match shown {
                    true => <div>{field}</div>,
                    false => <p>hidden</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>toggle</button>
            <div@e2>
              <input@e3>
            </div>
            -- click button@e1 --
            <button@e1>toggle</button>
            <p@e4>hidden</p>
            -- click button@e1 --
            <button@e1>toggle</button>
            <div@e5>
              <input@e3>
            </div>
        "#]],
    );
}

#[test]
fn state_a_match_hides_keeps_its_value_while_it_is_evaluated() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (shown, set_shown) = state!(true);
                let (count, set_count) = state!(0);
                let toggle_shown = mut fn() {
                  set_shown(fn(curr) { !curr })
                };
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };
                
                <>
                  <b on:click={toggle_shown}>toggle</b>
                  {match shown {
                    true => <button on:click={incr_count}>{count.to_string()}</button>,
                    false => <p>hidden</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>toggle</b>
            <button@e2>0</button>
            -- click button@e2 --
            <b@e1>toggle</b>
            <button@e2>1</button>
            -- click b@e1 --
            <b@e1>toggle</b>
            <p@e3>hidden</p>
            -- click b@e1 --
            <b@e1>toggle</b>
            <button@e4>1</button>
        "#]],
    );
}

#[test]
fn a_click_fails_and_changes_nothing_when_a_handler_around_the_element_fails() {
    check(
        indoc! {r#"
            fn forever(n: Int) -> Int {
              forever(n + 1)
            }
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };
                
                <>
                  <div on:click={mut fn() {
                    set_count(fn(curr) { forever(curr) })
                  }}>
                    <button on:click={incr_count}>
                      {count.to_string()}
                    </button>
                  </div>
                  <p>no handler</p>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(3)],
        expect![[r#"
            -- render --
            <div@e1>
              <button@e2>0</button>
            </div>
            <p@e3>no handler</p>
            -- click button@e2 --
            error: Program exceeded the call depth limit of 12
            -- click p@e3 --
            <div@e1>
              <button@e2>0</button>
            </div>
            <p@e3>no handler</p>
        "#]],
    );
}

#[test]
fn a_handler_that_writes_in_one_arm_of_a_match_writes_nothing_in_the_other() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr = mut fn() {
                  match count < 1 {
                    true => set_count(fn(curr) { curr + 1 }),
                    false => (),
                  }
                };
                
                <button on:click={incr}>
                  {count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>1</button>
            -- click button@e1 --
            <button@e1>1</button>
        "#]],
    );
}

#[test]
fn a_click_on_an_element_without_a_handler_runs_the_handlers_around_it() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };
                
                <>
                  <button on:click={incr_count}>
                    <b>
                      {count.to_string()}
                    </b>
                  </button>
                  <p>no handler</p>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(3)],
        expect![[r#"
            -- render --
            <button@e1>
              <b@e2>0</b>
            </button>
            <p@e3>no handler</p>
            -- click b@e2 --
            <button@e1>
              <b@e2>1</b>
            </button>
            <p@e3>no handler</p>
            -- click p@e3 --
            <button@e1>
              <b@e2>1</b>
            </button>
            <p@e3>no handler</p>
        "#]],
    );
}

#[test]
fn state_takes_its_initial_value_only_when_it_is_first_bound() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (a, set_a) = state!(1);
                let (b, set_b) = state!(a * 10);
                let incr_a = mut fn() {
                  set_a(fn(curr) { curr + 1 })
                };
                let incr_b = mut fn() {
                  set_b(fn(curr) { curr + 1 })
                };
                
                <>
                  <button on:click={incr_a}>
                    {a.to_string()}
                  </button>
                  <button on:click={incr_b}>
                    {b.to_string()}
                  </button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>1</button>
            <button@e2>10</button>
            -- click button@e1 --
            <button@e1>2</button>
            <button@e2>10</button>
            -- click button@e2 --
            <button@e1>2</button>
            <button@e2>11</button>
            -- click button@e1 --
            <button@e1>3</button>
            <button@e2>11</button>
        "#]],
    );
}

#[test]
fn state_keeps_its_value_when_the_argument_its_initial_value_reads_changes() {
    check(
        indoc! {r#"
            fn Counter(start: Int) -> Html {
              let (count, set_count) = state!(start);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };
              
              <button on:click={incr_count}>
                {start.to_string()}/{count.to_string()}
              </button>
            }
            page Test {
              fn body() -> Html {
                let (base, set_base) = state!(0);
                let incr_base = mut fn() {
                  set_base(fn(curr) { curr + 100 })
                };
                
                <>
                  <b on:click={incr_base}>incr</b>
                  <Counter start={base}/>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>incr</b>
            <button@e2>0/0</button>
            -- click button@e2 --
            <b@e1>incr</b>
            <button@e2>0/1</button>
            -- click b@e1 --
            <b@e1>incr</b>
            <button@e2>100/1</button>
        "#]],
    );
}

#[test]
fn each_application_of_a_lambda_has_states_of_its_own_and_shares_the_states_it_captures() {
    check(
        indoc! {r#"
            fn Twice(row: fn() -> Html) -> Html {
              <>
                {row()}
                {row()}
              </>
            }
            
            page Test {
              fn body() -> Html {
                let (outer, set_outer) = state!(0);
                
                <Twice row={fn() {
                  let (inner, set_inner) = state!(0);
                  let incr_both = mut fn() {
                    set_inner(fn(curr) { curr + 1 });
                    set_outer(fn(curr) { curr + 1 })
                  };
                  
                  <button on:click={incr_both}>
                    {inner.to_string()}/{outer.to_string()}
                  </button>
                }}/>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0/0</button>
            <button@e2>0/0</button>
            -- click button@e2 --
            <button@e1>0/1</button>
            <button@e2>1/1</button>
            -- click button@e2 --
            <button@e1>0/2</button>
            <button@e2>2/2</button>
            -- click button@e1 --
            <button@e1>1/3</button>
            <button@e2>2/3</button>
        "#]],
    );
}

#[test]
fn state_bound_in_an_arm_starts_over_when_the_match_takes_the_arm_again() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (left, set_left) = state!(true);
                let toggle_left = mut fn() {
                  set_left(fn(curr) { !curr })
                };
                
                <>
                  <b on:click={toggle_left}>switch</b>
                  {match left {
                    true => {
                      let (count, set_count) = state!(0);
                      let incr_count = mut fn() {
                        set_count(fn(curr) { curr + 1 })
                      };
                      
                      <button on:click={incr_count}>
                        left {count.to_string()}
                      </button>
                    },
                    false => {
                      let (count, set_count) = state!(100);
                      let incr = mut fn() {
                        set_count(fn(curr) { curr + 1 })
                      };
                      
                      <button on:click={incr}>
                        right {count.to_string()}
                      </button>
                    },
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(2),
            Event::Click(1),
            Event::Click(3),
            Event::Click(1),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <b@e1>switch</b>
            <button@e2>left 0</button>
            -- click button@e2 --
            <b@e1>switch</b>
            <button@e2>left 1</button>
            -- click button@e2 --
            <b@e1>switch</b>
            <button@e2>left 2</button>
            -- click b@e1 --
            <b@e1>switch</b>
            <button@e3>right 100</button>
            -- click button@e3 --
            <b@e1>switch</b>
            <button@e3>right 101</button>
            -- click b@e1 --
            <b@e1>switch</b>
            <button@e4>left 0</button>
            -- click b@e1 --
            <b@e1>switch</b>
            <button@e5>right 100</button>
        "#]],
    );
}

#[test]
fn state_bound_in_a_loop_follows_its_key_and_starts_over_when_the_key_comes_back() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (ids, set_ids) = state!([1, 2]);
                let flip = mut fn() {
                  set_ids(fn(_) { [2, 1] })
                };
                let drop = mut fn() {
                  set_ids(fn(_) { [2] })
                };
                
                <>
                  <b on:click={flip}>
                    flip
                  </b>
                  <i on:click={drop}>
                    drop
                  </i>
                  {for id in ids key id {
                    let (count, set_count) = state!(0);
                    let incr_count = mut fn() {
                      set_count(fn(curr) { curr + 1 })
                    };
                    
                    <button on:click={incr_count}>
                      {id.to_string()}:{count.to_string()}
                    </button>
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(3),
            Event::Click(1),
            Event::Click(2),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <i@e2>drop</i>
            <button@e3>1:0</button>
            <button@e4>2:0</button>
            -- click button@e3 --
            <b@e1>flip</b>
            <i@e2>drop</i>
            <button@e3>1:1</button>
            <button@e4>2:0</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <i@e2>drop</i>
            <button@e4>2:0</button>
            <button@e3>1:1</button>
            -- click i@e2 --
            <b@e1>flip</b>
            <i@e2>drop</i>
            <button@e4>2:0</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <i@e2>drop</i>
            <button@e4>2:0</button>
            <button@e5>1:0</button>
        "#]],
    );
}

#[test]
fn the_iterations_of_a_repeated_key_follow_the_order_they_take_it_in() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (ids, set_ids) = state!([1, 1]);
                let drop = mut fn() {
                  set_ids(fn(_) { [1] })
                };
                
                <>
                  <b on:click={drop}>
                    drop
                  </b>
                  {for id in ids key id {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() {
                      set_count(fn(curr) { curr + 1 })
                    };
                    
                    <button on:click={incr}>
                      {id.to_string()}:{count.to_string()}
                    </button>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(3), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>drop</b>
            <button@e2>1:0</button>
            <button@e3>1:0</button>
            -- click button@e3 --
            <b@e1>drop</b>
            <button@e2>1:0</button>
            <button@e3>1:1</button>
            -- click b@e1 --
            <b@e1>drop</b>
            <button@e2>1:0</button>
        "#]],
    );
}

#[test]
fn the_states_of_nested_loops_follow_the_keys_of_both_loops() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (rows, set_rows) = state!(["a", "b"]);
                let flip = mut fn() {
                  set_rows(fn(_) { ["b", "a"] })
                };
                
                <>
                  <b on:click={flip}>
                    flip
                  </b>
                  {for row in rows key row {
                    {for col in [1, 2] key col {
                      let (count, set_count) = state!(0);
                      let incr_count = mut fn() {
                        set_count(fn(curr) { curr + 1 })
                      };
                      
                      <button on:click={incr_count}>
                        {row}{col.to_string()}:{count.to_string()}
                      </button>
                    }}
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(4), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <button@e2>a1:0</button>
            <button@e3>a2:0</button>
            <button@e4>b1:0</button>
            <button@e5>b2:0</button>
            -- click button@e4 --
            <b@e1>flip</b>
            <button@e2>a1:0</button>
            <button@e3>a2:0</button>
            <button@e4>b1:1</button>
            <button@e5>b2:0</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e4>b1:1</button>
            <button@e5>b2:0</button>
            <button@e2>a1:0</button>
            <button@e3>a2:0</button>
        "#]],
    );
}

#[test]
fn an_element_keeps_its_identity_when_the_value_of_an_attribute_changes() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (on, set_on) = state!(false);
                let toggle_on = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                
                <button
                  class={match on {
                    true => "on",
                    false => "off",
                  }}
                  on:click={toggle_on}
                >
                  toggle
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1 class="off">toggle</button>
            -- click button@e1 --
            <button@e1 class="on">toggle</button>
            -- click button@e1 --
            <button@e1 class="off">toggle</button>
        "#]],
    );
}

#[test]
fn each_level_of_a_recursive_call_has_states_of_its_own() {
    check(
        indoc! {r#"
            fn Level(depth: Int, levels: Int) -> Html {
              let (count, set_count) = state!(0);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };
              
              <div>
                <button on:click={incr_count}>
                  {depth.to_string()}:{count.to_string()}
                </button>
                {match depth + 1 < levels {
                  true => <Level depth={depth + 1} levels={levels}/>,
                  false => <></>,
                }}
              </div>
            }
            
            page Test {
              fn body() -> Html {
                let (levels, set_levels) = state!(2);
                let incr_levels = mut fn() {
                  set_levels(fn(curr) { curr + 1 })
                };
                
                <>
                  <b on:click={incr_levels}>deeper</b>
                  <Level depth={0} levels={levels}/>
                </>
              }
            }
        "#},
        &[Event::Click(5), Event::Click(3), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>deeper</b>
            <div@e2>
              <button@e3>0:0</button>
              <div@e4>
                <button@e5>1:0</button>
              </div>
            </div>
            -- click button@e5 --
            <b@e1>deeper</b>
            <div@e2>
              <button@e3>0:0</button>
              <div@e4>
                <button@e5>1:1</button>
              </div>
            </div>
            -- click button@e3 --
            <b@e1>deeper</b>
            <div@e2>
              <button@e3>0:1</button>
              <div@e4>
                <button@e5>1:1</button>
              </div>
            </div>
            -- click b@e1 --
            <b@e1>deeper</b>
            <div@e2>
              <button@e3>0:1</button>
              <div@e4>
                <button@e5>1:1</button>
                <div@e6>
                  <button@e7>2:0</button>
                </div>
              </div>
            </div>
        "#]],
    );
}

#[test]
fn a_handler_calls_a_mut_fn_that_applies_the_mut_fns_it_takes() {
    check(
        indoc! {r#"
            mut fn both(
              first: mut fn() -> Tuple[],
              second: mut fn() -> Tuple[],
            ) -> Tuple[] {
              first();
              second()
            }
            
            page Test {
              fn body() -> Html {
                let (a, set_a) = state!(0);
                let (b, set_b) = state!(0);
                let incr_a = mut fn() {
                  set_a(fn(curr) { curr + 1 })
                };
                let incr_b = mut fn() {
                  set_b(fn(curr) { curr + 10 })
                };
                let incr_both = mut fn() {
                  both(incr_a, incr_b)
                };
                
                <button on:click={incr_both}>
                  {a.to_string()}/{b.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0/0</button>
            -- click button@e1 --
            <button@e1>1/10</button>
            -- click button@e1 --
            <button@e1>2/20</button>
        "#]],
    );
}

#[test]
fn a_handler_passed_as_a_parameter_is_the_click_of_the_element_that_takes_it() {
    check(
        indoc! {r#"
            fn Button(press: mut fn() -> Tuple[], children: Html) -> Html {
              <button on:click={press}>
                {children}
              </button>
            }
            
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };
                let reset_count = mut fn() {
                  set_count(fn(_) { 0 })
                };
                
                <>
                  <Button press={incr_count}>
                    {count.to_string()}
                  </Button>
                  <Button press={reset_count}>
                    reset
                  </Button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            <button@e2>reset</button>
            -- click button@e1 --
            <button@e1>1</button>
            <button@e2>reset</button>
            -- click button@e1 --
            <button@e1>2</button>
            <button@e2>reset</button>
            -- click button@e2 --
            <button@e1>0</button>
            <button@e2>reset</button>
        "#]],
    );
}

#[test]
fn a_handler_updates_a_field_of_a_record_in_state_and_keeps_the_others() {
    check(
        indoc! {r#"
            enum Mode { Light, Dark }
            record Setting { mode: Mode, level: Int }
            
            page Test {
              fn body() -> Html {
                let (setting, set_setting) = state!(Setting {mode: Mode::Light, level: 1});
                let dark = mut fn() {
                  set_setting(fn(curr) {
                    Setting {...curr, mode: Mode::Dark}
                  })
                };
                let raise = mut fn() {
                  set_setting(fn(curr) {
                    Setting {...curr, level: curr.level + 1}
                  })
                };
                let mode = match setting.mode {
                  Mode::Light => "light",
                  Mode::Dark => "dark",
                };
                
                <>
                  <button on:click={dark}>dark</button>
                  <button on:click={raise}>raise</button>
                  <p>{mode} {setting.level.to_string()}</p>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>dark</button>
            <button@e2>raise</button>
            <p@e3>light 1</p>
            -- click button@e2 --
            <button@e1>dark</button>
            <button@e2>raise</button>
            <p@e3>light 2</p>
            -- click button@e1 --
            <button@e1>dark</button>
            <button@e2>raise</button>
            <p@e3>dark 2</p>
            -- click button@e2 --
            <button@e1>dark</button>
            <button@e2>raise</button>
            <p@e3>dark 3</p>
        "#]],
    );
}

#[test]
fn a_click_whose_later_write_fails_changes_nothing() {
    check(
        indoc! {r#"
            fn forever(n: Int) -> Int {
              forever(n + 1)
            }
            
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };

                <>
                  <button on:click={mut fn() {
                    set_count(fn(curr) { curr + 1 });
                    set_count(fn(curr) { forever(curr) })
                  }}>
                    forever
                  </button>
                  <button on:click={incr_count}>
                    {count.to_string()}
                  </button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>forever</button>
            <button@e2>0</button>
            -- click button@e1 --
            error: Program exceeded the call depth limit of 12
            -- click button@e2 --
            <button@e1>forever</button>
            <button@e2>1</button>
        "#]],
    );
}

#[test]
fn each_call_has_state_of_its_own() {
    check(
        indoc! {r#"
            fn Counter() -> Html {
              let (count, set_count) = state!(0);
              
              <button on:click={mut fn() {
                set_count(fn(curr) { curr + 1 })
              }}>
                {count.to_string()}
              </button>
            }
            
            page Test {
              fn body() -> Html {
                <><Counter/><Counter/></>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            <button@e2>0</button>
            -- click button@e2 --
            <button@e1>0</button>
            <button@e2>1</button>
            -- click button@e2 --
            <button@e1>0</button>
            <button@e2>2</button>
            -- click button@e1 --
            <button@e1>1</button>
            <button@e2>2</button>
        "#]],
    );
}

#[test]
fn an_update_of_state_takes_the_value_an_earlier_update_of_the_click_gave() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                
                <button on:click={mut fn() {
                  set_count(fn(curr) { curr + 1 });
                  set_count(fn(curr) { curr + 10 })
                }}>
                  {count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>11</button>
            -- click button@e1 --
            <button@e1>22</button>
        "#]],
    );
}

#[test]
fn a_handler_reads_state_as_the_value_the_render_gave() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                
                <button on:click={mut fn() {
                  set_count(fn(_) { count + 1 });
                  set_count(fn(_) { count + 10 })
                }}>
                  {count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>10</button>
            -- click button@e1 --
            <button@e1>20</button>
        "#]],
    );
}

#[test]
fn a_handler_that_updates_a_record_state_twice_applies_both_updates() {
    check(
        indoc! {r#"
            record Setting { level: Int, size: Int }

            page Test {
              fn body() -> Html {
                let (setting, set_setting) = state!(Setting {level: 0, size: 0});
                let update = mut fn() {
                  set_setting(fn(curr) { Setting {...curr, level: curr.level + 1} });
                  set_setting(fn(curr) { Setting {...curr, size: curr.size + 1} })
                };

                <button on:click={update}>
                  {setting.level.to_string()}/{setting.size.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0/0</button>
            -- click button@e1 --
            <button@e1>1/1</button>
            -- click button@e1 --
            <button@e1>2/2</button>
        "#]],
    );
}

#[test]
fn the_states_of_a_keyed_loop_follow_the_keys() {
    check(
        indoc! {r#"
            fn Counter(label: String) -> Html {
              let (count, set_count) = state!(0);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };
              
              <button on:click={incr_count}>
                {label}:{count.to_string()}
              </button>
            }
            
            page Test {
              fn body() -> Html {
                let (ids, set_ids) = state!([1, 2]);
                let flip = mut fn() {
                  set_ids(fn(_) { [2, 1] })
                };
                
                <>
                  <b on:click={flip}>
                    flip
                  </b>
                  {for id in ids key id {
                    <Counter label={id.to_string()}/>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <button@e2>1:0</button>
            <button@e3>2:0</button>
            -- click button@e2 --
            <b@e1>flip</b>
            <button@e2>1:1</button>
            <button@e3>2:0</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e3>2:0</button>
            <button@e2>1:1</button>
        "#]],
    );
}

#[test]
fn a_component_reads_and_writes_a_state_its_caller_passes() {
    check(
        indoc! {r#"
            fn Stepper(value: Int, set_value: mut fn(fn(Int) -> Int) -> Tuple[]) -> Html {
              let step = mut fn() {
                set_value(fn(curr) { curr + 1 });
                set_value(fn(curr) { curr * 2 })
              };

              <button on:click={step}>
                {value.to_string()}
              </button>
            }

            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(1);

                <>
                  <Stepper
                    value={count}
                    set_value={set_count}
                  />
                  <p>
                    {count.to_string()}
                  </p>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>1</button>
            <p@e2>1</p>
            -- click button@e1 --
            <button@e1>4</button>
            <p@e2>4</p>
            -- click button@e1 --
            <button@e1>10</button>
            <p@e2>10</p>
        "#]],
    );
}

#[test]
fn a_let_pattern_takes_apart_state_that_holds_a_tuple() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (pair, set_pair) = state!((0, "a"));
                let (count, label) = pair;
                let step = mut fn() {
                  set_pair(fn(curr) {
                    let (n, l) = curr;
                    (n + 1, l + "b")
                  })
                };

                <button on:click={step}>
                  {label}:{count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>a:0</button>
            -- click button@e1 --
            <button@e1>ab:1</button>
            -- click button@e1 --
            <button@e1>abb:2</button>
        "#]],
    );
}

#[test]
fn each_closure_a_render_fn_returns_reads_and_writes_the_state_of_its_own_call() {
    check(
        indoc! {r#"
            render fn counter() -> render fn(String) -> Html {
              let (count, set_count) = state!(0);
              let incr_count = mut fn() {
                set_count(fn(curr) { curr + 1 })
              };

              fn(label) {
                <button on:click={incr_count}>
                  {label}:{count.to_string()}
                </button>
              }
            }

            page Test {
              fn body() -> Html {
                let a = counter();
                let b = counter();

                <>
                  {a("a")}
                  {b("b")}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>a:0</button>
            <button@e2>b:0</button>
            -- click button@e1 --
            <button@e1>a:1</button>
            <button@e2>b:0</button>
            -- click button@e1 --
            <button@e1>a:2</button>
            <button@e2>b:0</button>
            -- click button@e2 --
            <button@e1>a:2</button>
            <button@e2>b:1</button>
        "#]],
    );
}

#[test]
fn a_handler_a_render_fn_returns_writes_the_state_of_its_own_call() {
    check(
        indoc! {r#"
            render fn incrementer(step: Int, set_total: mut fn(fn(Int) -> Int) -> Tuple[]) -> mut fn() -> Tuple[] {
              let (clicks, set_clicks) = state!(0);
              mut fn() {
                set_clicks(fn(curr) { curr + 1 });
                set_total(fn(curr) { curr + step * (clicks + 1) })
              }
            }

            page Test {
              fn body() -> Html {
                let (total, set_total) = state!(0);
                let by_one = incrementer(1, set_total);
                let by_ten = incrementer(10, set_total);

                <>
                  <button on:click={by_one}>one</button>
                  <button on:click={by_ten}>ten</button>
                  <p>{total.to_string()}</p>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>one</button>
            <button@e2>ten</button>
            <p@e3>0</p>
            -- click button@e1 --
            <button@e1>one</button>
            <button@e2>ten</button>
            <p@e3>1</p>
            -- click button@e1 --
            <button@e1>one</button>
            <button@e2>ten</button>
            <p@e3>3</p>
            -- click button@e2 --
            <button@e1>one</button>
            <button@e2>ten</button>
            <p@e3>13</p>
        "#]],
    );
}

#[test]
fn a_handler_that_a_match_produces_runs_the_arm_of_the_latest_render() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let step = match count < 2 {
                  true => mut fn() { set_count(fn(curr) { curr + 1 }) },
                  false => mut fn() { set_count(fn(curr) { curr + 10 }) },
                };
                <button on:click={step}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>1</button>
            -- click button@e1 --
            <button@e1>2</button>
            -- click button@e1 --
            <button@e1>12</button>
        "#]],
    );
}

#[test]
fn a_handler_that_a_render_fn_returns_from_a_match_runs_the_arm_of_the_latest_render() {
    check(
        indoc! {r#"
            render fn stepper(count: Int, set_count: mut fn(fn(Int) -> Int) -> Tuple[]) -> mut fn() -> Tuple[] {
              match count < 2 {
                true => mut fn() { set_count(fn(curr) { curr + 1 }) },
                false => mut fn() { set_count(fn(curr) { curr + 10 }) },
              }
            }

            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let step = stepper(count, set_count);
                <button on:click={step}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>1</button>
            -- click button@e1 --
            <button@e1>2</button>
            -- click button@e1 --
            <button@e1>12</button>
        "#]],
    );
}

#[test]
fn state_of_a_closure_starts_over_when_a_match_produces_another_lambda() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (bold, set_bold) = state!(false);
                let flip = mut fn() { set_bold(fn(curr) { !curr }) };
                let counter = match bold {
                  true => fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                    <b on:click={incr}>{count.to_string()}</b>
                  },
                  false => fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                    <i on:click={incr}>{count.to_string()}</i>
                  },
                };
                <>
                  <button on:click={flip}>flip</button>
                  {counter()}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(2),
            Event::Click(1),
            Event::Click(3),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <i@e2>0</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>2</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <b@e3>0</b>
            -- click b@e3 --
            <button@e1>flip</button>
            <b@e3>1</b>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e4>0</i>
        "#]],
    );
}

#[test]
fn state_of_a_closure_stays_when_every_arm_of_a_match_produces_the_same_closure() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (flag, set_flag) = state!(false);
                let flip = mut fn() { set_flag(fn(curr) { !curr }) };
                let counter = fn() {
                  let (count, set_count) = state!(0);
                  let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                  <i on:click={incr}>{count.to_string()}</i>
                };
                let chosen = match flag {
                  true => counter,
                  false => counter,
                };
                <>
                  <button on:click={flip}>flip</button>
                  {chosen()}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <i@e2>0</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>2</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e2>2</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>3</i>
        "#]],
    );
}

#[test]
fn state_of_a_closure_stays_when_a_match_produces_the_same_lambda_with_other_captures() {
    check(
        indoc! {r#"
            fn counter(step: Int) -> fn() -> Html {
              fn() {
                let (count, set_count) = state!(0);
                let incr = mut fn() { set_count(fn(curr) { curr + step }) };
                <i on:click={incr}>{count.to_string()}</i>
              }
            }

            page Test {
              fn body() -> Html {
                let (big, set_big) = state!(false);
                let flip = mut fn() { set_big(fn(curr) { !curr }) };
                let chosen = match big {
                  true => counter(10),
                  false => counter(1),
                };
                <>
                  <button on:click={flip}>flip</button>
                  {chosen()}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <i@e2>0</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>11</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e2>11</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>12</i>
        "#]],
    );
}

#[test]
fn state_of_a_closure_applied_in_each_arm_starts_over_when_the_match_takes_another_arm() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (flag, set_flag) = state!(false);
                let flip = mut fn() { set_flag(fn(curr) { !curr }) };
                let counter = fn() {
                  let (count, set_count) = state!(0);
                  let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                  <i on:click={incr}>{count.to_string()}</i>
                };
                <>
                  <button on:click={flip}>flip</button>
                  {match flag {
                    true => counter(),
                    false => counter(),
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(3),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <i@e2>0</i>
            -- click i@e2 --
            <button@e1>flip</button>
            <i@e2>1</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e3>0</i>
            -- click i@e3 --
            <button@e1>flip</button>
            <i@e3>1</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e4>0</i>
        "#]],
    );
}

#[test]
fn handlers_in_an_array_of_tuples_update_the_cell_they_capture() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                let decr = mut fn() { set_count(fn(curr) { curr - 2 }) };
                let steps = [
                  (incr, "+"),
                  (decr, "-"),
                ];
                <>
                  <b>{count.to_string()}</b>
                  {for step in steps {
                    let (run, label) = step;
                    <button on:click={run}>{label}</button>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(2), Event::Click(3)],
        expect![[r#"
            -- render --
            <b@e1>0</b>
            <button@e2>+</button>
            <button@e3>-</button>
            -- click button@e2 --
            <b@e1>1</b>
            <button@e2>+</button>
            <button@e3>-</button>
            -- click button@e2 --
            <b@e1>2</b>
            <button@e2>+</button>
            <button@e3>-</button>
            -- click button@e3 --
            <b@e1>0</b>
            <button@e2>+</button>
            <button@e3>-</button>
        "#]],
    );
}

#[test]
fn state_of_closures_in_the_fields_of_records_follows_the_key_of_the_loop_that_applies_them() {
    check(
        indoc! {r#"
            record Counter {
              label: String,
              show: fn() -> Html,
            }

            fn counter(label: String) -> Counter {
              Counter {
                label: label,
                show: fn() {
                  let (count, set_count) = state!(0);
                  let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                  <b on:click={incr}>{label}{count.to_string()}</b>
                },
              }
            }

            page Test {
              fn body() -> Html {
                let (flipped, set_flipped) = state!(false);
                let flip = mut fn() { set_flipped(fn(curr) { !curr }) };
                let counters = match flipped {
                  true => [counter("b"), counter("a")],
                  false => [counter("a"), counter("b")],
                };
                <>
                  <button on:click={flip}>flip</button>
                  {for each in counters key each.label {
                    let show = each.show;
                    show()
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(2),
            Event::Click(3),
            Event::Click(1),
            Event::Click(2),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <b@e2>a0</b>
            <b@e3>b0</b>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e2>a1</b>
            <b@e3>b0</b>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e2>a2</b>
            <b@e3>b0</b>
            -- click b@e3 --
            <button@e1>flip</button>
            <b@e2>a2</b>
            <b@e3>b1</b>
            -- click button@e1 --
            <button@e1>flip</button>
            <b@e3>b1</b>
            <b@e2>a2</b>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e3>b1</b>
            <b@e2>a3</b>
        "#]],
    );
}

#[test]
fn state_of_a_closure_in_an_option_starts_over_when_the_option_holds_another_lambda() {
    check(
        indoc! {r#"
            fn Counter(tag: String) -> Html {
              let (count, set_count) = state!(0);
              let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
              <b on:click={incr}>{tag}{count.to_string()}</b>
            }

            page Test {
              fn body() -> Html {
                let (step, set_step) = state!(0);
                let next = mut fn() { set_step(fn(curr) { curr + 1 }) };
                let shown = match step == 0 {
                  true => Some(fn() {
                    <Counter tag="a"/>
                  }),
                  false => match step == 1 {
                    true => Some(fn() {
                      <Counter tag="b"/>
                    }),
                    false => None,
                  },
                };
                <>
                  <button on:click={next}>next</button>
                  {match shown {
                    Some(show) => show(),
                    None => {
                      <i>none</i>
                    },
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(3),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <button@e1>next</button>
            <b@e2>a0</b>
            -- click b@e2 --
            <button@e1>next</button>
            <b@e2>a1</b>
            -- click button@e1 --
            <button@e1>next</button>
            <b@e3>b0</b>
            -- click b@e3 --
            <button@e1>next</button>
            <b@e3>b1</b>
            -- click button@e1 --
            <button@e1>next</button>
            <i@e4>none</i>
        "#]],
    );
}

#[test]
fn each_call_of_a_render_fn_that_returns_a_state_has_a_state_of_its_own() {
    check(
        indoc! {r#"
            render fn counter(start: Int) -> Tuple[Int, mut fn(fn(Int) -> Int) -> Tuple[]] {
              state!(start)
            }

            page Test {
              fn body() -> Html {
                let (a, set_a) = counter(0);
                let (b, set_b) = counter(10);
                <>
                  <button on:click={mut fn() { set_a(fn(curr) { curr + 1 }) }}>{a.to_string()}</button>
                  <button on:click={mut fn() { set_b(fn(curr) { curr + 1 }) }}>{b.to_string()}</button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            <button@e2>10</button>
            -- click button@e1 --
            <button@e1>1</button>
            <button@e2>10</button>
            -- click button@e2 --
            <button@e1>1</button>
            <button@e2>11</button>
            -- click button@e2 --
            <button@e1>1</button>
            <button@e2>12</button>
        "#]],
    );
}

#[test]
fn handlers_in_a_record_a_render_fn_returns_write_the_state_of_its_call() {
    check(
        indoc! {r#"
            record CounterModel {
              count: Int,
              incr: mut fn() -> Tuple[],
              decr: mut fn() -> Tuple[],
            }

            render fn use_counter() -> CounterModel {
              let (count, update_count) = state!(0);
              CounterModel {
                count: count,
                incr: mut fn() {
                  update_count(fn(curr) {
                    curr + 1
                  })
                },
                decr: mut fn() {
                  update_count(fn(curr) {
                    curr - 1
                  })
                },
              }
            }

            fn Counter() -> Html {
              let CounterModel {count, incr, decr} = use_counter();
              <div>
                {count.to_string()}
                <button on:click={incr}>
                  incr
                </button>
                <button on:click={decr}>
                  decr
                </button>
              </div>
            }

            page Test {
              fn body() -> Html {
                <Counter/>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(2),
            Event::Click(3),
            Event::Click(3),
            Event::Click(3),
        ],
        expect![[r#"
            -- render --
            <div@e1>
              0
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e2 --
            <div@e1>
              1
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e2 --
            <div@e1>
              2
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e3 --
            <div@e1>
              1
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e3 --
            <div@e1>
              0
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e3 --
            <div@e1>
              -1
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
        "#]],
    );
}

#[test]
fn an_updater_chooses_the_next_record_state_from_the_current_one() {
    check(
        indoc! {r#"
            enum CounterErr {
              CantIncrease,
              CantDecrease,
            }

            record CounterState {
              count: Int,
              err: Option[CounterErr],
            }

            record CounterModel {
              count: Int,
              err: Option[CounterErr],
              incr: mut fn() -> Tuple[],
              decr: mut fn() -> Tuple[],
            }

            render fn use_counter() -> CounterModel {
              let (state, update) = state!(CounterState {count: 0, err: None});
              let max_count = 2;
              let min_count = 0;
              CounterModel {
                count: state.count,
                err: state.err,
                incr: mut fn() {
                  update(fn(s) {
                    match s.count >= max_count {
                      true => CounterState {...s, err: Some(CounterErr::CantIncrease)},
                      false => CounterState {count: s.count + 1, err: None},
                    }
                  })
                },
                decr: mut fn() {
                  update(fn(s) {
                    match s.count <= min_count {
                      true => CounterState {...s, err: Some(CounterErr::CantDecrease)},
                      false => CounterState {count: s.count - 1, err: None},
                    }
                  })
                },
              }
            }

            fn Counter() -> Html {
              let CounterModel {count, err, incr, decr} = use_counter();
              <div>
                {count.to_string()}
                {match err {
                  None => <></>,
                  Some(CounterErr::CantIncrease) => <div>Can't increase counter</div>,
                  Some(CounterErr::CantDecrease) => <div>Can't decrease counter</div>,
                }}
                <button on:click={incr}>
                  incr
                </button>
                <button on:click={decr}>
                  decr
                </button>
              </div>
            }

            page Test {
              fn body() -> Html {
                <Counter/>
              }
            }
        "#},
        &[
            Event::Click(3),
            Event::Click(2),
            Event::Click(2),
            Event::Click(2),
            Event::Click(3),
        ],
        expect![[r#"
            -- render --
            <div@e1>
              0
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e3 --
            <div@e1>
              0
              <div@e4>Can't decrease counter</div>
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e2 --
            <div@e1>
              1
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e2 --
            <div@e1>
              2
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e2 --
            <div@e1>
              2
              <div@e5>Can't increase counter</div>
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
            -- click button@e3 --
            <div@e1>
              1
              <button@e2>incr</button>
              <button@e3>decr</button>
            </div>
        "#]],
    );
}

#[test]
fn the_text_an_arm_binds_stays_when_the_subject_is_set_to_an_equal_value() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (picked, set_picked) = state!(Some("a"));
                let again = mut fn() {
                  set_picked(fn(curr) {
                    match curr {
                      Some(s) => Some(s),
                      None => None,
                    }
                  })
                };

                <>
                  <b on:click={again}>again</b>
                  {match picked {
                    Some(s) => <p>{s}</p>,
                    None => <i>none</i>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>again</b>
            <p@e2>a</p>
            -- click b@e1 --
            <b@e1>again</b>
            <p@e2>a</p>
        "#]],
    );
}

#[test]
fn a_text_stays_when_a_state_the_match_no_longer_chooses_changes() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (use_x, set_use_x) = state!(true);
                let (x, set_x) = state!("x");
                let flip = mut fn() {
                  set_use_x(fn(curr) { !curr })
                };
                let grow = mut fn() {
                  set_x(fn(curr) { curr + "!" })
                };
                let shown = match use_x {
                  true => x,
                  false => "y",
                };

                <>
                  <b on:click={flip}>flip</b>
                  <i on:click={grow}>grow</i>
                  <p>{shown}</p>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <i@e2>grow</i>
            <p@e3>x</p>
            -- click b@e1 --
            <b@e1>flip</b>
            <i@e2>grow</i>
            <p@e3>y</p>
            -- click i@e2 --
            <b@e1>flip</b>
            <i@e2>grow</i>
            <p@e3>y</p>
        "#]],
    );
}

#[test]
fn an_arm_a_match_leaves_does_not_take_the_element_the_new_arm_shows() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (on, set_on) = state!(false);
                let toggle = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                let field = <input/>;

                <>
                  <b on:click={toggle}>toggle</b>
                  {match on {
                    true => field,
                    false => {
                      <div>
                        {match on {
                          true => field,
                          false => <p>off</p>,
                        }}
                      </div>
                    }
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>toggle</b>
            <div@e2>
              <p@e3>off</p>
            </div>
            -- click b@e1 --
            <b@e1>toggle</b>
            <input@e4>
        "#]],
    );
}

#[test]
fn a_presence_attribute_follows_its_state_on_and_off() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (on, set_on) = state!(false);
                let toggle = mut fn() {
                  set_on(fn(curr) { !curr })
                };

                <>
                  <b on:click={toggle}>toggle</b>
                  <input checked={on}/>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>toggle</b>
            <input@e2>
            -- click b@e1 --
            <b@e1>toggle</b>
            <input@e2 checked>
            -- click b@e1 --
            <b@e1>toggle</b>
            <input@e2>
        "#]],
    );
}

#[test]
fn each_lambda_in_an_array_has_state_of_its_own() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let shows = [
                  fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                    <b on:click={incr}>{count.to_string()}</b>
                  },
                  fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 10 }) };
                    <i on:click={incr}>{count.to_string()}</i>
                  },
                ];
                <>
                  {for show in shows {
                    show()
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(1),
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
        ],
        expect![[r#"
            -- render --
            <b@e1>0</b>
            <i@e2>0</i>
            -- click b@e1 --
            <b@e1>1</b>
            <i@e2>0</i>
            -- click i@e2 --
            <b@e1>1</b>
            <i@e2>10</i>
            -- click b@e1 --
            <b@e1>2</b>
            <i@e2>10</i>
            -- click i@e2 --
            <b@e1>2</b>
            <i@e2>20</i>
        "#]],
    );
}

#[test]
fn state_of_lambdas_in_an_array_starts_over_when_a_loop_without_a_key_applies_another_at_the_index()
{
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (flipped, set_flipped) = state!(false);
                let flip = mut fn() { set_flipped(fn(curr) { !curr }) };
                let first = fn() {
                  let (count, set_count) = state!(0);
                  let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                  <b on:click={incr}>{count.to_string()}</b>
                };
                let second = fn() {
                  let (count, set_count) = state!(0);
                  let incr = mut fn() { set_count(fn(curr) { curr + 10 }) };
                  <i on:click={incr}>{count.to_string()}</i>
                };
                let shows = match flipped {
                  true => [second, first],
                  false => [first, second],
                };
                <>
                  <button on:click={flip}>flip</button>
                  {for show in shows {
                    show()
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(3),
            Event::Click(1),
            Event::Click(4),
            Event::Click(5),
            Event::Click(1),
            Event::Click(6),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <b@e2>0</b>
            <i@e3>0</i>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e2>1</b>
            <i@e3>0</i>
            -- click i@e3 --
            <button@e1>flip</button>
            <b@e2>1</b>
            <i@e3>10</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e4>0</i>
            <b@e5>0</b>
            -- click i@e4 --
            <button@e1>flip</button>
            <i@e4>10</i>
            <b@e5>0</b>
            -- click b@e5 --
            <button@e1>flip</button>
            <i@e4>10</i>
            <b@e5>1</b>
            -- click button@e1 --
            <button@e1>flip</button>
            <b@e6>0</b>
            <i@e7>0</i>
            -- click b@e6 --
            <button@e1>flip</button>
            <b@e6>1</b>
            <i@e7>0</i>
        "#]],
    );
}

#[test]
fn state_of_lambdas_in_an_array_follows_the_key_of_the_loop_that_applies_them() {
    check(
        indoc! {r#"
            record Item {
              label: String,
              show: fn() -> Html,
            }

            page Test {
              fn body() -> Html {
                let (flipped, set_flipped) = state!(false);
                let flip = mut fn() { set_flipped(fn(curr) { !curr }) };
                let first = Item {
                  label: "A",
                  show: fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 1 }) };
                    <b on:click={incr}>{count.to_string()}</b>
                  },
                };
                let second = Item {
                  label: "B",
                  show: fn() {
                    let (count, set_count) = state!(0);
                    let incr = mut fn() { set_count(fn(curr) { curr + 10 }) };
                    <i on:click={incr}>{count.to_string()}</i>
                  },
                };
                let items = match flipped {
                  true => [second, first],
                  false => [first, second],
                };
                <>
                  <button on:click={flip}>flip</button>
                  {for item in items key item.label {
                    let show = item.show;
                    show()
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(3),
            Event::Click(1),
            Event::Click(2),
            Event::Click(3),
            Event::Click(1),
            Event::Click(2),
        ],
        expect![[r#"
            -- render --
            <button@e1>flip</button>
            <b@e2>0</b>
            <i@e3>0</i>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e2>1</b>
            <i@e3>0</i>
            -- click i@e3 --
            <button@e1>flip</button>
            <b@e2>1</b>
            <i@e3>10</i>
            -- click button@e1 --
            <button@e1>flip</button>
            <i@e3>10</i>
            <b@e2>1</b>
            -- click b@e2 --
            <button@e1>flip</button>
            <i@e3>10</i>
            <b@e2>2</b>
            -- click i@e3 --
            <button@e1>flip</button>
            <i@e3>20</i>
            <b@e2>2</b>
            -- click button@e1 --
            <button@e1>flip</button>
            <b@e2>2</b>
            <i@e3>20</i>
            -- click b@e2 --
            <button@e1>flip</button>
            <b@e2>3</b>
            <i@e3>20</i>
        "#]],
    );
}

#[test]
fn an_arm_stays_when_the_updates_of_a_click_leave_its_subject_the_same() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (a, set_a) = state!(0);
                let (b, set_b) = state!(0);
                let (c, _) = state!(0);
                let rest = b + c;
                let sum = a + rest;
                let shift = mut fn() {
                  set_a(fn(curr) { curr + 1 });
                  set_b(fn(curr) { curr - 1 })
                };

                <>
                  <b on:click={shift}>shift</b>
                  {match sum == 0 {
                    true => <p>same</p>,
                    false => <p>other</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>shift</b>
            <p@e2>same</p>
            -- click b@e1 --
            <b@e1>shift</b>
            <p@e2>same</p>
        "#]],
    );
}

#[test]
fn the_rows_a_loop_keeps_show_state_bound_outside_the_loop() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let incr_count = mut fn() {
                  set_count(fn(curr) { curr + 1 })
                };

                <>
                  <b on:click={incr_count}>raise</b>
                  {for id in [1, 2] {
                    <p>{id.to_string()}:{count.to_string()}</p>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>raise</b>
            <p@e2>1:0</p>
            <p@e3>2:0</p>
            -- click b@e1 --
            <b@e1>raise</b>
            <p@e2>1:1</p>
            <p@e3>2:1</p>
        "#]],
    );
}

#[test]
fn an_arm_stays_when_the_value_its_pattern_binds_changes() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (picked, set_picked) = state!(Some(0));
                let raise = mut fn() {
                  set_picked(fn(curr) {
                    match curr {
                      Some(n) => Some(n + 1),
                      None => None,
                    }
                  })
                };

                <>
                  <b on:click={raise}>raise</b>
                  {match picked {
                    Some(n) => <p>{n.to_string()}</p>,
                    None => <i>none</i>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>raise</b>
            <p@e2>0</p>
            -- click b@e1 --
            <b@e1>raise</b>
            <p@e2>1</p>
        "#]],
    );
}

#[test]
fn render_closures_in_an_array_applied_outside_a_match_keep_their_state_and_elements() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (first, set_first) = state!(true);
                let flip = mut fn() {
                  set_first(fn(curr) { !curr })
                };
                let counters = [
                  render fn() -> Html {
                    let (count, set_count) = state!(0);
                    let incr_count = mut fn() {
                      set_count(fn(curr) { curr + 1 })
                    };
                    <button on:click={incr_count}>A:{count.to_string()}</button>
                  },
                  render fn() -> Html {
                    let (count, set_count) = state!(0);
                    let incr_count = mut fn() {
                      set_count(fn(curr) { curr + 1 })
                    };
                    <button on:click={incr_count}>B:{count.to_string()}</button>
                  },
                ];
                let show_a = counters.get(0).unwrap_or(render fn() -> Html { <></> });
                let show_b = counters.get(1).unwrap_or(render fn() -> Html { <></> });
                let a = show_a();
                let b = show_b();

                <>
                  <b on:click={flip}>flip</b>
                  {match first {
                    true => a,
                    false => b,
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(3),
            Event::Click(3),
            Event::Click(1),
            Event::Click(2),
            Event::Click(1),
        ],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <button@e2>A:0</button>
            -- click button@e2 --
            <b@e1>flip</b>
            <button@e2>A:1</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e3>B:0</button>
            -- click button@e3 --
            <b@e1>flip</b>
            <button@e3>B:1</button>
            -- click button@e3 --
            <b@e1>flip</b>
            <button@e3>B:2</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e2>A:1</button>
            -- click button@e2 --
            <b@e1>flip</b>
            <button@e2>A:2</button>
            -- click b@e1 --
            <b@e1>flip</b>
            <button@e3>B:2</button>
        "#]],
    );
}

#[test]
fn a_match_keeps_its_arm_when_a_nested_match_changes_arm_and_gives_the_same_value() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (entered, set_entered) = state!(false);
                let (n, set_n) = state!(0);
                let enter = mut fn() {
                  set_entered(fn(_) { true })
                };
                let raise = mut fn() {
                  set_n(fn(curr) { curr + 1 })
                };
                let zero = match entered {
                  true => match n == 0 {
                    true => n,
                    false => 0,
                  },
                  false => 0,
                };

                <>
                  <b on:click={enter}>enter</b>
                  <i on:click={raise}>raise</i>
                  {match zero == 0 {
                    true => <p>zero</p>,
                    false => <p>nonzero</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <p@e3>zero</p>
            -- click b@e1 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <p@e3>zero</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <p@e3>zero</p>
        "#]],
    );
}

#[test]
fn a_text_stays_when_the_match_takes_an_arm_that_gives_the_same_value() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (picked, set_picked) = state!(Some("none"));
                let clear = mut fn() {
                  set_picked(fn(_) { None })
                };
                let shown = match picked {
                  Some(s) => s,
                  None => "none",
                };

                <>
                  <b on:click={clear}>clear</b>
                  <p>{shown}</p>
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>clear</b>
            <p@e2>none</p>
            -- click b@e1 --
            <b@e1>clear</b>
            <p@e2>none</p>
        "#]],
    );
}

#[test]
fn text_reading_a_nested_match_in_an_arm() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (entered, set_entered) = state!(false);
                let (on, set_on) = state!(false);
                let enter = mut fn() {
                  set_entered(fn(_) { true })
                };
                let flip = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                let inner = match entered {
                  true => match on {
                    true => "A",
                    false => "B",
                  },
                  false => "-",
                };
                let mark = match on {
                  true => "1",
                  false => "0",
                };

                <>
                  <b on:click={enter}>enter</b>
                  <i on:click={flip}>flip</i>
                  <p>{inner + mark}</p>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>-0</p>
            -- click b@e1 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>B0</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>A1</p>
        "#]],
    );
}

#[test]
fn state_bound_in_an_arm_takes_the_latest_value_of_a_nested_match() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (entered, set_entered) = state!(false);
                let (on, set_on) = state!(false);
                let enter = mut fn() {
                  set_entered(fn(_) { true })
                };
                let flip = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                let inner = match entered {
                  true => match on {
                    true => "A",
                    false => "B",
                  },
                  false => "-",
                };

                <>
                  <b on:click={enter}>enter</b>
                  <i on:click={flip}>flip</i>
                  {match on {
                    true => {
                      let (first, _) = state!(inner);
                      <p>{first}</p>
                    },
                    false => <p>off</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>off</p>
            -- click b@e1 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>off</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e4>A</p>
        "#]],
    );
}

#[test]
fn nested_loops_over_the_same_keys_both_follow_a_reorder() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (keys, set_keys) = state!(["A", "B"]);
                let flip = mut fn() {
                  set_keys(fn(_) { ["B", "A"] })
                };

                <>
                  <b on:click={flip}>flip</b>
                  {for row in keys key row {
                    {for col in keys key col {
                      <i>{row + col}</i>
                    }}
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>flip</b>
            <i@e2>AA</i>
            <i@e3>AB</i>
            <i@e4>BA</i>
            <i@e5>BB</i>
            -- click b@e1 --
            <b@e1>flip</b>
            <i@e5>BB</i>
            <i@e4>BA</i>
            <i@e3>AB</i>
            <i@e2>AA</i>
        "#]],
    );
}

#[test]
fn state_bound_in_an_arm_takes_the_latest_value_that_an_arm_two_matches_out_binds() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (picked, set_picked) = state!(Some(1));
                let (open, _) = state!(true);
                let (n, set_n) = state!(0);
                let both = mut fn() {
                  set_n(fn(_) { 1 });
                  set_picked(fn(_) { Some(2) })
                };

                <>
                  <b on:click={both}>both</b>
                  {match picked {
                    Some(x) => <>
                      <u>{x.to_string()}</u>
                      {match open {
                        true => match n < 1 {
                          true => <p>off</p>,
                          false => {
                            let (kept, _) = state!(x);
                            <p>{kept.to_string()}</p>
                          },
                        },
                        false => <p>closed</p>,
                      }}
                    </>,
                    None => <p>none</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>both</b>
            <u@e2>1</u>
            <p@e3>off</p>
            -- click b@e1 --
            <b@e1>both</b>
            <u@e2>2</u>
            <p@e4>2</p>
        "#]],
    );
}

#[test]
fn state_bound_in_an_arm_starts_when_the_match_takes_the_arm_before_anything_reads_its_value() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (entered, set_entered) = state!(false);
                let (shown, set_shown) = state!(false);
                let (n, set_n) = state!(0);
                let enter = mut fn() {
                  set_entered(fn(_) { true })
                };
                let raise = mut fn() {
                  set_n(fn(curr) { curr + 1 })
                };
                let show = mut fn() {
                  set_shown(fn(_) { true })
                };
                let first = match entered {
                  true => {
                    let (kept, _) = state!(n);
                    kept
                  },
                  false => 0,
                };

                <>
                  <b on:click={enter}>enter</b>
                  <i on:click={raise}>raise</i>
                  <u on:click={show}>show</u>
                  {match shown {
                    true => <p>{first.to_string()}</p>,
                    false => <p>off</p>,
                  }}
                </>
              }
            }
        "#},
        &[
            Event::Click(2),
            Event::Click(1),
            Event::Click(2),
            Event::Click(3),
        ],
        expect![[r#"
            -- render --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <u@e3>show</u>
            <p@e4>off</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <u@e3>show</u>
            <p@e4>off</p>
            -- click b@e1 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <u@e3>show</u>
            <p@e4>off</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <u@e3>show</u>
            <p@e4>off</p>
            -- click u@e3 --
            <b@e1>enter</b>
            <i@e2>raise</i>
            <u@e3>show</u>
            <p@e5>1</p>
        "#]],
    );
}

#[test]
fn state_bound_in_an_arm_takes_the_latest_value_of_a_nested_match_that_binds_state_in_an_arm() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (entered, set_entered) = state!(false);
                let (on, set_on) = state!(false);
                let enter = mut fn() {
                  set_entered(fn(_) { true })
                };
                let flip = mut fn() {
                  set_on(fn(curr) { !curr })
                };
                let inner = match entered {
                  true => match on {
                    true => {
                      let (kept, _) = state!("A");
                      kept
                    },
                    false => "B",
                  },
                  false => "-",
                };

                <>
                  <b on:click={enter}>enter</b>
                  <i on:click={flip}>flip</i>
                  {match on {
                    true => {
                      let (first, _) = state!(inner);
                      <p>{first}</p>
                    },
                    false => <p>off</p>,
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>off</p>
            -- click b@e1 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e3>off</p>
            -- click i@e2 --
            <b@e1>enter</b>
            <i@e2>flip</i>
            <p@e4>A</p>
        "#]],
    );
}
