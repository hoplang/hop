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
fn each_call_has_cells_of_its_own() {
    check(
        indoc! {r#"
            fn Counter() -> Html {
              let (count, set_count) = state!(0);
              let bump = mut fn() { set_count(count + 1) };
              <button on:click={bump}>{count.to_string()}</button>
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
fn the_cells_of_a_keyed_loop_follow_the_keys() {
    check(
        indoc! {r#"
            fn Counter(label: String) -> Html {
              let (count, set_count) = state!(0);
              let bump = mut fn() { set_count(count + 1) };
              <button on:click={bump}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let (reversed, set_reversed) = state!(false);
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={mut fn() { set_reversed(!reversed) }}>flip</b>
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
fn the_cells_of_a_loop_without_a_key_follow_the_index() {
    check(
        indoc! {r#"
            fn Counter(label: String) -> Html {
              let (count, set_count) = state!(0);
              let bump = mut fn() { set_count(count + 1) };
              <button on:click={bump}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let (reversed, set_reversed) = state!(false);
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={mut fn() { set_reversed(!reversed) }}>flip</b>
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
fn a_cell_and_an_element_of_a_call_in_an_arm_start_over_when_the_match_takes_the_arm_again() {
    check(
        indoc! {r#"
            fn Counter() -> Html {
              let (count, set_count) = state!(0);
              let bump = mut fn() { set_count(count + 1) };
              <button on:click={bump}>{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let (open, set_open) = state!(true);
                <>
                  <b on:click={mut fn() { set_open(!open) }}>toggle</b>
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
                let field = <input/>;
                <>
                  <button on:click={mut fn() { set_on(!on) }}>toggle</button>
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
                <div on:click={mut fn() { set_clicked("div") }}>
                  <button on:click={mut fn() { set_clicked("button"); set_inner(inner + 1) }}>
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
fn a_handler_reaches_an_element_through_a_parameter() {
    check(
        indoc! {r#"
            fn Button(on_press: mut fn() -> Tuple[], children: Html) -> Html {
              <button on:click={on_press}>{children}</button>
            }
            page Test {
              fn body() -> Html {
                let (on, set_on) = state!(false);
                <Button on_press={mut fn() { set_on(!on) }}>
                  {match on {true => "on", false => "off"}}
                </Button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>off</button>
            -- click button@e1 --
            <button@e1>on</button>
            -- click button@e1 --
            <button@e1>off</button>
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
                let field = <input/>;
                <>
                  <button on:click={mut fn() { set_shown(!shown) }}>toggle</button>
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
fn a_cell_a_match_hides_keeps_its_value_while_it_is_evaluated() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (shown, set_shown) = state!(true);
                let (count, set_count) = state!(0);
                let bump = mut fn() { set_count(count + 1) };
                <>
                  <b on:click={mut fn() { set_shown(!shown) }}>toggle</b>
                  {match shown {
                    true => <button on:click={bump}>{count.to_string()}</button>,
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
fn a_click_whose_handler_fails_changes_nothing() {
    check(
        indoc! {r#"
            fn forever(n: Int) -> Int {
              forever(n + 1)
            }
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let bump = mut fn() { set_count(count + 1) };
                <>
                  <button on:click={mut fn() { set_count(forever(count)) }}>forever</button>
                  <button on:click={bump}>{count.to_string()}</button>
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
fn a_click_fails_and_changes_nothing_when_a_handler_around_the_element_fails() {
    check(
        indoc! {r#"
            fn forever(n: Int) -> Int {
              forever(n + 1)
            }
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let bump = mut fn() { set_count(count + 1) };
                <>
                  <div on:click={mut fn() { set_count(forever(count)) }}>
                    <button on:click={bump}>{count.to_string()}</button>
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
                <button on:click={mut fn() {
                  match count < 1 {
                    true => set_count(count + 1),
                    false => (),
                  }
                }}>{count.to_string()}</button>
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
                <>
                  <button on:click={mut fn() { set_count(count + 1) }}>
                    <b>{count.to_string()}</b>
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
fn a_cell_takes_its_initial_value_only_when_it_is_first_bound() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (a, set_a) = state!(1);
                let (b, set_b) = state!(a * 10);
                <>
                  <button on:click={mut fn() { set_a(a + 1) }}>{a.to_string()}</button>
                  <button on:click={mut fn() { set_b(b + 1) }}>{b.to_string()}</button>
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
fn a_cell_keeps_its_value_when_the_argument_its_initial_value_reads_changes() {
    check(
        indoc! {r#"
            fn Counter(start: Int) -> Html {
              let (count, set_count) = state!(start);
              <button on:click={mut fn() { set_count(count + 1) }}>
                {start.to_string()}/{count.to_string()}
              </button>
            }
            page Test {
              fn body() -> Html {
                let (base, set_base) = state!(0);
                <>
                  <b on:click={mut fn() { set_base(base + 100) }}>bump</b>
                  <Counter start={base}/>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- render --
            <b@e1>bump</b>
            <button@e2>0/0</button>
            -- click button@e2 --
            <b@e1>bump</b>
            <button@e2>0/1</button>
            -- click b@e1 --
            <b@e1>bump</b>
            <button@e2>100/1</button>
        "#]],
    );
}

#[test]
fn each_application_of_a_lambda_has_cells_of_its_own_and_shares_the_cells_it_captures() {
    check(
        indoc! {r#"
            fn Twice(row: fn() -> Html) -> Html {
              <>{row()}{row()}</>
            }
            page Test {
              fn body() -> Html {
                let (outer, set_outer) = state!(0);
                <Twice row={fn() {
                  let (inner, set_inner) = state!(0);
                  <button on:click={mut fn() { set_inner(inner + 1); set_outer(outer + 1) }}>
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
fn a_cell_bound_in_an_arm_starts_over_when_the_match_takes_the_arm_again() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (left, set_left) = state!(true);
                <>
                  <b on:click={mut fn() { set_left(!left) }}>switch</b>
                  {match left {
                    true => {
                      let (count, set_count) = state!(0);
                      let bump = mut fn() { set_count(count + 1) };
                      <button on:click={bump}>left {count.to_string()}</button>
                    },
                    false => {
                      let (count, set_count) = state!(100);
                      let bump = mut fn() { set_count(count + 1) };
                      <button on:click={bump}>right {count.to_string()}</button>
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
fn a_cell_bound_in_a_loop_follows_its_key_and_starts_over_when_the_key_comes_back() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (ids, set_ids) = state!([1, 2]);
                <>
                  <b on:click={mut fn() { set_ids([2, 1]) }}>flip</b>
                  <i on:click={mut fn() { set_ids([2]) }}>drop</i>
                  {for id in ids key id {
                    let (count, set_count) = state!(0);
                    let bump = mut fn() { set_count(count + 1) };
                    <button on:click={bump}>{id.to_string()}:{count.to_string()}</button>
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
                <>
                  <b on:click={mut fn() { set_ids([1]) }}>drop</b>
                  {for id in ids key id {
                    let (count, set_count) = state!(0);
                    let bump = mut fn() { set_count(count + 1) };
                    <button on:click={bump}>{id.to_string()}:{count.to_string()}</button>
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
fn the_cells_of_nested_loops_follow_the_keys_of_both_loops() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (rows, set_rows) = state!(["a", "b"]);
                <>
                  <b on:click={mut fn() { set_rows(["b", "a"]) }}>flip</b>
                  {for row in rows key row {
                    {for col in [1, 2] key col {
                      let (count, set_count) = state!(0);
                      <button on:click={mut fn() { set_count(count + 1) }}>
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
                <button
                  class={match on {true => "on", false => "off"}}
                  on:click={mut fn() { set_on(!on) }}
                >toggle</button>
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
fn each_level_of_a_recursive_call_has_cells_of_its_own() {
    check(
        indoc! {r#"
            fn Level(depth: Int, levels: Int) -> Html {
              let (count, set_count) = state!(0);
              <div>
                <button on:click={mut fn() { set_count(count + 1) }}>
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
                <>
                  <b on:click={mut fn() { set_levels(levels + 1) }}>deeper</b>
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
                let bump_a = mut fn() { set_a(a + 1) };
                let bump_b = mut fn() { set_b(b + 10) };
                <button on:click={mut fn() { both(bump_a, bump_b) }}>
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
              <button on:click={press}>{children}</button>
            }
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                let bump = mut fn() { set_count(count + 1) };
                <>
                  <Button press={bump}>{count.to_string()}</Button>
                  <Button press={mut fn() { set_count(0) }}>reset</Button>
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
fn a_handler_updates_a_field_of_a_record_in_a_cell_and_keeps_the_others() {
    check(
        indoc! {r#"
            enum Mode { Light, Dark }
            record Setting { mode: Mode, level: Int }
            page Test {
              fn body() -> Html {
                let (setting, set_setting) = state!(Setting {mode: Mode::Light, level: 1});
                let dark = mut fn() { set_setting(Setting {...setting, mode: Mode::Dark}) };
                let raise = mut fn() {
                  set_setting(Setting {...setting, level: setting.level + 1})
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
                <button on:click={mut fn() {
                  set_count(count + 1);
                  set_count(forever(count))
                }}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            error: Program exceeded the call depth limit of 12
        "#]],
    );
}

#[test]
fn a_write_reads_a_signal_that_an_earlier_write_of_the_click_wrote() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = signal!(0);
                let (label, set_label) = state!("none");
                <button on:click={mut fn() {
                  set_count(count() + 1);
                  set_label(count().to_string())
                }}>{label}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>none</button>
            -- click button@e1 --
            <button@e1>1</button>
            -- click button@e1 --
            <button@e1>2</button>
        "#]],
    );
}

#[test]
fn each_call_has_a_state_of_its_own() {
    check(
        indoc! {r#"
            fn Counter() -> Html {
              let (count, set_count) = state!(0);
              <button on:click={mut fn() { set_count(count + 1) }}>{count.to_string()}</button>
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
fn a_handler_sets_a_state_from_the_value_the_render_gave() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = state!(0);
                <button on:click={mut fn() { set_count(count + 1); set_count(count + 10) }}>
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
fn a_handler_reads_the_value_a_signal_has_now() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (count, set_count) = signal!(0);
                <button on:click={mut fn() { set_count(count() + 1); set_count(count() + 10) }}>
                  {count().to_string()}
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
fn a_handler_that_updates_a_record_state_twice_keeps_only_the_last_update() {
    check(
        indoc! {r#"
            record Setting { level: Int, size: Int }
            page Test {
              fn body() -> Html {
                let (setting, set_setting) = state!(Setting {level: 0, size: 0});
                let (live, set_live) = signal!(Setting {level: 0, size: 0});
                <>
                  <button on:click={mut fn() {
                    set_setting(Setting {...setting, level: setting.level + 1});
                    set_setting(Setting {...setting, size: setting.size + 1})
                  }}>
                    {setting.level.to_string()}/{setting.size.to_string()}
                  </button>
                  <button on:click={mut fn() {
                    set_live(Setting {...live(), level: live().level + 1});
                    set_live(Setting {...live(), size: live().size + 1})
                  }}>
                    {live().level.to_string()}/{live().size.to_string()}
                  </button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- render --
            <button@e1>0/0</button>
            <button@e2>0/0</button>
            -- click button@e1 --
            <button@e1>0/1</button>
            <button@e2>0/0</button>
            -- click button@e2 --
            <button@e1>0/1</button>
            <button@e2>1/1</button>
        "#]],
    );
}

#[test]
fn the_states_of_a_keyed_loop_follow_the_keys() {
    check(
        indoc! {r#"
            fn Counter(label: String) -> Html {
              let (count, set_count) = state!(0);
              <button on:click={mut fn() { set_count(count + 1) }}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let (ids, set_ids) = state!([1, 2]);
                <>
                  <b on:click={mut fn() { set_ids([2, 1]) }}>flip</b>
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
fn a_state_bound_in_an_arm_starts_over_when_the_match_takes_the_arm_again() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let (open, set_open) = state!(true);
                <>
                  <b on:click={mut fn() { set_open(!open) }}>toggle</b>
                  {match open {
                    true => {
                      let (count, set_count) = state!(0);
                      <button on:click={mut fn() { set_count(count + 1) }}>{count.to_string()}</button>
                    },
                    false => <i>closed</i>,
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
            <i@e3>closed</i>
            -- click b@e1 --
            <b@e1>toggle</b>
            <button@e4>0</button>
        "#]],
    );
}

#[test]
fn a_signal_passed_to_a_component_reads_and_writes_the_cell_of_the_caller() {
    check(
        indoc! {r#"
            fn Stepper(value: read fn() -> Int, set_value: mut fn(Int) -> Tuple[]) -> Html {
              <button on:click={mut fn() { set_value(value() + 1); set_value(value() * 2) }}>
                {value().to_string()}
              </button>
            }
            page Test {
              fn body() -> Html {
                let (count, set_count) = signal!(1);
                <>
                  <Stepper value={count} set_value={set_count}/>
                  <p>{count().to_string()}</p>
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
