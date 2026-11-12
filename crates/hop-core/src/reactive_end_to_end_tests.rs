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
              let mut count = 0;
              <button on:click={fn() { count.set(count + 1) }}>{count.to_string()}</button>
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
              let mut count = 0;
              <button on:click={fn() { count.set(count + 1) }}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut reversed = false;
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={fn() { reversed.set(!reversed) }}>flip</b>
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
              let mut count = 0;
              <button on:click={fn() { count.set(count + 1) }}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut reversed = false;
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={fn() { reversed.set(!reversed) }}>flip</b>
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
              let mut count = 0;
              <button on:click={fn() { count.set(count + 1) }}>{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut open = true;
                <>
                  <b on:click={fn() { open.set(!open) }}>toggle</b>
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
                let mut on = true;
                let field = <input/>;
                <>
                  <button on:click={fn() { on.set(!on) }}>toggle</button>
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
fn the_writes_of_a_click_read_the_cells_as_rendered_and_the_last_write_wins() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() { count.set(count + 1).and_then(fn(_) { count.set(count + 10) }) }}>
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
fn a_click_runs_the_handlers_of_the_elements_around_it_innermost_first() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut clicked = "none";
                let mut inner = 0;
                <div on:click={fn() { clicked.set("div") }}>
                  <button on:click={fn() { clicked.set("button").and_then(fn(_) { inner.set(inner + 1) }) }}>
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
            fn Button(on_press: fn() -> Effect[Tuple[]], children: Html) -> Html {
              <button on:click={on_press}>{children}</button>
            }
            page Test {
              fn body() -> Html {
                let mut on = false;
                <Button on_press={fn() { on.set(!on) }}>
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
                let mut shown = true;
                let field = <input/>;
                <>
                  <button on:click={fn() { shown.set(!shown) }}>toggle</button>
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
                let mut shown = true;
                let mut count = 0;
                <>
                  <b on:click={fn() { shown.set(!shown) }}>toggle</b>
                  {match shown {
                    true => <button on:click={fn() { count.set(count + 1) }}>{count.to_string()}</button>,
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
                let mut count = 0;
                <>
                  <button on:click={fn() { count.set(forever(count)) }}>forever</button>
                  <button on:click={fn() { count.set(count + 1) }}>{count.to_string()}</button>
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
                let mut count = 0;
                <>
                  <div on:click={fn() { count.set(forever(count)) }}>
                    <button on:click={fn() { count.set(count + 1) }}>{count.to_string()}</button>
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
fn a_handler_whose_effect_is_empty_writes_nothing() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() {
                  match count < 1 {
                    true => count.set(count + 1),
                    false => pure!(()),
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
                let mut count = 0;
                <>
                  <button on:click={fn() { count.set(count + 1) }}>
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
                let mut a = 1;
                let mut b = a * 10;
                <>
                  <button on:click={fn() { a.set(a + 1) }}>{a.to_string()}</button>
                  <button on:click={fn() { b.set(b + 1) }}>{b.to_string()}</button>
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
              let mut count = start;
              <button on:click={fn() { count.set(count + 1) }}>
                {start.to_string()}/{count.to_string()}
              </button>
            }
            page Test {
              fn body() -> Html {
                let mut base = 0;
                <>
                  <b on:click={fn() { base.set(base + 100) }}>bump</b>
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
                let mut outer = 0;
                <Twice row={fn() {
                  let mut inner = 0;
                  <button on:click={fn() { inner.set(inner + 1).and_then(fn(_) { outer.set(outer + 1) }) }}>
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
                let mut left = true;
                <>
                  <b on:click={fn() { left.set(!left) }}>switch</b>
                  {match left {
                    true => {
                      let mut count = 0;
                      <button on:click={fn() { count.set(count + 1) }}>left {count.to_string()}</button>
                    },
                    false => {
                      let mut count = 100;
                      <button on:click={fn() { count.set(count + 1) }}>right {count.to_string()}</button>
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
                let mut ids = [1, 2];
                <>
                  <b on:click={fn() { ids.set([2, 1]) }}>flip</b>
                  <i on:click={fn() { ids.set([2]) }}>drop</i>
                  {for id in ids key id {
                    let mut count = 0;
                    <button on:click={fn() { count.set(count + 1) }}>{id.to_string()}:{count.to_string()}</button>
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
                let mut ids = [1, 1];
                <>
                  <b on:click={fn() { ids.set([1]) }}>drop</b>
                  {for id in ids key id {
                    let mut count = 0;
                    <button on:click={fn() { count.set(count + 1) }}>{id.to_string()}:{count.to_string()}</button>
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
                let mut rows = ["a", "b"];
                <>
                  <b on:click={fn() { rows.set(["b", "a"]) }}>flip</b>
                  {for row in rows key row {
                    {for col in [1, 2] key col {
                      let mut count = 0;
                      <button on:click={fn() { count.set(count + 1) }}>
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
                let mut on = false;
                <button
                  class={match on {true => "on", false => "off"}}
                  on:click={fn() { on.set(!on) }}
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
              let mut count = 0;
              <div>
                <button on:click={fn() { count.set(count + 1) }}>
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
                let mut levels = 2;
                <>
                  <b on:click={fn() { levels.set(levels + 1) }}>deeper</b>
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
fn a_handler_performs_the_effect_a_function_gives() {
    check(
        indoc! {r#"
            fn both(first: fn() -> Effect[Tuple[]], second: fn() -> Effect[Tuple[]]) -> Effect[Tuple[]] {
              first().and_then(fn(_) { second() })
            }
            page Test {
              fn body() -> Html {
                let mut a = 0;
                let mut b = 0;
                <button on:click={fn() { both(fn() { a.set(a + 1) }, fn() { b.set(b + 10) }) }}>
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
fn a_handler_performs_an_effect_passed_as_a_parameter() {
    check(
        indoc! {r#"
            fn Button(press: Effect[Tuple[]], children: Html) -> Html {
              <button on:click={fn() { press }}>{children}</button>
            }
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <>
                  <Button press={count.set(count + 1)}>{count.to_string()}</Button>
                  <Button press={count.set(0)}>reset</Button>
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
                let mut setting = Setting {mode: Mode::Light, level: 1};
                <>
                  <button on:click={fn() { setting.set(Setting {...setting, mode: Mode::Dark}) }}>dark</button>
                  <button on:click={fn() { setting.set(Setting {...setting, level: setting.level + 1}) }}>raise</button>
                  <p>{match setting.mode {Mode::Light => "light", Mode::Dark => "dark"}} {setting.level.to_string()}</p>
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
fn a_continuation_reads_the_cell_as_the_effect_before_it_wrote_it() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() {
                  count.set(count + 1).and_then(fn(_) {
                    count.get().and_then(fn(n) { count.set(n + 1) })
                  })
                }}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            <button@e1>2</button>
            -- click button@e1 --
            <button@e1>4</button>
        "#]],
    );
}

#[test]
fn a_handler_gives_the_empty_effect_with_pure() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() {
                  match count < 2 {
                    true => count.set(count + 1),
                    false => pure!(()),
                  }
                }}>{count.to_string()}</button>
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
            <button@e1>2</button>
        "#]],
    );
}

#[test]
fn a_continuation_takes_the_value_a_function_gives_with_pure() {
    check(
        indoc! {r#"
            fn bump(n: Int) -> Effect[Int] {
              pure!(n + 1)
            }
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() {
                  bump(count).and_then(fn(n) { count.set(n) })
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
            <button@e1>2</button>
        "#]],
    );
}

#[test]
fn a_click_whose_continuation_fails_changes_nothing() {
    check(
        indoc! {r#"
            fn forever(n: Int) -> Int {
              forever(n + 1)
            }
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={fn() {
                  count.set(count + 1).and_then(fn(_) { count.set(forever(count)) })
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
fn a_continuation_takes_its_parameter_type_from_the_effect_before_it() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                let mut label = "none";
                <button on:click={fn() {
                  count.set(count + 1).and_then(fn(_) {
                    count.get().and_then(fn(n) { label.set(n.to_string()) })
                  })
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
