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
    let flat = program.to_string();
    let optimized = optimize_flat(program);
    assert_eq!(
        run(&optimized, page, events),
        out,
        "the optimized program renders differently"
    );
    expected.assert_eq(&format!("-- flat --\n{flat}\n{out}"));
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
              let bump = mut fn() { count = count + 1 };
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
            -- flat --
            render fn Counter@f0() -> Html {
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v10: mut () -> Tuple[] = fn() {
                let v6: Int = cell_read(v4)
                let v7: Int = 1
                let v8: Int = int_add(v6, v7)
                let v9: Tuple[] = cell_write(v4, v8)
                v9
              }
              let v11: String = int_to_string(v5)
              let v12: Html = escape(v11)
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "button", {}, v13) on_click v10
              v15
            }
            entrypoint body@e0() -> Html {
              let v0: Html = f0()
              let v1: Html = f0()
              let v2: Html = concat(v0, v1)
              v2
            }

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
              let bump = mut fn() { count = count + 1 };
              <button on:click={bump}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut reversed = false;
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={mut fn() { reversed = !reversed }}>flip</b>
                  {for id in ids key id {
                    <Counter label={id.to_string()}/>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Counter@f0(
              label@v23: String,
            ) -> Html {
              let v25: Cell[Int] = cell {
                let v24: Int = 0
                v24
              }
              let v26: Int = cell_read(v25)
              let v31: mut () -> Tuple[] = fn() {
                let v27: Int = cell_read(v25)
                let v28: Int = 1
                let v29: Int = int_add(v27, v28)
                let v30: Tuple[] = cell_write(v25, v29)
                v30
              }
              let v32: Html = escape(v23)
              let v33: Html = text(":")
              let v34: String = int_to_string(v26)
              let v35: Html = escape(v34)
              let v36: Html = concat(v32, v33, v35)
              let v37: Id = mint()
              let v38: Html = element(v37, "button", {}, v36) on_click v31
              v38
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v9: Array[Int] = match v2 {
                true => {
                  let v3: Int = 2
                  let v4: Int = 1
                  let v5: Array[Int] = [v3, v4]
                  v5
                }
                false => {
                  let v6: Int = 1
                  let v7: Int = 2
                  let v8: Array[Int] = [v6, v7]
                  v8
                }
              }
              let v13: mut () -> Tuple[] = fn() {
                let v10: Bool = cell_read(v1)
                let v11: Bool = bool_not(v10)
                let v12: Tuple[] = cell_write(v1, v11)
                v12
              }
              let v14: Html = text("flip")
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "b", {}, v15) on_click v13
              let v21: Html = for v18: Int in v9 key {
                v18
              } {
                let v19: String = int_to_string(v18)
                let v20: Html = f0(v19)
                v20
              }
              let v22: Html = concat(v17, v21)
              v22
            }

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
              let bump = mut fn() { count = count + 1 };
              <button on:click={bump}>{label}:{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut reversed = false;
                let ids = match reversed {
                  true => [2, 1],
                  false => [1, 2],
                };
                <>
                  <b on:click={mut fn() { reversed = !reversed }}>flip</b>
                  {for id in ids {
                    <Counter label={id.to_string()}/>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Counter@f0(
              label@v23: String,
            ) -> Html {
              let v25: Cell[Int] = cell {
                let v24: Int = 0
                v24
              }
              let v26: Int = cell_read(v25)
              let v31: mut () -> Tuple[] = fn() {
                let v27: Int = cell_read(v25)
                let v28: Int = 1
                let v29: Int = int_add(v27, v28)
                let v30: Tuple[] = cell_write(v25, v29)
                v30
              }
              let v32: Html = escape(v23)
              let v33: Html = text(":")
              let v34: String = int_to_string(v26)
              let v35: Html = escape(v34)
              let v36: Html = concat(v32, v33, v35)
              let v37: Id = mint()
              let v38: Html = element(v37, "button", {}, v36) on_click v31
              v38
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v9: Array[Int] = match v2 {
                true => {
                  let v3: Int = 2
                  let v4: Int = 1
                  let v5: Array[Int] = [v3, v4]
                  v5
                }
                false => {
                  let v6: Int = 1
                  let v7: Int = 2
                  let v8: Array[Int] = [v6, v7]
                  v8
                }
              }
              let v13: mut () -> Tuple[] = fn() {
                let v10: Bool = cell_read(v1)
                let v11: Bool = bool_not(v10)
                let v12: Tuple[] = cell_write(v1, v11)
                v12
              }
              let v14: Html = text("flip")
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "b", {}, v15) on_click v13
              let v21: Html = for v18: Int in v9 {
                let v19: String = int_to_string(v18)
                let v20: Html = f0(v19)
                v20
              }
              let v22: Html = concat(v17, v21)
              v22
            }

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
              let bump = mut fn() { count = count + 1 };
              <button on:click={bump}>{count.to_string()}</button>
            }
            page Test {
              fn body() -> Html {
                let mut open = true;
                <>
                  <b on:click={mut fn() { open = !open }}>toggle</b>
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
            -- flat --
            render fn Counter@f0() -> Html {
              let v19: Cell[Int] = cell {
                let v18: Int = 0
                v18
              }
              let v20: Int = cell_read(v19)
              let v25: mut () -> Tuple[] = fn() {
                let v21: Int = cell_read(v19)
                let v22: Int = 1
                let v23: Int = int_add(v21, v22)
                let v24: Tuple[] = cell_write(v19, v23)
                v24
              }
              let v26: String = int_to_string(v20)
              let v27: Html = escape(v26)
              let v28: Html = concat(v27)
              let v29: Id = mint()
              let v30: Html = element(v29, "button", {}, v28) on_click v25
              v30
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v6: mut () -> Tuple[] = fn() {
                let v3: Bool = cell_read(v1)
                let v4: Bool = bool_not(v3)
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v7: Html = text("toggle")
              let v8: Html = concat(v7)
              let v9: Id = mint()
              let v10: Html = element(v9, "b", {}, v8) on_click v6
              let v16: Html = match v2 {
                true => {
                  let v11: Html = f0()
                  v11
                }
                false => {
                  let v12: Html = text("closed")
                  let v13: Html = concat(v12)
                  let v14: Id = mint()
                  let v15: Html = element(v14, "i", {}, v13)
                  v15
                }
              }
              let v17: Html = concat(v10, v16)
              v17
            }

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
                  <button on:click={mut fn() { on = !on }}>toggle</button>
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v3: Id = mint()
              let v4: Html = element(v3, "input", {})
              let v8: mut () -> Tuple[] = fn() {
                let v5: Bool = cell_read(v1)
                let v6: Bool = bool_not(v5)
                let v7: Tuple[] = cell_write(v1, v6)
                v7
              }
              let v9: Html = text("toggle")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "button", {}, v10) on_click v8
              let v19: Html = match v2 {
                true => {
                  let v13: Html = concat(v4)
                  let v14: Id = mint()
                  let v15: Html = element(v14, "div", {}, v13)
                  v15
                }
                false => {
                  let v16: Html = concat(v4)
                  let v17: Id = mint()
                  let v18: Html = element(v17, "p", {}, v16)
                  v18
                }
              }
              let v20: Html = concat(v12, v19)
              v20
            }

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
fn a_write_of_a_click_reads_the_cells_as_the_writes_before_it_left_them() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                <button on:click={mut fn() { count = count + 1; count = count + 10 }}>
                  {count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v11: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                let v7: Int = cell_read(v1)
                let v8: Int = 10
                let v9: Int = int_add(v7, v8)
                let v10: Tuple[] = cell_write(v1, v9)
                v10
              }
              let v12: String = int_to_string(v2)
              let v13: Html = escape(v12)
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "button", {}, v14) on_click v11
              v16
            }

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
fn a_click_runs_the_handlers_of_the_elements_around_it_innermost_first() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut clicked = "none";
                let mut inner = 0;
                <div on:click={mut fn() { clicked = "div" }}>
                  <button on:click={mut fn() { clicked = "button"; inner = inner + 1 }}>
                    {clicked} {inner.to_string()}
                  </button>
                </div>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[String] = cell {
                let v0: String = "none"
                v0
              }
              let v2: String = cell_read(v1)
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v8: mut () -> Tuple[] = fn() {
                let v6: String = "div"
                let v7: Tuple[] = cell_write(v1, v6)
                v7
              }
              let v15: mut () -> Tuple[] = fn() {
                let v9: String = "button"
                let v10: Tuple[] = cell_write(v1, v9)
                let v11: Int = cell_read(v4)
                let v12: Int = 1
                let v13: Int = int_add(v11, v12)
                let v14: Tuple[] = cell_write(v4, v13)
                v14
              }
              let v16: Html = escape(v2)
              let v17: Html = text(" ")
              let v18: String = int_to_string(v5)
              let v19: Html = escape(v18)
              let v20: Html = concat(v16, v17, v19)
              let v21: Id = mint()
              let v22: Html = element(v21, "button", {}, v20) on_click v15
              let v23: Html = concat(v22)
              let v24: Id = mint()
              let v25: Html = element(v24, "div", {}, v23) on_click v8
              v25
            }

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
                let mut on = false;
                <Button on_press={mut fn() { on = !on }}>
                  {match on {true => "on", false => "off"}}
                </Button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Button@f0(
              on_press@v13: mut () -> Tuple[],
              children@v14: Html,
            ) -> Html {
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v13
              v17
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v6: mut () -> Tuple[] = fn() {
                let v3: Bool = cell_read(v1)
                let v4: Bool = bool_not(v3)
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v9: String = match v2 {
                true => {
                  let v7: String = "on"
                  v7
                }
                false => {
                  let v8: String = "off"
                  v8
                }
              }
              let v10: Html = escape(v9)
              let v11: Html = concat(v10)
              let v12: Html = f0(v6, v11)
              v12
            }

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
                  <button on:click={mut fn() { shown = !shown }}>toggle</button>
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v3: Id = mint()
              let v4: Html = element(v3, "input", {})
              let v8: mut () -> Tuple[] = fn() {
                let v5: Bool = cell_read(v1)
                let v6: Bool = bool_not(v5)
                let v7: Tuple[] = cell_write(v1, v6)
                v7
              }
              let v9: Html = text("toggle")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "button", {}, v10) on_click v8
              let v20: Html = match v2 {
                true => {
                  let v13: Html = concat(v4)
                  let v14: Id = mint()
                  let v15: Html = element(v14, "div", {}, v13)
                  v15
                }
                false => {
                  let v16: Html = text("hidden")
                  let v17: Html = concat(v16)
                  let v18: Id = mint()
                  let v19: Html = element(v18, "p", {}, v17)
                  v19
                }
              }
              let v21: Html = concat(v12, v20)
              v21
            }

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
                let bump = mut fn() { count = count + 1 };
                <>
                  <b on:click={mut fn() { shown = !shown }}>toggle</b>
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v10: mut () -> Tuple[] = fn() {
                let v6: Int = cell_read(v4)
                let v7: Int = 1
                let v8: Int = int_add(v6, v7)
                let v9: Tuple[] = cell_write(v4, v8)
                v9
              }
              let v14: mut () -> Tuple[] = fn() {
                let v11: Bool = cell_read(v1)
                let v12: Bool = bool_not(v11)
                let v13: Tuple[] = cell_write(v1, v12)
                v13
              }
              let v15: Html = text("toggle")
              let v16: Html = concat(v15)
              let v17: Id = mint()
              let v18: Html = element(v17, "b", {}, v16) on_click v14
              let v28: Html = match v2 {
                true => {
                  let v19: String = int_to_string(v5)
                  let v20: Html = escape(v19)
                  let v21: Html = concat(v20)
                  let v22: Id = mint()
                  let v23: Html = element(v22, "button", {}, v21) on_click v10
                  v23
                }
                false => {
                  let v24: Html = text("hidden")
                  let v25: Html = concat(v24)
                  let v26: Id = mint()
                  let v27: Html = element(v26, "p", {}, v25)
                  v27
                }
              }
              let v29: Html = concat(v18, v28)
              v29
            }

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
                let bump = mut fn() { count = count + 1 };
                <>
                  <button on:click={mut fn() { count = forever(count) }}>forever</button>
                  <button on:click={bump}>{count.to_string()}</button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- flat --
            fn forever@f0(
              n@v22: Int,
            ) -> Int {
              let v23: Int = 1
              let v24: Int = int_add(v22, v23)
              let v25: Int = f0(v24)
              v25
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v11: mut () -> Tuple[] = fn() {
                let v8: Int = cell_read(v1)
                let v9: Int = f0(v8)
                let v10: Tuple[] = cell_write(v1, v9)
                v10
              }
              let v12: Html = text("forever")
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "button", {}, v13) on_click v11
              let v16: String = int_to_string(v2)
              let v17: Html = escape(v16)
              let v18: Html = concat(v17)
              let v19: Id = mint()
              let v20: Html = element(v19, "button", {}, v18) on_click v7
              let v21: Html = concat(v15, v20)
              v21
            }

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
                let bump = mut fn() { count = count + 1 };
                <>
                  <div on:click={mut fn() { count = forever(count) }}>
                    <button on:click={bump}>{count.to_string()}</button>
                  </div>
                  <p>no handler</p>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(3)],
        expect![[r#"
            -- flat --
            fn forever@f0(
              n@v25: Int,
            ) -> Int {
              let v26: Int = 1
              let v27: Int = int_add(v25, v26)
              let v28: Int = f0(v27)
              v28
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v11: mut () -> Tuple[] = fn() {
                let v8: Int = cell_read(v1)
                let v9: Int = f0(v8)
                let v10: Tuple[] = cell_write(v1, v9)
                v10
              }
              let v12: String = int_to_string(v2)
              let v13: Html = escape(v12)
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "button", {}, v14) on_click v7
              let v17: Html = concat(v16)
              let v18: Id = mint()
              let v19: Html = element(v18, "div", {}, v17) on_click v11
              let v20: Html = text("no handler")
              let v21: Html = concat(v20)
              let v22: Id = mint()
              let v23: Html = element(v22, "p", {}, v21)
              let v24: Html = concat(v19, v23)
              v24
            }

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
                let mut count = 0;
                <button on:click={mut fn() {
                  match count < 1 {
                    true => count = count + 1,
                    false => (),
                  }
                }}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v12: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Bool = int_lt(v3, v4)
                let v11: Tuple[] = match v5 {
                  true => {
                    let v6: Int = cell_read(v1)
                    let v7: Int = 1
                    let v8: Int = int_add(v6, v7)
                    let v9: Tuple[] = cell_write(v1, v8)
                    v9
                  }
                  false => {
                    let v10: Tuple[] = ()
                    v10
                  }
                }
                v11
              }
              let v13: String = int_to_string(v2)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v12
              v17
            }

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
                  <button on:click={mut fn() { count = count + 1 }}>
                    <b>{count.to_string()}</b>
                  </button>
                  <p>no handler</p>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(3)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v8: String = int_to_string(v2)
              let v9: Html = escape(v8)
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "b", {}, v10)
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "button", {}, v13) on_click v7
              let v16: Html = text("no handler")
              let v17: Html = concat(v16)
              let v18: Id = mint()
              let v19: Html = element(v18, "p", {}, v17)
              let v20: Html = concat(v15, v19)
              v20
            }

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
                  <button on:click={mut fn() { a = a + 1 }}>{a.to_string()}</button>
                  <button on:click={mut fn() { b = b + 1 }}>{b.to_string()}</button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 1
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: Cell[Int] = cell {
                let v3: Int = 10
                let v4: Int = int_mul(v2, v3)
                v4
              }
              let v6: Int = cell_read(v5)
              let v11: mut () -> Tuple[] = fn() {
                let v7: Int = cell_read(v1)
                let v8: Int = 1
                let v9: Int = int_add(v7, v8)
                let v10: Tuple[] = cell_write(v1, v9)
                v10
              }
              let v12: String = int_to_string(v2)
              let v13: Html = escape(v12)
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "button", {}, v14) on_click v11
              let v21: mut () -> Tuple[] = fn() {
                let v17: Int = cell_read(v5)
                let v18: Int = 1
                let v19: Int = int_add(v17, v18)
                let v20: Tuple[] = cell_write(v5, v19)
                v20
              }
              let v22: String = int_to_string(v6)
              let v23: Html = escape(v22)
              let v24: Html = concat(v23)
              let v25: Id = mint()
              let v26: Html = element(v25, "button", {}, v24) on_click v21
              let v27: Html = concat(v16, v26)
              v27
            }

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
              <button on:click={mut fn() { count = count + 1 }}>
                {start.to_string()}/{count.to_string()}
              </button>
            }
            page Test {
              fn body() -> Html {
                let mut base = 0;
                <>
                  <b on:click={mut fn() { base = base + 100 }}>bump</b>
                  <Counter start={base}/>
                </>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Counter@f0(
              start@v14: Int,
            ) -> Html {
              let v15: Cell[Int] = cell {
                v14
              }
              let v16: Int = cell_read(v15)
              let v21: mut () -> Tuple[] = fn() {
                let v17: Int = cell_read(v15)
                let v18: Int = 1
                let v19: Int = int_add(v17, v18)
                let v20: Tuple[] = cell_write(v15, v19)
                v20
              }
              let v22: String = int_to_string(v14)
              let v23: Html = escape(v22)
              let v24: Html = text("/")
              let v25: String = int_to_string(v16)
              let v26: Html = escape(v25)
              let v27: Html = concat(v23, v24, v26)
              let v28: Id = mint()
              let v29: Html = element(v28, "button", {}, v27) on_click v21
              v29
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 100
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v8: Html = text("bump")
              let v9: Html = concat(v8)
              let v10: Id = mint()
              let v11: Html = element(v10, "b", {}, v9) on_click v7
              let v12: Html = f0(v2)
              let v13: Html = concat(v11, v12)
              v13
            }

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
                  <button on:click={mut fn() { inner = inner + 1; outer = outer + 1 }}>
                    {inner.to_string()}/{outer.to_string()}
                  </button>
                }}/>
              }
            }
        "#},
        &[Event::Click(2), Event::Click(2), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Twice@f0(
              row@v25: render () -> Html,
            ) -> Html {
              let v26: Html = v25()
              let v27: Html = v25()
              let v28: Html = concat(v26, v27)
              v28
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v23: render () -> Html = fn() {
                let v4: Cell[Int] = cell {
                  let v3: Int = 0
                  v3
                }
                let v5: Int = cell_read(v4)
                let v14: mut () -> Tuple[] = fn() {
                  let v6: Int = cell_read(v4)
                  let v7: Int = 1
                  let v8: Int = int_add(v6, v7)
                  let v9: Tuple[] = cell_write(v4, v8)
                  let v10: Int = cell_read(v1)
                  let v11: Int = 1
                  let v12: Int = int_add(v10, v11)
                  let v13: Tuple[] = cell_write(v1, v12)
                  v13
                }
                let v15: String = int_to_string(v5)
                let v16: Html = escape(v15)
                let v17: Html = text("/")
                let v18: String = int_to_string(v2)
                let v19: Html = escape(v18)
                let v20: Html = concat(v16, v17, v19)
                let v21: Id = mint()
                let v22: Html = element(v21, "button", {}, v20) on_click v14
                v22
              }
              let v24: Html = f0(v23)
              v24
            }

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
                  <b on:click={mut fn() { left = !left }}>switch</b>
                  {match left {
                    true => {
                      let mut count = 0;
                      let bump = mut fn() { count = count + 1 };
                      <button on:click={bump}>left {count.to_string()}</button>
                    },
                    false => {
                      let mut count = 100;
                      let bump = mut fn() { count = count + 1 };
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v6: mut () -> Tuple[] = fn() {
                let v3: Bool = cell_read(v1)
                let v4: Bool = bool_not(v3)
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v7: Html = text("switch")
              let v8: Html = concat(v7)
              let v9: Id = mint()
              let v10: Html = element(v9, "b", {}, v8) on_click v6
              let v39: Html = match v2 {
                true => {
                  let v12: Cell[Int] = cell {
                    let v11: Int = 0
                    v11
                  }
                  let v13: Int = cell_read(v12)
                  let v18: mut () -> Tuple[] = fn() {
                    let v14: Int = cell_read(v12)
                    let v15: Int = 1
                    let v16: Int = int_add(v14, v15)
                    let v17: Tuple[] = cell_write(v12, v16)
                    v17
                  }
                  let v19: Html = text("left ")
                  let v20: String = int_to_string(v13)
                  let v21: Html = escape(v20)
                  let v22: Html = concat(v19, v21)
                  let v23: Id = mint()
                  let v24: Html = element(v23, "button", {}, v22) on_click v18
                  v24
                }
                false => {
                  let v26: Cell[Int] = cell {
                    let v25: Int = 100
                    v25
                  }
                  let v27: Int = cell_read(v26)
                  let v32: mut () -> Tuple[] = fn() {
                    let v28: Int = cell_read(v26)
                    let v29: Int = 1
                    let v30: Int = int_add(v28, v29)
                    let v31: Tuple[] = cell_write(v26, v30)
                    v31
                  }
                  let v33: Html = text("right ")
                  let v34: String = int_to_string(v27)
                  let v35: Html = escape(v34)
                  let v36: Html = concat(v33, v35)
                  let v37: Id = mint()
                  let v38: Html = element(v37, "button", {}, v36) on_click v32
                  v38
                }
              }
              let v40: Html = concat(v10, v39)
              v40
            }

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
                  <b on:click={mut fn() { ids = [2, 1] }}>flip</b>
                  <i on:click={mut fn() { ids = [2] }}>drop</i>
                  {for id in ids key id {
                    let mut count = 0;
                    let bump = mut fn() { count = count + 1 };
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[Int]] = cell {
                let v0: Int = 1
                let v1: Int = 2
                let v2: Array[Int] = [v0, v1]
                v2
              }
              let v4: Array[Int] = cell_read(v3)
              let v9: mut () -> Tuple[] = fn() {
                let v5: Int = 2
                let v6: Int = 1
                let v7: Array[Int] = [v5, v6]
                let v8: Tuple[] = cell_write(v3, v7)
                v8
              }
              let v10: Html = text("flip")
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "b", {}, v11) on_click v9
              let v17: mut () -> Tuple[] = fn() {
                let v14: Int = 2
                let v15: Array[Int] = [v14]
                let v16: Tuple[] = cell_write(v3, v15)
                v16
              }
              let v18: Html = text("drop")
              let v19: Html = concat(v18)
              let v20: Id = mint()
              let v21: Html = element(v20, "i", {}, v19) on_click v17
              let v39: Html = for v22: Int in v4 key {
                v22
              } {
                let v24: Cell[Int] = cell {
                  let v23: Int = 0
                  v23
                }
                let v25: Int = cell_read(v24)
                let v30: mut () -> Tuple[] = fn() {
                  let v26: Int = cell_read(v24)
                  let v27: Int = 1
                  let v28: Int = int_add(v26, v27)
                  let v29: Tuple[] = cell_write(v24, v28)
                  v29
                }
                let v31: String = int_to_string(v22)
                let v32: Html = escape(v31)
                let v33: Html = text(":")
                let v34: String = int_to_string(v25)
                let v35: Html = escape(v34)
                let v36: Html = concat(v32, v33, v35)
                let v37: Id = mint()
                let v38: Html = element(v37, "button", {}, v36) on_click v30
                v38
              }
              let v40: Html = concat(v13, v21, v39)
              v40
            }

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
                  <b on:click={mut fn() { ids = [1] }}>drop</b>
                  {for id in ids key id {
                    let mut count = 0;
                    let bump = mut fn() { count = count + 1 };
                    <button on:click={bump}>{id.to_string()}:{count.to_string()}</button>
                  }}
                </>
              }
            }
        "#},
        &[Event::Click(3), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[Int]] = cell {
                let v0: Int = 1
                let v1: Int = 1
                let v2: Array[Int] = [v0, v1]
                v2
              }
              let v4: Array[Int] = cell_read(v3)
              let v8: mut () -> Tuple[] = fn() {
                let v5: Int = 1
                let v6: Array[Int] = [v5]
                let v7: Tuple[] = cell_write(v3, v6)
                v7
              }
              let v9: Html = text("drop")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "b", {}, v10) on_click v8
              let v30: Html = for v13: Int in v4 key {
                v13
              } {
                let v15: Cell[Int] = cell {
                  let v14: Int = 0
                  v14
                }
                let v16: Int = cell_read(v15)
                let v21: mut () -> Tuple[] = fn() {
                  let v17: Int = cell_read(v15)
                  let v18: Int = 1
                  let v19: Int = int_add(v17, v18)
                  let v20: Tuple[] = cell_write(v15, v19)
                  v20
                }
                let v22: String = int_to_string(v13)
                let v23: Html = escape(v22)
                let v24: Html = text(":")
                let v25: String = int_to_string(v16)
                let v26: Html = escape(v25)
                let v27: Html = concat(v23, v24, v26)
                let v28: Id = mint()
                let v29: Html = element(v28, "button", {}, v27) on_click v21
                v29
              }
              let v31: Html = concat(v12, v30)
              v31
            }

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
                  <b on:click={mut fn() { rows = ["b", "a"] }}>flip</b>
                  {for row in rows key row {
                    {for col in [1, 2] key col {
                      let mut count = 0;
                      <button on:click={mut fn() { count = count + 1 }}>
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[String]] = cell {
                let v0: String = "a"
                let v1: String = "b"
                let v2: Array[String] = [v0, v1]
                v2
              }
              let v4: Array[String] = cell_read(v3)
              let v9: mut () -> Tuple[] = fn() {
                let v5: String = "b"
                let v6: String = "a"
                let v7: Array[String] = [v5, v6]
                let v8: Tuple[] = cell_write(v3, v7)
                v8
              }
              let v10: Html = text("flip")
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "b", {}, v11) on_click v9
              let v37: Html = for v14: String in v4 key {
                v14
              } {
                let v15: Int = 1
                let v16: Int = 2
                let v17: Array[Int] = [v15, v16]
                let v36: Html = for v18: Int in v17 key {
                  v18
                } {
                  let v20: Cell[Int] = cell {
                    let v19: Int = 0
                    v19
                  }
                  let v21: Int = cell_read(v20)
                  let v26: mut () -> Tuple[] = fn() {
                    let v22: Int = cell_read(v20)
                    let v23: Int = 1
                    let v24: Int = int_add(v22, v23)
                    let v25: Tuple[] = cell_write(v20, v24)
                    v25
                  }
                  let v27: Html = escape(v14)
                  let v28: String = int_to_string(v18)
                  let v29: Html = escape(v28)
                  let v30: Html = text(":")
                  let v31: String = int_to_string(v21)
                  let v32: Html = escape(v31)
                  let v33: Html = concat(v27, v29, v30, v32)
                  let v34: Id = mint()
                  let v35: Html = element(v34, "button", {}, v33) on_click v26
                  v35
                }
                v36
              }
              let v38: Html = concat(v13, v37)
              v38
            }

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
                  on:click={mut fn() { on = !on }}
                >toggle</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: String = match v2 {
                true => {
                  let v3: String = "on"
                  v3
                }
                false => {
                  let v4: String = "off"
                  v4
                }
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Bool = cell_read(v1)
                let v7: Bool = bool_not(v6)
                let v8: Tuple[] = cell_write(v1, v7)
                v8
              }
              let v10: Html = text("toggle")
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "button", {class: v5}, v11) on_click v9
              v13
            }

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
                <button on:click={mut fn() { count = count + 1 }}>
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
                  <b on:click={mut fn() { levels = levels + 1 }}>deeper</b>
                  <Level depth={0} levels={levels}/>
                </>
              }
            }
        "#},
        &[Event::Click(5), Event::Click(3), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Level@f0(
              depth@v15: Int,
              levels@v16: Int,
            ) -> Html {
              let v18: Cell[Int] = cell {
                let v17: Int = 0
                v17
              }
              let v19: Int = cell_read(v18)
              let v24: mut () -> Tuple[] = fn() {
                let v20: Int = cell_read(v18)
                let v21: Int = 1
                let v22: Int = int_add(v20, v21)
                let v23: Tuple[] = cell_write(v18, v22)
                v23
              }
              let v25: String = int_to_string(v15)
              let v26: Html = escape(v25)
              let v27: Html = text(":")
              let v28: String = int_to_string(v19)
              let v29: Html = escape(v28)
              let v30: Html = concat(v26, v27, v29)
              let v31: Id = mint()
              let v32: Html = element(v31, "button", {}, v30) on_click v24
              let v33: Int = 1
              let v34: Int = int_add(v15, v33)
              let v35: Bool = int_lt(v34, v16)
              let v40: Html = match v35 {
                true => {
                  let v36: Int = 1
                  let v37: Int = int_add(v15, v36)
                  let v38: Html = f0(v37, v16)
                  v38
                }
                false => {
                  let v39: Html = concat()
                  v39
                }
              }
              let v41: Html = concat(v32, v40)
              let v42: Id = mint()
              let v43: Html = element(v42, "div", {}, v41)
              v43
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 2
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v8: Html = text("deeper")
              let v9: Html = concat(v8)
              let v10: Id = mint()
              let v11: Html = element(v10, "b", {}, v9) on_click v7
              let v12: Int = 0
              let v13: Html = f0(v12, v2)
              let v14: Html = concat(v11, v13)
              v14
            }

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
                let mut a = 0;
                let mut b = 0;
                let bump_a = mut fn() { a = a + 1 };
                let bump_b = mut fn() { b = b + 10 };
                <button on:click={mut fn() { both(bump_a, bump_b) }}>
                  {a.to_string()}/{b.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            mut fn both@f0(
              first@v26: mut () -> Tuple[],
              second@v27: mut () -> Tuple[],
            ) -> Tuple[] {
              let v28: Tuple[] = v26()
              let v29: Tuple[] = v27()
              v29
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v10: mut () -> Tuple[] = fn() {
                let v6: Int = cell_read(v1)
                let v7: Int = 1
                let v8: Int = int_add(v6, v7)
                let v9: Tuple[] = cell_write(v1, v8)
                v9
              }
              let v15: mut () -> Tuple[] = fn() {
                let v11: Int = cell_read(v4)
                let v12: Int = 10
                let v13: Int = int_add(v11, v12)
                let v14: Tuple[] = cell_write(v4, v13)
                v14
              }
              let v17: mut () -> Tuple[] = fn() {
                let v16: Tuple[] = f0(v10, v15)
                v16
              }
              let v18: String = int_to_string(v2)
              let v19: Html = escape(v18)
              let v20: Html = text("/")
              let v21: String = int_to_string(v5)
              let v22: Html = escape(v21)
              let v23: Html = concat(v19, v20, v22)
              let v24: Id = mint()
              let v25: Html = element(v24, "button", {}, v23) on_click v17
              v25
            }

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
                let mut count = 0;
                let bump = mut fn() { count = count + 1 };
                <>
                  <Button press={bump}>{count.to_string()}</Button>
                  <Button press={mut fn() { count = 0 }}>reset</Button>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1), Event::Click(2)],
        expect![[r#"
            -- flat --
            render fn Button@f0(
              press@v19: mut () -> Tuple[],
              children@v20: Html,
            ) -> Html {
              let v21: Html = concat(v20)
              let v22: Id = mint()
              let v23: Html = element(v22, "button", {}, v21) on_click v19
              v23
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                v6
              }
              let v8: String = int_to_string(v2)
              let v9: Html = escape(v8)
              let v10: Html = concat(v9)
              let v11: Html = f0(v7, v10)
              let v14: mut () -> Tuple[] = fn() {
                let v12: Int = 0
                let v13: Tuple[] = cell_write(v1, v12)
                v13
              }
              let v15: Html = text("reset")
              let v16: Html = concat(v15)
              let v17: Html = f0(v14, v16)
              let v18: Html = concat(v11, v17)
              v18
            }

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
                let dark = mut fn() { setting = Setting {...setting, mode: Mode::Dark} };
                let raise = mut fn() {
                  setting = Setting {...setting, level: setting.level + 1}
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Setting] = cell {
                let v0: Mode = Light
                let v1: Int = 1
                let v2: Setting = {mode: v0, level: v1}
                v2
              }
              let v4: Setting = cell_read(v3)
              let v10: mut () -> Tuple[] = fn() {
                let v5: Setting = cell_read(v3)
                let v6: Mode = Dark
                let v7: Int = v5.level
                let v8: Setting = {mode: v6, level: v7}
                let v9: Tuple[] = cell_write(v3, v8)
                v9
              }
              let v19: mut () -> Tuple[] = fn() {
                let v11: Setting = cell_read(v3)
                let v12: Mode = v11.mode
                let v13: Setting = cell_read(v3)
                let v14: Int = v13.level
                let v15: Int = 1
                let v16: Int = int_add(v14, v15)
                let v17: Setting = {mode: v12, level: v16}
                let v18: Tuple[] = cell_write(v3, v17)
                v18
              }
              let v20: Mode = v4.mode
              let v23: String = match v20 {
                Mode::Light => {
                  let v21: String = "light"
                  v21
                }
                Mode::Dark => {
                  let v22: String = "dark"
                  v22
                }
              }
              let v24: Html = text("dark")
              let v25: Html = concat(v24)
              let v26: Id = mint()
              let v27: Html = element(v26, "button", {}, v25) on_click v10
              let v28: Html = text("raise")
              let v29: Html = concat(v28)
              let v30: Id = mint()
              let v31: Html = element(v30, "button", {}, v29) on_click v19
              let v32: Html = escape(v23)
              let v33: Html = text(" ")
              let v34: Int = v4.level
              let v35: String = int_to_string(v34)
              let v36: Html = escape(v35)
              let v37: Html = concat(v32, v33, v36)
              let v38: Id = mint()
              let v39: Html = element(v38, "p", {}, v37)
              let v40: Html = concat(v27, v31, v39)
              v40
            }

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
                let mut count = 0;
                <button on:click={mut fn() {
                  count = count + 1;
                  count = forever(count)
                }}>{count.to_string()}</button>
              }
            }
        "#},
        &[Event::Click(1)],
        expect![[r#"
            -- flat --
            fn forever@f0(
              n@v16: Int,
            ) -> Int {
              let v17: Int = 1
              let v18: Int = int_add(v16, v17)
              let v19: Int = f0(v18)
              v19
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v10: mut () -> Tuple[] = fn() {
                let v3: Int = cell_read(v1)
                let v4: Int = 1
                let v5: Int = int_add(v3, v4)
                let v6: Tuple[] = cell_write(v1, v5)
                let v7: Int = cell_read(v1)
                let v8: Int = f0(v7)
                let v9: Tuple[] = cell_write(v1, v8)
                v9
              }
              let v11: String = int_to_string(v2)
              let v12: Html = escape(v11)
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "button", {}, v13) on_click v10
              v15
            }

            -- render --
            <button@e1>0</button>
            -- click button@e1 --
            error: Program exceeded the call depth limit of 12
        "#]],
    );
}

#[test]
fn a_write_reads_a_cell_that_an_earlier_write_of_the_click_wrote() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                let mut label = "none";
                <button on:click={mut fn() {
                  count = count + 1;
                  label = count.to_string()
                }}>{label}</button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v4: Cell[String] = cell {
                let v3: String = "none"
                v3
              }
              let v5: String = cell_read(v4)
              let v13: mut () -> Tuple[] = fn() {
                let v6: Int = cell_read(v1)
                let v7: Int = 1
                let v8: Int = int_add(v6, v7)
                let v9: Tuple[] = cell_write(v1, v8)
                let v10: Int = cell_read(v1)
                let v11: String = int_to_string(v10)
                let v12: Tuple[] = cell_write(v4, v11)
                v12
              }
              let v14: Html = escape(v5)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v13
              v17
            }

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
fn a_handler_reads_the_value_the_render_gave_through_a_let() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                let rendered = count;
                let bump = mut fn() {
                  count = rendered + 1;
                  count = rendered + 10
                };
                <button on:click={bump}>
                  {count.to_string()}
                </button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v9: mut () -> Tuple[] = fn() {
                let v3: Int = 1
                let v4: Int = int_add(v2, v3)
                let v5: Tuple[] = cell_write(v1, v4)
                let v6: Int = 10
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = cell_write(v1, v7)
                v8
              }
              let v10: String = int_to_string(v2)
              let v11: Html = escape(v10)
              let v12: Html = concat(v11)
              let v13: Id = mint()
              let v14: Html = element(v13, "button", {}, v12) on_click v9
              v14
            }

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
fn a_render_fn_keeps_its_cells_across_renders_and_each_call_has_its_own() {
    check(
        indoc! {r#"
            render fn first(value: Int) -> Int {
              let mut seen = value;
              seen
            }
            page Test {
              fn body() -> Html {
                let mut count = 0;
                let late = match count > 1 {
                  true => first(count),
                  false => 0,
                };
                <>
                  <button on:click={mut fn() { count = count + 1 }}>{count.to_string()}</button>
                  <i>{first(count).to_string()}</i>
                  <b>{late.to_string()}</b>
                  <s on:click={mut fn() { count = 0 }}>reset</s>
                </>
              }
            }
        "#},
        &[
            Event::Click(1),
            Event::Click(1),
            Event::Click(1),
            Event::Click(4),
            Event::Click(1),
            Event::Click(1),
            Event::Click(1),
        ],
        expect![[r#"
            -- flat --
            render fn first@f0(
              value@v37: Int,
            ) -> Int {
              let v38: Cell[Int] = cell {
                v37
              }
              let v39: Int = cell_read(v38)
              v39
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v3: Int = 1
              let v4: Bool = int_lt(v3, v2)
              let v7: Int = match v4 {
                true => {
                  let v5: Int = f0(v2)
                  v5
                }
                false => {
                  let v6: Int = 0
                  v6
                }
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: Int = cell_read(v1)
                let v9: Int = 1
                let v10: Int = int_add(v8, v9)
                let v11: Tuple[] = cell_write(v1, v10)
                v11
              }
              let v13: String = int_to_string(v2)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v12
              let v18: Int = f0(v2)
              let v19: String = int_to_string(v18)
              let v20: Html = escape(v19)
              let v21: Html = concat(v20)
              let v22: Id = mint()
              let v23: Html = element(v22, "i", {}, v21)
              let v24: String = int_to_string(v7)
              let v25: Html = escape(v24)
              let v26: Html = concat(v25)
              let v27: Id = mint()
              let v28: Html = element(v27, "b", {}, v26)
              let v31: mut () -> Tuple[] = fn() {
                let v29: Int = 0
                let v30: Tuple[] = cell_write(v1, v29)
                v30
              }
              let v32: Html = text("reset")
              let v33: Html = concat(v32)
              let v34: Id = mint()
              let v35: Html = element(v34, "s", {}, v33) on_click v31
              let v36: Html = concat(v17, v23, v28, v35)
              v36
            }

            -- render --
            <button@e1>0</button>
            <i@e2>0</i>
            <b@e3>0</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>1</button>
            <i@e2>0</i>
            <b@e3>0</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>2</button>
            <i@e2>0</i>
            <b@e3>2</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>3</button>
            <i@e2>0</i>
            <b@e3>2</b>
            <s@e4>reset</s>
            -- click s@e4 --
            <button@e1>0</button>
            <i@e2>0</i>
            <b@e3>0</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>1</button>
            <i@e2>0</i>
            <b@e3>0</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>2</button>
            <i@e2>0</i>
            <b@e3>2</b>
            <s@e4>reset</s>
            -- click button@e1 --
            <button@e1>3</button>
            <i@e2>0</i>
            <b@e3>2</b>
            <s@e4>reset</s>
        "#]],
    );
}

#[test]
fn a_render_lambda_that_returns_a_value_keeps_its_cells_per_application() {
    check(
        indoc! {r#"
            page Test {
              fn body() -> Html {
                let mut count = 0;
                let first = render fn(offset: Int) -> Int {
                  let mut seen = count + offset;
                  seen
                };
                <>
                  <button on:click={mut fn() { count = count + 1 }}>{count.to_string()}</button>
                  <i>{first(0).to_string()}</i>
                  <b>{first(100).to_string()}</b>
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v7: render (Int) -> Int = fn(
                v3: Int,
              ) {
                let v5: Cell[Int] = cell {
                  let v4: Int = int_add(v2, v3)
                  v4
                }
                let v6: Int = cell_read(v5)
                v6
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: Int = cell_read(v1)
                let v9: Int = 1
                let v10: Int = int_add(v8, v9)
                let v11: Tuple[] = cell_write(v1, v10)
                v11
              }
              let v13: String = int_to_string(v2)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v12
              let v18: Int = 0
              let v19: Int = v7(v18)
              let v20: String = int_to_string(v19)
              let v21: Html = escape(v20)
              let v22: Html = concat(v21)
              let v23: Id = mint()
              let v24: Html = element(v23, "i", {}, v22)
              let v25: Int = 100
              let v26: Int = v7(v25)
              let v27: String = int_to_string(v26)
              let v28: Html = escape(v27)
              let v29: Html = concat(v28)
              let v30: Id = mint()
              let v31: Html = element(v30, "b", {}, v29)
              let v32: Html = concat(v17, v24, v31)
              v32
            }

            -- render --
            <button@e1>0</button>
            <i@e2>0</i>
            <b@e3>100</b>
            -- click button@e1 --
            <button@e1>1</button>
            <i@e2>0</i>
            <b@e3>100</b>
            -- click button@e1 --
            <button@e1>2</button>
            <i@e2>0</i>
            <b@e3>100</b>
        "#]],
    );
}

#[test]
fn render_and_mut_lambdas_read_cells_and_plain_lambdas_take_their_values_as_arguments() {
    check(
        indoc! {r#"
            render fn first(start: Int) -> Int {
              let mut seen = start;
              let get = render fn() -> Int { seen };
              get()
            }
            page Test {
              fn body() -> Html {
                let mut count = 1;
                let mut step = count * 10;
                let scaled = fn(n: Int, by: Int) { n * by };
                let row = fn(n: Int) { <i>{scaled(n, count).to_string()}</i> };
                let bump = mut fn() { count = count + step };
                <>
                  <button on:click={bump}>{first(count).to_string()}/{count.to_string()}</button>
                  {for n in [1, 2] key n + count { row(n) }}
                </>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn first@f0(
              start@v41: Int,
            ) -> Int {
              let v42: Cell[Int] = cell {
                v41
              }
              let v43: Int = cell_read(v42)
              let v44: render () -> Int = fn() {
                v43
              }
              let v45: Int = v44()
              v45
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 1
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: Cell[Int] = cell {
                let v3: Int = 10
                let v4: Int = int_mul(v2, v3)
                v4
              }
              let v6: Int = cell_read(v5)
              let v10: (Int, Int) -> Int = fn(
                v7: Int,
                v8: Int,
              ) {
                let v9: Int = int_mul(v7, v8)
                v9
              }
              let v18: render (Int) -> Html = fn(
                v11: Int,
              ) {
                let v12: Int = v10(v11, v2)
                let v13: String = int_to_string(v12)
                let v14: Html = escape(v13)
                let v15: Html = concat(v14)
                let v16: Id = mint()
                let v17: Html = element(v16, "i", {}, v15)
                v17
              }
              let v23: mut () -> Tuple[] = fn() {
                let v19: Int = cell_read(v1)
                let v20: Int = cell_read(v5)
                let v21: Int = int_add(v19, v20)
                let v22: Tuple[] = cell_write(v1, v21)
                v22
              }
              let v24: Int = f0(v2)
              let v25: String = int_to_string(v24)
              let v26: Html = escape(v25)
              let v27: Html = text("/")
              let v28: String = int_to_string(v2)
              let v29: Html = escape(v28)
              let v30: Html = concat(v26, v27, v29)
              let v31: Id = mint()
              let v32: Html = element(v31, "button", {}, v30) on_click v23
              let v33: Int = 1
              let v34: Int = 2
              let v35: Array[Int] = [v33, v34]
              let v39: Html = for v36: Int in v35 key {
                let v37: Int = int_add(v36, v2)
                v37
              } {
                let v38: Html = v18(v36)
                v38
              }
              let v40: Html = concat(v32, v39)
              v40
            }

            -- render --
            <button@e1>1/1</button>
            <i@e2>1</i>
            <i@e3>2</i>
            -- click button@e1 --
            <button@e1>1/11</button>
            <i@e4>11</i>
            <i@e5>22</i>
            -- click button@e1 --
            <button@e1>1/21</button>
            <i@e6>21</i>
            <i@e7>42</i>
        "#]],
    );
}

#[test]
fn a_read_lambda_reads_the_value_a_cell_has_now_in_a_render_and_in_a_handler() {
    check(
        indoc! {r#"
            render fn WithState(init: Int, k: fn(read fn() -> Int, mut fn(Int) -> Tuple[]) -> Html) -> Html {
              let mut c = init;
              k(read fn() -> Int { c }, mut fn(v: Int) { c = v })
            }
            page Test {
              fn body() -> Html {
                <WithState init={1} k={fn(get: read fn() -> Int, set: mut fn(Int) -> Tuple[]) {
                  <button on:click={mut fn() {
                    set(get() + 1);
                    set(get() * 2)
                  }}>{get().to_string()}</button>
                }}/>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn WithState@f0(
              init@v20: Int,
              k@v21: render (read () -> Int, mut (Int) -> Tuple[]) -> Html,
            ) -> Html {
              let v22: Cell[Int] = cell {
                v20
              }
              let v23: Int = cell_read(v22)
              let v25: read () -> Int = fn() {
                let v24: Int = cell_read(v22)
                v24
              }
              let v28: mut (Int) -> Tuple[] = fn(
                v26: Int,
              ) {
                let v27: Tuple[] = cell_write(v22, v26)
                v27
              }
              let v29: Html = v21(v25, v28)
              v29
            }
            entrypoint body@e0() -> Html {
              let v0: Int = 1
              let v18: render (read () -> Int, mut (Int) -> Tuple[]) -> Html = fn(
                v1: read () -> Int,
                v2: mut (Int) -> Tuple[],
              ) {
                let v11: mut () -> Tuple[] = fn() {
                  let v3: Int = v1()
                  let v4: Int = 1
                  let v5: Int = int_add(v3, v4)
                  let v6: Tuple[] = v2(v5)
                  let v7: Int = v1()
                  let v8: Int = 2
                  let v9: Int = int_mul(v7, v8)
                  let v10: Tuple[] = v2(v9)
                  v10
                }
                let v12: Int = v1()
                let v13: String = int_to_string(v12)
                let v14: Html = escape(v13)
                let v15: Html = concat(v14)
                let v16: Id = mint()
                let v17: Html = element(v16, "button", {}, v15) on_click v11
                v17
              }
              let v19: Html = f0(v0, v18)
              v19
            }

            -- render --
            <button@e1>1</button>
            -- click button@e1 --
            <button@e1>4</button>
            -- click button@e1 --
            <button@e1>10</button>
        "#]],
    );
}
