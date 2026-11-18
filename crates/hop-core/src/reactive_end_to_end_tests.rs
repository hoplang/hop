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
            -- flat --
            render fn Counter@f0() -> Html {
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v8: mut (Int) -> Tuple[] = fn(
                v6: Int,
              ) {
                let v7: Tuple[] = cell_write(v4, v6)
                v7
              }
              let v12: mut () -> Tuple[] = fn() {
                let v9: Int = 1
                let v10: Int = int_add(v5, v9)
                let v11: Tuple[] = v8(v10)
                v11
              }
              let v13: String = int_to_string(v5)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v12
              v17
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
            -- flat --
            render fn Counter@f0(
              label@v25: String,
            ) -> Html {
              let v27: Cell[Int] = cell {
                let v26: Int = 0
                v26
              }
              let v28: Int = cell_read(v27)
              let v31: mut (Int) -> Tuple[] = fn(
                v29: Int,
              ) {
                let v30: Tuple[] = cell_write(v27, v29)
                v30
              }
              let v35: mut () -> Tuple[] = fn() {
                let v32: Int = 1
                let v33: Int = int_add(v28, v32)
                let v34: Tuple[] = v31(v33)
                v34
              }
              let v36: Html = escape(v25)
              let v37: Html = text(":")
              let v38: String = int_to_string(v28)
              let v39: Html = escape(v38)
              let v40: Html = concat(v36, v37, v39)
              let v41: Id = mint()
              let v42: Html = element(v41, "button", {}, v40) on_click v35
              v42
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v12: Array[Int] = match v2 {
                true => {
                  let v6: Int = 2
                  let v7: Int = 1
                  let v8: Array[Int] = [v6, v7]
                  v8
                }
                false => {
                  let v9: Int = 1
                  let v10: Int = 2
                  let v11: Array[Int] = [v9, v10]
                  v11
                }
              }
              let v15: mut () -> Tuple[] = fn() {
                let v13: Bool = bool_not(v2)
                let v14: Tuple[] = v5(v13)
                v14
              }
              let v16: Html = text("flip")
              let v17: Html = concat(v16)
              let v18: Id = mint()
              let v19: Html = element(v18, "b", {}, v17) on_click v15
              let v23: Html = for v20: Int in v12 key {
                v20
              } {
                let v21: String = int_to_string(v20)
                let v22: Html = f0(v21)
                v22
              }
              let v24: Html = concat(v19, v23)
              v24
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
            -- flat --
            render fn Counter@f0(
              label@v25: String,
            ) -> Html {
              let v27: Cell[Int] = cell {
                let v26: Int = 0
                v26
              }
              let v28: Int = cell_read(v27)
              let v31: mut (Int) -> Tuple[] = fn(
                v29: Int,
              ) {
                let v30: Tuple[] = cell_write(v27, v29)
                v30
              }
              let v35: mut () -> Tuple[] = fn() {
                let v32: Int = 1
                let v33: Int = int_add(v28, v32)
                let v34: Tuple[] = v31(v33)
                v34
              }
              let v36: Html = escape(v25)
              let v37: Html = text(":")
              let v38: String = int_to_string(v28)
              let v39: Html = escape(v38)
              let v40: Html = concat(v36, v37, v39)
              let v41: Id = mint()
              let v42: Html = element(v41, "button", {}, v40) on_click v35
              v42
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v12: Array[Int] = match v2 {
                true => {
                  let v6: Int = 2
                  let v7: Int = 1
                  let v8: Array[Int] = [v6, v7]
                  v8
                }
                false => {
                  let v9: Int = 1
                  let v10: Int = 2
                  let v11: Array[Int] = [v9, v10]
                  v11
                }
              }
              let v15: mut () -> Tuple[] = fn() {
                let v13: Bool = bool_not(v2)
                let v14: Tuple[] = v5(v13)
                v14
              }
              let v16: Html = text("flip")
              let v17: Html = concat(v16)
              let v18: Id = mint()
              let v19: Html = element(v18, "b", {}, v17) on_click v15
              let v23: Html = for v20: Int in v12 {
                let v21: String = int_to_string(v20)
                let v22: Html = f0(v21)
                v22
              }
              let v24: Html = concat(v19, v23)
              v24
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
            -- flat --
            render fn Counter@f0() -> Html {
              let v21: Cell[Int] = cell {
                let v20: Int = 0
                v20
              }
              let v22: Int = cell_read(v21)
              let v25: mut (Int) -> Tuple[] = fn(
                v23: Int,
              ) {
                let v24: Tuple[] = cell_write(v21, v23)
                v24
              }
              let v29: mut () -> Tuple[] = fn() {
                let v26: Int = 1
                let v27: Int = int_add(v22, v26)
                let v28: Tuple[] = v25(v27)
                v28
              }
              let v30: String = int_to_string(v22)
              let v31: Html = escape(v30)
              let v32: Html = concat(v31)
              let v33: Id = mint()
              let v34: Html = element(v33, "button", {}, v32) on_click v29
              v34
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: mut () -> Tuple[] = fn() {
                let v6: Bool = bool_not(v2)
                let v7: Tuple[] = v5(v6)
                v7
              }
              let v9: Html = text("toggle")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "b", {}, v10) on_click v8
              let v18: Html = match v2 {
                true => {
                  let v13: Html = f0()
                  v13
                }
                false => {
                  let v14: Html = text("closed")
                  let v15: Html = concat(v14)
                  let v16: Id = mint()
                  let v17: Html = element(v16, "i", {}, v15)
                  v17
                }
              }
              let v19: Html = concat(v12, v18)
              v19
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v6: Id = mint()
              let v7: Html = element(v6, "input", {})
              let v10: mut () -> Tuple[] = fn() {
                let v8: Bool = bool_not(v2)
                let v9: Tuple[] = v5(v8)
                v9
              }
              let v11: Html = text("toggle")
              let v12: Html = concat(v11)
              let v13: Id = mint()
              let v14: Html = element(v13, "button", {}, v12) on_click v10
              let v21: Html = match v2 {
                true => {
                  let v15: Html = concat(v7)
                  let v16: Id = mint()
                  let v17: Html = element(v16, "div", {}, v15)
                  v17
                }
                false => {
                  let v18: Html = concat(v7)
                  let v19: Id = mint()
                  let v20: Html = element(v19, "p", {}, v18)
                  v20
                }
              }
              let v22: Html = concat(v14, v21)
              v22
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[String] = cell {
                let v0: String = "none"
                v0
              }
              let v2: String = cell_read(v1)
              let v5: mut (String) -> Tuple[] = fn(
                v3: String,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v7: Cell[Int] = cell {
                let v6: Int = 0
                v6
              }
              let v8: Int = cell_read(v7)
              let v11: mut (Int) -> Tuple[] = fn(
                v9: Int,
              ) {
                let v10: Tuple[] = cell_write(v7, v9)
                v10
              }
              let v14: mut () -> Tuple[] = fn() {
                let v12: String = "div"
                let v13: Tuple[] = v5(v12)
                v13
              }
              let v20: mut () -> Tuple[] = fn() {
                let v15: String = "button"
                let v16: Tuple[] = v5(v15)
                let v17: Int = 1
                let v18: Int = int_add(v8, v17)
                let v19: Tuple[] = v11(v18)
                v19
              }
              let v21: Html = escape(v2)
              let v22: Html = text(" ")
              let v23: String = int_to_string(v8)
              let v24: Html = escape(v23)
              let v25: Html = concat(v21, v22, v24)
              let v26: Id = mint()
              let v27: Html = element(v26, "button", {}, v25) on_click v20
              let v28: Html = concat(v27)
              let v29: Id = mint()
              let v30: Html = element(v29, "div", {}, v28) on_click v14
              v30
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
                let (on, set_on) = state!(false);
                <Button on_press={mut fn() { set_on(!on) }}>
                  {match on {true => "on", false => "off"}}
                </Button>
              }
            }
        "#},
        &[Event::Click(1), Event::Click(1)],
        expect![[r#"
            -- flat --
            render fn Button@f0(
              on_press@v15: mut () -> Tuple[],
              children@v16: Html,
            ) -> Html {
              let v17: Html = concat(v16)
              let v18: Id = mint()
              let v19: Html = element(v18, "button", {}, v17) on_click v15
              v19
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: mut () -> Tuple[] = fn() {
                let v6: Bool = bool_not(v2)
                let v7: Tuple[] = v5(v6)
                v7
              }
              let v11: String = match v2 {
                true => {
                  let v9: String = "on"
                  v9
                }
                false => {
                  let v10: String = "off"
                  v10
                }
              }
              let v12: Html = escape(v11)
              let v13: Html = concat(v12)
              let v14: Html = f0(v8, v13)
              v14
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v6: Id = mint()
              let v7: Html = element(v6, "input", {})
              let v10: mut () -> Tuple[] = fn() {
                let v8: Bool = bool_not(v2)
                let v9: Tuple[] = v5(v8)
                v9
              }
              let v11: Html = text("toggle")
              let v12: Html = concat(v11)
              let v13: Id = mint()
              let v14: Html = element(v13, "button", {}, v12) on_click v10
              let v22: Html = match v2 {
                true => {
                  let v15: Html = concat(v7)
                  let v16: Id = mint()
                  let v17: Html = element(v16, "div", {}, v15)
                  v17
                }
                false => {
                  let v18: Html = text("hidden")
                  let v19: Html = concat(v18)
                  let v20: Id = mint()
                  let v21: Html = element(v20, "p", {}, v19)
                  v21
                }
              }
              let v23: Html = concat(v14, v22)
              v23
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v7: Cell[Int] = cell {
                let v6: Int = 0
                v6
              }
              let v8: Int = cell_read(v7)
              let v11: mut (Int) -> Tuple[] = fn(
                v9: Int,
              ) {
                let v10: Tuple[] = cell_write(v7, v9)
                v10
              }
              let v15: mut () -> Tuple[] = fn() {
                let v12: Int = 1
                let v13: Int = int_add(v8, v12)
                let v14: Tuple[] = v11(v13)
                v14
              }
              let v18: mut () -> Tuple[] = fn() {
                let v16: Bool = bool_not(v2)
                let v17: Tuple[] = v5(v16)
                v17
              }
              let v19: Html = text("toggle")
              let v20: Html = concat(v19)
              let v21: Id = mint()
              let v22: Html = element(v21, "b", {}, v20) on_click v18
              let v32: Html = match v2 {
                true => {
                  let v23: String = int_to_string(v8)
                  let v24: Html = escape(v23)
                  let v25: Html = concat(v24)
                  let v26: Id = mint()
                  let v27: Html = element(v26, "button", {}, v25) on_click v15
                  v27
                }
                false => {
                  let v28: Html = text("hidden")
                  let v29: Html = concat(v28)
                  let v30: Id = mint()
                  let v31: Html = element(v30, "p", {}, v29)
                  v31
                }
              }
              let v33: Html = concat(v22, v32)
              v33
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
            -- flat --
            fn forever@f0(
              n@v23: Int,
            ) -> Int {
              let v24: Int = 1
              let v25: Int = int_add(v23, v24)
              let v26: Int = f0(v25)
              v26
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v12: mut () -> Tuple[] = fn() {
                let v10: Int = f0(v2)
                let v11: Tuple[] = v5(v10)
                v11
              }
              let v13: Html = text("forever")
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "button", {}, v14) on_click v12
              let v17: String = int_to_string(v2)
              let v18: Html = escape(v17)
              let v19: Html = concat(v18)
              let v20: Id = mint()
              let v21: Html = element(v20, "button", {}, v19) on_click v9
              let v22: Html = concat(v16, v21)
              v22
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
            -- flat --
            fn forever@f0(
              n@v26: Int,
            ) -> Int {
              let v27: Int = 1
              let v28: Int = int_add(v26, v27)
              let v29: Int = f0(v28)
              v29
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v12: mut () -> Tuple[] = fn() {
                let v10: Int = f0(v2)
                let v11: Tuple[] = v5(v10)
                v11
              }
              let v13: String = int_to_string(v2)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v9
              let v18: Html = concat(v17)
              let v19: Id = mint()
              let v20: Html = element(v19, "div", {}, v18) on_click v12
              let v21: Html = text("no handler")
              let v22: Html = concat(v21)
              let v23: Id = mint()
              let v24: Html = element(v23, "p", {}, v22)
              let v25: Html = concat(v20, v24)
              v25
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v13: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Bool = int_lt(v2, v6)
                let v12: Tuple[] = match v7 {
                  true => {
                    let v8: Int = 1
                    let v9: Int = int_add(v2, v8)
                    let v10: Tuple[] = v5(v9)
                    v10
                  }
                  false => {
                    let v11: Tuple[] = ()
                    v11
                  }
                }
                v12
              }
              let v14: String = int_to_string(v2)
              let v15: Html = escape(v14)
              let v16: Html = concat(v15)
              let v17: Id = mint()
              let v18: Html = element(v17, "button", {}, v16) on_click v13
              v18
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v10: String = int_to_string(v2)
              let v11: Html = escape(v10)
              let v12: Html = concat(v11)
              let v13: Id = mint()
              let v14: Html = element(v13, "b", {}, v12)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v9
              let v18: Html = text("no handler")
              let v19: Html = concat(v18)
              let v20: Id = mint()
              let v21: Html = element(v20, "p", {}, v19)
              let v22: Html = concat(v17, v21)
              v22
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 1
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: Cell[Int] = cell {
                let v6: Int = 10
                let v7: Int = int_mul(v2, v6)
                v7
              }
              let v9: Int = cell_read(v8)
              let v12: mut (Int) -> Tuple[] = fn(
                v10: Int,
              ) {
                let v11: Tuple[] = cell_write(v8, v10)
                v11
              }
              let v16: mut () -> Tuple[] = fn() {
                let v13: Int = 1
                let v14: Int = int_add(v2, v13)
                let v15: Tuple[] = v5(v14)
                v15
              }
              let v17: String = int_to_string(v2)
              let v18: Html = escape(v17)
              let v19: Html = concat(v18)
              let v20: Id = mint()
              let v21: Html = element(v20, "button", {}, v19) on_click v16
              let v25: mut () -> Tuple[] = fn() {
                let v22: Int = 1
                let v23: Int = int_add(v9, v22)
                let v24: Tuple[] = v12(v23)
                v24
              }
              let v26: String = int_to_string(v9)
              let v27: Html = escape(v26)
              let v28: Html = concat(v27)
              let v29: Id = mint()
              let v30: Html = element(v29, "button", {}, v28) on_click v25
              let v31: Html = concat(v21, v30)
              v31
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
            -- flat --
            render fn Counter@f0(
              start@v16: Int,
            ) -> Html {
              let v17: Cell[Int] = cell {
                v16
              }
              let v18: Int = cell_read(v17)
              let v21: mut (Int) -> Tuple[] = fn(
                v19: Int,
              ) {
                let v20: Tuple[] = cell_write(v17, v19)
                v20
              }
              let v25: mut () -> Tuple[] = fn() {
                let v22: Int = 1
                let v23: Int = int_add(v18, v22)
                let v24: Tuple[] = v21(v23)
                v24
              }
              let v26: String = int_to_string(v16)
              let v27: Html = escape(v26)
              let v28: Html = text("/")
              let v29: String = int_to_string(v18)
              let v30: Html = escape(v29)
              let v31: Html = concat(v27, v28, v30)
              let v32: Id = mint()
              let v33: Html = element(v32, "button", {}, v31) on_click v25
              v33
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 100
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v10: Html = text("bump")
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "b", {}, v11) on_click v9
              let v14: Html = f0(v2)
              let v15: Html = concat(v13, v14)
              v15
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
            -- flat --
            render fn Twice@f0(
              row@v29: render () -> Html,
            ) -> Html {
              let v30: Html = v29()
              let v31: Html = v29()
              let v32: Html = concat(v30, v31)
              v32
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v27: render () -> Html = fn() {
                let v7: Cell[Int] = cell {
                  let v6: Int = 0
                  v6
                }
                let v8: Int = cell_read(v7)
                let v11: mut (Int) -> Tuple[] = fn(
                  v9: Int,
                ) {
                  let v10: Tuple[] = cell_write(v7, v9)
                  v10
                }
                let v18: mut () -> Tuple[] = fn() {
                  let v12: Int = 1
                  let v13: Int = int_add(v8, v12)
                  let v14: Tuple[] = v11(v13)
                  let v15: Int = 1
                  let v16: Int = int_add(v2, v15)
                  let v17: Tuple[] = v5(v16)
                  v17
                }
                let v19: String = int_to_string(v8)
                let v20: Html = escape(v19)
                let v21: Html = text("/")
                let v22: String = int_to_string(v2)
                let v23: Html = escape(v22)
                let v24: Html = concat(v20, v21, v23)
                let v25: Id = mint()
                let v26: Html = element(v25, "button", {}, v24) on_click v18
                v26
              }
              let v28: Html = f0(v27)
              v28
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: mut () -> Tuple[] = fn() {
                let v6: Bool = bool_not(v2)
                let v7: Tuple[] = v5(v6)
                v7
              }
              let v9: Html = text("switch")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "b", {}, v10) on_click v8
              let v45: Html = match v2 {
                true => {
                  let v14: Cell[Int] = cell {
                    let v13: Int = 0
                    v13
                  }
                  let v15: Int = cell_read(v14)
                  let v18: mut (Int) -> Tuple[] = fn(
                    v16: Int,
                  ) {
                    let v17: Tuple[] = cell_write(v14, v16)
                    v17
                  }
                  let v22: mut () -> Tuple[] = fn() {
                    let v19: Int = 1
                    let v20: Int = int_add(v15, v19)
                    let v21: Tuple[] = v18(v20)
                    v21
                  }
                  let v23: Html = text("left ")
                  let v24: String = int_to_string(v15)
                  let v25: Html = escape(v24)
                  let v26: Html = concat(v23, v25)
                  let v27: Id = mint()
                  let v28: Html = element(v27, "button", {}, v26) on_click v22
                  v28
                }
                false => {
                  let v30: Cell[Int] = cell {
                    let v29: Int = 100
                    v29
                  }
                  let v31: Int = cell_read(v30)
                  let v34: mut (Int) -> Tuple[] = fn(
                    v32: Int,
                  ) {
                    let v33: Tuple[] = cell_write(v30, v32)
                    v33
                  }
                  let v38: mut () -> Tuple[] = fn() {
                    let v35: Int = 1
                    let v36: Int = int_add(v31, v35)
                    let v37: Tuple[] = v34(v36)
                    v37
                  }
                  let v39: Html = text("right ")
                  let v40: String = int_to_string(v31)
                  let v41: Html = escape(v40)
                  let v42: Html = concat(v39, v41)
                  let v43: Id = mint()
                  let v44: Html = element(v43, "button", {}, v42) on_click v38
                  v44
                }
              }
              let v46: Html = concat(v12, v45)
              v46
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[Int]] = cell {
                let v0: Int = 1
                let v1: Int = 2
                let v2: Array[Int] = [v0, v1]
                v2
              }
              let v4: Array[Int] = cell_read(v3)
              let v7: mut (Array[Int]) -> Tuple[] = fn(
                v5: Array[Int],
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: Int = 2
                let v9: Int = 1
                let v10: Array[Int] = [v8, v9]
                let v11: Tuple[] = v7(v10)
                v11
              }
              let v13: Html = text("flip")
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "b", {}, v14) on_click v12
              let v20: mut () -> Tuple[] = fn() {
                let v17: Int = 2
                let v18: Array[Int] = [v17]
                let v19: Tuple[] = v7(v18)
                v19
              }
              let v21: Html = text("drop")
              let v22: Html = concat(v21)
              let v23: Id = mint()
              let v24: Html = element(v23, "i", {}, v22) on_click v20
              let v44: Html = for v25: Int in v4 key {
                v25
              } {
                let v27: Cell[Int] = cell {
                  let v26: Int = 0
                  v26
                }
                let v28: Int = cell_read(v27)
                let v31: mut (Int) -> Tuple[] = fn(
                  v29: Int,
                ) {
                  let v30: Tuple[] = cell_write(v27, v29)
                  v30
                }
                let v35: mut () -> Tuple[] = fn() {
                  let v32: Int = 1
                  let v33: Int = int_add(v28, v32)
                  let v34: Tuple[] = v31(v33)
                  v34
                }
                let v36: String = int_to_string(v25)
                let v37: Html = escape(v36)
                let v38: Html = text(":")
                let v39: String = int_to_string(v28)
                let v40: Html = escape(v39)
                let v41: Html = concat(v37, v38, v40)
                let v42: Id = mint()
                let v43: Html = element(v42, "button", {}, v41) on_click v35
                v43
              }
              let v45: Html = concat(v16, v24, v44)
              v45
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[Int]] = cell {
                let v0: Int = 1
                let v1: Int = 1
                let v2: Array[Int] = [v0, v1]
                v2
              }
              let v4: Array[Int] = cell_read(v3)
              let v7: mut (Array[Int]) -> Tuple[] = fn(
                v5: Array[Int],
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v11: mut () -> Tuple[] = fn() {
                let v8: Int = 1
                let v9: Array[Int] = [v8]
                let v10: Tuple[] = v7(v9)
                v10
              }
              let v12: Html = text("drop")
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "b", {}, v13) on_click v11
              let v35: Html = for v16: Int in v4 key {
                v16
              } {
                let v18: Cell[Int] = cell {
                  let v17: Int = 0
                  v17
                }
                let v19: Int = cell_read(v18)
                let v22: mut (Int) -> Tuple[] = fn(
                  v20: Int,
                ) {
                  let v21: Tuple[] = cell_write(v18, v20)
                  v21
                }
                let v26: mut () -> Tuple[] = fn() {
                  let v23: Int = 1
                  let v24: Int = int_add(v19, v23)
                  let v25: Tuple[] = v22(v24)
                  v25
                }
                let v27: String = int_to_string(v16)
                let v28: Html = escape(v27)
                let v29: Html = text(":")
                let v30: String = int_to_string(v19)
                let v31: Html = escape(v30)
                let v32: Html = concat(v28, v29, v31)
                let v33: Id = mint()
                let v34: Html = element(v33, "button", {}, v32) on_click v26
                v34
              }
              let v36: Html = concat(v15, v35)
              v36
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[String]] = cell {
                let v0: String = "a"
                let v1: String = "b"
                let v2: Array[String] = [v0, v1]
                v2
              }
              let v4: Array[String] = cell_read(v3)
              let v7: mut (Array[String]) -> Tuple[] = fn(
                v5: Array[String],
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: String = "b"
                let v9: String = "a"
                let v10: Array[String] = [v8, v9]
                let v11: Tuple[] = v7(v10)
                v11
              }
              let v13: Html = text("flip")
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "b", {}, v14) on_click v12
              let v42: Html = for v17: String in v4 key {
                v17
              } {
                let v18: Int = 1
                let v19: Int = 2
                let v20: Array[Int] = [v18, v19]
                let v41: Html = for v21: Int in v20 key {
                  v21
                } {
                  let v23: Cell[Int] = cell {
                    let v22: Int = 0
                    v22
                  }
                  let v24: Int = cell_read(v23)
                  let v27: mut (Int) -> Tuple[] = fn(
                    v25: Int,
                  ) {
                    let v26: Tuple[] = cell_write(v23, v25)
                    v26
                  }
                  let v31: mut () -> Tuple[] = fn() {
                    let v28: Int = 1
                    let v29: Int = int_add(v24, v28)
                    let v30: Tuple[] = v27(v29)
                    v30
                  }
                  let v32: Html = escape(v17)
                  let v33: String = int_to_string(v21)
                  let v34: Html = escape(v33)
                  let v35: Html = text(":")
                  let v36: String = int_to_string(v24)
                  let v37: Html = escape(v36)
                  let v38: Html = concat(v32, v34, v35, v37)
                  let v39: Id = mint()
                  let v40: Html = element(v39, "button", {}, v38) on_click v31
                  v40
                }
                v41
              }
              let v43: Html = concat(v16, v42)
              v43
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = false
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: String = match v2 {
                true => {
                  let v6: String = "on"
                  v6
                }
                false => {
                  let v7: String = "off"
                  v7
                }
              }
              let v11: mut () -> Tuple[] = fn() {
                let v9: Bool = bool_not(v2)
                let v10: Tuple[] = v5(v9)
                v10
              }
              let v12: Html = text("toggle")
              let v13: Html = concat(v12)
              let v14: Id = mint()
              let v15: Html = element(v14, "button", {class: v8}, v13) on_click v11
              v15
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
            -- flat --
            render fn Level@f0(
              depth@v17: Int,
              levels@v18: Int,
            ) -> Html {
              let v20: Cell[Int] = cell {
                let v19: Int = 0
                v19
              }
              let v21: Int = cell_read(v20)
              let v24: mut (Int) -> Tuple[] = fn(
                v22: Int,
              ) {
                let v23: Tuple[] = cell_write(v20, v22)
                v23
              }
              let v28: mut () -> Tuple[] = fn() {
                let v25: Int = 1
                let v26: Int = int_add(v21, v25)
                let v27: Tuple[] = v24(v26)
                v27
              }
              let v29: String = int_to_string(v17)
              let v30: Html = escape(v29)
              let v31: Html = text(":")
              let v32: String = int_to_string(v21)
              let v33: Html = escape(v32)
              let v34: Html = concat(v30, v31, v33)
              let v35: Id = mint()
              let v36: Html = element(v35, "button", {}, v34) on_click v28
              let v37: Int = 1
              let v38: Int = int_add(v17, v37)
              let v39: Bool = int_lt(v38, v18)
              let v44: Html = match v39 {
                true => {
                  let v40: Int = 1
                  let v41: Int = int_add(v17, v40)
                  let v42: Html = f0(v41, v18)
                  v42
                }
                false => {
                  let v43: Html = concat()
                  v43
                }
              }
              let v45: Html = concat(v36, v44)
              let v46: Id = mint()
              let v47: Html = element(v46, "div", {}, v45)
              v47
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 2
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v10: Html = text("deeper")
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "b", {}, v11) on_click v9
              let v14: Int = 0
              let v15: Html = f0(v14, v2)
              let v16: Html = concat(v13, v15)
              v16
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
            -- flat --
            mut fn both@f0(
              first@v30: mut () -> Tuple[],
              second@v31: mut () -> Tuple[],
            ) -> Tuple[] {
              let v32: Tuple[] = v30()
              let v33: Tuple[] = v31()
              v33
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v7: Cell[Int] = cell {
                let v6: Int = 0
                v6
              }
              let v8: Int = cell_read(v7)
              let v11: mut (Int) -> Tuple[] = fn(
                v9: Int,
              ) {
                let v10: Tuple[] = cell_write(v7, v9)
                v10
              }
              let v15: mut () -> Tuple[] = fn() {
                let v12: Int = 1
                let v13: Int = int_add(v2, v12)
                let v14: Tuple[] = v5(v13)
                v14
              }
              let v19: mut () -> Tuple[] = fn() {
                let v16: Int = 10
                let v17: Int = int_add(v8, v16)
                let v18: Tuple[] = v11(v17)
                v18
              }
              let v21: mut () -> Tuple[] = fn() {
                let v20: Tuple[] = f0(v15, v19)
                v20
              }
              let v22: String = int_to_string(v2)
              let v23: Html = escape(v22)
              let v24: Html = text("/")
              let v25: String = int_to_string(v8)
              let v26: Html = escape(v25)
              let v27: Html = concat(v23, v24, v26)
              let v28: Id = mint()
              let v29: Html = element(v28, "button", {}, v27) on_click v21
              v29
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
            -- flat --
            render fn Button@f0(
              press@v21: mut () -> Tuple[],
              children@v22: Html,
            ) -> Html {
              let v23: Html = concat(v22)
              let v24: Id = mint()
              let v25: Html = element(v24, "button", {}, v23) on_click v21
              v25
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v9: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                v8
              }
              let v10: String = int_to_string(v2)
              let v11: Html = escape(v10)
              let v12: Html = concat(v11)
              let v13: Html = f0(v9, v12)
              let v16: mut () -> Tuple[] = fn() {
                let v14: Int = 0
                let v15: Tuple[] = v5(v14)
                v15
              }
              let v17: Html = text("reset")
              let v18: Html = concat(v17)
              let v19: Html = f0(v16, v18)
              let v20: Html = concat(v13, v19)
              v20
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Setting] = cell {
                let v0: Mode = Light
                let v1: Int = 1
                let v2: Setting = {mode: v0, level: v1}
                v2
              }
              let v4: Setting = cell_read(v3)
              let v7: mut (Setting) -> Tuple[] = fn(
                v5: Setting,
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: Mode = Dark
                let v9: Int = v4.level
                let v10: Setting = {mode: v8, level: v9}
                let v11: Tuple[] = v7(v10)
                v11
              }
              let v19: mut () -> Tuple[] = fn() {
                let v13: Mode = v4.mode
                let v14: Int = v4.level
                let v15: Int = 1
                let v16: Int = int_add(v14, v15)
                let v17: Setting = {mode: v13, level: v16}
                let v18: Tuple[] = v7(v17)
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
              let v27: Html = element(v26, "button", {}, v25) on_click v12
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
            -- flat --
            fn forever@f0(
              n@v17: Int,
            ) -> Int {
              let v18: Int = 1
              let v19: Int = int_add(v17, v18)
              let v20: Int = f0(v19)
              v20
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v11: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                let v9: Int = f0(v2)
                let v10: Tuple[] = v5(v9)
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v3: read () -> Int = fn() {
                let v2: Int = cell_read(v1)
                v2
              }
              let v6: mut (Int) -> Tuple[] = fn(
                v4: Int,
              ) {
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v8: Cell[String] = cell {
                let v7: String = "none"
                v7
              }
              let v9: String = cell_read(v8)
              let v12: mut (String) -> Tuple[] = fn(
                v10: String,
              ) {
                let v11: Tuple[] = cell_write(v8, v10)
                v11
              }
              let v20: mut () -> Tuple[] = fn() {
                let v13: Int = v3()
                let v14: Int = 1
                let v15: Int = int_add(v13, v14)
                let v16: Tuple[] = v6(v15)
                let v17: Int = v3()
                let v18: String = int_to_string(v17)
                let v19: Tuple[] = v12(v18)
                v19
              }
              let v21: Html = escape(v9)
              let v22: Html = concat(v21)
              let v23: Id = mint()
              let v24: Html = element(v23, "button", {}, v22) on_click v20
              v24
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
            -- flat --
            render fn Counter@f0() -> Html {
              let v4: Cell[Int] = cell {
                let v3: Int = 0
                v3
              }
              let v5: Int = cell_read(v4)
              let v8: mut (Int) -> Tuple[] = fn(
                v6: Int,
              ) {
                let v7: Tuple[] = cell_write(v4, v6)
                v7
              }
              let v12: mut () -> Tuple[] = fn() {
                let v9: Int = 1
                let v10: Int = int_add(v5, v9)
                let v11: Tuple[] = v8(v10)
                v11
              }
              let v13: String = int_to_string(v5)
              let v14: Html = escape(v13)
              let v15: Html = concat(v14)
              let v16: Id = mint()
              let v17: Html = element(v16, "button", {}, v15) on_click v12
              v17
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v2: Int = cell_read(v1)
              let v5: mut (Int) -> Tuple[] = fn(
                v3: Int,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v12: mut () -> Tuple[] = fn() {
                let v6: Int = 1
                let v7: Int = int_add(v2, v6)
                let v8: Tuple[] = v5(v7)
                let v9: Int = 10
                let v10: Int = int_add(v2, v9)
                let v11: Tuple[] = v5(v10)
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 0
                v0
              }
              let v3: read () -> Int = fn() {
                let v2: Int = cell_read(v1)
                v2
              }
              let v6: mut (Int) -> Tuple[] = fn(
                v4: Int,
              ) {
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v15: mut () -> Tuple[] = fn() {
                let v7: Int = v3()
                let v8: Int = 1
                let v9: Int = int_add(v7, v8)
                let v10: Tuple[] = v6(v9)
                let v11: Int = v3()
                let v12: Int = 10
                let v13: Int = int_add(v11, v12)
                let v14: Tuple[] = v6(v13)
                v14
              }
              let v16: Int = v3()
              let v17: String = int_to_string(v16)
              let v18: Html = escape(v17)
              let v19: Html = concat(v18)
              let v20: Id = mint()
              let v21: Html = element(v20, "button", {}, v19) on_click v15
              v21
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v3: Cell[Setting] = cell {
                let v0: Int = 0
                let v1: Int = 0
                let v2: Setting = {level: v0, size: v1}
                v2
              }
              let v4: Setting = cell_read(v3)
              let v7: mut (Setting) -> Tuple[] = fn(
                v5: Setting,
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v11: Cell[Setting] = cell {
                let v8: Int = 0
                let v9: Int = 0
                let v10: Setting = {level: v8, size: v9}
                v10
              }
              let v13: read () -> Setting = fn() {
                let v12: Setting = cell_read(v11)
                v12
              }
              let v16: mut (Setting) -> Tuple[] = fn(
                v14: Setting,
              ) {
                let v15: Tuple[] = cell_write(v11, v14)
                v15
              }
              let v29: mut () -> Tuple[] = fn() {
                let v17: Int = v4.level
                let v18: Int = 1
                let v19: Int = int_add(v17, v18)
                let v20: Int = v4.size
                let v21: Setting = {level: v19, size: v20}
                let v22: Tuple[] = v7(v21)
                let v23: Int = v4.level
                let v24: Int = v4.size
                let v25: Int = 1
                let v26: Int = int_add(v24, v25)
                let v27: Setting = {level: v23, size: v26}
                let v28: Tuple[] = v7(v27)
                v28
              }
              let v30: Int = v4.level
              let v31: String = int_to_string(v30)
              let v32: Html = escape(v31)
              let v33: Html = text("/")
              let v34: Int = v4.size
              let v35: String = int_to_string(v34)
              let v36: Html = escape(v35)
              let v37: Html = concat(v32, v33, v36)
              let v38: Id = mint()
              let v39: Html = element(v38, "button", {}, v37) on_click v29
              let v56: mut () -> Tuple[] = fn() {
                let v40: Setting = v13()
                let v41: Setting = v13()
                let v42: Int = v41.level
                let v43: Int = 1
                let v44: Int = int_add(v42, v43)
                let v45: Int = v40.size
                let v46: Setting = {level: v44, size: v45}
                let v47: Tuple[] = v16(v46)
                let v48: Setting = v13()
                let v49: Int = v48.level
                let v50: Setting = v13()
                let v51: Int = v50.size
                let v52: Int = 1
                let v53: Int = int_add(v51, v52)
                let v54: Setting = {level: v49, size: v53}
                let v55: Tuple[] = v16(v54)
                v55
              }
              let v57: Setting = v13()
              let v58: Int = v57.level
              let v59: String = int_to_string(v58)
              let v60: Html = escape(v59)
              let v61: Html = text("/")
              let v62: Setting = v13()
              let v63: Int = v62.size
              let v64: String = int_to_string(v63)
              let v65: Html = escape(v64)
              let v66: Html = concat(v60, v61, v65)
              let v67: Id = mint()
              let v68: Html = element(v67, "button", {}, v66) on_click v56
              let v69: Html = concat(v39, v68)
              v69
            }

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
            -- flat --
            render fn Counter@f0(
              label@v22: String,
            ) -> Html {
              let v24: Cell[Int] = cell {
                let v23: Int = 0
                v23
              }
              let v25: Int = cell_read(v24)
              let v28: mut (Int) -> Tuple[] = fn(
                v26: Int,
              ) {
                let v27: Tuple[] = cell_write(v24, v26)
                v27
              }
              let v32: mut () -> Tuple[] = fn() {
                let v29: Int = 1
                let v30: Int = int_add(v25, v29)
                let v31: Tuple[] = v28(v30)
                v31
              }
              let v33: Html = escape(v22)
              let v34: Html = text(":")
              let v35: String = int_to_string(v25)
              let v36: Html = escape(v35)
              let v37: Html = concat(v33, v34, v36)
              let v38: Id = mint()
              let v39: Html = element(v38, "button", {}, v37) on_click v32
              v39
            }
            entrypoint body@e0() -> Html {
              let v3: Cell[Array[Int]] = cell {
                let v0: Int = 1
                let v1: Int = 2
                let v2: Array[Int] = [v0, v1]
                v2
              }
              let v4: Array[Int] = cell_read(v3)
              let v7: mut (Array[Int]) -> Tuple[] = fn(
                v5: Array[Int],
              ) {
                let v6: Tuple[] = cell_write(v3, v5)
                v6
              }
              let v12: mut () -> Tuple[] = fn() {
                let v8: Int = 2
                let v9: Int = 1
                let v10: Array[Int] = [v8, v9]
                let v11: Tuple[] = v7(v10)
                v11
              }
              let v13: Html = text("flip")
              let v14: Html = concat(v13)
              let v15: Id = mint()
              let v16: Html = element(v15, "b", {}, v14) on_click v12
              let v20: Html = for v17: Int in v4 key {
                v17
              } {
                let v18: String = int_to_string(v17)
                let v19: Html = f0(v18)
                v19
              }
              let v21: Html = concat(v16, v20)
              v21
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
            -- flat --
            entrypoint body@e0() -> Html {
              let v1: Cell[Bool] = cell {
                let v0: Bool = true
                v0
              }
              let v2: Bool = cell_read(v1)
              let v5: mut (Bool) -> Tuple[] = fn(
                v3: Bool,
              ) {
                let v4: Tuple[] = cell_write(v1, v3)
                v4
              }
              let v8: mut () -> Tuple[] = fn() {
                let v6: Bool = bool_not(v2)
                let v7: Tuple[] = v5(v6)
                v7
              }
              let v9: Html = text("toggle")
              let v10: Html = concat(v9)
              let v11: Id = mint()
              let v12: Html = element(v11, "b", {}, v10) on_click v8
              let v32: Html = match v2 {
                true => {
                  let v14: Cell[Int] = cell {
                    let v13: Int = 0
                    v13
                  }
                  let v15: Int = cell_read(v14)
                  let v18: mut (Int) -> Tuple[] = fn(
                    v16: Int,
                  ) {
                    let v17: Tuple[] = cell_write(v14, v16)
                    v17
                  }
                  let v22: mut () -> Tuple[] = fn() {
                    let v19: Int = 1
                    let v20: Int = int_add(v15, v19)
                    let v21: Tuple[] = v18(v20)
                    v21
                  }
                  let v23: String = int_to_string(v15)
                  let v24: Html = escape(v23)
                  let v25: Html = concat(v24)
                  let v26: Id = mint()
                  let v27: Html = element(v26, "button", {}, v25) on_click v22
                  v27
                }
                false => {
                  let v28: Html = text("closed")
                  let v29: Html = concat(v28)
                  let v30: Id = mint()
                  let v31: Html = element(v30, "i", {}, v29)
                  v31
                }
              }
              let v33: Html = concat(v12, v32)
              v33
            }

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
            -- flat --
            render fn Stepper@f0(
              value@v15: read () -> Int,
              set_value@v16: mut (Int) -> Tuple[],
            ) -> Html {
              let v25: mut () -> Tuple[] = fn() {
                let v17: Int = v15()
                let v18: Int = 1
                let v19: Int = int_add(v17, v18)
                let v20: Tuple[] = v16(v19)
                let v21: Int = v15()
                let v22: Int = 2
                let v23: Int = int_mul(v21, v22)
                let v24: Tuple[] = v16(v23)
                v24
              }
              let v26: Int = v15()
              let v27: String = int_to_string(v26)
              let v28: Html = escape(v27)
              let v29: Html = concat(v28)
              let v30: Id = mint()
              let v31: Html = element(v30, "button", {}, v29) on_click v25
              v31
            }
            entrypoint body@e0() -> Html {
              let v1: Cell[Int] = cell {
                let v0: Int = 1
                v0
              }
              let v3: read () -> Int = fn() {
                let v2: Int = cell_read(v1)
                v2
              }
              let v6: mut (Int) -> Tuple[] = fn(
                v4: Int,
              ) {
                let v5: Tuple[] = cell_write(v1, v4)
                v5
              }
              let v7: Html = f0(v3, v6)
              let v8: Int = v3()
              let v9: String = int_to_string(v8)
              let v10: Html = escape(v9)
              let v11: Html = concat(v10)
              let v12: Id = mint()
              let v13: Html = element(v12, "p", {}, v11)
              let v14: Html = concat(v7, v13)
              v14
            }

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
