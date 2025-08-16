use std::fs;
use syn::visit::Visit;
use syn::{Expr, ExprMatch, Pat, Stmt};

#[derive(Debug, Clone)]
struct StateTransition {
    from_state: String,
    to_state: String,
    condition: String,
    actions: Vec<String>,
}

#[derive(Default)]
struct StateMachineVisitor {
    states: Vec<String>,
    transitions: Vec<StateTransition>,
    current_match_expr: Option<String>,
    current_state: Option<String>,
}

impl StateMachineVisitor {
    fn new() -> Self {
        Self::default()
    }

    fn extract_condition_from_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(binary) => {
                format!(
                    "{} {} {}",
                    self.extract_simple_expr(&binary.left),
                    binary.op.to_token_stream(),
                    self.extract_simple_expr(&binary.right)
                )
            }
            Expr::Call(call) => {
                if let Expr::Path(path) = &*call.func {
                    if let Some(ident) = path.path.get_ident() {
                        return format!("{}()", ident);
                    }
                }
                "function_call".to_string()
            }
            Expr::MethodCall(method) => {
                format!(
                    "{}.{}()",
                    self.extract_simple_expr(&method.receiver),
                    method.method
                )
            }
            _ => "condition".to_string(),
        }
    }

    fn extract_simple_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Path(path) => {
                if let Some(ident) = path.path.get_ident() {
                    ident.to_string()
                } else {
                    "path".to_string()
                }
            }
            Expr::Lit(lit) => format!("{}", lit.lit.to_token_stream()),
            _ => "expr".to_string(),
        }
    }

    fn extract_actions_from_block(&self, stmts: &[Stmt]) -> Vec<String> {
        let mut actions = Vec::new();

        for stmt in stmts {
            if let Stmt::Expr(expr, _) = stmt {
                if let Some(action) = self.extract_action_from_expr(expr) {
                    actions.push(action);
                }
            }
        }

        // Validate that set_state is the last action if there are any actions
        if let Some(last_action) = actions.last() {
            if last_action != "set_state" {
                panic!(
                    "Last action in block is not set_state. Actions: {:?}",
                    actions
                );
            } else {
                // Remove last action from output since it is always set_state
                actions.pop();
            }
        }

        actions
    }

    fn extract_action_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::MethodCall(method) => {
                let receiver = self.extract_simple_expr(&method.receiver);
                let method_name = method.method.to_string();

                // Identify key tokenizer actions
                if receiver == "builder" {
                    match method_name.as_str() {
                        "push_current_token" => Some("push_token".to_string()),
                        "append_to_current_token_value" => Some("append_to_token".to_string()),
                        "set_current_token_kind" => Some("set_token_kind".to_string()),
                        "reset" => Some("reset".to_string()),
                        _ => None,
                    }
                } else if receiver == "cursor" {
                    match method_name.as_str() {
                        "advance" => Some("advance".to_string()),
                        "advance_n" => Some("advance_n".to_string()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Expr::Assign(assign) => {
                if let Expr::Path(path) = &*assign.left {
                    if let Some(ident) = path.path.get_ident() {
                        if ident == "state" {
                            return Some("set_state".to_string());
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn process_if_chain(&mut self, expr: &Expr, current_state: &str) {
        if let Expr::If(if_expr) = expr {
            let condition = self.extract_condition_from_expr(&if_expr.cond);
            let actions = self.extract_actions_from_block(&if_expr.then_branch.stmts);

            // Look for state transitions in the then block
            for stmt in &if_expr.then_branch.stmts {
                if let Some(target_state) = self.extract_state_transition(stmt) {
                    self.transitions.push(StateTransition {
                        from_state: current_state.to_string(),
                        to_state: target_state,
                        condition: condition.clone(),
                        actions: actions.clone(),
                    });
                }
            }

            // Process else branch
            if let Some((_, else_expr)) = &if_expr.else_branch {
                match &**else_expr {
                    Expr::If(_) => {
                        self.process_if_chain(else_expr, current_state);
                    }
                    Expr::Block(block) => {
                        let else_actions = self.extract_actions_from_block(&block.block.stmts);
                        for stmt in &block.block.stmts {
                            if let Some(target_state) = self.extract_state_transition(stmt) {
                                self.transitions.push(StateTransition {
                                    from_state: current_state.to_string(),
                                    to_state: target_state,
                                    condition: "else".to_string(),
                                    actions: else_actions.clone(),
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn extract_state_transition(&self, stmt: &Stmt) -> Option<String> {
        if let Stmt::Expr(Expr::Assign(assign), _) = stmt {
            if let Expr::Path(path) = &*assign.left {
                if let Some(ident) = path.path.get_ident() {
                    if ident == "state" {
                        if let Expr::Path(right_path) = &*assign.right {
                            if let Some(segments) = right_path.path.segments.last() {
                                return Some(segments.ident.to_string());
                            }
                        }
                    }
                }
            }
        }
        None
    }
}

impl<'ast> Visit<'ast> for StateMachineVisitor {
    fn visit_item_enum(&mut self, node: &'ast syn::ItemEnum) {
        // Extract states from TokenizerState enum
        if node.ident == "TokenizerState" {
            for variant in &node.variants {
                let state_name = variant.ident.to_string();
                if !self.states.contains(&state_name) {
                    self.states.push(state_name);
                }
            }
        }
        syn::visit::visit_item_enum(self, node);
    }

    fn visit_expr_match(&mut self, node: &'ast ExprMatch) {
        // Check if this is matching on 'state'
        if let Expr::Path(path) = &*node.expr {
            if let Some(ident) = path.path.get_ident() {
                if ident == "state" {
                    self.current_match_expr = Some("state".to_string());

                    // Process each arm of the match
                    for arm in &node.arms {
                        if let Pat::Path(pat_path) = &arm.pat {
                            if let Some(segments) = pat_path.path.segments.last() {
                                let state_name = segments.ident.to_string();
                                self.current_state = Some(state_name.clone());

                                // Process the arm body for state transitions
                                if let Expr::Block(block) = &*arm.body {
                                    for stmt in &block.block.stmts {
                                        match stmt {
                                            Stmt::Expr(Expr::If(_), _) => {
                                                if let Stmt::Expr(expr, _) = stmt {
                                                    self.process_if_chain(expr, &state_name);
                                                }
                                            }
                                            _ => {
                                                if let Some(target_state) =
                                                    self.extract_state_transition(stmt)
                                                {
                                                    self.transitions.push(StateTransition {
                                                        from_state: state_name.clone(),
                                                        to_state: target_state,
                                                        condition: "direct".to_string(),
                                                        actions: vec![],
                                                    });
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        syn::visit::visit_expr_match(self, node);
    }
}

fn generate_graphviz(states: &[String], transitions: &[StateTransition]) -> String {
    let mut dot = String::new();
    dot.push_str("digraph TokenizerStateMachine {\n");
    dot.push_str(
        "  graph [rankdir=LR, fontname=\"Arial\", splines=true, nodesep=0.8, ranksep=1.0];\n",
    );
    dot.push_str("  node [shape=circle, style=filled, fillcolor=lightblue, fontname=\"Arial\", fontsize=12];\n");
    dot.push_str("  edge [fontname=\"Arial\", fontsize=10];\n\n");

    // Add nodes
    for state in states {
        dot.push_str(&format!("  {};\n", state));
    }

    dot.push('\n');

    // Add invisible start state
    dot.push_str("  Start [style=invis];\n");
    dot.push_str("  Start -> Text;\n");

    // Add edges
    for transition in transitions {
        let mut label = transition.condition.clone();
        if !transition.actions.is_empty() {
            label.push_str("\\n");
            label.push_str(&transition.actions.join(", "));
        } else {
            panic!(
                "Transition has no actions:\nFrom: {}\nTo: {}\nCondition: {}\n",
                transition.from_state, transition.to_state, transition.condition
            );
        }

        // Escape quotes for graphviz
        let escaped_label = label.replace("\"", "\\\"");

        // Check if actions contain "reset" to color edge red
        let edge_color = if transition
            .actions
            .iter()
            .any(|action| action.contains("reset"))
        {
            " color=red"
        } else {
            ""
        };

        dot.push_str(&format!(
            "  {} -> {} [label=\"{}\"{}];\n",
            transition.from_state, transition.to_state, escaped_label, edge_color
        ));
    }

    dot.push_str("}\n");
    dot
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the tokenizer source file
    let source = fs::read_to_string("../src/tokenizer.rs")?;

    // Parse the Rust code
    let syntax_tree = syn::parse_file(&source)?;

    // Visit the AST to extract state machine information
    let mut visitor = StateMachineVisitor::new();
    visitor.visit_file(&syntax_tree);

    // Generate graphviz output
    let dot_content = generate_graphviz(&visitor.states, &visitor.transitions);

    // Write to file
    fs::write("../tokenizer_state_machine.dot", &dot_content)?;

    println!("Generated tokenizer_state_machine.dot");
    println!("States found: {}", visitor.states.len());
    println!("Transitions found: {}", visitor.transitions.len());
    println!("\nTo render the graph:");
    println!("  dot -Tpng tokenizer_state_machine.dot -o tokenizer_state_machine.png");
    println!("  dot -Tsvg tokenizer_state_machine.dot -o tokenizer_state_machine.svg");
    println!("\nOr view online at: https://dreampuf.github.io/GraphvizOnline/");

    Ok(())
}

use quote::ToTokens;
