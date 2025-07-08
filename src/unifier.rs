use crate::common::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct UnificationError {
    pub message: String,
}

impl UnificationError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

pub struct Unifier {
    substitutions: HashMap<i32, Type>,
    next_type_var_id: i32,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
            next_type_var_id: 0,
        }
    }

    fn next_type_var(&mut self) -> i32 {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        id
    }

    pub fn new_type_var(&mut self) -> Type {
        Type::TypeVar(self.next_type_var())
    }

    pub fn new_object(&mut self, map: HashMap<String, Type>) -> Type {
        Type::Object(map, self.next_type_var())
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Option<UnificationError> {
        if self.types_equal(a, b) {
            return None;
        }

        match (a, b) {
            (Type::TypeVar(id_a), _) => self.unify_type_var(*id_a, b),
            (_, Type::TypeVar(id_b)) => self.unify_type_var(*id_b, a),
            (Type::Array(type_a), Type::Array(type_b)) => self.unify(type_a, type_b),
            (Type::Object(props_a, rest_a), Type::Object(props_b, rest_b)) => {
                // Find common properties and unify them
                for (key, type_a) in props_a {
                    if let Some(type_b) = props_b.get(key) {
                        if let Some(err) = self.unify(type_a, type_b) {
                            return Some(err);
                        }
                    }
                }

                // Collect missing properties
                let missing_from_a: HashMap<String, Type> = props_b
                    .iter()
                    .filter(|(key, _)| !props_a.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                let missing_from_b: HashMap<String, Type> = props_a
                    .iter()
                    .filter(|(key, _)| !props_b.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                // Create shared rest type variable
                let shared_rest = self.next_type_var();

                // Unify rest types with missing properties
                if let Some(err) = self.unify(
                    &Type::TypeVar(*rest_a),
                    &Type::Object(missing_from_a, shared_rest),
                ) {
                    return Some(err);
                }

                if let Some(err) = self.unify(
                    &Type::TypeVar(*rest_b),
                    &Type::Object(missing_from_b, shared_rest),
                ) {
                    return Some(err);
                }

                None
            }
            _ => Some(UnificationError::new("Can not unify types".to_string())),
        }
    }

    fn unify_type_var(&mut self, var_id: i32, other_type: &Type) -> Option<UnificationError> {
        if let Some(substituted_type) = self.substitutions.get(&var_id) {
            return self.unify(&substituted_type.clone(), other_type);
        }

        if let Type::TypeVar(other_id) = other_type {
            if let Some(other_substituted) = self.substitutions.get(other_id) {
                return self.unify(&Type::TypeVar(var_id), &other_substituted.clone());
            }
        }

        self.substitutions.insert(var_id, other_type.clone());
        None
    }

    pub fn query(&self, t: &Type) -> Type {
        match t {
            Type::TypeVar(id) => {
                if let Some(substituted_type) = self.substitutions.get(id) {
                    self.query(substituted_type)
                } else {
                    t.clone()
                }
            }
            Type::Array(sub_type) => Type::Array(Box::new(self.query(sub_type))),
            Type::Object(props, rest) => {
                let queried_props: HashMap<String, Type> = props
                    .iter()
                    .map(|(k, v)| (k.clone(), self.query(v)))
                    .collect();

                match self.query(&Type::TypeVar(*rest)) {
                    Type::Object(rest_props, rest_rest) => {
                        let mut merged_props = queried_props;
                        for (k, v) in rest_props {
                            merged_props.insert(k, v);
                        }
                        Type::Object(merged_props, rest_rest)
                    }
                    Type::TypeVar(rest_id) => Type::Object(queried_props, rest_id),
                    _ => panic!("Invalid type substitution for object rest"),
                }
            }
            Type::Bool | Type::String | Type::Void => t.clone(),
        }
    }

    fn types_equal(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::TypeVar(id_a), Type::TypeVar(id_b)) => id_a == id_b,
            (Type::Array(type_a), Type::Array(type_b)) => self.types_equal(type_a, type_b),
            (Type::Object(props_a, rest_a), Type::Object(props_b, rest_b)) => {
                rest_a == rest_b
                    && props_a.len() == props_b.len()
                    && props_a
                        .iter()
                        .all(|(k, v)| props_b.get(k).is_some_and(|v2| self.types_equal(v, v2)))
            }
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::Path;

    #[derive(Debug, Clone, PartialEq)]
    pub enum SExpr {
        Command(String, Vec<SExpr>),
        Symbol(String),
    }

    impl SExpr {
        /// Parse an S-expression from a string. Panics on invalid input.
        pub fn parse(input: &str) -> SExpr {
            let mut pos = 0;
            let result = Self::parse_expr(input, &mut pos);
            Self::skip_whitespace(input, &mut pos);
            assert!(
                pos >= input.len(),
                "Unexpected characters after S-expression"
            );
            result
        }

        fn parse_expr(input: &str, pos: &mut usize) -> SExpr {
            Self::skip_whitespace(input, pos);
            assert!(*pos < input.len(), "Unexpected end of input");
            let chars: Vec<char> = input.chars().collect();
            match chars[*pos] {
                '(' => Self::parse_command(input, pos),
                ')' => panic!("Unexpected closing parenthesis"),
                _ => Self::parse_symbol(input, pos),
            }
        }

        fn parse_command(input: &str, pos: &mut usize) -> SExpr {
            let chars: Vec<char> = input.chars().collect();
            assert!(
                *pos < chars.len() && chars[*pos] == '(',
                "Expected opening parenthesis"
            );
            *pos += 1;
            let mut elements = Vec::new();
            loop {
                Self::skip_whitespace(input, pos);
                assert!(*pos < chars.len(), "Unclosed command");
                if chars[*pos] == ')' {
                    *pos += 1; // Consume closing parenthesis
                    break;
                }
                elements.push(Self::parse_expr(input, pos));
            }
            assert!(
                !elements.is_empty(),
                "Command must have at least one element (name)"
            );
            let name = match &elements[0] {
                SExpr::Symbol(s) => s.clone(),
                SExpr::Command(_, _) => panic!("Command name must be a symbol, not a command"),
            };
            let args = elements.into_iter().skip(1).collect();
            SExpr::Command(name, args)
        }

        fn parse_symbol(input: &str, pos: &mut usize) -> SExpr {
            let chars: Vec<char> = input.chars().collect();
            let mut symbol = String::new();
            while *pos < chars.len() {
                let ch = chars[*pos];
                if ch.is_whitespace() || ch == '(' || ch == ')' {
                    break;
                }
                symbol.push(ch);
                *pos += 1;
            }
            assert!(!symbol.is_empty(), "Empty symbol");
            SExpr::Symbol(symbol)
        }

        fn skip_whitespace(input: &str, pos: &mut usize) {
            let chars: Vec<char> = input.chars().collect();
            while *pos < chars.len() && chars[*pos].is_whitespace() {
                *pos += 1;
            }
        }
    }

    fn sexpr_to_type(sexpr: SExpr, table: &HashMap<String, Type>, unifier: &mut Unifier) -> Type {
        match sexpr {
            SExpr::Command(cmd, args) => match cmd.as_str() {
                "array" => {
                    assert!(args.len() == 1);
                    Type::Array(Box::new(sexpr_to_type(args[0].clone(), table, unifier)))
                }
                "object" => {
                    let mut map: HashMap<String, Type> = HashMap::new();
                    for chunk in args.chunks(2) {
                        let value = sexpr_to_type(chunk[1].clone(), table, unifier);
                        let key = match &chunk[0] {
                            SExpr::Symbol(s) => s.clone(),
                            _ => panic!(),
                        };
                        map.insert(key, value);
                    }
                    Type::Object(map, unifier.next_type_var())
                }
                _ => panic!(),
            },
            SExpr::Symbol(str) => match str.as_str() {
                "string" => Type::String,
                "bool" => Type::Bool,
                "void" => Type::Void,
                _ => {
                    // Get type var from table
                    assert!(str.len() > 0 && str.starts_with('t'));
                    table.get(&str).unwrap().clone()
                }
            },
        }
    }

    #[test]
    fn test_unifier() {
        let entries = fs::read_dir(Path::new("test_data/unifier")).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let input = archive.get("in").unwrap().content.trim();
            let expected = archive.get("out").unwrap().content.trim();
            let mut table: HashMap<String, Type> = HashMap::new();

            let mut unifier = Unifier::new();

            // Reserve type vars t1, t2, ... in a table
            for n in 1..101 {
                table.insert(format!("t{}", n), unifier.new_type_var());
            }

            let mut lines: Vec<String> = Vec::new();

            println!("{}", file_name);

            for line in input.split("\n") {
                match SExpr::parse(line) {
                    SExpr::Command(cmd, args) => match cmd.as_str() {
                        "unify" => {
                            assert!(args.len() == 2);
                            let t1 = &sexpr_to_type(args[0].clone(), &table, &mut unifier);
                            let t2 = &sexpr_to_type(args[1].clone(), &table, &mut unifier);
                            unifier.unify(t1, t2);
                        }
                        "query" => {
                            assert!(args.len() == 1);
                            let t1 = &sexpr_to_type(args[0].clone(), &table, &mut unifier);
                            lines.push(format!("{}", unifier.query(t1)));
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }

            let output = lines.join("\n");

            assert_eq!(output, expected, "Mismatch in file: {}", file_name);
        }
    }

    #[test]
    fn test_sexpr() {
        assert_eq!(SExpr::parse("hello"), SExpr::Symbol("hello".to_string()));
        assert_eq!(
            SExpr::parse("(print)"),
            SExpr::Command("print".to_string(), vec![])
        );
        assert_eq!(
            SExpr::parse("(add a b)"),
            SExpr::Command(
                "add".to_string(),
                vec![
                    SExpr::Symbol("a".to_string()),
                    SExpr::Symbol("b".to_string())
                ]
            )
        );
        assert_eq!(
            SExpr::parse("(if (> x 0) (print positive))"),
            SExpr::Command(
                "if".to_string(),
                vec![
                    SExpr::Command(
                        ">".to_string(),
                        vec![
                            SExpr::Symbol("x".to_string()),
                            SExpr::Symbol("0".to_string())
                        ]
                    ),
                    SExpr::Command(
                        "print".to_string(),
                        vec![SExpr::Symbol("positive".to_string())]
                    )
                ]
            )
        );
    }
}
