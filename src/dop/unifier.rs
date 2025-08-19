use std::collections::BTreeMap;
use std::fmt;

pub type TypeVarId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Row {
    Closed,
    Open(TypeVarId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DopType {
    Object(BTreeMap<String, DopType>, Row),
    Array(Box<DopType>),
    Bool,
    String,
    Number,
    Void,
    TypeVar(Option<TypeVarId>),
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties, state) => {
                write!(f, "object[")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                if matches!(state, Row::Open(_)) {
                    if !properties.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "..")?;
                }
                write!(f, "]")
            }
            DopType::Array(inner_type) => write!(f, "array[{}]", inner_type),
            DopType::Bool => write!(f, "boolean"),
            DopType::String => write!(f, "string"),
            DopType::Number => write!(f, "number"),
            DopType::Void => write!(f, "void"),
            DopType::TypeVar(Some(id)) => write!(f, "?t{}", id),
            DopType::TypeVar(None) => write!(f, "any"),
        }
    }
}

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
    substitutions: Vec<Option<DopType>>,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            substitutions: Vec::new(),
        }
    }

    fn next_type_var(&mut self) -> TypeVarId {
        let id = self.substitutions.len();
        self.substitutions.push(None);
        id
    }

    pub fn new_type_var(&mut self) -> DopType {
        DopType::TypeVar(Some(self.next_type_var()))
    }

    pub fn new_object(&mut self, map: BTreeMap<String, DopType>) -> DopType {
        DopType::Object(map, Row::Open(self.next_type_var()))
    }

    /// Construct a type that is the least upper bound of `a` and `b` and constrain `a` and `b` to
    /// be this type, or fail if there is no representation of the least upper bound of `a` and `b`
    /// in the type system.
    pub fn unify(&mut self, a: &DopType, b: &DopType) -> Result<(), UnificationError> {
        match (a, b) {
            (DopType::Bool, DopType::Bool) => Ok(()),
            (DopType::String, DopType::String) => Ok(()),
            (DopType::Number, DopType::Number) => Ok(()),
            (DopType::Void, DopType::Void) => Ok(()),
            (DopType::TypeVar(a), DopType::TypeVar(b)) if a == b => Ok(()),
            (DopType::TypeVar(Some(id_a)), _) => self.unify_type_var(*id_a, b),
            (_, DopType::TypeVar(Some(id_b))) => self.unify_type_var(*id_b, a),
            (DopType::Array(type_a), DopType::Array(type_b)) => self.unify(type_a, type_b),
            (DopType::Object(props_a, state_a), DopType::Object(props_b, state_b)) => {
                // Find common properties and unify them
                for (key, type_a) in props_a {
                    if let Some(type_b) = props_b.get(key) {
                        self.unify(type_a, type_b)?;
                    }
                }

                // Collect missing properties
                let missing_from_a: BTreeMap<String, DopType> = props_b
                    .iter()
                    .filter(|(key, _)| !props_a.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                let missing_from_b: BTreeMap<String, DopType> = props_a
                    .iter()
                    .filter(|(key, _)| !props_b.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                match (state_a, state_b) {
                    (Row::Open(rest_a_id), Row::Open(rest_b_id)) => {
                        let shared_rest = self.next_type_var();
                        self.unify_type_var(
                            *rest_a_id,
                            &DopType::Object(missing_from_a, Row::Open(shared_rest)),
                        )?;
                        self.unify_type_var(
                            *rest_b_id,
                            &DopType::Object(missing_from_b, Row::Open(shared_rest)),
                        )?;
                    }
                    (Row::Closed, Row::Open(rest_b_id)) => {
                        if !missing_from_a.is_empty() {
                            return Err(UnificationError::new(
                                "Closed object missing required properties".to_string(),
                            ));
                        }
                        self.unify_type_var(
                            *rest_b_id,
                            &DopType::Object(missing_from_b, Row::Closed),
                        )?;
                    }
                    (Row::Open(rest_a_id), Row::Closed) => {
                        if !missing_from_b.is_empty() {
                            return Err(UnificationError::new(
                                "Closed object missing required properties".to_string(),
                            ));
                        }
                        self.unify_type_var(
                            *rest_a_id,
                            &DopType::Object(missing_from_a, Row::Closed),
                        )?;
                    }
                    (Row::Closed, Row::Closed) => {
                        if !missing_from_a.is_empty() || !missing_from_b.is_empty() {
                            return Err(UnificationError::new(
                                "Closed objects have different properties".to_string(),
                            ));
                        }
                    }
                }

                Ok(())
            }
            _ => Err(UnificationError::new("Can not unify types".to_string())),
        }
    }

    fn unify_type_var(
        &mut self,
        var_id: TypeVarId,
        other_type: &DopType,
    ) -> Result<(), UnificationError> {
        if let Some(substituted_type) = &self.substitutions[var_id] {
            return self.unify(&substituted_type.clone(), other_type);
        }

        if let DopType::TypeVar(Some(other_id)) = other_type {
            if let Some(other_substituted) = &self.substitutions[*other_id] {
                return self.unify(&DopType::TypeVar(Some(var_id)), &other_substituted.clone());
            }
        }

        self.substitutions[var_id] = Some(other_type.clone());
        Ok(())
    }

    /// Resolves a type to its concrete form.
    ///
    /// Note that the returned type will be immutable and not
    /// open to extension.
    pub fn resolve(&self, t: &DopType) -> DopType {
        match t {
            DopType::TypeVar(Some(id)) => {
                if let Some(substituted_type) = &self.substitutions[*id] {
                    self.resolve(substituted_type)
                } else {
                    DopType::TypeVar(None)
                }
            }
            DopType::TypeVar(None) => DopType::TypeVar(None),
            DopType::Array(sub_type) => DopType::Array(Box::new(self.resolve(sub_type))),
            DopType::Object(props, state) => {
                let resolved_props: BTreeMap<String, DopType> = props
                    .iter()
                    .map(|(k, v)| (k.clone(), self.resolve(v)))
                    .collect();

                match state {
                    Row::Open(rest_id) => match self.resolve(&DopType::TypeVar(Some(*rest_id))) {
                        DopType::Object(rest_props, _) => {
                            let mut merged_props = resolved_props;
                            for (k, v) in rest_props {
                                merged_props.insert(k, v);
                            }
                            DopType::Object(merged_props, Row::Closed)
                        }
                        DopType::TypeVar(None) => DopType::Object(resolved_props, Row::Closed),
                        _ => panic!("Invalid type substitution for object rest"),
                    },
                    Row::Closed => DopType::Object(resolved_props, Row::Closed),
                }
            }
            DopType::Bool | DopType::String | DopType::Number | DopType::Void => t.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_test_cases;
    use pretty_assertions::assert_eq;

    use std::fs;
    use std::path::PathBuf;

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

    fn sexpr_to_type(
        sexpr: SExpr,
        table: &std::collections::HashMap<String, DopType>,
        unifier: &mut Unifier,
    ) -> DopType {
        match sexpr {
            SExpr::Command(cmd, args) => match cmd.as_str() {
                "array" => {
                    assert!(args.len() == 1);
                    DopType::Array(Box::new(sexpr_to_type(args[0].clone(), table, unifier)))
                }
                "object" => {
                    let mut map: BTreeMap<String, DopType> = BTreeMap::new();
                    for chunk in args.chunks(2) {
                        let value = sexpr_to_type(chunk[1].clone(), table, unifier);
                        let key = match &chunk[0] {
                            SExpr::Symbol(s) => s.clone(),
                            _ => panic!(),
                        };
                        map.insert(key, value);
                    }
                    DopType::Object(map, Row::Open(unifier.next_type_var()))
                }
                _ => panic!(),
            },
            SExpr::Symbol(str) => match str.as_str() {
                "string" => DopType::String,
                "bool" => DopType::Bool,
                "number" => DopType::Number,
                "void" => DopType::Void,
                _ => {
                    // Get type var from table
                    assert!(str.starts_with('t'));
                    table.get(&str).unwrap().clone()
                }
            },
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

    #[test]
    fn test_unifier() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/unifier.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let input = archive
                .get("in")
                .expect("Missing 'in' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();
            let mut table: std::collections::HashMap<String, DopType> =
                std::collections::HashMap::new();

            let mut unifier = Unifier::new();

            // Reserve type vars t1, t2, ... in a table
            for n in 1..101 {
                table.insert(format!("t{}", n), unifier.new_type_var());
            }

            let mut lines: Vec<String> = Vec::new();

            println!("Test case {} (line {})", case_num + 1, line_number);

            for line in input.split("\n") {
                if line.trim().is_empty() {
                    continue;
                }
                match SExpr::parse(line) {
                    SExpr::Command(cmd, args) => match cmd.as_str() {
                        "unify" => {
                            assert!(args.len() == 2);
                            let t1 = &sexpr_to_type(args[0].clone(), &table, &mut unifier);
                            let t2 = &sexpr_to_type(args[1].clone(), &table, &mut unifier);
                            if let Err(err) = unifier.unify(t1, t2) {
                                lines.push(err.message);
                            }
                        }
                        "resolve" => {
                            assert!(args.len() == 1);
                            let t1 = &sexpr_to_type(args[0].clone(), &table, &mut unifier);
                            let val = match &args[0] {
                                SExpr::Symbol(s) => s,
                                SExpr::Command(..) => panic!(),
                            };
                            lines.push(format!("{} : {}", val, unifier.resolve(t1)));
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }

            let output = lines.join("\n");

            assert_eq!(
                output,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }
}
