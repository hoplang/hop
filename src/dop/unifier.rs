use std::collections::BTreeMap;
use std::fmt;

pub type TypeVarId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteDopType {
    Object(BTreeMap<String, ConcreteDopType>),
    Array(Box<ConcreteDopType>),
    Bool,
    String,
    Number,
    Void,
    Any,
}

impl fmt::Display for ConcreteDopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConcreteDopType::Object(properties) => {
                write!(f, "object[")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "]")
            }
            ConcreteDopType::Array(inner_type) => write!(f, "array[{}]", inner_type),
            ConcreteDopType::Bool => write!(f, "boolean"),
            ConcreteDopType::String => write!(f, "string"),
            ConcreteDopType::Number => write!(f, "number"),
            ConcreteDopType::Void => write!(f, "void"),
            ConcreteDopType::Any => write!(f, "any"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AbstractDopType {
    Object(BTreeMap<String, AbstractDopType>, TypeVarId),
    Array(Box<AbstractDopType>),
    Bool,
    String,
    Number,
    Void,
    TypeVar(TypeVarId),
}

impl fmt::Display for AbstractDopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbstractDopType::Object(properties, _rest_id) => {
                write!(f, "object[")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                if !properties.is_empty() {
                    write!(f, ", ")?;
                }
                write!(f, "..")?;
                write!(f, "]")
            }
            AbstractDopType::Array(inner_type) => write!(f, "array[{}]", inner_type),
            AbstractDopType::Bool => write!(f, "boolean"),
            AbstractDopType::String => write!(f, "string"),
            AbstractDopType::Number => write!(f, "number"),
            AbstractDopType::Void => write!(f, "void"),
            AbstractDopType::TypeVar(id) => write!(f, "?t{}", id),
        }
    }
}


pub struct Unifier {
    substitutions: Vec<Option<AbstractDopType>>,
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

    pub fn new_type_var(&mut self) -> AbstractDopType {
        AbstractDopType::TypeVar(self.next_type_var())
    }

    pub fn new_object(&mut self, map: BTreeMap<String, AbstractDopType>) -> AbstractDopType {
        AbstractDopType::Object(map, self.next_type_var())
    }

    /// Construct a type that is the least upper bound of `a` and `b` and constrain `a` and `b` to
    /// be this type, or fail if there is no representation of the least upper bound of `a` and `b`
    /// in the type system.
    pub fn unify(
        &mut self,
        a: &AbstractDopType,
        b: &AbstractDopType,
    ) -> Result<(), String> {
        match (a, b) {
            (AbstractDopType::Bool, AbstractDopType::Bool) => Ok(()),
            (AbstractDopType::String, AbstractDopType::String) => Ok(()),
            (AbstractDopType::Number, AbstractDopType::Number) => Ok(()),
            (AbstractDopType::Void, AbstractDopType::Void) => Ok(()),
            (AbstractDopType::TypeVar(a), AbstractDopType::TypeVar(b)) if a == b => Ok(()),
            (AbstractDopType::TypeVar(id_a), _) => self.unify_type_var(*id_a, b),
            (_, AbstractDopType::TypeVar(id_b)) => self.unify_type_var(*id_b, a),
            (AbstractDopType::Array(type_a), AbstractDopType::Array(type_b)) => {
                self.unify(type_a, type_b)
            }
            (
                AbstractDopType::Object(props_a, rest_a_id),
                AbstractDopType::Object(props_b, rest_b_id),
            ) => {
                // Find common properties and unify them
                for (key, type_a) in props_a {
                    if let Some(type_b) = props_b.get(key) {
                        self.unify(type_a, type_b)?;
                    }
                }

                // Collect missing properties
                let missing_from_a: BTreeMap<String, AbstractDopType> = props_b
                    .iter()
                    .filter(|(key, _)| !props_a.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                let missing_from_b: BTreeMap<String, AbstractDopType> = props_a
                    .iter()
                    .filter(|(key, _)| !props_b.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                let shared_rest = self.next_type_var();
                self.unify_type_var(
                    *rest_a_id,
                    &AbstractDopType::Object(missing_from_a, shared_rest),
                )?;
                self.unify_type_var(
                    *rest_b_id,
                    &AbstractDopType::Object(missing_from_b, shared_rest),
                )?;

                Ok(())
            }
            _ => Err("Can not unify types".to_string()),
        }
    }

    fn unify_type_var(
        &mut self,
        var_id: TypeVarId,
        other_type: &AbstractDopType,
    ) -> Result<(), String> {
        if let Some(substituted_type) = &self.substitutions[var_id] {
            return self.unify(&substituted_type.clone(), other_type);
        }

        if let AbstractDopType::TypeVar(other_id) = other_type {
            if let Some(other_substituted) = &self.substitutions[*other_id] {
                return self.unify(
                    &AbstractDopType::TypeVar(var_id),
                    &other_substituted.clone(),
                );
            }
        }

        self.substitutions[var_id] = Some(other_type.clone());
        Ok(())
    }

    /// Constrains `a` to be a subtype of `b`, where `b` must be resolved.
    ///
    /// This function ensures that `a` can be extended to match `b`, but unlike unify,
    /// it leaves `a` open to extension. The key difference from unify is:
    /// - `a` can have fewer properties than `b` (subtype relationship)
    /// - `a` can be extended with additional properties
    /// - `b` must be fully resolved (no type variables)
    ///
    /// # Arguments
    ///
    /// * `a` - The type to constrain (can be extended)
    /// * `b` - The supertype constraint (must be resolved)
    ///
    /// # Returns
    ///
    /// `Ok(())` if the constraint is satisfiable, `Err` otherwise
    pub fn constrain(
        &mut self,
        a: &AbstractDopType,
        b: &ConcreteDopType,
    ) -> Result<(), String> {
        match (a, b) {
            (_, ConcreteDopType::Any) => Ok(()),
            (AbstractDopType::Bool, ConcreteDopType::Bool) => Ok(()),
            (AbstractDopType::String, ConcreteDopType::String) => Ok(()),
            (AbstractDopType::Number, ConcreteDopType::Number) => Ok(()),
            (AbstractDopType::Void, ConcreteDopType::Void) => Ok(()),
            (AbstractDopType::TypeVar(id_a), _) => {
                if let Some(substituted_type) = &self.substitutions[*id_a] {
                    self.constrain(&substituted_type.clone(), b)
                } else {
                    // For closed objects and arrays containing objects, make them open so they can be extended with more constraints
                    self.substitutions[*id_a] = Some(self.unresolve(b));
                    Ok(())
                }
            }
            (AbstractDopType::Array(type_a), ConcreteDopType::Array(type_b)) => {
                self.constrain(type_a, type_b)
            }
            (AbstractDopType::Object(props_a, rest_id), ConcreteDopType::Object(props_b)) => {
                let mut missing_props = BTreeMap::new();
                for (key, type_b) in props_b {
                    if let Some(type_a) = props_a.get(key) {
                        self.constrain(type_a, type_b)?;
                    } else {
                        missing_props.insert(key.clone(), type_b.clone());
                    }
                }
                if !missing_props.is_empty() {
                    let missing_closed = ConcreteDopType::Object(missing_props);
                    self.constrain(&AbstractDopType::TypeVar(*rest_id), &missing_closed)?;
                }

                Ok(())
            }
            _ => Err(format!(
                "Cannot constrain {} to {}",
                a, b
            )),
        }
    }

    /// Makes a constraint type extensible by converting closed objects to open objects recursively.
    fn unresolve(&mut self, constraint_type: &ConcreteDopType) -> AbstractDopType {
        match constraint_type {
            ConcreteDopType::Any => {
                // Create a new type variable for Any
                self.new_type_var()
            }
            ConcreteDopType::Object(props) => {
                // Make nested types extensible too
                let open_props: BTreeMap<String, AbstractDopType> = props
                    .iter()
                    .map(|(k, v)| (k.clone(), self.unresolve(v)))
                    .collect();
                self.new_object(open_props)
            }
            ConcreteDopType::Array(element_type) => {
                let open_element = self.unresolve(element_type);
                AbstractDopType::Array(Box::new(open_element))
            }
            ConcreteDopType::Bool => AbstractDopType::Bool,
            ConcreteDopType::String => AbstractDopType::String,
            ConcreteDopType::Number => AbstractDopType::Number,
            ConcreteDopType::Void => AbstractDopType::Void,
        }
    }

    /// Resolves a type to its concrete form.
    ///
    /// Note that the returned type will be immutable and not
    /// open to extension.
    pub fn resolve(&self, t: &AbstractDopType) -> ConcreteDopType {
        let result = match t {
            AbstractDopType::TypeVar(id) => {
                if let Some(substituted_type) = &self.substitutions[*id] {
                    self.resolve(substituted_type)
                } else {
                    ConcreteDopType::Any
                }
            }
            AbstractDopType::Array(sub_type) => {
                ConcreteDopType::Array(Box::new(self.resolve(sub_type)))
            }
            AbstractDopType::Object(props, rest_id) => {
                let resolved_props: BTreeMap<String, ConcreteDopType> = props
                    .iter()
                    .map(|(k, v)| (k.clone(), self.resolve(v)))
                    .collect();

                let rest_resolved = self.resolve(&AbstractDopType::TypeVar(*rest_id));
                match rest_resolved {
                    ConcreteDopType::Object(rest_props) => {
                        let mut merged_props = resolved_props;
                        for (k, v) in rest_props {
                            merged_props.insert(k, v);
                        }
                        ConcreteDopType::Object(merged_props)
                    }
                    ConcreteDopType::Any => ConcreteDopType::Object(resolved_props),
                    _ => panic!("Invalid type substitution for object rest"),
                }
            }
            AbstractDopType::Bool => ConcreteDopType::Bool,
            AbstractDopType::String => ConcreteDopType::String,
            AbstractDopType::Number => ConcreteDopType::Number,
            AbstractDopType::Void => ConcreteDopType::Void,
        };

        result
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
        table: &std::collections::HashMap<String, AbstractDopType>,
        unifier: &mut Unifier,
    ) -> AbstractDopType {
        match sexpr {
            SExpr::Command(cmd, args) => match cmd.as_str() {
                "array" => {
                    assert!(args.len() == 1);
                    AbstractDopType::Array(Box::new(sexpr_to_type(args[0].clone(), table, unifier)))
                }
                "object" => {
                    let mut map: BTreeMap<String, AbstractDopType> = BTreeMap::new();
                    for chunk in args.chunks(2) {
                        let value = sexpr_to_type(chunk[1].clone(), table, unifier);
                        let key = match &chunk[0] {
                            SExpr::Symbol(s) => s.clone(),
                            _ => panic!(),
                        };
                        map.insert(key, value);
                    }
                    unifier.new_object(map)
                }
                _ => panic!(),
            },
            SExpr::Symbol(str) => match str.as_str() {
                "string" => AbstractDopType::String,
                "bool" => AbstractDopType::Bool,
                "number" => AbstractDopType::Number,
                "void" => AbstractDopType::Void,
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
            let mut table: std::collections::HashMap<String, AbstractDopType> =
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
                                lines.push(err);
                            }
                        }
                        "constrain" => {
                            assert!(args.len() == 2);
                            let t1 = &sexpr_to_type(args[0].clone(), &table, &mut unifier);
                            let t2 = &sexpr_to_type(args[1].clone(), &table, &mut unifier);
                            let t2_resolved = unifier.resolve(t2);
                            if let Err(err) = unifier.constrain(t1, &t2_resolved) {
                                lines.push(err);
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
