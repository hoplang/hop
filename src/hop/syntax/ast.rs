use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::ParseTree;
use crate::dop::ParsedType;
use crate::dop::VarName;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::node::Node;

/// A Parameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct Parameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    Expressions(Vec<ParseTree>),
    String(DocumentRange),
}

/// An Attribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: DocumentRange,
    pub value: Option<AttributeValue>,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub name: ModuleName,
    imports: Vec<Import>,
    records: Vec<Record>,
    enums: Vec<Enum>,
    component_definitions: Vec<ComponentDefinition>,
}

impl Ast {
    pub fn new(
        name: ModuleName,
        component_definitions: Vec<ComponentDefinition>,
        imports: Vec<Import>,
        records: Vec<Record>,
        enums: Vec<Enum>,
    ) -> Self {
        Self {
            name,
            component_definitions,
            imports,
            records,
            enums,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&ComponentDefinition> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Finds a record declaration by name.
    pub fn get_record(&self, name: &str) -> Option<&Record> {
        self.records.iter().find(|&r| r.name() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[ComponentDefinition] {
        &self.component_definitions
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[Record] {
        &self.records
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[Enum] {
        &self.enums
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &Node> {
        self.component_definitions
            .iter()
            .flat_map(|n| &n.children)
            .flat_map(|n| n.iter_depth_first())
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub type_name: TypeName,
    /// The range of the type name in the source (for error reporting)
    pub type_name_range: DocumentRange,
    /// The full path range for error reporting (covers module::TypeName)
    pub path: DocumentRange,
    pub module_name: ModuleName,
}

impl Import {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_type_name(&self) -> &TypeName {
        &self.type_name
    }
    pub fn type_name_range(&self) -> &DocumentRange {
        &self.type_name_range
    }
    pub fn imports_type(&self, type_name: &str) -> bool {
        self.type_name.as_str() == type_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: FieldName,
    pub field_type: ParsedType,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<RecordField>,
}

impl Record {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: TypeName,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub variants: Vec<EnumVariant>,
}

impl Enum {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct ComponentDefinition {
    pub component_name: ComponentName,
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<Parameter>, DocumentRange)>,
    pub children: Vec<Node>,
    pub range: DocumentRange,
}

impl Ranged for ComponentDefinition {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl ComponentDefinition {
    pub fn tag_name_ranges(&self) -> impl Iterator<Item = &DocumentRange> {
        self.closing_tag_name.iter().chain(Some(&self.tag_name))
    }
}
