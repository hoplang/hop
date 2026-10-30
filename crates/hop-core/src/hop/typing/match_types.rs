use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

pub trait MatchTypes {
    type Type: Clone;

    fn shape<'a>(&'a self, typ: &'a Self::Type) -> TypeShape<'a, Self::Type>;
}

pub enum TypeShape<'a, T> {
    Bool,
    Option(&'a T),
    Tuple(&'a [T]),
    Record {
        name: &'a TypeName,
        fields: Vec<(&'a FieldName, &'a T)>,
    },
    Enum {
        name: &'a TypeName,
        variants: Vec<(&'a TypeName, Vec<(&'a FieldName, &'a T)>)>,
    },
    Unmatchable,
}
