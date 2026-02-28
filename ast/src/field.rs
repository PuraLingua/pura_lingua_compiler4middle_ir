use compiler_base::{abstract_info::type_reference::TypeReference, global::attrs::FieldAttr};
use either::Either;

use crate::identifier::Identifier;

#[derive(Clone, Debug)]
pub struct Field {
    pub attr: FieldAttr,
    pub index: u32,
    pub name: Identifier,
    pub ty: TypeReference,
}

pub type FieldRef = Either<u32, Identifier>;
