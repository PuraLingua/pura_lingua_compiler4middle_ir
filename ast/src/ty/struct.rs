use std::collections::HashMap;

use compiler_base::global::attrs::TypeAttr;

use crate::{field::Field, generics::GenericBound, identifier::Identifier, method::Method};

#[derive(Clone, Debug)]
pub enum StructItem {
    Method(Method),
    Field(Field),
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub attr: TypeAttr,
    pub index: u32,
    pub name: Identifier,
    pub generics: Vec<Identifier>,
    pub is_generic_infinite: bool,
    pub generic_bounds: HashMap<Identifier, Vec<GenericBound>>,
    pub methods: Vec<Method>,
    pub fields: Vec<Field>,
}

impl StructDef {
    pub fn sort_items(&mut self) {
        self.methods.sort_by_key(|x| x.index);
        self.fields.sort_by_key(|x| x.index);
    }
}
