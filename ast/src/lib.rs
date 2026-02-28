#![feature(impl_trait_in_assoc_type)]
#![feature(super_let)]
#![feature(debug_closure_helpers)]
#![feature(never_type)]

use crate::ty::TypeDef;

pub mod field;
pub mod generics;
pub mod identifier;
pub mod method;
pub mod punctuated;
pub mod ty;

#[derive(Clone, Debug)]
pub struct File {
    pub assembly_name: String,
    pub is_header: bool,
    pub attrs: Vec<!>, // todo
    pub types: Vec<TypeDef>,
}

impl File {
    pub fn new(assembly_name: String, is_header: bool) -> Self {
        Self {
            assembly_name,
            is_header,
            attrs: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn merge(&mut self, mut other: Self) {
        self.attrs.append(&mut other.attrs);
        self.types.append(&mut other.types);
    }

    pub fn sort(&mut self) {
        self.types.sort_by_key(|x| x.index());
        for ty in &mut self.types {
            ty.sort_items();
        }
    }
}
