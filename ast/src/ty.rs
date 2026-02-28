use crate::{
    field::Field,
    method::Method,
    ty::{class::ClassDef, r#struct::StructDef},
};

pub mod class;
pub mod r#struct;

#[derive(Clone, Debug)]
pub enum TypeDef {
    Class(ClassDef),
    Struct(StructDef),
}

impl TypeDef {
    pub fn name(&self) -> &str {
        match self {
            TypeDef::Class(class_def) => class_def.name.as_ref(),
            TypeDef::Struct(struct_def) => struct_def.name.as_ref(),
        }
    }
    pub fn index(&self) -> u32 {
        match self {
            TypeDef::Class(class_def) => class_def.index,
            TypeDef::Struct(struct_def) => struct_def.index,
        }
    }
    pub fn sort_items(&mut self) {
        match self {
            TypeDef::Class(class_def) => class_def.sort_items(),
            TypeDef::Struct(struct_def) => struct_def.sort_items(),
        }
    }
    pub fn methods(&self) -> &[Method] {
        match self {
            TypeDef::Class(class_def) => &class_def.methods,
            TypeDef::Struct(struct_def) => &struct_def.methods,
        }
    }
    pub fn fields(&self) -> &[Field] {
        match self {
            TypeDef::Class(class_def) => &class_def.fields,
            TypeDef::Struct(struct_def) => &struct_def.fields,
        }
    }
}
