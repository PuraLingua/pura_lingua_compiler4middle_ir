use std::collections::HashMap;

use compiler_base::{
    abstract_info::type_reference::TypeReference,
    global::attrs::{CallConvention, MethodAttr, ParameterAttr},
};
use either::Either;

use crate::{generics::GenericBound, identifier::Identifier};

mod statement;
pub use statement::*;

#[derive(Clone, Debug)]
pub struct MethodReference {
    pub index: Either<u32, Identifier>,
    pub generics: Vec<TypeReference>,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub attr: MethodAttr<TypeReference>,
    pub call_conv: CallConvention,
    pub index: u32,
    pub name: Identifier,
    pub generics: Vec<Identifier>,
    pub is_generic_infinite: bool,
    pub args: Vec<Parameter>,
    pub return_type: TypeReference,
    pub locals: Vec<Identifier>,
    pub generic_bounds: HashMap<String, Vec<GenericBound>>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub attr: ParameterAttr,
    pub ty: TypeReference,
}
