use compiler_base::abstract_info::type_reference::TypeReference;

#[derive(Clone, Debug)]
pub enum GenericBound {
    Type(TypeReference),
}
