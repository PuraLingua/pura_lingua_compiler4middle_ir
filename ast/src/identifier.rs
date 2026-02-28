use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct Identifier {
    pub var: String,
}

impl Identifier {
    pub fn new(var: String) -> Self {
        Self { var }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <_ as std::fmt::Display>::fmt(&self.var, f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.var
    }
}

impl<T: AsRef<str> + ?Sized> PartialEq<T> for Identifier {
    fn eq(&self, other: &T) -> bool {
        self.var.eq(other.as_ref())
    }
}
