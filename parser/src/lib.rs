#![feature(decl_macro)]
#![feature(tuple_trait)]
#![feature(never_type)]

use ast::{File, method::MethodReference};
use compiler_base::{AnyResult, abstract_info::type_reference::TypeReference, span::Span};
use token::{LiteralKindType, SpecialChar, TokenInfo, TokenKind};

use crate::cursor_mod::TokenCursor;

pub mod cursor_mod;

pub(crate) mod field;
pub(crate) mod method;
pub(crate) mod punctuated;
pub(crate) mod ty;

#[derive(thiserror::Error, derive_more::Display, Debug)]
pub enum ParseError {
    UnexpectEOF,
    SimpleMismatch,
    Unsupported,
    VarNotFound(String),
    #[display("UnknownSuffix({_0})")]
    UnknownSuffix(String),
    #[display("AssemblyNotFound({_0})")]
    AssemblyNotFound(String),
    #[display("TypeNotFound({_0:?})")]
    TypeNotFound(TypeReference),
    #[display("FieldNotFound({_0})")]
    FieldNotFound(String),
    #[display("MethodNotFound({_0:?})")]
    MethodNotFound(MethodReference),
    OverrideUnsupportedInStructs,
    #[display("Expect({_0} at {_1})")]
    Expect(TokenKind, Span),
    #[display("ExpectMany({_0:?})")]
    ExpectMany(Vec<TokenKind>),
    #[display("ExpectLiteral({_0:?})")]
    ExpectLiteral(LiteralKindType),
    #[display("ExpectAllLiteral")]
    ExpectAllLiteral,
    #[display("ExpectLiterals({_0:?})")]
    ExpectLiterals(Vec<LiteralKindType>),
    #[display("Unexpect({_0})")]
    Unexpect(TokenInfo),
    #[display("UnexpectIdentifier({_0})")]
    UnexpectIdentifier(String),
    #[display("UnescapeFailed({_0})")]
    UnescapeFailed(compiler_base::descape::InvalidEscape),
}

impl From<!> for ParseError {
    fn from(value: !) -> Self {
        value
    }
}

impl From<compiler_base::descape::InvalidEscape> for ParseError {
    fn from(value: compiler_base::descape::InvalidEscape) -> Self {
        Self::UnescapeFailed(value)
    }
}

pub fn parse(assembly_name: String, is_header: bool, src: &str) -> AnyResult<File> {
    let mut token_cursor = TokenCursor::new(src);
    let mut file = File::new(assembly_name, is_header);

    while token_cursor
        .peek_require_special_char(SpecialChar::At)
        .is_ok()
    {
        todo!("Attributes have not been implemented yet")
    }

    while !token_cursor.is_eof() {
        file.types.push(ty::type_def(&mut token_cursor)?);
    }

    Ok(file)
}
