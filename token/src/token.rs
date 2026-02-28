#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(const_convert)]
#![feature(const_cmp)]
#![feature(formatting_options)]
#![feature(debug_closure_helpers)]
#![feature(derive_const)]
#![feature(core_intrinsics)]
#![feature(const_clone)]
#![feature(decl_macro)]
// #![feature(slice_as_array)]
#![feature(const_default)]
#![allow(internal_features)]

#[derive(Debug, Clone)]
pub struct BorrowedTokenStream<'a> {
    pub src: &'a str,
    pub tokens: Vec<TokenInfo>,
}

impl<'a> BorrowedTokenStream<'a> {
    /// Map Identifier to Keyword if possible
    pub fn rerender_tokens(&mut self) {
        for tok in self.tokens.iter_mut() {
            if matches!(tok.kind, TokenKind::Identifier) {
                if let Ok(kw) = Keyword::try_from(&self.src[tok.span.into_range()]) {
                    tok.kind = TokenKind::Keyword(kw);
                } else if let Ok(kw) = StatementKeyword::try_from(&self.src[tok.span.into_range()])
                {
                    tok.kind = TokenKind::StatementKeyword(kw);
                }
            }
        }
    }

    pub fn filter_whitespaces(&mut self) {
        self.tokens.retain(|x| x.kind != TokenKind::WhiteSpace);
    }

    pub fn filter_comments(&mut self) {
        self.tokens
            .retain(|x| !matches!(x.kind, TokenKind::Comment(_)));
    }

    /// Return indexes that are invalid
    pub fn get_all_invalid(&self) -> Vec<usize> {
        self.tokens
            .iter()
            .enumerate()
            .filter_map(|(i, tok)| match tok.kind {
                TokenKind::Literal {
                    kind,
                    suffix_start: _,
                } => match kind {
                    LiteralKind::Char { terminated }
                    | LiteralKind::Byte { terminated }
                    | LiteralKind::Str { terminated }
                    | LiteralKind::ByteStr { terminated }
                        if !terminated =>
                    {
                        Some(i)
                    }
                    _ => None,
                },
                TokenKind::InvalidIdentifier | TokenKind::Unknown => Some(i),
                _ => None,
            })
            .collect()
    }
}

impl<'a> BorrowedTokenStream<'a> {
    pub fn new(s: &'a str, tokens: impl Iterator<Item = TokenInfo>) -> Self {
        let tokens = tokens.collect();
        Self { src: s, tokens }
    }
}

impl Display for BorrowedTokenStream<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.with_options(*FormattingOptions::new().alternate(true));
        let mut debugger = f.debug_list();
        let mut debugger = &mut debugger;
        for t in &self.tokens {
            debugger = debugger.entry_with(|ff| {
                ff.debug_struct("Token")
                    .field("kind", &t.kind)
                    .field("span", &t.span)
                    .field("source", &&self.src[t.span.into_range()])
                    .finish()
            });
        }

        debugger.finish()
    }
}

mod comment;
mod fmt_with_src_impl;
pub mod keyword;
mod literal;
pub mod special_char;
use std::{
    borrow::Cow,
    fmt::{Display, FormattingOptions},
};

pub use comment::*;
use compiler_base::{global::UnwrapEnum, span::Span};
pub use fmt_with_src_impl::*;
pub use keyword::Keyword;
use konst::const_panic::PanicFmt;
pub use literal::*;
pub use special_char::SpecialChar;

use crate::keyword::StatementKeyword;

pub const trait IToken {
    fn kind(&self) -> &crate::TokenKind;
    fn span(&self) -> &Span;
}

impl const IToken for TokenInfo {
    fn kind(&self) -> &crate::TokenKind {
        &self.kind
    }
    fn span(&self) -> &Span {
        &self.span
    }
}

#[allow(unused)]
#[derive(Debug, Copy, PanicFmt, Hash, UnwrapEnum)]
#[derive_const(Clone, PartialEq, Eq)]
#[pfmt(crate = konst::const_panic)]
#[unwrap_enum(owned)]
pub enum TokenKind {
    Keyword(Keyword),
    StatementKeyword(StatementKeyword),
    SpecialChar(SpecialChar),
    Comment(Comment),
    Literal {
        kind: LiteralKind,
        suffix_start: u32,
    },
    GuardedStrPrefix,

    /// Maybe including keywords
    Identifier,
    InvalidIdentifier,
    RawIdentifier,
    UnknownPrefix,

    WhiteSpace,
    Unknown,
    Eof,
}

impl TokenKind {
    pub fn get_back_pair(&self) -> Option<TokenKind> {
        if let TokenKind::SpecialChar(special_char) = self {
            special_char.get_back_pair().map(TokenKind::SpecialChar)
        } else {
            None
        }
    }

    pub fn get_front_pair(&self) -> Option<Self> {
        if let TokenKind::SpecialChar(special_char) = self {
            special_char.get_front_pair().map(TokenKind::SpecialChar)
        } else {
            None
        }
    }
}

impl const PartialEq<SpecialChar> for TokenKind {
    fn eq(&self, other: &SpecialChar) -> bool {
        match self {
            Self::SpecialChar(this) => this.eq(other),
            _ => false,
        }
    }
}

impl const PartialEq<Keyword> for TokenKind {
    fn eq(&self, other: &Keyword) -> bool {
        match self {
            Self::Keyword(this) => this.eq(other),
            _ => false,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Keyword(keyword) => Display::fmt(keyword, f),
            TokenKind::StatementKeyword(keyword) => Display::fmt(keyword, f),
            TokenKind::SpecialChar(special_char) => Display::fmt(special_char, f),
            TokenKind::Comment(comment) => Display::fmt(comment, f),

            TokenKind::Literal { kind, suffix_start } => {
                write!(f, "Literal({}, {})", kind, suffix_start)
            }
            TokenKind::GuardedStrPrefix => write!(f, "GuardedStrPrefix"),

            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::InvalidIdentifier => write!(f, "InvalidIdentifier"),
            TokenKind::RawIdentifier => write!(f, "RawIdentifier"),

            TokenKind::UnknownPrefix => write!(f, "UnknownPrefix"),
            TokenKind::WhiteSpace => write!(f, "WhiteSpace"),
            TokenKind::Unknown => write!(f, "Unknown"),
            TokenKind::Eof => write!(f, "Eof"),
        }
    }
}

impl TokenKind {
    pub const fn from_keyword_array<const N: usize>(keywords: [Keyword; N]) -> [Self; N] {
        use konst::array::ArrayBuilder;
        let mut array_builder = ArrayBuilder::of_copy();
        konst::iter::for_each! {
            kw in &keywords => {
                array_builder.push(Self::Keyword(*kw));
            }
        }

        array_builder.build()
    }

    pub const fn from_special_char_array<const N: usize>(chars: [SpecialChar; N]) -> [Self; N] {
        use konst::array::ArrayBuilder;
        let mut array_builder = ArrayBuilder::of_copy();
        konst::iter::for_each! {
            c in &chars => {
                array_builder.push(Self::SpecialChar(*c));
            }
        }

        array_builder.build()
    }
}

#[derive(Debug, Copy, Hash)]
#[derive_const(Clone, PartialEq, Eq)]
pub struct TokenInfo {
    pub kind: TokenKind,
    pub span: Span,
}

impl TokenInfo {
    pub fn is_int(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Literal {
                kind: LiteralKind::Int {
                    base: _,
                    empty_int: false,
                },
                suffix_start: _,
            }
        )
    }
    pub fn get_as_int(&self, src: &str) -> Option<u64> {
        let TokenKind::Literal {
            kind:
                LiteralKind::Int {
                    base,
                    empty_int: false,
                },
            suffix_start,
        } = self.kind
        else {
            return None;
        };
        let value_s = &src[self.span][..(suffix_start as usize)];
        match base {
            Base::Binary => u64::from_str_radix(&value_s[2..], 2).ok(),
            Base::Octal => u64::from_str_radix(&value_s[2..], 8).ok(),
            Base::Decimal => value_s.parse::<u64>().ok(),
            Base::Hexadecimal => u64::from_str_radix(&value_s[2..], 16).ok(),
        }
    }

    pub fn get_as_string<'a>(&self, src: &'a str) -> Option<Cow<'a, str>> {
        match self.kind {
            TokenKind::Literal { kind, suffix_start } => match kind {
                LiteralKind::Str { terminated } => {
                    assert!(terminated);
                    let s = &src[self.span][1..(suffix_start as usize - 1)];
                    compiler_base::descape::UnescapeExt::to_unescaped(s).ok()
                }
                LiteralKind::RawStr { n_hashes } => {
                    let prefix_offset = 2 + n_hashes.unwrap_or(0) as usize;
                    let until = suffix_start as usize - 1 - n_hashes.unwrap_or(0) as usize;
                    Some(Cow::Borrowed(&src[self.span][prefix_offset..until]))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}`({})", self.kind, self.span)
    }
}

impl TokenInfo {
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

trait TokenSealed {}

#[allow(private_bounds)]
pub trait TokenName: TokenSealed {
    const TOKEN_NAME: &str;
}

macro impl_token_info_alias($i:ident) {
    impl const ::core::convert::From<$crate::TokenInfo> for $i {
        fn from(v: $crate::TokenInfo) -> Self {
            $i(v)
        }
    }

    impl ::std::fmt::Debug for $i {
        fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
            <$crate::TokenInfo as ::std::fmt::Debug>::fmt(&self.0, f)
        }
    }

    impl ::std::fmt::Display for $i {
        fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
            <$crate::TokenInfo as ::std::fmt::Display>::fmt(&self.0, f)
        }
    }

    impl const $crate::IToken for $i {
        fn kind(&self) -> &$crate::TokenKind {
            &self.0.kind
        }
        fn span(&self) -> &compiler_base::span::Span {
            &self.0.span
        }
    }

    impl fmt_with_src::DebugWithSrc for $i {
        type Debug<'a> = $crate::fmt_with_src_impl::TokenDebug<'a, $i>;
        fn debug<'a>(&'a self, src: &'a str) -> Self::Debug<'a> {
            $crate::fmt_with_src_impl::TokenDebug(self, src)
        }
    }

    impl fmt_with_src::DisplayWithSrc for $i {
        type Display<'a> = $crate::fmt_with_src_impl::TokenDisplay<'a, $i>;
        fn display<'a>(&'a self, src: &'a str) -> Self::Display<'a> {
            $crate::fmt_with_src_impl::TokenDisplay(self, src)
        }
    }

    impl $crate::TokenSealed for $i {}

    impl $crate::TokenName for $i {
        const TOKEN_NAME: &str = stringify!($i);
    }
}
