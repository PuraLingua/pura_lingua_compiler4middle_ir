use std::fmt::{Debug, Display};

use fmt_with_src::{DebugWithSrc, DisplayWithSrc};

use crate::{IToken, TokenInfo};

#[doc(hidden)]
pub struct TokenDebug<'a, T = TokenInfo>(pub &'a T, pub &'a str);
#[doc(hidden)]
pub struct TokenDisplay<'a, T = TokenInfo>(pub &'a T, pub &'a str);

impl<'a, T: IToken> Debug for TokenDebug<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenInfo")
            .field("kind", self.0.kind())
            .field("span", &self.0.span())
            .field("src", &self.1.get(self.0.span().into_range()))
            .finish()
    }
}

impl<'a, T: IToken> Display for TokenDisplay<'a, T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        write!(
            f,
            "{}({}, {}, {})",
            stringify!($i),
            self.0.kind(),
            self.0.span(),
            self.1.get(self.0.span().into_range()).unwrap_or_default(),
        )
    }
}

impl DebugWithSrc for TokenInfo {
    type Debug<'a>
        = TokenDebug<'a>
    where
        Self: 'a;

    fn debug<'a>(&'a self, src: &'a str) -> Self::Debug<'a> {
        TokenDebug(self, src)
    }
}

impl DisplayWithSrc for TokenInfo {
    type Display<'a>
        = TokenDisplay<'a>
    where
        Self: 'a;

    fn display<'a>(&'a self, src: &'a str) -> Self::Display<'a> {
        TokenDisplay(self, src)
    }
}
