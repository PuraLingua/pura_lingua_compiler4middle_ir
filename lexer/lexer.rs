#![feature(formatting_options)]
#![feature(debug_closure_helpers)]
#![feature(iter_advance_by)]
#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(const_convert)]
#![feature(const_cmp)]
#![feature(derive_const)]
#![feature(const_clone)]

use std::str::Chars;

use compiler_base::memchr;

#[cfg(test)]
mod tests;
mod token;

use compiler_base::span::{LineCol, Span};
#[allow(unused_imports)]
pub use token::*;

pub(crate) const EOF_CHAR: char = '\0';

pub struct Tokenizer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = TokenInfo;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    }
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = TokenInfo> {
    let cursor = Cursor::new(input);
    Tokenizer { cursor }
}

pub struct Cursor<'a> {
    total_str: &'a str,
    len_remaining: usize,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
    #[cfg(debug_assertions)]
    prev: char,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            total_str: input,
            len_remaining: input.len(),
            chars: input.chars(),
            #[cfg(debug_assertions)]
            prev: EOF_CHAR,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    /// Returns the last eaten symbol (or `'\0'` in release builds).
    /// (For debug assertions only.)
    pub(crate) fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }

        #[cfg(not(debug_assertions))]
        {
            EOF_CHAR
        }
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub fn first(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peeks the second symbol from the input stream without consuming it.
    pub(crate) fn second(&self) -> char {
        // `.next()` optimizes better than `.nth(1)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Peeks the third symbol from the input stream without consuming it.
    pub fn third(&self) -> char {
        // `.next()` optimizes better than `.nth(2)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub(crate) fn pos(&self) -> usize {
        self.total_str.len() - self.chars.as_str().len()
    }

    pub(crate) fn get_token(&self, prev_pos: usize, token_kind: TokenKind) -> TokenInfo {
        TokenInfo::new(
            token_kind,
            Span::new(
                LineCol::of(self.total_str, prev_pos),
                self.current_line_col(),
            ),
        )
    }

    pub(crate) fn current_line_col(&self) -> LineCol {
        LineCol::of(self.total_str, self.pos())
    }

    /// Returns amount of already consumed symbols.
    pub(crate) fn pos_within_token(&self) -> u32 {
        (self.len_remaining - self.chars.as_str().len()) as u32
    }

    /// Resets the number of bytes consumed to 0.
    pub(crate) fn reset_pos_within_token(&mut self) {
        self.len_remaining = self.chars.as_str().len();
    }

    /// Moves to the next character.
    pub(crate) fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        Some(c)
    }

    /// Moves to a substring by a number of bytes.
    #[allow(unused)]
    pub(crate) fn bump_bytes(&mut self, n: usize) {
        self.chars = self.as_str()[n..].chars();
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        // It was tried making optimized version of this for eg. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    pub(crate) fn eat_until(&mut self, byte: u8) {
        self.chars = match memchr::memchr(byte, self.as_str().as_bytes()) {
            Some(index) => self.as_str()[index..].chars(),
            None => "".chars(),
        }
    }
}
