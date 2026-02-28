//! Some are copied from [rustc-lexer](https://github.com/rust-lang/rust/tree/master/compiler/rustc_lexer)

use compiler_base::{
    global::d_println,
    span::{LineCol, Span},
    unicode_properties::UnicodeEmoji,
    unicode_xid::UnicodeXID,
};
use konst::const_panic::PanicFmt;

use crate::{Cursor, EOF_CHAR};

pub use token_info::*;

#[derive(
    Copy, Debug, PartialEq, Eq, PartialOrd, Ord, PanicFmt, derive_more::Display, thiserror::Error,
)]
#[derive_const(Clone)]
#[pfmt(crate = konst::const_panic)]
pub enum RawStrError {
    /// Non `#` characters exist between `r` and `"`, e.g. `r##~"abcde"##`
    #[display("InvalidStarter {{ bad_char: {bad_char} }}")]
    InvalidStarter { bad_char: char },
    /// The string was not terminated, e.g. `r###"abcde"##`.
    /// `possible_terminator_offset` is the number of characters after `r` or
    /// `br` where they may have intended to terminate it.
    #[display(
        "Unterminated {{ expected: {expected}, found: {found}, possible_terminator_offset: {} }}",
        possible_terminator_offset.as_ref().map(|x| x.to_string()).unwrap_or("None".to_owned())
    )]
    NoTerminator {
        expected: u32,
        found: u32,
        possible_terminator_offset: Option<u32>,
    },

    /// More than 255 `#`s exist.
    #[display("TooManyDelimiters {{ found: {found} }}")]
    TooManyDelimiters { found: u32 },
}

/// Validates a raw string literal. Used for getting more information about a
/// problem with a `RawStr`/`RawByteStr` with a `None` field.
#[inline]
pub fn validate_raw_str(input: &str, prefix_len: u32) -> Result<(), RawStrError> {
    debug_assert!(!input.is_empty());
    let mut cursor = Cursor::new(input);
    // Move past the leading `r` or `br`.
    for _ in 0..prefix_len {
        cursor.bump().unwrap();
    }
    cursor.raw_double_quoted_string(prefix_len).map(|_| ())
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    c == '_' || UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_continue(c: char) -> bool {
    UnicodeXID::is_xid_continue(c)
}

impl<'a> Cursor<'a> {
    pub fn advance_token(&mut self) -> TokenInfo {
        let prev_pos = self.pos();
        let Some(first_char) = self.bump() else {
            return TokenInfo::new(TokenKind::Eof, Span::default());
        };

        let token_kind = match first_char {
            c if is_whitespace(c) => self.whitespace(),
            '/' => match self.first() {
                '/' => TokenKind::Comment(self.line_comment()),
                '*' => TokenKind::Comment(self.block_comment()),
                _ => TokenKind::SpecialChar(SpecialChar::Solidus),
            },

            // Raw identifier, raw string literal or identifier.
            'r' => match (self.first(), self.second()) {
                ('#', c1) if is_id_start(c1) => self.raw_identifier(),
                ('#', _) | ('"', _) => {
                    let res = self.raw_double_quoted_string(1);
                    let suffix_start = self.pos_within_token();
                    if res.is_ok() {
                        self.eat_literal_suffix();
                    }
                    let kind = LiteralKind::RawStr { n_hashes: res.ok() };
                    TokenKind::Literal { kind, suffix_start }
                }
                _ => self.identifier_or_unknown_prefix(),
            },

            // Byte literal, byte string literal, raw byte string literal or identifier.
            'b' => self.byte_string(
                |terminated| LiteralKind::ByteStr { terminated },
                |n_hashes| LiteralKind::RawByteStr { n_hashes },
                Some(|terminated| LiteralKind::Byte { terminated }),
            ),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.identifier_or_unknown_prefix(),

            // Numeric literal.
            c @ '0'..='9' => {
                let literal_kind = self.number(c);
                let suffix_start = self.pos_within_token();
                self.eat_literal_suffix();
                TokenKind::Literal {
                    kind: literal_kind,
                    suffix_start,
                }
            }

            // Guarded string literal prefix: `#"` or `##`
            '#' if matches!(self.first(), '"' | '#') => {
                self.bump();
                TokenKind::GuardedStrPrefix
            }

            // Character literal.
            '\'' => self.char(),

            '"' => {
                let terminated = self.double_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_literal_suffix();
                }
                let kind = LiteralKind::Str { terminated };
                TokenKind::Literal { kind, suffix_start }
            }

            _ if let Some(c) = SpecialChar::parse_prefix(&self.total_str[prev_pos..]) => {
                let _ = self.chars.advance_by(c.as_str().len() - 1);

                TokenKind::SpecialChar(c)
            }

            c if !c.is_ascii() && c.is_emoji_char() => self.invalid_identifier(),
            EOF_CHAR => TokenKind::Eof,
            _ => {
                d_println!(
                    "UNKNOWN CHAR: `{first_char}` at {}(i.e. {prev_pos})",
                    LineCol::of(self.total_str, prev_pos)
                );
                TokenKind::Unknown
            }
        };
        let res = self.get_token(prev_pos, token_kind);
        self.reset_pos_within_token();
        res
    }

    fn line_comment(&mut self) -> Comment {
        debug_assert!(self.prev() == '/' && self.first() == '/');
        self.bump();

        let doc_style = match self.first() {
            // `//!` is an inner line doc comment.
            '!' => Some(DocStyle::Inner),
            // `////` (more than 3 slashes) is not considered a doc comment.
            '/' if self.second() != '/' => Some(DocStyle::Outer),
            _ => None,
        };

        self.eat_until(b'\n');
        Comment::Line(doc_style)
    }

    fn block_comment(&mut self) -> Comment {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();

        let doc_style = match self.first() {
            // `/*!` is an inner block doc comment.
            '!' => Some(DocStyle::Inner),
            // `/***` (more than 2 stars) is not considered a doc comment.
            // `/**/` is not considered a doc comment.
            '*' if !matches!(self.second(), '*' | '/') => Some(DocStyle::Outer),
            _ => None,
        };

        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                _ => (),
            }
        }

        Comment::Block {
            style: doc_style,
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        // the prev char is pushed by the caller.
        self.eat_while(is_whitespace);
        TokenKind::WhiteSpace
    }

    fn raw_identifier(&mut self) -> TokenKind {
        debug_assert!(self.prev() == 'r' && self.first() == '#' && is_id_start(self.second()));
        // Eat "#" symbol.
        self.bump();
        // Eat the identifier part of RawIdent.
        self.eat_identifier();
        TokenKind::RawIdentifier
    }

    fn identifier_or_unknown_prefix(&mut self) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        match self.first() {
            '#' | '"' | '\'' => TokenKind::UnknownPrefix,
            c if !c.is_ascii() && c.is_emoji_char() => self.invalid_identifier(),
            _ => TokenKind::Identifier,
        }
    }

    fn invalid_identifier(&mut self) -> TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(|c| {
            const ZERO_WIDTH_JOINER: char = '\u{200d}';
            is_id_continue(c) || (!c.is_ascii() && c.is_emoji_char()) || c == ZERO_WIDTH_JOINER
        });
        // An invalid identifier followed by '#' or '"' or '\'' could be
        // interpreted as an invalid literal prefix. We don't bother doing that
        // because the treatment of invalid identifiers and invalid prefixes
        // would be the same.
        TokenKind::InvalidIdentifier
    }

    fn byte_string(
        &mut self,
        make_kind: fn(bool) -> LiteralKind,
        make_kind_raw: fn(Option<u8>) -> LiteralKind,
        single_quoted: Option<fn(bool) -> LiteralKind>,
    ) -> TokenKind {
        match (self.first(), self.second(), single_quoted) {
            ('\'', _, Some(single_quoted)) => {
                self.bump();
                let terminated = self.single_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_literal_suffix();
                }
                let kind = single_quoted(terminated);
                TokenKind::Literal { kind, suffix_start }
            }
            ('"', _, _) => {
                self.bump();
                let terminated = self.double_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_literal_suffix();
                }
                let kind = make_kind(terminated);
                TokenKind::Literal { kind, suffix_start }
            }
            ('r', '"', _) | ('r', '#', _) => {
                self.bump();
                let res = self.raw_double_quoted_string(2);
                let suffix_start = self.pos_within_token();
                if res.is_ok() {
                    self.eat_literal_suffix();
                }
                let kind = make_kind_raw(res.ok());
                TokenKind::Literal { kind, suffix_start }
            }
            _ => self.identifier_or_unknown_prefix(),
        }
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    if !self.eat_hexadecimal_digits() {
                        return LiteralKind::Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                // Not a base prefix; consume additional digits.
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                // Also not a base prefix; nothing more to do here.
                '.' | 'e' | 'E' => {}

                // Just a 0.
                _ => {
                    return LiteralKind::Int {
                        base,
                        empty_int: false,
                    };
                }
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        }

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            '.' if self.second() != '.' && !is_id_start(self.second()) => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.bump();
                let mut empty_exponent = false;
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                LiteralKind::Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                LiteralKind::Float {
                    base,
                    empty_exponent,
                }
            }
            _ => LiteralKind::Int {
                base,
                empty_int: false,
            },
        }
    }

    fn char(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '\'');

        let terminated = self.single_quoted_string();
        let suffix_start = self.pos_within_token();
        if terminated {
            self.eat_literal_suffix();
        }
        let kind = LiteralKind::Char { terminated };
        TokenKind::Literal { kind, suffix_start }
    }

    fn single_quoted_string(&mut self) -> bool {
        debug_assert!(self.prev() == '\'');
        // Check if it's a one-symbol literal.
        if self.second() == '\'' && self.first() != '\\' {
            self.bump();
            self.bump();
            return true;
        }

        // Literal has more than one symbol.

        // Parse until either quotes are terminated or error is detected.
        loop {
            match self.first() {
                // Quotes are terminated, finish parsing.
                '\'' => {
                    self.bump();
                    return true;
                }
                // Probably beginning of the comment, which we don't want to include
                // to the error report.
                '/' => break,
                // Newline without following '\'' means unclosed quote, stop parsing.
                '\n' if self.second() != '\'' => break,
                // End of file, stop parsing.
                #[allow(unused_variables)]
                EOF_CHAR if self.is_eof() => break,
                // Escaped slash is considered one character, so bump twice.
                '\\' => {
                    self.bump();
                    self.bump();
                }
                // Skip the character.
                _ => {
                    self.bump();
                }
            }
        }
        // String was not terminated.
        false
    }

    /// Eats double-quoted string and returns true
    /// if string is terminated.
    fn double_quoted_string(&mut self) -> bool {
        debug_assert!(self.prev() == '"');
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character.
                    self.bump();
                }
                _ => (),
            }
        }
        // End of file reached.
        false
    }

    /// Attempt to lex for a guarded string literal.
    ///
    /// Used by `rustc_parse::lexer` to lex for guarded strings
    /// conditionally based on edition.
    ///
    /// Note: this will not reset the `Cursor` when a
    /// guarded string is not found. It is the caller's
    /// responsibility to do so.
    pub fn guarded_double_quoted_string(&mut self) -> Option<GuardedStr> {
        debug_assert!(self.prev() != '#');

        let mut n_start_hashes: u32 = 0;
        while self.first() == '#' {
            n_start_hashes += 1;
            self.bump();
        }

        if self.first() != '"' {
            return None;
        }
        self.bump();
        debug_assert!(self.prev() == '"');

        // Lex the string itself as a normal string literal
        // so we can recover that for older editions later.
        let terminated = self.double_quoted_string();
        if !terminated {
            let token_len = self.pos_within_token();
            self.reset_pos_within_token();

            return Some(GuardedStr {
                n_hashes: n_start_hashes,
                terminated: false,
                token_len,
            });
        }

        // Consume closing '#' symbols.
        // Note that this will not consume extra trailing `#` characters:
        // `###"abcde"####` is lexed as a `GuardedStr { n_end_hashes: 3, .. }`
        // followed by a `#` token.
        let mut n_end_hashes = 0;
        while self.first() == '#' && n_end_hashes < n_start_hashes {
            n_end_hashes += 1;
            self.bump();
        }

        // Reserved syntax, always an error, so it doesn't matter if
        // `n_start_hashes != n_end_hashes`.

        self.eat_literal_suffix();

        let token_len = self.pos_within_token();
        self.reset_pos_within_token();

        Some(GuardedStr {
            n_hashes: n_start_hashes,
            terminated: true,
            token_len,
        })
    }

    /// Eats the double-quoted string and returns `n_hashes` and an error if encountered.
    fn raw_double_quoted_string(&mut self, prefix_len: u32) -> Result<u8, RawStrError> {
        // Wrap the actual function to handle the error with too many hashes.
        // This way, it eats the whole raw string.
        let n_hashes = self.raw_string_unvalidated(prefix_len)?;
        // Only up to 255 `#`s are allowed in raw strings
        match u8::try_from(n_hashes) {
            Ok(num) => Ok(num),
            Err(_) => Err(RawStrError::TooManyDelimiters { found: n_hashes }),
        }
    }

    fn raw_string_unvalidated(&mut self, prefix_len: u32) -> Result<u32, RawStrError> {
        debug_assert!(self.prev() == 'r');
        let start_pos = self.pos_within_token();
        let mut possible_terminator_offset = None;
        let mut max_hashes = 0;

        // Count opening '#' symbols.
        let mut eaten = 0;
        while self.first() == '#' {
            eaten += 1;

            // This fucking statement is used to avoid moving.
            self.bump();
        }

        let n_start_hashes = eaten;

        // Check that string is started.
        match self.bump() {
            Some('"') => (),
            c => {
                let c = c.unwrap_or(EOF_CHAR);
                return Err(RawStrError::InvalidStarter { bad_char: c });
            }
        }

        // Skip the string contents and on each '#' character met, check if this is
        // a raw string termination.
        loop {
            self.eat_until(b'"');

            if self.is_eof() {
                return Err(RawStrError::NoTerminator {
                    expected: n_start_hashes,
                    found: max_hashes,
                    possible_terminator_offset,
                });
            }

            // Eat closing double quote.
            self.bump();

            // Check that amount of closing '#' symbols
            // is equal to the amount of opening ones.
            // Note that this will not consume extra trailing `#` characters:
            // `r###"abcde"####` is lexed as a `RawStr { n_hashes: 3 }`
            // followed by a `#` token.
            let mut n_end_hashes = 0;
            while self.first() == '#' && n_end_hashes < n_start_hashes {
                n_end_hashes += 1;
                self.bump();
            }

            if n_end_hashes == n_start_hashes {
                return Ok(n_start_hashes);
            } else if n_end_hashes > max_hashes {
                // Keep track of possible terminators to give a hint about
                // where there might be a missing terminator
                possible_terminator_offset =
                    Some(self.pos_within_token() - start_pos - n_end_hashes + prefix_len);
                max_hashes = n_end_hashes;
            }
        }
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }

    // Eats the suffix of the literal, e.g. "u8".
    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }

    // Eats the identifier. Note: succeeds on `_`, which isn't a valid
    // identifier.
    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_id_continue);
    }
}
