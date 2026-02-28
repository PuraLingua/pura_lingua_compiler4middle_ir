use ast::identifier::Identifier;
use token::{BorrowedTokenStream, Keyword, SpecialChar, TokenInfo, TokenKind};

#[derive(Debug)]
pub struct TokenCursor<'a> {
    pub tokens: BorrowedTokenStream<'a>,
    pub index: usize,
}

impl<'a> TokenCursor<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut tokens = BorrowedTokenStream::new(src, lexer::tokenize(src));
        tokens.rerender_tokens();
        tokens.filter_comments();
        tokens.filter_whitespaces();
        Self { tokens, index: 0 }
    }

    pub fn is_eof(&self) -> bool {
        self.tokens
            .tokens
            .get(self.index)
            .is_none_or(|x| x.kind == TokenKind::Eof)
    }

    pub fn current(&self) -> Option<&TokenInfo> {
        self.tokens.tokens.get(self.index)
    }
    pub fn peek(&self) -> Option<&TokenInfo> {
        self.tokens.tokens.get(self.index + 1)
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }
    pub fn consume(&mut self) -> Option<&TokenInfo> {
        self.advance();
        self.tokens.tokens.get(self.index - 1)
    }

    pub fn get_token_seq<P: FnMut(&TokenInfo) -> bool>(&self, mut predicate: P) -> &[TokenInfo] {
        let tokens = &self.tokens.tokens[self.index..];
        match tokens.iter().position(move |x| !predicate(x)) {
            Some(x) => &tokens[..x],
            None => tokens,
        }
    }

    pub fn peek_require_special_char(
        &self,
        ch: SpecialChar,
    ) -> Result<TokenInfo, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        if current.kind == ch {
            Ok(current)
        } else {
            Err(crate::ParseError::Expect(
                TokenKind::SpecialChar(ch),
                current.span,
            ))
        }
    }
    pub fn require_special_char(
        &mut self,
        ch: SpecialChar,
    ) -> Result<TokenInfo, crate::ParseError> {
        self.peek_require_special_char(ch)
            .inspect(|_| self.advance())
    }

    pub fn peek_require_keyword(&self, kw: Keyword) -> Result<TokenInfo, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        if current.kind == kw {
            Ok(current)
        } else {
            Err(crate::ParseError::Unexpect(current))
        }
    }
    pub fn require_keyword(&mut self, kw: Keyword) -> Result<TokenInfo, crate::ParseError> {
        self.peek_require_keyword(kw).inspect(|_| self.advance())
    }

    pub fn peek_statement_keyword(&self) -> Result<TokenInfo, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        if matches!(current.kind, TokenKind::StatementKeyword(_)) {
            Ok(current)
        } else {
            Err(crate::ParseError::Unexpect(current))
        }
    }
    pub fn consume_statement_keyword(&mut self) -> Result<TokenInfo, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        if matches!(current.kind, TokenKind::StatementKeyword(_)) {
            self.advance();
            Ok(current)
        } else {
            Err(crate::ParseError::Unexpect(current))
        }
    }

    pub fn peek_require_identifier(&self) -> Result<Identifier, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        match current.kind {
            token::TokenKind::Literal { kind, suffix_start } => match kind {
                token::LiteralKind::Str { terminated: true } => {
                    let s = &self.tokens.src[current.span][1..(suffix_start as usize - 1)];
                    Ok(Identifier::new(
                        compiler_base::descape::UnescapeExt::to_unescaped(s)?.into_owned(),
                    ))
                }
                token::LiteralKind::RawStr { n_hashes } => {
                    let prefix_offset = 2 + n_hashes.unwrap_or(0) as usize;
                    let until = suffix_start as usize - 1 - n_hashes.unwrap_or(0) as usize;
                    Ok(Identifier::new(
                        self.tokens.src[current.span][prefix_offset..until].to_owned(),
                    ))
                }
                _ => Err(crate::ParseError::Unexpect(current)),
            },
            token::TokenKind::Identifier => {
                Ok(Identifier::new(self.tokens.src[current.span].to_owned()))
            }
            token::TokenKind::RawIdentifier => Ok(Identifier::new(
                self.tokens.src[current.span][2..].to_owned(),
            )),
            _ => Err(crate::ParseError::Unexpect(current)),
        }
    }
    pub fn peek_require_non_literal_identifier(&self) -> Result<&'a str, crate::ParseError> {
        let current = *self.current().ok_or(crate::ParseError::UnexpectEOF)?;
        match current.kind {
            token::TokenKind::Identifier => Ok(&self.tokens.src[current.span]),
            token::TokenKind::RawIdentifier => Ok(&self.tokens.src[current.span][2..]),
            _ => Err(crate::ParseError::Unexpect(current)),
        }
    }
    pub fn require_identifier(&mut self) -> Result<Identifier, crate::ParseError> {
        self.peek_require_identifier().inspect(|_| self.advance())
    }
    pub fn require_non_literal_identifier(&mut self) -> Result<&'a str, crate::ParseError> {
        self.peek_require_non_literal_identifier()
            .inspect(|_| self.advance())
    }
}
