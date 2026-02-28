use ast::{field::Field, identifier::Identifier};
use compiler_base::{
    AnyResult,
    global::attrs::{FieldAttr, FieldImplementationFlags, Visibility},
};
use enumflags2::BitFlags;
use token::{Keyword, LiteralKind, SpecialChar, TokenInfo, TokenKind};

use crate::{ParseError, cursor_mod::TokenCursor};

pub fn field(cursor: &mut TokenCursor, generics: &[Identifier]) -> AnyResult<Field> {
    assert!(cursor.consume().is_some_and(|x| x.kind == Keyword::Field));

    cursor.require_special_char(SpecialChar::BracketOpen)?;
    let attr_tokens = cursor.get_token_seq(|x| {
        matches!(x.kind, TokenKind::Keyword(a) if a.is_field_modifier()) || x.is_int()
    });
    let (attr, index) = field_attr(attr_tokens, cursor.tokens.src)?;
    cursor.index += attr_tokens.len();
    cursor.require_special_char(SpecialChar::BracketClose)?;

    let name = cursor.require_identifier()?;

    cursor.require_special_char(SpecialChar::Colon)?;

    let ty = crate::ty::type_reference(cursor, generics, &[])?;

    cursor.require_special_char(SpecialChar::Semicolon)?;

    Ok(Field {
        attr,
        index,
        name,
        ty,
    })
}

fn field_attr(tokens: &[TokenInfo], src: &str) -> Result<(FieldAttr, u32), ParseError> {
    let mut result = FieldAttr::new(Visibility::Private, BitFlags::empty());
    let mut index = None;

    for token in tokens {
        if let TokenKind::Literal {
            kind:
                LiteralKind::Int {
                    base: _,
                    empty_int: _,
                },
            suffix_start: _,
        } = token.kind
        {
            index = Some(
                token
                    .get_as_int(src)
                    .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))?,
            );
            continue;
        }
        let TokenKind::Keyword(tk_kw) = token.kind else {
            return Err(ParseError::Unexpect(*token));
        };
        if !tk_kw.is_field_modifier() {
            return Err(ParseError::Unexpect(*token));
        }
        match tk_kw {
            token::Keyword::Public => {
                result.set_vis(Visibility::Public);
            }
            token::Keyword::Private => {
                result.set_vis(Visibility::Private);
            }
            token::Keyword::Internal => {
                result.set_vis(Visibility::AssemblyOnly);
            }
            token::Keyword::Static => {
                result
                    .impl_flags_mut()
                    .insert(FieldImplementationFlags::Static);
            }

            _ => unreachable!(),
        }
    }

    Ok((
        result,
        index
            .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))?
            .try_into()
            .unwrap(),
    ))
}
