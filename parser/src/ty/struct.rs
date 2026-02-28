use std::collections::HashMap;

use ast::ty::r#struct::{StructDef, StructItem};
use compiler_base::{
    AnyResult,
    global::attrs::{StructImplementationFlags, TypeAttr, TypeSpecificAttr, Visibility},
};
use enumflags2::BitFlags;
use token::{Keyword, SpecialChar, TokenInfo, TokenKind};

use crate::{ParseError, cursor_mod::TokenCursor};

pub fn struct_def(cursor: &mut TokenCursor) -> AnyResult<StructDef> {
    assert!(cursor.consume().is_some_and(|x| x.kind == Keyword::Struct));

    cursor.require_special_char(SpecialChar::BracketOpen)?;
    let attr_tokens = cursor.get_token_seq(|x| {
        matches!(x.kind, TokenKind::Keyword(a) if a.is_struct_modifier()) || x.is_int()
    });
    let (attr, index) = struct_attr(attr_tokens, cursor.tokens.src)?;
    cursor.index += attr_tokens.len();
    cursor.require_special_char(SpecialChar::BracketClose)?;

    let name = cursor.require_identifier()?;

    let mut generics = Vec::new();
    let mut is_generic_infinite = false;
    if cursor
        .require_special_char(SpecialChar::BracketOpen)
        .is_ok()
    {
        while let Ok(g) = cursor.require_identifier() {
            generics.push(g);
        }
        if cursor.require_special_char(SpecialChar::Plus).is_ok() {
            is_generic_infinite = true;
        }
        cursor.require_special_char(SpecialChar::BracketClose)?;
    }

    let generic_bounds = HashMap::new();
    if cursor.require_keyword(Keyword::Where).is_ok() {
        todo!("Wheres haven't been implemented")
    }

    cursor.require_special_char(SpecialChar::BraceOpen)?;
    let mut this = StructDef {
        attr,
        index,
        name,
        generics,
        is_generic_infinite,
        generic_bounds,
        methods: Vec::new(),
        fields: Vec::new(),
    };
    while let Ok(item) = struct_item(cursor, &this) {
        match item {
            StructItem::Method(method) => this.methods.push(method),
            StructItem::Field(field) => this.fields.push(field),
        }
    }
    cursor.require_special_char(SpecialChar::BraceClose)?;

    Ok(this)
}

fn struct_item(cursor: &mut TokenCursor, s: &StructDef) -> AnyResult<StructItem> {
    let next_token = cursor.current().ok_or(ParseError::UnexpectEOF)?;
    match next_token.kind {
        TokenKind::Keyword(Keyword::Field) => {
            crate::field::field(cursor, &s.generics).map(StructItem::Field)
        }
        TokenKind::Keyword(Keyword::Method) => crate::method::method(cursor, &s.generics)
            .and_then(|x| {
                if x.attr.overrides().is_some() {
                    Err(ParseError::OverrideUnsupportedInStructs.into())
                } else {
                    Ok(x)
                }
            })
            .map(StructItem::Method),

        _ => Err(ParseError::Unexpect(*next_token).into()),
    }
}

fn struct_attr(tokens: &[TokenInfo], src: &str) -> Result<(TypeAttr, u32), ParseError> {
    let mut result = TypeAttr::new(
        Visibility::Private,
        TypeSpecificAttr::Struct(BitFlags::empty()),
    );
    let mut index = None;

    for token in tokens {
        if token.is_int() {
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
        if !tk_kw.is_struct_modifier() {
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
            token::Keyword::Ref => {
                result
                    .specific_mut()
                    .unwrap_struct_mut()
                    .insert(StructImplementationFlags::Ref);
            }
            token::Keyword::Partial => {
                result
                    .specific_mut()
                    .unwrap_struct_mut()
                    .insert(StructImplementationFlags::Partial);
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
