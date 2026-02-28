use std::collections::HashMap;

use ast::ty::class::{ClassDef, ClassItem};
use compiler_base::{
    AnyResult,
    global::attrs::{ClassImplementationFlags, TypeAttr, TypeSpecificAttr, Visibility},
};
use enumflags2::BitFlags;
use token::{Keyword, SpecialChar, TokenInfo, TokenKind};

use crate::{ParseError, cursor_mod::TokenCursor};

pub fn class_def(cursor: &mut TokenCursor) -> AnyResult<ClassDef> {
    assert!(cursor.consume().is_some_and(|x| x.kind == Keyword::Class));

    cursor.require_special_char(SpecialChar::BracketOpen)?;
    let attr_tokens = cursor.get_token_seq(|x| {
        matches!(x.kind, TokenKind::Keyword(a) if a.is_class_modifier()) || x.is_int()
    });
    let (attr, index) = class_attr(attr_tokens, cursor.tokens.src)?;
    cursor.index += attr_tokens.len();
    cursor.require_special_char(SpecialChar::BracketClose)?;

    let mut main = None;
    if cursor.require_keyword(Keyword::Main).is_ok() {
        cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
        let main_tk = *cursor.consume().ok_or(ParseError::UnexpectEOF)?;
        main = Some(
            main_tk
                .get_as_int(cursor.tokens.src)
                .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))?
                .try_into()
                .unwrap(),
        );
        cursor.require_special_char(SpecialChar::ParenthesisClose)?;
    }

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

    let mut parent = None;
    if cursor.require_special_char(SpecialChar::Colon).is_ok() {
        parent = Some(crate::ty::type_reference(cursor, &generics, &[])?);
    }

    let generic_bounds = HashMap::new();
    if cursor.require_keyword(Keyword::Where).is_ok() {
        todo!("Wheres haven't been implemented")
    }

    cursor.require_special_char(SpecialChar::BraceOpen)?;
    let mut this = ClassDef {
        attr,
        index,
        main,
        name,
        generics,
        is_generic_infinite,
        parent,
        generic_bounds,
        methods: Vec::new(),
        fields: Vec::new(),
    };
    while cursor
        .peek_require_special_char(SpecialChar::BraceClose)
        .is_err()
    {
        match class_item(cursor, &this)? {
            ClassItem::Method(method) => this.methods.push(method),
            ClassItem::Field(field) => this.fields.push(field),
        }
    }
    cursor.require_special_char(SpecialChar::BraceClose)?;

    Ok(this)
}

fn class_item(cursor: &mut TokenCursor, class: &ClassDef) -> AnyResult<ClassItem> {
    let next_token = cursor.current().ok_or(ParseError::UnexpectEOF)?;
    match next_token.kind {
        TokenKind::Keyword(Keyword::Field) => {
            crate::field::field(cursor, &class.generics).map(ClassItem::Field)
        }
        TokenKind::Keyword(Keyword::Method) => {
            crate::method::method(cursor, &class.generics).map(ClassItem::Method)
        }

        _ => Err(ParseError::Unexpect(*next_token).into()),
    }
}

fn class_attr(tokens: &[TokenInfo], src: &str) -> Result<(TypeAttr, u32), ParseError> {
    let mut result = TypeAttr::new(
        Visibility::Private,
        TypeSpecificAttr::Class(BitFlags::empty()),
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
        if !tk_kw.is_class_modifier() {
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
                    .specific_mut()
                    .unwrap_class_mut()
                    .insert(ClassImplementationFlags::Static);
            }
            token::Keyword::Partial => {
                result
                    .specific_mut()
                    .unwrap_class_mut()
                    .insert(ClassImplementationFlags::Partial);
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
