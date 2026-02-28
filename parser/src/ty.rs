use ast::{identifier::Identifier, ty::TypeDef};
use compiler_base::{AnyResult, abstract_info::type_reference::TypeReference};
use token::{Keyword, SpecialChar, TokenKind};

use crate::{ParseError, cursor_mod::TokenCursor};

mod class;
mod r#struct;

pub fn type_reference(
    cursor: &mut TokenCursor,
    ty_generics: &[Identifier],
    method_generics: &[Identifier],
) -> AnyResult<TypeReference> {
    if cursor
        .require_special_char(SpecialChar::BracketOpen)
        .is_ok()
    {
        let assembly_name = cursor
            .require_special_char(SpecialChar::ExclamationMark)
            .map(|x| cursor.tokens.src[x.span].to_owned())
            .or_else(|_| cursor.require_identifier().map(|x| x.var))?;
        cursor.require_special_char(SpecialChar::BracketClose)?;
        let type_name = cursor.require_identifier()?;
        if cursor
            .require_special_char(SpecialChar::LessThanSign)
            .is_ok()
        {
            let generics = crate::punctuated::punctuated::<TypeReference, _, _, _>(
                cursor,
                false,
                |cursor| type_reference(cursor, ty_generics, method_generics),
                |cursor| {
                    cursor
                        .require_special_char(SpecialChar::Comma)
                        .map_err(From::from)
                },
            )?;
            cursor.require_special_char(SpecialChar::GreaterThanSign)?;
            Ok(TypeReference {
                is_generic: false,
                assembly_name: Some(assembly_name),
                name: type_name.var,
                index: None,
                generics: generics.into_iter().collect(),
            })
        } else {
            Ok(TypeReference {
                is_generic: false,
                assembly_name: Some(assembly_name),
                name: type_name.var,
                index: None,
                generics: Vec::new(),
            })
        }
    } else {
        let ident = cursor.require_identifier()?;
        if let Some(pos) = ty_generics.iter().position(|x| *x == ident) {
            return Ok(TypeReference {
                is_generic: true,
                assembly_name: None,
                name: ident.var,
                index: Some(pos as u32),
                generics: vec![],
            });
        }

        if let Some(pos) = method_generics.iter().position(|x| *x == ident) {
            return Ok(TypeReference {
                is_generic: true,
                assembly_name: None,
                name: ident.var,
                index: Some((pos + ty_generics.len()) as u32),
                generics: vec![],
            });
        } else {
            return Err(ParseError::TypeNotFound(TypeReference {
                is_generic: true,
                assembly_name: None,
                name: ident.var,
                index: None,
                generics: vec![],
            })
            .into());
        }
    }
}

pub fn type_def(cursor: &mut TokenCursor) -> AnyResult<TypeDef> {
    let tok = cursor.current().ok_or(ParseError::UnexpectEOF)?;
    match tok.kind {
        TokenKind::Keyword(Keyword::Class) => class::class_def(cursor).map(TypeDef::Class),
        TokenKind::Keyword(Keyword::Struct) => r#struct::struct_def(cursor).map(TypeDef::Struct),

        _ => Err(ParseError::Unexpect(*tok).into()),
    }
}
