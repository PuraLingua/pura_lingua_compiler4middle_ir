use std::{borrow::Cow, collections::HashMap};

use ast::{
    identifier::Identifier,
    method::{
        CheckKind, JumpCondition, JumpTargetType, Literal, Method, MethodReference, Parameter,
        Statement,
    },
};
use compiler_base::{
    AnyResult,
    abstract_info::type_reference::TypeReference,
    global::attrs::{
        CallConvention, MethodAttr, MethodImplementationFlags, ParameterAttr,
        ParameterImplementationFlags, Visibility,
    },
};
use either::Either;
use enumflags2::BitFlags;
use token::{Keyword, LiteralKind, LiteralKindType, SpecialChar, TokenInfo, TokenKind};

use crate::{ParseError, cursor_mod::TokenCursor};

pub fn method(cursor: &mut TokenCursor, ty_generics: &[Identifier]) -> AnyResult<Method> {
    assert!(cursor.consume().is_some_and(|x| x.kind == Keyword::Method));

    cursor.require_special_char(SpecialChar::BracketOpen)?;
    let attr_tokens = cursor.get_token_seq(|x| {
        matches!(x.kind, TokenKind::Keyword(a) if a.is_method_modifier()) || x.is_int()
    });
    let (mut attr, call_convention, index) = method_attr(attr_tokens, cursor.tokens.src)?;
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

    cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
    let args = crate::punctuated::punctuated(
        cursor,
        false,
        |cursor| parse_parameter(cursor, ty_generics, &generics),
        |cursor| {
            cursor
                .require_special_char(SpecialChar::Comma)
                .map_err(From::from)
        },
    )?
    .into_iter()
    .collect();
    cursor.require_special_char(SpecialChar::ParenthesisClose)?;

    cursor.require_special_char(SpecialChar::ThinArrow)?;
    let return_type = crate::ty::type_reference(cursor, ty_generics, &generics)?;

    let generic_bounds = HashMap::new();
    if cursor.require_keyword(Keyword::Where).is_ok() {
        todo!("Wheres haven't been implemented")
    }

    cursor.require_special_char(SpecialChar::BraceOpen)?;
    let variables = parse_locals(cursor, &mut attr, ty_generics, &generics)?;
    let mut statements = Vec::new();
    while cursor
        .peek_require_special_char(SpecialChar::BraceClose)
        .is_err()
    {
        statements.push(parse_statement(cursor, ty_generics, &generics)?);
    }
    cursor.require_special_char(SpecialChar::BraceClose)?;

    Ok(Method {
        attr,
        call_conv: call_convention,
        index,
        name,
        generics,
        is_generic_infinite,
        args,
        return_type,
        generic_bounds,
        locals: variables,
        statements,
    })
}

fn parse_parameter(
    cursor: &mut TokenCursor,
    ty_generics: &[Identifier],
    method_generics: &[Identifier],
) -> AnyResult<Parameter> {
    cursor.require_special_char(SpecialChar::BracketOpen)?;
    let attr_tokens = cursor.get_token_seq(|x| {
        matches!(x.kind, TokenKind::Keyword(a) if a.is_parameter_modifier()) || x.is_int()
    });
    let attr = parameter_attr(attr_tokens)?;
    cursor.index += attr_tokens.len();
    cursor.require_special_char(SpecialChar::BracketClose)?;
    let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
    Ok(Parameter { attr, ty })
}

fn parameter_attr(tokens: &[TokenInfo]) -> Result<ParameterAttr, ParseError> {
    let mut result = ParameterAttr::new(BitFlags::empty());

    for token in tokens {
        let TokenKind::Keyword(tk_kw) = token.kind else {
            return Err(ParseError::Unexpect(*token));
        };
        if !tk_kw.is_parameter_modifier() {
            return Err(ParseError::Unexpect(*token));
        }
        match tk_kw {
            Keyword::Ref => {
                result
                    .impl_flags_mut()
                    .insert(ParameterImplementationFlags::ByRef);
            }

            _ => unreachable!(),
        }
    }

    Ok(result)
}

fn parse_locals(
    cursor: &mut TokenCursor,
    attr: &mut MethodAttr<TypeReference>,
    ty_generics: &[Identifier],
    method_generics: &[Identifier],
) -> AnyResult<Vec<Identifier>> {
    let mut locals = Vec::new();
    if cursor.require_keyword(Keyword::Locals).is_err() {
        return Ok(Vec::new());
    };

    cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
    while let Ok(name) = cursor.require_identifier() {
        cursor.require_special_char(SpecialChar::Colon)?;
        let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
        attr.add_local_variable(ty);
        locals.push(name);
    }
    cursor.require_special_char(SpecialChar::ParenthesisClose)?;
    Ok(locals)
}

fn method_attr(
    tokens: &[TokenInfo],
    src: &str,
) -> Result<(MethodAttr<TypeReference>, CallConvention, u32), ParseError> {
    let mut result = MethodAttr::new(Visibility::Private, BitFlags::empty(), None, Vec::new());
    let mut index: Option<u32> = None;
    let mut call_convention = CallConvention::PlatformDefault;
    let mut is_override = false;

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
                    .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))?
                    .try_into()
                    .unwrap(),
            );
            continue;
        }
        let TokenKind::Keyword(tk_kw) = token.kind else {
            return Err(ParseError::Unexpect(*token));
        };
        if !tk_kw.is_method_modifier() {
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
                    .insert(MethodImplementationFlags::Static);
            }
            token::Keyword::HideWhenCapturing => {
                result
                    .impl_flags_mut()
                    .insert(MethodImplementationFlags::HideWhenCapturing);
            }
            token::Keyword::Override => {
                is_override = true;
                continue;
            }
            Keyword::CallConvention_C => {
                call_convention = CallConvention::CDecl;
            }
            Keyword::CallConvention_CWithVararg => {
                call_convention = CallConvention::CDeclWithVararg;
            }
            Keyword::CallConvention_FASTCALL => {
                call_convention = CallConvention::Fastcall;
            }
            Keyword::CallConvention_STDCALL => {
                call_convention = CallConvention::Stdcall;
            }
            Keyword::CallConvention_SystemV => {
                call_convention = CallConvention::SystemV;
            }
            Keyword::CallConvention_WIN64 => {
                call_convention = CallConvention::Win64;
            }

            _ => unreachable!(),
        }
    }

    if is_override {
        *result.overrides_mut() = index;
    }

    Ok((
        result,
        call_convention,
        index
            .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))?
            .try_into()
            .unwrap(),
    ))
}

pub fn parse_statement(
    cursor: &mut TokenCursor,
    ty_generics: &[Identifier],
    method_generics: &[Identifier],
) -> AnyResult<Statement> {
    fn parse_string<'a>(cursor: &'a mut TokenCursor) -> Result<Cow<'a, str>, ParseError> {
        let tk = *cursor.consume().ok_or(ParseError::UnexpectEOF)?;
        tk.get_as_string(cursor.tokens.src)
            .ok_or(ParseError::ExpectLiterals(vec![
                LiteralKindType::Str,
                LiteralKindType::RawStr,
            ]))
    }
    fn parse_from_u64<T, F: FnOnce(u64) -> T>(
        cursor: &mut TokenCursor,
        caster: F,
    ) -> Result<T, ParseError> {
        let tk = *cursor.consume().ok_or(ParseError::UnexpectEOF)?;
        tk.get_as_int(cursor.tokens.src)
            .ok_or(ParseError::ExpectLiteral(token::LiteralKindType::Int))
            .map(caster)
    }
    fn parse_check_kind(cursor: &mut TokenCursor) -> Result<CheckKind, ParseError> {
        cursor
            .require_keyword(Keyword::Check_AllZero)
            .map(|_| CheckKind::AllZero)
    }

    fn parse_field_ref(cursor: &mut TokenCursor) -> Result<Either<u32, Identifier>, ParseError> {
        cursor
            .require_identifier()
            .ok()
            .map(Either::Right)
            .or_else(|| parse_from_u64(cursor, try_into).ok().map(Either::Left))
            .ok_or_else(|| {
                ParseError::ExpectLiterals(vec![
                    LiteralKindType::Int,
                    LiteralKindType::Str,
                    LiteralKindType::RawStr,
                ])
            })
    }

    fn parse_jump_target_type(cursor: &mut TokenCursor) -> Result<JumpTargetType, ParseError> {
        cursor
            .require_keyword(Keyword::Jump_Absolute)
            .map(|_| JumpTargetType::Absolute)
            .or_else(|_| {
                cursor
                    .require_keyword(Keyword::Jump_Forward)
                    .map(|_| JumpTargetType::Forward)
            })
            .or_else(|_| {
                cursor
                    .require_keyword(Keyword::Jump_Backward)
                    .map(|_| JumpTargetType::Backward)
            })
    }

    #[inline(always)]
    fn noop<T>(v: T) -> T {
        v
    }

    fn try_into<T, U: TryFrom<T>>(v: T) -> U
    where
        <U as TryFrom<T>>::Error: std::fmt::Debug,
    {
        U::try_from(v).unwrap()
    }

    fn literal(cursor: &mut TokenCursor, is_negative: Option<bool>) -> AnyResult<Literal> {
        let tk = *cursor.consume().ok_or(ParseError::UnexpectEOF)?;
        match tk.kind {
            TokenKind::SpecialChar(SpecialChar::Hyphen) => {
                literal(cursor, Some(!is_negative.unwrap_or(false)))
            }
            TokenKind::Literal { kind, suffix_start } => match kind {
                LiteralKind::Int {
                    base,
                    empty_int: false,
                } => {
                    let value_s = &cursor.tokens.src[tk.span][..(suffix_start as usize)];
                    let ty = &cursor.tokens.src[tk.span][(suffix_start as usize)..];
                    let radix = match base {
                        token::Base::Binary => 2,
                        token::Base::Octal => 8,
                        token::Base::Decimal => 10,
                        token::Base::Hexadecimal => 16,
                    };
                    let start = if radix == 10 { 0 } else { 2 };
                    match ty {
                        "u8" => Ok(Literal::U8(u8::from_str_radix(&value_s[start..], radix)?)),
                        "u16" => Ok(Literal::U16(u16::from_str_radix(&value_s[start..], radix)?)),
                        "u32" => Ok(Literal::U32(u32::from_str_radix(&value_s[start..], radix)?)),
                        "u64" => Ok(Literal::U64(u64::from_str_radix(&value_s[start..], radix)?)),
                        "usize" => {
                            if std::io::IsTerminal::is_terminal(&std::io::stderr()) {
                                eprintln!(
                                    "You are using usize, whose size depends on current platform"
                                );
                            }
                            match size_of::<usize>() {
                                1 => Ok(Literal::U8(u8::from_str_radix(&value_s[start..], radix)?)),
                                2 => {
                                    Ok(Literal::U16(u16::from_str_radix(&value_s[start..], radix)?))
                                }
                                4 => {
                                    Ok(Literal::U32(u32::from_str_radix(&value_s[start..], radix)?))
                                }
                                8 => {
                                    Ok(Literal::U64(u64::from_str_radix(&value_s[start..], radix)?))
                                }
                                _ => unimplemented!(),
                            }
                        }
                        "i8" => Ok(Literal::I8(if is_negative.is_some_and(|x| x) {
                            -i8::from_str_radix(&value_s[start..], radix)?
                        } else {
                            i8::from_str_radix(&value_s[start..], radix)?
                        })),
                        "i16" => Ok(Literal::I16(if is_negative.is_some_and(|x| x) {
                            -i16::from_str_radix(&value_s[start..], radix)?
                        } else {
                            i16::from_str_radix(&value_s[start..], radix)?
                        })),
                        "i32" => Ok(Literal::I32(if is_negative.is_some_and(|x| x) {
                            -i32::from_str_radix(&value_s[start..], radix)?
                        } else {
                            i32::from_str_radix(&value_s[start..], radix)?
                        })),
                        "i64" => Ok(Literal::I64(if is_negative.is_some_and(|x| x) {
                            -i64::from_str_radix(&value_s[start..], radix)?
                        } else {
                            i64::from_str_radix(&value_s[start..], radix)?
                        })),
                        "isize" => {
                            if std::io::IsTerminal::is_terminal(&std::io::stderr()) {
                                eprintln!(
                                    "You are using isize, whose size depends on current platform"
                                );
                            }
                            match size_of::<isize>() {
                                1 => Ok(Literal::I8(if is_negative.is_some_and(|x| x) {
                                    -i8::from_str_radix(&value_s[start..], radix)?
                                } else {
                                    i8::from_str_radix(&value_s[start..], radix)?
                                })),
                                2 => Ok(Literal::I16(if is_negative.is_some_and(|x| x) {
                                    -i16::from_str_radix(&value_s[start..], radix)?
                                } else {
                                    i16::from_str_radix(&value_s[start..], radix)?
                                })),
                                4 => Ok(Literal::I32(if is_negative.is_some_and(|x| x) {
                                    -i32::from_str_radix(&value_s[start..], radix)?
                                } else {
                                    i32::from_str_radix(&value_s[start..], radix)?
                                })),
                                8 => Ok(Literal::I64(if is_negative.is_some_and(|x| x) {
                                    -i64::from_str_radix(&value_s[start..], radix)?
                                } else {
                                    i64::from_str_radix(&value_s[start..], radix)?
                                })),
                                _ => unimplemented!(),
                            }
                        }
                        _ => Err(ParseError::UnknownSuffix(ty.to_owned()).into()),
                    }
                }
                LiteralKind::Int {
                    base: _,
                    empty_int: true,
                } => Err(ParseError::ExpectAllLiteral.into()),
                LiteralKind::Float { .. } => todo!(),
                LiteralKind::Char { terminated } => {
                    assert!(terminated);
                    assert!(is_negative.is_none());
                    let s = &cursor.tokens.src[tk.span][1..(suffix_start as usize - 1)];
                    compiler_base::descape::UnescapeExt::to_unescaped(s)?
                        .chars()
                        .next()
                        .ok_or(ParseError::ExpectLiteral(LiteralKindType::Char).into())
                        .map(Literal::Char)
                }
                LiteralKind::Byte { terminated } => {
                    assert!(terminated);
                    assert!(is_negative.is_none());
                    let s = &cursor.tokens.src[tk.span][1..(suffix_start as usize - 1)];
                    compiler_base::descape::UnescapeExt::to_unescaped(s)?
                        .as_bytes()
                        .first()
                        .copied()
                        .ok_or(ParseError::ExpectLiteral(LiteralKindType::Byte).into())
                        .map(Literal::Byte)
                }
                LiteralKind::Str { terminated } => {
                    assert!(terminated);
                    assert!(is_negative.is_none());
                    let s = &cursor.tokens.src[tk.span][1..(suffix_start as usize - 1)];
                    match compiler_base::descape::UnescapeExt::to_unescaped(s)? {
                        Cow::Borrowed(x) => Ok(Literal::String(x.to_owned())),
                        Cow::Owned(x) => Ok(Literal::String(x)),
                    }
                }
                LiteralKind::ByteStr { terminated } => {
                    assert!(terminated);
                    assert!(is_negative.is_none());
                    let s = &cursor.tokens.src[tk.span][1..(suffix_start as usize - 1)];
                    match compiler_base::descape::UnescapeExt::to_unescaped(s)? {
                        Cow::Borrowed(x) => Ok(Literal::ByteString(x.as_bytes().to_owned())),
                        Cow::Owned(x) => Ok(Literal::ByteString(x.into_bytes())),
                    }
                }
                LiteralKind::RawStr { n_hashes } => {
                    assert!(is_negative.is_none());
                    let prefix_offset = 2 + n_hashes.unwrap_or(0) as usize;
                    let until = suffix_start as usize - 1 - n_hashes.unwrap_or(0) as usize;
                    Ok(Literal::String(
                        cursor.tokens.src[tk.span][prefix_offset..until].to_owned(),
                    ))
                }
                LiteralKind::RawByteStr { n_hashes } => {
                    assert!(is_negative.is_none());
                    let prefix_offset = 2 + n_hashes.unwrap_or(0) as usize;
                    let until = suffix_start as usize - 1 - n_hashes.unwrap_or(0) as usize;
                    Ok(Literal::ByteString(
                        cursor.tokens.src[tk.span].as_bytes()[prefix_offset..until].to_owned(),
                    ))
                }
            },
            TokenKind::Keyword(Keyword::True) => {
                if is_negative.is_some_and(|x| x) {
                    Ok(Literal::False)
                } else {
                    Ok(Literal::True)
                }
            }
            TokenKind::Keyword(Keyword::False) => {
                if is_negative.is_some_and(|x| x) {
                    Ok(Literal::True)
                } else {
                    Ok(Literal::False)
                }
            }
            TokenKind::Keyword(Keyword::This) => {
                assert!(is_negative.is_none());
                Ok(Literal::This)
            }
            _ => {
                cursor.index -= 1;
                Err(ParseError::ExpectAllLiteral.into())
            }
        }
    }

    fn common_args(cursor: &mut TokenCursor) -> AnyResult<Vec<Identifier>> {
        crate::punctuated::punctuated(
            cursor,
            false,
            |cursor| cursor.require_identifier().map_err(From::from),
            |cursor| {
                cursor
                    .require_special_char(SpecialChar::Comma)
                    .map_err(From::from)
            },
        )
        .map(|x| x.into_iter().collect())
    }

    let statement_tk = cursor.consume_statement_keyword()?;
    match statement_tk.kind.unwrap_statement_keyword() {
        token::keyword::StatementKeyword::Load => {
            let literal = literal(cursor, None)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let var = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::Load { literal, var })
        }
        token::keyword::StatementKeyword::LoadTypeValueSize => {
            let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let var = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::LoadTypeValueSize { ty, var })
        }
        token::keyword::StatementKeyword::ReadPointerTo => {
            let ptr = cursor.require_identifier()?;
            let size = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let destination = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::ReadPointerTo {
                ptr,
                size,
                destination,
            })
        }
        token::keyword::StatementKeyword::WritePointer => {
            let source = cursor.require_identifier()?;
            let size = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let ptr = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::WritePointer { source, size, ptr })
        }

        token::keyword::StatementKeyword::Check => {
            let kind = parse_check_kind(cursor)?;
            let to_check = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::Check {
                kind,
                to_check,
                result,
            })
        }

        token::keyword::StatementKeyword::NewObject => {
            let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            let ctor = parse_method_reference(cursor, ty_generics, method_generics)?;
            cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
            let args = common_args(cursor)?;
            cursor.require_special_char(SpecialChar::ParenthesisClose)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::NewObject {
                ty,
                ctor,
                args,
                result,
            })
        }
        token::keyword::StatementKeyword::NewArray => {
            let element_ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            let len = cursor
                .require_identifier()
                .map(Either::Left)
                .or_else(|_| parse_from_u64(cursor, noop).map(Either::Right))?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::NewArray {
                element_ty,
                len,
                result,
            })
        }

        token::keyword::StatementKeyword::InstanceCall => {
            let val = cursor.require_identifier()?;
            let method = parse_method_reference(cursor, ty_generics, method_generics)?;
            cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
            let args = common_args(cursor)?;
            cursor.require_special_char(SpecialChar::ParenthesisClose)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::InstanceCall {
                val,
                method,
                args,
                result,
            })
        }
        token::keyword::StatementKeyword::StaticCall => {
            let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            let method = parse_method_reference(cursor, ty_generics, method_generics)?;
            cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
            let args = common_args(cursor)?;
            cursor.require_special_char(SpecialChar::ParenthesisClose)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::StaticCall {
                ty,
                method,
                args,
                result,
            })
        }
        token::keyword::StatementKeyword::NonPurusCall => {
            cursor.require_special_char(SpecialChar::NumberSign)?;
            let config = cursor.require_identifier()?;
            let f_pointer = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ParenthesisOpen)?;
            let args = common_args(cursor)?;
            cursor.require_special_char(SpecialChar::ParenthesisClose)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let result = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::NonPurusCall {
                config,
                f_pointer,
                args,
                result,
            })
        }
        token::keyword::StatementKeyword::LoadArg => {
            let arg = parse_from_u64(cursor, noop)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let local = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::LoadArg { arg, local })
        }
        token::keyword::StatementKeyword::LoadStatic => {
            let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            let field = parse_field_ref(cursor)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let local = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::LoadStatic { ty, field, local })
        }
        token::keyword::StatementKeyword::LoadField => {
            let container = cursor.require_identifier()?;
            let field = parse_field_ref(cursor)?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let local = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::LoadField {
                container,
                field,
                local,
            })
        }
        token::keyword::StatementKeyword::SetThisField => {
            let val = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let field = parse_field_ref(cursor)?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::SetThisField { val, field })
        }
        token::keyword::StatementKeyword::SetStaticField => {
            let val = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::ThinArrow)?;
            let ty = crate::ty::type_reference(cursor, ty_generics, method_generics)?;
            let field = parse_field_ref(cursor)?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::SetStaticField { val, ty, field })
        }
        token::keyword::StatementKeyword::Throw => {
            let val = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::Throw { val })
        }
        token::keyword::StatementKeyword::ReturnVal => {
            let val = cursor.require_identifier()?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::ReturnVal { val })
        }
        token::keyword::StatementKeyword::Jump => {
            let condition = match cursor.require_keyword(Keyword::If) {
                Ok(_) => {
                    let current = *cursor.current().ok_or(ParseError::UnexpectEOF)?;
                    match current.kind {
                        TokenKind::Keyword(Keyword::True) => {
                            cursor.advance();
                            let to_check = cursor.require_identifier()?;
                            JumpCondition::IfTrue(to_check)
                        }
                        TokenKind::Keyword(Keyword::Success) => {
                            cursor.advance();
                            let check_kind = parse_check_kind(cursor)?;
                            let to_check = cursor.require_identifier()?;
                            JumpCondition::CheckSuccess(check_kind, to_check)
                        }
                        TokenKind::Keyword(Keyword::Failure) => {
                            cursor.advance();
                            let check_kind = parse_check_kind(cursor)?;
                            let to_check = cursor.require_identifier()?;
                            JumpCondition::CheckFailure(check_kind, to_check)
                        }
                        _ => {
                            return Err(ParseError::ExpectMany(vec![
                                TokenKind::Keyword(Keyword::True),
                                TokenKind::Keyword(Keyword::Success),
                                TokenKind::Keyword(Keyword::Failure),
                            ])
                            .into());
                        }
                    }
                }
                Err(_) => JumpCondition::Unconditional,
            };
            let ty = parse_jump_target_type(cursor)?;
            let val = parse_from_u64(cursor, noop)?;
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::Jump { condition, ty, val })
        }
        token::keyword::StatementKeyword::Nop => {
            cursor.require_special_char(SpecialChar::Semicolon)?;
            Ok(Statement::Nop)
        }
    }
}

fn parse_method_reference(
    cursor: &mut TokenCursor,
    ty_generics: &[Identifier],
    method_generics: &[Identifier],
) -> AnyResult<MethodReference> {
    let index = *cursor.consume().ok_or(ParseError::UnexpectEOF)?;
    let index = index
        .get_as_int(cursor.tokens.src)
        .map(|x| Either::Left(u32::try_from(x).unwrap()))
        .or_else(|| {
            cursor.index -= 1;
            cursor.require_identifier().ok().map(Either::Right)
        })
        .ok_or(ParseError::ExpectLiterals(vec![
            LiteralKindType::Int,
            LiteralKindType::Str,
            LiteralKindType::RawStr,
        ]))?
        .try_into()
        .unwrap();

    let mut generics = Vec::new();
    if cursor
        .require_special_char(SpecialChar::BracketOpen)
        .is_ok()
    {
        while let Ok(g) = crate::ty::type_reference(cursor, ty_generics, method_generics) {
            generics.push(g);
        }
        cursor.require_special_char(SpecialChar::BracketClose)?;
    }

    Ok(MethodReference { index, generics })
}
