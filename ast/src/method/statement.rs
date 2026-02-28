use compiler_base::abstract_info::type_reference::TypeReference;
use either::Either;

use crate::{identifier::Identifier, method::MethodReference};

#[derive(Clone, Debug)]
pub enum Statement {
    /// [`Instruction`] LoadTrue, LoadFalse, Load_*, LoadThis
    ///
    /// [`Instruction`]: pura_lingua::global::instruction::Instruction
    Load {
        literal: Literal,
        var: Identifier,
    },
    LoadTypeValueSize {
        ty: TypeReference,
        var: Identifier,
    },
    ReadPointerTo {
        ptr: Identifier,
        size: Identifier,
        destination: Identifier,
    },
    WritePointer {
        source: Identifier,
        size: Identifier,
        ptr: Identifier,
    },

    /// [`Instruction`] IsAllZero
    ///
    /// [`Instruction`]: pura_lingua::global::instruction::Instruction
    Check {
        kind: CheckKind,
        to_check: Identifier,
        result: Identifier,
    },

    NewObject {
        ty: TypeReference,
        ctor: MethodReference,
        args: Vec<Identifier>,
        result: Identifier,
    },
    NewArray {
        element_ty: TypeReference,
        len: Either<Identifier, u64>,
        result: Identifier,
    },

    InstanceCall {
        val: Identifier,
        method: MethodReference,
        args: Vec<Identifier>,
        result: Identifier,
    },
    StaticCall {
        ty: TypeReference,
        method: MethodReference,
        args: Vec<Identifier>,
        result: Identifier,
    },
    NonPurusCall {
        config: Identifier,
        f_pointer: Identifier,
        args: Vec<Identifier>,
        result: Identifier,
    },

    LoadArg {
        arg: u64,
        local: Identifier,
    },

    LoadStatic {
        ty: TypeReference,
        field: Either<u32, Identifier>,
        local: Identifier,
    },

    LoadField {
        container: Identifier,
        field: Either<u32, Identifier>,
        local: Identifier,
    },

    SetThisField {
        val: Identifier,
        field: Either<u32, Identifier>,
    },

    SetStaticField {
        val: Identifier,
        ty: TypeReference,
        field: Either<u32, Identifier>,
    },

    Throw {
        val: Identifier,
    },

    ReturnVal {
        val: Identifier,
    },

    Jump {
        condition: JumpCondition,
        ty: JumpTargetType,
        val: u64,
    },
    Nop,
}

#[derive(Clone, Debug)]
pub enum JumpCondition {
    Unconditional,
    IfTrue(Identifier),
    CheckSuccess(CheckKind, Identifier),
    CheckFailure(CheckKind, Identifier),
}

#[derive(Clone, Debug, Copy)]
pub enum JumpTargetType {
    Absolute,
    Forward,
    Backward,
}

#[derive(Clone, Debug, Copy)]
pub enum CheckKind {
    AllZero,
}

// #[derive(Clone, Debug)]
// pub enum Operand {
//     /// It cannot be a string literal
//     Identifier(Identifier),
//     Literal(Literal),
// }

#[derive(Clone, Debug)]
pub enum Literal {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    String(String),
    Char(char),
    ByteString(Vec<u8>),
    Byte(u8),
    True,
    False,
    This,
}
