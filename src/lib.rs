#![feature(iterator_try_collect)]
#![feature(never_type)]
#![feature(decl_macro)]
#![allow(non_snake_case)]

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use ast::{
    File,
    field::Field,
    identifier::Identifier,
    method::{Method, MethodReference},
    ty::TypeDef,
};
use compiler_base::abstract_info::type_reference::TypeReference;
use either::Either;
use parser::ParseError;
use pura_lingua::{
    binary::{
        self,
        prelude::{
            Assembly, MethodToken, MethodTokenBuilder, MethodType, TypeToken, TypeTokenBuilder,
            TypeType,
        },
        ty::{MethodSpec, TypeRef, TypeSpec},
    },
    global::instruction::{Instruction, JumpTargetBuilder, RegisterAddr},
};

#[cfg(test)]
mod tests;

pub struct CompileContext {
    files: HashMap<String, File>,
}

pub fn lang_spec_dependency_path(dep: &str) -> pura_lingua::global::Result<PathBuf> {
    let mut dir = PathBuf::from(pura_lingua::global::path_searcher::get_stdlib_dir()?);
    dir.push("LanguageSpecific");
    dir.push("MiddleIR");
    dir.push(dep);
    Ok(dir)
}

pub fn core_dir() -> pura_lingua::global::Result<PathBuf> {
    lang_spec_dependency_path("Core")
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn add_direct(
        &mut self,
        content: &str,
        assembly_name: String,
        is_header: bool,
    ) -> pura_lingua::global::Result<()> {
        match self.files.entry(assembly_name) {
            std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                let f = parser::parse(occupied_entry.key().clone(), is_header, content)?;
                occupied_entry.get_mut().merge(f);
                Ok(())
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                let f = parser::parse(vacant_entry.key().clone(), is_header, content)?;
                vacant_entry.insert(f);
                Ok(())
            }
        }
    }

    pub fn add_file(
        &mut self,
        file_path: impl AsRef<Path>,
        assembly_name: String,
        is_header: bool,
    ) -> pura_lingua::global::Result<()> {
        let file_path = file_path.as_ref();
        self.add_direct(
            &std::fs::read_to_string(file_path)?,
            assembly_name,
            is_header,
        )
    }

    pub fn add_glob(
        &mut self,
        pattern: &str,
        assembly_name: String,
        is_header: bool,
    ) -> pura_lingua::global::Result<()> {
        for p in glob::glob(pattern)? {
            let p = p?;
            if p.is_dir() {
                continue;
            }
            self.add_file(&p, assembly_name.clone(), is_header)?;
        }

        Ok(())
    }

    pub fn add_core(&mut self) -> pura_lingua::global::Result<()> {
        self.add_language_specific_dependency("Core")
    }

    pub fn add_language_specific_dependency(
        &mut self,
        dep: &str,
    ) -> pura_lingua::global::Result<()> {
        let mut dir = lang_spec_dependency_path(dep)?;
        dir.push("src");
        dir.push("*.pl_middle_ir");
        self.add_glob(
            dir.to_str().unwrap(),
            if dep == "Core" {
                "!".to_owned()
            } else {
                format!("MiddleIR::{dep}")
            },
            dep == "Core",
        )
    }

    pub fn resolve_type_reference(&self, r: &TypeReference) -> Option<&TypeDef> {
        if r.is_generic {
            return None;
        }
        let assembly_name = r.assembly_name.as_ref().unwrap();
        self.files
            .get(assembly_name)?
            .types
            .iter()
            .find(|x| (x.name() == &r.name) && r.index.is_none_or(|y| x.index() == y))
    }

    pub fn compile(mut self) -> pura_lingua::global::Result<HashMap<String, Assembly>> {
        for f in self.files.values_mut() {
            f.sort();
        }
        fn map_type_reference(
            this: &CompileContext,
            assembly: &mut Assembly,
            current_file: &File,
            r: &TypeReference,
        ) -> Result<TypeToken, ParseError> {
            if r.is_generic {
                return Ok(TypeTokenBuilder::new_without_defaults()
                    .with_ty(TypeType::Generic)
                    .with_index(r.index.unwrap())
                    .build());
            }

            let assembly_name = r.assembly_name.as_ref().unwrap();
            if assembly_name == &current_file.assembly_name {
                let id = current_file
                    .types
                    .iter()
                    .find_map(|x| {
                        if x.name() == &r.name {
                            Some(x.index())
                        } else {
                            None
                        }
                    })
                    .ok_or(ParseError::TypeNotFound(r.clone()))?;
                return Ok(TypeTokenBuilder::new_without_defaults()
                    .with_ty(TypeType::TypeDef)
                    .with_index(id)
                    .build());
            }
            let f = this
                .files
                .get(assembly_name)
                .ok_or_else(|| ParseError::AssemblyNotFound(assembly_name.clone()))?;
            let id = f
                .types
                .iter()
                .find_map(|x| {
                    if x.name() == &r.name {
                        Some(x.index())
                    } else {
                        None
                    }
                })
                .ok_or(ParseError::TypeNotFound(r.clone()))?;
            let ty_ref = TypeRef {
                assembly: assembly.add_string(assembly_name),
                index: id,
            };
            let ty_ref_index = assembly.add_type_ref(ty_ref);
            if r.generics.is_empty() {
                Ok(TypeTokenBuilder::new_without_defaults()
                    .with_ty(TypeType::TypeRef)
                    .with_index(ty_ref_index)
                    .build())
            } else {
                let generics = r
                    .generics
                    .iter()
                    .map(|x| map_type_reference(this, assembly, current_file, x))
                    .try_collect()?;
                let spec = TypeSpec {
                    ty: TypeTokenBuilder::new_without_defaults()
                        .with_ty(TypeType::TypeRef)
                        .with_index(ty_ref_index)
                        .build(),
                    generics,
                };
                assembly.type_specs.push(spec);
                Ok(TypeTokenBuilder::new_without_defaults()
                    .with_ty(TypeType::TypeSpec)
                    .with_index(assembly.type_specs.len() as u32 - 1)
                    .build())
            }
        }

        fn map_field_reference(
            _this: &CompileContext,
            _assembly: &mut Assembly,
            _current_file: &File,
            container: Option<&TypeDef>,
            is_static: bool,
            r: &Either<u32, Identifier>,
        ) -> Result<u32, ParseError> {
            match r {
                Either::Left(x) => Ok(*x),
                Either::Right(rr) if let Some(container) = container => container
                    .fields()
                    .iter()
                    .find_map(|x| {
                        if (&x.name == rr) && (x.attr.is_static() == is_static) {
                            Some(x.index)
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| ParseError::FieldNotFound(rr.var.clone())),
                Either::Right(rr) => {
                    eprintln!("Container not found");
                    Err(ParseError::FieldNotFound(rr.var.clone()))
                }
            }
        }

        fn map_method_reference(
            this: &CompileContext,
            assembly: &mut Assembly,
            current_file: &File,
            container: Option<&TypeDef>,
            r: &MethodReference,
        ) -> Result<MethodToken, ParseError> {
            let index = match &r.index {
                Either::Left(index) => *index,
                Either::Right(rr) if let Some(container) = container => container
                    .methods()
                    .iter()
                    .find_map(|x| if &x.name == rr { Some(x.index) } else { None })
                    .ok_or_else(|| ParseError::MethodNotFound(r.clone()))?,
                _ => return Err(ParseError::MethodNotFound(r.clone())),
            };
            if r.generics.is_empty() {
                Ok(MethodTokenBuilder::new_without_defaults()
                    .with_ty(MethodType::Method)
                    .with_index(index)
                    .build())
            } else {
                let generics = r
                    .generics
                    .iter()
                    .map(|r| map_type_reference(this, assembly, current_file, r))
                    .try_collect()?;

                let spec = MethodSpec { m: index, generics };

                assembly.method_specs.push(spec);

                Ok(MethodTokenBuilder::new_without_defaults()
                    .with_ty(MethodType::MethodSpec)
                    .with_index(assembly.method_specs.len() as u32 - 1)
                    .build())
            }
        }

        fn map_method(
            this: &CompileContext,
            assembly: &mut Assembly,
            current_file: &File,
            current_ty: &TypeDef,
            method: &Method,
        ) -> pura_lingua::global::Result<binary::ty::Method> {
            fn map_variable(
                var: &Identifier,
                locals: &[Identifier],
            ) -> Result<RegisterAddr, ParseError> {
                Ok(RegisterAddr::new(
                    locals
                        .iter()
                        .position(|x| x == var)
                        .ok_or(ParseError::VarNotFound(var.var.clone()))?
                        as u64,
                ))
            }
            fn resolve_variable_type<'a>(
                var: &Identifier,
                this: &'a CompileContext,
                method: &Method,
            ) -> Option<&'a TypeDef> {
                let r = method
                    .attr
                    .local_variable_types()
                    .get(map_variable(var, &method.locals).ok()?.get_usize())?;
                this.resolve_type_reference(r)
            }
            Ok(binary::ty::Method {
                name: assembly.add_string(method.name.as_ref()),
                attr: method
                    .attr
                    .clone()
                    .try_map_types(|x| map_type_reference(this, assembly, current_file, &x))?,
                args: method
                    .args
                    .iter()
                    .map(|orig| {
                        Ok::<_, ParseError>(binary::ty::Parameter {
                            ty: map_type_reference(this, assembly, current_file, &orig.ty)?,
                            attr: orig.attr,
                        })
                    })
                    .try_collect()?,
                return_type: map_type_reference(this, assembly, current_file, &method.return_type)?,
                call_convention: method.call_conv,
                generic_bounds: None, // TODO
                instructions: method
                    .statements
                    .iter()
                    .map(|x| -> compiler_base::AnyResult<Instruction<_, _, _, _>> {
                        match x {
                            ast::method::Statement::Load { literal, var } => {
                                let register_addr = RegisterAddr::new(
                                    method
                                        .locals
                                        .iter()
                                        .position(|x| x == var)
                                        .ok_or(ParseError::VarNotFound(var.var.clone()))?
                                        as u64,
                                );
                                match literal {
                                    &ast::method::Literal::U8(val) => {
                                        Ok(Instruction::Load_u8 { register_addr, val })
                                    }
                                    &ast::method::Literal::U16(val) => {
                                        Ok(Instruction::Load_u16 { register_addr, val })
                                    }
                                    &ast::method::Literal::U32(val) => {
                                        Ok(Instruction::Load_u32 { register_addr, val })
                                    }
                                    &ast::method::Literal::U64(val) => {
                                        Ok(Instruction::Load_u64 { register_addr, val })
                                    }
                                    &ast::method::Literal::I8(val) => {
                                        Ok(Instruction::Load_i8 { register_addr, val })
                                    }
                                    &ast::method::Literal::I16(val) => {
                                        Ok(Instruction::Load_i16 { register_addr, val })
                                    }
                                    &ast::method::Literal::I32(val) => {
                                        Ok(Instruction::Load_i32 { register_addr, val })
                                    }
                                    &ast::method::Literal::I64(val) => {
                                        Ok(Instruction::Load_i64 { register_addr, val })
                                    }
                                    ast::method::Literal::String(val) => {
                                        Ok(Instruction::Load_String {
                                            register_addr,
                                            val: assembly.add_string(val),
                                        })
                                    }
                                    ast::method::Literal::Char(_val) => todo!(),
                                    ast::method::Literal::ByteString(_val) => todo!(),
                                    ast::method::Literal::Byte(_val) => todo!(),
                                    ast::method::Literal::True => {
                                        Ok(Instruction::LoadTrue { register_addr })
                                    }
                                    ast::method::Literal::False => {
                                        Ok(Instruction::LoadFalse { register_addr })
                                    }
                                    ast::method::Literal::This => {
                                        Ok(Instruction::LoadThis { register_addr })
                                    }
                                }
                            }
                            ast::method::Statement::LoadTypeValueSize { ty, var } => {
                                let register_addr = map_variable(var, &method.locals)?;
                                let ty = map_type_reference(this, assembly, current_file, ty)?;
                                Ok(Instruction::LoadTypeValueSize { register_addr, ty })
                            }
                            ast::method::Statement::ReadPointerTo {
                                ptr,
                                size,
                                destination,
                            } => {
                                let ptr = map_variable(ptr, &method.locals)?;
                                let size = map_variable(size, &method.locals)?;
                                let destination = map_variable(destination, &method.locals)?;
                                Ok(Instruction::ReadPointerTo {
                                    ptr,
                                    size,
                                    destination,
                                })
                            }
                            ast::method::Statement::WritePointer { source, size, ptr } => {
                                let source = map_variable(source, &method.locals)?;
                                let size = map_variable(size, &method.locals)?;
                                let ptr = map_variable(ptr, &method.locals)?;
                                Ok(Instruction::WritePointer { source, size, ptr })
                            }
                            ast::method::Statement::Check {
                                kind,
                                to_check,
                                result,
                            } => {
                                let register_addr = map_variable(result, &method.locals)?;
                                let to_check = map_variable(to_check, &method.locals)?;
                                match kind {
                                    ast::method::CheckKind::AllZero => Ok(Instruction::IsAllZero {
                                        register_addr,
                                        to_check,
                                    }),
                                }
                            }
                            ast::method::Statement::NewObject {
                                ty,
                                ctor,
                                args,
                                result,
                            } => {
                                let ty_token =
                                    map_type_reference(this, assembly, current_file, ty)?;
                                let ctor_name = map_method_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    this.resolve_type_reference(ty),
                                    ctor,
                                )?;
                                let args = args
                                    .iter()
                                    .map(|x| map_variable(x, &method.locals))
                                    .try_collect()?;
                                let register_addr = map_variable(result, &method.locals)?;
                                Ok(Instruction::NewObject {
                                    ty: ty_token,
                                    ctor_name,
                                    args,
                                    register_addr,
                                })
                            }
                            ast::method::Statement::NewArray {
                                element_ty,
                                len,
                                result,
                            } => {
                                let element_type =
                                    map_type_reference(this, assembly, current_file, element_ty)?;
                                let register_addr = map_variable(result, &method.locals)?;
                                match len {
                                    Either::Left(len) => {
                                        let len_addr = map_variable(len, &method.locals)?;
                                        Ok(Instruction::NewDynamicArray {
                                            element_type,
                                            len_addr,
                                            register_addr,
                                        })
                                    }
                                    &Either::Right(len) => Ok(Instruction::NewArray {
                                        element_type,
                                        len,
                                        register_addr,
                                    }),
                                }
                            }
                            ast::method::Statement::InstanceCall {
                                val,
                                method: method_ref,
                                args,
                                result,
                            } => {
                                let val_addr = map_variable(val, &method.locals)?;
                                let method_ref = map_method_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    resolve_variable_type(val, this, method),
                                    method_ref,
                                )?;
                                let args = args
                                    .iter()
                                    .map(|x| map_variable(x, &method.locals))
                                    .try_collect()?;
                                let ret_at = map_variable(result, &method.locals)?;
                                Ok(Instruction::InstanceCall {
                                    val: val_addr,
                                    method: method_ref,
                                    args,
                                    ret_at,
                                })
                            }
                            ast::method::Statement::StaticCall {
                                ty,
                                method: method_ref,
                                args,
                                result,
                            } => {
                                let ty_token =
                                    map_type_reference(this, assembly, current_file, ty)?;
                                let method_ref = map_method_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    this.resolve_type_reference(ty),
                                    method_ref,
                                )?;
                                let args = args
                                    .iter()
                                    .map(|x| map_variable(x, &method.locals))
                                    .try_collect()?;
                                let ret_at = map_variable(result, &method.locals)?;
                                Ok(Instruction::StaticCall {
                                    ty: ty_token,
                                    method: method_ref,
                                    args,
                                    ret_at,
                                })
                            }
                            ast::method::Statement::NonPurusCall {
                                config,
                                f_pointer,
                                args,
                                result,
                            } => {
                                let f_pointer = map_variable(f_pointer, &method.locals)?;
                                let config = map_variable(config, &method.locals)?;
                                let args = args
                                    .iter()
                                    .map(|x| map_variable(x, &method.locals))
                                    .try_collect()?;
                                let ret_at = map_variable(result, &method.locals)?;
                                Ok(Instruction::DynamicNonPurusCall {
                                    f_pointer,
                                    config,
                                    args,
                                    ret_at,
                                })
                            }
                            ast::method::Statement::LoadArg { arg, local } => {
                                let register_addr = map_variable(local, &method.locals)?;
                                Ok(Instruction::LoadArg {
                                    register_addr,
                                    arg: *arg,
                                })
                            }
                            ast::method::Statement::LoadStatic { ty, field, local } => {
                                let register_addr = map_variable(local, &method.locals)?;
                                let ty_token =
                                    map_type_reference(this, assembly, current_file, ty)?;
                                let field = map_field_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    this.resolve_type_reference(ty),
                                    true,
                                    field,
                                )?;
                                Ok(Instruction::LoadStatic {
                                    register_addr,
                                    ty: ty_token,
                                    field,
                                })
                            }
                            ast::method::Statement::LoadField {
                                container,
                                field,
                                local,
                            } => {
                                let container_addr = map_variable(container, &method.locals)?;
                                let field = map_field_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    resolve_variable_type(container, this, method),
                                    false,
                                    field,
                                )?;
                                let register_addr = map_variable(local, &method.locals)?;
                                Ok(Instruction::LoadField {
                                    container: container_addr,
                                    field,
                                    register_addr,
                                })
                            }
                            ast::method::Statement::SetThisField { val, field } => {
                                let val_addr = map_variable(val, &method.locals)?;
                                let field = map_field_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    Some(current_ty),
                                    false,
                                    field,
                                )?;
                                Ok(Instruction::SetThisField { val_addr, field })
                            }
                            ast::method::Statement::SetStaticField { val, ty, field } => {
                                let val_addr = map_variable(val, &method.locals)?;
                                let ty_token =
                                    map_type_reference(this, assembly, current_file, ty)?;
                                let field = map_field_reference(
                                    this,
                                    assembly,
                                    current_file,
                                    this.resolve_type_reference(ty),
                                    true,
                                    field,
                                )?;
                                Ok(Instruction::SetStaticField {
                                    val_addr,
                                    ty: ty_token,
                                    field,
                                })
                            }
                            ast::method::Statement::Throw { val } => {
                                let exception_addr = map_variable(val, &method.locals)?;
                                Ok(Instruction::Throw { exception_addr })
                            }
                            ast::method::Statement::ReturnVal { val } => {
                                let register_addr = map_variable(val, &method.locals)?;
                                Ok(Instruction::ReturnVal { register_addr })
                            }
                            ast::method::Statement::Jump { condition, ty, val } => {
                                let target = JumpTargetBuilder::new_without_defaults()
                                    .with_ty(match ty {
                                        ast::method::JumpTargetType::Absolute => pura_lingua::global::instruction::JumpTargetType::Absolute,
                                        ast::method::JumpTargetType::Forward => pura_lingua::global::instruction::JumpTargetType::Forward,
                                        ast::method::JumpTargetType::Backward => pura_lingua::global::instruction::JumpTargetType::Backward,
                                    })
                                    .with_val(*val)
                                    .build();
                                match condition {
                                    ast::method::JumpCondition::Unconditional => Ok(Instruction::Jump { target }),
                                    ast::method::JumpCondition::IfTrue(val) => {
                                        let register_addr = map_variable(val, &method.locals)?;
                                        Ok(Instruction::JumpIf { register_addr, target })
                                    },
                                    ast::method::JumpCondition::CheckSuccess(
                                        check_kind,
                                        to_check,
                                    ) => match check_kind {
                                        ast::method::CheckKind::AllZero => {
                                            let to_check = map_variable(to_check, &method.locals)?;
                                            Ok(Instruction::JumpIfAllZero { to_check, target })
                                        },
                                    },
                                    ast::method::JumpCondition::CheckFailure(
                                        check_kind,
                                        to_check,
                                    ) => match check_kind {
                                        ast::method::CheckKind::AllZero => {
                                            let to_check = map_variable(to_check, &method.locals)?;
                                            Ok(Instruction::JumpIfNotAllZero { to_check, target })
                                        }
                                    },
                                }
                            }
                            ast::method::Statement::Nop => Ok(Instruction::Nop),
                        }
                    })
                    .try_collect()?,
            })
        }

        fn map_field(
            this: &CompileContext,
            assembly: &mut Assembly,
            current_file: &File,
            field: &Field,
        ) -> pura_lingua::global::Result<binary::ty::Field> {
            Ok(binary::ty::Field {
                name: assembly.add_string(field.name.as_ref()),
                attr: field.attr,
                ty: map_type_reference(this, assembly, current_file, &field.ty)?,
            })
        }

        let mut result = HashMap::new();

        for (name, file) in &self.files {
            if file.is_header {
                continue;
            }
            let mut assem = Assembly::new(name);
            for ty in &file.types {
                match ty {
                    ast::ty::TypeDef::Class(class_def) => {
                        let marshaled = binary::ty::ClassDef {
                            main: class_def.main,
                            name: assem.add_string(class_def.name.as_ref()),
                            attr: class_def.attr,
                            parent: class_def
                                .parent
                                .as_ref()
                                .map(|r| map_type_reference(&self, &mut assem, file, r))
                                .transpose()?,
                            method_table: class_def
                                .methods
                                .iter()
                                .map(|x| map_method(&self, &mut assem, file, ty, x))
                                .try_collect()?,
                            fields: class_def
                                .fields
                                .iter()
                                .map(|x| map_field(&self, &mut assem, file, x))
                                .try_collect()?,
                            sctor: None,
                            generic_bounds: None, // TODO
                        };
                        assem.type_defs.push(binary::ty::TypeDef::Class(marshaled));
                    }
                    ast::ty::TypeDef::Struct(struct_def) => {
                        let marshaled = binary::ty::StructDef {
                            name: assem.add_string(struct_def.name.as_ref()),
                            attr: struct_def.attr,
                            method_table: struct_def
                                .methods
                                .iter()
                                .map(|x| map_method(&self, &mut assem, file, ty, x))
                                .try_collect()?,
                            fields: struct_def
                                .fields
                                .iter()
                                .map(|x| map_field(&self, &mut assem, file, x))
                                .try_collect()?,
                            sctor: None,
                            generic_bounds: None, // TODO
                        };
                        assem.type_defs.push(binary::ty::TypeDef::Struct(marshaled));
                    }
                }
            }
            debug_assert!(result.insert(name.clone(), assem).is_none());
        }

        Ok(result)
    }
}
