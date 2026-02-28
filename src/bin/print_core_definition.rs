#![feature(maybe_uninit_uninit_array_transpose)]
#![feature(maybe_uninit_array_assume_init)]

use std::mem::MaybeUninit;

use line_ending::LineEnding;
use pura_lingua::{
    global::{
        self,
        attrs::{FieldAttr, ParameterAttr, TypeAttr, Visibility},
    },
    stdlib_header::{CoreTypeId, CoreTypeRef},
    stdlib_header_serde::{
        CoreTypeKind, GenericCount, GetCoreTypeInfo,
        definitions::{CoreTypeInfo, InfoMethodAttr, MethodInfo},
    },
};

fn map_visibility(x: Visibility) -> &'static str {
    match x {
        Visibility::Public => "public",
        Visibility::Private => "private",
        Visibility::AssemblyOnly => "internal",
    }
}

fn map_type_attr(attr: TypeAttr) -> String {
    std::iter::once(map_visibility(attr.vis()))
        .chain(
            match attr.specific() {
                global::attrs::TypeSpecificAttr::Class(bit_flags) => bit_flags
                    .iter()
                    .map(|x| match x {
                        global::attrs::ClassImplementationFlags::Static => "static",
                        global::attrs::ClassImplementationFlags::Partial => "partial",
                    })
                    .collect::<Vec<_>>(),
                global::attrs::TypeSpecificAttr::Struct(bit_flags) => bit_flags
                    .iter()
                    .map(|x| match x {
                        global::attrs::StructImplementationFlags::Ref => "ref",
                        global::attrs::StructImplementationFlags::Partial => "partial",
                    })
                    .collect::<Vec<_>>(),
                global::attrs::TypeSpecificAttr::Interface(bit_flags) => bit_flags
                    .iter()
                    .map::<&'static str, _>(|x| match x {
                        global::attrs::InterfaceImplementationFlags::__ => unreachable!(),
                    })
                    .collect::<Vec<_>>(),
            }
            .into_iter(),
        )
        .collect::<Vec<_>>()
        .join(" ")
}

fn map_field_attr(attr: FieldAttr) -> String {
    std::iter::once(map_visibility(attr.vis()))
        .chain(attr.impl_flags().iter().map(|x| match x {
            global::attrs::FieldImplementationFlags::Static => "static",
        }))
        .collect::<Vec<_>>()
        .join(" ")
}

fn map_method_attr(attr: &InfoMethodAttr) -> String {
    std::iter::once(map_visibility(attr.vis))
        .chain(attr.impl_flags.iter().map(|x| match x {
            global::attrs::MethodImplementationFlags::Static => "static",
            global::attrs::MethodImplementationFlags::ImplementedByRuntime => "",
            global::attrs::MethodImplementationFlags::HideWhenCapturing => "hide_when_capturing",
        }))
        .collect::<Vec<_>>()
        .join(" ")
}

fn map_parameter_attr(attr: ParameterAttr) -> String {
    attr.impl_flags()
        .iter()
        .map(|x| match x {
            global::attrs::ParameterImplementationFlags::ByRef => "ref",
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn map_generic_count(c: GenericCount) -> String {
    let mut additional = "";
    if c.is_infinite {
        additional = "+";
    }
    format!(
        "[{}{additional}]",
        (0..c.count)
            .map(|x| format!("\"{x}\""))
            .collect::<Vec<_>>()
            .join(" ")
    )
}

fn map_core_type_ref(r: &CoreTypeRef) -> String {
    match r {
        CoreTypeRef::Core(core_type_id) => format!("[!]\"{}\"", core_type_id.name()),
        CoreTypeRef::WithGeneric(core_type_id, core_type_refs) => {
            format!(
                "[!]\"{}\"<{}>",
                core_type_id.name(),
                core_type_refs
                    .iter()
                    .map(map_core_type_ref)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        CoreTypeRef::Generic(x) => format!("\"{x}\""),
    }
}

fn map_method(x: &MethodInfo) -> String {
    format!(
        "
    method[{id} {attr}] \"{name}\"{generic_count}({parameters}) -> {return_type}
    {{
        // impl by runtime
    }}",
        id = x.id,
        attr = map_method_attr(&x.attr),
        name = x.name,
        generic_count = x.generic_count.map(map_generic_count).unwrap_or_default(),
        parameters = x
            .args
            .iter()
            .map(|x| format!(
                "[{attr}] {ty}",
                attr = map_parameter_attr(x.attr),
                ty = map_core_type_ref(&x.ty)
            ))
            .collect::<Vec<_>>()
            .join(", "),
        return_type = map_core_type_ref(&x.return_type),
    )
}

fn map_core_type_info(info: CoreTypeInfo) -> global::Result<String> {
    Ok(format!(
        "{kind}[{id} {attr}] \"{name}\"{generic_count}{parent}
{{
// Fields
{fields}
// Methods
{methods}
// Static methods
{static_methods}
}}
",
        kind = match info.kind {
            CoreTypeKind::Class => "class",
            CoreTypeKind::Struct => "struct",
        },
        id = info.id as u32,
        attr = map_type_attr(info.attr),
        name = info.name,
        generic_count = info
            .generic_count
            .map(map_generic_count)
            .unwrap_or_default(),
        parent = info
            .parent
            .as_ref()
            .map(map_core_type_ref)
            .map(|x| format!(": {x}"))
            .unwrap_or_default(),
        fields = info
            .fields
            .iter()
            .map(|x| format!(
                "\tfield[{id} {attr}] \"{name}\": {ty};",
                id = x.id,
                attr = map_field_attr(x.attr),
                name = x.name,
                ty = map_core_type_ref(&x.ty),
            ))
            .fold(String::new(), |mut out, mut x| {
                unsafe {
                    out.as_mut_vec().append(x.as_mut_vec());
                }
                out.push_str(LineEnding::from_current_platform().as_str());
                out
            }),
        methods = info
            .methods
            .iter()
            .map(map_method)
            .fold(String::new(), |mut out, mut x| {
                unsafe {
                    out.as_mut_vec().append(x.as_mut_vec());
                }
                out.push_str(LineEnding::from_current_platform().as_str());
                out
            }),
        static_methods =
            info.static_methods
                .iter()
                .map(map_method)
                .fold(String::new(), |mut out, mut x| {
                    unsafe {
                        out.as_mut_vec().append(x.as_mut_vec());
                    }
                    out.push_str(LineEnding::from_current_platform().as_str());
                    out
                }),
    ))
}

fn main() -> global::Result<()> {
    let mut result_iter = CoreTypeId::ALL_VARIANTS
        .into_iter()
        .map(GetCoreTypeInfo::get_core_type_info);
    let mut result = MaybeUninit::<[String; CoreTypeId::ALL_VARIANTS.len()]>::uninit().transpose();
    let mut i = 0usize;
    while let Some(x) = result_iter.next() {
        result[i] = MaybeUninit::new(map_core_type_info(x())?);
        i += 1;
    }
    let result = unsafe { MaybeUninit::array_assume_init(result) }
        .join(LineEnding::from_current_platform().as_str());
    if std::env::args()
        .nth(1)
        .is_some_and(|x| x.as_str() == "--to-stdlib")
    {
        let mut dir = Compiler_MiddleIR::core_dir()?;
        dir.push("src");
        std::fs::create_dir_all(&dir)?;
        std::fs::write(dir.join("Core.pl_middle_ir"), result)?;
    }
    Ok(())
}
