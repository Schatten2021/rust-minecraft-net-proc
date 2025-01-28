use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{GenericArgument, PathArguments};

pub enum FieldType {
    None,
    Boolean,
    Byte,
    UByte,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    UUID,
    Float,
    Double,
    String,
    Identifier,
    Angle,
    VarInt,
    VarLong,
    PrefixedArray(Box<FieldType>),
    PrefixedOptional(Box<FieldType>),
    Other(syn::Path),
}
impl Parse for FieldType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path = input.parse::<syn::Path>()?;
        Ok(Self::from_path(path))
    }
}
impl FieldType {
    pub fn get_encoder(&self, val_ref: impl ToTokens) -> TokenStream {
        match self {
            Self::Boolean => quote! {crate::fields::encode_bool(#val_ref)},
            Self::Byte => quote! {crate::fields::encode_byte(#val_ref)},
            Self::UByte => quote! {crate::fields::encode_ubyte(#val_ref)},
            Self::Short => quote! {crate::fields::encode_short(#val_ref)},
            Self::UShort => quote! {crate::fields::encode_ushort(#val_ref)},
            Self::Int => quote! {crate::fields::encode_int(#val_ref)},
            Self::UInt => quote! {crate::fields::encode_uint(#val_ref)},
            Self::Long => quote! {crate::fields::encode_long(#val_ref)},
            Self::UUID => quote! {crate::fields::encode_uuid(#val_ref)},
            Self::Float => quote! {crate::fields::encode_float(#val_ref)},
            Self::Double => quote! {crate::fields::encode_double(#val_ref)},
            Self::String => quote! {crate::fields::encode_identifier(#val_ref .to_string())},
            Self::Identifier => quote! {crate::fields::encode_identifier(#val_ref .to_string())},
            Self::Angle => quote! {crate::fields::encode_angle(#val_ref)},
            Self::VarInt => quote! {crate::fields::encode_var_int(#val_ref)},
            Self::VarLong => quote! {crate::fields::encode_var_long(#val_ref)},
            Self::PrefixedArray(inner) => {
                let inner_encoder = inner.get_encoder(quote!{ v.clone() });
                quote! { vec![
                    crate::fields::encode_var_int(#val_ref.len() as i32),
                    #val_ref.iter().flat_map(|v| #inner_encoder).collect::<Vec<u8>>()
                ].iter().cloned().flatten().collect::<Vec<u8>>()}
            },
            Self::PrefixedOptional(inner) => {
                let inner_encoder = inner.get_encoder(quote! { v });
                quote! {
                    if let Some(v) = #val_ref.clone() {
                        vec![crate::fields::encode_bool(true), #inner_encoder].iter().cloned().flatten().collect::<Vec<u8>>()
                    } else {
                        crate::fields::encode_bool(false)
                    }
                }
            },
            FieldType::Other(_) => quote! {crate::Field::to_bytes(&#val_ref)},
            FieldType::None => panic!("no encoder associated with None field")
        }
    }
    pub fn get_decoder(&self) -> TokenStream {
        match self {
            FieldType::Boolean => quote! { reader.read_bool()? },
            FieldType::Byte => quote! { reader.read_byte() },
            FieldType::UByte => quote! { reader.read_ubyte() },
            FieldType::Short => quote! { reader.read_short() },
            FieldType::UShort => quote! { reader.read_ushort() },
            FieldType::Int => quote! { reader.read_int() },
            FieldType::UInt => quote! { reader.read_uint() },
            FieldType::Long => quote! { reader.read_long() },
            FieldType::UUID => quote! { reader.read_uuid() },
            FieldType::Float => quote! { reader.read_float() },
            FieldType::Double => quote! { reader.read_double() },
            FieldType::String => quote! { reader.read_string()? },
            FieldType::Identifier => quote! { reader.read_identifier()? },
            FieldType::Angle => quote! { reader.read_angle() },
            FieldType::VarInt => quote! { reader.read_var_int()? },
            FieldType::VarLong => quote! { reader.read_var_long()? },
            FieldType::PrefixedArray(inner) => {
                let inner_decoder = inner.get_decoder();
                quote! {
                    {
                        let len = reader.read_var_int()?;
                        let mut v = Vec::with_capacity(len as usize);
                        for _ in 0..len {
                            v.push(#inner_decoder);
                        }
                        v
                    }
                }
            }
            FieldType::PrefixedOptional(inner) => {
                let inner_decoder = inner.get_decoder();
                quote! { if reader.read_bool()? {Some(#inner_decoder)} else {None} }
            }
            FieldType::Other(_) => quote! {crate::Field::from_reader(reader)?},
            FieldType::None => panic!("no decoder associated with None field")
        }
    }
    pub fn from_path(path: syn::Path) -> Self {
        let last_segment = path.segments.last().expect("path has no last segment");
        let inner = match &last_segment.arguments {
            PathArguments::AngleBracketed(inner) => match inner.args.last() {
                Some(GenericArgument::Type(syn::Type::Path(path))) => Some(Box::new(Self::from_path(path.clone().path))),
                None => None,
                _ => panic!("Invalid type")
            }
            PathArguments::None => None,
            _ => panic!("invalid type"),
        };
        let t = match last_segment.ident.to_string().as_str() {
            "bool" => Self::Boolean,
            "Byte" => Self::Byte,
            "UByte" => Self::UByte,
            "Short" => Self::Short,
            "UShort" => Self::UShort,
            "Int" => Self::Int,
            "UInt" => Self::UInt,
            "Long" => Self::Long,
            "UUID" => Self::UUID,
            "Float" => Self::Float,
            "Double" => Self::Double,
            "String" => Self::String,
            "Identifier" => Self::Identifier,
            "Angle" => Self::Angle,
            "VarInt" => Self::VarInt,
            "VarLong" => Self::VarLong,
            "PrefixedArray" => Self::PrefixedArray(inner.expect("PrefixedArray requires inner type")),
            "PrefixedOptional" => Self::PrefixedOptional(inner.expect("PrefixedOptional requires inner type")),
            _ => Self::Other(path),
        };
        t
    }
    pub fn is_none(&self) -> bool {
        match self {
            FieldType::None => true,
            _ => false
        }
    }
}
impl ToTokens for FieldType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match self {
            Self::Boolean => quote! { bool },
            Self::Byte => quote! { crate::fields::types::Byte },
            Self::UByte => quote! { crate::fields::types::UByte },
            Self::Short => quote! { crate::fields::types::Short },
            Self::UShort => quote! { crate::fields::types::UShort },
            Self::Int => quote! { crate::fields::types::Int },
            Self::UInt => quote! { crate::fields::types::UInt },
            Self::Long => quote! { crate::fields::types::Long },
            Self::UUID => quote! { crate::fields::types::UUID },
            Self::Float => quote! { crate::fields::types::Float },
            Self::Double => quote! { crate::fields::types::Double },
            Self::String => quote! { String },
            Self::Identifier => quote! { crate::fields::types::Identifier },
            Self::Angle => quote! { crate::fields::types::Angle },
            Self::VarInt => quote! { crate::fields::types::VarInt },
            Self::VarLong => quote! { crate::fields::types::VarLong },
            Self::PrefixedArray(inner) => quote! { crate::fields::types::PrefixedArray<#inner> },
            Self::PrefixedOptional(inner) => quote! { crate::fields::types::PrefixedOptional<#inner> },
            Self::Other(i) => i.to_token_stream(),
            Self::None => TokenStream::new(),
        });
    }
}
pub struct Field {
    pub name: syn::Ident,
    pub r#type: FieldType,
}
impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut input: CustomParseStream = input.into();
        let name = input.parse::<syn::Ident>()?;
        if !input.try_consume::<syn::Token![:]>() {
            input.try_consume::<syn::Token![,]>();
            return Ok(Self {
                name,
                r#type: FieldType::None,
            })
        }
        let r#type = input.parse::<FieldType>()?;
        input.try_consume::<syn::Token![,]>();
        Ok(Self {
            name,
            r#type,
        })
    }
}
impl Field {
    pub fn get_struct_encoder(&self) -> TokenStream {
        let name = &self.name;
        self.r#type.get_encoder(quote! {self.#name})
    }
    pub fn get_struct_decoder(&self) -> TokenStream {
        self.r#type.get_decoder()
    }
    pub fn get_enum_encoder(&self, index: usize) -> TokenStream {
        let name = &self.name;
        let ty = match &self.r#type {
            FieldType::None => return quote! {Self::#name => crate::fields::encode_var_int(#index as i32)},
            ty => match std::panic::catch_unwind(|| ty.get_encoder(quote! {v})) {
                Ok(encoder) => encoder,
                Err(e) => {println!("error while trying to get encoder for field {}", self.name); std::panic::resume_unwind(e) },
            },
        };
        quote! {Self::#name(v) => {
            vec![crate::fields::encode_var_int(#index as i32), #ty].iter().flatten().cloned().collect::<Vec<u8>>()
        }}
    }
    pub fn get_enum_decoder(&self, index: usize) -> TokenStream {
        let index = index as i32;
        let name = &self.name;
        let ty = match &self.r#type {
            FieldType::None => return quote! { #index => Self::#name },
            ty => ty,
        };
        let decoder = ty.get_decoder();
        quote! { #index => Self::#name(#decoder) }
    }
}

pub struct CustomParseStream<'a> {
    inner: syn::parse::ParseStream<'a>,
}
impl<'a> CustomParseStream<'a> {
    pub fn new(input: syn::parse::ParseStream<'a>) -> Self {
        Self { inner: input }
    }
    pub fn try_consume<T: Parse >(&mut self) -> bool {
        if let Err(_) = self.inner.fork().parse::<T>() {
            false
        } else {
            self.inner.parse::<T>().expect("could parse on fork but not original parse stream");
            true
        }
    }
    pub fn expect<T: Parse>(&mut self) -> syn::Result<()> {
        self.inner.parse::<T>()?;
        Ok(())
    }
    pub fn parse<T: syn::parse::Parse>(&mut self) -> syn::Result<T> {
        self.inner.parse::<T>()
    }
}
impl<'a> From<syn::parse::ParseStream<'a>> for CustomParseStream<'a> {
    fn from(input: syn::parse::ParseStream<'a>) -> Self {
        Self { inner: input }
    }
}
