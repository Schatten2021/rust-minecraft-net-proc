use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};

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
    PrefixedArray,
    PrefixedOptional,
    Other(syn::Path),
}
impl Parse for FieldType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path = input.parse::<syn::Path>()?;
        let t = match path.segments.last().expect("path has no last segment").ident.to_string().as_str() {
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
            "PrefixedArray" => Self::PrefixedArray,
            "PrefixedOptional" => Self::PrefixedOptional,
            _ => Self::Other(path),
        };
        Ok(t)
    }
}
pub struct Type {
    pub r#type: FieldType,
    pub inner: Option<Box<Type>>,
}
impl Type {
    pub fn get_encoder(&self, val_ref: impl ToTokens) -> TokenStream {
        match self.r#type {
            FieldType::Boolean => quote! {crate::fields::encode_bool(#val_ref)},
            FieldType::Byte => quote! {crate::fields::encode_byte(#val_ref)},
            FieldType::UByte => quote! {crate::fields::encode_ubyte(#val_ref)},
            FieldType::Short => quote! {crate::fields::encode_short(#val_ref)},
            FieldType::UShort => quote! {crate::fields::encode_ushort(#val_ref)},
            FieldType::Int => quote! {crate::fields::encode_int(#val_ref)},
            FieldType::UInt => quote! {crate::fields::encode_uint(#val_ref)},
            FieldType::Long => quote! {crate::fields::encode_long(#val_ref)},
            FieldType::UUID => quote! {crate::fields::encode_uuid(#val_ref)},
            FieldType::Float => quote! {crate::fields::encode_float(#val_ref)},
            FieldType::Double => quote! {crate::fields::encode_double(#val_ref)},
            FieldType::String => quote! {crate::fields::encode_identifier(#val_ref .clone())},
            FieldType::Identifier => quote! {crate::fields::encode_identifier(#val_ref .clone())},
            FieldType::Angle => quote! {crate::fields::encode_angle(#val_ref)},
            FieldType::VarInt => quote! {crate::fields::encode_var_int(#val_ref)},
            FieldType::VarLong => quote! {crate::fields::encode_var_long(#val_ref)},
            FieldType::PrefixedArray => {
                let inner_encoder = match &self.inner {
                    None => panic!("Prefixed Array requires inner type"),
                    Some(inner) => inner.get_encoder("v"),
                };
                quote! { vec![
                    crate::fields::encode_var_int(#val_ref.len() as i32),
                    #val_ref.iter().cloned().flat_map(|v| #inner_encoder).collect()
                ].iter().cloned().flatten().collect()}
            },
            FieldType::PrefixedOptional => {
                let inner_encoder = match &self.inner {
                    None => panic!("Prefixed Array requires inner type"),
                    Some(inner) => inner.get_encoder("v"),
                };
                quote! {
                    if let Some(v) = #val_ref.clone() {
                        vec![crate::fields::encode_bool(true), #inner_encoder].iter().cloned().flatten().collect()
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
        match self.r#type {
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
            FieldType::PrefixedArray => {
                let inner_decoder = match &self.inner {
                    None => panic!("Prefixed Array requires inner type"),
                    Some(inner) => inner.get_decoder(),
                };
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
            FieldType::PrefixedOptional => {
                let inner_decoder = match &self.inner {
                    None => panic!("Prefixed Optional requires inner type"),
                    Some(inner) => inner.get_decoder(),
                };
                quote! { if reader.read_bool()? {Some(#inner_decoder)} else {None} }
            }
            FieldType::Other(_) => quote! {crate::Field::from_reader(reader)?},
            FieldType::None => panic!("no decoder associated with None field")
        }
    }
}
impl Parse for Type {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut input: CustomParseStream = input.into();
        let r#type = input.parse::<FieldType>()?;
        let inner = if input.try_consume::<syn::Token![<]>() {
            Some(Box::new(input.parse()?))
        } else {
            None
        };
        Ok(Self { r#type, inner })
    }
}
impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match &self.r#type {
            FieldType::Boolean => quote! { bool },
            FieldType::Byte => quote! { crate::fields::types::Byte },
            FieldType::UByte => quote! { crate::fields::types::UByte },
            FieldType::Short => quote! { crate::fields::types::Short },
            FieldType::UShort => quote! { crate::fields::types::UShort },
            FieldType::Int => quote! { crate::fields::types::Int },
            FieldType::UInt => quote! { crate::fields::types::UInt },
            FieldType::Long => quote! { crate::fields::types::Long },
            FieldType::UUID => quote! { crate::fields::types::UUID },
            FieldType::Float => quote! { crate::fields::types::Float },
            FieldType::Double => quote! { crate::fields::types::Double },
            FieldType::String => quote! { String },
            FieldType::Identifier => quote! { crate::fields::types::Identifier },
            FieldType::Angle => quote! { crate::fields::types::Angle },
            FieldType::VarInt => quote! { crate::fields::types::VarInt },
            FieldType::VarLong => quote! { crate::fields::types::VarLong },
            FieldType::PrefixedArray => quote! { crate::fields::types::PrefixedArray },
            FieldType::PrefixedOptional => quote! { crate::fields::types::PrefixedOptional },
            FieldType::Other(i) => i.to_token_stream(),
            FieldType::None => TokenStream::new(),
        });
    }
}
pub struct Field {
    pub name: syn::Ident,
    pub r#type: Option<Type>,
}
impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut input: CustomParseStream = input.into();
        let name = input.parse::<syn::Ident>()?;
        if !input.try_consume::<syn::Token![:]>() {
            input.try_consume::<syn::Token![,]>();
            return Ok(Self {
                name, r#type: None,
            })
        }
        let r#type = input.parse::<Type>()?;
        input.try_consume::<syn::Token![,]>();
        Ok(Self {
            name,
            r#type: Some(r#type),
        })
    }
}
impl Field {
    pub fn get_struct_encoder(&self) -> TokenStream {
        let name = &self.name;
        self.r#type.as_ref().unwrap().get_encoder(quote! {self.#name})
    }
    pub fn get_struct_decoder(&self) -> TokenStream {
        self.r#type.as_ref().unwrap().get_decoder()
    }
    pub fn get_enum_encoder(&self, index: usize) -> TokenStream {
        let name = &self.name;
        let ty = match &self.r#type {
            Some(ty) => ty.get_encoder(quote! {v}),
            None => return quote! {Self::#name => crate::fields::encode_var_int(#index as i32)},
        };
        quote! {Self::#name(v) => {
            vec![crate::fields::encode_var_int(#index as i32), #ty].iter().flatten().cloned().collect::<Vec<u8>>()
        }}
    }
    pub fn get_enum_decoder(&self, index: usize) -> TokenStream {
        let index = index as i32;
        let name = &self.name;
        let ty = match &self.r#type {
            Some(ty) => ty,
            None => return quote! { #index => Self::#name },
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
