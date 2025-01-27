use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::discouraged::AnyDelimiter;
use syn::parse::{Parse, ParseStream};
use syn::{GenericArgument, Token};
use crate::types::Type;

pub enum FieldType {
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
pub struct Field {
    pub r#type: FieldType,
    pub inner: Vec<Box<Field>>,
}
impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let t = syn::Type::parse(input)?;
        Ok(Self::from_type(t))
    }
}

impl Field {
    pub fn from_type(t: syn::Type) -> Field {
        let path = get_path(t);
        let last_segment = path.segments.last().expect("can't auto-encode empty path");
        let ident = last_segment.ident.to_string();
        let r#type = match &*ident {
            "bool" => FieldType::Boolean,
            "Byte" => FieldType::Byte,
            "UByte" => FieldType::UByte,
            "Short" => FieldType::Short,
            "UShort" => FieldType::UShort,
            "Int" => FieldType::Int,
            "UInt" => FieldType::UInt,
            "Long" => FieldType::Long,
            "UUID" => FieldType::UUID,
            "Float" => FieldType::Float,
            "Double" => FieldType::Double,
            "String" => FieldType::String,

            // legacy
            "u8" => FieldType::UByte,
            "i8" => FieldType::Byte,
            "u16" => FieldType::UShort,
            "i16" => FieldType::Short,
            "u128" => FieldType::UUID,
            "f32" => FieldType::Float,
            "f64" => FieldType::Double,
            // default
            _ => FieldType::Other(path.clone())
        };
        let inner = if let syn::PathArguments::AngleBracketed(arguments) = &last_segment.arguments {
            if arguments.args.len() != 1 {
                panic!("can't auto-encode multiple generic arguments");
            };
            arguments.args.iter().filter_map(|arg| {
                if let GenericArgument::Type(t) = arg {
                    Some(Box::new(Field::from_type(t.clone())))
                } else {
                    None
                }
            }).collect()
        } else {
            Vec::new()
        };
        Self {
            r#type,
            inner
        }

    }
    pub fn get_encoder(&self, val_ref: impl ToTokens + Clone) -> proc_macro2::TokenStream {
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
                let inner_encoder = self.get_inner().get_encoder("v");
                quote! { vec![
                    crate::fields::encode_var_int(#val_ref.len() as i32),
                    #val_ref.iter().cloned().flat_map(|v| #inner_encoder).collect()
                ].iter().cloned().flatten().collect()}
            },
            FieldType::PrefixedOptional => {
                let inner_encoder = self.get_inner().get_encoder("v");
                quote! {
                    if let Some(v) = &#val_ref {
                        vec![crate::fields::encode_bool(true), #inner_encoder].iter().cloned().flatten().collect()
                    } else {
                        crate::fields::encode_bool(false)
                    }
                }
            },
            FieldType::Other(_) => quote! {crate::Field::to_bytes(#val_ref)},
        }
    }
    pub fn get_decoder(&self) -> proc_macro2::TokenStream {

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
                let inner_decoder = self.get_inner().get_decoder();
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
                let inner_decoder = self.get_inner().get_decoder();
                quote! {
                    if reader.read_bool()? {Some(#inner_decoder)} else {None}
                }
            }
            FieldType::Other(_) => quote! {crate::Field::from_reader(reader)}
        }
    }
    fn get_inner(&self) -> &Box<Field> {
        if self.inner.len() != 1 {
            panic!("Array expects 1 type argument, found {}", self.inner.len());
        }
        &self.inner[0]
    }
}
impl ToTokens for FieldType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let stream = match &self {
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
            FieldType::String => quote! { crate::fields::types::String },
            FieldType::Identifier => quote! { crate::fields::types::Identifier },
            FieldType::Angle => quote! { crate::fields::types::Angle },
            FieldType::VarInt => quote! { crate::fields::types::VarInt },
            FieldType::VarLong => quote! { crate::fields::types::VarLong },
            FieldType::PrefixedArray => quote! { crate::fields::types::PrefixedArray },
            FieldType::PrefixedOptional => quote! { crate::fields::types::PrefixedOptional },
            FieldType::Other(i) => i.to_token_stream(),
        };
        tokens.extend(stream);
    }
}
fn get_path(t: syn::Type) -> syn::Path {
    match t {
        syn::Type::Path(path) => path.path,
        syn::Type::Tuple(tuple) => {
            if tuple.elems.len() != 1 {
                panic!("Tuples must have exactly one inner type")
            }
            get_path(tuple.elems[0].clone())
        }
        _ => panic!("Invalid type"),
    }
}

pub struct StructField {
    pub name: syn::Ident,
    pub field: Field,
}
impl Parse for StructField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let _: syn::Token![:] = input.parse()?;
        let field: Field = input.parse()?;
        let _ = input.parse::<syn::Token![,]>();
        Ok(Self { name, field })
    }
}
impl StructField {
    pub fn get_encoder(&self) -> proc_macro2::TokenStream {
        self.field.get_encoder(self.name.clone())
    }
    pub fn get_decoder(&self) -> proc_macro2::TokenStream {
        let inner = self.field.get_decoder();
        let name = &self.name;
        quote! { let #name = #inner; }
    }
}
pub struct Struct {
    pub name: syn::Ident,
    pub fields: Vec<StructField>,
}
impl Parse for Struct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _ = if input.peek(Token![pub]) { input.parse::<syn::Token![pub]>().expect("coudn't parse token that existed");} else {};
        let _ = input.parse::<syn::Token![struct]>()?;
        let name: syn::Ident = input.parse()?;
        let _ = input.parse_any_delimiter();
        let mut fields = Vec::new();
        while let Ok(field) = input.parse() {
            fields.push(field);
        }
        Ok(Self { name, fields })
    }
}
impl Struct {
    fn field_implementation(&self) -> proc_macro2::TokenStream {
        let name = &self.name;
        let encoders = self.fields.iter().map(|field| field.get_encoder()).collect::<Vec<_>>();
        let decoders = self.fields.iter().map(|field| field.get_decoder()).collect::<Vec<_>>();
        let field_names = self.fields.iter().map(|field| &field.name).collect::<Vec<_>>();
        quote! {
            impl crate::Field for #name {
                fn to_bytes(&self) -> Vec<u8> {
                    vec![
                        #(#encoders),*
                    ].iter().flatten().cloned().collect()
                }
                fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::Result<Self> {
                    #(#decoders)*;
                    Ok(Self {
                        #(#field_names),*
                    })
                }
            }
        }
    }
}