extern crate proc_macro;
mod types;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{braced, parse_macro_input, Attribute, Data, Lit, Meta, Token};
use syn::{DeriveInput, Expr, LitInt, Path, PathArguments, PathSegment, Type};
use crate::types::FieldType;

struct FieldMacroInput {
    name: syn::Ident,
    body: Vec<types::Field>,
}
impl Parse for FieldMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse::<syn::Ident>()?;
        input.parse::<Token![,]>()?;
        let raw_body;
        braced!(raw_body in input);
        let mut body = Vec::new();
        while let Ok(f) = raw_body.parse::<types::Field>() {
            if f.r#type.is_none() {
                panic!("Packet field type can't be None");
            }
            body.push(f);
        }
        Ok(Self { name, body })
    }
}
impl ToTokens for FieldMacroInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        if self.body.len() == 0 {
            tokens.extend(quote! {
                #[derive(Debug, Clone)]
                pub struct #name {}
                impl crate::Field for #name {
                    fn to_bytes(&self) -> Vec<u8> {Vec::new()}
                    fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {Ok(Self {})}
                }
                impl #name {
                    pub fn new() -> Self {Self {}}
                }
            });
            return;
        }
        let field_names = self.body.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_types = self.body.iter().map(|f| &f.r#type).collect::<Vec<_>>();
        let encoders = self.body.iter().map(|f| f.get_struct_encoder()).collect::<Vec<_>>();
        let decoders = self.body.iter().map(|f| f.get_struct_decoder()).collect::<Vec<_>>();
        tokens.append_all(quote! {
            #[derive(Debug, Clone)]
            pub struct #name {
                #(pub #field_names: #field_types,)*
            }
            impl crate::Field for #name {
                fn to_bytes(&self) -> Vec<u8> {
                    vec![#(#encoders,)*].iter().flatten().cloned().collect()
                }
                fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {
                    Ok(Self {
                        #(#field_names: #decoders,)*
                    })
                }
            }
        });
        tokens.extend(quote! {
            impl #name {
                pub fn new(#(#field_names: #field_types,)*) -> Self {
                    Self {#(#field_names,)*}
                }
            }
        })
    }
}
#[proc_macro]
#[allow(non_snake_case)]
pub fn Field(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as FieldMacroInput);
    input.to_token_stream().into()
}

struct PacketMacroInput {
    name: syn::Ident,
    id: syn::LitInt,
    body: Vec<types::Field>,
}
impl Parse for PacketMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse::<syn::Ident>()?;
        input.parse::<Token![,]>()?;
        let id = input.parse::<syn::LitInt>()?;
        input.parse::<Token![,]>()?;
        let raw_body;
        braced!(raw_body in input);
        let mut body = Vec::new();
        while let Ok(f) = raw_body.parse::<types::Field>() {
            if f.r#type.is_none() {
                panic!("Packet field type can't be None");
            }
            body.push(f);
        }
        Ok(Self { name, id, body })
    }
}
impl ToTokens for PacketMacroInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let id = &self.id;
        if self.body.len() == 0 {
            tokens.extend(quote! {
                #[derive(Debug, Clone)]
                pub struct #name {}
                impl crate::Packet for #name {
                    const ID: i32 = #id;
                    fn to_bytes(&self) -> Vec<u8> {Vec::new()}
                    fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {Ok(Self {})}
                }
                impl #name {
                    pub fn new() -> Self {Self {}}
                }
            });
            return;
        }
        let field_names = self.body.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_types = self.body.iter().map(|f| &f.r#type).collect::<Vec<_>>();
        let encoders = self.body.iter().map(|f| f.get_struct_encoder()).collect::<Vec<_>>();
        let decoders = self.body.iter().map(|f| f.get_struct_decoder()).collect::<Vec<_>>();
        tokens.append_all(quote! {
            #[derive(Debug, Clone)]
            pub struct #name {
                #(pub #field_names: #field_types,)*
            }
            impl crate::Packet for #name {
                const ID: i32 = #id;
                fn to_bytes(&self) -> Vec<u8> {
                    vec![#(#encoders,)*].iter().flatten().cloned().collect()
                }
                fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {
                    Ok(Self {
                        #(#field_names: #decoders,)*
                    })
                }
            }
        });
        tokens.extend(quote! {
            impl #name {
                pub fn new(#(#field_names: #field_types,)*) -> Self {
                    Self {#(#field_names,)*}
                }
            }
        })
    }
}
#[proc_macro]
#[allow(non_snake_case)]
pub fn Packet(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as PacketMacroInput);
    input.to_token_stream().into()
}


struct EnumMacroInput {
    name: syn::Ident,
    body: Vec<types::Field>,
}
impl Parse for EnumMacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse::<syn::Ident>()?;
        input.parse::<Token![,]>()?;
        let raw_body;
        braced!(raw_body in input);
        let mut body = Vec::new();
        while let Ok(f) = raw_body.parse::<types::Field>() {
            body.push(f);
        }
        Ok(Self { name, body })
    }
}
impl ToTokens for EnumMacroInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        if self.body.len() == 0 {
            tokens.extend(quote! {
                #[derive(Debug, Clone)]
                pub enum #name {}
                impl crate::Field for #name {
                    fn to_bytes(&self) -> Vec<u8> {compile_error!("can't convert empty enum to bytes")}
                    fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {compile_error!("can't convert empty enum to bytes")}
                }
                impl crate::Field for #name {
                    pub fn new() -> Self {Self {}}
                }
            })
        }
        if self.body.len() > i32::MAX as usize {
            panic!("can't encode an enum with more options than an Integer. How did you even get here?")
        }
        let field_names = self.body.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_types = self.body.iter().map(|f| match &f.r#type {
            FieldType::None => quote! {},
            v => quote!{(#v)},
        }).collect::<Vec<_>>();
        let encoders = self.body.iter()
            .enumerate().map(|(i, f)| match std::panic::catch_unwind(|| {
                f.get_enum_encoder(i)}) {
                Err(e) => {println!("error while parsing {}", self.name); std::panic::resume_unwind(e)},
                Ok(v) => v,
            })
            .collect::<Vec<_>>();
        let decoders = self.body.iter()
            .enumerate()
            .map(|(i, f)| f.get_enum_decoder(i))
            .collect::<Vec<_>>();
        tokens.extend(quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #(#field_names #field_types,)*
            }
            impl crate::Field for #name {
                fn to_bytes(&self) -> Vec<u8> {
                    match self.clone() {
                        #(#encoders,)*
                    }
                }
                fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::errors::Result<Self> {
                    Ok(match reader.read_var_int()? {
                        #(#decoders,)*
                        v => return Err(crate::errors::Errors::InvalidEnum(format!("Integer {v} is outside of range for enum"))),
                    })
                }
            }
        })
    }
}
#[proc_macro]
#[allow(non_snake_case)]
pub fn VarIntEnum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream = parse_macro_input!(input as EnumMacroInput).to_token_stream().into();
    println!("{}", stream);
    stream
}


#[proc_macro_derive(Packet_old, attributes(id, len, Var, Const, when))]
pub fn derive_packet(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let ast: DeriveInput = syn::parse(input).unwrap();

    let id = get_packet_id(&ast.attrs);
    let name = &ast.ident;
    let Data::Struct(s) = &ast.data else {
        panic!("Can only be used for structs");
    };

    let mut field_encoders = Vec::with_capacity(s.fields.len());
    let mut field_decoders = Vec::with_capacity(s.fields.len());
    let mut field_names = Vec::with_capacity(s.fields.len());
    for field in &s.fields {
        let field_name = field.ident.clone().expect("unnamed fields");
        let field_type = &field.ty;
        field_names.push(field_name.clone());
        let (encoder, decoder) = get_encoder_and_decoder_for_field(field_type, quote! {self.#field_name}, &field.attrs);
        let decoder = quote! {let #field_name = #decoder};
        field_encoders.push(encoder);
        field_decoders.push(decoder);
    };

    let res = quote! {
        impl crate::Packet for #name {
            const ID: i32 = #id;
            fn to_bytes(&self) -> Vec<u8> {
                let vector: Vec<Vec<u8>> = vec![
                    #(#field_encoders),*
                ];
                vector.iter().flatten().cloned().collect()
            }
            fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::Result<Self> {
                #(#field_decoders);*;
                Ok(Self {
                    #(#field_names),*
                })
            }
        }
    };
    // #[cfg(debug_assertions)]
    // println!("{:#}", &res);
    res.into()
}

#[proc_macro_derive(Field_old, attributes(len, Var, Const, when))]
pub fn derive_field(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let ast: DeriveInput = syn::parse(input).unwrap();

    let name = &ast.ident;
    let Data::Struct(s) = &ast.data else {
        panic!("Can only be used for structs");
    };

    let mut field_encoders = Vec::with_capacity(s.fields.len());
    let mut field_decoders = Vec::with_capacity(s.fields.len());
    let mut field_names = Vec::with_capacity(s.fields.len());
    for field in &s.fields {
        let field_name = field.ident.clone().expect("unnamed fields");
        let field_type = &field.ty;
        field_names.push(field_name.clone());
        let (encoder, decoder) = get_encoder_and_decoder_for_field(field_type, quote! {self.#field_name}, &field.attrs);
        let decoder = quote! {let #field_name = #decoder};
        field_encoders.push(encoder);
        field_decoders.push(decoder);
    };

    let res = quote! {
        impl crate::Field for #name {
            fn to_bytes(&self) -> Vec<u8> {
                let vector: Vec<Vec<u8>> = vec![
                    #(#field_encoders),*
                ];
                vector.iter().flatten().cloned().collect()
            }
                fn from_reader(reader: &mut crate::fields::PacketReader) -> crate::Result<Self> {
                #(#field_decoders);*;
                Ok(Self {
                    #(#field_names),*
                })
            }
        }
    };
    // #[cfg(debug_assertions)]
    // println!("{:#}", &res);
    res.into()
}
fn get_packet_id(attrs: &[Attribute]) -> &LitInt {
    for attr in attrs {
        let Meta::NameValue(name_value) = &attr.meta else {
            continue;
        };
        if !name_value.path.is_ident("id") {continue}
        let Expr::Lit(lit) = &name_value.value else {
            panic!("id must a literal")
        };
        let Lit::Int(int) = &lit.lit else {
            panic!("id must be an integer")
        };
        return int;
    }
    panic!("requires id attribute for struct");
}

fn get_encoder_and_decoder_for_field(field_type: &Type, val_ref: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let path = match field_type {
        Type::Paren(type_paren) => return get_encoder_and_decoder_for_field(&type_paren.elem, val_ref, attrs),
        Type::Path(path) => &path.path,
        t => panic!("invalid field type: {}", t.to_token_stream().to_string()),
    };
    let segment = path.segments.last().unwrap();
    let ident = &segment.ident;
    
    match ident.to_string().as_ref() {
        "bool" => (quote! { crate::fields::encode_bool(#val_ref) }, quote! { reader.read_bool()? }),
        "u8" => (quote! { crate::fields::encode_ubyte(#val_ref) }, quote! { reader.read_ubyte() }),
        "i8" => (quote! { crate::fields::encode_byte(#val_ref) }, quote! { reader.read_byte() }),
        "u16" => (quote! { crate::fields::encode_ushort(#val_ref) }, quote! { reader.read_ushort() }),
        "i16" => (quote! { crate::fields::encode_short(#val_ref) }, quote! { reader.read_short() }),
        "i32" => handle_int(attrs, val_ref),
        "i64" => handle_long(attrs, val_ref),
        "u128" => (quote! { crate::fields::encode_uuid(#val_ref) }, quote! { reader.read_uuid() }),
        "f32" => (quote! { crate::fields::encode_float(#val_ref) }, quote! { reader.read_float() }),
        "f64" => (quote! { crate::fields::encode_double(#val_ref) }, quote! { reader.read_double() }),
        "String" => (quote! { crate::fields::encode_string(#val_ref .clone()) }, quote! { reader.read_string()? }),
        
        "Byte" => (quote! { crate::fields::encode_byte(#val_ref) }, quote! { reader.read_byte() }),
        "UByte" => (quote! { crate::fields::encode_ubyte(#val_ref) }, quote! { reader.read_ubyte() }),
        "Short" => (quote! { crate::fields::encode_short(#val_ref) }, quote! { reader.read_short() }),
        "UShort" => (quote! { crate::fields::encode_ushort(#val_ref) }, quote! { reader.read_ushort() }),
        "Int" => (quote! { crate::fields::encode_int(#val_ref) }, quote! { reader.read_int() }),
        "UInt" => (quote! { crate::fields::encode_uint(#val_ref) }, quote! { reader.read_uint() }),
        "Long" => (quote! { crate::fields::encode_long(#val_ref) }, quote! { reader.read_long() }),
        "UUID" => (quote! { crate::fields::encode_uuid(#val_ref) }, quote! { reader.read_uuid() }),
        "Float" => (quote! { crate::fields::encode_float(#val_ref) }, quote! { reader.read_float() }),
        "Double" => (quote! { crate::fields::encode_double(#val_ref) }, quote! { reader.read_double() }),
        
        "Identifier" => (quote! { crate::fields::encode_identifier(#val_ref .clone()) }, quote! { reader.read_identifier()? }),
        "Angle" => (quote! { crate::fields::encode_angle(#val_ref) }, quote! { reader.read_angle() }),
        "VarInt" => (quote! { crate::fields::encode_var_int(#val_ref) }, quote! { reader.read_var_int()? }),
        "VarLong" => (quote! { crate::fields::encode_var_long(#val_ref) }, quote! { reader.read_var_long()? }),
        "PrefixedArray" => handle_prefixed_array(segment, val_ref, attrs),
        "PrefixedOptional" => handle_prefixed_option(segment, val_ref, attrs),
        
        _ => if ident == "Vec" {
            handle_vec(segment, val_ref, attrs)
        } else if ident == "Option" {
            handle_option(segment, val_ref, attrs)
        } else {
            println!("unhandled type {:?}", ident);
            handle_generic(val_ref, path)
        },
    }
}
fn handle_prefixed_array(segment: &PathSegment, name: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        panic!("Invalid vector inner type")
    };
    let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() else {
        panic!("Invalid vector inner type")
    };
    let (inner_encoder, inner_decoder) = get_encoder_and_decoder_for_field(&inner_type, quote! {v.clone()}, attrs);
    (quote! {
        vec![crate::fields::encode_var_int(#name.len() as i32), #name.iter().cloned().flat_map(|v| #inner_encoder).collect()].iter().cloned().flatten().collect()
    }, quote! {
        {
            let len = reader.read_var_int()?;
            let mut v = Vec::with_capacity(len as usize);
            for _ in 0..len {
                v.push(#inner_decoder);
            }
            v
        }
    })
}

fn handle_prefixed_option(segment: &PathSegment, name: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        panic!("Invalid vector inner type")
    };
    let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() else {
        panic!("Invalid vector inner type")
    };
    let (inner_encoder, inner_decoder) = get_encoder_and_decoder_for_field(&inner_type, quote! {v.clone()}, attrs);
    (quote! {
        if let Some(v) = &#name {vec![crate::fields::encode_bool(true), #inner_encoder].iter().cloned().flatten().collect()} else {crate::fields::encode_bool(false)}
    }, quote! {
        if reader.read_bool()? {Some(#inner_decoder)} else {None} 
    })
}
fn is_var(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        let Meta::Path(path) = &attr.meta else {continue};
        if path.is_ident("Var") {
          return true;
        }
    }
    false
}
fn is_const(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        let Meta::Path(path) = &attr.meta else {continue};
        if path.is_ident("Const") {
            return true;
        }
    }
    false
}
fn handle_int(attrs: &[Attribute], name: impl ToTokens) -> (TokenStream, TokenStream) {
    if is_var(attrs) {
        (quote! {
            crate::fields::encode_var_int(#name)
        }, quote! {
            reader.read_var_int()?
        })
    } else {
        #[cfg(debug_assertions)]
        if !is_const(attrs) {
            println!("warning: unspecified Integer used for field {}", name.to_token_stream());
        }
        (quote! {
            crate::fields::encode_int(#name)
        }, quote! {
            reader.read_int()
        })
    }
}
fn handle_long(attrs: &[Attribute], name: impl ToTokens) -> (TokenStream, TokenStream) {
    if is_var(attrs) {
        (quote! {
            crate::fields::encode_var_long(#name)
        }, quote! {
            reader.read_var_long()?
        })
    } else {
        #[cfg(debug_assertions)]
        if !is_const(attrs) {
            println!("warning: unspecified Long used for field {}", name.to_token_stream());
        }
        (quote! {
            crate::fields::encode_long(#name)
        }, quote! {
            reader.read_long()
        })
    }
}
fn handle_vec(segment: &PathSegment, name: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        panic!("Invalid vector inner type")
    };
    let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() else {
        panic!("Invalid vector inner type")
    };
    let len_resp_field = get_length_responsible_attr(attrs);
    let (inner_encoder, inner_decoder) = get_encoder_and_decoder_for_field(&inner_type, quote! {v.clone()}, attrs);
    (quote! {
        #name.iter().flat_map(|v| #inner_encoder).collect()
    }, quote! {
        {
            let mut v = Vec::with_capacity(#len_resp_field as usize);
            for _ in 0..#len_resp_field {
                v.push(#inner_decoder);
            }
            v
        }
    })
}
fn get_length_responsible_attr(attrs: &[Attribute]) -> Path {
    for attr in attrs {
        let Meta::NameValue(meta) = &attr.meta else {
            continue;
        };
        if !meta.path.is_ident("len") {continue}
        let Expr::Lit(expr) = &meta.value else {
            panic!("len must be literal expression")
        };
        let Lit::Str(path_str) = &expr.lit else {
            panic!("len must be a string literal")
        };
        return syn::parse_str::<Path>(&*path_str.value()).expect("Invalid path");
    }
    panic!("requires a \"len\" attribute");
}
fn handle_option(segment: &PathSegment, name: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        panic!("Invalid option inner type")
    };
    let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() else {
        panic!("Invalid option inner type")
    };
    let dependency = get_dependency(attrs);

    let (inner_encoder, inner_decoder) = get_encoder_and_decoder_for_field(&inner_type, quote! {v}, attrs);
    (quote! {
        if self.#dependency {
            let v = #name.clone().unwrap();
            #inner_encoder
        } else {vec![]}
    }, quote! {
        if #dependency {core::option::Option::Some(#inner_decoder)} else {core::option::Option::None}
    })
}
fn get_dependency(attrs: &[Attribute]) -> Path {
    for attr in attrs {
        let Meta::NameValue(meta) = &attr.meta else {
            continue;
        };
        if !meta.path.is_ident("when") {continue}
        let Expr::Lit(expr) = &meta.value else {
            panic!("when must be literal expression")
        };
        let Lit::Str(path_str) = &expr.lit else {
            panic!("when must be a string literal")
        };
        return syn::parse_str::<Path>(&*path_str.value()).expect("Invalid path");
    }
    panic!("requires a \"len\" attribute");
}

fn handle_generic(name: impl ToTokens, path: &Path) -> (TokenStream, TokenStream) {
    (quote! {crate::Field::to_bytes(&#name)}, quote! {{use crate::Field; #path::from_reader(reader)?}})
}