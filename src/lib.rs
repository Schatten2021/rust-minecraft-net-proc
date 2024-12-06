extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Attribute, Data, Lit, Meta};
use syn::{DeriveInput, Expr, LitInt, Path, PathArguments, PathSegment, Type};

#[proc_macro_derive(Packet, attributes(id, len, Var, Const, when))]
pub fn derive_packet(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let input = syn::parse(input).unwrap();

    // Generate code based on the input
    impl_packet_derive(&input)
}
fn impl_packet_derive(ast: &DeriveInput) -> proc_macro::TokenStream {
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
        let (encoder, decoder) = handle_field(field_type, quote! {self.#field_name}, &field.attrs);
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

fn handle_field(field_type: &Type, val_ref: impl ToTokens + Clone, attrs: &[Attribute]) -> (TokenStream, TokenStream) {
    let path = match field_type {
        Type::Paren(type_paren) => return handle_field(&type_paren.elem, val_ref, attrs),
        Type::Path(path) => &path.path,
        Type::Verbatim(_) => panic!("unhandled type verbatim"),
        Type::Array(_) => panic!("unhandled type array"),
        Type::Group(_) => panic!("unhandled type group"),
        Type::BareFn(_) => panic!("unhandled type bare_fn"),
        Type::Ptr(_) => panic!("unhandled type ptr"),
        Type::Reference(_) => panic!("unhandled type reference"),
        Type::Slice(_) => panic!("unhandled type slice"),
        Type::Tuple(_) => panic!("unhandled type tuple"),
        Type::Macro(_) => panic!("unhandled type macro"),
        Type::TraitObject(_) => panic!("unhandled type trait object"),
        Type::ImplTrait(_) => panic!("unhandled type impl trait"),
        Type::Infer(_) => panic!("unhandled type infer"),
        _ => panic!("unhandled type"),
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
        // "Vec" => handle_vec(field_type, name, attrs),
        "u128" => (quote! { crate::fields::encode_uuid(#val_ref) }, quote! { reader.read_uuid() }),
        "f32" => (quote! { crate::fields::encode_float(#val_ref) }, quote! { reader.read_float() }),
        "f64" => (quote! { crate::fields::encode_double(#val_ref) }, quote! { reader.read_double() }),
        "String" => (quote! { crate::fields::encode_string(#val_ref .clone()) }, quote! { reader.read_string()? }),
        t => if ident == "Vec" {
            handle_vec(segment, val_ref, attrs)
        } else if ident == "Option" {
            handle_option(segment, val_ref, attrs)
        } else {
            panic!("unhandled type {t}")
        }
    }
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
    let (inner_encoder, inner_decoder) = handle_field(&inner_type, quote! {v.clone()}, attrs);
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

    let (inner_encoder, inner_decoder) = handle_field(&inner_type, quote! {v}, attrs);
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