extern crate core;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::Data::Struct;
use syn::Fields::Named;
use syn::{
    AngleBracketedGenericArguments, GenericArgument, Ident, LitStr, Path, PathArguments,
    PathSegment, Token, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let name_builder = Ident::new((name.to_string() + "Builder").as_str(), Span::call_site());

    let parsed_fields: Vec<BuilderField>;

    let mut output = proc_macro2::TokenStream::new();

    // Input parser
    if let Struct(data) = input.data {
        // Use the holy RUST pattern-matching power to parse this damn AST
        if let Named(named) = data.fields {
            let fields = named.named;
            parsed_fields = fields
                .iter()
                .cloned()
                .map(|x| {
                    if let Some((outer, inner)) = parse_generic_type(&x.ty) {
                        match outer.to_string().as_str() {
                            "Option" => {
                                return BuilderField {
                                    field_name: x.ident.unwrap(),
                                    field_type: inner,
                                    is_required: false,
                                    repeated_pattern_meta: None,
                                };
                            }
                            "Vec" => {
                                if let Some(attr) = x.attrs.into_iter().next() {
                                    if attr.path.is_ident("builder") {
                                        match attr.parse_args() {
                                            Ok(AttributeHelper { name, value }) => {
                                                if name == "each" {
                                                    return BuilderField {
                                                        field_name: x.ident.unwrap(),
                                                        field_type: x.ty,
                                                        is_required: true,
                                                        repeated_pattern_meta: Some((
                                                            Ident::new(
                                                                value.as_str(),
                                                                Span::call_site(),
                                                            ),
                                                            inner,
                                                        )),
                                                    };
                                                } else {
                                                    output.extend(quote_spanned!( name.span() => compile_error!("expected `each`");));
                                                }
                                            }
                                            Err(error) => output.extend(error.to_compile_error()),
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    BuilderField {
                        field_name: x.ident.unwrap(),
                        field_type: x.ty,
                        is_required: true,
                        repeated_pattern_meta: None,
                    }
                })
                .collect();
        } else {
            parsed_fields = vec![];
        }
    } else {
        parsed_fields = vec![];
        output.extend(quote!(compile_error!("Derived object is not a struct");));
    }

    // Builder struct generator
    let mut prepared_body = prepare_fields(
        &parsed_fields,
        |BuilderField {
             field_name,
             field_type,
             ..
         }| {
            quote! {
                #field_name: std::option::Option<#field_type>,
            }
        },
    );

    output.extend(quote! {
        pub struct #name_builder {
            #prepared_body
        }
    });

    // builder() method generator
    prepared_body = prepare_fields(
        &parsed_fields,
        |BuilderField {
             field_name,
             repeated_pattern_meta,
             ..
         }| match repeated_pattern_meta {
            Some(_) => quote! {
                #field_name: std::option::Option::Some(vec!()),
            },
            None => quote! {
                #field_name: std::option::Option::None,
            },
        },
    );

    output.extend(quote! {
        impl #name {
            pub fn builder() -> #name_builder {
                #name_builder {
                    #prepared_body
                }
            }
        }
    });

    // Builder impl setters generator
    prepared_body = prepare_fields(
        &parsed_fields,
        |BuilderField {
             field_name,
             field_type,
             repeated_pattern_meta,
             ..
         }| {
            if let Some((setter_name, ..)) = repeated_pattern_meta {
                if setter_name == &field_name.to_string() {
                    return quote!();
                }
            }
            quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = std::option::Option::Some(#field_name);
                    self
                }
            }
        },
    );
    output.extend(quote! {
        impl #name_builder {
            #prepared_body
        }
    });

    // Builder impl vec setters generator
    prepared_body = prepare_fields(
        &parsed_fields,
        |BuilderField {
             field_name,
             repeated_pattern_meta,
             ..
         }| {
            if let Some((setter_name, setter_type)) = repeated_pattern_meta {
                quote! {
                    fn #setter_name(&mut self, value: #setter_type) -> &mut Self {
                        self.#field_name.as_mut().unwrap().push(value);
                        self
                    }
                }
            } else {
                quote!()
            }
        },
    );
    output.extend(quote! {
        impl #name_builder {
            #prepared_body
        }
    });

    // Builder impl build generator
    prepared_body = prepare_fields(
        &parsed_fields,
        |BuilderField {
             field_name,
             is_required,
             ..
         }| {
            if *is_required {
                quote! {
                    #field_name: self.#field_name.clone()?,
                }
            } else {
                quote! {
                    #field_name: self.#field_name.clone(),
                }
            }
        },
    );

    output.extend(quote! {
        impl #name_builder {
            pub fn build(&mut self) -> std::result::Result<#name, std::string::String> {
                if let std::option::Option::Some(result) = self.try_build() {
                    std::result::Result::Ok(result)
                } else {
                    std::result::Result::Err(std::string::String::from("Some of fields are not set!"))
                }
            }

            fn try_build(&mut self) -> std::option::Option<#name> {
                std::option::Option::Some(#name {
                    #prepared_body
                })
            }
        }
    });

    output.into()
}

fn parse_generic_type(data: &Type) -> Option<(Ident, Type)> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path {
            segments: outer_segment,
            ..
        },
    }) = data
    {
        if let PathSegment {
            ident: type_name,
            arguments:
                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args: inner_argument,
                    ..
                }),
        } = outer_segment.first().unwrap()
        {
            if let GenericArgument::Type(inner_type_name) = inner_argument.first().unwrap() {
                return Some((type_name.clone(), inner_type_name.clone()));
            }
        }
    }
    None
}

struct BuilderField {
    field_name: Ident,
    field_type: Type,
    is_required: bool,
    repeated_pattern_meta: Option<(Ident, Type)>,
}

fn prepare_fields(
    fields: &Vec<BuilderField>,
    generator: fn(&BuilderField) -> proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let mut result = proc_macro2::TokenStream::new();
    for field in fields {
        result.extend(generator(field));
    }
    result
}

struct AttributeHelper<T> {
    name: Ident,
    value: T,
}

impl Parse for AttributeHelper<String> {
    fn parse(tokens: ParseStream) -> syn::Result<Self> {
        let name: Ident = tokens.parse()?;
        let _: Token![=] = tokens.parse()?;
        let val: LitStr = tokens.parse()?;

        Ok(AttributeHelper {
            name,
            value: val.value(),
        })
    }
}
