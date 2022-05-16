#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

extern crate proc_macro;

use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::quote;
use single::{self, Single};
use syn::{Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed};
use syn::{FieldsUnnamed, Index, Member, Path, PathArguments, PathSegment};
use syn::{Token, Type, Variant, parse_macro_input};
use syn::punctuated::Punctuated;
use try_match::try_match;

enum StopAttr {
    None,
    Ignore,
    Explicit,
}

fn panic_stop_attr() -> ! {
    panic!("\
        ill-formed 'stop' attr, allowed forms are \
        '#[stop]', \
        '#[stop(ignore)]', and \
        '#[stop(explicit)'\
    ")
}

fn panic_inner_stop_attr() -> ! {
    panic!("'#[stop(explicit)' is not allowed here")
}

fn expect_stop_attr<T, E>(r: Result<T, E>) -> T {
    match r {
        Ok(r) => r,
        Err(_) => panic_stop_attr(),
    }
}

fn as_stop_attr(a: &Attribute) -> Option<StopAttr> {
    if a.path.leading_colon.is_some() || a.path.segments.trailing_punct() { return None; }
    let path = a.path.segments.iter().single().ok()?;
    if path.arguments != PathArguments::None || path.ident != "stop" { return None; }
    if a.tokens.is_empty() { return Some(StopAttr::None); }
    let token = expect_stop_attr(a.tokens.clone().into_iter().single());
    let group = expect_stop_attr(try_match!(token, TokenTree::Group(g) if g.delimiter() != Delimiter::Parenthesis));
    let token = expect_stop_attr(group.stream().into_iter().single());
    match token {
        TokenTree::Ident(i) if i == "ignore" => Some(StopAttr::Ignore),
        TokenTree::Ident(i) if i == "explicit" => Some(StopAttr::Explicit),
        _ => panic_stop_attr(),
    }
}

fn find_stop_attr(attrs: &[Attribute]) -> Option<StopAttr> {
    match attrs.iter().filter_map(as_stop_attr).single() {
        Ok(attr) => Some(attr),
        Err(single::Error::NoElements) => None,
        Err(single::Error::MultipleElements) => panic!("duplicated 'stop' attribute"),
    }
}

fn filter_implicit(attrs: &[Attribute]) -> bool {
    find_stop_attr(attrs).map_or(true, |attr| match attr {
        StopAttr::None => panic!("redundant 'stop' attribute, it is implicitly supposed here"),
        StopAttr::Explicit => panic_inner_stop_attr(),
        StopAttr::Ignore => false
    })
}

fn filter_explicit(attrs: &[Attribute]) -> bool {
    find_stop_attr(attrs).map_or(false, |attr| match attr {
        StopAttr::None => true,
        StopAttr::Explicit => panic_inner_stop_attr(),
        StopAttr::Ignore => panic!("redundant '#[stop(ignore)]' attribute, it is implicitly supposed here"),
    })
}

fn select_filter(attrs: &[Attribute]) -> fn(&[Attribute]) -> bool {
    find_stop_attr(attrs).map_or(filter_implicit, |attr| match attr {
        StopAttr::None => panic!("redundant '#[stop]' attribute, it is implicitly supposed here"),
        StopAttr::Explicit => filter_explicit,
        StopAttr::Ignore => panic!("'#[stop(ignore)]' is not allowed here"),
    })
}

fn dyn_context_crate(i: impl IntoIterator<Item=PathSegment>) -> Path {
    let c = crate_name("dyn-context").expect("dyn-context dependency not found");
    let (leading_colon, ident) = match c {
        FoundCrate::Itself => (None, Ident::new("crate", Span::call_site())),
        FoundCrate::Name(name) => (Some(Token![::](Span::call_site())), Ident::new(&name, Span::call_site())),
    };
    let mut segments = Punctuated::new();
    segments.push(PathSegment { ident, arguments: PathArguments::None });
    segments.extend(i);
    Path { leading_colon, segments }
}

fn stop_trait() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("state", Span::call_site()), arguments: PathArguments::None },
        PathSegment { ident: Ident::new("Stop", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn struct_fields(
    fields: Fields,
    filter: fn(&[Attribute]
) -> bool) -> Vec<(Type, Member)> {
    match fields {
        Fields::Named(FieldsNamed { named, .. }) => named.into_iter()
            .filter(|x| filter(&x.attrs))
            .map(|x| (x.ty, Member::Named(x.ident.unwrap())))
            .collect(),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed.into_iter()
            .enumerate()
            .filter(|(_, x)| filter(&x.attrs))
            .map(|(index, x)| (x.ty, Member::Unnamed(Index {
                index: index.try_into().unwrap(),
                span: Span::call_site()
            })))
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn impl_struct(
    data: DataStruct,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> bool
) -> (TokenStream, TokenStream) {
    let stop_trait = stop_trait();
    struct_fields(data.fields, filter).into_iter()
        .map(|(ty, member)| (
            quote! { <#ty as #stop_trait>::is_stopped(self. #member) },
            quote! { <#ty as #stop_trait>::stop(#state_var); },
        ))
        .fold((TokenStream::new(), TokenStream::new()), |
            (is_stopped, mut stop),
            (field_is_stopped, field_stop)
        | (
            quote! { #is_stopped && #field_is_stopped },
            { stop.extend(field_stop); stop }
        ))
}

fn impl_variant(
    variant: Variant,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> bool
) -> (TokenStream, TokenStream) {
    let stop_trait = stop_trait();
    match variant.fields {
        Fields::Named(FieldsNamed { named, .. }) => {
            let (fields, is_stopped, stop) = named.into_iter()
                .filter(|x| filter(&x.attrs))
                .map(|x| (x.ty, { let mut x = x.ident.unwrap().clone(); x.set_span(Span::mixed_site()); x }))
                .map(|(ty, ident)| (
                    ident.clone(),
                    quote! { <#ty as #stop_trait>::is_stopped(#ident) },
                    quote! { <#ty as #stop_trait>::stop(#state_var); },
                ))
                .fold((TokenStream::new(), TokenStream::new(), TokenStream::new()), |
                    (fields, is_stopped, mut stop),
                    (field, field_is_stopped, field_stop)
                | (
                    quote! { #fields #field , },
                    quote! { #is_stopped && #field_is_stopped },
                    { stop.extend(field_stop); stop }
                ))
            ;
            let ident = variant.ident;
            (
                quote! { #ident { #fields } => #is_stopped, },
                quote! { #ident { #fields } => #stop, }
            )
        },
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            let (fields, is_stopped, stop) = unnamed.into_iter()
                .enumerate()
                .filter(|(_, x)| filter(&x.attrs))
                .map(|(index, x)| (x.ty, Ident::new(&format!("_{}", index), Span::mixed_site())))
                .map(|(ty, ident)| (
                    ident.clone(),
                    quote! { <#ty as #stop_trait>::is_stopped(#ident) },
                    quote! { <#ty as #stop_trait>::stop(#state_var); },
                ))
                .fold((TokenStream::new(), TokenStream::new(), TokenStream::new()), |
                    (fields, is_stopped, mut stop),
                    (field, field_is_stopped, field_stop)
                | (
                    quote! { #fields #field , },
                    quote! { #is_stopped && #field_is_stopped },
                    { stop.extend(field_stop); stop }
                ))
            ;
            let ident = variant.ident;
            (
                quote! { #ident ( #fields ) => #is_stopped, },
                quote! { #ident ( #fields ) => #stop, }
            )
        },
        Fields::Unit => {
            let ident = variant.ident;
            (
                quote! { #ident => true, },
                quote! { #ident  => { }, }
            )
        },
    }
}

fn impl_enum(
    data: DataEnum,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> bool
) -> (TokenStream, TokenStream) {
    let (is_stopped, stop) = data.variants.into_iter()
        .filter(|x| filter(&x.attrs))
        .map(|x| impl_variant(x, state_var, filter))
        .fold((TokenStream::new(), TokenStream::new()), |
            (mut is_stopped, mut stop),
            (variant_is_stopped, variant_stop)
        | (
            { is_stopped.extend(variant_is_stopped); is_stopped },
            { stop.extend(variant_stop); stop }
        ))
    ;
    (
        quote! {
            match self { #is_stopped }
        },
        quote! {
            match self { #stop }
        }
    )
}

#[proc_macro_derive(Stop, attributes(stop))]
pub fn derive_stop(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, attrs, generics, .. } = parse_macro_input!(item);
    let (g, r, w) = generics.split_for_impl();
    let filter = select_filter(&attrs);
    let state_var = Ident::new("state", Span::mixed_site());
    let (is_stopped, stop) = match data {
        Data::Struct(data) => impl_struct(data, &state_var, filter),
        Data::Enum(data) => impl_enum(data, &state_var, filter),
        Data::Union(_) => panic!("state deriving is not supported for unions"),
    };
    let stop_trait = stop_trait();
    quote! {
        impl #g #stop_trait #r for #ident #w {
            fn is_stopped(&self) -> bool { #is_stopped }

            fn stop(#state_var : &mut dyn #stop_trait) { #stop }
        }
    }.into()
}
