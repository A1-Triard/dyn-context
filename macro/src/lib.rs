#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![allow(clippy::type_complexity)]

extern crate proc_macro;

use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};
use single::{self, Single};
use syn::{Attribute, Data, DataStruct, DeriveInput, Fields, FieldsNamed};
use syn::{FieldsUnnamed, Index, LitStr, Member, Path, PathArguments, PathSegment};
use syn::{Token, Type, parse_macro_input};
use syn::punctuated::Punctuated;
use try_match::try_match;

enum StopAttr {
    None(Span),
    Ignore(Span),
    Explicit(Span),
}

impl StopAttr {
    fn span(&self) -> Span {
        match self {
            StopAttr::None(span) => *span,
            StopAttr::Ignore(span) => *span,
            StopAttr::Explicit(span) => *span,
        }
    }
}

struct Error {
    span: Span,
    msg: &'static str,
}

fn ill_formed_attr(span: Span) -> Error {
    Error { span, msg: "\
        ill-formed 'stop' attr, allowed forms are \
        '#[stop]', \
        '#[stop(ignore)]', and \
        '#[stop(explicit)]'\
    " }
}

fn wrong_inner_attr(span: Span) -> Error {
    Error { span, msg: "'#[stop(explicit)]' is not allowed here" }
}

fn duplicated_attr(span: Span) -> Error {
    Error { span, msg: "duplicated 'stop' attribute" }
}

fn redundant_stop_attr(span: Span) -> Error {
    Error { span, msg: "redundant 'stop' attribute, it is implicitly supposed here" }
}

fn redundant_ignore_attr(span: Span) -> Error {
    Error { span, msg: "redundant '#[stop(ignore)]' attribute, it is implicitly supposed here" }
}

fn wrong_outer_attr(span: Span) -> Error {
    Error { span, msg: "'#[stop(ignore)]' is not allowed here" }
}

fn as_stop_attr(a: &Attribute) -> Result<StopAttr, Option<Error>> {
    if a.path.leading_colon.is_some() || a.path.segments.trailing_punct() { return Err(None); }
    let path = a.path.segments.iter().single().map_err(|_| None)?;
    if path.arguments != PathArguments::None || path.ident != "stop" { return Err(None); }
    if a.tokens.is_empty() { return Ok(StopAttr::None(a.bracket_token.span)); }
    let token = a.tokens.clone().into_iter().single()
        .map_err(|_| Some(ill_formed_attr(a.bracket_token.span)))?;
    let group = try_match!(token, TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis)
        .map_err(|_| Some(ill_formed_attr(a.bracket_token.span)))?;
    let token = group.stream().into_iter().single()
        .map_err(|_| Some(ill_formed_attr(a.bracket_token.span)))?;
    match token {
        TokenTree::Ident(i) if i == "ignore" => Ok(StopAttr::Ignore(i.span())),
        TokenTree::Ident(i) if i == "explicit" => Ok(StopAttr::Explicit(i.span())),
        _ => Err(Some(ill_formed_attr(token.span()))),
    }
}

fn tr<T, E>(x: Result<T, Option<E>>) -> Option<Result<T, E>> {
    match x {
        Ok(x) => Some(Ok(x)),
        Err(None) => None,
        Err(Some(e)) => Some(Err(e)),
    }
}

fn find_stop_attr(attrs: &[Attribute]) -> Result<Option<StopAttr>, Error> {
    let mut attrs = attrs.iter().filter_map(|x| tr(as_stop_attr(x)));
    if let Some(attr_1) = attrs.next() {
        let attr_1 = attr_1?;
        if let Some(attr_2) = attrs.next() {
            let attr_2 = attr_2?;
            Err(duplicated_attr(attr_2.span()))
        } else {
            Ok(Some(attr_1))
        }
    } else {
        Ok(None)
    }
}

fn filter_implicit(attrs: &[Attribute]) -> Result<bool, Error> {
    find_stop_attr(attrs)?.map_or(Ok(true), |attr| match attr {
        StopAttr::None(span) => Err(redundant_stop_attr(span)),
        StopAttr::Explicit(span) => Err(wrong_inner_attr(span)),
        StopAttr::Ignore(_) => Ok(false)
    })
}

fn filter_explicit(attrs: &[Attribute]) -> Result<bool, Error> {
    find_stop_attr(attrs)?.map_or(Ok(false), |attr| match attr {
        StopAttr::None(_) => Ok(true),
        StopAttr::Explicit(span) => Err(wrong_inner_attr(span)),
        StopAttr::Ignore(span) => Err(redundant_ignore_attr(span)),
    })
}

fn select_filter(attrs: &[Attribute]) -> Result<fn(&[Attribute]) -> Result<bool, Error>, Error> {
    find_stop_attr(attrs)?.map_or(Ok(filter_implicit), |attr| match attr {
        StopAttr::None(span) => Err(redundant_stop_attr(span)),
        StopAttr::Explicit(_) => Ok(filter_explicit),
        StopAttr::Ignore(span) => Err(wrong_outer_attr(span)),
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

fn state_trait() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("state", Span::call_site()), arguments: PathArguments::None },
        PathSegment { ident: Ident::new("State", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn struct_fields(
    fields: Fields,
    filter: fn(&[Attribute]) -> Result<bool, Error>
) -> Result<Vec<(Type, Member)>, Error> {
    match fields {
        Fields::Named(FieldsNamed { named, .. }) => named.into_iter()
            .filter_map(|x| filter(&x.attrs).map(|k| Some(x).filter(|_| k)).transpose())
            .map(|x| x.map(|x| (x.ty, Member::Named(x.ident.unwrap()))))
            .collect(),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed.into_iter()
            .enumerate()
            .filter_map(|(i, x)| filter(&x.attrs).map(|k| Some((i, x)).filter(|_| k)).transpose())
            .map(|x| x.map(|(index, x)| (x.ty, Member::Unnamed(Index {
                index: index.try_into().unwrap(),
                span: Span::call_site()
            }))))
            .collect(),
        Fields::Unit => Ok(Vec::new()),
    }
}

fn impl_struct(
    data: DataStruct,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> Result<bool, Error>
) -> Result<(TokenStream, TokenStream), Error> {
    let stop_trait = stop_trait();
    Ok(struct_fields(data.fields, filter)?.into_iter()
        .map(|(ty, member)| (
            quote! { <#ty as #stop_trait>::is_stopped(&self. #member) },
            quote! { <#ty as #stop_trait>::stop(#state_var); },
        ))
        .fold((quote! { true }, TokenStream::new()), |
            (is_stopped, mut stop),
            (field_is_stopped, field_stop)
        | (
            quote! { #is_stopped && #field_is_stopped },
            { stop.extend(field_stop); stop }
        ))
    )
}

fn try_derive_stop(input: DeriveInput) -> Result<TokenStream, Error> {
    let DeriveInput { ident, data, attrs, generics, .. } = input;
    let (g, r, w) = generics.split_for_impl();
    let filter = select_filter(&attrs)?;
    let state_var = Ident::new("state", Span::mixed_site());
    let (is_stopped, stop) = match data {
        Data::Struct(data) => impl_struct(data, &state_var, filter)?,
        Data::Enum(_) => panic!("'Stop' deriving is not supported for enums"),
        Data::Union(_) => panic!("'Stop' deriving is not supported for unions"),
    };
    let stop_trait = stop_trait();
    let state_trait = state_trait();
    Ok(quote! {
        impl #g #stop_trait #r for #ident #w {
            fn is_stopped(&self) -> bool { #is_stopped }

            fn stop(#state_var : &mut dyn #state_trait) { #stop }
        }
    })
}

fn compile_error() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("std_compile_error", Span::call_site()), arguments: PathArguments::None },
    ])
}

#[proc_macro_derive(Stop, attributes(stop))]
pub fn derive_stop(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item);
    match try_derive_stop(input) {
        Ok(r) => r,
        Err(e) => {
            let compile_error = compile_error();
            let msg = LitStr::new(e.msg, Span::call_site());
            quote_spanned! { e.span => #compile_error ! ( #msg ); }
        }
    }.into()
}
