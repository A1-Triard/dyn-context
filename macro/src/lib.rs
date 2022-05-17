#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![allow(clippy::type_complexity)]

extern crate proc_macro;

use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::{ToTokens, quote};
use single::{self, Single};
use syn::{Attribute, Data, DataStruct, DeriveInput, Fields, FieldsNamed};
use syn::{FieldsUnnamed, Index, Member, Path, PathArguments, PathSegment};
use syn::{Token, Type, parse};
use syn::punctuated::Punctuated;
use try_match::try_match;

struct Parsed<T> {
    value: T,
    source: TokenStream,
}

impl<T> Parsed<T> {
    fn new<S: ToTokens>(value:T, source: &S) -> Self {
        Parsed { value, source: source.to_token_stream() }
    }
}

enum Stop {
    None,
    Ignore,
    Explicit,
}

const ERR_ILL_FORMED_STOP: &str = "\
    ill-formed 'stop' attr, allowed forms are \
    '#[stop]', \
    '#[stop(ignore)]', and \
    '#[stop(explicit)]'\
";

const ERR_DUPLICATED_STOP: &str = "\
    duplicated 'stop' attribute\
";

const ERR_NO_STOP_EXPLICIT_HERE: &str = "\
    '#[stop(explicit)]' is not allowed here\
";

const ERR_NO_STOP_IGNORE_HERE: &str = "\
    '#[stop(ignore)]' is not allowed here\
";

const ERR_REDUNDANT_STOP: &str = "\
    redundant 'stop' attribute, it is implicitly supposed here\
";

const ERR_REDUNDANT_STOP_IGNORE: &str = "\
    redundant '#[stop(ignore)]' attribute, it is implicitly supposed here\
";

fn as_stop_attr(a: &Attribute) -> Option<Parsed<Stop>> {
    if a.path.leading_colon.is_some() || a.path.segments.trailing_punct() { return None; }
    let path = a.path.segments.iter().single().ok()?;
    if path.arguments != PathArguments::None || path.ident != "stop" { return None; }
    if a.tokens.is_empty() { return Some(Parsed::new(Stop::None, a)); }
    Some((|| {
        let token = a.tokens.clone().into_iter().single().ok()?;
        let group = try_match!(token, TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis).ok()?;
        let token = group.stream().into_iter().single().ok()?;
        match token {
            TokenTree::Ident(i) if i == "ignore" => Some(Parsed::new(Stop::Ignore, a)),
            TokenTree::Ident(i) if i == "explicit" => Some(Parsed::new(Stop::Explicit, a)),
            _ => None,
        }
    })().unwrap_or_else(|| abort!(a, ERR_ILL_FORMED_STOP)))
}

fn find_stop_attr(attrs: &[Attribute]) -> Option<Parsed<Stop>> {
    let mut attrs = attrs.iter().filter_map(as_stop_attr);
    let attr = attrs.next()?;
    if let Some(duplicated_attr) = attrs.next() {
        abort!(duplicated_attr.source, ERR_DUPLICATED_STOP);
    }
    Some(attr)
}

fn filter_implicit(attrs: &[Attribute]) -> bool {
    match find_stop_attr(attrs) {
        None => true,
        Some(Parsed { value: Stop::None, source }) => abort!(source, ERR_REDUNDANT_STOP),
        Some(Parsed { value: Stop::Explicit, source }) => abort!(source, ERR_NO_STOP_EXPLICIT_HERE),
        Some(Parsed { value: Stop::Ignore, .. }) => false,
    }
}

fn filter_explicit(attrs: &[Attribute]) -> bool {
    match find_stop_attr(attrs) {
        None => false,
        Some(Parsed { value: Stop::None, .. }) => true,
        Some(Parsed { value: Stop::Explicit, source }) => abort!(source, ERR_NO_STOP_EXPLICIT_HERE),
        Some(Parsed { value: Stop::Ignore, source }) => abort!(source, ERR_REDUNDANT_STOP_IGNORE),
    }
}

fn select_filter(attrs: &[Attribute]) -> fn(&[Attribute]) -> bool {
    match find_stop_attr(attrs) {
        None => filter_implicit,
        Some(Parsed { value: Stop::None, source }) => abort!(source, ERR_REDUNDANT_STOP),
        Some(Parsed { value: Stop::Explicit, .. }) => filter_explicit,
        Some(Parsed { value: Stop::Ignore, source }) => abort!(source, ERR_NO_STOP_IGNORE_HERE),
    }
}

fn dyn_context_crate(path: impl IntoIterator<Item=PathSegment>) -> Path {
    let c = crate_name("dyn-context").unwrap_or_else(|_| abort_call_site!("dyn-context dependency not found"));
    let (leading_colon, ident) = match c {
        FoundCrate::Itself => (None, Ident::new("crate", Span::call_site())),
        FoundCrate::Name(name) => (Some(Token![::](Span::call_site())), Ident::new(&name, Span::call_site())),
    };
    let mut segments = Punctuated::new();
    segments.push(PathSegment { ident, arguments: PathArguments::None });
    segments.extend(path);
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

fn filter_struct_fields(
    fields: Fields,
    filter: fn(&[Attribute]) -> bool
) -> Vec<(Type, Member)> {
    match fields {
        Fields::Named(FieldsNamed { named, .. }) => named.into_iter()
            .filter(|f| filter(&f.attrs))
            .map(|f| (f.ty, Member::Named(f.ident.unwrap())))
            .collect(),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed.into_iter()
            .enumerate()
            .filter(|(_, f)| filter(&f.attrs))
            .map(|(i, f)| (f.ty, Member::Unnamed(Index {
                index: i.try_into().unwrap(),
                span: Span::call_site()
            })))
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn call_struct_fields(
    data: DataStruct,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> bool
) -> (TokenStream, TokenStream) {
    let stop_trait = stop_trait();
    filter_struct_fields(data.fields, filter).into_iter()
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
}

#[proc_macro_error]
#[proc_macro_derive(Stop, attributes(stop))]
pub fn derive_stop(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, attrs, generics, .. } = parse(item).unwrap();
    let (g, r, w) = generics.split_for_impl();
    let filter = select_filter(&attrs);
    let state_var = Ident::new("state", Span::mixed_site());
    let (is_stopped, stop) = match data {
        Data::Struct(data) => call_struct_fields(data, &state_var, filter),
        Data::Enum(_) => abort_call_site!("'Stop' deriving is not supported for enums"),
        Data::Union(_) => abort_call_site!("'Stop' deriving is not supported for unions"),
    };
    let stop_trait = stop_trait();
    let state_trait = state_trait();
    quote! {
        impl #g #stop_trait #r for #ident #w {
            fn is_stopped(&self) -> bool { #is_stopped }

            fn stop(#state_var : &mut dyn #state_trait) { #stop }
        }
    }.into()
}
