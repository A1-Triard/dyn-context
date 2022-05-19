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
use syn::{AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Fields, FieldsNamed};
use syn::{FieldsUnnamed, GenericArgument, Index, Member, Path, PathArguments, PathSegment};
use syn::{Token, TraitBound, TraitBoundModifier, Type, TypeParamBound, TypeReference, TypeTraitObject, parse};
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
    Implicit,
    Explicit,
}

const ERR_ILL_FORMED_STOP: &str = "\
    ill-formed 'stop' attr, allowed forms are \
    '#[stop]', \
    '#[stop(ignore)]', \
    '#[stop(implicit)]', and \
    '#[stop(explicit)]'\
";

const ERR_DUPLICATED_STOP: &str = "\
    duplicated 'stop' attribute\
";

const ERR_NO_STOP_EXPLICIT_HERE: &str = "\
    '#[stop(explicit)]' is not allowed here\
";

const ERR_NO_STOP_IMPLICIT_HERE: &str = "\
    '#[stop(implicit)]' is not allowed here\
";

const ERR_NO_STOP_IGNORE_HERE: &str = "\
    '#[stop(ignore)]' is not allowed here\
";

const ERR_REDUNDANT_STOP_ON_STRUCT: &str = "\
    redundant 'stop' attribute, use \
    '#[stop(implicit)]' or \
    '#[stop(explicit)] \
    to manually choose fields selection rule\
";

const ERR_REDUNDANT_STOP: &str = "\
    redundant '#[stop]' attribute, it is implicitly supposed here\
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
            TokenTree::Ident(i) if i == "implicit" => Some(Parsed::new(Stop::Implicit, a)),
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
        Some(Parsed { value: Stop::None, source }) => abort!(source, ERR_REDUNDANT_STOP_ON_STRUCT),
        Some(Parsed { value: Stop::Explicit, source }) => abort!(source, ERR_NO_STOP_EXPLICIT_HERE),
        Some(Parsed { value: Stop::Implicit, source }) => abort!(source, ERR_NO_STOP_IMPLICIT_HERE),
        Some(Parsed { value: Stop::Ignore, .. }) => false,
    }
}

fn filter_explicit(attrs: &[Attribute]) -> bool {
    match find_stop_attr(attrs) {
        None => false,
        Some(Parsed { value: Stop::None, .. }) => true,
        Some(Parsed { value: Stop::Explicit, source }) => abort!(source, ERR_NO_STOP_EXPLICIT_HERE),
        Some(Parsed { value: Stop::Implicit, source }) => abort!(source, ERR_NO_STOP_IMPLICIT_HERE),
        Some(Parsed { value: Stop::Ignore, source }) => abort!(source, ERR_REDUNDANT_STOP_IGNORE),
    }
}

fn select_filter(attrs: &[Attribute], named_fields: bool) -> fn(&[Attribute]) -> bool {
    match find_stop_attr(attrs) {
        None => if named_fields { filter_explicit } else { filter_implicit },
        Some(Parsed { value: Stop::None, source }) => abort!(source, ERR_REDUNDANT_STOP),
        Some(Parsed { value: Stop::Explicit, .. }) => filter_explicit,
        Some(Parsed { value: Stop::Implicit, .. }) => filter_implicit,
        Some(Parsed { value: Stop::Ignore, source }) => abort!(source, ERR_NO_STOP_IGNORE_HERE),
    }
}

fn dyn_context_crate(path: impl IntoIterator<Item=PathSegment>) -> Path {
    let c = crate_name("dyn-context").unwrap_or_else(|_| abort_call_site!("dyn-context dependency not found"));
    let name = match &c {
        FoundCrate::Itself => "dyn_context",
        FoundCrate::Name(name) => name,
    };
    let mut segments = Punctuated::new();
    segments.push(PathSegment { ident: Ident::new(name, Span::call_site()), arguments: PathArguments::None });
    segments.extend(path);
    Path { leading_colon: Some(Token![::](Span::call_site())), segments }
}

fn stop_trait() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("Stop", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn state_trait() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("State", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn iterate_struct_fields(
    fields: Fields,
) -> Vec<(Vec<Attribute>, Type, Member)> {
    match fields {
        Fields::Named(FieldsNamed { named, .. }) => named.into_iter()
            .map(|f| (f.attrs, f.ty, Member::Named(f.ident.unwrap())))
            .collect(),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed.into_iter()
            .enumerate()
            .map(|(i, f)| (f.attrs, f.ty, Member::Unnamed(Index {
                index: i.try_into().unwrap(),
                span: Span::call_site()
            })))
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn call_stop_struct_fields(
    data: DataStruct,
    state_var: &Ident,
    filter: fn(&[Attribute]) -> bool
) -> (TokenStream, TokenStream) {
    let stop_trait = stop_trait();
    iterate_struct_fields(data.fields).into_iter()
        .filter(|(attrs, _, _)| filter(attrs))
        .map(|(_, ty, member)| (
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
    let state_var = Ident::new("state", Span::mixed_site());
    let (is_stopped, stop) = match data {
        Data::Struct(data) => {
            let filter = select_filter(&attrs, matches!(&data.fields, Fields::Named { .. }));
            call_stop_struct_fields(data, &state_var, filter)
        },
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

enum State {
    None,
    Part,
}

const ERR_ILL_FORMED_STATE: &str = "\
    ill-formed 'state' attr, allowed forms are '#[state]', and '#[state(part)]'\
";

const ERR_DUPLICATED_STATE: &str = "\
    duplicated 'state' attribute\
";

const ERR_REDUNDANT_STATE_ON_STRUCT: &str = "\
    redundant 'state' attribute, use \
    '#[state(part)]' \
    to include the struct as itself part\
";

fn as_state_attr(a: &Attribute) -> Option<Parsed<State>> {
    if a.path.leading_colon.is_some() || a.path.segments.trailing_punct() { return None; }
    let path = a.path.segments.iter().single().ok()?;
    if path.arguments != PathArguments::None || path.ident != "state" { return None; }
    if a.tokens.is_empty() { return Some(Parsed::new(State::None, a)); }
    Some((|| {
        let token = a.tokens.clone().into_iter().single().ok()?;
        let group = try_match!(token, TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis).ok()?;
        let token = group.stream().into_iter().single().ok()?;
        match token {
            TokenTree::Ident(i) if i == "part" => Some(Parsed::new(State::Part, a)),
            _ => None,
        }
    })().unwrap_or_else(|| abort!(a, ERR_ILL_FORMED_STATE)))
}

fn find_state_attr(attrs: &[Attribute]) -> Option<Parsed<State>> {
    let mut attrs = attrs.iter().filter_map(as_state_attr);
    let attr = attrs.next()?;
    if let Some(duplicated_attr) = attrs.next() {
        abort!(duplicated_attr.source, ERR_DUPLICATED_STATE);
    }
    Some(attr)
}

fn option_type(ty: Type) -> Path {
    let mut args = Punctuated::new();
    args.push(GenericArgument::Type(ty));
    dyn_context_crate([
        PathSegment {
            ident: Ident::new("std_option_Option", Span::call_site()),
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Token![<](Span::call_site()),
                gt_token: Token![>](Span::call_site()),
                args
            })
        },
    ])
}

fn any_trait() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("std_any_Any", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn option_none() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("std_option_Option", Span::call_site()), arguments: PathArguments::None },
        PathSegment { ident: Ident::new("None", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn option_some() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("std_option_Option", Span::call_site()), arguments: PathArguments::None },
        PathSegment { ident: Ident::new("Some", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn type_id() -> Path {
    dyn_context_crate([
        PathSegment { ident: Ident::new("std_any_TypeId", Span::call_site()), arguments: PathArguments::None },
    ])
}

fn option_dyn_any(mutable: bool) -> Path {
    let mut bounds = Punctuated::new();
    bounds.push(TypeParamBound::Trait(TraitBound {
        paren_token: None,
        modifier: TraitBoundModifier::None,
        lifetimes: None,
        path: any_trait()
    }));
    option_type(Type::Reference(TypeReference {
        and_token: Token![&](Span::call_site()),
        lifetime: None,
        mutability: if mutable { Some(Token![mut](Span::call_site())) } else { None },
        elem: Box::new(Type::TraitObject(TypeTraitObject {
            dyn_token: Some(Token![dyn](Span::call_site())),
            bounds
        }))
    }))
}

fn call_state_struct_fields(
    data: DataStruct,
    attrs: &[Attribute],
    ty_var: &Ident,
) -> (TokenStream, TokenStream) {
    let state_trait = state_trait();
    let option_none = option_none();
    let option_some = option_some();
    let type_id = type_id();
    let x_var = Ident::new("x", Span::mixed_site());
    let (get_raw, get_mut_raw) = iterate_struct_fields(data.fields).into_iter()
        .fold((quote! { #option_none }, quote! { #option_none }), |
            (get_raw, get_mut_raw),
            (attrs, ty, member)
        | match find_state_attr(&attrs) {
            None => (get_raw, get_mut_raw),
            Some(Parsed { value: State::None, .. }) => (
                quote! {
                    if let #option_some (#x_var) = <#ty as #state_trait>::get_raw(&self. #member, #ty_var) {
                        #option_some (#x_var)
                    } else {
                        #get_raw
                    }
                },
                quote! {
                    if let #option_some (#x_var) = <#ty as #state_trait>::get_mut_raw(&mut self. #member, #ty_var) {
                        #option_some (#x_var)
                    } else {
                        #get_mut_raw
                    }
                },
            ),
            Some(Parsed { value: State::Part, .. }) => (
                quote! { if #ty_var == <#type_id>::of::<#ty>() { #option_some (&self. #member) } else { #get_raw } },
                quote! { if #ty_var == <#type_id>::of::<#ty>() { #option_some (&mut self. #member) } else { #get_mut_raw } },
            ),
        })
    ;
    match find_state_attr(attrs) {
        None => (get_raw, get_mut_raw),
        Some(Parsed { source, value: State::None }) => abort!(source, ERR_REDUNDANT_STATE_ON_STRUCT),
        Some(Parsed { value: State::Part, .. }) => (
            quote! { if #ty_var == <#type_id>::of::<Self>() { #option_some (self) } else { #get_raw } },
            quote! { if #ty_var == <#type_id>::of::<Self>() { #option_some (self) } else { #get_mut_raw } },
        ),
    }
}

#[proc_macro_error]
#[proc_macro_derive(State, attributes(state))]
pub fn derive_state(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, attrs, generics, .. } = parse(item).unwrap();
    let (g, r, w) = generics.split_for_impl();
    let ty_var = Ident::new("ty", Span::mixed_site());
    let (get_raw, get_mut_raw) = match data {
        Data::Struct(data) => call_state_struct_fields(data, &attrs, &ty_var),
        Data::Enum(_) => abort_call_site!("'State' deriving is not supported for enums"),
        Data::Union(_) => abort_call_site!("'State' deriving is not supported for unions"),
    };
    let state_trait = state_trait();
    let option_ref_dyn_any = option_dyn_any(false);
    let option_mut_dyn_any = option_dyn_any(true);
    let type_id = type_id();
    quote! {
        impl #g #state_trait #r for #ident #w {
            fn get_raw(&self, #ty_var : #type_id) -> #option_ref_dyn_any { #get_raw }

            fn get_mut_raw(&mut self, #ty_var : #type_id) -> #option_mut_dyn_any { #get_mut_raw }
        }
    }.into()
}
