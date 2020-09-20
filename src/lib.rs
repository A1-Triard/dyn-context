#![cfg_attr(feature="nightly", feature(never_type))]
#![deny(warnings)]

#![no_std]

use core::any::{TypeId, Any, type_name};

#[doc(hidden)]
pub use core::any::Any as std_any_Any;
#[doc(hidden)]
pub use core::any::TypeId as std_any_TypeId;
#[doc(hidden)]
pub use core::compile_error as std_compile_error;
#[doc(hidden)]
pub use core::concat as std_concat;
#[doc(hidden)]
pub use core::ops::FnOnce as std_ops_FnOnce;
#[doc(hidden)]
pub use core::option::Option as std_option_Option;
#[doc(hidden)]
pub use core::stringify as std_stringify;
#[doc(hidden)]
pub use generics::parse as generics_parse;
#[doc(hidden)]
pub use paste::paste as paste_paste;

/// A service provider pattern implementation = associated read-only container with type as a key.
///
/// Useful for building complex systems with callbacks without generic parameters.
///
/// # Examples
///
/// ```rust
/// mod call_back {
///     use dyn_context::Context;
///
///     pub struct CallBack {
///         callback: Option<fn(context: &mut dyn Context)>
///     }
///
///     impl CallBack {
///         pub fn new() -> Self { CallBack { callback: None } }
///
///         pub fn set_callback(&mut self, callback: fn(context: &mut dyn Context)) {
///             self.callback.replace(callback);
///         }
///
///         pub fn call_back(&self, context: &mut dyn Context) {
///             self.callback.map(|callback| callback(context));
///         }
///     }
/// }
///
/// use call_back::CallBack;
/// use dyn_context::{Context, ContextExt};
/// use macro_attr_2018::macro_attr;
/// use std::convert::Into;
///
/// macro_attr! {
///     #[derive(Context!)]
///     struct PrintContext {
///          value: String
///     }
/// }
///
/// # fn main() {
/// let mut call_back = CallBack::new();
/// call_back.set_callback(|context| {
///     let print: &PrintContext = context.get();
///     println!("{}", &print.value);
/// });
/// call_back.call_back(&mut PrintContext { value: "Hello, world!".into() });
/// # }
///
/// ```
/// 
/// For using `&str` instead of `String` the `context!` macro can be used:
/// ```rust
/// # mod call_back {
/// #     use dyn_context::Context;
/// # 
/// #     pub struct CallBack {
/// #         callback: Option<fn(context: &mut dyn Context)>
/// #     }
/// # 
/// #     impl CallBack {
/// #         pub fn new() -> Self { CallBack { callback: None } }
/// # 
/// #         pub fn set_callback(&mut self, callback: fn(context: &mut dyn Context)) {
/// #             self.callback.replace(callback);
/// #         }
/// # 
/// #         pub fn call_back(&self, context: &mut dyn Context) {
/// #             self.callback.map(|callback| callback(context));
/// #         }
/// #     }
/// # }
/// # 
/// use dyn_context::{context, ContextExt};
/// use call_back::CallBack;
///
/// context! {
///     struct PrintValue {
///         value: ref str
///     }
/// }
///
/// context! {
///     dyn struct PrintContext {
///         value: ref PrintValue
///     }
/// }
///
/// # fn main() {
/// let mut call_back = CallBack::new();
/// call_back.set_callback(|context| {
///     let print_value: &PrintValue = context.get();
///     println!("{}", print_value.value());
/// });
/// PrintValue::call("Hello, world!", |print_value| {
///     PrintContext::call(print_value, |context| call_back.call_back(context));
/// });
/// # }
/// ```
pub trait Context: 'static {
    /// Borrows shareable data entry.
    ///
    /// Prefer high-level [`get`](ContextExt::get) wrap.
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any>;

    /// Borrows mutable data entry.
    ///
    /// Prefer high-level [`get_mut`](ContextExt::get_mut) wrap.
    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any>;
}

#[cfg(feature="nightly")]
impl Context for ! {
    fn get_raw(&self, _ty: TypeId) -> Option<&dyn Any> { Some(self) }
    fn get_mut_raw(&mut self, _ty: TypeId) -> Option<&mut dyn Any> { Some(self) }
}

impl Context for () {
    fn get_raw(&self, _ty: TypeId) -> Option<&dyn Any> { None }
    fn get_mut_raw(&mut self, _ty: TypeId) -> Option<&mut dyn Any> { None }
}

/// Extends [`Context`](Context) with methods that make it easier to access the content of the context.
pub trait ContextExt: Context {
    /// Borrows shareable data reference.
    ///
    /// Panics if the context does not provide requested type.
    fn get<T: 'static>(&self) -> &T {
        self.get_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_ref::<T>().expect("invalid cast")
    }

    /// Borrows mutable data reference.
    ///
    /// Panics if the context does not provide requested type.
    fn get_mut<T: 'static>(&mut self) -> &mut T {
        self.get_mut_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_mut::<T>().expect("invalid cast")
    }
}

impl<T: Context + ?Sized> ContextExt for T { }

/// A [macro attribute](https://crates.io/crates/macro-attr-2018)
/// deriving trivial [`Context`](Context) implementation.
/// A trivial-implemented context is a context containing itself only.
///
/// # Examples
///
/// ```rust
/// # use dyn_context::{Context, ContextExt};
/// # use macro_attr_2018::macro_attr;
/// #
/// macro_attr! {
///     #[derive(Context!)]
///     struct SomeData {
///         data: u16,
///     }
/// }
///
/// fn get_data_from_context(context: &dyn Context) -> u16 {
///     let some_data: &SomeData = context.get();
///     some_data.data
/// }
///
/// # fn main() {
/// let some_data = SomeData { data: 7 };
/// let data_from_context = get_data_from_context(&some_data);
/// assert_eq!(data_from_context, 7);
/// # }
#[macro_export]
macro_rules! Context {
    (
        ()
        $vis:vis struct $name:ident $($body:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::Context { @impl [$name] } $($body)*
        }
    };
    (
        ()
        $vis:vis enum $name:ident $($body:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::Context { @impl [$name] } $($body)*
        }
    };
    (
        @impl [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*] $($body:tt)*
    ) => {
        impl $($g)* $crate::Context for $name $($r)* $($w)* {
            fn get_raw(
                &self,
                ty: $crate::std_any_TypeId
            ) -> $crate::std_option_Option<&dyn $crate::std_any_Any> {
                if ty == $crate::std_any_TypeId::of::<Self>() {
                    $crate::std_option_Option::Some(self)
                } else {
                    $crate::std_option_Option::None
                }
            }

            fn get_mut_raw(
                &mut self,
                ty: $crate::std_any_TypeId
            ) -> $crate::std_option_Option<&mut dyn $crate::std_any_Any> {
                if ty == $crate::std_any_TypeId::of::<Self>() {
                    $crate::std_option_Option::Some(self)
                } else {
                    $crate::std_option_Option::None
                }
            }
        }
    };
}

/// In Rust, lifetimes are intrusive, and sometimes it can lead to
/// an inadequately complex code. Moreover, in some cases it can lead to an _impossible code_,
/// means code so complex, so it can not make to compiles, even it is logically meaningful.
/// (Such situations could occur because Rust does not support existential types
/// with infinite parameters list.)
///
/// The `context` macro creates structure, allowing to pack several referencies into a
/// a one refernce to a `'static` type.
///
/// For example, using `context` you can pack two `str` referencies into a one:
/// ```rust
/// # use dyn_context::{context};
/// #
/// context! {
///     struct DoubleStr {
///         str_1: ref str,
///         str_2: ref str
///     }
/// }
///
/// fn call_back<T: 'static, R>(t: &T, callback: impl FnOnce(&T) -> R) -> R {
///     callback(t)
/// }
///
/// # fn main() {
/// let s_1 = String::from("str1");
/// let s_2 = String::from("str2");
/// let r = DoubleStr::call(&s_1[1..], &s_2[2..], |double_str| call_back(double_str, |double_str| {
///     format!("{}{}", double_str.str_1(), double_str.str_2())
/// }));
/// assert_eq!(r, "tr1r2");
/// # }
/// ```
#[macro_export]
macro_rules! context {
    (
        $(#[$attr:meta])*
        $vis:vis struct $name:ident $($body:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::context {
                @struct [$([$attr])*] [$vis] [$name]
            }
            $($body)*
        }
    };
    (
        $(#[$attr:meta])*
        $vis:vis dyn struct $name:ident $($body:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::context {
                @struct dyn [$([$attr])*] [$vis] [$name]
            }
            $($body)*
        }
    };
    (
        @struct [$([$attr:meta])*] [$vis:vis] [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        {
            $($(
                $field_1:ident $($field_2:ident)? : $field_mod:ident $field_ty:ty
            ),+ $(,)?)?
        }
    ) => {
        $crate::context! {
            @impl struct [static]
            [$name] [$([$attr])*] [$vis] [ty] [this]
            [$($g)*] [$($r)*] [$($w)*]
            [] [] [] []
            [$($([$field_1 $($field_2)? : $field_mod $field_ty])+)?]
        }
    };
    (
        @struct dyn [$([$attr:meta])*] [$vis:vis] [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        {
            $($(
                $field_1:ident $($field_2:ident)? : $field_mod:ident $field_ty:ty
            ),+ $(,)?)?
        }
    ) => {
        $crate::context! {
            @impl struct [dyn]
            [$name] [$([$attr])*] [$vis] [ty] [this]
            [$($g)*] [$($r)*] [$($w)*]
            [] [] [] []
            [$($([$field_1 $($field_2)? : $field_mod $field_ty])+)?]
        }
        $crate::context! {
            @impl trait
            [$name] [ty] [this]
            [$($g)*] [$($r)*] [$($w)*]
            [] []
            [$($([$field_1 $($field_2)? : $field_mod $field_ty])+)?]
        }
    };
    (
        @struct $(dyn)? [$([$attr:meta])*] [$vis:vis] [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        $($body:tt)*
    ) => {
        $crate::std_compile_error!("\
            invalid context definition, allowed form is\n\
            \n\
            $(#[attr])* $vis $(dyn)? struct $name {\n\
                $(dyn)? $field_1_name: $(ref | mut | const) $field_1_type,\n\
                $(dyn)? $field_2_name: $(ref | mut | const) $field_2_type,\n\
                ...\n\
            }\n\
            \n\
        ");
    };
    (
        @impl struct [static]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[dyn $($field_2:ident)? : $field_mod:ident $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::std_compile_error!($crate::std_concat!(
            "dynamic fields in non-dynamic context are not allowed, ",
            "consider changing 'struct' to 'dyn struct' or remove 'dyn' from field definition: '",
            $crate::std_stringify!(dyn $($field_2)? : $field_mod $field_ty),
            "'"
        ));
    };
    (
        @impl struct [$channel:ident]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : ref $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [$channel] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : *const $field_ty,
            ]
            [
                $($ctor_args)*
                $field : &$field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $field as *const $field_ty,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> &$field_ty { unsafe { &*self.$field } }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [dyn]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[dyn $field:ident : ref $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [dyn] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : *const $field_ty,
            ]
            [
                $($ctor_args)*
                $field : &$field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $field as *const $field_ty,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> &$field_ty { unsafe { &*self.$field } }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [$channel:ident]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : mut $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [$channel] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : *mut $field_ty,
            ]
            [
                $($ctor_args)*
                $field : &mut $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $field as *mut $field_ty,
            ]
            [
                $($struct_methods)*

                #[allow(dead_code)]
                $vis fn $field (&self) -> &$field_ty { unsafe { &*self.$field } }

                #[allow(dead_code)]
                $vis fn [< $field _mut >] (&mut self) -> &mut $field_ty { unsafe { &mut *self.$field } }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [dyn]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[dyn $field:ident : mut $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [dyn] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : *mut $field_ty,
            ]
            [
                $($ctor_args)*
                $field : &mut $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $field as *mut $field_ty,
            ]
            [
                $($struct_methods)*

                #[allow(dead_code)]
                $vis fn $field (&self) -> &$field_ty { unsafe { &*self.$field } }

                #[allow(dead_code)]
                $vis fn [< $field _mut >] (&mut self) -> &mut $field_ty { unsafe { &mut *self.$field } }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [$channel:ident]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : const $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [$channel] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : $field_ty,
            ]
            [
                $($ctor_args)*
                $field : $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> $field_ty { self.$field }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [dyn]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : const $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl struct [dyn] [$name] [$([$attr])*] [$vis] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($struct_fields)*
                $field : $field_ty,
            ]
            [
                $($ctor_args)*
                $field : $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> $field_ty { self.$field }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct [$channel:ident]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field_1:ident $($field_2:ident)? : $field_mod:ident $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::std_compile_error!($crate::std_concat!(
            "invalid context field '",
            $crate::std_stringify!($field_1 $($field_2)? : $field_mod $field_ty),
            "', allowed form is '$(dyn)? $name: $(const | ref | mut) $type'",
        ));
    };
    (
        @impl struct [$channel:ident]
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_args:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        []
    ) => {
        $crate::paste_paste! {
            $(#[$attr])*
            $vis struct $name $($g)* $($w)* {
                $($struct_fields)*
            }

            impl $($g)* $name $($r)* $($w)* {
                $vis fn call<ContextCallReturnType>(
                    $($ctor_args)*
                    f: impl $crate::std_ops_FnOnce(&mut Self) -> ContextCallReturnType 
                ) -> ContextCallReturnType {
                    let mut context = Self {
                        $($ctor_assignments)*
                    };
                    f(&mut context)
                }

                $($struct_methods)*
            }

            unsafe impl $($g)* Send for $name $($r)* $($w)* { }
            unsafe impl $($g)* Sync for $name $($r)* $($w)* { }
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[$field:ident : ref $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if $ty == $crate::std_any_TypeId::of::<$field_ty>() {
                    Some($this.$field())
                } else
            ]
            [
                $($trait_impl_mut)*
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[dyn $field:ident : ref $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if let Some(res) = $crate::Context::get_raw($this.$field(), $ty) {
                    Some(res)
                } else
            ]
            [
                $($trait_impl_mut)*
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[$field:ident : mut $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if $ty == $crate::std_any_TypeId::of::<$field_ty>() {
                    Some($this.$field())
                } else
            ]
            [
                $($trait_impl_mut)*
                if $ty == $crate::std_any_TypeId::of::<$field_ty>() {
                    Some($this. [< $field _mut >] ())
                } else
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[dyn $field:ident : mut $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if let Some(res) = $crate::Context::get_raw($this.$field(), $ty) {
                    Some(res)
                } else
            ]
            [
                $($trait_impl_mut)*
                if let Some(res) = $crate::Context::get_mut_raw($this. [< $field _mut >] (), $ty) {
                    Some(res)
                } else
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[$field:ident : const $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if $ty == $crate::std_any_TypeId::of::<$field_ty>() {
                    Some(&$this.$field)
                } else
            ]
            [
                $($trait_impl_mut)*
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[dyn $field:ident : const $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::context! {
            @impl trait [$name] [$ty] [$this] [$($g)*] [$($r)*] [$($w)*]
            [
                $($trait_impl_ref)*
                if let Some(res) = $crate::Context::get_raw(&$this.$field, $ty) {
                    Some(res)
                } else
            ]
            [
                $($trait_impl_mut)*
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        [[$field_1:ident $($field_2:ident)? : $field_mod:ident $field_ty:ty] $($other_fields:tt)*]
    ) => {
    };
    (
        @impl trait
        [$name:ident] [$ty:ident] [$this:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($trait_impl_ref:tt)*]
        [$($trait_impl_mut:tt)*]
        []
    ) => {
        $crate::paste_paste! {
            impl $($g)* $crate::Context for $name $($r)* $($w)* {
                fn get_raw(&self, $ty: $crate::std_any_TypeId) -> Option<&dyn $crate::std_any_Any> {
                    let $this = self;
                    $($trait_impl_ref)*
                    { None }
                }

                fn get_mut_raw(&mut self, $ty: $crate::std_any_TypeId) -> Option<&mut dyn $crate::std_any_Any> {
                    let $this = self;
                    $($trait_impl_mut)*
                    { None }
                }
            }
        }
    };
}

#[cfg(docsrs)]
pub mod example {
    pub struct Data {
        pub x: i16,
        pub y: i16
    }

    context! {
        pub struct StrData {
            r: ref str,
        }
    }

    context! {
        pub dyn struct ExampleContext {
            data: ref Data,
            str_data: mut StrData,
            id: const usize,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ContextExt;
    use core::mem::replace;

    context! {
        struct Context1 {
            a: const u8,
            b: ref u16,
            c: mut u32,
        }
    }

    #[test]
    fn test_context_1() {
        let mut x = 3;
        let res = Context1::call(1, &2, &mut x, |context| {
            assert_eq!(context.a(), 1u8);
            assert_eq!(context.b(), &2u16);
            assert_eq!(replace(context.c_mut(), 12), 3u32);
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    context! {
        dyn struct Context2 {
            a: const u8,
            b: ref u16,
            c: mut u32,
        }
    }

    #[test]
    fn test_context_2() {
        let mut x = 3;
        let res = Context2::call(1, &2, &mut x, |context| {
            assert_eq!(context.a(), 1u8);
            assert_eq!(context.b(), &2u16);
            assert_eq!(replace(context.c_mut(), 12), 3u32);
            assert_eq!(context.get::<u32>(), &12);
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    #[derive(Debug, Clone, Copy)]
    struct PrivStr;

    context! {
        dyn struct Context3 {
            a: const PrivStr,
            b: ref u16,
            c: mut u32,
        }
    }

    #[test]
    fn test_context_3() {
        let mut x = 3;
        let res = Context3::call(PrivStr, &2, &mut x, |context| {
            let _ = context.a();
            assert_eq!(context.b(), &2u16);
            assert_eq!(replace(context.c_mut(), 12), 3u32);
            assert_eq!(context.get::<u32>(), &12);
            assert_eq!(context.get::<u16>(), &2);
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    context! {
        dyn struct Context4<T> where T: Copy + 'static {
            dyn c_3: mut Context3,
            b: const T
        }
    }

    #[test]
    fn test_context_4() {
        let mut x = 3;
        let res = Context3::call(PrivStr, &2, &mut x, |context| {
            Context4::call(context, 7u8, |context| {
                assert_eq!(context.b(), 7);
                assert_eq!(replace(context.get_mut::<u32>(), 9), 3);
                assert_eq!(context.get::<u8>(), &7);
                "res"
            })
        });
        assert_eq!(res, "res");
        assert_eq!(x, 9);
    }
}
