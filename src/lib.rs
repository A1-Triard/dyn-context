#![cfg_attr(feature="nightly", feature(never_type))]
#![deny(warnings)]

#![no_std]
extern crate alloc;
pub(crate) mod std {
    pub use core::*;
}

use core::any::{TypeId, Any, type_name};

#[doc(hidden)]
pub use core::ops::FnOnce as std_ops_FnOnce;
#[doc(hidden)]
pub use core::any::Any as std_any_Any;
#[doc(hidden)]
pub use core::any::TypeId as std_any_TypeId;
#[doc(hidden)]
pub use paste::paste as paste_paste;

/// A service provider pattern implementation.
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
/// use std::convert::Into;
/// use dyn_context::{TrivialContext, ContextExt};
/// use call_back::CallBack;
///
/// struct PrintContext {
///     value: String
/// }
///
/// impl TrivialContext for PrintContext { }
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
///     struct PrintContext {
///         dyn value: ref PrintValue
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
///
/// ```
pub trait Context {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any>;
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

pub trait ContextExt: Context {
    fn get<T: 'static>(&self) -> &T {
        self.get_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_ref::<T>().expect("invalid cast")
    }

    fn get_mut<T: 'static>(&mut self) -> &mut T {
        self.get_mut_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_mut::<T>().expect("invalid cast")
    }
}

impl<T: Context + ?Sized> ContextExt for T { }

/// A `TrivialContext` implementer automatically implements [`Context`](Context)
/// as a context containing itself only.
///
/// # Examples
///
/// ```rust
/// # use dyn_context::{TrivialContext, Context, ContextExt};
/// #
/// struct SomeData {
///     data: u16,
/// }
///
/// impl TrivialContext for SomeData { }
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
pub trait TrivialContext: 'static { }

impl<T: TrivialContext> Context for T {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any> {
        if ty == TypeId::of::<Self>() {
            Some(self)
        } else {
            None
        }
    }

    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any> {
        if ty == TypeId::of::<Self>() {
            Some(self)
        } else {
            None
        }
    }
}

#[macro_export]
macro_rules! context {
    (
        $vis:vis struct $name:ident
        $(< $( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+ $(,)?>)?
        {
            $($(
                $field_1:ident $($field_2:ident)? : $field_mod:ident $field_ty:ty
            ),+ $(,)?)?
        }
    ) => {
        context! {
            @impl $name ($vis) ty this
            [ $(< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)?] [ $(< $( $lt ),+ >)?]
            {} {} {} {} {} {} { $($($field_1 $($field_2)? : $field_mod $field_ty),+)? }
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {$field:ident : ref $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : *const $ty}
            }
            {
                $({$($p)*})*
                {$field : &$ty}
            }
            {
                $({$($a)*})*
                {$field : $field as *const $ty}
            }
            {
                $({$($b)*})*
                {
                    $vis fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {dyn $field:ident : ref $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : *const $ty}
            }
            {
                $({$($p)*})*
                {$field : &$ty}
            }
            {
                $({$($a)*})*
                {$field : $field as *const $ty}
            }
            {
                $({$($b)*})*
                {
                    $vis fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                }
            }
            {
                $($d)*
                if $tr == $crate::std_any_TypeId::of::<$ty>() {
                    Some($this.$field())
                } else
            }
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {$field:ident : mut $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : *mut $ty}
            }
            {
                $({$($p)*})*
                {$field : &mut $ty}
            }
            {
                $({$($a)*})*
                {$field : $field as *mut $ty}
            }
            {
                $({$($b)*})*
                {
                    #[allow(dead_code)]
                    $vis fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                    #[allow(dead_code)]
                    $vis fn [< $field _mut >] (&mut self) -> &mut $ty { unsafe { &mut *self.$field } }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {dyn $field:ident : mut $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : *mut $ty}
            }
            {
                $({$($p)*})*
                {$field : &mut $ty}
            }
            {
                $({$($a)*})*
                {$field : $field as *mut $ty}
            }
            {
                $({$($b)*})*
                {
                    #[allow(dead_code)]
                    $vis fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                    #[allow(dead_code)]
                    $vis fn [< $field _mut >] (&mut self) -> &mut $ty { unsafe { &mut *self.$field } }
                }
            }
            {
                $($d)*
                if $tr == $crate::std_any_TypeId::of::<$ty>() {
                    Some($this.$field())
                } else
            }
            {
                $($m)*
                if $tr == $crate::std_any_TypeId::of::<$ty>() {
                    Some($this.[< $field _mut >]())
                } else
            }
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {$field:ident : const $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : $ty}
            }
            {
                $({$($p)*})*
                {$field : $ty}
            }
            {
                $({$($a)*})*
                {$field}
            }
            {
                $({$($b)*})*
                {
                    $vis fn $field (&self) -> $ty { self.$field }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {dyn $field:ident : const $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name ($vis) $tr $this
            [$($g)*] [$($r)*]
            {
                $({$($f)*})*
                {$field : $ty}
            }
            {
                $({$($p)*})*
                {$field : $ty}
            }
            {
                $({$($a)*})*
                {$field}
            }
            {
                $({$($b)*})*
                {
                    $vis fn $field (&self) -> $ty { self.$field }
                }
            }
            {
                $($d)*
                if $tr == $crate::std_any_TypeId::of::<$ty>() {
                    Some(&$this.$field)
                } else
            }
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident ($vis:vis) $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*} {$({$($p:tt)*})*} {$({$($a:tt)*})*} {$({$($b:tt)*})*}
        {$($d:tt)*} {$($m:tt)*} {}
    ) => {
        $crate::paste_paste! {
            $vis struct $name $($g)* {
                $($($f)*),*
            }

            impl $($g)* $name $($r)* {
                $vis fn call<ContextCallReturnType>(
                    $($($p)*),*,
                    f: impl $crate::std_ops_FnOnce(&mut Self) -> ContextCallReturnType 
                ) -> ContextCallReturnType {
                    let mut context = Self {
                        $($($a)*),*
                    };
                    f(&mut context)
                }

                $($($b)*)*
            }

            unsafe impl $($g)* Send for $name $($r)* { }
            unsafe impl $($g)* Sync for $name $($r)* { }

            context! { @impl (dyn) $name $tr $this [$($g:tt)*] [$($r:tt)*] {$($d)*} {$($m)*} }
        }
    };
    (
        @impl (dyn) $name:ident $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {} {}
    ) => {
    };
    (
        @impl (dyn) $name:ident $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$($d:tt)*} {$($m:tt)*}
    ) => {
        impl $($g)* $crate::Context for $name $($r)* {
            fn get_raw(&self, $tr: $crate::std_any_TypeId) -> Option<&dyn $crate::std_any_Any> {
                let $this = self;
                $($d)*
                { None }
            }

            fn get_mut_raw(&mut self, $tr: $crate::std_any_TypeId) -> Option<&mut dyn $crate::std_any_Any> {
                let $this = self;
                $($m)*
                { None }
            }
        }
    };
}

#[cfg(docsrs)]
pub mod example {
    use core::fmt::Display;

    pub struct Data {
        pub x: i16,
        pub y: i16
    }

    context! {
        pub struct ExampleContext {
            dyn data: mut Data,
            display: ref (dyn Display + 'static),
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
        struct Context2 {
            a: const u8,
            b: ref u16,
            dyn c: mut u32,
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
        struct Context3 {
            a: const PrivStr,
            dyn b: ref u16,
            dyn c: mut u32,
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
}
