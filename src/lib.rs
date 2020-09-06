#![cfg_attr(feature="nightly", feature(never_type))]
#![deny(warnings)]

#![no_std]
extern crate alloc;
pub(crate) mod std {
    pub use core::*;
}

use core::any::{TypeId, Any};

#[doc(hidden)]
pub use core::ops::FnOnce as std_ops_FnOnce;
#[doc(hidden)]
pub use core::any::Any as std_any_Any;
#[doc(hidden)]
pub use core::any::TypeId as std_any_TypeId;

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
    fn get<T: 'static>(&self) -> Option<&T> {
        self.get_raw(TypeId::of::<T>()).map(|x| x.downcast_ref::<T>().expect("invalid cast"))
    }

    fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.get_mut_raw(TypeId::of::<T>()).map(|x| x.downcast_mut::<T>().expect("invalid cast"))
    }
}

impl<T: Context + ?Sized> ContextExt for T { }

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
        mod $name:ident
        $(< $( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+ $(,)?>)?
        {
            $($(
                $field_1:ident $($field_2:ident)? $(/ $field_mut:ident)? : $field_mod:ident $field_ty:ty
            ),+ $(,)?)?
        }
    ) => {
        mod $name {
            #[allow(unused_imports)]
            use super::*;

            context! {
                @impl Context ty this
                [ $(< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)?] [ $(< $( $lt ),+ >)?]
                {} {} {} {} {} {} { $($($field_1 $($field_2)? $(/ $field_mut)? : $field_mod $field_ty),+)? }
            }
        }
    };
    (
        @impl $name:ident $tr:ident $this:ident
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
            @impl $name $tr $this
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
                    pub fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident $tr:ident $this:ident
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
            @impl $name $tr $this
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
                    pub fn $field (&self) -> &$ty { unsafe { &*self.$field } }
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
        @impl $name:ident $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {$field:ident / $field_mut:ident : mut $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name $tr $this
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
                    pub fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                    #[allow(dead_code)]
                    pub fn $field_mut (&mut self) -> &mut $ty { unsafe { &mut *self.$field } }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*}
        {$({$($p:tt)*})*}
        {$({$($a:tt)*})*}
        {$({$($b:tt)*})*}
        {$($d:tt)*}
        {$($m:tt)*}
        {dyn $field:ident / $field_mut:ident : mut $ty:ty $(, $($other_fields:tt)+)?}
    ) => {
        context! {
            @impl $name $tr $this
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
                    pub fn $field (&self) -> &$ty { unsafe { &*self.$field } }
                    #[allow(dead_code)]
                    pub fn $field_mut (&mut self) -> &mut $ty { unsafe { &mut *self.$field } }
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
                    Some($this.$field_mut())
                } else
            }
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident $tr:ident $this:ident
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
            @impl $name $tr $this
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
                    pub fn $field (&self) -> $ty { self.$field }
                }
            }
            {$($d)*}
            {$($m)*}
            {$($($other_fields)+)?}
        }
    };
    (
        @impl $name:ident $tr:ident $this:ident
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
            @impl $name $tr $this
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
                    pub fn $field (&self) -> $ty { self.$field }
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
        @impl $name:ident $tr:ident $this:ident
        [$($g:tt)*] [$($r:tt)*]
        {$({$($f:tt)*})*} {$({$($p:tt)*})*} {$({$($a:tt)*})*} {$({$($b:tt)*})*}
        {$($d:tt)*} {$($m:tt)*} {}
    ) => {
        pub struct $name $($g)* {
            $($($f)*),*
        }

        impl $($g)* $name $($r)* {
            pub fn call<ContextCallReturnType>(
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
        mod example_context {
            dyn data/data_mut: mut Data,
            display: ref dyn Display,
            id: const usize,
        }
    }

    pub use example_context::Context as ExampleContext;
}

#[cfg(test)]
mod test {
    use crate::ContextExt;
    use core::mem::replace;

    context! {
        mod context_1 {
            a: const u8,
            b: ref u16,
            c/c_mut: mut u32,
        }
    }

    type Context1 = context_1::Context;

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
        mod context_2 {
            a: const u8,
            b: ref u16,
            dyn c/c_mut: mut u32,
        }
    }

    pub type Context2 = context_2::Context;

    #[test]
    fn test_context_2() {
        let mut x = 3;
        let res = Context2::call(1, &2, &mut x, |context| {
            assert_eq!(context.a(), 1u8);
            assert_eq!(context.b(), &2u16);
            assert_eq!(replace(context.c_mut(), 12), 3u32);
            assert_eq!(context.get::<u32>(), Some(&12));
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    context! {
        mod context_3 {
            a: const u8,
            dyn b: ref u16,
            dyn c/c_mut: mut u32,
        }
    }

    pub type Context3 = context_3::Context;

    #[test]
    fn test_context_3() {
        let mut x = 3;
        let res = Context3::call(1, &2, &mut x, |context| {
            assert_eq!(context.a(), 1u8);
            assert_eq!(context.b(), &2u16);
            assert_eq!(replace(context.c_mut(), 12), 3u32);
            assert_eq!(context.get::<u32>(), Some(&12));
            assert_eq!(context.get::<u16>(), Some(&2));
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }
}
