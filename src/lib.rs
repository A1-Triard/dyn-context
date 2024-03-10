#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]
#![allow(clippy::mixed_attributes_style)]
#![allow(unknown_lints)]

#![no_std]

#[doc=include_str!("../README.md")]
type _DocTestReadme = ();

#[doc(hidden)]
pub use core::concat as std_concat;
#[doc(hidden)]
pub use core::ops::FnOnce as std_ops_FnOnce;
#[doc(hidden)]
pub use core::stringify as std_stringify;
#[doc(hidden)]
pub use generics::concat as generics_concat;
#[doc(hidden)]
pub use generics::parse as generics_parse;
#[doc(hidden)]
pub use indoc::indoc as indoc_indoc;
#[doc(hidden)]
pub use paste::paste as paste_paste;

/// Creates structure, allowing to pack several references into
/// a one reference to a `'static` type.
///
/// Accepts input in the following form:
///
/// ```ignore
/// $(#[$attr:meta])*
/// $vis:vis struct $name:ident $(<$generics> $(where $where_clause)?)? {
///     $($(
///         $field:ident : $($lt:lifetime ref | $lt:lifetime mut | const) $ty:ty
///     ),+ $(,)?)?
/// }
/// ```
///
/// In Rust, lifetimes are intrusive, and sometimes it can lead to
/// an inadequately complex code. Moreover, in some cases it can lead to an _impossible code_,
/// means code so complex, so it can not make to compiles, even it is logically meaningful.
/// (Such situations could occur because Rust does not support existential types
/// with infinite parameters list.)
///
/// The `free_lifetimes!` macro allows to "compress" several lifetimes into a one.
///
/// For example, you can pack together two `str` references and use them with
/// a code, requiring a `'static` type:
///
/// ```rust
/// # use dyn_context::{free_lifetimes};
/// #
/// free_lifetimes! {
///     struct DoubleStr {
///         str_1: 'str_1 ref str,
///         str_2: 'str_2 ref str
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
/// let r = DoubleStrBuilder {
///     str_1: &s_1[1..],
///     str_2: &s_2[2..]
/// }.build_and_then(|double_str| call_back(double_str, |double_str| {
///     format!("{}{}", double_str.str_1(), double_str.str_2())
/// }));
/// assert_eq!(r, "tr1r2");
/// # }
/// ```
#[macro_export]
macro_rules! free_lifetimes {
    (
        $(#[$attr:meta])*
        $vis:vis struct $name:ident $($token:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::free_lifetimes_impl {
                @struct [$([$attr])*] [$vis] [$name]
            }
            $($token)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! free_lifetimes_impl {
    (
        @struct [$([$attr:meta])*] [$vis:vis] [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        {
            $($(
                $field:ident : $($field_lt:lifetime)? $field_mod:ident $field_ty:ty
            ),+ $(,)?)?
        }
    ) => {
        $crate::free_lifetimes_impl! {
            @impl struct
            [$name] [$([$attr])*] [$vis] [ty] [this] [builder]
            [$($g)*] [$($r)*] [$($w)*]
            [] [] [] [] []
            [$($([$field : $($field_lt)? $field_mod $field_ty])+)?]
        }
    };
    (
        @struct [$([$attr:meta])*] [$vis:vis] [$name:ident] [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        $($body:tt)*
    ) => {
        $crate::std_compile_error!($crate::indoc_indoc!("
            invalid free lifetimes struct definition, allowed form is

            $(#[$attr:meta])*
            $vis:vis struct $name:ident $(<$generics> $(where $where_clause)?)? {
                $($(
                    $field:ident : $($lt:lifetime ref | $lt:lifetime mut | const) $ty:ty
                ),+ $(,)?)?
            }

        "));
    };
    (
        @impl struct
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($builder_lts:tt)*]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : $field_lt:lifetime ref $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::free_lifetimes_impl! {
            @impl struct [$name] [$([$attr])*] [$vis] [$ty] [$this] [$builder] [$($g)*] [$($r)*] [$($w)*]
            [
                $($builder_lts)*
                [$field_lt]
            ]
            [
                $($builder_fields)*
                pub $field : & $field_lt $field_ty,
            ]
            [
                $($struct_fields)*
                $field : &'static $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : unsafe { &*($builder . $field as *const $field_ty) },
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> &$field_ty { self.$field }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($builder_lts:tt)*]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : $field_lt:lifetime mut $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::free_lifetimes_impl! {
            @impl struct [$name] [$([$attr])*] [$vis] [$ty] [$this] [$builder] [$($g)*] [$($r)*] [$($w)*]
            [
                $($builder_lts)*
                [$field_lt]
            ]
            [
                $($builder_fields)*
                pub $field : & $field_lt mut $field_ty,
            ]
            [
                $($struct_fields)*
                $field : &'static mut $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : unsafe { &mut *($builder . $field as *mut $field_ty) },
            ]
            [
                $($struct_methods)*

                #[allow(dead_code)]
                $vis fn $field (&self) -> &$field_ty { self.$field }

                #[allow(dead_code)]
                $vis fn [< $field _mut >] (&mut self) -> &mut $field_ty { self.$field }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($builder_lts:tt)*]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : const $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::free_lifetimes_impl! {
            @impl struct [$name] [$([$attr])*] [$vis] [$ty] [$this] [$builder] [$($g)*] [$($r)*] [$($w)*]
            [$($builder_lts)*]
            [
                $($builder_fields)*
                pub $field : $field_ty,
            ]
            [
                $($struct_fields)*
                $field : $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field: $builder . $field,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> $field_ty { self.$field }
            ]
            [$($other_fields)*]
        }
    };
    (
        @impl struct
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($builder_lts:tt)*]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [[$field:ident : $($field_lt:lifetime)? $field_mod:ident $field_ty:ty] $($other_fields:tt)*]
    ) => {
        $crate::std_compile_error!($crate::std_concat!(
            "invalid free lifetimes struct field '",
            $crate::std_stringify!($field : $($field_lt)? $field_mod $field_ty),
            "\
                ', allowed form is '\
                $field:ident : $($lt:lifetime ref | $lt:lifetime mut | const) $ty:ty\
                '\
            ",
        ));
    };
    (
        @impl struct
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($([$builder_lts:tt])+)?]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        []
    ) => {    
        $crate::generics_concat! {
            $crate::free_lifetimes_impl {
                @impl [$name] [$([$attr])*] [$vis] [$ty] [$this] [$builder] [$($g)*] [$($r)*] [$($w)*]
                [$($builder_fields)*]
                [$($struct_fields)*]
                [$($ctor_assignments)*]
                [$($struct_methods)*]
            }
            [$( < $($builder_lts),+ > )?] [$( < $($builder_lts),+ > )?] [],
            [$($g)*] [$($r)*] [$($w)*]
        }
    };
    (
        @impl
        [$name:ident] [$([$attr:meta])*] [$vis:vis] [$ty:ident] [$this:ident] [$builder:ident]
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        [$($builder_fields:tt)*]
        [$($struct_fields:tt)*]
        [$($ctor_assignments:tt)*]
        [$($struct_methods:tt)*]
        [$($builder_g:tt)*] [$($builder_r:tt)*] [$($builder_w:tt)*]
    ) => {
        $crate::paste_paste! {
            $vis struct [< $name Builder >] $($builder_g)* $($w)* {
                $($builder_fields)*
            }

            impl $($builder_g)* [< $name Builder >] $($builder_r)* $($builder_w)* {
                /// Converts regular structure into a special structure with "erased" field lifetimes
                /// and passes result to provided function.
                $vis fn build_and_then<FreeLifetimesStructBuildReturnType>(
                    &mut self,
                    f: impl $crate::std_ops_FnOnce(&mut $name $($r)*) -> FreeLifetimesStructBuildReturnType
                ) -> FreeLifetimesStructBuildReturnType {
                    let $builder = self;
                    let mut free_lifetimes = $name {
                        $($ctor_assignments)*
                    };
                    f(&mut free_lifetimes)
                }
            }
                        
            $(#[$attr])*
            $vis struct $name $($g)* $($w)* {
                $($struct_fields)*
            }

            impl $($g)* $name $($r)* $($w)* {
                $($struct_methods)*
            }
        }
    };
}

#[cfg(docsrs)]
pub mod example {
    //! [`free_lifetimes`] macro expansion example.
    //!
    //! ```ignore
    //! free_lifetimes! {
    //!     pub struct FreeLifetimesStruct {
    //!         data: 'data mut Data,
    //!         str_data: 'str_data ref str,
    //!         id: const usize,
    //!     }
    //! }
    //!
    //! ```

    pub struct Data {
        pub x: i16,
        pub y: i16
    }

    free_lifetimes! {
        pub struct FreeLifetimesStruct {
            data: 'data mut Data,
            str_data: 'str_data ref str,
            id: const usize,
        }
    }
}

#[cfg(test)]
mod test {
    use core::mem::replace;
    use core::ops::Deref;

    free_lifetimes! {
        struct State1 {
            a: const u8,
            b: 'b ref u16,
            c: 'c mut u32,
        }
    }
    
    #[test]
    fn test_state_1() {
        let mut x = 3;
        let res = State1Builder {
            a: 1,
            b: &2,
            c: &mut x
        }.build_and_then(|state| {
            assert_eq!(state.a(), 1u8);
            assert_eq!(state.b(), &2u16);
            assert_eq!(replace(state.c_mut(), 12), 3u32);
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    #[test]
    fn test_state_1_const() {
        let mut x = 3;
        let res = State1Builder {
            a: 1,
            b: &2,
            c: &mut x
        }.build_and_then(|state| {
            assert_eq!(state.a(), 1u8);
            assert_eq!(state.b(), &2u16);
            assert_eq!(state.c(), &3u32);
            "res"
        });
        assert_eq!(res, "res");
        assert_eq!(x, 3);
    }

    #[allow(dead_code)]
    #[derive(Debug, Clone, Copy)]
    struct PrivStr;

    free_lifetimes! {
        #[derive(Debug)]
        pub struct Items<ItemType: 'static> {
            items: 'items ref [ItemType],
        }
    }

    impl<ItemType> Deref for Items<ItemType> {
        type Target = [ItemType];

        fn deref(&self) -> &Self::Target {
            self.items()
        }
    }

    #[test]
    fn free_lifetimes_with_generics() {
        let items = &[1, 2, 3];
        let sum: u8 = ItemsBuilder {
            items
        }.build_and_then(|items| {
            items.iter().sum()
        });
        assert_eq!(sum, 6);
    }
}
