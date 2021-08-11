//! **Crate features**
//!
//! * `"nightly"`
//! Enabled by default. Disable to make the library compatible with stable and beta Rust channels.

#![cfg_attr(feature="nightly", feature(never_type))]
#![cfg_attr(feature="nightly", feature(thread_local))]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![no_std]

#[cfg(feature="nightly")]
include!("doc_test_readme.include");

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
pub use generics::concat as generics_concat;
#[doc(hidden)]
pub use generics::parse as generics_parse;
#[doc(hidden)]
pub use paste::paste as paste_paste;

use core::any::{TypeId, Any, type_name};
#[cfg(feature="nightly")]
use core::cell::{self, RefCell};
#[cfg(feature="nightly")]
use core::ops::{Deref, DerefMut};
#[cfg(feature="nightly")]
use core::ptr::NonNull;

/// A service provider pattern implementation = associated read-only container with type as a key.
///
/// Useful for building complex systems with callbacks without generic parameters.
///
/// # Examples
///
/// ```rust
/// mod call_back {
///     use dyn_context::State;
///
///     pub struct CallBack {
///         callback: Option<fn(state: &mut dyn State)>
///     }
///
///     impl CallBack {
///         pub fn new() -> Self { CallBack { callback: None } }
///
///         pub fn set_callback(&mut self, callback: fn(state: &mut dyn State)) {
///             self.callback.replace(callback);
///         }
///
///         pub fn call_back(&self, state: &mut dyn State) {
///             self.callback.map(|callback| callback(state));
///         }
///     }
/// }
///
/// use call_back::CallBack;
/// use dyn_context::{SelfState, StateExt};
/// use std::convert::Into;
///
/// struct PrintState {
///      value: String
/// }
///
/// impl SelfState for PrintState { }
///
/// # fn main() {
/// let mut call_back = CallBack::new();
/// call_back.set_callback(|state| {
///     let print: &PrintState = state.get();
///     println!("{}", &print.value);
/// });
/// call_back.call_back(&mut PrintState { value: "Hello, world!".into() });
/// # }
/// ```
/// 
/// For using `&str` instead of `String` the [`free_lifetimes!`](free_lifetimes) macro can be used:
/// ```rust
/// # mod call_back {
/// #     use dyn_context::State;
/// # 
/// #     pub struct CallBack {
/// #         callback: Option<fn(state: &mut dyn State)>
/// #     }
/// # 
/// #     impl CallBack {
/// #         pub fn new() -> Self { CallBack { callback: None } }
/// # 
/// #         pub fn set_callback(&mut self, callback: fn(state: &mut dyn State)) {
/// #             self.callback.replace(callback);
/// #         }
/// # 
/// #         pub fn call_back(&self, state: &mut dyn State) {
/// #             self.callback.map(|callback| callback(state));
/// #         }
/// #     }
/// # }
/// # 
/// use dyn_context::{free_lifetimes, SelfState, StateExt};
/// use call_back::CallBack;
///
/// free_lifetimes! {
///     struct PrintState {
///         value: 'value ref str
///     }
/// }
///
/// impl SelfState for PrintState { }
///
/// # fn main() {
/// let mut call_back = CallBack::new();
/// call_back.set_callback(|state| {
///     let print: &PrintState = state.get();
///     println!("{}", print.value());
/// });
/// PrintStateBuilder {
///     value: "Hello, world!"
/// }.build_and_then(|state| call_back.call_back(state));
/// # }
/// ```
pub trait State: 'static {
    /// Borrows shareable data entry.
    ///
    /// Prefer high-level [`get`](StateExt::get) wrap.
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any>;

    /// Borrows mutable data entry.
    ///
    /// Prefer high-level [`get_mut`](StateExt::get_mut) wrap.
    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any>;
}

#[cfg(feature="nightly")]
impl State for ! {
    fn get_raw(&self, _ty: TypeId) -> Option<&dyn Any> { Some(self) }
    fn get_mut_raw(&mut self, _ty: TypeId) -> Option<&mut dyn Any> { Some(self) }
}

impl State for () {
    fn get_raw(&self, _ty: TypeId) -> Option<&dyn Any> { None }
    fn get_mut_raw(&mut self, _ty: TypeId) -> Option<&mut dyn Any> { None }
}

/// Marks implementor as a trivial [`State`](trait@State).
/// A trivial-implemented state is a state containing itself only.
///
/// # Examples
///
/// ```rust
/// # use dyn_context::{SelfState, State, StateExt};
/// #
/// struct SomeData {
///     data: u16,
/// }
///
/// impl SelfState for SomeData { }
///
/// fn get_data_from_state(state: &dyn State) -> u16 {
///     let some_data: &SomeData = state.get();
///     some_data.data
/// }
///
/// # fn main() {
/// let some_data = SomeData { data: 7 };
/// let data_from_state = get_data_from_state(&some_data);
/// assert_eq!(data_from_state, 7);
/// # }
pub trait SelfState: 'static { }

impl<T: SelfState> State for T {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any> {
        if ty == TypeId::of::<T>() {
            Some(self)
        } else {
            None
        }
    }

    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any> {
        if ty == TypeId::of::<T>() {
            Some(self)
        } else {
            None
        }
    }
}

/// Extends [`State`](trait@State) with methods that make it easier to access the content of the state.
pub trait StateExt: State {
    /// Borrows shareable data reference.
    ///
    /// Panics if the state does not provide requested type.
    fn get<T: 'static>(&self) -> &T {
        self.get_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_ref::<T>().expect("invalid cast")
    }

    /// Borrows mutable data reference.
    ///
    /// Panics if the state does not provide requested type.
    fn get_mut<T: 'static>(&mut self) -> &mut T {
        self.get_mut_raw(TypeId::of::<T>())
            .unwrap_or_else(|| panic!("{} required", type_name::<T>()))
            .downcast_mut::<T>().expect("invalid cast")
    }

}

impl<T: State + ?Sized> StateExt for T { }

#[cfg(feature="nightly")]
#[thread_local]
static APP: RefCell<Option<NonNull<dyn State>>> = RefCell::new(None);

/// Allows safely store state in thread-local storage.
#[cfg(feature="nightly")]
pub struct App(());

#[cfg(feature="nightly")]
impl App {
    /// Store the state into a thread-local storage and call the provided function.
    /// During the function execution the state is accessible through the [`App::with`] method.
    pub fn set_and_then<T>(f: impl FnOnce() -> T, state: &mut dyn State) -> T {
        let _app = App::new(state);
        f()
    }

    /// Get state from a thread-local storage. If this method is call outside of
    /// [`App::set_and_then`] execution context, state is not accessible, and
    /// the method panics.
    pub fn borrow_and_then<T>(f: impl FnOnce(AppMut) -> T) -> T {
        let ref_mut = APP.borrow_mut();
        f(AppMut(ref_mut))
    }

    fn new(state: &mut dyn State) -> App {
        APP.borrow_mut().replace(unsafe { NonNull::new_unchecked(state as *mut _) });
        App(())
    }
}

#[cfg(feature="nightly")]
impl Drop for App {
    fn drop(&mut self) {
        APP.borrow_mut().take();
    }
}

#[cfg(feature="nightly")]
pub struct AppMut<'a>(cell::RefMut<'a, Option<NonNull<dyn State>>>);

impl<'a> Deref for AppMut<'a> {
    type Target = dyn State;

    fn deref(&self) -> &dyn State {
        unsafe { (&self.0).expect("App required").as_ref() }
    }
}

impl<'a> DerefMut for AppMut<'a> {
    fn deref_mut(&mut self) -> &mut dyn State {
        unsafe { (&self.0).expect("App required").as_mut() }
    }
}

free_lifetimes! {
    struct StateSum {
        a: 'a ref dyn State,
        b: 'b ref dyn State,
    }
}

impl State for StateSum {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any> {
        if let Some(r) = self.a().get_raw(ty) {
            Some(r)
        } else if let Some(r) = self.b().get_raw(ty) {
            Some(r)
        } else {
            None
        }
    }

    fn get_mut_raw(&mut self, _ty: TypeId) -> Option<&mut dyn Any> {
        unreachable!()
    }
}

free_lifetimes! {
    struct StateSumMut {
        a: 'a mut dyn State,
        b: 'b mut dyn State,
    }
}

impl State for StateSumMut {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any> {
        if let Some(r) = self.a().get_raw(ty) {
            Some(r)
        } else if let Some(r) = self.b().get_raw(ty) {
            Some(r)
        } else {
            None
        }
    }

    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any> {
        let r = if let Some(r) = self.a_mut().get_mut_raw(ty) {
            Some(r as *mut _)
        } else if let Some(r) = self.b_mut().get_mut_raw(ty) {
            Some(r as *mut _)
        } else {
            None
        };
        r.map(|x| unsafe { &mut *x })
    }
}

/// Provides method allowing combine two read-only [`State`](trait@State)s into one.
pub trait StateRef {
    /// Merges two states into one and calls provided function with the combined state.
    fn merge_and_then<T>(self, f: impl FnOnce(&dyn State) -> T, other: &dyn State) -> T;
}

impl<C: State> StateRef for &C {
    fn merge_and_then<T>(self, f: impl FnOnce(&dyn State) -> T, other: &dyn State) -> T {
        StateSumBuilder {
            a: self,
            b: other,
        }.build_and_then(|x| f(x))
    }
}

impl StateRef for &dyn State {
    fn merge_and_then<T>(self, f: impl FnOnce(&dyn State) -> T, other: &dyn State) -> T {
        StateSumBuilder {
            a: self,
            b: other,
        }.build_and_then(|x| f(x))
    }
}

/// Provides method allowing combine two [`State`](trait@State)s into one.
pub trait StateRefMut {
    /// Merges two states into one and calls provided function with the combined state.
    fn merge_mut_and_then<T>(self, f: impl FnOnce(&mut dyn State) -> T, other: &mut dyn State) -> T;
}

impl<C: State> StateRefMut for &mut C {
    fn merge_mut_and_then<T>(self, f: impl FnOnce(&mut dyn State) -> T, other: &mut dyn State) -> T {
        StateSumMutBuilder {
            a: self,
            b: other,
        }.build_and_then(|x| f(x))
    }
}

impl StateRefMut for &mut dyn State {
    fn merge_mut_and_then<T>(self, f: impl FnOnce(&mut dyn State) -> T, other: &mut dyn State) -> T {
        StateSumMutBuilder {
            a: self,
            b: other,
        }.build_and_then(|x| f(x))
    }
}

/// Creates structure, allowing to pack several references into
/// a one reference to a `'static` type.
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
        $vis:vis struct $name:ident $($body:tt)*
    ) => {
        $crate::generics_parse! {
            $crate::free_lifetimes_impl {
                @struct [$([$attr])*] [$vis] [$name]
            }
            $($body)*
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
        $crate::std_compile_error!("\
            invalid free lifetimes struct definition, allowed form is\n\
            \n\
            $(#[attr])* $vis struct $name {\n\
                $field_1_name: $('field_1_lt ref | 'field_1_lt mut | const) $field_1_type,\n\
                $field_2_name: $('field_2_lt ref | 'field_2_lt mut | const) $field_2_type,\n\
                ...\n\
            }\n\
            \n\
        ");
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
                $field : *const $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $builder . $field as *const $field_ty,
            ]
            [
                $($struct_methods)*
                $vis fn $field (&self) -> &$field_ty { unsafe { &*self.$field } }
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
                $field : *mut $field_ty,
            ]
            [
                $($ctor_assignments)*
                $field : $builder . $field as *mut $field_ty,
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
            "', allowed form is '$name: $('lt ref | 'lt mut | const) $type'",
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
                    f: impl $crate::std_ops_FnOnce(&mut $name) -> FreeLifetimesStructBuildReturnType 
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

            unsafe impl $($g)* Send for $name $($r)* $($w)* { }
            unsafe impl $($g)* Sync for $name $($r)* $($w)* { }
        }
    };
}

#[cfg(docsrs)]
pub mod example {
    //! [`free_lifetimes`](free_lifetimes) macro expansion example.
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
    use crate::{App, SelfState, StateExt, StateRefMut};
    use core::mem::replace;

    free_lifetimes! {
        struct State1 {
            a: const u8,
            b: 'b ref u16,
            c: 'c mut u32,
        }
    }
    
    impl SelfState for State1 { }

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

    #[derive(Debug, Clone, Copy)]
    struct PrivStr;

    impl SelfState for PrivStr { }

    #[test]
    fn test_state_4() {
        let mut x = 3;
        let res = State1Builder {
            a: 1,
            b: &2,
            c: &mut x
        }.build_and_then(|state| {
            state.merge_mut_and_then(|state| {
                assert_eq!(state.get::<State1>().a(), 1u8);
                assert_eq!(state.get::<State1>().b(), &2u16);
                assert_eq!(replace(state.get_mut::<State1>().c_mut(), 12), 3u32);
                "res"
            }, &mut PrivStr)
        });
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    #[test]
    fn app() {
        let mut x = 3;
        let res = State1Builder {
            a: 1,
            b: &2,
            c: &mut x
        }.build_and_then(|state| App::set_and_then(|| {
            App::borrow_and_then(|mut state| {
                let state: &mut State1 = state.get_mut();
                assert_eq!(state.a(), 1u8);
                assert_eq!(state.b(), &2u16);
                assert_eq!(replace(state.c_mut(), 12), 3u32);
                "res"
            })
        }, state));
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }
}
