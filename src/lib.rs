//! **Crate features**
//!
//! * `"nightly"`
//! Enabled by default. Disable to make the library compatible with stable and beta Rust channels.

#![cfg_attr(feature="nightly", feature(never_type))]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![no_std]

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
pub use core::ops::Drop as std_ops_Drop;
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
pub use indoc::indoc as indoc_indoc;
#[doc(hidden)]
pub use paste::paste as paste_paste;

mod state;
pub use state::*;

mod free_lifetimes;
pub use free_lifetimes::*;

/// Derives [`State`] implementation for structs.
///
/// This derive macro allows easily combine
/// new `State` from several parts and inner `States`.
///
/// Supports `#[state]` attribute, which can be in any
/// of two forms: `#[state]`, and `#[state(part)]`.
/// First form, `#[state]`, should be used to include
/// fields which type have implemented [`State`] trait as
/// an inner entry of building state. Second form, `#[state(part)]`,
/// marks field as well as struct itself as a building state part,
/// i.e. an object accessible by its type through [`State`] / [`StateExt`] methods.
///
/// # Example
///
/// ```rust
/// # use dyn_context::{SelfState, State, StateExt};
/// #
/// struct StatePart(i32);
///
/// struct InnerState1(i32);
///
/// impl SelfState for InnerState1 { }
///
/// struct InnerInnerState(i32);
///
/// impl SelfState for InnerInnerState { }
///
/// #[derive(State)]
/// #[state(part)]
/// struct InnerState2 {
///     var: i32,
///     #[state(part)]
///     part: StatePart,
///     #[state]
///     inner_inner: InnerInnerState,
/// }
///
/// #[derive(State)]
/// struct RootState {
///     #[state]
///     inner_1: InnerState1,
///     #[state]
///     inner_2: InnerState2,
/// }
///
/// # fn main() {
/// let part = StatePart(1);
/// let inner_1 = InnerState1(2);
/// let inner_inner = InnerInnerState(3);
/// let inner_2 = InnerState2 { var: 4, part, inner_inner };
/// let state = RootState { inner_1, inner_2 };
///
/// assert_eq!(state.get::<StatePart>().0, 1);
/// assert_eq!(state.get::<InnerState1>().0, 2);
/// assert_eq!(state.get::<InnerInnerState>().0, 3);
/// assert_eq!(state.get::<InnerState2>().var, 4);
/// # }
/// ```
pub use dyn_context_macro::State;

/// Derives [`Stop`] implementation for structs.
///
/// Supports `#[stop]` attribute, which can be in any
/// of following forms:
/// 
/// - `#[stop(explicit)]`
/// - `#[stop(implicit)]`
/// - `#[stop]`
/// - `#[stop(ignore)`
///
/// This macro can have two modes: implicit and explicit.
/// In the explicit mode all struct fields not marked with
/// `#[stop]` attribute are ignored. In the implicit mode
/// the behavior is the opposite: the implementation uses
/// all fields not marked with `#[stop(ignore)]`.
///
/// The mode can be selected by adding to the struct
/// `#[stop(explicit)]`, or
/// `#[stop(implicit)]` attribute respectively. By default,
/// mode is implicit for tuple structures (`struct Struct(...)`),
/// and explicit for ordinary structures (`struct Struct { ... }`).
pub use dyn_context_macro::Stop;

#[cfg(test)]
mod test {
    use crate::{SelfState, State, StateExt, StateRefMut};
    use crate::{Stop, free_lifetimes, impl_stop_and_drop};
    use core::mem::replace;
    use core::ops::Deref;

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

    struct TestStop {
        stopped: bool
    }

    impl SelfState for TestStop { }

    impl_stop_and_drop!(for TestStop {
        fn is_stopped(&self) -> bool { self.stopped }

        fn stop(state: &mut dyn State) {
            state.get_mut::<TestStop>().stopped = true;
        }
    });

    #[derive(Stop)]
    #[_crate]
    struct N(TestStop);

    #[derive(Stop)]
    #[_crate]
    #[stop(explicit)]
    struct X {
        #[stop]
        s: TestStop,
        #[allow(dead_code)]
        i: i32
    }

    struct A;
    impl SelfState for A { }
    struct B;
    impl SelfState for B { }

    #[derive(State)]
    #[_crate]
    #[state(part)]
    struct TestDerive {
        #[state]
        a: A,
        #[state]
        b: B,
        #[state(part)]
        c: i32,
    }
}
