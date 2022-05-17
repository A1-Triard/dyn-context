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
pub use paste::paste as paste_paste;

pub mod state;

pub mod free_lifetimes;

pub use dyn_context_macro::State;
pub use dyn_context_macro::Stop;

#[cfg(test)]
mod test {
    use crate::{State, Stop, free_lifetimes, impl_stop_and_appropriate_drop};
    use crate::state::{SelfState, State, StateExt, StateRefMut};
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

    impl_stop_and_appropriate_drop!(for TestStop {
        fn is_stopped(&self) -> bool { self.stopped }

        fn stop(state: &mut dyn State) {
            state.get_mut::<TestStop>().stopped = true;
        }
    });

    #[derive(Stop)]
    struct N(TestStop);

    #[derive(Stop)]
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
