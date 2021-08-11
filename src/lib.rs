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

pub mod state;

pub mod free_lifetimes;

#[cfg(feature="nightly")]
pub mod tls;

#[cfg(test)]
mod test {
    use crate::tls::Tls;
    use crate::free_lifetimes;
    use crate::state::{SelfState, StateExt, StateRefMut};
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
    fn tls() {
        let mut x = 3;
        let res = State1Builder {
            a: 1,
            b: &2,
            c: &mut x
        }.build_and_then(|state| Tls::set_and_then(|| {
            Tls::acquire_and_then(|tls| {
                let state: &mut State1 = &mut tls.borrow_mut();
                assert_eq!(state.a(), 1u8);
                assert_eq!(state.b(), &2u16);
                assert_eq!(replace(state.c_mut(), 12), 3u32);
                "res"
            })
        }, state));
        assert_eq!(res, "res");
        assert_eq!(x, 12);
    }

    struct Value(u32);

    impl SelfState for Value { }

    struct Increment;

    impl Increment {
        fn new(tls: &mut Tls) -> Increment {
            let value: &mut Value = &mut tls.borrow_mut();
            value.0 += 1;
            Increment
        }
    }

    impl Drop for Increment {
        fn drop(&mut self) {
            Tls::acquire_and_then(|tls| {
                let value: &mut Value = &mut tls.borrow_mut();
                value.0 -= 1;
            });
        }
    }

    #[test]
    fn increment() {
        let mut value = Value(0);
        Tls::set_and_then(|| Tls::acquire_and_then(|tls| {
            {
                let value: &mut Value = &mut tls.borrow_mut();
                assert_eq!(value.0, 0);
            }
            let _increment = Increment::new(tls);
            {
                let value: &mut Value = &mut tls.borrow_mut();
                assert_eq!(value.0, 1);
            }
            {
                let _another_increment = Increment::new(tls);
                {
                    let value: &mut Value = &mut tls.borrow_mut();
                    assert_eq!(value.0, 2);
                }
            }
            {
                let value: &mut Value = &mut tls.borrow_mut();
                assert_eq!(value.0, 1);
            }
        }), &mut value);
        assert_eq!(value.0, 0);
    }
}
