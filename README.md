# dyn-context

This crate provides simple mechanism for safely erasing of 1) lifetimes, 2) generics, and 3) function arguments.

1. Erasing lifetimes.

   In Rust, lifetimes are intrusive, and sometimes it can lead to
   an inadequately complex code. Moreover, in some cases it can lead to an _impossible code_,
   means code so complex, so it can not make to compiles, even it is logically meaningful.
   (Such situations could occur because Rust does not support existential types
   with infinite parameters list.)

   The crate provides a way to "compress" several lifetimed references into a one reference
   to a `'static` type. This mechanism guarantees type-safety.

2. Erasing generics.

   There are many reasons, why generics could be not a best choice in some concrete situation.
   This library provides `State` trait, which may be helpful in such cases.

3. Erasing function parameters.

   Although this should only be done as a last resort, it can be useful to pass state through
   thread-local static storage.

Combining all mechanics (lifetimes compression and dynamic context trait
passing through thread-local storage) allows building complex systems with callbacks:
```rust
mod call_back {
    pub struct CallBack {
        callback: Option<fn()>
    }

    impl CallBack {
        pub fn new() -> Self { CallBack { callback: None } }

        pub fn set_callback(&mut self, callback: fn()) {
            self.callback.replace(callback);
        }

        pub fn call_back(&self) {
            self.callback.map(|callback| callback());
        }
    }
}

use call_back::CallBack;
use dyn_context::free_lifetimes;
use dyn_context::state::SelfState;
use dyn_context::tls::Tls;

free_lifetimes! {
    struct PrintState {
        value: 'value ref str
    }
}

impl SelfState for PrintState { }

fn main() {
    let mut call_back = CallBack::new();
    call_back.set_callback(|| Tls::acquire_and_then(|tls| {
        let print: &PrintState = &tls.borrow();
        println!("{}", print.value());
    }));
    PrintStateBuilder {
        value: "Hello, world!"
    }.build_and_then(|state| Tls::set_and_then(|| {
       call_back.call_back();
    }, state));
}
```
