# dyn-context

This crate provides simple mechanism for lifetimes and generics safely erasing.

1. Erasing lifetimes.

   In Rust, lifetimes are intrusive, and sometimes it can lead to
   an inadequately complex code. Moreover, in some cases it can lead to an _impossible code_,
   means code so complex, so it can not make to compile, even it is logically meaningful.
   (Such situations could occur because Rust does not support existential types
   with infinite parameters list.)

   The crate provides a way to "compress" several lifetimed references into a one reference
   to a `'static` type. This mechanism guarantees type-safety.

2. Erasing generics.

   There are many reasons, why generics could be not a best choice in some concrete situation.
   This library provides `State` trait, which may be helpful in such cases.

Combining both mechanics (lifetimes compression and dynamic state trait)
allows building complex systems with callbacks:
```rust
mod call_back {
    use dyn_context::State;

    pub struct CallBack {
        callback: Option<fn(state: &mut dyn State)>
    }

    impl CallBack {
        pub fn new() -> Self { CallBack { callback: None } }

        pub fn set_callback(&mut self, callback: fn(state: &mut dyn State)) {
            self.callback.replace(callback);
        }

        pub fn call_back(&self, state: &mut dyn State) {
            self.callback.map(|callback| callback(state));
        }
    }
}

use dyn_context::{free_lifetimes, State, StateExt};
use call_back::CallBack;

free_lifetimes! {
    struct PrintState {
        value: 'value ref str
    }
}

State!(() struct PrintState { .. });

fn main() {
    let mut call_back = CallBack::new();
    call_back.set_callback(|state| {
        let print: &PrintState = state.get();
        println!("{}", print.value());
    });
    PrintStateBuilder {
        value: "Hello, world!"
    }.build_and_then(|state| call_back.call_back(state));
}
```
