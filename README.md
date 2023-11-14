![maintenance: experimental](https://img.shields.io/badge/maintenance-experimental-blue.svg)

# dyn-context

This crate provides simple mechanism for lifetimes erasing.

In Rust, lifetimes are intrusive, and sometimes it can lead to
an inadequately complex code. Moreover, in some cases it can lead to an _impossible code_,
means code so complex, so it can not make to compile, even it is logically meaningful.
(Such situations could occur because Rust does not support existential types
with infinite parameters list.)

The crate provides a way to "compress" several lifetimed references into a one reference
to a `'static` type. This mechanism guarantees type-safety.

This mechanics allows building complex systems with callbacks:

```rust
mod call_back {
    pub struct CallBack<State: ?Sized> {
        callback: Option<fn(state: &mut State)>
    }

    impl<State: ?Sized> CallBack<State> {
        pub fn new() -> Self { CallBack { callback: None } }

        pub fn set_callback(&mut self, callback: fn(state: &mut State)) {
            self.callback.replace(callback);
        }

        pub fn call_back(&self, state: &mut State) {
            self.callback.map(|callback| callback(state));
        }
    }
}

use call_back::CallBack;
use dyn_context::free_lifetimes;

free_lifetimes! {
    struct PrintState {
        value: 'value ref str
    }
}

fn main() {
    let mut call_back = CallBack::new();
    call_back.set_callback(|state: &mut PrintState| {
        println!("{}", state.value());
    });
    PrintStateBuilder {
        value: "Hello, world!"
    }.build_and_then(|state| 
        call_back.call_back(state)
    );
}
```
