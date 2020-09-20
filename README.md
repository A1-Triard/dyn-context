![travis](https://travis-ci.org/A1-Triard/dyn-context.svg?branch=master)

# dyn-context

This crate provides simple mechanism for lifetimes and generics safely erasing.

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
   This library provides `Context` trait, which may be helpful in such cases.

Combining both mechanics (lifetimes compression and dynamic context trait)
allows to build complex systems with callbacks:
```rust
mod call_back {
    use dyn_context::Context;

    pub struct CallBack {
        callback: Option<fn(context: &mut dyn Context)>
    }

    impl CallBack {
        pub fn new() -> Self { CallBack { callback: None } }

        pub fn set_callback(&mut self, callback: fn(context: &mut dyn Context)) {
            self.callback.replace(callback);
        }

        pub fn call_back(&self, context: &mut dyn Context) {
            self.callback.map(|callback| callback(context));
        }
    }
}
context! {
    struct PrintValue {
        value: ref str
    }
}

context! {
    dyn struct PrintContext {
        value: ref PrintValue
    }
}

fn main() {
    let mut call_back = CallBack::new();
    call_back.set_callback(|context| {
        let print_value: &PrintValue = context.get();
        println!("{}", print_value.value());
    });
    PrintValue::call("Hello, world!", |print_value| {
        PrintContext::call(print_value, |context| call_back.call_back(context));
    });
}
```
