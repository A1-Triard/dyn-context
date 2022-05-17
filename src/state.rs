use crate::free_lifetimes;
use core::any::{TypeId, Any, type_name};
use panicking::panicking;

/// A service provider pattern implementation = associated read-only container with type as a key.
///
/// Useful for building complex systems with callbacks without generic parameters.
///
/// # Examples
///
/// ```rust
/// mod call_back {
///     use dyn_context::state::State;
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
/// use dyn_context::state::{SelfState, StateExt};
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
/// For using `&str` instead of `String` the [`free_lifetimes!`](free_lifetimes!) macro can be used:
/// ```rust
/// # mod call_back {
/// #     use dyn_context::state::State;
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
/// use call_back::CallBack;
/// use dyn_context::free_lifetimes;
/// use dyn_context::state::{SelfState, StateExt};
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

pub trait Stop: Sized {
    fn is_stopped(&self) -> bool;

    fn stop(state: &mut dyn State);

    fn drop(&mut self) {
        if !self.is_stopped() && !panicking() {
            panic!("{} requires explicit stop function call before dropping", type_name::<Self>());
        }
    }
}

/// Marks implementor as a trivial [`State`](trait@State).
/// A trivial-implemented state is a state containing itself only.
///
/// # Examples
///
/// ```rust
/// # use dyn_context::state::{SelfState, State, StateExt};
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

free_lifetimes! {
    struct StateSum {
        a: 'a ref dyn State,
        b: 'b ref dyn State,
    }
}

impl State for StateSum {
    fn get_raw(&self, ty: TypeId) -> Option<&dyn Any> {
        self.a().get_raw(ty).or_else(|| self.b().get_raw(ty))
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
        self.a().get_raw(ty).or_else(|| self.b().get_raw(ty))
    }

    fn get_mut_raw(&mut self, ty: TypeId) -> Option<&mut dyn Any> {
        borrow_mut_either(
            self,
            |x| x.a_mut().get_mut_raw(ty),
            |x| x.b_mut().get_mut_raw(ty)
        )
    }
}

fn borrow_mut_either<T: ?Sized, R: ?Sized>(
    x: &mut T,
    a: impl FnOnce(&mut T) -> Option<&mut R>,
    b: impl FnOnce(&mut T) -> Option<&mut R>
) -> Option<&mut R> {
    let r = if let Some (r) = a(x) {
        r as *mut _
    } else if let Some(r) = b(x) {
        r as *mut _
    } else {
        return None;
    };
    Some(unsafe { &mut *r })
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

#[macro_export]
macro_rules! impl_stop_and_appropriate_drop {
    (
        $($token:tt)+
    ) => {
        $crate::generics_parse! {
            $crate::impl_stop_and_appropriate_drop_impl {
            }
            $($token)+
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_stop_and_appropriate_drop_impl {
    (
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        for $t:ty {
            $($body:tt)*
        }
    ) => {
        impl $($g)* $crate::state::Stop for $t $($w)* {
            $($body)*
        }

        impl $($g)* $crate::std_ops_Drop for $t $($w)* {
            fn drop(&mut self) {
                <$t as $crate::state::Stop>::drop(self);
            }
        }
    };
    (
        [$($g:tt)*] [$($r:tt)*] [$($w:tt)*]
        $($token:tt)*
    ) => {
        $crate::std_compile_error!("\
            invalid Stop trait implementation, allowed form is \
            '$(<$generics>)? for $t:ty $(where $where_clause)? { ... }'\
        ");
    };
}
