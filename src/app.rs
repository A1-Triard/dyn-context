use crate::state::{State, StateExt};
use core::cell::{self, RefCell};
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use educe::Educe;
use phantom_type::PhantomType;

#[thread_local]
static APP: RefCell<Option<NonNull<dyn State>>> = RefCell::new(None);

struct ThreadLocalApp(Option<NonNull<dyn State>>);

impl ThreadLocalApp {
    fn new(state: &mut dyn State) -> ThreadLocalApp {
        ThreadLocalApp(APP.borrow_mut().replace(unsafe { NonNull::new_unchecked(state as *mut _) }))
    }
}

impl Drop for ThreadLocalApp {
    fn drop(&mut self) {
        *APP.borrow_mut() = self.0.take();
    }
}

/// Allows safely store state in thread-local storage.
pub struct App<'a>(&'a RefCell<Option<NonNull<dyn State>>>);

impl<'a> App<'a> {
    /// Store the state into a thread-local storage and call the provided function.
    /// During the function execution the state is accessible through the [`App::with`] method.
    pub fn set_and_then<T>(f: impl FnOnce() -> T, state: &mut dyn State) -> T {
        let _tla = ThreadLocalApp::new(state);
        f()
    }

    /// Get state from a thread-local storage. If this method is call outside of
    /// [`App::set_and_then`] execution context, state is not accessible, and
    /// the method panics.
    pub fn acquire_and_then<T>(f: impl FnOnce(&mut App) -> T) -> T {
        f(&mut App(&APP))
    }

    pub fn borrow<T: 'static>(&self) -> AppRef<T> {
        AppRef(self.0.borrow(), PhantomType::new())
    }

    pub fn borrow_mut<T: 'static>(&mut self) -> AppMut<T> {
        AppMut(self.0.borrow_mut(), PhantomType::new())
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct AppRef<'a, T: 'static>(cell::Ref<'a, Option<NonNull<dyn State>>>, PhantomType<T>);

impl<'a, T: 'static> Deref for AppRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { (&self.0).expect("App required").as_ref().get() }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct AppMut<'a, T: 'static>(cell::RefMut<'a, Option<NonNull<dyn State>>>, PhantomType<T>);

impl<'a, T: 'static> Deref for AppMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { (&self.0).expect("App required").as_ref().get() }
    }
}

impl<'a, T> DerefMut for AppMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { (&self.0).expect("App required").as_mut().get_mut() }
    }
}
