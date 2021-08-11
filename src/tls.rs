use crate::state::{State, StateExt};
use core::cell::{self, RefCell};
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use educe::Educe;
use phantom_type::PhantomType;
use core::any::{TypeId, Any};

#[thread_local]
static APP: RefCell<Option<NonNull<dyn State>>> = RefCell::new(None);

struct ThreadLocalTls(Option<NonNull<dyn State>>);

impl ThreadLocalTls {
    fn new(state: &mut dyn State) -> ThreadLocalTls {
        ThreadLocalTls(APP.borrow_mut().replace(unsafe { NonNull::new_unchecked(state as *mut _) }))
    }
}

impl Drop for ThreadLocalTls {
    fn drop(&mut self) {
        *APP.borrow_mut() = self.0.take();
    }
}

/// Allows safely store state in thread-local storage.
pub struct Tls<'a>(&'a RefCell<Option<NonNull<dyn State>>>);

impl<'a> Tls<'a> {
    /// Store the state into a thread-local storage and call the provided function.
    /// During the function execution the state is accessible
    /// through the [`Tls::acquire_and_then`] method.
    pub fn set_and_then<T>(f: impl FnOnce() -> T, state: &mut dyn State) -> T {
        let _tla = ThreadLocalTls::new(state);
        f()
    }

    /// Get state from a thread-local storage. If this method is call outside of
    /// [`Tls::set_and_then`] execution context, state is not accessible, and
    /// the method panics.
    pub fn acquire_and_then<T>(f: impl FnOnce(&mut Tls) -> T) -> T {
        f(&mut Tls(&APP))
    }

    pub fn borrow_raw(&self, ty: TypeId) -> TlsRefRaw {
        TlsRefRaw { state: self.0.borrow(), ty }
    }

    pub fn borrow_mut_raw(&mut self, ty: TypeId) -> TlsMutRaw {
        TlsMutRaw { state: self.0.borrow_mut(), ty }
    }

    pub fn borrow<T: 'static>(&self) -> TlsRef<T> {
        TlsRef(self.0.borrow(), PhantomType::new())
    }

    pub fn borrow_mut<T: 'static>(&mut self) -> TlsMut<T> {
        TlsMut(self.0.borrow_mut(), PhantomType::new())
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TlsRefRaw<'a> {
    state: cell::Ref<'a, Option<NonNull<dyn State>>>,
    ty: TypeId,
}

impl<'a> Deref for TlsRefRaw<'a> {
    type Target = dyn Any;

    fn deref(&self) -> &dyn Any {
        unsafe {
            (&self.state)
                .expect("Tls required")
                .as_ref()
                .get_raw(self.ty)
                .unwrap_or_else(|| panic!("{:?} required", self.ty))
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TlsMutRaw<'a> {
    state: cell::RefMut<'a, Option<NonNull<dyn State>>>,
    ty: TypeId,
}

impl<'a> Deref for TlsMutRaw<'a> {
    type Target = dyn Any;

    fn deref(&self) -> &dyn Any {
        unsafe {
            (&self.state)
                .expect("Tls required")
                .as_ref()
                .get_raw(self.ty)
                .unwrap_or_else(|| panic!("{:?} required", self.ty))
        }
    }
}

impl<'a> DerefMut for TlsMutRaw<'a> {
    fn deref_mut(&mut self) -> &mut dyn Any {
        unsafe {
            (&mut self.state)
                .expect("Tls required")
                .as_mut()
                .get_mut_raw(self.ty)
                .unwrap_or_else(|| panic!("{:?} required", self.ty))
        }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TlsRef<'a, T: 'static>(cell::Ref<'a, Option<NonNull<dyn State>>>, PhantomType<T>);

impl<'a, T: 'static> Deref for TlsRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { (&self.0).expect("Tls required").as_ref().get() }
    }
}

#[derive(Educe)]
#[educe(Debug)]
pub struct TlsMut<'a, T: 'static>(cell::RefMut<'a, Option<NonNull<dyn State>>>, PhantomType<T>);

impl<'a, T: 'static> Deref for TlsMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { (&self.0).expect("Tls required").as_ref().get() }
    }
}

impl<'a, T> DerefMut for TlsMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { (&self.0).expect("Tls required").as_mut().get_mut() }
    }
}
