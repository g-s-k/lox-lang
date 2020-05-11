use std::{
    any::{self, Any},
    cell::Cell,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
struct ObjBox<T: ?Sized> {
    mark: Cell<bool>,
    value: T,
}

#[derive(Clone, Debug)]
pub struct Gc<T: ?Sized>(*mut ObjBox<T>);

impl<T> Gc<T> {
    pub(crate) fn new(value: T) -> Self {
        Self(Box::into_raw(Box::new(ObjBox {
            mark: Cell::new(false),
            value,
        })))
    }
}

impl<T: ?Sized> Gc<T> {
    fn report_null(&self) -> ! {
        panic!(
            "Holding null reference to type {} at address {:p}.",
            any::type_name::<T>(),
            self.0
        );
    }

    fn deref_non_null(&self) -> &ObjBox<T> {
        if self.0.is_null() {
            self.report_null();
        } else {
            unsafe { &*self.0 }
        }
    }
}

impl<T: ?Sized + fmt::Debug> Gc<T> {
    pub(crate) fn is_marked(&self) -> bool {
        self.deref_non_null().mark.get()
    }

    pub(crate) fn mark(&self) {
        #[cfg(feature = "trace-gc")]
        log::debug!("{:p} mark {:?}", self.0, self.deref());

        self.deref_non_null().mark.set(true);
    }

    pub(crate) fn clear_mark(&self) {
        self.deref_non_null().mark.set(false);
    }

    pub(crate) fn free(self) {
        #[cfg(feature = "trace-gc")]
        log::debug!("{:p} free {:?}", self.0, self.deref());

        unsafe {
            // drop inner wrapper, and thus the value it owns
            Box::from_raw(self.0);
        }
    }
}

impl<T: Any> Gc<T> {
    pub(crate) fn as_any(&self) -> Gc<dyn Any> {
        Gc(self.0 as *mut ObjBox<dyn Any>)
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.deref_non_null().value
    }
}

impl<T: ?Sized> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if self.0.is_null() {
            self.report_null();
        } else {
            &mut unsafe { &mut (*self.0) }.value
        }
    }
}

impl<T: ?Sized> fmt::Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
