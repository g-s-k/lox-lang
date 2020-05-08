use std::{
    any::{self, Any},
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Debug)]
pub struct Gc<T: ?Sized>(*mut T, *mut bool);

impl<T: ?Sized> Gc<T> {
    pub(crate) fn new(value: Box<T>) -> Self {
        Self(Box::leak(value), Box::leak(Box::new(false)))
    }

    fn report_null(&self) -> ! {
        panic!(
            "Holding null reference to type {} at address {:p}.",
            any::type_name::<T>(),
            self.0
        );
    }
}

impl<T: ?Sized + fmt::Debug> Gc<T> {
    pub(crate) fn is_marked(&self) -> bool {
        if self.1.is_null() {
            panic!("Null reference to GC mark cell");
        } else {
            unsafe { *self.1 }
        }
    }

    pub(crate) fn mark(&self) {
        if self.1.is_null() {
            panic!("Null reference to GC mark cell");
        } else {
            #[cfg(feature = "trace-gc")]
            log::debug!("{:p} mark {:?}", self.0, self.deref());

            unsafe {
                *self.1 = true;
            };
        }
    }

    pub(crate) fn clear_mark(&self) {
        if self.1.is_null() {
            panic!("Null reference to GC mark cell");
        } else {
            unsafe {
                *self.1 = false;
            }
        }
    }

    pub(crate) fn free(self) {
        #[cfg(feature = "trace-gc")]
        log::debug!("{:p} free {:?}", self, self.deref());

        // drop both contained values
        unsafe {
            Box::from_raw(self.0);
            Box::from_raw(self.1);
        }
    }
}

impl Gc<dyn Any + 'static> {
    pub(crate) fn downcast<T: 'static>(&mut self) -> Option<Gc<T>> {
        if let Some(payload) = self.deref_mut().downcast_mut() {
            Some(Gc(&mut *payload, self.1))
        } else {
            None
        }
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        if self.0.is_null() {
            self.report_null()
        } else {
            unsafe { &(*self.0) }
        }
    }
}

impl<T: ?Sized> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if self.0.is_null() {
            self.report_null()
        } else {
            unsafe { &mut (*self.0) }
        }
    }
}

impl<T: Any> From<Gc<T>> for Gc<dyn Any> {
    fn from(Gc(item, mark): Gc<T>) -> Self {
        Self(item as *mut dyn Any, mark)
    }
}

impl<T: ?Sized> fmt::Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
