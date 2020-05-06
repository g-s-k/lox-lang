use super::{Fun, List, Value};

#[derive(Debug, Default)]
pub(crate) struct GC {
    objects: Option<Box<List<Val>>>,
}

macro_rules! alloc_impl {
    ($( $name: ident : $typ: ty => $variant: ident , )*) => {
        impl GC {
            $(
                pub(crate) fn $name(&mut self, obj: $typ) -> *mut $typ {
                    let val_ptr = List::push(&mut self.objects, Val::$variant(Box::new(obj)));

                    if let Val::$variant(ref mut o) = unsafe { &mut *val_ptr } {
                        &mut **o
                    } else {
                        unreachable!(
                            concat!(
                                "Just pushed object of type ",
                                stringify!(Val::$variant),
                                ". It should be on top of the object stack."
                            )
                        );
                    }
                }
             )*
        }
    };
}

alloc_impl!(
    alloc_fun : Fun => Fun,
    alloc_upvalue : UpvalueType => Upvalue,
    alloc_value : Value => Value,
);

#[derive(Debug)]
enum Val {
    Fun(Box<Fun>),
    Upvalue(Box<UpvalueType>),
    Value(Box<Value>),
}

#[derive(Clone, Copy, Debug)]
pub enum UpvalueType {
    Live(usize),
    Captured(*mut Value),
}

impl UpvalueType {
    pub(crate) fn close(&mut self, ptr: *mut Value) {
        *self = Self::Captured(ptr);
    }
}
