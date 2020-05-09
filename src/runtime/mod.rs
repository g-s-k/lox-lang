use std::{any::Any, error, fmt};

use crate::{Chunk, Fun, Gc, Value};

pub mod vm;

#[derive(Debug)]
pub(crate) enum RuntimeError {
    ArgumentTypes,
    StackEmpty,
    BadStackIndex(usize, usize),
    UndefinedGlobal(String),
    NotCallable,
    ArityMismatch(u8, u8),
    CallStackOverflow,
    ValueStackOverflow,
    UndefinedProperty(String),
    NativeFunError(Box<dyn error::Error>),
}

impl error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::NativeFunError(e) => Some(e.as_ref()),
            _ => None,
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ArgumentTypes => write!(f, "incompatible types for operation"),
            Self::StackEmpty => write!(f, "tried to pop value from empty stack"),
            Self::BadStackIndex(wanted, len) => write!(
                f,
                "tried to access value at index {} beyond end of stack (height {})",
                wanted, len
            ),
            Self::UndefinedGlobal(name) => {
                write!(f, "tried to access undefined variable `{}`", name)
            }
            Self::NotCallable => write!(f, "tried to call a non-callable value"),
            Self::ArityMismatch(expected, got) => {
                write!(f, "expected {} arguments but got {}", expected, got)
            }
            Self::CallStackOverflow => write!(f, "call stack overflowed"),
            Self::ValueStackOverflow => write!(f, "too many temporaries and locals on the stack"),
            Self::UndefinedProperty(name) => write!(
                f,
                "tried to access undefined property `{}` on instance",
                name
            ),
            Self::NativeFunError(inner) => {
                write!(f, "native function returned an error: {}", inner)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum UpvalueRef {
    Live(usize),
    Captured(Box<Value>),
}

impl UpvalueRef {
    pub(crate) fn new(slot: usize) -> Self {
        Self::Live(slot)
    }

    pub(crate) fn close(&mut self, ptr: Value) {
        *self = Self::Captured(Box::new(ptr));
    }
}

impl fmt::Display for UpvalueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Live(idx) => write!(f, "at stack index {}", idx),
            Self::Captured(ptr) => write!(f, "at address {:p}", ptr),
        }
    }
}

#[derive(Clone, Debug)]
struct CallFrame {
    func: Gc<Fun>,
    inst: usize,
    base: usize,
    upvalues: Box<[Gc<UpvalueRef>]>,
}

impl CallFrame {
    fn chunk(&self) -> &Chunk {
        &(*self.func).chunk
    }

    fn get_upvalue(&self, index: usize) -> Option<Gc<UpvalueRef>> {
        self.upvalues.get(index).cloned()
    }

    fn mark(&self, grays: &mut Vec<Gc<dyn Any>>) {
        self.func.mark();
        grays.push(self.func.clone().into());

        for u_val in self.upvalues.iter() {
            u_val.mark();
            grays.push(u_val.clone().into());
        }
    }
}
