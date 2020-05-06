use std::{error, fmt};

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
            Self::NativeFunError(inner) => {
                write!(f, "native function returned an error: {}", inner)
            }
        }
    }
}
