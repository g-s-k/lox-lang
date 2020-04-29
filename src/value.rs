use std::fmt;

use super::RuntimeError;

#[derive(Clone, PartialEq)]
pub(crate) enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    r#String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::r#String(s) => write!(f, "{}", s),
        }
    }
}

type OpResult = Result<Value, RuntimeError>;

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Boolean(false) => true,
            _ => false,
        }
    }

    pub fn negate(self) -> OpResult {
        if let Self::Number(a) = self {
            Ok(Self::Number(-a))
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }
}
