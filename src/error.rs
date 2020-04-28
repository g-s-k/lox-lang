use std::fmt;

pub enum ErrorCategory {
    Compilation,
    Runtime,
}

#[derive(Clone, Copy, Debug)]
pub enum Error {
    // compilation
    UnexpectedChar,
    UnterminatedString,
    MissingRightParen,
    MissingPrefixExpr,
    ExpectedEOF,

    // runtime
    ArgumentTypes,
    StackEmpty,
}

impl Error {
    pub fn category(self) -> ErrorCategory {
        match self {
            Self::UnexpectedChar
            | Self::UnterminatedString
            | Self::ExpectedEOF
            | Self::MissingRightParen
            | Self::MissingPrefixExpr => ErrorCategory::Compilation,
            Self::ArgumentTypes | Self::StackEmpty => ErrorCategory::Runtime,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar => write!(f, "unexpected character"),
            Self::UnterminatedString => write!(f, "unterminated string"),
            Self::MissingRightParen => write!(f, "expected ')'"),
            Self::MissingPrefixExpr => write!(f, "expected expression"),
            Self::ExpectedEOF => write!(f, "expected end of input"),
            Self::ArgumentTypes => write!(f, "incompatible types for operation"),
            Self::StackEmpty => write!(f, "tried to pop value from empty stack"),
        }
    }
}

impl std::error::Error for Error {}
