use std::{error, fmt};

#[derive(Debug)]
pub(crate) struct CompileError {
    pub err: CompileErrorType,
    pub line: usize,
    pub text: Option<String>,
}

impl error::Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error", self.line)?;
        if let Some(t) = &self.text {
            write!(f, " at \"{}\"", t)?;
        }
        write!(f, ": {}", self.err)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum CompileErrorType {
    // compilation
    UnexpectedChar(char),
    UnterminatedString,
    MissingLeftParen(&'static str),
    MissingRightParen(&'static str),
    MissingLeftBrace(&'static str),
    MissingRightBrace(&'static str),
    MissingDot(&'static str),
    MissingPrefixExpr,
    MissingSemi,
    MissingVarName,
    MissingClassName,
    MissingPropertyName,
    MissingMethodName,
    InvalidThis,
    InvalidSuper(&'static str),
    ReturnFromInit,
    MissingSuperclass,
    InheritFromSelf,
    MissingFunName,
    MissingParamName,
    TooManyParams,
    DuplicateLocal(String),
    ReadBeforeDefined(String),
    JumpTooLarge(usize),
    TopLevelReturn,
    ExpectedEOF,
}

impl fmt::Display for CompileErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "unexpected character '{}'", c),
            Self::UnterminatedString => write!(f, "unterminated string"),
            Self::MissingLeftParen(location) => write!(f, "expected '(' {}", location),
            Self::MissingRightParen(location) => write!(f, "expected ')' {}", location),
            Self::MissingLeftBrace(location) => write!(f, "expected '{{' {}", location),
            Self::MissingRightBrace(location) => write!(f, "expected '}}' {}", location),
            Self::MissingDot(location) => write!(f, "expected '.' {}", location),
            Self::MissingPrefixExpr => write!(f, "expected expression"),
            Self::MissingSemi => write!(f, "expected ';' after statement"),
            Self::MissingVarName => write!(f, "expect variable name"),
            Self::MissingClassName => write!(f, "expect class name"),
            Self::MissingPropertyName => write!(f, "expect property name after '.'"),
            Self::MissingMethodName => write!(f, "expect method name"),
            Self::InvalidThis => write!(f, "cannot use 'this' outside of a class"),
            Self::InvalidSuper(location) => write!(f, "cannot use 'super' {}", location),
            Self::ReturnFromInit => write!(f, "cannot return a value from 'init' method"),
            Self::MissingSuperclass => write!(f, "expect superclass name after '<'"),
            Self::InheritFromSelf => write!(f, "a class cannot inherit from itself"),
            Self::MissingFunName => write!(f, "expect function name"),
            Self::MissingParamName => write!(f, "expect parameter name"),
            Self::TooManyParams => write!(f, "cannot exceed 255 parameters"),
            Self::DuplicateLocal(who) => {
                write!(f, "variable `{}` already declared in this scope", who)
            }
            Self::ReadBeforeDefined(who) => {
                write!(f, "tried to use variable `{}` in its own initializer", who)
            }
            Self::JumpTooLarge(distance) => write!(
                f,
                "jump distance {:x} is too large to fit in a `u16` (max value {:x})",
                distance,
                std::u16::MAX
            ),
            Self::TopLevelReturn => write!(f, "return statement outside a function body"),
            Self::ExpectedEOF => write!(f, "expected end of input"),
        }
    }
}
