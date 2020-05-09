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
    MissingPrefixExpr,
    MissingSemi,
    MissingVarName,
    MissingClassName,
    MissingPropertyName,
    MissingMethodName,
    InvalidThis,
    ReturnFromInit,
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
        use CompileErrorType::*;

        match self {
            UnexpectedChar(c) => write!(f, "unexpected character '{}'", c),
            UnterminatedString => write!(f, "unterminated string"),
            MissingLeftParen(location) => write!(f, "expected '(' {}", location),
            MissingRightParen(location) => write!(f, "expected ')' {}", location),
            MissingLeftBrace(location) => write!(f, "expected '{{' {}", location),
            MissingRightBrace(location) => write!(f, "expected '}}' {}", location),
            MissingPrefixExpr => write!(f, "expected expression"),
            MissingSemi => write!(f, "expected ';' after statement"),
            MissingVarName => write!(f, "expect variable name"),
            MissingClassName => write!(f, "expect class name"),
            MissingPropertyName => write!(f, "expect property name after '.'"),
            MissingMethodName => write!(f, "expect method name"),
            InvalidThis => write!(f, "cannot use 'this' outside of a class"),
            ReturnFromInit => write!(f, "cannot return a value from 'init' method"),
            MissingFunName => write!(f, "expect function name"),
            MissingParamName => write!(f, "expect parameter name"),
            TooManyParams => write!(f, "cannot exceed 255 parameters"),
            DuplicateLocal(who) => write!(f, "variable `{}` already declared in this scope", who),
            ReadBeforeDefined(who) => {
                write!(f, "tried to use variable `{}` in its own initializer", who)
            }
            JumpTooLarge(distance) => write!(
                f,
                "jump distance {:x} is too large to fit in a `u16` (max value {:x})",
                distance,
                u16::MAX
            ),
            TopLevelReturn => write!(f, "return statement outside a function body"),
            ExpectedEOF => write!(f, "expected end of input"),
        }
    }
}
