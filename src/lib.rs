#![deny(clippy::all)]
#![warn(clippy::pedantic)]

//! An implementation of the Lox programming language.
//!
//! ## About Lox
//!
//! Lox is a dynamically typed, interpreted scripting language. It was designed by Bob Nystrom for
//! his book [Crafting Interpreters](http://craftinginterpreters.com).
//!
//! ## About this implementation
//!
//! This library aims to implement Lox faithfully to the quasi-specification laid out in the book,
//! as well as some language extensions suggested as exercises to the reader. It is loosely based
//! off of the book's third part, which is a guide to implementing an interpreter in C.
//!
//! ## Usage
//!
//! Included in this package (and installable via `cargo install lox_lang`) is a small wrapper
//! executable named `loxi`. It is the simplest route to trying out the Lox language.
//!
//! If you want to embed Lox in a larger Rust project, you will need to create an instance of
//! [`VM`](struct.VM.html) and use its [`interpret`](struct.VM.html#method.interpret) method to run
//! your code:
//!
//! ```
//! let mut my_vm = lox_lang::VM::default();
//! my_vm.interpret(r#" print "hello " + "world"; "#).unwrap();
//! ```

use std::{convert::From, error, fmt};

mod chunk;
mod compiler;
mod ops;
mod scanner;
mod token;
mod value;
mod vm;

pub use value::{NativeFun, Value};
pub use vm::VM;

use {
    chunk::Chunk,
    compiler::{CompileError, CompileErrorType, Compiler},
    ops::Op,
    scanner::{ScanError, Scanner},
    token::{Token, TokenType},
    value::Fun,
    vm::RuntimeError,
};

/// Compilation and runtime errors in the Lox VM.
#[derive(Debug)]
pub struct Error {
    inner: Box<dyn error::Error>,
    category: ErrorCategory,
    line: usize,
}

impl Error {
    /// Which type of error was encountered
    pub fn category(&self) -> ErrorCategory {
        self.category
    }

    /// The line in the Lox source code where the error occurred
    pub fn line(&self) -> usize {
        self.line
    }

    fn from_runtime_error(err: RuntimeError, line: Option<usize>) -> Self {
        Self {
            inner: Box::new(err),
            category: ErrorCategory::Runtime,
            line: line.unwrap_or(1),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&*self.inner)
    }
}

/// Where in the pipeline an error occurred.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub enum ErrorCategory {
    Compilation,
    Runtime,
}

impl From<CompileError> for Error {
    fn from(inner: CompileError) -> Self {
        Self {
            line: inner.line,
            inner: Box::new(inner),
            category: ErrorCategory::Compilation,
        }
    }
}
