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
//! as well as some language extensions suggested as exercises to the reader.
//!
//! It is loosely based off of the book's third part, which is a guide to implementing an
//! interpreter in C. Unlike the C implementation in the book, this library is 100% safe Rust and
//! relies on `std` for its fundamental data structures.
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

use std::{error, fmt};

mod chunk;
mod compiler;
mod ops;
mod scanner;
mod token;
mod value;
mod vm;

pub use vm::VM;

use {
    chunk::Chunk,
    compiler::{CompileErrorType, Compiler},
    ops::Op,
    scanner::{ScanError, Scanner},
    token::{Token, TokenType},
    value::Value,
    vm::RuntimeError,
};

impl error::Error for dyn Error {}

/// A common interface for errors that occur in the Lox runtime.
///
/// All errors are upcast to a trait object. More specific information about the particular error
/// can be obtained from its `Display` implementation.
pub trait Error: fmt::Debug + fmt::Display {
    fn category(&self) -> ErrorCategory;
}

/// Where in the pipeline an error occurred.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ErrorCategory {
    Compilation,
    Runtime,
}
