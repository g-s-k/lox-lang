use std::{
    collections::HashMap,
    error, fmt,
    io::{self, Write},
};

mod chunk;
mod compiler;
mod scanner;
mod token;
mod value;

use {
    chunk::Chunk,
    compiler::{CompileErrorType, Compiler},
    scanner::{ScanError, Scanner},
    token::{Token, TokenType},
    value::Value,
};

impl error::Error for dyn Error {}

pub trait Error: fmt::Debug + fmt::Display {
    fn category(&self) -> ErrorCategory;
}

#[derive(Clone, Debug)]
pub enum ErrorCategory {
    Compilation,
    Runtime,
}

#[derive(Clone, Debug)]
pub enum RuntimeError {
    // runtime
    ArgumentTypes,
    StackEmpty,
    UndefinedGlobal(String),
}

impl Error for RuntimeError {
    fn category(&self) -> ErrorCategory {
        ErrorCategory::Runtime
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RuntimeError::*;

        match self {
            ArgumentTypes => write!(f, "incompatible types for operation"),
            StackEmpty => write!(f, "tried to pop value from empty stack"),
            UndefinedGlobal(name) => write!(f, "tried to access undefined variable `{}`", name),
        }
    }
}

/// The Lox virtual machine.
///
/// ### Example
///
/// ```
/// # use lox_lang::VM;
/// let mut vm = VM::default();
/// vm.interpret("3 + 3 == 6"); // true
/// ```
pub struct VM<'a> {
    stdout: Option<&'a mut dyn Write>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl<'a> Default for VM<'a> {
    /// The `VM` constructor.
    ///
    /// Program output defaults to [`Stdout`]. To customize
    /// this behavior, see the [`set_streams`] method.
    ///
    /// [`set_streams`]: #method.set_streams
    /// [`Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
    fn default() -> Self {
        Self {
            stdout: None,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }
}

impl<'a, 'b> VM<'a> {
    /// Compile and run Lox code from source, reporting and returning any errors encountered in the process.
    pub fn interpret<T: AsRef<str> + 'b>(
        &mut self,
        source: T,
    ) -> Result<(), Vec<Box<dyn Error + 'b>>> {
        let chunk = Compiler::compile(source.as_ref()).map_err(|errs| {
            errs.into_iter()
                .map(|err| Box::new(err) as Box<dyn Error>)
                .collect::<Vec<_>>()
        })?;

        #[cfg(feature = "trace-compilation")]
        log::debug!("\n{}", chunk);

        self.run(chunk).map_err(|err| {
            log::error!("{}", err);
            vec![Box::new(err) as Box<dyn Error>]
        })
    }

    fn run(&mut self, chunk: Chunk) -> Result<(), RuntimeError> {
        macro_rules! read_constant {
            ( $index:expr ) => {{
                chunk.read_constant(*$index as usize)
            }};
        }

        macro_rules! read_string {
            ( $index:expr ) => {{
                if let Value::r#String(s) = read_constant!($index) {
                    s
                } else {
                    unreachable!();
                }
            }};
        }

        macro_rules! define_global {
            ( $index:expr ) => {{
                let var_name = read_string!($index);
                let value = self.peek(0)?.clone();
                self.globals.insert(var_name.clone(), value);
                self.pop()?;
            }};
        }

        macro_rules! get_global {
            ($index: expr) => {{
                let var_name = read_string!($index);
                if let Some(value) = self.globals.get(var_name) {
                    self.stack.push(value.clone());
                } else {
                    return Err(RuntimeError::UndefinedGlobal(var_name.to_string()));
                }
            }};
        }

        macro_rules! set_global {
            ($index: expr) => {{
                let var_name = read_string!($index);
                let value = self.peek(0)?.clone();
                if self.globals.insert(var_name.clone(), value).is_none() {
                    self.globals.remove(var_name);
                    return Err(RuntimeError::UndefinedGlobal(var_name.to_string()));
                }
                self.pop()?;
            }};
        }

        macro_rules! binary_op_body {
            ( $op:tt, $variant:ident ) => {
                if let (Value::Number(a), Value::Number(b)) = self.pop_pair()? {
                    self.stack.push(Value::$variant(a $op b));
                }
            };
        }

        macro_rules! binary_op {
            ( $op:tt, $variant:ident ) => {{
                match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => (),
                    _ => return Err(RuntimeError::ArgumentTypes),
                }

                // should be infallible now
                binary_op_body!($op, $variant)
            }};
        }

        for (_index, instruction) in chunk.code.iter().enumerate() {
            {
                #![cfg(feature = "trace-execution")]
                // print stack before operation
                log::debug!(
                    "          {}",
                    self.stack
                        .iter()
                        .map(|v| format!("[ {} ]", v))
                        .collect::<String>()
                );

                // print operation and arguments
                log::debug!("{:1$x}", chunk, _index);
            }

            use Op::*;

            match instruction {
                Constant(index) => {
                    self.stack.push(read_constant!(index).clone());
                }
                ConstantLong(index) => {
                    self.stack.push(read_constant!(index).clone());
                }
                Nil => self.stack.push(Value::Nil),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Pop => {
                    self.pop()?;
                }
                PopN(count) => {
                    self.pop_many(*count as usize)?;
                }
                GetGlobal(index) => get_global!(index),
                GetGlobalLong(index) => get_global!(index),
                DefineGlobal(index) => define_global!(index),
                DefineGlobalLong(index) => define_global!(index),
                SetGlobal(index) => set_global!(index),
                SetGlobalLong(index) => set_global!(index),
                GetLocal(index) => self.stack.push(
                    self.stack
                        .get(*index as usize)
                        .ok_or(RuntimeError::StackEmpty)?
                        .clone(),
                ),
                GetLocalLong(index) => self.stack.push(
                    self.stack
                        .get(*index as usize)
                        .ok_or(RuntimeError::StackEmpty)?
                        .clone(),
                ),
                SetLocal(index) => {
                    self.stack[*index as usize] = self.peek(0)?.clone();
                }
                SetLocalLong(index) => {
                    self.stack[*index as usize] = self.peek(0)?.clone();
                }
                Equal => {
                    let (a, b) = self.pop_pair()?;
                    self.stack.push(Value::Boolean(b == a));
                }
                Greater => binary_op!(>, Boolean),
                Less => binary_op!(<, Boolean),
                Add => match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => {
                        binary_op_body!(+, Number);
                    }
                    (Value::r#String(_), Value::r#String(_)) => {
                        if let (Value::r#String(a), Value::r#String(b)) = self.pop_pair()? {
                            self.stack.push(Value::r#String(a + &b));
                        }
                    }
                    _ => return Err(RuntimeError::ArgumentTypes),
                },
                Subtract => binary_op!(-, Number),
                Multiply => binary_op!(*, Number),
                Divide => binary_op!(/, Number),
                Not => {
                    let val = Value::Boolean(self.pop()?.is_falsey());
                    self.stack.push(val);
                }
                Negate => {
                    let value = self.pop()?;
                    self.stack.push(value.negate()?);
                }
                Print => {
                    let val = self.pop()?;
                    self.print(format_args!("{}\n", val));
                }
                Return => {
                    break;
                }
            }
        }

        Ok(())
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackEmpty)
    }

    fn pop_pair(&mut self) -> Result<(Value, Value), RuntimeError> {
        let b = self.pop()?;
        let a = self.pop()?;
        Ok((a, b))
    }

    fn pop_many(&mut self, count: usize) -> Result<Vec<Value>, RuntimeError> {
        if self.stack.len() < count {
            Err(RuntimeError::StackEmpty)
        } else {
            Ok(self.stack.split_off(self.stack.len() - count))
        }
    }

    fn peek(&self, distance: usize) -> Result<&Value, RuntimeError> {
        if self.stack.len() < distance + 1 {
            return Err(RuntimeError::StackEmpty);
        }

        self.stack
            .get(self.stack.len() - 1 - distance)
            .ok_or(RuntimeError::StackEmpty)
    }

    /// Set the stream for program output. Any stream that implements [`Write`] is supported. Pass
    /// in `None` to use the default value ([`Stdout`]).
    ///
    /// ### Example
    ///
    /// ```
    /// # use lox_lang::VM;
    /// let mut out_buffer = Vec::new();
    ///
    /// let mut vm = VM::default();
    /// vm.set_streams(Some(&mut out_buffer), None);
    /// vm.interpret("nil == true");
    /// vm.interpret("5 * 6 + 12");
    ///
    /// assert_eq!(String::from_utf8(out_buffer).unwrap(), "false\n42\n");
    /// ```
    ///
    /// [`Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
    /// [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
    pub fn set_stream(&mut self, out: Option<&'a mut dyn Write>) {
        self.stdout = out;
    }

    fn print(&mut self, args: fmt::Arguments) {
        if let Some(out) = &mut self.stdout {
            out.write_fmt(args).unwrap();
        } else {
            io::stdout().write_fmt(args).unwrap();
        }
    }
}

#[derive(Clone)]
enum Op {
    // constants
    Constant(u8),
    ConstantLong(u16),

    // immediates
    Nil,
    True,
    False,

    // actions
    Pop,
    PopN(u8),
    GetGlobal(u8),
    GetGlobalLong(u16),
    DefineGlobal(u8),
    DefineGlobalLong(u16),
    SetGlobal(u8),
    SetGlobalLong(u16),
    GetLocal(u8),
    GetLocalLong(u16),
    SetLocal(u8),
    SetLocalLong(u16),

    // operators
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,

    // control flow
    Print,
    Return,
}

impl Op {
    fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        use Op::*;

        macro_rules! constant_instr {
            ($name:literal, $index:expr) => {{
                write!(f, concat!($name, "\t{}"), chunk.constants[*$index as usize])
            }};
        }

        match self {
            Constant(index) => constant_instr!("CONSTANT", index),
            ConstantLong(index) => constant_instr!("CONSTANT_LONG", index),
            Nil => write!(f, "NIL"),
            True => write!(f, "TRUE"),
            False => write!(f, "FALSE"),
            Pop => write!(f, "POP"),
            PopN(count) => write!(f, "POP_N\t{}", count),
            GetGlobal(index) => constant_instr!("GET_GLOBAL", index),
            GetGlobalLong(index) => constant_instr!("GET_GLOBAL_LONG", index),
            DefineGlobal(index) => constant_instr!("DEF_GLOBAL", index),
            DefineGlobalLong(index) => constant_instr!("DEF_GLOBAL_LONG", index),
            SetGlobal(index) => constant_instr!("SET_GLOBAL", index),
            SetGlobalLong(index) => constant_instr!("SET_GLOBAL_LONG", index),
            GetLocal(index) => write!(f, "GET_LOCAL\t{}", index),
            GetLocalLong(index) => write!(f, "GET_LOCAL_LONG\t{}", index),
            SetLocal(index) => write!(f, "SET_LOCAL\t{}", index),
            SetLocalLong(index) => write!(f, "SET_LOCAL_LONG\t{}", index),
            Equal => write!(f, "EQUAL"),
            Greater => write!(f, "GREATER"),
            Less => write!(f, "LESS"),
            Add => write!(f, "ADD"),
            Subtract => write!(f, "SUBTRACT"),
            Multiply => write!(f, "MULTIPLY"),
            Divide => write!(f, "DIVIDE"),
            Not => write!(f, "NOT"),
            Negate => write!(f, "NEGATE"),
            Print => write!(f, "PRINT"),
            Return => write!(f, "RETURN"),
        }
    }
}
