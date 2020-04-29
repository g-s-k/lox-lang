use std::{collections::HashMap, fmt, io::Write};

use super::{Chunk, Compiler, Error, ErrorCategory, Op, Value};

#[derive(Clone, Debug)]
pub(crate) enum RuntimeError {
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
        match self {
            Self::ArgumentTypes => write!(f, "incompatible types for operation"),
            Self::StackEmpty => write!(f, "tried to pop value from empty stack"),
            Self::UndefinedGlobal(name) => {
                write!(f, "tried to access undefined variable `{}`", name)
            }
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
    /// Compile and run Lox code from a source string.
    ///
    /// ### Errors
    ///
    /// Errors are returned in a `Vec`. This is because compilation (not runtime) is able to
    /// continue after error(s) are encountered.
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

        self.run(&chunk).map_err(|err| {
            log::error!("{}", err);
            vec![Box::new(err) as Box<dyn Error>]
        })
    }

    #[allow(clippy::too_many_lines)]
    fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        macro_rules! read_constant {
            ($index: expr) => {
                chunk.read_constant(*$index as usize)
            };
        }

        macro_rules! write_constant {
            ($index: expr) => {{
                self.stack.push(read_constant!($index).clone());
            }};
        }

        macro_rules! read_string {
            ($index: expr) => {{
                if let Value::r#String(s) = read_constant!($index) {
                    s
                } else {
                    unreachable!();
                }
            }};
        }

        macro_rules! define_global {
            ($index: expr) => {{
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
            }};
        }

        macro_rules! get_local {
            ($index: expr) => {{
                self.stack.push(
                    self.stack
                        .get(*$index as usize)
                        .ok_or(RuntimeError::StackEmpty)?
                        .clone(),
                )
            }};
        }

        macro_rules! set_local {
            ($index: expr) => {{
                self.stack[*$index as usize] = self.peek(0)?.clone();
            }};
        }

        macro_rules! binary_op_body {
            ($op: tt, $variant: ident) => {
                if let (Value::Number(a), Value::Number(b)) = self.pop_pair()? {
                    self.stack.push(Value::$variant(a $op b));
                }
            };
        }

        macro_rules! binary_op {
            ($op: tt, $variant: ident) => {{
                match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => (),
                    _ => return Err(RuntimeError::ArgumentTypes),
                }

                // should be infallible now
                binary_op_body!($op, $variant)
            }};
        }

        let mut ip = 0;

        while let Some(instruction) = chunk.code.get(ip) {
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
                log::debug!("{:1$x}", chunk, ip);
            }

            match instruction {
                Op::Constant(index) => write_constant!(index),
                Op::ConstantLong(index) => write_constant!(index),
                Op::Nil => self.stack.push(Value::Nil),
                Op::True => self.stack.push(Value::Boolean(true)),
                Op::False => self.stack.push(Value::Boolean(false)),
                Op::Pop => {
                    self.pop()?;
                }
                Op::PopN(count) => {
                    self.pop_many(*count as usize)?;
                }
                Op::GetGlobal(index) => get_global!(index),
                Op::GetGlobalLong(index) => get_global!(index),
                Op::DefineGlobal(index) => define_global!(index),
                Op::DefineGlobalLong(index) => define_global!(index),
                Op::SetGlobal(index) => set_global!(index),
                Op::SetGlobalLong(index) => set_global!(index),
                Op::GetLocal(index) => get_local!(index),
                Op::GetLocalLong(index) => get_local!(index),
                Op::SetLocal(index) => set_local!(index),
                Op::SetLocalLong(index) => set_local!(index),
                Op::Equal => {
                    let (a, b) = self.pop_pair()?;
                    self.stack.push(Value::Boolean(b == a));
                }
                Op::Greater => binary_op!(>, Boolean),
                Op::Less => binary_op!(<, Boolean),
                Op::Add => match (self.peek(1)?, self.peek(0)?) {
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
                Op::Subtract => binary_op!(-, Number),
                Op::Multiply => binary_op!(*, Number),
                Op::Divide => binary_op!(/, Number),
                Op::Not => {
                    let val = Value::Boolean(self.pop()?.is_falsey());
                    self.stack.push(val);
                }
                Op::Negate => {
                    let value = self.pop()?;
                    self.stack.push(value.negate()?);
                }
                Op::Print => {
                    let val = self.pop()?;
                    self.print(format_args!("{}\n", val));
                }
                Op::JumpIfFalse(distance) => {
                    if self.peek(0)?.is_falsey() {
                        ip += *distance as usize
                    }
                }
                Op::Jump(distance) => ip += *distance as usize,
                Op::Loop(distance) => ip -= *distance as usize,
                Op::Return => {
                    break;
                }
            }

            ip += 1;
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
    /// let (mut out_buffer_1, mut out_buffer_2) = (Vec::new(), Vec::new());
    /// let mut vm = VM::default();
    ///
    /// vm.replace_stream(Some(&mut out_buffer_1));
    /// vm.interpret("print nil == true;");
    ///
    /// // can also change streams on an existing instance
    /// vm.replace_stream(Some(&mut out_buffer_2));
    /// vm.interpret("for (var a = 3; a < 12; a = a + 3) print a;");
    ///
    /// assert_eq!(std::str::from_utf8(&out_buffer_1).unwrap(), "false\n");
    /// assert_eq!(std::str::from_utf8(&out_buffer_2).unwrap(), "3\n6\n9\n");
    /// ```
    ///
    /// [`Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
    /// [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
    pub fn replace_stream(&mut self, out: Option<&'a mut dyn Write>) {
        self.stdout.take();
        self.stdout = out;
    }

    fn print(&mut self, args: fmt::Arguments) {
        if let Some(ref mut out) = self.stdout {
            out.write_fmt(args).unwrap();
        } else {
            print!("{}", args);
        }
    }
}
