use std::{collections::HashMap, error, fmt, io::Write};

use super::{Chunk, Compiler, Error, Fun, Op, Value};

#[derive(Debug)]
pub(crate) enum RuntimeError {
    ArgumentTypes,
    StackEmpty,
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

#[derive(Clone, Copy, Debug)]
struct CallFrame {
    // we know the `Fun` ref will be valid for the full lifetime of the call frame, but the
    // compiler doesn't believe us if we use a regular Rust reference
    func: *const Fun,
    inst: usize,
    base: u16,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            func: std::ptr::null(),
            inst: 0,
            base: 0,
        }
    }
}

impl CallFrame {
    fn new(func: &Fun, stack_top: u16) -> Self {
        Self {
            func: &*func,
            inst: 0,
            base: stack_top.checked_sub(1).unwrap_or_default() - u16::from(func.arity),
        }
    }

    fn chunk(&self) -> &Chunk {
        unsafe { &(*self.func).chunk }
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
pub struct VM<'writer> {
    stdout: Option<&'writer mut dyn Write>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<Box<str>, Value>,
}

impl Default for VM<'_> {
    /// The `VM` constructor.
    ///
    /// Program output defaults to [`Stdout`]. To customize
    /// this behavior, see the [`replace_stream`] method.
    ///
    /// [`replace_stream`]: #method.replace_stream
    /// [`Stdout`]: https://doc.rust-lang.org/std/io/struct.Stdout.html
    fn default() -> Self {
        VM {
            stdout: None,
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        }
    }
}

impl<'writer, 'source> VM<'writer> {
    /// Compile and run Lox code from a source string.
    ///
    /// ### Errors
    ///
    /// Errors are returned in a `Vec`. This is because compilation (not runtime) is able to
    /// continue after error(s) are encountered.
    pub fn interpret<T: AsRef<str> + 'source>(&mut self, source: T) -> Result<(), Vec<Error>> {
        let fun = Compiler::compile(source.as_ref())
            .map_err(|errs| errs.into_iter().map(Into::into).collect::<Vec<_>>())?;

        let _ = self.stack.push(Value::Fun(Box::into_raw(Box::new(fun))));
        let _ = self.call_value(0);

        self.run().map_err(|err| {
            let mut line_no = None;
            let mut backtrace = format!("{}", err);

            for frame in self.frames.iter().rev() {
                let function = unsafe { &(*frame.func).name };
                let (line, _) = frame.chunk().find_line(frame.inst);

                if line_no.is_none() {
                    line_no = Some(line);
                }

                backtrace += &format!("\n=>> line {} in `{}`", line, function);
            }

            log::error!("{}", backtrace);

            vec![Error::from_runtime_error(err, line_no)]
        })
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
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

        while let Some(instruction) = self.step() {
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
                let frame = self.frame();
                log::debug!("{:1$x}", frame.chunk(), frame.inst);
            }

            self.frame_mut().inst += 1;

            match instruction {
                Op::Constant(index) => {
                    self.stack.push(self.read_constant(index).clone());
                }
                Op::ConstantLong(index) => {
                    self.stack.push(self.read_constant(index).clone());
                }
                Op::Nil => self.stack.push(Value::Nil),
                Op::True => self.stack.push(Value::Boolean(true)),
                Op::False => self.stack.push(Value::Boolean(false)),
                Op::Pop => {
                    self.stack.pop();
                }
                Op::PopN(count) => {
                    self.pop_many(count.into())?;
                }
                Op::GetGlobal(index) => self.fetch_global(index)?,
                Op::GetGlobalLong(index) => self.fetch_global(index)?,
                Op::DefineGlobal(index) => self.define_global_from_stack(index)?,
                Op::DefineGlobalLong(index) => self.define_global_from_stack(index)?,
                Op::SetGlobal(index) => self.set_global(index)?,
                Op::SetGlobalLong(index) => self.set_global(index)?,
                Op::GetLocal(index) => self.fetch_local(index)?,
                Op::GetLocalLong(index) => self.fetch_local(index)?,
                Op::SetLocal(index) => self.set_local(index)?,
                Op::SetLocalLong(index) => self.set_local(index)?,
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
                            let mut a = a.into_string();
                            a.push_str(&b);
                            self.stack.push(Value::r#String(a.into_boxed_str()));
                        }
                    }
                    _ => return Err(RuntimeError::ArgumentTypes),
                },
                Op::Subtract => binary_op!(-, Number),
                Op::Multiply => binary_op!(*, Number),
                Op::Divide => binary_op!(/, Number),
                Op::Not => {
                    if let Some(val) = self.stack.pop() {
                        self.stack.push(Value::Boolean(val.is_falsey()));
                    }
                }
                Op::Negate => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(value.negate()?);
                    }
                }
                Op::Print => {
                    if let Some(val) = self.stack.pop() {
                        self.print(format_args!("{}\n", val));
                    }
                }
                Op::JumpIfFalse(distance) => {
                    if self.peek(0)?.is_falsey() {
                        self.frame_mut().inst += distance as usize;
                    }
                }
                Op::Jump(distance) => self.frame_mut().inst += distance as usize,
                Op::Loop(distance) => self.frame_mut().inst -= distance as usize,
                Op::Call(arg_count) => {
                    self.call_value(arg_count)?;
                }
                Op::Return => {
                    let base_ptr = self.frame().base;
                    let result = self.stack.pop().ok_or(RuntimeError::StackEmpty)?;

                    self.frames.pop();
                    if self.frames.is_empty() {
                        self.stack.pop();
                        break;
                    }

                    self.stack.truncate(base_ptr as usize);
                    self.stack.push(result);
                }
            }
        }

        Ok(())
    }

    fn frame(&self) -> &CallFrame {
        if let Some(frame) = self.frames.last() {
            return frame;
        }

        unreachable!("Current call frame not found.")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        if let Some(frame) = self.frames.last_mut() {
            return frame;
        }

        unreachable!("Current call frame not found.")
    }

    fn step(&mut self) -> Option<Op> {
        let frame = self.frame();
        frame.chunk().code.get(frame.inst).copied()
    }

    fn read_constant<T: Into<usize>>(&self, index: T) -> &Value {
        self.frame().chunk().read_constant(index.into())
    }

    fn read_string<T: Into<usize> + fmt::Display + Copy>(&self, index: T) -> &str {
        match self.read_constant(index) {
            Value::r#String(s) => s,
            c =>
            unreachable!(
                "Invariant violation: tried to load a string value from the constant table at index {}; found non-string value {}",
                index, c
            ),
        }
    }

    fn define_global_from_stack<T: Into<usize> + fmt::Display + Copy>(
        &mut self,
        index: T,
    ) -> Result<(), RuntimeError> {
        if let Some(value) = self.stack.pop() {
            let var_name = self.read_string(index).to_string().into_boxed_str();
            self.globals.insert(var_name, value);
            Ok(())
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn fetch_global<T: Into<usize> + fmt::Display + Copy>(
        &mut self,
        index: T,
    ) -> Result<(), RuntimeError> {
        let var_name = self.read_string(index);
        if let Some(value) = self.globals.get(var_name).cloned() {
            self.stack.push(value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedGlobal(var_name.to_string()))
        }
    }

    fn set_global<T: Into<usize> + fmt::Display + Copy>(
        &mut self,
        index: T,
    ) -> Result<(), RuntimeError> {
        let var_str = self.read_string(index).to_string();
        if self.globals.contains_key(&*var_str) {
            let value = self.peek(0)?.clone();
            self.globals.insert(var_str.into_boxed_str(), value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedGlobal(var_str))
        }
    }

    fn fetch_local<T: Into<usize>>(&mut self, index: T) -> Result<(), RuntimeError> {
        let base_ptr = usize::from(self.frame().base);

        if let Some(val) = self.stack.get(index.into() + base_ptr).cloned() {
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn set_local<T: Into<usize>>(&mut self, index: T) -> Result<(), RuntimeError> {
        let base_ptr = usize::from(self.frame().base);
        let new_val = self.peek(0)?.clone();

        if let Some(el) = self.stack.get_mut(index.into() + base_ptr) {
            *el = new_val;
        }

        Ok(())
    }

    fn pop_pair(&mut self) -> Result<(Value, Value), RuntimeError> {
        if let Some(b) = self.stack.pop() {
            if let Some(a) = self.stack.pop() {
                return Ok((a, b));
            }
        }

        Err(RuntimeError::StackEmpty)
    }

    fn pop_many(&mut self, count: u16) -> Result<(), RuntimeError> {
        if let Some(new_len) = self.stack.len().checked_sub(count as usize) {
            self.stack.truncate(new_len);
            Ok(())
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn peek(&self, distance: usize) -> Result<&Value, RuntimeError> {
        if let Some(idx) = self.stack.len().checked_sub(distance + 1) {
            if let Some(val) = self.stack.get(idx) {
                return Ok(val);
            }
        }

        Err(RuntimeError::StackEmpty)
    }

    fn call_value(&mut self, arg_count: u8) -> Result<(), RuntimeError> {
        match *self.peek(arg_count as usize)? {
            Value::Fun(f) => self.call(unsafe { &*f }, arg_count),
            Value::NativeFun(f) => {
                let from = self
                    .stack
                    .len()
                    .checked_sub(arg_count.into())
                    .ok_or(RuntimeError::StackEmpty)?;

                let result = f(&self.stack[from..]);
                self.pop_many(u16::from(arg_count) + 1)?;
                self.stack
                    .push(result.map_err(RuntimeError::NativeFunError)?);

                Ok(())
            }
            _ => Err(RuntimeError::NotCallable),
        }
    }

    fn call(&mut self, callee: &Fun, arg_count: u8) -> Result<(), RuntimeError> {
        if arg_count != callee.arity {
            Err(RuntimeError::ArityMismatch(callee.arity, arg_count))
        } else {
            self.frames
                .push(CallFrame::new(callee, self.stack.len() as u16));
            Ok(())
        }
    }

    /// Define a global variable inside the runtime.
    ///
    /// Since globals are late bound in Lox, functions that reference the provided name will see
    /// the provided value, **even if they were declared in the runtime _before_ calling this
    /// method**.
    pub fn define_global<T: ToString>(&mut self, name: T, value: Value) {
        self.globals
            .insert(name.to_string().into_boxed_str(), value);
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
    pub fn replace_stream(&mut self, out: Option<&'writer mut dyn Write>) {
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
