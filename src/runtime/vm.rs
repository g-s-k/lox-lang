use std::{
    any::{self, Any},
    collections::HashMap,
    fmt,
    io::Write,
    mem,
    ops::Deref,
};

use {
    super::{CallFrame, RuntimeError, UpvalueRef},
    crate::{Compiler, Error, Fun, Gc, List, Op, Upvalue, Value},
};

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
    open_upvalues: List<Gc<UpvalueRef>>,

    objects: List<Gc<dyn Any>>,
    compiler_roots: Vec<Value>,
    total_allocations: usize,
    next_gc: usize,
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
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(256),
            globals: HashMap::new(),
            open_upvalues: List::new(),
            objects: List::new(),
            compiler_roots: Vec::new(),
            total_allocations: 0,
            next_gc: 1 << 20,
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
        self.compiler_roots.clear();

        let mut alloc_fun = |f| {
            let val = Value::Fun(self.alloc(f));
            self.compiler_roots.push(val.clone());
            val
        };

        let fun = Compiler::compile(source.as_ref(), &mut alloc_fun)
            .map_err(|errs| errs.into_iter().map(Into::into).collect::<Vec<_>>())?;

        let fun = alloc_fun(fun);

        self.stack.push(fun);
        self.compiler_roots.clear();

        let _ = self.call_value(0);

        self.run().map_err(|err| {
            let mut line_no = None;
            let mut backtrace = format!("{}", err);

            for frame in self.frames.iter().rev() {
                let function = &(*frame.func).name;
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
                Op::GetUpvalue(index) => self.fetch_upvalue(index)?,
                Op::GetUpvalueLong(index) => self.fetch_upvalue(index)?,
                Op::SetUpvalue(index) => self.set_upvalue(index)?,
                Op::SetUpvalueLong(index) => self.set_upvalue(index)?,
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
                Op::Closure(index, upvals) => self.create_closure(index, &upvals)?,
                Op::ClosureLong(index, upvals) => self.create_closure(index, &upvals)?,
                Op::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                }
                Op::Return => {
                    let base_ptr = self.frame().base;
                    let result = self.stack.pop().ok_or(RuntimeError::StackEmpty)?;

                    self.close_upvalues(base_ptr);

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
        frame.chunk().code.get(frame.inst).cloned()
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
            Err(RuntimeError::BadStackIndex(index.into(), self.stack.len()))
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

        let index = index.into();
        if let Some(val) = self.stack.get(index + base_ptr).cloned() {
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::BadStackIndex(index, self.stack.len()))
        }
    }

    fn set_local<T: Into<usize>>(&mut self, index: T) -> Result<(), RuntimeError> {
        let base_ptr = self.frame().base;
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
                Ok(val)
            } else {
                Err(RuntimeError::BadStackIndex(idx, self.stack.len()))
            }
        } else {
            Err(RuntimeError::StackEmpty)
        }
    }

    fn create_closure<T: Into<usize>>(
        &mut self,
        index: T,
        upvals: &[Upvalue],
    ) -> Result<(), RuntimeError> {
        if let Value::Fun(f) = self.read_constant(index).clone() {
            let mut upvalues = Vec::new();
            for Upvalue { index, is_local } in upvals {
                upvalues.push(if *is_local {
                    self.capture_upvalue(self.frame().base + index)
                } else {
                    self.frame().upvalues[*index].clone()
                });
            }

            self.stack
                .push(Value::Closure(f, upvalues.into_boxed_slice()));
            Ok(())
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }

    // possible TODO: double iteration here. not sure if it's worth the custom code to do it in one
    // iteration, given that the list is almost always empty or very short
    fn capture_upvalue(&mut self, slot: usize) -> Gc<UpvalueRef> {
        if let Some(r) = self.open_upvalues.find(|u_val| match &**u_val {
            UpvalueRef::Live(s) if *s == slot => true,
            _ => false,
        }) {
            r.clone()
        } else {
            let allocated = self.alloc(UpvalueRef::new(slot));

            self.open_upvalues
                .insert_before(allocated, |u_val| match &**u_val {
                    UpvalueRef::Live(s) if *s < slot => true,
                    _ => false,
                })
                .clone()
        }
    }

    fn fetch_upvalue<T: Into<usize>>(&mut self, index: T) -> Result<(), RuntimeError> {
        match self.frame().get_upvalue(index.into()) {
            Some(u) => match &*u {
                UpvalueRef::Live(slot) => {
                    if let Some(val) = self.stack.get(*slot).cloned() {
                        self.stack.push(val);
                        Ok(())
                    } else {
                        Err(RuntimeError::BadStackIndex(*slot, self.stack.len()))
                    }
                }
                UpvalueRef::Captured(ptr) => {
                    let val = (*ptr).clone();
                    self.stack.push(*val);
                    Ok(())
                }
            },
            _ => unreachable!(),
        }
    }

    fn set_upvalue<T: Into<usize>>(&mut self, index: T) -> Result<(), RuntimeError> {
        match self.frame().get_upvalue(index.into()) {
            Some(mut u) => match &mut *u {
                UpvalueRef::Live(slot) => {
                    let slot = *slot;
                    let val = self.peek(0)?.clone();
                    if let Some(el) = self.stack.get_mut(slot) {
                        *el = val;
                        Ok(())
                    } else {
                        Err(RuntimeError::BadStackIndex(slot, self.stack.len()))
                    }
                }
                UpvalueRef::Captured(ref mut val) => {
                    **val = self.peek(0)?.clone();
                    Ok(())
                }
            },
            _ => unreachable!(),
        }
    }

    fn close_upvalues(&mut self, max_height: usize) {
        while let Some(u_val) = self.open_upvalues.peek() {
            let slot = if let UpvalueRef::Live(slot) = &**u_val {
                if *slot < max_height {
                    break;
                }

                *slot
            } else {
                unreachable!("open upvalue list should not contain any closed upvalues")
            };

            if let Some(mut ptr) = self.open_upvalues.pop() {
                ptr.close(self.stack[slot].clone());
            }
        }
    }

    fn call_value(&mut self, arg_count: u8) -> Result<(), RuntimeError> {
        match self.peek(arg_count.into())? {
            Value::Closure(f, u) => {
                let (f, u) = (f.clone(), u.clone());
                self.call(f, u, arg_count)
            }
            Value::Fun(f) => {
                let f = f.clone();
                self.call(f, Box::new([]), arg_count)
            }
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

    fn call(
        &mut self,
        callee: Gc<Fun>,
        upvalues: Box<[Gc<UpvalueRef>]>,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        if arg_count == callee.arity {
            self.frames.push(CallFrame {
                func: callee,
                inst: 0,
                base: self
                    .stack
                    .len()
                    .checked_sub(1 + usize::from(arg_count))
                    .ok_or(RuntimeError::StackEmpty)?,
                upvalues,
            });
            Ok(())
        } else {
            Err(RuntimeError::ArityMismatch(callee.arity, arg_count))
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

    fn mark_roots(&mut self) -> Vec<Gc<dyn Any>> {
        let mut gray_stack = Vec::new();

        // locals and temporaries
        for el in &self.stack {
            el.mark(&mut gray_stack);
        }

        // globals
        for val in self.globals.values() {
            val.mark(&mut gray_stack);
        }

        // call frames
        for frame in &self.frames {
            frame.mark(&mut gray_stack);
        }

        // open upvalues
        for c in self.open_upvalues.iter() {
            c.mark();
            gray_stack.push(c.clone().into());
        }

        // compiler roots
        for root in &self.compiler_roots {
            root.mark(&mut gray_stack);
        }

        gray_stack
    }

    fn trace_references(&mut self, gray_stack: &mut Vec<Gc<dyn Any>>) {
        while let Some(top_obj) = gray_stack.pop() {
            self.blacken(top_obj, gray_stack);
        }
    }

    fn blacken(&mut self, obj: Gc<dyn Any>, gray_stack: &mut Vec<Gc<dyn Any>>) {
        if obj.is_marked() {
            return;
        }

        #[cfg(feature = "trace-gc")]
        log::debug!("{0:p} blacken\t{0:?}", obj);

        if let Some(f) = obj.downcast_ref::<Fun>() {
            for c in &f.chunk.constants {
                c.mark(gray_stack);
            }
        } else if let Some(u) = obj.downcast_ref::<UpvalueRef>() {
            if let UpvalueRef::Captured(ref v) = *u {
                v.mark(gray_stack);
            }
        }
    }

    fn sweep(&mut self) {
        let to_drop = self.objects.retain(|obj| {
            if obj.is_marked() {
                obj.clear_mark();
                true
            } else {
                false
            }
        });

        for ptr in to_drop {
            self.total_allocations -= mem::size_of_val(&*ptr);
            ptr.free();
        }
    }

    fn collect_garbage(&mut self) {
        #[cfg(feature = "trace-gc")]
        let before = self.total_allocations;

        #[cfg(feature = "trace-gc")]
        log::debug!("gc begin :: total allocations {} bytes", before);

        let mut gray_stack = self.mark_roots();
        self.trace_references(&mut gray_stack);
        self.sweep();

        // adjust threshold
        self.next_gc = self.total_allocations * 2;

        #[cfg(feature = "trace-gc")]
        log::debug!(
            "gc end   :: collected {} bytes (total was {}, now {}) :: next at {}",
            before - self.total_allocations,
            before,
            self.total_allocations,
            self.next_gc
        );
    }

    fn should_collect(&self) -> bool {
        cfg!(feature = "stress-test-gc") || self.total_allocations > self.next_gc
    }

    fn alloc<T: Any>(&mut self, obj: T) -> Gc<T> {
        if self.should_collect() {
            self.collect_garbage();
        }

        let size = mem::size_of::<T>();
        self.total_allocations += size;

        let el_ptr = self.objects.push(Gc::new(Box::new(obj)));

        if let Some(obj_ref) = el_ptr.downcast() {
            #[cfg(feature = "trace-gc")]
            log::debug!(
                "{:p} allocate {} bytes for {}",
                obj_ref,
                size,
                any::type_name::<T>()
            );

            obj_ref
        } else {
            unreachable!(
                "Pushed a value of type {}, but was unable to coerce a reference with that type from {:?}.",
                any::type_name::<T>(),
                el_ptr.deref()
            );
        }
    }
}
