use std::{any::Any, collections::HashMap};

mod errors;
mod frame;
mod gc;
mod io;
mod list;
mod run;

pub(crate) use errors::RuntimeError;

use {
    crate::{Compiler, Error, Gc, UpvalueRef, Value},
    frame::CallFrame,
    list::List,
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
pub struct VM {
    stdout: Option<Vec<u8>>,

    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<Box<str>, Value>,
    open_upvalues: List<Gc<UpvalueRef>>,

    objects: List<Gc<dyn Any>>,
    compiler_roots: Vec<Value>,
    total_allocations: usize,
    next_gc: usize,
}

impl Default for VM {
    /// The `VM` constructor.
    ///
    /// Program output defaults to [`Stdout`]. To customize
    /// this behavior, see the [`buffer_output`] method.
    ///
    /// [`buffer_output`]: #method.buffer_output
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

impl<'source> VM {
    /// Compile and run Lox code from a source string.
    ///
    /// ### Errors
    ///
    /// Errors are returned in a `Vec`. This is because compilation (not runtime) is able to
    /// continue after error(s) are encountered.
    pub fn interpret<T: AsRef<str> + 'source>(&mut self, source: T) -> Result<(), Vec<Error>> {
        self.compiler_roots.clear();

        let fun = Compiler::compile(source.as_ref(), &mut |f| {
            let val = Value::Fun(self.alloc(f));
            self.compiler_roots.push(val.clone());
            val
        })
        .map_err(|errs| errs.into_iter().map(Into::into).collect::<Vec<_>>())?;

        let fun = Value::Fun(self.alloc(fun));

        self.stack.push(fun);
        self.compiler_roots.clear();

        let _ = self.call_value_from_stack(0);

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

    /// Define a global variable inside the runtime.
    ///
    /// Since globals are late bound in Lox, functions that reference the provided name will see
    /// the provided value, **even if they were declared in the runtime _before_ calling this
    /// method**.
    ///
    /// ### Example
    ///
    /// ```
    /// use std::io::Read;
    ///
    /// let mut vm = lox_lang::VM::default();
    /// vm.buffer_output(true);
    ///
    /// vm.interpret("fun hello() { print world; }");
    /// vm.define_global("world", "greetings, Earthling.".into());
    /// vm.interpret("hello();");
    ///
    /// let mut buffer = String::new();
    /// vm.read_to_string(&mut buffer).unwrap();
    /// assert_eq!(buffer, "greetings, Earthling.\n");
    /// ```
    pub fn define_global<T: ?Sized + ToString>(&mut self, name: &T, value: Value) {
        self.globals
            .insert(name.to_string().into_boxed_str(), value);
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // we free all objects here, as the VM is ultimately an arena for them. all other
        // references to those objects are weak ones.
        //
        // note that we do not touch the `open_upvalues` list - they are actually owned by
        // `objects`.
        while let Some(obj) = self.objects.pop() {
            obj.free();
        }
    }
}
