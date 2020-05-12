use std::{
    convert::TryInto, env, error::Error, ffi::OsString, fmt, fs, process::Command, time::Instant,
};

use crate::{Class, Instance, NativeFun, Value, VM};

type StaticNativeFun = fn(&[Value]) -> Result<Value, Box<dyn Error>>;

impl VM {
    pub fn add_std_globals(&mut self) {
        // free functions
        self.define_closure_value("clock", create_clock_fn());
        self.define_fn_value("readFile", read_file);
        self.define_fn_value("writeFile", write_file);
        self.define_fn_value("shell", shell);
        self.define_fn_value("env", environment_var);
        self.define_fn_value("setEnv", set_environment_var);

        // global instances
        self.add_arguments_instance();
    }

    fn define_fn_value(&mut self, name: &'static str, f: StaticNativeFun) {
        let fn_value = Value::NativeFun(self.alloc(Box::new(f)));
        self.define_global(name, fn_value);
    }

    fn define_closure_value(&mut self, name: &'static str, c: NativeFun) {
        let fn_value = Value::NativeFun(self.alloc(c));
        self.define_global(name, fn_value);
    }

    pub fn add_arguments_instance(&mut self) {
        let args = env::args_os().collect::<Vec<_>>();
        let count = args.len();

        let get_arg = Box::new(move |a: &[Value]| match a {
            [Value::Number(n)] if n.is_finite() && n.is_sign_positive() => Ok(args
                .get(n.round().abs() as usize)
                .map_or(Value::Nil, |val: &OsString| {
                    val.to_string_lossy().as_ref().into()
                })),
            [Value::Number(_)] => Err(Box::new(LoxStdErr::InvalidArgument {
                expected: "a positive, finite number as argument index",
            }) as Box<dyn Error>),
            _ if a.len() != 1 => Err(Box::new(LoxStdErr::ArityMismatch {
                expected: 1,
                got: a.len().try_into().unwrap_or(255),
            }) as Box<dyn Error>),
            _ => Err(Box::new(LoxStdErr::ArgumentTypes {
                expected: "a number (command line argument index)",
            }) as Box<dyn Error>),
        });

        let mut arg_class = Class::new(&"Arguments");
        arg_class
            .methods
            .insert("get".into(), Value::NativeFun(self.alloc(get_arg)));

        let mut arg_inst = Instance::new(self.alloc(arg_class));
        arg_inst
            .fields
            .insert("count".into(), Value::Number(count as f64));
        let arg_value = Value::Instance(self.alloc(arg_inst));
        self.define_global("arguments", arg_value);
    }
}

#[derive(Debug)]
enum LoxStdErr {
    ArityMismatch { expected: u8, got: u8 },
    ArgumentTypes { expected: &'static str },
    InvalidArgument { expected: &'static str },
}

impl fmt::Display for LoxStdErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ArityMismatch { expected, got } => {
                write!(f, "expected {} arguments, got {}", expected, got)
            }
            Self::ArgumentTypes { expected } => {
                write!(f, "wrong argument type(s) supplied: expected {}", expected)
            }
            Self::InvalidArgument { expected } => write!(
                f,
                "invalid argument value(s) supplied: expected {}",
                expected
            ),
        }
    }
}

impl Error for LoxStdErr {}

/// Create a stateful `clock` function.
///
/// Comparable to the `clock` function in `time.h`. When invoked inside the runtime, it returns a
/// floating-point number representing the number of seconds elapsed since the closure was created.
#[must_use]
pub fn create_clock_fn() -> NativeFun {
    let start_time = Instant::now();
    let clock_cls = move |_: &[Value]| Ok(start_time.elapsed().as_secs_f64().into());
    Box::new(clock_cls) as NativeFun
}

/// Read a file into a string.
///
/// ### Errors
///
/// An `Err` will be returned if:
///
/// - The specified file is not found.
/// - The specified file is not valid UTF-8.
pub fn read_file(args: &[Value]) -> Result<Value, Box<dyn Error>> {
    match args {
        [Value::r#String(s)] => Ok(fs::read_to_string(s.as_ref())?.into()),
        _ if args.len() != 1 => Err(Box::new(LoxStdErr::ArityMismatch {
            expected: 1,
            got: args.len().try_into().unwrap_or(255),
        })),
        _ => Err(Box::new(LoxStdErr::ArgumentTypes {
            expected: "string for filename",
        })),
    }
}

/// Write a string into a file.
///
/// If the file does not already exist, it will be created. If it does exist, its contents will be
/// replaced with the string passed in.
///
/// ### Errors
///
/// An `Err` will be returned if:
///
/// - The specified file is not found.
/// - The specified file is not valid UTF-8.
pub fn write_file(args: &[Value]) -> Result<Value, Box<dyn Error>> {
    match args {
        [Value::r#String(file_name), Value::r#String(contents)] => {
            fs::write(file_name.as_ref(), contents.as_ref())?;
            Ok(Value::Nil)
        }
        _ if args.len() != 2 => Err(Box::new(LoxStdErr::ArityMismatch {
            expected: 2,
            got: args.len().try_into().unwrap_or(255),
        })),
        _ => Err(Box::new(LoxStdErr::ArgumentTypes {
            expected: "string for filename",
        })),
    }
}

/// Run a shell command.
///
/// ### Errors
///
/// An `Err` will be returned if the command fails to execute.
pub fn shell(args: &[Value]) -> Result<Value, Box<dyn Error>> {
    match args {
        [Value::r#String(cmd)] => {
            let output = if cfg!(target_os = "windows") {
                Command::new("cmd").args(&["/C", &cmd]).output()?
            } else {
                Command::new("sh").args(&["-c", &cmd]).output()?
            };

            Ok(String::from_utf8_lossy(&output.stdout).as_ref().into())
        }
        _ if args.len() != 1 => Err(Box::new(LoxStdErr::ArityMismatch {
            expected: 1,
            got: args.len().try_into().unwrap_or(255),
        })),
        _ => Err(Box::new(LoxStdErr::ArgumentTypes {
            expected: "string for shell command",
        })),
    }
}

/// Get the value of an environment variable by name.
///
/// If the requested variable does not exist, `nil` is returned. If the variable's value contains
/// invalid Unicode, those characters will be replaced with the Unicode replacement character
/// (`U+FFFD`).
pub fn environment_var(args: &[Value]) -> Result<Value, Box<dyn Error>> {
    match args {
        [Value::r#String(name)] => {
            Ok(env::var_os(name.as_ref())
                .map_or(Value::Nil, |s| s.to_string_lossy().as_ref().into()))
        }
        _ if args.len() != 1 => Err(Box::new(LoxStdErr::ArityMismatch {
            expected: 1,
            got: args.len().try_into().unwrap_or(255),
        })),
        _ => Err(Box::new(LoxStdErr::ArgumentTypes {
            expected: "name of environment variable",
        })),
    }
}

/// Set the value of an environment variable.
pub fn set_environment_var(args: &[Value]) -> Result<Value, Box<dyn Error>> {
    match args {
        [Value::r#String(name), Value::r#String(value)] => {
            env::set_var(name.as_ref(), value.as_ref());
            Ok(Value::Nil)
        }
        _ if args.len() != 2 => Err(Box::new(LoxStdErr::ArityMismatch {
            expected: 2,
            got: args.len().try_into().unwrap_or(255),
        })),
        _ => Err(Box::new(LoxStdErr::ArgumentTypes {
            expected: "name of environment variable",
        })),
    }
}
