use std::{any::Any, cmp, collections::HashMap, error, fmt};

mod chunk;
mod obj;
mod ops;

pub(crate) use {chunk::Chunk, obj::Gc, ops::Op};

use super::RuntimeError;

/// Underlying representation of runtime values in Lox.
#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    r#String(Box<str>),
    NativeFun(Gc<NativeFun>),

    #[doc(hidden)]
    Fun(Gc<Fun>),
    #[doc(hidden)]
    Closure(Gc<Fun>, Box<[Gc<UpvalueRef>]>),
    #[doc(hidden)]
    BoundMethod {
        recv: Gc<Instance>,
        fun: Gc<Fun>,
        upvalues: Box<[Gc<UpvalueRef>]>,
    },
    #[doc(hidden)]
    Class(Gc<Class>),
    #[doc(hidden)]
    Instance(Gc<Instance>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::r#String(s) => write!(f, "{}", s),
            Self::Fun(fun) => write!(f, "{}", **fun),
            Self::Closure(fun, _) => write!(f, "{}", **fun),
            Self::Class(c) => write!(f, "{}", **c),
            Self::Instance(i) => write!(f, "{} instance", *i.class),
            Self::BoundMethod { fun, .. } => write!(f, "{}", **fun),
            Self::NativeFun(_) => write!(f, "#<native fun>"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Boolean(b) => write!(f, "Boolean({})", b),
            Self::Number(v) => write!(f, "Number({})", v),
            Self::r#String(s) => write!(f, "String({})", s),
            Self::Fun(fun) => write!(f, "Fun({})", **fun),
            Self::Closure(fun, upvals) => write!(f, "Closure({}, {:#?})", **fun, upvals),
            Self::Class(c) => write!(f, "Class({})", **c),
            Self::Instance(i) => write!(f, "Instance({:?})", i),
            Self::BoundMethod {
                recv,
                fun,
                upvalues,
            } => write!(
                f,
                "BoundMethod {{ {:?}#{} {:#?} }}",
                **recv, **fun, upvalues
            ),
            Self::NativeFun(ptr) => write!(f, "NativeFun({:p})", ptr),
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::r#String(a), Self::r#String(b)) => a == b,
            _ => false,
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::Number(f)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::r#String(s.into_boxed_str())
    }
}

impl Value {
    pub(crate) fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Boolean(false) => true,
            _ => false,
        }
    }

    pub(crate) fn negate(self) -> Result<Value, RuntimeError> {
        if let Self::Number(a) = self {
            Ok(Self::Number(-a))
        } else {
            Err(RuntimeError::ArgumentTypes)
        }
    }

    pub(crate) fn mark(&self, grays: &mut Vec<Gc<dyn Any>>) {
        match self {
            Self::Fun(f) => {
                f.mark();
                grays.push(f.as_any());
            }
            Self::Closure(f, u) => {
                f.mark();
                grays.push(f.as_any());

                for u_val in u.iter() {
                    u_val.mark();
                    grays.push(u_val.as_any());
                }
            }
            Self::Class(c) => {
                c.mark();
                grays.push(c.as_any());
            }
            Self::Instance(i) => {
                i.mark();
                grays.push(i.as_any());
            }
            Self::BoundMethod {
                recv,
                fun,
                upvalues,
            } => {
                recv.mark();
                grays.push(recv.as_any());

                fun.mark();
                grays.push(fun.as_any());

                for u_val in upvalues.iter() {
                    u_val.mark();
                    grays.push(u_val.as_any());
                }
            }
            Self::NativeFun(nf) => {
                nf.mark();
                grays.push(nf.as_any());
            }
            _ => (),
        }
    }
}

impl Gc<dyn Any> {
    pub(crate) fn blacken(self, gray_stack: &mut Vec<Gc<dyn Any>>) {
        if self.is_marked() {
            return;
        }

        #[cfg(feature = "trace-gc")]
        log::debug!("{0:p} blacken\t{0:?}", self);

        if let Some(f) = self.downcast_ref::<Fun>() {
            for c in &f.chunk.constants {
                c.mark(gray_stack);
            }
        } else if let Some(u) = self.downcast_ref::<UpvalueRef>() {
            if let UpvalueRef::Captured(ref v) = *u {
                v.mark(gray_stack);
            }
        } else if let Some(Class { methods, .. }) = self.downcast_ref::<Class>() {
            for method in methods.values() {
                method.mark(gray_stack);
            }
        } else if let Some(Instance { class, fields }) = self.downcast_ref::<Instance>() {
            class.mark();
            gray_stack.push(class.as_any());

            for value in fields.values() {
                value.mark(gray_stack);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Fun {
    pub(crate) name: Box<str>,
    pub(crate) arity: u8,
    pub(crate) chunk: Chunk,
}

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<fun {}/{}>", self.name, self.arity)
    }
}

impl Fun {
    pub(crate) fn new<T: ToString>(name: T, arity: u8) -> Self {
        Self {
            arity,
            name: name.to_string().into_boxed_str(),
            chunk: Chunk::new(name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum UpvalueRef {
    Live(usize),
    Captured(Box<Value>),
}

impl UpvalueRef {
    pub(crate) fn new(slot: usize) -> Self {
        Self::Live(slot)
    }

    pub(crate) fn close(&mut self, ptr: Value) {
        *self = Self::Captured(Box::new(ptr));
    }
}

impl fmt::Display for UpvalueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Live(idx) => write!(f, "at stack index {}", idx),
            Self::Captured(ptr) => write!(f, "at address {:p}", ptr),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub(crate) name: Box<str>,
    pub(crate) methods: HashMap<Box<str>, Value>,
}

impl Class {
    pub(crate) fn new<T: ToString>(name: &T) -> Self {
        Self {
            name: name.to_string().into_boxed_str(),
            methods: HashMap::new(),
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    pub(crate) class: Gc<Class>,
    pub(crate) fields: HashMap<Box<str>, Value>,
}

impl Instance {
    pub(crate) fn new(class: Gc<Class>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

/// A natively-implemented function that can be called from Lox code.
///
/// ## Example
///
/// ```
/// # use {std::{error::Error, fmt, io::Read}, lox_lang::{Value, VM}};
/// # #[derive(Debug)]
/// # struct MyError;
/// # impl fmt::Display for MyError {
/// #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
/// #         write!(f, "wrong types")
/// #     }
/// # }
/// # impl Error for MyError {}
/// # let mut vm = VM::default();
/// # vm.buffer_output(true);
/// /// This function wraps `str::replace` for use in Lox
/// let replace = vm.alloc(Box::new(|args: &[Value]| {
///     match args {
///         [Value::r#String(text), Value::r#String(pat), Value::r#String(rep)] =>
///             Ok(text.replace(pat.as_ref(), rep.as_ref()).into()),
///         _ => Err(Box::new(MyError) as Box<dyn Error>),
///     }
/// }) as lox_lang::NativeFun);
///
/// vm.define_global("replace", Value::NativeFun(replace));
///
/// vm.interpret(r#"
///     var proverb = "what is old becomes new again";
///     print replace(proverb, "new", "old");
/// "#);
///
/// # let mut output = String::new();
/// # vm.read_to_string(&mut output).unwrap();
/// assert_eq!(output, "what is old becomes old again\n");
/// ```
pub type NativeFun = Box<dyn FnMut(&[Value]) -> Result<Value, Box<dyn error::Error>>>;
