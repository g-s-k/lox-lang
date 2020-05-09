use std::{any::Any, cmp, collections::HashMap, error, fmt};

use super::{Chunk, Gc, RuntimeError, UpvalueRef};

/// Underlying representation of runtime values in Lox.
#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    r#String(Box<str>),
    #[doc(hidden)]
    Fun(Gc<Fun>),
    #[doc(hidden)]
    Closure(Gc<Fun>, Box<[Gc<UpvalueRef>]>),
    Class(Gc<Class>),
    Instance(Gc<Instance>),
    NativeFun(NativeFun),
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
                grays.push(f.clone().into());
            }
            Self::Closure(f, u) => {
                f.mark();
                grays.push(f.clone().into());

                for u_val in u.iter() {
                    u_val.mark();
                    grays.push(u_val.clone().into());
                }
            }
            Self::Class(c) => {
                c.mark();
                grays.push(c.clone().into());
            }
            Self::Instance(i) => {
                i.mark();
                grays.push(i.clone().into());
            }
            _ => (),
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
pub struct Class {
    name: Box<str>,
}

impl Class {
    pub(crate) fn new<T: ToString>(name: T) -> Self {
        Self {
            name: name.to_string().into_boxed_str(),
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
/// # use {std::{error::Error, fmt}, lox_lang::{Value, VM}};
/// # #[derive(Debug)]
/// # struct MyError;
/// # impl fmt::Display for MyError {
/// #     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
/// #         write!(f, "wrong types")
/// #     }
/// # }
/// # impl Error for MyError {}
/// /// This function wraps `str::replace` for use in Lox
/// fn replace(args: &[Value]) -> Result<Value, Box<dyn Error>> {
///     match args {
///         [Value::r#String(text), Value::r#String(pat), Value::r#String(rep)] =>
///             Ok(Value::r#String(text.replace(pat.as_ref(), rep).into())),
///         _ => Err(Box::new(MyError)),
///     }
/// }
///
/// let mut output = Vec::new();
/// let mut vm = VM::default();
/// vm.replace_stream(Some(&mut output));
/// vm.define_global("replace", Value::NativeFun(replace));
///
/// vm.interpret(r#"
///     var proverb = "what is old becomes new again";
///     print replace(proverb, "new", "old");
/// "#);
///
/// assert_eq!(
///     String::from_utf8(output).unwrap(),
///     "what is old becomes old again\n"
/// );
/// ```
pub type NativeFun = fn(args: &[Value]) -> Result<Value, Box<dyn error::Error>>;
