use std::{cmp, error, fmt};

use super::{Chunk, RuntimeError};

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

/// Underlying representation of runtime values in Lox.
#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    r#String(Box<str>),
    #[doc(hidden)]
    Fun(&'static Fun),
    #[doc(hidden)]
    Closure(&'static Fun, &'static [*mut upvalue::Obj]),
    NativeFun(NativeFun),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::r#String(s) => write!(f, "{}", s),
            Self::Fun(fun) => write!(f, "{}", fun),
            Self::Closure(fun, _) => write!(f, "{}", fun),
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
            Self::Fun(fun) => write!(f, "Fun({})", fun),
            Self::Closure(fun, upvals) => write!(f, "Closure({}, {:#?})", fun, upvals),
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

pub use upvalue::{Obj as UpvalueObj, Type as UpvalueType};

mod upvalue {
    use std::mem;

    #[derive(Clone, Debug)]
    pub struct Obj {
        pub(crate) next: Option<Box<Obj>>,
        pub(crate) value: Type,
    }

    impl Obj {
        pub fn new(index: usize) -> Self {
            Self {
                next: None,
                value: Type::Live(index),
            }
        }

        fn prepend(self, head: &mut Option<Box<Self>>) -> *mut Self {
            let old_head = mem::replace(head, Some(Box::new(self)));

            if let Some(ref mut new_head) = head {
                new_head.next = old_head;
                &mut **new_head
            } else {
                unreachable!();
            }
        }

        pub fn insert_live(head: &mut Option<Box<Self>>, slot: usize) -> *mut Self {
            if let Some(inner) = head {
                match inner.value {
                    Type::Live(c) if c == slot => &mut **inner,
                    Type::Live(c) if c < slot => Self::new(slot).prepend(head),
                    _ => Self::insert_live(&mut inner.next, slot),
                }
            } else {
                Self::new(slot).prepend(head)
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum Type {
        Live(usize),
        Captured(*mut super::Value),
    }
}
