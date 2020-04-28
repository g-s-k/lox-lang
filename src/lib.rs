use std::{
    cmp, fmt,
    rc::{Rc, Weak},
};

mod chunk;
mod compiler;
mod error;
mod scanner;
mod token;

pub use error::{Error, ErrorCategory};

use {
    chunk::Chunk,
    compiler::Compiler,
    scanner::{ScanError, Scanner},
    token::{Token, TokenType},
};

pub struct VM {
    stack: Vec<Value>,
    heap: Option<Rc<Object>>,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            heap: None,
        }
    }
}

impl VM {
    pub fn interpret<T: AsRef<str>>(&mut self, source: T) -> Result<(), Error> {
        match Compiler::new(source.as_ref(), self).compile() {
            Ok(chunk) => {
                #[cfg(feature = "trace-compilation")]
                println!("{}", chunk);

                match self.run(chunk) {
                    Ok(_) => Ok(()),
                    Err(e) => {
                        eprintln!("{}", e);
                        Err(e)
                    }
                }
            }
            Err(compile_errors) => {
                // report all errors
                for err in &compile_errors {
                    eprintln!("{}", err);
                }

                Err(compile_errors.last().unwrap().err)
            }
        }
    }

    fn run(&mut self, chunk: Chunk) -> Result<(), Error> {
        macro_rules! binary_op {
            ( $op:tt, $variant:ident ) => {{
                match (self.peek(1)?, self.peek(0)?) {
                    (Value::Number(_), Value::Number(_)) => (),
                    _ => return Err(Error::ArgumentTypes),
                }

                // should be infallible now
                if let (Value::Number(b), Value::Number(a)) =
                    (self.pop()?, self.pop()?) {
                    self.stack.push(Value::$variant(a $op b));
                }
            }};
        }

        for (_index, instruction) in chunk.code.iter().enumerate() {
            {
                #![cfg(feature = "trace-execution")]
                // print stack before operation
                eprint!("          ");
                for value in &self.stack {
                    eprint!("[ {} ]", value);
                }
                eprintln!();

                // print operation and arguments
                eprintln!("{:1$x}", chunk, _index);
            }

            use Op::*;

            match instruction {
                Constant(index) => {
                    let constant = chunk.read_constant(*index as usize);
                    self.stack.push(constant);
                }
                ConstantLong(index) => {
                    let constant = chunk.read_constant(*index as usize);
                    self.stack.push(constant);
                }
                Nil => self.stack.push(Value::Nil),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Equal => {
                    let (b, a) = (self.pop()?, self.pop()?);
                    self.stack.push(Value::Boolean(b == a));
                }
                Greater => binary_op!(>, Boolean),
                Less => binary_op!(<, Boolean),
                Add => {
                    match (self.peek(1)?, self.peek(0)?) {
                        (Value::Number(_), Value::Number(_)) => {
                            if let (Value::Number(b), Value::Number(a)) = (self.pop()?, self.pop()?)
                            {
                                self.stack.push(Value::Number(b + a));
                                continue;
                            }
                        }
                        (Value::Object(obj_a), Value::Object(obj_b)) => {
                            if let (Some(ref_a), Some(ref_b)) = (obj_a.upgrade(), obj_b.upgrade()) {
                                if let (ObjectData::r#String(a), ObjectData::r#String(b)) =
                                    (&ref_a.data, &ref_b.data)
                                {
                                    self.pop()?;
                                    self.pop()?;
                                    let new_str = ObjectData::r#String(a.to_string() + b);
                                    let alloc_obj = self.alloc(new_str);
                                    self.stack.push(Value::Object(alloc_obj));
                                    continue;
                                }
                            }
                        }
                        _ => (),
                    }
                    return Err(Error::ArgumentTypes);
                }
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
                Return => {
                    println!("{}", self.pop()?);
                    break;
                }
            }
        }

        Ok(())
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack.pop().ok_or(Error::StackEmpty)
    }

    fn peek(&self, distance: usize) -> Result<&Value, Error> {
        if self.stack.len() < distance + 1 {
            return Err(Error::StackEmpty);
        }

        self.stack
            .get(self.stack.len() - 1 - distance)
            .ok_or(Error::StackEmpty)
    }

    fn alloc(&mut self, data: ObjectData) -> Weak<Object> {
        let owner = Rc::new(Object {
            next: self.heap.take(),
            data,
        });

        let weak_ref = Rc::downgrade(&owner);
        self.heap.replace(owner);

        weak_ref
    }
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Object(Weak<Object>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::Object(obj) => write!(f, "{}", obj.upgrade().expect(Self::UPGRADE_PANIC)),
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Object(a), Self::Object(b)) => Weak::ptr_eq(a, b) || a.upgrade() == b.upgrade(),
            _ => false,
        }
    }
}

type OpResult = Result<Value, Error>;

impl Value {
    const UPGRADE_PANIC: &'static str =
        "CRITICAL: Failed to obtain reference to garbage collected object";

    fn is_falsey(&self) -> bool {
        match self {
            Self::Nil | Self::Boolean(false) => true,
            _ => false,
        }
    }

    fn negate(self) -> OpResult {
        if let Self::Number(a) = self {
            Ok(Self::Number(-a))
        } else {
            Err(Error::ArgumentTypes)
        }
    }
}

pub struct Object {
    next: Option<Rc<Self>>,
    data: ObjectData,
}

impl cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

#[derive(PartialEq)]
enum ObjectData {
    r#String(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.data {
            ObjectData::r#String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone)]
enum Op {
    Constant(u8),
    ConstantLong(u16),
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
}

impl Op {
    fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        match self {
            Self::Constant(index) => write!(f, "CONSTANT\t{}", chunk.constants[*index as usize]),
            Self::ConstantLong(index) => {
                write!(f, "CONSTANT_LONG\t{}", chunk.constants[*index as usize])
            }
            Self::Nil => write!(f, "NIL"),
            Self::True => write!(f, "TRUE"),
            Self::False => write!(f, "FALSE"),
            Self::Equal => write!(f, "EQUAL"),
            Self::Greater => write!(f, "GREATER"),
            Self::Less => write!(f, "LESS"),
            Self::Add => write!(f, "ADD"),
            Self::Subtract => write!(f, "SUBTRACT"),
            Self::Multiply => write!(f, "MULTIPLY"),
            Self::Divide => write!(f, "DIVIDE"),
            Self::Not => write!(f, "NOT"),
            Self::Negate => write!(f, "NEGATE"),
            Self::Return => write!(f, "RETURN"),
        }
    }
}
