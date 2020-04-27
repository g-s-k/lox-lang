use std::fmt;

mod compiler;
mod scanner;
mod token;

use {
    compiler::Compiler,
    scanner::Scanner,
    token::{Token, TokenType},
};

pub struct VM {
    stack: Vec<Value>,
}

impl Default for VM {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl VM {
    pub fn interpret(&mut self, source: String) -> Result<(), Error> {
        let mut compiler = Compiler::new(&source);
        let chunk = compiler.compile()?;
        self.run(chunk)
    }

    fn run(&mut self, chunk: Chunk) -> Result<(), Error> {
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

            match instruction {
                Op::Constant(index) => {
                    let constant = chunk.read_constant(*index as usize);
                    self.stack.push(constant);
                }
                Op::ConstantLong(index) => {
                    let constant = chunk.read_constant(*index as usize);
                    self.stack.push(constant);
                }
                Op::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(a.plus(b)?);
                }
                Op::Subtract => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(a.minus(b)?);
                }
                Op::Multiply => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(a.times(b)?);
                }
                Op::Divide => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(a.divided_by(b)?);
                }
                Op::Negate => {
                    let value = self.pop()?;
                    self.stack.push(value.negate()?);
                }
                Op::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    break;
                }
            }
        }

        Ok(())
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack.pop().ok_or(Error::StackEmpty)
    }
}

pub enum ErrorCategory {
    Compilation,
    Runtime,
}

#[derive(Debug)]
pub enum Error {
    StackEmpty,
}

impl Error {
    pub fn category(&self) -> ErrorCategory {
        match self {
            Self::StackEmpty => ErrorCategory::Runtime,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lox error")
    }
}

impl std::error::Error for Error {}

#[derive(Clone)]
pub struct Value(pub f64);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Value {
    fn plus(mut self, other: Self) -> Result<Self, Error> {
        self.0 += other.0;
        Ok(self)
    }

    fn minus(mut self, other: Self) -> Result<Self, Error> {
        self.0 -= other.0;
        Ok(self)
    }

    fn times(mut self, other: Self) -> Result<Self, Error> {
        self.0 *= other.0;
        Ok(self)
    }

    fn divided_by(mut self, other: Self) -> Result<Self, Error> {
        self.0 /= other.0;
        Ok(self)
    }

    fn negate(mut self) -> Result<Self, Error> {
        self.0 *= -1.0;
        Ok(self)
    }
}

#[derive(Clone)]
pub enum Op {
    Constant(u8),
    ConstantLong(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
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
            Self::Add => write!(f, "ADD"),
            Self::Subtract => write!(f, "SUBTRACT"),
            Self::Multiply => write!(f, "MULTIPLY"),
            Self::Divide => write!(f, "DIVIDE"),
            Self::Negate => write!(f, "NEGATE"),
            Self::Return => write!(f, "RETURN"),
        }
    }
}

#[derive(Clone)]
struct LineRecord {
    line: usize,
    count: usize,
}

pub struct Chunk {
    name: String,
    constants: Vec<Value>,
    code: Vec<Op>,
    lines: Vec<LineRecord>,
}

impl Chunk {
    pub fn new<T: ToString>(name: T) -> Self {
        Self {
            name: name.to_string(),
            constants: Vec::new(),
            code: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, op: Op, line: usize) {
        self.code.push(op);

        match self.lines.last() {
            None => self.push_line_record(line),
            Some(rec) if rec.line < line => self.push_line_record(line),
            Some(rec) if rec.line == line => {
                // a little weird looking, but seems like the idiomatic way to update an Option's
                // wrapped value in place
                for last in self.lines.last_mut().iter_mut() {
                    last.count += 1;
                }
            }
            _ => unreachable!("Line number stack should not go backward"),
        }
    }

    fn push_line_record(&mut self, line: usize) {
        self.lines.push(LineRecord { line, count: 1 });
    }

    pub fn write_constant(&mut self, value: Value, line: usize) -> usize {
        let idx = self.add_constant(value);

        self.write(
            if idx > 255 {
                Op::ConstantLong(idx as u16)
            } else {
                Op::Constant(idx as u8)
            },
            line,
        );

        idx
    }

    /// Adds the value to the Chunk's constant table and returns its index
    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn disassemble_instruction<W: fmt::Write>(&self, index: usize, f: &mut W) -> fmt::Result {
        write!(f, "{:04} ", index)?;

        let (line, is_first) = self.find_line(index);

        if is_first {
            write!(f, "{:4} ", line)?;
        } else {
            write!(f, "   | ")?;
        }

        self.code[index].disassemble(self, f)
    }

    fn find_line(&self, instruction_index: usize) -> (usize, bool) {
        let mut line_num = 1;
        let mut is_first = true;

        let mut idx_counter = 0;
        'outer: for LineRecord { line, count } in &self.lines {
            line_num = *line;
            is_first = true;

            for _ in 0..*count {
                if idx_counter == instruction_index {
                    break 'outer;
                }

                idx_counter += 1;
                is_first = false;
            }
        }

        (line_num, is_first)
    }

    fn read_constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "== {} ==", self.name)?;

        let mut lines = self.lines.iter();
        let mut line;
        let mut line_count = 0; // immediately hooks into advancement below
        for (idx, op) in self.code.iter().enumerate() {
            write!(f, "{:04} ", idx)?;

            if line_count == 0 {
                // advance
                if let Some(LineRecord { line: l, count: c }) = lines.next() {
                    line = l;
                    line_count = *c;
                } else {
                    unreachable!("Should not run out of lines before running out of instructions.");
                }

                write!(f, "{:4} ", line)?;
            } else {
                write!(f, "   | ")?;
            }

            line_count -= 1;

            op.disassemble(self, f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

/// A terrible, shameful, but ultimately necessary thing to do
impl fmt::LowerHex for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let index = f.width().unwrap_or_default();

        self.disassemble_instruction(index, f)
    }
}
