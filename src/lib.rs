use std::fmt;

pub struct Value(pub f64);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum Op {
    Constant(u8),
    ConstantLong(u16),
    Return,
}

impl Op {
    fn disassemble(&self, chunk: &Chunk, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Constant(index) => write!(f, "CONSTANT\t{}", chunk.constants[*index as usize]),
            Self::ConstantLong(index) => {
                write!(f, "CONSTANT_LONG\t{}", chunk.constants[*index as usize])
            }
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

impl fmt::Debug for Chunk {
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
}
