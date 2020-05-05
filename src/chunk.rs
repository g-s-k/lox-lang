use std::{convert::TryInto, fmt};

use super::{CompileErrorType, Op, Value};

#[derive(Clone, Debug)]
struct LineRecord {
    line: usize,
    count: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct Chunk {
    name: String,
    pub constants: Vec<Value>,
    pub code: Vec<Op>,
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

    pub fn write(&mut self, op: Op, line: usize) -> usize {
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

        self.code.len() - 1
    }

    fn push_line_record(&mut self, line: usize) {
        self.lines.push(LineRecord { line, count: 1 });
    }

    /// Adds the value to the Chunk's constant table and returns its index
    pub fn add_constant(&mut self, value: Value) -> usize {
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

    pub fn find_line(&self, instruction_index: usize) -> (usize, bool) {
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

    pub fn read_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    pub fn patch_jump(&mut self, index: usize) -> Result<(), CompileErrorType> {
        let distance = self.code.len() - index - 1;

        let distance = match distance.try_into() {
            Err(e) => {
                log::error!("{}", e);
                return Err(CompileErrorType::JumpTooLarge(distance));
            }
            Ok(d) => d,
        };

        match self.code.get_mut(index) {
            Some(Op::Jump(ref mut old)) | Some(Op::JumpIfFalse(ref mut old)) => *old = distance,
            other => unreachable!(
                "attempted to patch jump instruction with wrong index ({:05x} => {:?})",
                index, other,
            ),
        }

        Ok(())
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
