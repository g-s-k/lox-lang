use std::fmt;

use super::Chunk;

#[derive(Copy, Clone, Debug)]
pub(crate) enum Op {
    // constants
    Constant(u8),
    ConstantLong(u16),

    // immediates
    Nil,
    True,
    False,

    // actions
    Pop,
    PopN(u8),
    GetGlobal(u8),
    GetGlobalLong(u16),
    DefineGlobal(u8),
    DefineGlobalLong(u16),
    SetGlobal(u8),
    SetGlobalLong(u16),
    GetLocal(u8),
    GetLocalLong(u16),
    SetLocal(u8),
    SetLocalLong(u16),

    // operators
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,

    // control flow
    Print,
    Jump(u16),
    JumpIfFalse(u16),
    Loop(u16),
    Call(u8),
    Return,
}

impl Op {
    pub fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        macro_rules! fmt {
            ($c:expr) => {
                write!(f, "{:16} {}", self, $c)
            };
        }

        match self {
            Op::Constant(i) | Op::GetGlobal(i) | Op::DefineGlobal(i) | Op::SetGlobal(i) => {
                fmt!(chunk.constants[*i as usize])
            }
            Op::ConstantLong(i)
            | Op::GetGlobalLong(i)
            | Op::DefineGlobalLong(i)
            | Op::SetGlobalLong(i) => fmt!(chunk.constants[*i as usize]),

            Op::GetLocalLong(c)
            | Op::SetLocalLong(c)
            | Op::Jump(c)
            | Op::JumpIfFalse(c)
            | Op::Loop(c) => fmt!(c),
            Op::GetLocal(c) | Op::SetLocal(c) | Op::PopN(c) | Op::Call(c) => fmt!(c),

            _ => write!(f, "{}", self),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:width$}",
            match self {
                Op::Constant(_) => "CONSTANT",
                Op::ConstantLong(_) => "CONSTANT_LONG",

                Op::Nil => "NIL",
                Op::True => "TRUE",
                Op::False => "FALSE",

                Op::Pop => "POP",
                Op::PopN(_) => "POP_N",
                Op::GetGlobal(_) => "GET_GLOBAL",
                Op::GetGlobalLong(_) => "GET_GLOBAL_LONG",
                Op::DefineGlobal(_) => "DEF_GLOBAL",
                Op::DefineGlobalLong(_) => "DEF_GLOBAL_LONG",
                Op::SetGlobal(_) => "SET_GLOBAL",
                Op::SetGlobalLong(_) => "SET_GLOBAL_LONG",
                Op::GetLocal(_) => "GET_LOCAL",
                Op::GetLocalLong(_) => "GET_LOCAL_LONG",
                Op::SetLocal(_) => "SET_LOCAL",
                Op::SetLocalLong(_) => "SET_LOCAL_LONG",

                Op::Equal => "EQUAL",
                Op::Greater => "GREATER",
                Op::Less => "LESS",
                Op::Add => "ADD",
                Op::Subtract => "SUBTRACT",
                Op::Multiply => "MULTIPLY",
                Op::Divide => "DIVIDE",
                Op::Not => "NOT",
                Op::Negate => "NEGATE",

                Op::Print => "PRINT",
                Op::Jump(_) => "JMP",
                Op::JumpIfFalse(_) => "JMP_FALSE",
                Op::Loop(_) => "LOOP",
                Op::Call(_) => "CALL",
                Op::Return => "RETURN",
            },
            width = f.width().unwrap_or_default(),
        )
    }
}
