use std::fmt;

use super::Chunk;

#[derive(Clone, Debug)]
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
    Return,
}

impl Op {
    pub fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        use Op::*;

        macro_rules! constant_instr {
            ($name:literal, $index:expr) => {{
                write!(f, concat!($name, "\t{}"), chunk.constants[*$index as usize])
            }};
        }

        match self {
            Constant(index) => constant_instr!("CONSTANT", index),
            ConstantLong(index) => constant_instr!("CONSTANT_LONG", index),

            Nil => write!(f, "NIL"),
            True => write!(f, "TRUE"),
            False => write!(f, "FALSE"),

            Pop => write!(f, "POP"),
            PopN(count) => write!(f, "POP_N\t{}", count),
            GetGlobal(index) => constant_instr!("GET_GLOBAL", index),
            GetGlobalLong(index) => constant_instr!("GET_GLOBAL_LONG", index),
            DefineGlobal(index) => constant_instr!("DEF_GLOBAL", index),
            DefineGlobalLong(index) => constant_instr!("DEF_GLOBAL_LONG", index),
            SetGlobal(index) => constant_instr!("SET_GLOBAL", index),
            SetGlobalLong(index) => constant_instr!("SET_GLOBAL_LONG", index),
            GetLocal(index) => write!(f, "GET_LOCAL\t{}", index),
            GetLocalLong(index) => write!(f, "GET_LOCAL_LONG\t{}", index),
            SetLocal(index) => write!(f, "SET_LOCAL\t{}", index),
            SetLocalLong(index) => write!(f, "SET_LOCAL_LONG\t{}", index),

            Equal => write!(f, "EQUAL"),
            Greater => write!(f, "GREATER"),
            Less => write!(f, "LESS"),
            Add => write!(f, "ADD"),
            Subtract => write!(f, "SUBTRACT"),
            Multiply => write!(f, "MULTIPLY"),
            Divide => write!(f, "DIVIDE"),
            Not => write!(f, "NOT"),
            Negate => write!(f, "NEGATE"),

            Print => write!(f, "PRINT"),
            Jump(to) => write!(f, "JMP\t{:04}", to),
            JumpIfFalse(to) => write!(f, "JMP_FALSE\t{:04}", to),
            Loop(to) => write!(f, "LOOP\t{:04}", to),
            Return => write!(f, "RETURN"),
        }
    }
}
