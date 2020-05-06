use std::fmt;

use super::{Chunk, Upvalue};

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
    GetUpvalue(u8),
    GetUpvalueLong(u16),
    SetUpvalue(u8),
    SetUpvalueLong(u16),

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
    Closure(u8, Box<[Upvalue]>),
    ClosureLong(u16, Box<[Upvalue]>),
    CloseUpvalue,
    Return,
}

impl Op {
    pub fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        macro_rules! fmt {
            ($c:expr) => {
                write!(f, "{:16} {}", self, $c)
            };
        }

        macro_rules! closure {
            ($i: expr, $u: expr) => {{
                fmt!(chunk.constants[*$i as usize])?;

                write!(f, "\t {{")?;
                let mut first = true;
                for Upvalue { index, is_local } in $u.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }

                    if *is_local {
                        write!(f, "local")?;
                    } else {
                        write!(f, "upvalue")?;
                    }

                    write!(f, ": {}", index)?;
                }
                write!(f, "}}")
            }};
        }

        match self {
            Op::Closure(i, u_vals) => closure!(i, u_vals),
            Op::ClosureLong(i, u_vals) => closure!(i, u_vals),

            Op::Constant(i) | Op::GetGlobal(i) | Op::DefineGlobal(i) | Op::SetGlobal(i) => {
                fmt!(chunk.constants[*i as usize])
            }

            Op::ConstantLong(i)
            | Op::GetGlobalLong(i)
            | Op::DefineGlobalLong(i)
            | Op::SetGlobalLong(i) => fmt!(chunk.constants[*i as usize]),

            Op::GetLocalLong(c)
            | Op::SetLocalLong(c)
            | Op::GetUpvalueLong(c)
            | Op::SetUpvalueLong(c)
            | Op::Jump(c)
            | Op::JumpIfFalse(c)
            | Op::Loop(c) => fmt!(c),

            Op::GetLocal(c)
            | Op::SetLocal(c)
            | Op::GetUpvalue(c)
            | Op::SetUpvalue(c)
            | Op::Call(c) => fmt!(c),

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
                Op::GetUpvalue(_) => "GET_UPVALUE",
                Op::GetUpvalueLong(_) => "GET_UPVALUE_LONG",
                Op::SetUpvalue(_) => "SET_UPVALUE",
                Op::SetUpvalueLong(_) => "SET_UPVALUE_LONG",

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
                Op::Closure(_, _) => "CLOSURE",
                Op::ClosureLong(_, _) => "CLOSURE_LONG",
                Op::CloseUpvalue => "CLOSE_UPVALUE",
                Op::Return => "RETURN",
            },
            width = f.width().unwrap_or_default(),
        )
    }
}
