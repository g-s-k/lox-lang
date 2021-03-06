use std::fmt;

use crate::{Chunk, Upvalue};

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
    GetProperty(u8),
    GetPropertyLong(u16),
    SetProperty(u8),
    SetPropertyLong(u16),
    GetSuper(u8),
    GetSuperLong(u16),

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
    Invoke(u8, u8),
    InvokeLong(u16, u8),
    SuperInvoke(u8, u8),
    SuperInvokeLong(u16, u8),
    Closure(u8, Box<[Upvalue]>),
    ClosureLong(u16, Box<[Upvalue]>),
    CloseUpvalue,
    Return,
    Class(u8),
    ClassLong(u16),
    Inherit,
    Method(u8),
    MethodLong(u16),
}

impl Op {
    pub fn disassemble<W: fmt::Write>(&self, chunk: &Chunk, f: &mut W) -> fmt::Result {
        macro_rules! fmt {
            ($c: expr) => {
                write!(f, "{:16} {}", self, $c)
            };
            ($c1: expr, $c2: expr) => {{
                fmt!($c1)?;
                write!(f, " ({})", $c2)
            }};
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

            Op::Invoke(i, c) | Op::SuperInvoke(i, c) => fmt!(chunk.constants[*i as usize], c),
            Op::InvokeLong(i, c) | Op::SuperInvokeLong(i, c) => {
                fmt!(chunk.constants[*i as usize], c)
            }

            Op::Constant(i)
            | Op::GetGlobal(i)
            | Op::DefineGlobal(i)
            | Op::SetGlobal(i)
            | Op::Class(i)
            | Op::GetProperty(i)
            | Op::SetProperty(i)
            | Op::GetSuper(i)
            | Op::Method(i) => fmt!(chunk.constants[*i as usize]),

            Op::ConstantLong(i)
            | Op::GetGlobalLong(i)
            | Op::DefineGlobalLong(i)
            | Op::SetGlobalLong(i)
            | Op::ClassLong(i)
            | Op::GetPropertyLong(i)
            | Op::SetPropertyLong(i)
            | Op::GetSuperLong(i)
            | Op::MethodLong(i) => fmt!(chunk.constants[*i as usize]),

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
                Op::GetProperty(_) => "GET_PROPERTY",
                Op::GetPropertyLong(_) => "GET_PROPERTY_LONG",
                Op::SetProperty(_) => "SET_PROPERTY",
                Op::SetPropertyLong(_) => "SET_PROPERTY_LONG",
                Op::GetSuper(_) => "GET_SUPER",
                Op::GetSuperLong(_) => "GET_SUPER_LONG",

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
                Op::Invoke(_, _) => "INVOKE",
                Op::InvokeLong(_, _) => "INVOKE_LONG",
                Op::SuperInvoke(_, _) => "SUPER_INVOKE",
                Op::SuperInvokeLong(_, _) => "SUPER_INVOKE_LONG",
                Op::Closure(_, _) => "CLOSURE",
                Op::ClosureLong(_, _) => "CLOSURE_LONG",
                Op::CloseUpvalue => "CLOSE_UPVALUE",
                Op::Return => "RETURN",
                Op::Class(_) => "CLASS",
                Op::ClassLong(_) => "CLASS_LONG",
                Op::Inherit => "INHERIT",
                Op::Method(_) => "METHOD",
                Op::MethodLong(_) => "METHOD_LONG",
            },
            width = f.width().unwrap_or_default(),
        )
    }
}
