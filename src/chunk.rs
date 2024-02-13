use crate::value::Value;
use std::num::TryFromIntError;

macro_rules! generate_opcode {
    ($n:expr;) => {};
    ($n:expr; $name:ident $(, $rest:ident)*) => {
        pub const $name: u8 = $n;

        generate_opcode!($n + 1; $($rest),*);
    };

    ($($names:ident),+) => {
        pub struct OpCode;
        impl OpCode {
            generate_opcode!(0; $($names),*);
        }
    };
}


generate_opcode!(
    CONSTANT, CONSTANT_LONG,
    TRUE, FALSE, BOOL, NUL,
    ADD, SUB, MUL, DIV, REM, UMINUS,
    BITOR, BITXOR, BITAND,
    EQUAL, LESS, GREATER, NOT,
    
    DEFINE_GLOBAL, DEFINE_GLOBAL_LONG,
    SET_GLOBAL, SET_GLOBAL_LONG, SET_LOCAL,
    GET_GLOBAL, GET_GLOBAL_LONG, GET_LOCAL,
    PRINT, POP, POP_SCOPE,

    JUMP, JUMP_IF_TRUE, JUMP_IF_FALSE, POP_JUMP_IF_TRUE, POP_JUMP_IF_FALSE,
    LOOP,
    RETURN, EOF
);

#[derive(Clone, Copy)]
struct LineEnocding {
    amount: u32,
    line: u32,
}

#[derive(Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    lines: Vec<LineEnocding>,
}

impl Chunk {
    pub const fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }
    pub fn add_opcode(&mut self, opcode: u8, line: u32) {
        self.code.push(opcode);
        self.add_line(line);
    }
    pub fn add_constant(&mut self, value: Value) -> Result<u16, TryFromIntError> {
        self.constants.push(value);
        (self.constants.len() - 1).try_into()
    }
    pub fn write_constant(&mut self, value: Value, line: u32) -> Result<(), TryFromIntError> {
        let index: u16 = self.add_constant(value)?;
        if index > (u8::MAX as u16) {
            self.add_opcode(OpCode::CONSTANT_LONG, line);
            let upper = (index >> 8) as u8;
            let lower = (index & 0xFF) as u8;
            self.add_opcode(upper, line);
            self.add_opcode(lower, line);
        }
        else {
            self.add_opcode(OpCode::CONSTANT, line);
            self.add_opcode(index as u8, line);
        }
        Ok(())
    }

    pub fn line(&self, i: usize) -> u32 {
        let mut current_line: u32 = 0;
        for l in &self.lines {
            current_line += l.amount;
            if current_line as usize > i {
                return l.line;
            }
        }
        0
    }
    pub fn add_line(&mut self, line: u32) {
        let len = self.lines.len();
        if len > 0 {
            let last = &mut self.lines[len - 1];
            if last.line == line {
                last.amount += 1;
                return;
            }
        }
        self.lines.push(LineEnocding{amount: 1, line});
    } 
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}