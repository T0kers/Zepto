use crate::value::*;

pub struct OpCode;
impl OpCode {
    pub const CONSTANT: u8 = 0x00;
    pub const RETURN: u8 = 0x01;
}

struct LineEnocding {
    amount: u32,
    line: u32,
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    lines: Vec<LineEnocding>,
    index: usize,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
            index: 0,
        }
    }
    pub fn add_opcode(&mut self, opcode: u8, line: u32) {
        self.code.push(opcode);
        self.add_line(line);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1).try_into().unwrap()
    }
    pub fn write_constant(&mut self, value: Value, line: u32) {
        self.add_opcode(OpCode::CONSTANT, line);
        let index = self.add_constant(value);
        self.add_opcode(index, line);
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

impl Iterator for Chunk {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.code.get(self.index).cloned();
        self.index += 1;
        result
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}