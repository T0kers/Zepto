use crate::{chunk::Chunk, debug, value::value_to_string, OpCode, value::Value};

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub struct VM {
    pub chunk: Chunk,
    ip: *const u8,
    stack: [Value; 256],
}

impl VM {
    pub fn new() -> Self {
        let chunk = Chunk::new();
        let ip = chunk.code.as_ptr();
        Self {
            chunk,
            ip,
            stack: [Value::Nul; 256],
        }
    }
    unsafe fn read_byte(&mut self) -> u8 {
        let instruction = *self.ip;
        self.ip = self.ip.add(1);
        instruction
    }
    pub fn interpret(&mut self) -> Result<(), VMError> {
        self.ip = self.chunk.code.as_ptr();
        unsafe {
            loop {
                debug::disassemble_instruction(&self.chunk, self.ip.offset_from(self.chunk.code.as_ptr()) as usize);
                match self.read_byte() {
                    OpCode::CONSTANT => {
                        let index = self.read_byte() as usize;
                        let constant = self.chunk.constants[index];
                        println!("{}", value_to_string(constant));
                    }
                    OpCode::CONSTANT_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        let constant = self.chunk.constants[index];
                        println!("{}", value_to_string(constant));
                    }
                    OpCode::RETURN => continue,
                    OpCode::EOF => return Ok(()),
                    _ => return Err(VMError::RuntimeError),
                }
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}