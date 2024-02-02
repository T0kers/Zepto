use crate::{chunk::Chunk, debug, chunk::OpCode, value::Value};



pub enum VMError {
    CompileError,
    RuntimeError,
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: *const u8,
    stack: [Value; 256],
    stack_top: *mut Value,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        let ip = chunk.code.as_ptr();
        let mut stack = [Value::Nul; 256];
        Self {
            chunk,
            ip,
            stack,
            stack_top: stack.as_mut_ptr(),
        }
    }
    fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
    }
    unsafe fn read_byte(&mut self) -> u8 {
        let instruction = *self.ip;
        self.ip = self.ip.add(1);
        instruction
    }
    
    pub fn run(&mut self) -> Result<(), VMError> {
        macro_rules! binary_op {
            ($oper:tt) => {
                {
                    let b;
                    match self.pop() {
                        Value::Int(num) => b = num,
                        Value::Bool(boolean) => b = boolean as i64,
                        _ => {
                            self.runtime_error("Operand must be an integer or boolean.");
                            return Err(VMError::RuntimeError)
                        },
                    }
                    let a;
                    match self.pop() {
                        Value::Int(num) => a = num,
                        Value::Bool(boolean) => a = boolean as i64,
                        _ => {
                            self.runtime_error("Operand must be an integer or boolean.");
                            return Err(VMError::RuntimeError)
                        },
                    }
                    self.push(Value::Int(a $oper b));
                }
            };
        }
        self.ip = self.chunk.code.as_ptr();
        self.stack_top = self.stack.as_mut_ptr();
        unsafe {
            loop {
                #[cfg(feature = "debug_code")]
                {
                    print!("        [ ");
                    let stack_size = self.stack_top.offset_from(self.stack.as_ptr()) as usize;
                    for index in 0..stack_size {
                        print!("{} ", self.stack[index]);
                    }
                    println!("]");
                    debug::disassemble_instruction(&self.chunk, self.ip.offset_from(self.chunk.code.as_ptr()) as usize);
                }
                
                match self.read_byte() {
                    OpCode::CONSTANT => {
                        let index = self.read_byte() as usize;
                        let constant = self.chunk.constants[index];
                        self.push(constant);
                    }
                    OpCode::CONSTANT_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        let constant = self.chunk.constants[index];
                        self.push(constant);
                    }
                    OpCode::TRUE => self.push(Value::Bool(true)),
                    OpCode::FALSE => self.push(Value::Bool(false)),
                    OpCode::NUL => self.push(Value::Nul),
                    OpCode::ADD => binary_op!(+),
                    OpCode::SUB => binary_op!(-),
                    OpCode::MUL => binary_op!(*),
                    OpCode::DIV => binary_op!(/),
                    OpCode::NEG => {
                        match self.pop() { // TODO: maybe do not pop here because it might effect garbage collection. https://craftinginterpreters.com/types-of-values.html
                            Value::Int(n) => self.push(Value::Int(-n)),
                            Value::Bool(b) => self.push(Value::Int(-(b as i64))),
                            Value::Nul => return Err(VMError::RuntimeError),
                        }
                    }
                    OpCode::RETURN => {println!("Value: {:?}", self.pop())},
                    OpCode::EOF => return Ok(()),
                    _ => {
                        self.runtime_error("Operand must be a number.");
                        return Err(VMError::RuntimeError);
                    },
                }
            }
        }
    }
    unsafe fn push(&mut self, value: Value) {
        *self.stack_top = value;
        self.stack_top = self.stack_top.add(1);
    }
    unsafe fn pop(&mut self) -> Value {
        self.stack_top = self.stack_top.sub(1);
        *self.stack_top
    }
    unsafe fn peek(&mut self, distance: usize) {
        self.stack_top.sub(1 + distance);
    }
    unsafe fn runtime_error(&mut self, msg: &str) { // this function should maybe be turned into a macro.
        eprintln!("{}", msg);
        eprintln!("[Line {}] in script.", self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize));
        self.reset_stack();
    }
}
