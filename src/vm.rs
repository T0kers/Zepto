use crate::{chunk::{Chunk, OpCode}, value::{Value, Number}};


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

macro_rules! build_comparison_op {
    ($name:ident, $oper:tt) => {
        pub unsafe fn $name(&mut self) -> Result<(), VMError> {
            let b = self.pop();
            let a = self.pop();
            match (a, b) {
                (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a $oper b)),
                (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a $oper b)),
                (Value::Num(a), Value::Bool(b)) => self.push(Value::Bool(a $oper Number::Int(b as i64))),
                (Value::Bool(a), Value::Num(b)) => self.push(Value::Bool(Number::Int(a as i64) $oper b)),
                (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
                (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            }
            Ok(())
        }
    };
}

impl<'a> VM<'a> {
    pub unsafe fn add(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a + b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64 + b as i64))),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Num(a + Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Num(Number::Int(a as i64) + b)),
            (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
            (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            _ => return self.runtime_error("Unexpected operand types."),
        }
        Ok(())
    }
    pub unsafe fn sub(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a - b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64 - b as i64))),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Num(a - Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Num(Number::Int(a as i64) - b)),
            (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
            (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            _ => return self.runtime_error("Unexpected operand types."),
        }
        Ok(())
    }
    pub unsafe fn mul(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a * b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64 * b as i64))),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Num(a * Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Num(Number::Int(a as i64) * b)),
            (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
            (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            _ => return self.runtime_error("Unexpected operand types."),
        }
        Ok(())
    }
    pub unsafe fn div(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a / b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64 / b as i64))),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Num(a / Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Num(Number::Int(a as i64) / b)),
            (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
            (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            _ => return self.runtime_error("Unexpected operand types."),
        }
        Ok(())
    }
    pub unsafe fn neg(&mut self) -> Result<(), VMError> {
        match self.pop() {
            Value::Num(a) => self.push(Value::Num(-a)),
            Value::Bool(a) => self.push(Value::Num(Number::Int(-(a as i64)))),
            Value::Nul => return self.runtime_error("Operand was nul."),
            _ => return self.runtime_error("Unexpected operand type."),
        }
        Ok(())
    }
    pub unsafe fn equal(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a == b)),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Bool(a == Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Bool(Number::Int(a as i64) == b)),
            (Value::Nul, Value::Nul) => self.push(Value::Bool(true)),
            (Value::Nul, _) => self.push(Value::Bool(false)),
            (_, Value::Nul) => self.push(Value::Bool(false)),
        }
        Ok(())
    }
    build_comparison_op!(less, <);
    build_comparison_op!(greater, >);
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
                    OpCode::ADD => self.add()?,
                    OpCode::SUB => self.sub()?,
                    OpCode::MUL => self.mul()?,
                    OpCode::DIV => self.div()?,
                    OpCode::UMINUS => self.neg()?,
                    OpCode::EQUAL => self.equal()?,
                    OpCode::LESS => self.less()?,
                    OpCode::GREATER => self.greater()?,
                    OpCode::NOT => {
                        let result = !self.pop().as_bool();
                        self.push(Value::Bool(result));
                    }
                    
                    OpCode::RETURN => {println!("Returned: {}", self.pop())},
                    OpCode::EOF => return Ok(()),
                    _ => {
                        self.runtime_error("Found unknown opcode.")?
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
    unsafe fn peek(&mut self, distance: usize) -> Value {
        *self.stack_top.sub(1 + distance)
    }

    pub unsafe fn runtime_error(&mut self, msg: &str) -> Result<(), VMError> { // TODO: this function should maybe be turned into a macro.
        eprintln!("{}", msg);
        eprintln!("[Line {}] in script.", self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize));
        self.reset_stack();
        Err(VMError::RuntimeError)
    }
}
