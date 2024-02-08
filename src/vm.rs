use crate::{chunk::{Chunk, OpCode}, value::{Value, Number}, object::Object};


pub enum VMError {
    CompileError,
    RuntimeError,
}

pub struct VM<'a> {
    chunk: &'a Chunk<'a>,
    ip: *const u8,
    stack: [Value<'a>; 256],
    stack_top: *mut Value<'a>,
    globals: Box<[Option<Value<'a>>]>
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
                _ => return self.runtime_error("Unexpected operand type."),
            }
            Ok(())
        }
    };
}

macro_rules! build_int_op {
    ($name:ident, $oper:tt) => {
        pub unsafe fn $name(&mut self) -> Result<(), VMError> {
            let b = self.pop();
            let a = self.pop();
            let result = match (a, b) {
                (Value::Num(a), Value::Num(b)) => a $oper b,
                (Value::Bool(a), Value::Bool(b)) => Number::Int(a as i64) $oper Number::Int(b as i64),
                (Value::Num(a), Value::Bool(b)) => a $oper Number::Int(b as i64),
                (Value::Bool(a), Value::Num(b)) => Number::Int(a as i64) $oper b,
                (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
                (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
                _ => return self.runtime_error("Unexpected operand types."),
            };
            if let Some(n) = result {
                self.push(Value::Num(n));
                Ok(())
            }
            else {
                self.runtime_error("Calculation of remainder failed.")
            }
        }
    };
}

impl<'a> VM<'a> {
    pub unsafe fn add(&mut self) -> Result<(), VMError> {
        let b = self.pop();
        let a = self.pop();
        match (a, b) {
            (Value::Obj(a), Value::Obj(b)) => match (*a, *b) {
                (Object::Str(a), Object::Str(b)) => self.push(Value::Obj(Box::new(Object::Str(a + &b)))),
                _ => return self.runtime_error("Unexpected operand types."),
            },
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a + b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64) + Number::Int(b as i64))),
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
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64) - Number::Int(b as i64))),
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
            (Value::Obj(a), Value::Num(b)) => match (*a, b) {
                (Object::Str(a), Number::Int(b)) => self.push(Value::Obj(Box::new(Object::Str(a.repeat(b as usize))))),
                _ => return self.runtime_error("Unexpected operand types."),
            }
            (Value::Num(a), Value::Obj(b)) => match (a, *b) {
                (Number::Int(a), Object::Str(b)) => self.push(Value::Obj(Box::new(Object::Str(b.repeat(a as usize))))),
                _ => return self.runtime_error("Unexpected operand types."),
            }
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a * b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64) * Number::Int(b as i64))),
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
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Num(Number::Int(a as i64) / Number::Int(b as i64))),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Num(a / Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Num(Number::Int(a as i64) / b)),
            (Value::Nul, _) => return self.runtime_error("Left operand was nul."),
            (_, Value::Nul) => return self.runtime_error("Right operand was nul."),
            _ => return self.runtime_error("Unexpected operand types."),
        }
        Ok(())
    }
    
    build_int_op!(rem, %);
    build_int_op!(bitor, |);
    build_int_op!(bitxor, ^);
    build_int_op!(bitand, &);

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
            (Value::Obj(a), Value::Obj(b)) => self.push(Value::Bool(a == b)),
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a == b)),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Bool(a == Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Bool(Number::Int(a as i64) == b)),
            (Value::Nul, Value::Nul) => self.push(Value::Bool(true)),
            (Value::Nul, _) => self.push(Value::Bool(false)),
            (_, Value::Nul) => self.push(Value::Bool(false)),
            _ => return self.runtime_error("Unexpected operand type."),
        }
        Ok(())
    }
    build_comparison_op!(less, <);
    build_comparison_op!(greater, >);
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk, global_count: u16) -> Self {
        let ip = chunk.code.as_ptr();
        const ARRAY_REPEAT_VALUE: Value<'_> = Value::Nul;
        let mut stack = [ARRAY_REPEAT_VALUE; 256];
        Self {
            chunk,
            ip,
            stack_top: stack.as_mut_ptr(),
            stack,
            globals: vec![None; global_count as usize].into_boxed_slice(),
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
                    use crate::debug;
                    print!("        [ ");
                    let stack_size = self.stack_top.offset_from(self.stack.as_ptr()) as usize;
                    for index in 0..stack_size {
                        print!("{} ", match &self.stack[index] {
                            Value::Obj(o) => o.debug_string(),
                            other => other.to_string(),
                        });
                    }
                    println!("]");
                    debug::disassemble_instruction(&self.chunk, self.ip.offset_from(self.chunk.code.as_ptr()) as usize);
                }
                
                match self.read_byte() {
                    OpCode::CONSTANT => {
                        let index = self.read_byte() as usize;
                        let constant = self.chunk.constants[index].clone();
                        self.push(constant);
                    }
                    OpCode::CONSTANT_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        let constant = self.chunk.constants[index].clone();
                        self.push(constant);
                    }
                    OpCode::TRUE => self.push(Value::Bool(true)),
                    OpCode::FALSE => self.push(Value::Bool(false)),
                    OpCode::NUL => self.push(Value::Nul),
                    OpCode::ADD => self.add()?,
                    OpCode::SUB => self.sub()?,
                    OpCode::MUL => self.mul()?,
                    OpCode::DIV => self.div()?,
                    OpCode::REM => self.rem()?,
                    OpCode::UMINUS => self.neg()?,
                    OpCode::BITOR => self.bitor()?,
                    OpCode::BITXOR => self.bitxor()?,
                    OpCode::BITAND => self.bitand()?,
                    OpCode::EQUAL => self.equal()?,
                    OpCode::LESS => self.less()?,
                    OpCode::GREATER => self.greater()?,
                    OpCode::NOT => {
                        let result = !self.pop().as_bool();
                        self.push(Value::Bool(result));
                    }
                    OpCode::DEFINE_GLOBAL => {
                        let index = self.read_byte() as usize;
                        self.globals[index] = Some(self.pop()); // TODO: MAYBE change this because of garbage collection.
                    }
                    OpCode::DEFINE_GLOBAL_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        self.globals[index] = Some(self.pop()); // TODO: MAYBE change this because of garbage collection.
                    },
                    OpCode::SET_GLOBAL => {
                        let index = self.read_byte() as usize;
                        match self.globals[index] {
                            Some(_) => {
                                self.globals[index] = Some(self.peek(0)); // TODO: MAYBE change this because of garbage collection.
                            }
                            None => {
                                self.runtime_error("Cannot assign to undefined variable.")?; // TODO: show variable name in message.
                            }
                        }
                    }
                    OpCode::SET_GLOBAL_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        match self.globals[index] {
                            Some(_) => {
                                self.globals[index] = Some(self.peek(0)); // TODO: MAYBE change this because of garbage collection.
                            }
                            None => {
                                self.runtime_error("Cannot assign to undefined variable.")?; // TODO: show variable name in message.
                            }
                        }
                    }
                    OpCode::GET_GLOBAL => {
                        let index = self.read_byte() as usize;
                        if let Some(value) = &self.globals[index] {
                            self.push(value.clone());
                        } else {
                            self.runtime_error("Undefined variable.")? // TODO: show variable name in message.
                        }
                    }
                    OpCode::GET_GLOBAL_LONG => {
                        let mut index = self.read_byte() as usize;
                        index = index << 8 | self.read_byte() as usize;
                        if let Some(value) = &self.globals[index] {
                            self.push(value.clone());
                        } else {
                            self.runtime_error("Undefined variable.")? // TODO: show variable name in message.
                        }
                    }
                    OpCode::PRINT => println!("{}", self.pop()),
                    OpCode::POP => {self.pop();},
                    OpCode::POP_SCOPE => for _ in 0..self.read_byte() {
                        self.pop();
                    },
                    OpCode::RETURN => {println!("Returned: {}", self.pop())},
                    OpCode::EOF => return Ok(()),
                    _ => {
                        self.runtime_error("Found unknown opcode.")?
                    },
                }
            }
        }
    }

    unsafe fn push(&mut self, value: Value<'a>) {
        *self.stack_top = value;
        self.stack_top = self.stack_top.add(1);
    }
    unsafe fn pop(&mut self) -> Value<'a> {
        self.stack_top = self.stack_top.sub(1);
        (*self.stack_top).clone()
    }
    unsafe fn peek(&mut self, distance: usize) -> Value<'a> {
        (*self.stack_top.sub(1 + distance)).clone()
    }

    pub unsafe fn runtime_error(&mut self, msg: &str) -> Result<(), VMError> { // TODO: this function should maybe be turned into a macro.
        eprintln!("{}", msg);
        eprintln!("[Line {}] in script.", self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize));
        self.reset_stack();
        Err(VMError::RuntimeError)
    }
}
