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

macro_rules! build_op {
    ($name:ident, $checked:ident, $op:tt) => {
        unsafe fn $name(&mut self) -> Result<(), VMError> {
            let b: i64 = match self.pop() {
                Value::Bool(b) => b as i64,
                Value::Float(b) => {
                    let a: f64 = match self.pop() {
                        Value::Bool(a) => a as u8 as f64,
                        Value::Int(a) => a as f64,
                        Value::Float(a) => a,
                        Value::Nul => return self.runtime_error("Operand can't be nul."),
                    };
                    self.push(Value::Float(a $op b));
                    return Ok(());
                },
                Value::Int(b) => b,
                Value::Nul => return self.runtime_error("Operand can't be nul."),
            };
            let a: i64 = match self.pop() {
                Value::Bool(a) => a as i64,
                Value::Int(a) => a as i64,
                Value::Float(a) => {
                    self.push(Value::Float(a $op b as f64));
                    return Ok(());
                },
                Value::Nul => return self.runtime_error("Operand can't be nul."),
            };
            match a.$checked(b) {
                Some(n) => self.push(Value::Int(n)),
                None => self.push(Value::Float(a as f64 $op b as f64)),
            }
            Ok(())
        }
    };
}

macro_rules! build_compare {
    ($name:ident, $oper:tt) => {
        unsafe fn $name(&mut self) -> Result<(), VMError> {
            let b: f64 = match self.pop() {
                Value::Int(num) => num as f64,
                Value::Float(num) => num,
                Value::Bool(boolean) => boolean as i64 as f64,
                Value::Nul => return self.runtime_error("Operands must be an integer or boolean."),
            };
            let a: f64 = match self.pop() {
                Value::Int(num) => num as f64,
                Value::Float(num) => num,
                Value::Bool(boolean) => boolean as i64 as f64,
                Value::Nul => return self.runtime_error("Operands must be an integer or boolean."),
            };
            self.push(Value::Bool(a $oper b));
            Ok(())
        }
    };
}

impl<'a> VM<'a> {
    build_op!(add, checked_add, +);
    build_op!(sub, checked_sub, -);
    build_op!(mul, checked_mul, *);
    build_op!(div, checked_div, /);
    build_compare!(equal, ==);
    build_compare!(less, <);
    build_compare!(greater, >);

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
                    OpCode::UMINUS => {
                        match self.pop() { // TODO: maybe do not pop here because it might effect garbage collection. https://craftinginterpreters.com/types-of-values.html
                            Value::Int(i) => self.push(Value::Int(-i)),
                            Value::Float(n) => self.push(Value::Float(-n)),
                            Value::Bool(b) => self.push(Value::Int(-(b as i64))),
                            Value::Nul => return Err(VMError::RuntimeError),
                        }
                    }
                    OpCode::EQUAL => self.equal()?,
                    OpCode::LESS => self.less()?,
                    OpCode::GREATER => self.greater()?,
                    OpCode::NOT => {
                        let result = !self.pop().to_bool();
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
    unsafe fn peek(&mut self, distance: usize) {
        self.stack_top.sub(1 + distance);
    }
    unsafe fn runtime_error(&mut self, msg: &str) -> Result<(), VMError> { // this function should maybe be turned into a macro.
        eprintln!("{}", msg);
        eprintln!("[Line {}] in script.", self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize));
        self.reset_stack();
        Err(VMError::RuntimeError)
    }
}
