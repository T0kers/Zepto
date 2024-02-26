use crate::{chunk::{Chunk, OpCode}, value::{Value, Number}, object::{Closure, UpValueLocation, UpValue}, errors::VMError};
use std::{ptr, vec};


struct CallInfo {
    return_address: *const u8,
    stack_offset: usize,
    locals_to_capture: Vec<UpValueLocation>,
}

impl CallInfo {
    pub const fn new() -> Self {
        Self {
            return_address: ptr::null(),
            stack_offset: 0,
            locals_to_capture: vec![]
        }
    }
    pub fn set(&mut self, return_address: *const u8, stack_offset: usize) {
        self.return_address = return_address;
        self.stack_offset = stack_offset;
        self.locals_to_capture.clear();
    }
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: *const u8,
    stack: [Value; 256],
    stack_top: *mut Value,
    globals: Box<[Option<Value>]>,
    call_stack: [CallInfo; 64],
    call_stack_ptr: *mut CallInfo,
    upvalues: Vec<UpValue>,
    closed_upvalues: Vec<Value>,
}

impl<'a> VM<'a> {
    fn get_upvalue_value(&self, location: UpValueLocation) -> Value {
        let upvalue = &self.upvalues[location.location()];
        if upvalue.is_closed {
            self.closed_upvalues[upvalue.location].clone()
        }
        else {
            self.stack[upvalue.location].clone()
        }
    }
    fn set_upvalue_value(&mut self, location: UpValueLocation, value: Value) {
        let upvalue = &self.upvalues[location.location()];
        if upvalue.is_closed {
            self.closed_upvalues[upvalue.location] = value
        }
        else {
            self.stack[upvalue.location] = value
        }
    }
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
            (Value::Str(a), Value::Str(b)) => self.push(Value::Str(Box::new(*a + b.as_str()))),
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
            (Value::Str(a), Value::Num(Number::Int(b))) => self.push(Value::Str(Box::new(a.repeat(b as usize)))),
            (Value::Num(Number::Int(a)), Value::Str(b)) => self.push(Value::Str(Box::new(b.repeat(a as usize)))),
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
            (Value::Str(a), Value::Str(b)) => self.push(Value::Bool(a == b)),
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a == b)),
            (Value::Num(a), Value::Bool(b)) => self.push(Value::Bool(a == Number::Int(b as i64))),
            (Value::Bool(a), Value::Num(b)) => self.push(Value::Bool(Number::Int(a as i64) == b)),
            (Value::Nul, Value::Nul) => self.push(Value::Bool(true)),
            _ => self.push(Value::Bool(false)),
        }
        Ok(())
    }
    build_comparison_op!(less, <);
    build_comparison_op!(greater, >);
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk, globals: Vec<Option<Value>>) -> Self {
        const STACK_REPEAT_VALUE: Value = Value::Nul;
        const CALL_STACK_REPEAT_VALUE: CallInfo = CallInfo::new();
        Self {
            chunk,
            ip: ptr::null(),
            stack_top: ptr::null_mut(),
            stack: [STACK_REPEAT_VALUE; 256],
            globals: globals.into_boxed_slice(),
            call_stack: [CALL_STACK_REPEAT_VALUE; 64],
            call_stack_ptr: ptr::null_mut(),
            upvalues: vec![],
            closed_upvalues: vec![],
        }
    }
    fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
    }
    unsafe fn read_byte(&mut self) -> u8 {
        let byte = *self.ip;
        self.ip = self.ip.add(1);
        byte
    }
    unsafe fn read_bytes(&mut self) -> u16 {
        let bytes = (self.read_byte() as u16) << 8;
        bytes | self.read_byte() as u16
    }
    
    pub fn run(&mut self) -> Result<Value, VMError> {
        self.ip = self.chunk.code.as_ptr();
        self.call_stack_ptr = self.call_stack.as_mut_ptr();
        self.stack_top = self.stack.as_mut_ptr();
        unsafe {
            loop {
                #[cfg(feature = "debug_code")]
                {
                    use crate::debug;
                    print!("        [ ");
                    let stack_size = self.stack_top.offset_from(self.stack.as_ptr()) as usize;
                    for index in 0..stack_size {
                        print!("{} ", debug::print_value(&self.stack[index]));
                    }
                    println!("]");
                    debug::disassemble_instruction(self.chunk, self.ip.offset_from(self.chunk.code.as_ptr()) as usize);
                }
                
                match self.read_byte() {
                    OpCode::CONSTANT => {
                        let index = self.read_byte() as usize;
                        let constant = self.chunk.constants[index].clone();
                        self.push(constant);
                    }
                    OpCode::CONSTANT_LONG => {
                        let constant = self.chunk.constants[self.read_bytes() as usize].clone();
                        self.push(constant);
                    }
                    OpCode::TRUE => self.push(Value::Bool(true)),
                    OpCode::FALSE => self.push(Value::Bool(false)),
                    OpCode::BOOL => {
                        let result = self.pop().as_bool();
                        self.push(Value::Bool(result))
                    },
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
                        self.globals[self.read_bytes() as usize] = Some(self.pop()); // TODO: MAYBE change this because of garbage collection.
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
                        let index = self.read_bytes() as usize;
                        match self.globals[index] {
                            Some(_) => {
                                self.globals[index] = Some(self.peek(0)); // TODO: MAYBE change this because of garbage collection.
                            }
                            None => {
                                self.runtime_error("Cannot assign to undefined variable.")?; // TODO: show variable name in message.
                            }
                        }
                    }
                    OpCode::SET_LOCAL => {
                        let index = self.read_byte() as usize + (*self.call_stack_ptr.sub(1)).stack_offset;
                        self.stack[index] = self.peek(0); // TODO: MAYBE change this because of garbage collection.
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
                        let index = self.read_bytes() as usize;
                        if let Some(value) = &self.globals[index] {
                            self.push(value.clone());
                        } else {
                            self.runtime_error("Undefined variable.")? // TODO: show variable name in message.
                        }
                    }
                    OpCode::GET_LOCAL => {
                        let index = self.read_byte() as usize + (*self.call_stack_ptr.sub(1)).stack_offset;
                        self.push(self.stack[index].clone());
                    }
                    OpCode::SET_UPVALUE => {
                        let slot = self.read_byte() as usize;
                        let value = self.peek(0);
                        if let Value::Closure(c) = self.stack[(*self.call_stack_ptr.sub(1)).stack_offset - 1].clone() {
                            self.set_upvalue_value(c.upvalues[slot], value);
                        }
                        else {
                            panic!("This error should only occur if Tokers has made anything wrong!");
                        }
                    },
                    OpCode::GET_UPVALUE => {
                        let slot = self.read_byte() as usize;
                        if let Value::Closure(c) = self.stack[(*self.call_stack_ptr.sub(1)).stack_offset - 1].clone() {
                            self.push(self.get_upvalue_value(c.upvalues[slot]));
                        }
                        else {
                            panic!("This error should only occur if Tokers has made anything wrong!");
                        }
                    },
                    OpCode::CLOSURE_LONG => {
                        if let Value::Fn(f) = self.chunk.constants[self.read_bytes() as usize].clone() { // TODO: change so no runtime check is used.
                            let upvalue_count = self.read_byte() as usize;
                            let mut closure = Closure::new(*f, upvalue_count);
                            for i in 0..upvalue_count {
                                let is_local = self.read_byte();
                                let index = self.read_byte() as usize;

                                if is_local != 0 {
                                    closure.upvalues[i] = self.capture_upvalue((*self.call_stack_ptr.sub(1)).stack_offset + index);
                                }
                                else {
                                    closure.upvalues[i] = {
                                        if let Value::Closure(c) = self.stack[(*self.call_stack_ptr.sub(1)).stack_offset - 1].clone() {
                                            c.upvalues[index]
                                        }
                                        else {
                                            panic!("This error should only occur if Tokers has made anything wrong!");
                                        }
                                    }
                                }
                            }
                            self.push(Value::Closure(Box::new(closure)));
                        }
                        else {
                            self.runtime_error("This error should never happen, unless my code is wrong!")?;
                        }
                    }
                    OpCode::CLOSE_UPVALUE => {
                        let index = self.stack_top.offset_from(self.stack.as_ptr()) as usize - 1;
                        for upvalue_location in (*self.call_stack_ptr).locals_to_capture.iter_mut() {
                            let upvalue = &mut self.upvalues[upvalue_location.location()];
                            if upvalue.is_closed {
                                continue;
                            }
                            if upvalue.location == index {
                                upvalue.is_closed = true;
                                self.closed_upvalues.push(self.stack[upvalue.location].clone());
                                upvalue.location = self.closed_upvalues.len() - 1;
                            }
                        }
                    }
                    OpCode::POP => {self.pop();},
                    OpCode::POP_SCOPE => {
                        self.stack_top = self.stack_top.sub(self.read_byte() as usize);
                    }
                    OpCode::JUMP => {
                        let offset = self.read_bytes() as usize;
                        self.ip = self.ip.add(offset);
                    }
                    OpCode::JUMP_IF_TRUE => {
                        let offset = self.read_bytes() as usize;
                        if self.peek(0).as_bool() {
                            self.ip = self.ip.add(offset);
                        }
                    }
                    OpCode::JUMP_IF_FALSE => {
                        let offset = self.read_bytes() as usize;
                        if !self.peek(0).as_bool() {
                            self.ip = self.ip.add(offset);
                        }
                    }
                    OpCode::POP_JUMP_IF_TRUE => {
                        let offset = self.read_bytes() as usize;
                        if self.pop().as_bool() {
                            self.ip = self.ip.add(offset);
                        }
                    }
                    OpCode::POP_JUMP_IF_FALSE => {
                        let offset = self.read_bytes() as usize;
                        if !self.pop().as_bool() {
                            self.ip = self.ip.add(offset);
                        }
                    }
                    OpCode::LOOP => {
                        let offset = self.read_bytes() as usize;
                        self.ip = self.ip.sub(offset);
                    }
                    OpCode::CALL => {
                        let arguments = self.read_byte() as usize;
                        match self.peek(arguments) {
                            Value::Fn(f) => {
                                if f.arity as usize != arguments {
                                    self.runtime_error(
                                        &format!("Expected {} arguments but got {}.", f.arity, arguments)
                                    )?;
                                }
                                (*self.call_stack_ptr).set(self.ip, self.stack_top.offset_from(self.stack.as_ptr()) as usize - f.arity as usize);
                                self.call_stack_ptr = self.call_stack_ptr.add(1);
                                self.ip = self.chunk.code.as_ptr().add(f.start);
                            }
                            Value::Closure(f) => {
                                if f.function.arity as usize != arguments {
                                    self.runtime_error(
                                        &format!("Expected {} arguments but got {}.", f.function.arity, arguments)
                                    )?;
                                }
                                (*self.call_stack_ptr).set(self.ip, self.stack_top.offset_from(self.stack.as_ptr()) as usize - f.function.arity as usize);
                                self.call_stack_ptr = self.call_stack_ptr.add(1);
                                self.ip = self.chunk.code.as_ptr().add(f.function.start);
                            }
                            Value::NativeFn(n) => {
                                let result = n(self, std::slice::from_raw_parts(self.stack_top.sub(arguments), arguments))?;
                                self.stack_top = self.stack_top.sub(arguments + 1);
                                self.push(result);
                            },
                            _ => self.runtime_error("Can only call functions.")?,
                        }
                    }
                    OpCode::RETURN => {
                        let return_value = self.pop();
                        self.call_stack_ptr = self.call_stack_ptr.sub(1);
                        for upvalue_location in (*self.call_stack_ptr).locals_to_capture.iter_mut() {
                            let upvalue = &mut self.upvalues[upvalue_location.location()];
                            upvalue.is_closed = true;
                            self.closed_upvalues.push(self.stack[upvalue.location].clone());
                            upvalue.location = self.closed_upvalues.len() - 1;
                        }
                        self.stack_top = self.stack.as_mut_ptr().add((*self.call_stack_ptr).stack_offset - 1);
                        self.push(return_value);
                        self.ip = (*self.call_stack_ptr).return_address;
                    },
                    OpCode::EOF => return Ok(self.pop()),
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
        (*self.stack_top).clone()
    }
    unsafe fn peek(&mut self, distance: usize) -> Value {
        (*self.stack_top.sub(1 + distance)).clone()
    }
    unsafe fn capture_upvalue(&mut self, local: usize) -> UpValueLocation {
        let local = UpValue::new(local);
        let index = self.upvalues.iter().enumerate().find_map(|(index, value)| {
            if *value == local {
                Some(index)
            } else {
                None
            }
        });
        match index {
            Some(i) => {
                UpValueLocation::new(i)
            },
            None => {
                self.upvalues.push(local);
                let location = UpValueLocation::new(self.upvalues.len() - 1);
                let locals_to_capture = &mut (*self.call_stack_ptr.sub(1)).locals_to_capture;
                locals_to_capture.push(location);
                location
            },
        }
    }

    pub unsafe fn runtime_error(&mut self, msg: &str) -> Result<(), VMError> {
        self.reset_stack();
        Err(VMError::runtime_error(format!("{} [Line {}] in script.", msg, self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize))))
    }

    pub unsafe fn runtime_value_error(&mut self, msg: &str) -> Result<Value, VMError> {
        self.reset_stack();
        Err(VMError::runtime_error(format!("{} [Line {}] in script.", msg, self.chunk.line((self.ip.offset_from(self.chunk.code.as_ptr()) - 1) as usize))))
    }
}
