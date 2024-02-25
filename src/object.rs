
#[derive(Clone, Debug, Copy)]
pub struct UpValueLocation {
    location: usize,
}

impl UpValueLocation {
    pub fn new(location: usize) -> Self {
        Self {location}
    }
    pub fn location(&self) -> usize {
        self.location
    }
}

#[derive(PartialEq)]
pub struct UpValue {
    pub location: usize,
    pub is_closed: bool,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self {location, is_closed: false}
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub arity: u8,
    pub start: usize,
}

impl Function {
    pub fn new(arity: u8, start: usize) -> Self {
        Self {arity, start}
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Box<[UpValueLocation]>,
}

impl Closure {
    pub fn new(function: Function, upvalue_count: usize) -> Self {
        Self {function, upvalues: vec![UpValueLocation::new(0); upvalue_count].into_boxed_slice()}
    }
}