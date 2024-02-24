
#[derive(Clone, Debug)]
pub struct UpValue {
    pub location: usize
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self {location}
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
    pub upvalues: Box<[UpValue]>,
}

impl Closure {
    pub fn new(function: Function, upvalue_count: usize) -> Self {
        Self {function, upvalues: vec![UpValue{location: 0}; upvalue_count].into_boxed_slice()}
    }
}