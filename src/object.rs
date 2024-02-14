
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