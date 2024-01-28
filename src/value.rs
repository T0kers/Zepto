use std::fmt;

#[derive(Debug)]
#[derive(Copy)]
#[derive(Clone)]
pub enum Value {
    Int(i64),
    Nul,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Nul => write!(f, "nul"),
        }
    }
}
