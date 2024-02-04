use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nul,
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Int(n) => *n != 0,
            Value::Float(n) => *n!= 0.0,
            Value::Bool(b) => *b,
            Value::Nul => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(n) => write!(f, "{}", {
                if n.fract() == 0.0 {
                    format!("{:.1}", n)
                }
                else {
                    n.to_string()
                }
            }),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nul => write!(f, "nul"),
        }
    }
}
