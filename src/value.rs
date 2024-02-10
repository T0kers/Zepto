use std::{fmt, ops, cmp::Ordering};
use crate::object::Object;

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    fn as_bool(&self) -> bool {
        match self {
            Number::Int(i) => *i != 0,
            Number::Float(f) => *f != 0.0,
        }
    }
}

impl ops::Add for Number {
    type Output = Number;
    fn add(self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => match a.checked_add(b) {
                Some(n) => Number::Int(n),
                None => Number::Float(a as f64 + b as f64),
            },
            (Number::Int(a), Number::Float(b)) => Number::Float(a as f64 + b),
            (Number::Float(a), Number::Int(b)) => Number::Float(a + b as f64),
            (Number::Float(a), Number::Float(b)) => Number::Float(a + b),
        }
    }
}

impl ops::Sub for Number {
    type Output = Number;
    fn sub(self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => match a.checked_sub(b) {
                Some(n) => Number::Int(n),
                None => Number::Float(a as f64 - b as f64),
            },
            (Number::Int(a), Number::Float(b)) => Number::Float(a as f64 - b),
            (Number::Float(a), Number::Int(b)) => Number::Float(a - b as f64),
            (Number::Float(a), Number::Float(b)) => Number::Float(a - b),
        }
    }
}

impl ops::Mul for Number {
    type Output = Number;
    fn mul(self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => match a.checked_mul(b) {
                Some(n) => Number::Int(n),
                None => Number::Float(a as f64 * b as f64),
            },
            (Number::Int(a), Number::Float(b)) => Number::Float(a as f64 * b),
            (Number::Float(a), Number::Int(b)) => Number::Float(a * b as f64),
            (Number::Float(a), Number::Float(b)) => Number::Float(a * b),
        }
    }
}

impl ops::Div for Number {
    type Output = Number;
    fn div(self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) if a % b == 0 => match a.checked_div(b) {
                Some(n) => Number::Int(n),
                None => Number::Float(a as f64 / b as f64),
            },
            (Number::Int(a), Number::Int(b)) => Number::Float(a as f64 / b as f64),
            (Number::Int(a), Number::Float(b)) => Number::Float(a as f64 / b),
            (Number::Float(a), Number::Int(b)) => Number::Float(a / b as f64),
            (Number::Float(a), Number::Float(b)) => Number::Float(a / b),
        }
    }
}

impl ops::Rem for Number {
    type Output = Option<Number>;
    fn rem(self, other: Number) -> Option<Number> {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a.checked_rem(b).map(Number::Int),
            _ => None,
        }
    }
}

impl ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Number {
        match self {
            Number::Int(a) => match a.checked_neg() {
                Some(n) => Number::Int(n),
                None => Number::Float(-(a as f64)),
            },
            Number::Float(f) => Number::Float(-f),
        }
    }
}

macro_rules! build_bitwise_ops {
    ($type:ident, $func:ident, $op:tt) => {
        impl ops::$type for Number {
            type Output = Option<Number>;
            fn $func(self, other: Number) -> Option<Number> {
                match (self, other) {
                    (Number::Int(a), Number::Int(b)) => Some(Number::Int(a $op b)),
                    _ => None,
                }
            }
        }
    };
}

build_bitwise_ops!(BitOr, bitor, |);
build_bitwise_ops!(BitXor, bitxor, ^);
build_bitwise_ops!(BitAnd, bitand, &);

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            (Number::Int(a), Number::Float(b)) => (*a as f64) == *b,
            (Number::Float(a), Number::Int(b)) => *a == (*b as f64),
        }
    }
}
impl Eq for Number {}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a.partial_cmp(b),
            (Number::Float(a), Number::Float(b)) => a.partial_cmp(b),
            (Number::Int(a), Number::Float(b)) => (*a as f64).partial_cmp(b),
            (Number::Float(a), Number::Int(b)) => a.partial_cmp(&(*b as f64)),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{}", i),
            Number::Float(n) => write!(f, "{}", {
                if n.fract() == 0.0 {
                    format!("{:.1}", n)
                }
                else {
                    n.to_string()
                }
            }),
        }
    }
}

#[derive(Clone)]
pub enum Value<'a> {
    Obj(Box<Object<'a>>),
    Num(Number),
    Bool(bool),
    Nul,
}

impl<'a> Value<'a> {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Obj(o) => o.as_bool(),
            Value::Num(n) => n.as_bool(),
            Value::Bool(b) => *b,
            Value::Nul => false,
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Obj(o) => write!(f, "{}", o),
            Value::Num(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nul => write!(f, "nul"),
        }
    }
}
