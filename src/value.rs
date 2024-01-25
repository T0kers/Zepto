

#[derive(Copy)]
#[derive(Clone)]
pub enum Value {
    Int(i32),
    Nul,
}

pub fn value_to_string(value: Value) -> String {
    match value {
        Value::Int(n) => n.to_string(),
        Value::Nul => "nul".to_string(),
    }
}