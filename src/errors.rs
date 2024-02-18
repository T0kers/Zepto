
pub enum VMError {
    CompileError(String),
    RuntimeError(String),
}

impl VMError {
    pub fn compile_error(msg: impl Into<String>) -> VMError {
        VMError::CompileError(msg.into())
    }

    pub fn runtime_error(msg: impl Into<String>) -> VMError {
        VMError::RuntimeError(msg.into())
    }
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            VMError::CompileError(ref err) => write!(f, "{}", err),
            VMError::RuntimeError(ref err) => write!(f, "{}", err),
        }
    }
}