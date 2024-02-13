use crate::{compiler::Compiler, vm::{VM, VMError}, value::Value};

pub fn interpret(source: &str) -> Result<Value, VMError> {
    let mut compiler = Compiler::new(source);
    compiler.compile()?;
    let mut vm = VM::new(&compiler.chunk, compiler.globals.globals);
    vm.run()
}