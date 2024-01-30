use crate::{compiler::Compiler, vm::VMError, vm::VM};

pub fn interpret(source: &str) -> Result<(), VMError> {
    let mut compiler = Compiler::new(source);
    compiler.compile()?;
    let mut vm = VM::new(&compiler.chunk);
    vm.run()
}