use crate::{compiler, vm::VMError, vm::VM};

pub fn interpret(source: &str) -> Result<(), VMError> {
    let chunk = compiler::compile(source)?;
    let mut vm = VM::new(&chunk);
    vm.run()
}