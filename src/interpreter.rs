use crate::{compiler::Compiler, vm::{VM, VMError}};

pub fn interpret(source: &str) -> Result<(), VMError> {
    let mut compiler = Compiler::new(source);
    compiler.compile()?;
    let mut vm = VM::new(&compiler.chunk, compiler.globals.next_id);
    vm.run()
}