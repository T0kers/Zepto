use crate::{compiler::{Parser, FunctionKind}, vm::{VM, VMError}, function::Function};

pub fn interpret(source: &str) -> Result<(), VMError> {
    let mut parser = Parser::new(source, FunctionKind::Script);
    let function = parser.compile()?;
    let mut vm = VM::new(parser.current_chunk(), parser.globals.next_id);
    vm.run()
}