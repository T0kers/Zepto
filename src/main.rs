pub mod chunk;
use chunk::*;
pub mod debug;
pub mod value;
use value::*;
pub mod vm;
use vm::*;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");

    let val = Value::Int(2);
    println!("{}", std::mem::size_of_val(&val));

    let mut vm: VM = VM::new();
    
    vm.chunk.add_opcode(OpCode::RETURN, 1);
    vm.chunk.write_constant(Value::Int(3), 6);
    vm.chunk.write_constant(Value::Int(3), 6);
    vm.chunk.add_opcode(OpCode::EOF, 3999);
    debug::disassemble_chunk(&vm.chunk, "Test chunk.");
    let res = vm.interpret();
}
