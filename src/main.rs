pub mod chunk;
use chunk::*;
pub mod debug;
pub mod value;

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    let mut chunk = Chunk::new();
    chunk.add_opcode(OpCode::RETURN, 1);

    chunk.write_constant(2, 6);

    chunk.add_opcode(OpCode::RETURN, 3999);

    debug::disassemble_chunk(&chunk, "Test chunk.");
}
