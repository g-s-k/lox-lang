use lox_lang::*;

fn main() {
    let mut chunk = Chunk::new("script");

    chunk.write_constant(Value(1.2), 123);
    chunk.write_constant(Value(3.4), 123);

    chunk.write(Op::Add, 123);

    chunk.write_constant(Value(5.6), 123);

    chunk.write(Op::Divide, 123);
    chunk.write(Op::Negate, 123);
    chunk.write(Op::Return, 123);

    let mut vm = VM::default();
    vm.interpret(chunk).unwrap()
}
