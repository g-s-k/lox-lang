fn main() {
    let mut chunk = lox_lang::Chunk::new("script");

    chunk.write_constant(lox_lang::Value(1.25), 123);
    chunk.write(lox_lang::Op::Return, 123);

    println!("{:?}", chunk);
}
