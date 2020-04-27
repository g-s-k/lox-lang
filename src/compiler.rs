use super::{Chunk, Error, Scanner, Token};

pub(crate) struct Compiler<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            scanner: Scanner::new(source),
        }
    }

    pub fn compile(&mut self) -> Result<Chunk, Error> {
        for token in &mut self.scanner {
            match token {
                Ok(Token {
                    line, r#type, text, ..
                }) => {
                    println!("{:4} {:?}\t'{}'", line, r#type, text);
                }
                Err((line, text)) => {
                    println!("{:4} '{}'", line, text);
                }
            }
        }

        Ok(Chunk::new(""))
    }
}
