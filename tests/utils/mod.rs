#![macro_use]

macro_rules! run {
    ($source: literal -> $( $expected: literal ),*) => {{
        let mut buffer = Vec::new();
        let mut vm = lox_lang::VM::default();
        vm.replace_stream(Some(&mut buffer));
        vm.interpret($source).map_err(|mut v| v.pop().unwrap())?;
        assert_eq!(String::from_utf8(buffer).unwrap(), concat!($( $expected, "\n" ),*));
        Ok(())
    }};
}

pub type Result = std::result::Result<(), lox_lang::Error>;
