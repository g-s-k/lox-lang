#![macro_use]

use simplelog::{ConfigBuilder, LevelFilter, SimpleLogger};

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

#[allow(dead_code)]
pub fn setup_logger() {
    let _ = SimpleLogger::init(
        LevelFilter::Debug,
        ConfigBuilder::new()
            .set_thread_level(LevelFilter::Off)
            .set_time_level(LevelFilter::Off)
            .set_location_level(LevelFilter::Debug)
            .build(),
    );
}
