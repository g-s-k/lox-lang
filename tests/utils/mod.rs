#![macro_use]

use simplelog::{ConfigBuilder, LevelFilter, SimpleLogger};

macro_rules! run {
    ($source: literal -> $( $expected: literal ),*) => {{
        use std::io::Read;

        let mut vm = lox_lang::VM::default();
        vm.buffer_output(true);

        vm.interpret($source).map_err(|mut v| v.pop().unwrap())?;

        let mut buffer = String::new();
        vm.read_to_string(&mut buffer).unwrap();
        assert_eq!(buffer, concat!($( $expected, "\n" ),*));
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
