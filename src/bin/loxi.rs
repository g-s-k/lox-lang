use std::{
    error, fs,
    io::{self, BufRead, Write},
    path::{Path, PathBuf},
    process,
    time::Instant,
};

use {
    clap::Clap,
    lazy_static::{initialize, lazy_static},
    lox_lang::{Error, ErrorCategory, Value, VM},
    simplelog::{ConfigBuilder, LevelFilter, SimpleLogger},
};

#[derive(Clap)]
struct Params {
    script: Option<PathBuf>,
}

fn main() {
    let args = Params::parse();
    SimpleLogger::init(
        LevelFilter::Debug,
        ConfigBuilder::new()
            .set_thread_level(LevelFilter::Off)
            .set_time_level(LevelFilter::Off)
            .set_location_level(LevelFilter::Debug)
            .build(),
    )
    .unwrap();

    if let Some(path) = args.script {
        if let Err(e) = run_file(&path) {
            process::exit(match e.category() {
                ErrorCategory::Compilation => 65,
                ErrorCategory::Runtime => 70,
                _ => 1,
            })
        };
    } else {
        repl()
    }
}

fn repl() {
    let mut vm = vm_with_global();
    let mut buffer = String::new();
    loop {
        buffer.clear();
        print!("> ");
        io::stdout().flush().unwrap();
        match io::stdin().lock().read_line(&mut buffer) {
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
            Ok(0) => {
                // EOF
                println!("bye");
                break;
            }
            _ => (),
        }

        let _ = vm.interpret(buffer.clone());
    }
}

fn run_file(path: &Path) -> Result<(), Error> {
    let source = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Could not read file {}: {}", path.to_string_lossy(), e);
            process::exit(74);
        }
    };

    vm_with_global()
        .interpret(source)
        .map_err(|mut errs| errs.pop().unwrap())
}

fn vm_with_global() -> VM<'static> {
    let mut vm = VM::default();
    initialize(&START_TIME);
    vm.define_global("clock", Value::NativeFun(clock_native));
    vm
}

lazy_static! {
    static ref START_TIME: Instant = Instant::now();
}

fn clock_native(_: &[Value]) -> Result<Value, Box<dyn error::Error>> {
    Ok(Value::Number(START_TIME.elapsed().as_secs_f64()))
}
