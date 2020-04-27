use std::{
    fs,
    io::{self, BufRead, Write},
    path::{Path, PathBuf},
    process,
};

use {
    clap::Clap,
    lox_lang::{Error, ErrorCategory, VM},
};

#[derive(Clap)]
struct Params {
    script: Option<PathBuf>,
}

fn main() {
    let args = Params::parse();

    if let Err(e) = if let Some(path) = args.script {
        run_file(&path)
    } else {
        repl()
    } {
        process::exit(match e.category() {
            ErrorCategory::Compilation => 65,
            ErrorCategory::Runtime => 70,
        });
    }
}

fn repl() -> Result<(), Error> {
    let mut vm = VM::default();
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

        vm.interpret(buffer.clone())?;
    }

    Ok(())
}

fn run_file(path: &Path) -> Result<(), Error> {
    let source = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Could not read file {}: {}", path.to_string_lossy(), e);
            process::exit(74);
        }
    };

    VM::default().interpret(source)
}
