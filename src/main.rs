use std::{
    io::{Cursor, Write},
    path::PathBuf,
};

use clap::Parser;
use color_eyre::{eyre::WrapErr, Result};
mod interpreter;
mod lexer;
mod parser;

fn eval_code(input: &str) -> Result<parser::Expression> {
    let mut parser = parser::Parser::new(input);
    let ast = parser.parse()?;
    let mut interpreter = interpreter::Interpreter::new();

    Ok(interpreter.eval(ast)?)
}

fn eval_file(input: &str) -> Result<()> {
    let mut parser = parser::Parser::new(input);
    let mut interpreter = interpreter::Interpreter::new();

    loop {
        let ast = parser.parse()?;
        if ast == parser::Expression::Atom(parser::Value::Nil) {
            break;
        }

        interpreter.eval(ast)?;
    }

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about)]
struct InterpreterCli {
    #[arg(help = "file to interpret, or stdin if not provided")]
    filename: Option<PathBuf>,
}

fn reader(input: impl std::io::Read) -> Result<String> {
    std::io::read_to_string(input).wrap_err("could not read from file")
}

/// Installs our default backtrace tracking for compiler info & error logging (not usually
/// user-facing)
fn install_tracing() {
    use tracing_error::ErrorLayer;
    use tracing_subscriber::prelude::*;
    use tracing_subscriber::{fmt, EnvFilter};

    let fmt_layer = fmt::layer().with_target(false);
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new("info"))
        .unwrap();

    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .with(ErrorLayer::default())
        .init();
}

fn main() -> Result<()> {
    color_eyre::install()?;
    install_tracing();

    let compiler_args = InterpreterCli::parse();

    match compiler_args.filename {
        Some(file_path) => {
            let file = std::fs::File::open(file_path)?;
            let code = reader(file)?;
            _ = eval_file(&code)?;
        }
        None => {
            // either eval, or go into repl mode, based on whether stdin is a tty (it won't be if
            // the user pipes input to the interpreter directly, e.g. ./the-interpreter <<< 'some input')
            if atty::is(atty::Stream::Stdin) {
                print!("> ");
                std::io::stdout().flush()?;
                for line in std::io::stdin().lines() {
                    match line {
                        Ok(line) => {
                            let code = reader(Cursor::new(line))?;
                            let output = eval_code(&code)?;
                            println!("{output:?}");
                            print!("> ");
                            std::io::stdout().flush()?;
                        }
                        Err(e) => {
                            eprintln!("Error reading line: {e}")
                        }
                    }
                }
            } else {
                let code = reader(std::io::stdin().lock())?;
                print!("{code}");
            }
        }
    };

    Ok(())
}
