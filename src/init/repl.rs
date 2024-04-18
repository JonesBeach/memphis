use std::io;
use std::io::Write;
use std::process;

use crate::core::{Container, InterpreterEntrypoint, Voidable};
use crate::init::Builder;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::treewalk::State;
use crate::types::errors::MemphisError;

pub struct Repl {
    in_block: bool,
    errors: Vec<MemphisError>,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            // this may need to become a state for a FSM, but this seems to be
            // working fine for now
            in_block: false,
            errors: vec![],
        }
    }

    fn marker(&mut self) -> String {
        if !self.in_block {
            ">>>".to_string()
        } else {
            "...".to_string()
        }
    }

    pub fn run(&mut self) {
        println!(
            "memphis {} REPL (Type 'exit()' to quit)",
            env!("CARGO_PKG_VERSION")
        );

        let state = Container::new(State::new());

        let (_, mut interpreter) = Builder::new().state(state.clone()).text("").build();
        let mut input = String::new();

        loop {
            print!("{} ", self.marker());
            io::stdout().flush().expect("Failed to flush stdout");

            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .expect("Failed to read line");

            if line.trim_end() == "exit()" {
                println!("Exiting...");

                let error_code = match self.errors.len() {
                    0 => 0,
                    _ => 1,
                };
                process::exit(error_code);
            }

            input.push_str(&line);

            if self.should_interpret(&input) {
                let lexer = Lexer::new(&input);
                let mut parser = Parser::new(lexer.tokens(), state.clone());
                match interpreter.run(&mut parser) {
                    Ok(i) => {
                        if !i.is_void() {
                            println!("{}", i);
                        }
                    }
                    Err(err) => {
                        self.errors.push(err.clone());
                        eprintln!("{}", err);
                    }
                }

                input.clear();
                self.in_block = false;
            } else {
                self.in_block = true;
            }
        }
    }

    fn should_interpret(&mut self, input: &str) -> bool {
        let last_two = &input[input.len() - 2..];
        if !self.in_block {
            // The start of blocks always begin with : and a newline
            last_two != ":\n"
        } else {
            // The end of blocks are indicated by an empty line
            last_two == "\n\n"
        }
    }
}
