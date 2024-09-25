use std::{
    io::{self, Write},
    process,
};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode},
    terminal, ExecutableCommand,
};

use crate::{
    core::{Container, InterpreterEntrypoint, Voidable},
    init::Builder,
    lexer::Lexer,
    parser::Parser,
    types::errors::MemphisError,
};

pub struct Repl {
    in_block: bool,
    errors: Vec<MemphisError>,
    input: String,
    history: Vec<String>,
    history_index: Option<usize>,
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
            input: String::new(),
            history: vec![],
            history_index: None,
        }
    }

    fn marker(&self) -> String {
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

        let (_, mut interpreter) = Builder::default().build();
        let mut line = String::new();

        // Enable raw mode to handle individual keypresses
        terminal::enable_raw_mode().unwrap();
        io::stdout()
            .execute(terminal::Clear(terminal::ClearType::All))
            .unwrap();

        loop {
            io::stdout().execute(cursor::MoveTo(0, 0)).unwrap();
            print!("{} ", self.marker());
            io::stdout().flush().unwrap();

            if let Event::Key(event) = event::read().unwrap() {
                match event.code {
                    KeyCode::Char(c) => {
                        line.push(c);
                        print!("{}", c); // Print the character
                        io::stdout().flush().unwrap();
                    }
                    KeyCode::Backspace => {
                        if !line.is_empty() {
                            line.pop();
                            print!("\x08 \x08"); // Handle backspace
                            io::stdout().flush().unwrap();
                        }
                    }
                    KeyCode::Enter => {
                        println!();
                        self.history.push(line.clone());
                        self.history_index = None;
                        self.process_line(&line);
                        line.clear();
                    }
                    KeyCode::Up => {
                        if let Some(index) = self.history_index {
                            if index > 0 {
                                self.history_index = Some(index - 1);
                            }
                        } else if !self.history.is_empty() {
                            self.history_index = Some(self.history.len() - 1);
                        }

                        if let Some(index) = self.history_index {
                            line = self.history[index].clone();
                            self.redraw_input(&line);
                        }
                    }
                    KeyCode::Down => {
                        if let Some(index) = self.history_index {
                            if index < self.history.len() - 1 {
                                self.history_index = Some(index + 1);
                            } else {
                                self.history_index = None;
                                line.clear();
                            }

                            if let Some(index) = self.history_index {
                                line = self.history[index].clone();
                            } else {
                                line.clear();
                            }

                            self.redraw_input(&line);
                        }
                    }
                    KeyCode::Right => break,
                    _ => {}
                }
            }
        }

        // Don't forget to disable raw mode at the end
        terminal::disable_raw_mode().unwrap();
    }

    fn should_interpret(&self, input: &str) -> bool {
        let last_two = &input[input.len() - 2..];
        if !self.in_block {
            // The start of blocks always begin with : and a newline
            last_two != ":\n"
        } else {
            // The end of blocks are indicated by an empty line
            last_two == "\n\n"
        }
    }

    /// Clear current input and redraw it
    fn redraw_input(&self, line: &str) {
        print!("\r{} {}", self.marker(), line);
        io::stdout().flush().unwrap();
    }

    fn process_line(&mut self, line: &str) {
        if line.trim_end() == "exit()" {
            println!("Exiting...");

            let error_code = match self.errors.len() {
                0 => 0,
                _ => 1,
            };
            process::exit(error_code);
        }

        self.input.push_str(&line);

        if self.should_interpret(&self.input) {
            let lexer = Lexer::new(&self.input);
            let mut parser = Parser::new(lexer.tokens(), self.state.clone());
            match self.interpreter.run(&mut parser) {
                Ok(i) => {
                    if !i.is_none() {
                        println!("{}", i);
                    }
                }
                Err(err) => {
                    self.errors.push(err.clone());
                    eprintln!("{}", err);
                }
            }

            self.input.clear();
            self.in_block = false;
        } else {
            self.in_block = true;
        }
    }
}
