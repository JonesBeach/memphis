use std::{
    io::{self, Write},
    panic, process,
};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode},
    terminal, ExecutableCommand,
};

use crate::{
    core::{InterpreterEntrypoint, Voidable},
    init::Builder,
    lexer::Lexer,
    parser::Parser,
    treewalk::Interpreter,
    types::errors::MemphisError,
};

pub struct Repl {
    /// `in_block` may need to become a state for a FSM, but a `bool` seems to be working fine for
    /// now
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
            in_block: false,
            errors: vec![],
            input: String::new(),
            history: vec![],
            history_index: None,
        }
    }

    fn marker(&self) -> &str {
        match self.in_block {
            false => ">>>",
            true => "...",
        }
    }

    pub fn run(&mut self) {
        println!(
            "memphis {} REPL (Type 'exit()' to quit)",
            env!("CARGO_PKG_VERSION")
        );

        // Install a panic hook to ensure raw mode is disabled on panic
        panic::set_hook(Box::new(|info| {
            let _ = terminal::disable_raw_mode();
            if let Some(s) = info.payload().downcast_ref::<&str>() {
                eprintln!("\n\rPanic: {s:?}");
            } else if let Some(s) = info.payload().downcast_ref::<String>() {
                eprintln!("\n\rPanic: {s:?}");
            } else {
                eprintln!("\n\rPanic occurred!");
            }

            if let Some(location) = info.location() {
                println!("in file '{}' at line {}", location.file(), location.line(),);
            } else {
                println!("in an unknown location.");
            }
            process::exit(1);
        }));

        let (_, mut interpreter) = Builder::default().build_treewalk_expl();
        let mut line = String::new();

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        let _ = terminal::enable_raw_mode();

        print!("{} ", self.marker());
        io::stdout().flush().unwrap();
        loop {
            if let Event::Key(event) = event::read().unwrap() {
                match event.code {
                    KeyCode::Char(c) => {
                        line.push(c);
                        print!("{}", c);
                        io::stdout().flush().unwrap();
                    }
                    KeyCode::Backspace => {
                        if !line.is_empty() {
                            line.pop();
                            print!("\x08 \x08"); // Escape sequence to emit a backspace char
                            io::stdout().flush().unwrap();
                            io::stdout().execute(cursor::MoveLeft(1)).unwrap();
                        }
                    }
                    KeyCode::Enter => {
                        self.history.push(line.clone());
                        self.history_index = None;
                        self.process_line(&mut interpreter, &line);
                        line.clear();

                        println!();
                        io::stdout().execute(cursor::MoveToNextLine(1)).unwrap();
                        print!("{} ", self.marker());
                        io::stdout().flush().unwrap();
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

        unreachable!()
    }

    fn end_of_statement(&self, input: &str) -> bool {
        if let Some(last_char) = input.chars().last() {
            match self.in_block {
                // The start of blocks always begin with : and a newline
                false => last_char != ':',

                // The end of blocks are indicated by an empty line
                true => last_char == '\n',
            }
        } else {
            // empty string
            true
        }
    }

    /// Clear current input and redraw it
    fn redraw_input(&self, line: &str) {
        print!("\r{} {}", self.marker(), line);
        io::stdout().flush().unwrap();
    }

    fn process_line(&mut self, interpreter: &mut Interpreter, line: &str) {
        if line.trim_end() == "exit()" {
            print!("\n\rExiting...\n\r");

            let _ = terminal::disable_raw_mode();
            let error_code = match self.errors.len() {
                0 => 0,
                _ => 1,
            };
            process::exit(error_code);
        }

        self.input.push_str(line);

        if self.end_of_statement(line) {
            let lexer = Lexer::new(&self.input);
            let mut parser = Parser::new(lexer.tokens(), interpreter.state.clone());
            match interpreter.run(&mut parser) {
                Ok(i) => {
                    if !i.is_none() {
                        print!("\n\r{}", i);
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
