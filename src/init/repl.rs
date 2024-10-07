use std::{
    fmt::Display,
    io::{self, Write},
    panic, process,
};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{self, Clear, ClearType},
};

use crate::{
    core::{InterpreterEntrypoint, Voidable},
    init::Builder,
    lexer::Lexer,
    parser::Parser,
    treewalk::Interpreter,
    types::errors::MemphisError,
};

fn process_output_for_raw_mode(output: &str) -> String {
    if terminal::is_raw_mode_enabled().unwrap() {
        output.replace("\n", "\n\r")
    } else {
        output.to_string()
    }
}

fn normalize<T: Display>(err: &T) -> String {
    let formatted = format!("{}", err);
    process_output_for_raw_mode(&formatted)
}

fn print_std<T: Display>(val: &T) {
    print!("{}", normalize(val));
    io::stdout().flush().unwrap();
}

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
            false => ">>> ",
            true => "... ",
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
                eprintln!("\nPanic: {s:?}");
            } else if let Some(s) = info.payload().downcast_ref::<String>() {
                eprintln!("\nPanic: {s:?}");
            } else {
                eprintln!("\nPanic occurred!");
            }

            if let Some(location) = info.location() {
                println!("in file '{}' at line {}", location.file(), location.line(),);
            } else {
                println!("in an unknown location.");
            }
            process::exit(1);
        }));

        let (_, mut interpreter) = Builder::default().build_treewalk_expl();

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        let _ = terminal::enable_raw_mode();

        let mut line = String::new();
        let mut line_index = 0;
        print_std(&self.marker());

        loop {
            if let Event::Key(event) = event::read().unwrap() {
                match (event.code, event.modifiers) {
                    (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                        panic!("Ctrl-C detected!");
                    }
                    (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                        panic!("^D");
                    }
                    _ => {}
                }

                match event.code {
                    KeyCode::Char(c) => {
                        line.insert(line_index, c);
                        line_index += 1;
                        self.redraw_and_position(&line, line_index);
                    }
                    KeyCode::Backspace => {
                        if line_index > 0 {
                            line_index -= 1;
                            line.remove(line_index);
                            self.redraw_and_position(&line, line_index);
                        }
                    }
                    KeyCode::Enter => {
                        self.history.push(line.clone());
                        self.history_index = None;
                        self.process_line(&mut interpreter, &line);

                        line.clear();
                        line_index = 0;
                        print_std(&self.marker());
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

                            line_index = line.len();
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

                            line_index = line.len();
                            self.redraw_and_position(&line, line_index);
                        }
                    }
                    KeyCode::Right => {
                        if line_index < line.len() {
                            line_index += 1;
                            self.redraw_and_position(&line, line_index);
                        }
                    }
                    KeyCode::Left => {
                        if line_index > 0 {
                            line_index -= 1;
                            self.redraw_and_position(&line, line_index);
                        }
                    }
                    _ => {}
                }
            }
        }
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
        execute!(io::stdout(), Clear(ClearType::CurrentLine)).unwrap();
        print_std(&"\r");
        print_std(&self.marker());
        print_std(&line);
    }

    fn redraw_and_position(&self, line: &str, line_index: usize) {
        self.redraw_input(&line);
        let cursor_col = (line_index + self.marker().len()) as u16;
        execute!(io::stdout(), cursor::MoveToColumn(cursor_col)).unwrap();
    }

    fn process_line(&mut self, interpreter: &mut Interpreter, line: &str) {
        print_std(&"\n");
        if line.trim_end() == "exit()" {
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
                        print_std(&i);
                        print_std(&"\n");
                    }
                }
                Err(err) => {
                    self.errors.push(err.clone());
                    print_std(&err);
                    print_std(&"\n");
                }
            }

            self.input.clear();
            self.in_block = false;
        } else {
            self.in_block = true;
        }
    }
}
