use std::{
    fmt::Display,
    io::{self, Write},
    panic, process,
};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
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

/// When the terminal is in raw mode, we must emit a carriage return in addition to a newline,
/// because that does not happen automatically.
fn normalize<T: Display>(err: &T) -> String {
    let formatted = format!("{}", err);
    if terminal::is_raw_mode_enabled().expect("Failed to query terminal raw mode") {
        formatted.replace("\n", "\n\r")
    } else {
        formatted.to_string()
    }
}

/// Print command which will normalize newlines + carriage returns before printing.
fn print_raw<T: Display>(val: &T) {
    print!("{}", normalize(val));
    io::stdout().flush().expect("Failed to flush stdout");
}

/// Print command which will normalize newlines + carriage returns before printing and include a
/// newline at the end of the value.
fn println_raw<T: Display>(val: &T) {
    print_raw(&format!("{}\n", val));
}

/// Install a panic hook to ensure raw mode is disabled on panic.
fn install_custom_panic_hook() {
    panic::set_hook(Box::new(|info| {
        // This line is critical!! The rest of this function is just debug info, but without this
        // line, your shell will become unusable on an unexpected panic.
        let _ = terminal::disable_raw_mode();

        if let Some(s) = info.payload().downcast_ref::<&str>() {
            eprintln!("\nPanic: {s:?}");
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            eprintln!("\nPanic: {s:?}");
        } else {
            eprintln!("\nPanic occurred!");
        }

        if let Some(location) = info.location() {
            eprintln!(
                "  in file '{}' at line {}",
                location.file(),
                location.line()
            );
        } else {
            eprintln!("  in an unknown location.");
        }

        process::exit(1);
    }));
}

/// The Memphis Read-Evaluate-Print-Loop (REPL).
pub struct Repl {
    /// `in_block` may need to become a state for a FSM, but a `bool` seems to be working fine for
    /// now.
    in_block: bool,

    /// Track any interpreter errors so that we can properly emit an useful exit code.
    errors: Vec<MemphisError>,

    /// The current line being manipulated by the user.
    line: String,

    /// The current cursor position on the current line. This _excludes_ the `marker`.
    line_index: usize,

    /// The current statement being constructed. This will consist of the last 1 or more `line`
    /// values.
    input: String,

    /// A list of all the lines (_not_ statements) recording during this REPL session.
    history: Vec<String>,

    /// If Up/Down has been pressed, the index in `history` the user is currently selecting.
    history_index: Option<usize>,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
    /// Initialize an empty REPL.
    pub fn new() -> Self {
        Repl {
            in_block: false,
            errors: vec![],
            line: String::new(),
            line_index: 0,
            input: String::new(),
            history: vec![],
            history_index: None,
        }
    }

    /// The primary entrypoint to the REPL.
    pub fn run(&mut self) {
        println!(
            "memphis {} REPL (Type 'exit()' to quit)",
            env!("CARGO_PKG_VERSION")
        );

        let (_, mut interpreter) = Builder::default().build_treewalk_expl();

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        install_custom_panic_hook();
        let _ = terminal::enable_raw_mode();

        self.initialize_prompt(false);
        loop {
            if let Event::Key(event) = event::read().expect("Failed to read key event") {
                match (event.code, event.modifiers) {
                    (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                        self.initialize_prompt(true);
                        continue;
                    }
                    (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                        panic!("^D");
                    }
                    _ => {}
                }

                self.handle_key_event(&mut interpreter, event);
            }
        }
    }

    /// Update the terminal and interpreter state based on the given `KeyEvent`.
    fn handle_key_event(&mut self, interpreter: &mut Interpreter, event: KeyEvent) {
        match event.code {
            KeyCode::Char(c) => {
                self.line.insert(self.line_index, c);
                self.line_index += 1;
                self.redraw_and_position();
            }
            KeyCode::Backspace => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                    self.line.remove(self.line_index);
                    self.redraw_and_position();
                }
            }
            KeyCode::Enter => {
                self.history.push(self.line.clone());
                self.history_index = None;

                // This newline simulates the user pressing the enter key
                println_raw(&"");
                self.process_line(interpreter, &self.line.clone());

                self.initialize_prompt(false);
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
                    self.line = self.history[index].clone();
                    self.line_index = self.line.len();
                    self.redraw_and_position();
                }
            }
            KeyCode::Down => {
                if let Some(index) = self.history_index {
                    if index < self.history.len() - 1 {
                        self.history_index = Some(index + 1);
                    } else {
                        self.history_index = None;
                        self.line.clear();
                    }

                    if let Some(index) = self.history_index {
                        self.line = self.history[index].clone();
                    } else {
                        self.line.clear();
                    }

                    self.line_index = self.line.len();
                    self.redraw_and_position();
                }
            }
            KeyCode::Right => {
                if self.line_index < self.line.len() {
                    self.line_index += 1;
                    self.redraw_and_position();
                }
            }
            KeyCode::Left => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                    self.redraw_and_position();
                }
            }
            _ => {}
        }
    }

    /// Gives the indicator for the start of the given line, based on whether or not the most
    /// recent line provided by the user completed a statement or not.
    fn prompt(&self) -> &str {
        match self.in_block {
            false => ">>> ",
            true => "... ",
        }
    }

    /// Check if the provided input str indicates the end of a statement.
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

    /// Clear the REPL prompt to prepare for user input.
    fn initialize_prompt(&mut self, add_new_line: bool) {
        self.line.clear();
        self.line_index = 0;
        if add_new_line {
            print_raw(&"\n");
        }
        print_raw(&self.prompt());
    }

    /// Clear the current input, redraw it, and align the cursor to the proper column.
    fn redraw_and_position(&self) {
        // Redraw
        execute!(io::stdout(), Clear(ClearType::CurrentLine))
            .expect("Failed to execute terminal command");
        print_raw(&format!("\r{}{}", self.prompt(), self.line));

        // Position
        let cursor_col = (self.line_index + self.prompt().len()) as u16;
        execute!(io::stdout(), cursor::MoveToColumn(cursor_col))
            .expect("Failed to execute terminal command");
    }

    /// Append the provided line to the constructed statement and evaluate it through the
    /// `Interpreter`.
    fn process_line(&mut self, interpreter: &mut Interpreter, line: &str) {
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
                Ok(result) => {
                    if !result.is_none() {
                        println_raw(&result);
                    }
                }
                Err(err) => {
                    self.errors.push(err.clone());
                    println_raw(&err);
                }
            }

            self.input.clear();
            self.in_block = false;
        } else {
            // This wasn't the end of a statement, so add a newline. We could do this in the
            // handling for `KeyEvent::Enter` above, but that would add complexity to
            // `end_of_statement`. Trade-offs!
            self.input.push('\n');
            self.in_block = true;
        }
    }
}
