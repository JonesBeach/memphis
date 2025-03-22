use std::{io, panic, process};

use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, Clear, ClearType},
};

use crate::{
    core::{InterpreterEntrypoint, Voidable},
    init::{MemphisContext, TerminalIO},
    lexer::Lexer,
    parser::Parser,
    treewalk::Interpreter,
    types::errors::MemphisError,
};

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
    pub fn run<T: TerminalIO>(&mut self, terminal_io: &mut T) {
        let _ = terminal_io.writeln(format!(
            "memphis {} REPL (Type 'exit()' to quit)",
            env!("CARGO_PKG_VERSION")
        ));

        // TODO clean up this flow for incremental evaluation
        let mut context = MemphisContext::default();
        // we need this to initialize the interpreter - again, not a clean interface
        let _ = context.run_treewalk();
        // we should probably just pass around a mutable context, but for now, stick with the &mut
        // Interpreter since we need to maintain the state across calls
        let mut interpreter = context.ensure_treewalk_mut();

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        install_custom_panic_hook();
        let _ = terminal::enable_raw_mode();

        self.initialize_prompt(terminal_io, false);
        loop {
            match terminal_io.read_event() {
                Ok(Event::Key(event)) => {
                    match (event.code, event.modifiers) {
                        (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                            // CPython emits a `KeyboardInterrupt` here. We could do that and then
                            // probably handle it one level up? That could help for the other
                            // panics as well.
                            self.initialize_prompt(terminal_io, true);
                            continue;
                        }
                        (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                            // TODO this probably shouldn't full-on panic, which makes it difficult
                            // to test
                            panic!("^D");
                        }
                        _ => {}
                    }

                    self.handle_key_event(terminal_io, &mut interpreter, event);
                }
                Ok(_) => {}
                Err(_) => break,
            }
        }

        // We should only get here during the tests when we throw an io error because there are no
        // more events. In interactive mode, we will exit via a panic (and the custom panic handler
        // above) or by typing exit().
        let _ = terminal::disable_raw_mode();
        let _ = panic::take_hook();
    }

    /// Update the terminal and interpreter state based on the given `KeyEvent`.
    fn handle_key_event<T: TerminalIO>(
        &mut self,
        terminal_io: &mut T,
        interpreter: &mut Interpreter,
        event: KeyEvent,
    ) {
        match event.code {
            KeyCode::Char(c) => {
                self.line.insert(self.line_index, c);
                self.line_index += 1;
                self.redraw_and_position(terminal_io);
            }
            KeyCode::Backspace => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                    self.line.remove(self.line_index);
                    self.redraw_and_position(terminal_io);
                }
            }
            KeyCode::Enter => {
                self.history.push(self.line.clone());
                self.history_index = None;

                // This newline simulates the user pressing the enter key
                let _ = terminal_io.writeln("");
                self.process_line(terminal_io, interpreter, &self.line.clone());

                self.initialize_prompt(terminal_io, false);
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
                    self.redraw_and_position(terminal_io);
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
                    self.redraw_and_position(terminal_io);
                }
            }
            KeyCode::Right => {
                if self.line_index < self.line.len() {
                    self.line_index += 1;
                    self.redraw_and_position(terminal_io);
                }
            }
            KeyCode::Left => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                    self.redraw_and_position(terminal_io);
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
    fn initialize_prompt<T: TerminalIO>(&mut self, terminal_io: &mut T, add_new_line: bool) {
        self.line.clear();
        self.line_index = 0;
        if add_new_line {
            let _ = terminal_io.writeln("");
        }
        let _ = terminal_io.write(self.prompt());
    }

    /// Clear the current input, redraw it, and align the cursor to the proper column.
    fn redraw_and_position<T: TerminalIO>(&self, terminal_io: &mut T) {
        // Redraw
        execute!(io::stdout(), Clear(ClearType::CurrentLine))
            .expect("Failed to execute terminal command");
        let _ = terminal_io.write(format!("\r{}{}", self.prompt(), self.line));

        // Position
        let cursor_col = (self.line_index + self.prompt().len()) as u16;
        execute!(io::stdout(), cursor::MoveToColumn(cursor_col))
            .expect("Failed to execute terminal command");
    }

    /// Append the provided line to the constructed statement and evaluate it through the
    /// `Interpreter`.
    fn process_line<T: TerminalIO>(
        &mut self,
        terminal_io: &mut T,
        interpreter: &mut Interpreter,
        line: &str,
    ) {
        if line.trim_end() == "exit()" {
            let _ = terminal::disable_raw_mode();
            let error_code = match self.errors.len() {
                0 => 0,
                _ => 1,
            };
            // TODO same here - this is difficult to test
            process::exit(error_code);
        }

        self.input.push_str(line);

        if self.end_of_statement(line) {
            let mut lexer = Lexer::new();
            let _ = lexer.tokenize(&self.input);
            let mut parser = Parser::new(lexer.tokens());
            match interpreter.run(&mut parser) {
                Ok(result) => {
                    if !result.is_none() {
                        let _ = terminal_io.writeln(&result);
                    }
                }
                Err(err) => {
                    self.errors.push(err.clone());
                    let _ = terminal_io.writeln(&err);
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

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use super::*;

    /// Run the complete flow, from input code string to return value string. If you need any Ctrl
    /// modifiers, do not use this!
    fn run_and_return(input: &str) -> String {
        let mut terminal = MockTerminalIO::from_str(input);
        Repl::new().run(&mut terminal);
        terminal.return_val()
    }

    fn string_to_events(input: &str) -> Vec<Event> {
        input
            .chars()
            .map(|c| {
                let key_code = match c {
                    '\n' => KeyCode::Enter,
                    _ => KeyCode::Char(c),
                };
                Event::Key(KeyEvent::new(key_code, KeyModifiers::NONE))
            })
            .collect()
    }

    /// A mock for testing that doesn't use `crossterm`.
    struct MockTerminalIO {
        /// Predefined events for testing
        events: Vec<Event>,

        /// Captured output for assertions
        output: Vec<String>,
    }

    impl MockTerminalIO {
        fn new(events: Vec<Event>) -> Self {
            Self {
                events,
                output: vec![],
            }
        }

        fn from_str(input: &str) -> Self {
            Self {
                events: string_to_events(input),
                output: vec![],
            }
        }

        /// For a populated `MockTerminalIO`, fetch the last return value.
        fn return_val(&self) -> String {
            // End of the output will be similar to this, which is why we look for the 3rd to last
            // element.
            //
            // "Traceback....NameError...",
            // "\n",
            // ">>> ",
            let third_from_last = self
                .output
                .len()
                .checked_sub(3)
                .and_then(|index| self.output.get(index))
                .expect("Not enough elements in output");

            third_from_last.to_string()
        }
    }

    impl TerminalIO for MockTerminalIO {
        fn read_event(&mut self) -> Result<Event, io::Error> {
            if self.events.is_empty() {
                Err(io::Error::new(io::ErrorKind::Other, "No more events"))
            } else {
                // remove from the front (semantically similar to VecDequeue::pop_front).
                Ok(self.events.remove(0))
            }
        }

        fn write<T: Display>(&mut self, output: T) -> io::Result<()> {
            self.output.push(format!("{}", output));
            Ok(())
        }

        fn writeln<T: Display>(&mut self, output: T) -> io::Result<()> {
            self.write(output)?;
            self.write("\n")?;
            Ok(())
        }
    }

    #[test]
    fn test_repl_name_error() {
        let return_val = run_and_return("e\n");
        assert!(return_val.contains("NameError: name 'e' is not defined"));
    }

    #[test]
    fn test_repl_expr() {
        let third_from_last = run_and_return("12345\n");
        assert_eq!(third_from_last, "12345");
    }

    #[test]
    fn test_repl_statement() {
        let return_val = run_and_return("a = 5.5\n");

        // empty string because a statement does not have a return value
        assert_eq!(return_val, "");
    }

    #[test]
    fn test_repl_function() {
        let code = r#"
def foo():
    a = 10
    return 2 * a

foo()
"#;
        let return_val = run_and_return(code);
        assert_eq!(return_val, "20");
    }

    #[test]
    fn test_repl_ctrl_c() {
        let mut events = string_to_events("123456789\n");
        let ctrl_c = Event::Key(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
        events.insert(4, ctrl_c);
        let mut terminal = MockTerminalIO::new(events);

        Repl::new().run(&mut terminal);
        assert_eq!(terminal.return_val(), "56789");
    }
}
