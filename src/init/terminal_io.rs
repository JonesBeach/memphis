use std::{
    fmt::Display,
    io::{self, Write},
};

use crossterm::{
    event::{self, Event},
    terminal,
};

pub trait TerminalIO {
    fn read_event(&mut self) -> Result<Event, io::Error>;
    fn write<T: Display>(&mut self, output: T) -> io::Result<()>;
    fn writeln<T: Display>(&mut self, output: T) -> io::Result<()>;
}

pub struct CrosstermIO;

impl TerminalIO for CrosstermIO {
    /// Use `crossterm` to read events
    fn read_event(&mut self) -> Result<Event, io::Error> {
        event::read()
    }

    /// Emit output to stdout, normalizing for any needed carriage returns
    fn write<T: Display>(&mut self, output: T) -> io::Result<()> {
        print_raw(output);
        Ok(())
    }

    /// Same as `write_output` but with a `\n` char at the end.
    fn writeln<T: Display>(&mut self, output: T) -> io::Result<()> {
        println_raw(output);
        Ok(())
    }
}

/// When the terminal is in raw mode, we must emit a carriage return in addition to a newline,
/// because that does not happen automatically.
fn normalize<T: Display>(err: T) -> String {
    let formatted = format!("{}", err);
    if terminal::is_raw_mode_enabled().expect("Failed to query terminal raw mode") {
        formatted.replace("\n", "\n\r")
    } else {
        formatted.to_string()
    }
}

/// Print command which will normalize newlines + carriage returns before printing.
fn print_raw<T: Display>(val: T) {
    print!("{}", normalize(val));
    io::stdout().flush().expect("Failed to flush stdout");
}

/// Print command which will normalize newlines + carriage returns before printing and include a
/// newline at the end of the value.
fn println_raw<T: Display>(val: T) {
    print_raw(format!("{}\n", val));
}
