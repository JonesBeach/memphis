use std::{env, process};

#[cfg(feature = "repl")]
use memphis::init::{CrosstermIO, Repl};
use memphis::{init::Memphis, Engine};

fn main() {
    let args: Vec<String> = env::args().collect();
    let engine = Engine::from_env();

    match args.len() {
        #[cfg(feature = "repl")]
        1 => Repl::default().run(&mut CrosstermIO),
        #[cfg(not(feature = "repl"))]
        1 => {
            eprintln!("Must enable 'repl' feature flag!");
            process::exit(1);
        }
        2 => Memphis::start(&args[1], engine),
        _ => {
            eprintln!("Usage: memphis [<filename>]");
            process::exit(1);
        }
    }
}
