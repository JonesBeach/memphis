use std::{env, process};

use memphis::{Engine, Memphis};

fn main() {
    let args: Vec<String> = env::args().collect();
    let engine = Engine::from_env();

    match args.len() {
        #[cfg(feature = "repl")]
        1 => Memphis::run_repl(engine),
        #[cfg(not(feature = "repl"))]
        1 => {
            eprintln!("Must enable 'repl' feature flag!");
            process::exit(1);
        }
        2 => Memphis::run_script(&args[1], engine),
        _ => {
            eprintln!("Usage: memphis [<filename>]");
            process::exit(1);
        }
    }
}
