/// These are each allowed to be dead code in case the code base happens to not have any at that
/// level at the moment.
#[derive(Debug, PartialEq, PartialOrd)]
pub enum LogLevel {
    #[allow(dead_code)]
    Error,
    #[allow(dead_code)]
    Warn,
    #[allow(dead_code)]
    Info,
    #[allow(dead_code)]
    Debug,
    #[allow(dead_code)]
    Trace,
}

static CURRENT_LOG_LEVEL: LogLevel = LogLevel::Info;

/// Accept a closure to avoid calling the construction of the formatted strings until necessary
/// (i.e. we know that our logging level asks for it).
pub fn log<F: FnOnce() -> String>(level: LogLevel, message_fn: F) {
    if level <= CURRENT_LOG_LEVEL {
        let message = message_fn();
        println!("[{:?}] {}", level, message);
    }
}

/// Sometimes we want to log something that does not return a string but we still want to respect
/// our logging level.
#[allow(dead_code)]
pub fn log_impure<F: FnOnce()>(level: LogLevel, message_fn: F) {
    if level <= CURRENT_LOG_LEVEL {
        message_fn();
    }
}
