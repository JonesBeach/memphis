use crate::{domain::MemphisValue, domain::Source, Engine, MemphisContext, MemphisError};

pub struct IncrementalContext {
    context: MemphisContext,
}

impl IncrementalContext {
    pub fn new(engine: Engine) -> Self {
        Self {
            context: MemphisContext::new(engine, Source::default()),
        }
    }

    pub fn add_line(&mut self, line: &str) {
        self.context
            .lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    pub fn run(&mut self) -> Result<MemphisValue, MemphisError> {
        self.context.run()
    }
}
