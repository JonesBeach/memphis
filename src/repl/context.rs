use crate::{domain::MemphisValue, domain::Source, errors::MemphisResult, Engine, MemphisContext};

pub struct IncrementalContext {
    context: MemphisContext,
}

impl IncrementalContext {
    pub fn new(engine: Engine) -> Self {
        Self {
            context: MemphisContext::new(engine, Source::from_text("")),
        }
    }

    pub fn add_line(&mut self, line: &str) {
        self.context
            .lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.context.run()
    }
}
