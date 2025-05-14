use crate::{
    domain::{MemphisValue, Source},
    errors::{MemphisError, MemphisResult},
    Engine, MemphisContext,
};

pub struct CrosscheckSession {
    treewalk: MemphisContext,
    vm: MemphisContext,
}

impl CrosscheckSession {
    /// Create a new session from a `Source`.
    pub fn new(source: Source) -> Self {
        let treewalk = MemphisContext::new(Engine::Treewalk, source.clone());
        let vm = MemphisContext::new(Engine::BytecodeVm, source);
        Self { treewalk, vm }
    }

    /// Run both engines; discard the return value and return the session. Useful for later reads.
    pub fn run(mut self) -> MemphisResult<Self> {
        self.treewalk.run()?;
        self.vm.run()?;
        Ok(self)
    }

    /// Read a value from both engines; confirm they return the same value, then return the value.
    pub fn read(&mut self, name: &str) -> Option<MemphisValue> {
        let a = self.treewalk.read(name)?;
        let b = self.vm.read(name)?;

        assert_eq!(a, b, "Engines returned different values");
        Some(a)
    }

    /// Run both engines; confirm they return the same value, then return the value.
    pub fn eval(&mut self) -> (MemphisValue, MemphisValue) {
        let tw_val = self.treewalk.run().expect("Treewalk run failed.");
        let vm_val = self.vm.run().expect("VM run failed.");

        (tw_val, vm_val)
    }

    /// Run both engines; confirm they return the same error, then return the error.
    pub fn run_expect_error(mut self) -> MemphisError {
        let treewalk_err = self
            .treewalk
            .run()
            .expect_err("Expected error from treewalk engine");
        let vm_err = self.vm.run().expect_err("Expected error from bytecode VM");

        assert_eq!(treewalk_err, vm_err, "Engines returned different errors");
        treewalk_err
    }
}
