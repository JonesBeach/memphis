use crate::{
    bytecode_vm::{
        runtime::{components::ErrorBuilder, Frame},
        VmResult,
    },
    core::{log, Container, LogLevel},
    runtime::MemphisState,
};

/// All code which is executed lives inside a [`Frame`] on this call stack.
pub struct CallStack {
    stack: Vec<Frame>,
    state: Container<MemphisState>,
    error_builder: ErrorBuilder,
}

impl CallStack {
    pub fn new(state: Container<MemphisState>) -> Self {
        let error_builder = ErrorBuilder::new(state.clone());

        Self {
            stack: vec![],
            state,
            error_builder,
        }
    }

    pub fn push(&mut self, frame: Frame) {
        log(LogLevel::Trace, || {
            format!("Pushing frame: {}", frame.function.code_object.name())
        });
        // If we don't save the current line number, we won't properly record where in the current
        // file we called the next function from. We do something similar in the treewalk
        // interpreter.
        if !self.is_finished() {
            self.state.save_line_number();
        }
        self.state.push_stack_frame(&frame);
        self.stack.push(frame);
    }

    pub fn pop(&mut self) -> Option<Frame> {
        self.state.pop_stack_frame();

        if let Some(frame) = self.stack.pop() {
            log(LogLevel::Trace, || {
                format!("Popping frame: {}", frame.function.code_object.name())
            });
            Some(frame)
        } else {
            None
        }
    }

    pub fn top(&self) -> Option<&Frame> {
        self.stack.last()
    }

    pub fn top_mut(&mut self) -> Option<&mut Frame> {
        self.stack.last_mut()
    }

    pub fn top_frame(&self) -> VmResult<&Frame> {
        self.top().ok_or_else(|| self.error_builder.runtime_error())
    }

    pub fn top_frame_mut(&mut self) -> VmResult<&mut Frame> {
        if self.is_finished() {
            // This only uses &self, so it happens before any &mut
            let err = self.error_builder.runtime_error();
            return Err(err);
        }

        Ok(self.top_mut().expect("Unreachable"))
    }

    pub fn is_finished(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn advance_pc(&mut self) -> VmResult<()> {
        let frame = self.top_frame_mut()?;
        log(LogLevel::Trace, || {
            format!(
                "Advancing PC in module: {}",
                frame.function.code_object.name()
            )
        });
        frame.pc += 1;
        Ok(())
    }

    pub fn jump_to_offset(&mut self, offset: isize) -> VmResult<()> {
        let frame = self.top_frame_mut()?;
        frame.pc = (frame.pc as isize + offset) as usize;
        Ok(())
    }
}
