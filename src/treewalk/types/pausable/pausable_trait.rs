use crate::{
    core::Container,
    parser::types::Statement,
    treewalk::{
        types::{ExprResult, Function},
        Interpreter, Scope, StackFrame,
    },
    types::errors::InterpreterError,
};

use super::{Frame, PausableContext, PausableState, PausableToken};

/// This instructs [`Pausable::run_until_pause`] what action should happen next.
pub enum PausableStepResult {
    NoOp,
    BreakAndReturn(ExprResult),
    Return(ExprResult),
    Break,
}

/// The interface for generators and coroutines, which share the ability to be paused and resumed.
pub trait Pausable {
    /// A getter for the [`PausableContext`] of a pausable function.
    fn context(&self) -> Container<PausableContext>;

    /// A getter for the [`Scope`] of a pausable function.
    fn scope(&self) -> Container<Scope>;

    /// A setter for the [`Scope`] of a pausable function.
    fn set_scope(&self, scope: Container<Scope>);

    /// A getter for the [`Function`] of a pausable function.
    fn function(&self) -> Container<Function>;

    /// A handle to perform any necessary cleanup once this function returns, including set its
    /// return value.
    fn finish(
        &self,
        interpreter: &Interpreter,
        result: ExprResult,
    ) -> Result<ExprResult, InterpreterError>;

    /// A handle to invoke the discrete operation of evaluating an individual statement and
    /// producing a [`PausableStepResult`] based on the control flow instructions and or the
    /// expression return values encountered.
    fn handle_step(
        &self,
        interpreter: &Interpreter,
        statement: Statement,
        control_flow: bool,
    ) -> Result<PausableStepResult, InterpreterError>;

    /// The default behavior which selects the next [`Statement`] and manually evaluates any
    /// control flow statements. This then calls [`Pausable::handle_step`] to set up any return
    /// values based on whether a control flow structure was encountered.
    fn step(&self, interpreter: &Interpreter) -> Result<PausableStepResult, InterpreterError> {
        let statement = self.context().next_statement();

        // Delegate to the common function for control flow
        let encountered_control_flow =
            self.execute_control_flow_statement(&statement, interpreter)?;

        self.handle_step(interpreter, statement, encountered_control_flow)
    }

    /// The default behavior required to perform the necessary context switching when entering a
    /// pausable function.
    fn on_entry(&self, interpreter: &Interpreter) {
        interpreter.state.push_local(self.scope());
        interpreter
            .state
            .push_context(StackFrame::new_function(self.function().borrow().clone()));
    }

    /// The default behavior required to perform the necessary context switching when exiting a
    /// pausable function.
    fn on_exit(&self, interpreter: &Interpreter) {
        interpreter.state.pop_context();
        if let Some(scope) = interpreter.state.pop_local() {
            self.set_scope(scope);
        }
    }

    /// This function manually executes any control flow statements. Any changes are reflected by
    /// invoking [`Container<PausableContext>::push_context`] with the new [`Frame`] and
    /// [`PausableState`].
    ///
    /// This implementation uses a stack-based control flow to remember the next instruction
    /// whenever this coroutine is awaited.
    ///
    /// A boolean is returned indicated whether a control flow statement was encountered.
    fn execute_control_flow_statement(
        &self,
        statement: &Statement,
        interpreter: &Interpreter,
    ) -> Result<bool, InterpreterError> {
        match statement {
            Statement::WhileLoop { body, condition } => {
                if interpreter.evaluate_expr(condition)?.as_boolean() {
                    self.context().push_context(PausableToken::new(
                        Frame::new(body.clone()),
                        PausableState::InWhileLoop(condition.clone()),
                    ));
                }

                Ok(true)
            }
            Statement::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                if interpreter.evaluate_expr(&if_part.condition)?.as_boolean() {
                    self.context().push_context(PausableToken::new(
                        Frame::new(if_part.block.clone()),
                        PausableState::InBlock,
                    ));

                    return Ok(true);
                }

                for elif_part in elif_parts {
                    if interpreter
                        .evaluate_expr(&elif_part.condition)?
                        .as_boolean()
                    {
                        self.context().push_context(PausableToken::new(
                            Frame::new(elif_part.block.clone()),
                            PausableState::InBlock,
                        ));

                        return Ok(true);
                    }
                }

                if let Some(else_body) = else_part {
                    self.context().push_context(PausableToken::new(
                        Frame::new(else_body.clone()),
                        PausableState::InBlock,
                    ));
                }

                Ok(true)
            }
            Statement::ForInLoop {
                index,
                iterable,
                body,
                ..
            } => {
                let items = interpreter.evaluate_expr(iterable)?.as_list().ok_or(
                    InterpreterError::ExpectedList(interpreter.state.call_stack()),
                )?;

                let mut queue = items.borrow().as_queue();

                if let Some(item) = queue.pop_front() {
                    interpreter.state.write_loop_index(index, item);
                    self.context().push_context(PausableToken::new(
                        Frame::new(body.clone()),
                        PausableState::InForLoop {
                            index: index.clone(),
                            queue: Container::new(queue),
                        },
                    ));
                }

                Ok(true)
            }
            _ => Ok(false), // only control flow statements are handled here
        }
    }

    /// Run this [`Pausable`] until it reaches a pause event.
    fn run_until_pause(&self, interpreter: &Interpreter) -> Result<ExprResult, InterpreterError> {
        self.on_entry(interpreter);

        let mut result = ExprResult::None;
        loop {
            match self.context().current_state() {
                PausableState::Created => {
                    self.context().start();
                }
                PausableState::Running => {
                    if self.context().current_frame().is_finished() {
                        self.context().set_state(PausableState::Finished);
                        self.on_exit(interpreter);
                        return self.finish(interpreter, result);
                    }

                    match self.step(interpreter)? {
                        PausableStepResult::NoOp => {}
                        PausableStepResult::BreakAndReturn(val) => {
                            break Ok(val);
                        }
                        PausableStepResult::Return(val) => {
                            result = val;
                        }
                        PausableStepResult::Break => {
                            break Ok(ExprResult::Void);
                        }
                    };
                }
                PausableState::InForLoop { index, queue } => {
                    if self.context().current_frame().is_finished() {
                        let item = queue.borrow_mut().pop_front();
                        if let Some(item) = item {
                            interpreter.state.write_loop_index(&index, item);
                            self.context().restart_frame();
                        } else {
                            self.context().pop_context();
                            continue;
                        }
                    }

                    match self.step(interpreter)? {
                        PausableStepResult::NoOp => {}
                        PausableStepResult::BreakAndReturn(val) => {
                            break Ok(val);
                        }
                        PausableStepResult::Return(val) => {
                            result = val;
                        }
                        PausableStepResult::Break => {
                            break Ok(ExprResult::Void);
                        }
                    };
                }
                PausableState::InBlock => {
                    if self.context().current_frame().is_finished() {
                        self.context().pop_context();
                        continue;
                    }

                    match self.step(interpreter)? {
                        PausableStepResult::NoOp => {}
                        PausableStepResult::BreakAndReturn(val) => {
                            break Ok(val);
                        }
                        PausableStepResult::Return(val) => {
                            result = val;
                        }
                        PausableStepResult::Break => {
                            break Ok(ExprResult::Void);
                        }
                    };
                }
                PausableState::InWhileLoop(condition) => {
                    if self.context().current_frame().is_finished() {
                        self.context().pop_context();
                        continue;
                    }

                    match self.step(interpreter)? {
                        PausableStepResult::NoOp => {}
                        PausableStepResult::BreakAndReturn(val) => {
                            break Ok(val);
                        }
                        PausableStepResult::Return(val) => {
                            result = val;
                        }
                        PausableStepResult::Break => {
                            break Ok(ExprResult::Void);
                        }
                    };

                    if self.context().current_frame().is_finished()
                        && interpreter.evaluate_expr(&condition)?.as_boolean()
                    {
                        self.context().restart_frame();
                    }
                }
                PausableState::Finished => unreachable!(),
            }
        }
    }
}
