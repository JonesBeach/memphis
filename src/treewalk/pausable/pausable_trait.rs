use crate::{
    core::Container,
    parser::types::{Statement, StatementKind},
    treewalk::{types::List, Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue},
};

use super::{Frame, PausableContext, PausableState, PausableToken};

/// This instructs [`Pausable::run_until_pause`] what action should happen next.
pub enum PausableStepResult {
    NoOp,
    BreakAndReturn(TreewalkValue),
    Return(TreewalkValue),
    Break,
}

/// The interface for generators and coroutines, which share the ability to be paused and resumed.
pub trait Pausable {
    /// A getter for the [`PausableContext`] of a pausable function.
    fn context(&self) -> &PausableContext;

    fn context_mut(&mut self) -> &mut PausableContext;

    /// A getter for the [`Scope`] of a pausable function.
    fn scope(&self) -> Container<Scope>;

    /// A handle to perform any necessary cleanup once this function returns, including set its
    /// return value.
    fn finish(
        &mut self,
        interpreter: &TreewalkInterpreter,
        result: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue>;

    /// A handle to invoke the discrete operation of evaluating an individual statement and
    /// producing a [`PausableStepResult`] based on the control flow instructions and or the
    /// expression return values encountered.
    fn handle_step(
        &mut self,
        interpreter: &TreewalkInterpreter,
        statement: Statement,
        control_flow: bool,
    ) -> TreewalkResult<PausableStepResult>;

    /// The default behavior which selects the next [`Statement`] and manually evaluates any
    /// control flow statements. This then calls [`Pausable::handle_step`] to set up any return
    /// values based on whether a control flow structure was encountered.
    fn step(&mut self, interpreter: &TreewalkInterpreter) -> TreewalkResult<PausableStepResult> {
        let statement = self.context_mut().next_statement();

        // Delegate to the common function for control flow
        let encountered_control_flow =
            self.execute_control_flow_statement(&statement, interpreter)?;

        self.handle_step(interpreter, statement, encountered_control_flow)
    }

    /// The default behavior required to perform the necessary context switching when entering a
    /// pausable function.
    /// TODO we used to push a new stack frame here, perhaps we need do to that for each statement
    /// now?
    fn on_entry(&self, interpreter: &TreewalkInterpreter) {
        interpreter.state.push_local(self.scope());
    }

    /// The default behavior required to perform the necessary context switching when exiting a
    /// pausable function.
    fn on_exit(&mut self, interpreter: &TreewalkInterpreter) {
        interpreter.state.pop_local();
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
        &mut self,
        stmt: &Statement,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<bool> {
        match &stmt.kind {
            StatementKind::WhileLoop(cond_ast) => {
                if interpreter.evaluate_expr(&cond_ast.condition)?.as_boolean() {
                    self.context_mut().push(PausableToken::new(
                        Frame::new(cond_ast.ast.clone()),
                        PausableState::InWhileLoop(cond_ast.condition.clone()),
                    ));
                }

                Ok(true)
            }
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                if interpreter.evaluate_expr(&if_part.condition)?.as_boolean() {
                    self.context_mut().push(PausableToken::new(
                        Frame::new(if_part.ast.clone()),
                        PausableState::InBlock,
                    ));

                    return Ok(true);
                }

                for elif_part in elif_parts {
                    if interpreter
                        .evaluate_expr(&elif_part.condition)?
                        .as_boolean()
                    {
                        self.context_mut().push(PausableToken::new(
                            Frame::new(elif_part.ast.clone()),
                            PausableState::InBlock,
                        ));

                        return Ok(true);
                    }
                }

                if let Some(else_body) = else_part {
                    self.context_mut().push(PausableToken::new(
                        Frame::new(else_body.clone()),
                        PausableState::InBlock,
                    ));
                }

                Ok(true)
            }
            StatementKind::ForInLoop {
                index,
                iterable,
                body,
                ..
            } => {
                let items: Container<List> = interpreter
                    .evaluate_expr(iterable)?
                    .try_into()
                    .map_err(|_| interpreter.type_error("Expected an iterable"))?;

                let mut queue = items.borrow().as_queue();

                if let Some(item) = queue.pop_front() {
                    interpreter.write_loop_index(index, item);
                    self.context_mut().push(PausableToken::new(
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
    fn run_until_pause(
        &mut self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<TreewalkValue> {
        self.on_entry(interpreter);

        let mut result = TreewalkValue::None;
        loop {
            match self.context().current_state() {
                PausableState::Created => {
                    self.context_mut().start();
                }
                PausableState::Running => {
                    if self.context().current_frame().is_finished() {
                        self.context_mut().set_state(PausableState::Finished);
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
                            break Ok(TreewalkValue::None);
                        }
                    };
                }
                PausableState::InForLoop { index, queue } => {
                    if self.context().current_frame().is_finished() {
                        let item = queue.borrow_mut().pop_front();
                        if let Some(item) = item {
                            interpreter.write_loop_index(&index, item);
                            self.context_mut().restart_frame();
                        } else {
                            self.context_mut().pop();
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
                            break Ok(TreewalkValue::None);
                        }
                    };
                }
                PausableState::InBlock => {
                    if self.context().current_frame().is_finished() {
                        self.context_mut().pop();
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
                            break Ok(TreewalkValue::None);
                        }
                    };
                }
                PausableState::InWhileLoop(condition) => {
                    if self.context().current_frame().is_finished() {
                        self.context_mut().pop();
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
                            break Ok(TreewalkValue::None);
                        }
                    };

                    if self.context().current_frame().is_finished()
                        && interpreter.evaluate_expr(&condition)?.as_boolean()
                    {
                        self.context_mut().restart_frame();
                    }
                }
                PausableState::Finished => unreachable!(),
            }
        }
    }
}
