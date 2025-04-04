use std::time::{Duration, Instant};

use crate::core::Container;
use crate::parser::types::Statement;
use crate::treewalk::interpreter::{TreewalkDisruption, TreewalkResult, TreewalkSignal};
use crate::{
    treewalk::types::{
        domain::{
            builtins::utils,
            traits::{Callable, MethodProvider, Typed},
            Type,
        },
        pausable::{Frame, Pausable, PausableContext, PausableState, PausableStepResult},
        utils::ResolvedArguments,
        ExprResult, Function,
    },
    treewalk::{Interpreter, Scope},
};

pub enum Poll {
    Waiting,
    Ready(ExprResult),
}

/// Stateful encapsulation of a pausable `Function` with a `Scope`. This struct needs a
/// `CoroutineExecutor` to be run.
pub struct Coroutine {
    scope: Container<Scope>,
    context: Container<PausableContext>,
    wait_on: Option<Container<Coroutine>>,
    wake_at: Option<Instant>,
    return_val: Option<ExprResult>,
}

impl Typed for Coroutine {
    fn get_type() -> Type {
        Type::Coroutine
    }
}

impl MethodProvider for Coroutine {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(CloseBuiltin)]
    }
}

impl Coroutine {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableContext::new(frame),
            wait_on: None,
            wake_at: None,
            return_val: None,
        }
    }

    pub fn sleep(&mut self, duration_in_s: f64) {
        let micros = duration_in_s * 1_000_000.0;
        let diff = Duration::from_micros(micros as u64);
        self.wake_at = Some(Instant::now() + diff);
    }

    fn is_blocked(&self) -> bool {
        self.wake_at.is_some_and(|t| t > Instant::now())
            || self
                .wait_on
                .clone()
                .is_some_and(|coroutine| coroutine.borrow().is_finished().is_none())
    }

    pub fn is_finished(&self) -> Option<ExprResult> {
        self.return_val.clone()
    }

    pub fn has_work(&self) -> bool {
        !(self.is_blocked() || self.is_finished().is_some())
    }

    pub fn wait_on(&mut self, coroutine: Container<Coroutine>) {
        self.wait_on = Some(coroutine);
    }

    pub fn set_return_val(&mut self, return_val: ExprResult) {
        self.return_val = Some(return_val);
    }
}

impl Pausable for Container<Coroutine> {
    fn context(&self) -> Container<PausableContext> {
        self.borrow().context.clone()
    }

    fn scope(&self) -> Container<Scope> {
        self.borrow().scope.clone()
    }

    fn set_scope(&self, scope: Container<Scope>) {
        self.borrow_mut().scope = scope;
    }

    fn finish(&self, _interpreter: &Interpreter, result: ExprResult) -> TreewalkResult<ExprResult> {
        self.borrow_mut().set_return_val(result.clone());
        Ok(ExprResult::None)
    }

    fn handle_step(
        &self,
        interpreter: &Interpreter,
        stmt: Statement,
        control_flow: bool,
    ) -> TreewalkResult<PausableStepResult> {
        match self.execute_statement(interpreter, stmt, control_flow)? {
            Poll::Ready(val) => Ok(PausableStepResult::Return(val)),
            Poll::Waiting => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::Break)
            }
        }
    }
}

impl Container<Coroutine> {
    pub fn has_started(&self) -> bool {
        self.context().current_state() != PausableState::Created
    }

    /// Execute the next instruction in the `Frame` and return whether we hit an `await` or not. If
    /// the next instruction is a control flow statement which leads the execution into a block,
    /// the coroutine state is updated to reflect this.
    fn execute_statement(
        &self,
        interpreter: &Interpreter,
        stmt: Statement,
        control_flow: bool,
    ) -> TreewalkResult<Poll> {
        if !control_flow {
            match interpreter.evaluate_statement(&stmt) {
                // We cannot return the default value here because certain statement types may
                // actually have a return value (expression, return, etc).
                Ok(result) => Ok(Poll::Ready(result)),
                Err(TreewalkDisruption::Signal(TreewalkSignal::Sleep)) => Ok(Poll::Waiting),
                Err(TreewalkDisruption::Signal(TreewalkSignal::Await)) => {
                    self.context().step_back();
                    Ok(Poll::Waiting)
                }
                Err(TreewalkDisruption::Signal(TreewalkSignal::Return(result))) => {
                    Ok(Poll::Ready(result))
                }
                Err(e) => Err(e),
            }
        } else {
            // We return `None` here because this is the return type of all statements (with a few
            // exceptions that we don't have to worry about here).
            Ok(Poll::Ready(ExprResult::None))
        }
    }
}

struct CloseBuiltin;

// I'm not sure what coroutine.close() should do. The stdlib says this is used to prevent a
// ResourceWarning, but I'm not doing anything when I invoke a coroutine right now that would lead
// to this.
impl Callable for CloseBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 0, interpreter)?;
        Ok(ExprResult::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}
