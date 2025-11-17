use std::time::{Duration, Instant};

use crate::{
    core::Container,
    domain::{DomainResult, Type},
    parser::types::Statement,
    treewalk::{
        macros::*,
        pausable::{Frame, Pausable, PausableStack, PausableState, PausableStepResult},
        protocols::Callable,
        result::Raise,
        types::Function,
        utils::{check_args, Args},
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkValue,
    },
};

pub enum Poll {
    Waiting,
    Ready(TreewalkValue),
}

/// Stateful encapsulation of a pausable `Function` with a `Scope`. This must be run by an
/// `Executor`.
pub struct Coroutine {
    scope: Container<Scope>,
    context: PausableStack,
    wait_on: Option<Container<Coroutine>>,
    wake_at: Option<Instant>,
    return_val: Option<TreewalkValue>,
}

impl_typed!(Coroutine, Type::Coroutine);
impl_method_provider!(Coroutine, [CloseBuiltin]);

impl Coroutine {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableStack::new(frame),
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
                .as_ref()
                .is_some_and(|coroutine| !coroutine.borrow().is_finished())
    }

    pub fn is_finished(&self) -> bool {
        self.return_val.is_some()
    }

    pub fn is_finished_with(&self) -> Option<TreewalkValue> {
        self.return_val.clone()
    }

    pub fn has_work(&self) -> bool {
        !(self.is_blocked() || self.is_finished())
    }

    pub fn wait_on(&mut self, coroutine: Container<Coroutine>) {
        self.wait_on = Some(coroutine);
    }

    pub fn set_return_val(&mut self, return_val: TreewalkValue) {
        self.return_val = Some(return_val);
    }

    pub fn has_started(&self) -> bool {
        self.context().current_state() != PausableState::Created
    }

    /// Execute the next instruction in the `Frame` and return whether we hit an `await` or not. If
    /// the next instruction is a control flow statement which leads the execution into a block,
    /// the coroutine state is updated to reflect this.
    fn execute_statement(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
    ) -> TreewalkResult<Poll> {
        match interpreter.evaluate_statement(&stmt) {
            // We cannot return the default value here because certain statement types may
            // actually have a return value (expression, return, etc).
            Ok(result) => Ok(Poll::Ready(result)),
            Err(TreewalkDisruption::Signal(TreewalkSignal::Sleep)) => Ok(Poll::Waiting),
            Err(TreewalkDisruption::Signal(TreewalkSignal::Await)) => {
                self.context_mut().step_back();
                Ok(Poll::Waiting)
            }
            Err(TreewalkDisruption::Signal(TreewalkSignal::Return(result))) => {
                Ok(Poll::Ready(result))
            }
            Err(e) => Err(e),
        }
    }
}

impl Pausable for Coroutine {
    fn context(&self) -> &PausableStack {
        &self.context
    }

    fn context_mut(&mut self) -> &mut PausableStack {
        &mut self.context
    }

    fn scope(&self) -> Container<Scope> {
        self.scope.clone()
    }

    fn finish(&mut self, result: TreewalkValue) -> DomainResult<TreewalkValue> {
        self.set_return_val(result.clone());
        Ok(TreewalkValue::None)
    }

    fn handle_step(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
    ) -> TreewalkResult<PausableStepResult> {
        match self.execute_statement(interpreter, stmt)? {
            Poll::Ready(val) => Ok(PausableStepResult::Return(val)),
            Poll::Waiting => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::Break)
            }
        }
    }
}

#[derive(Clone)]
struct CloseBuiltin;

// I'm not sure what coroutine.close() should do. The stdlib says this is used to prevent a
// ResourceWarning, but I'm not doing anything when I invoke a coroutine right now that would lead
// to this.
impl Callable for CloseBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "close".into()
    }
}
