use crate::{
    core::Container,
    treewalk::types::{
        domain::{builtins::utils, traits::Callable},
        pausable::Pausable,
        utils::ResolvedArguments,
        Coroutine, ExprResult,
    },
    types::errors::InterpreterError,
};

use super::Interpreter;

/// An event loop which runs `Coroutine` objects using the `CoroutineExecutor` utility.
pub struct Executor {
    pub current_coroutine: Container<Option<Container<Coroutine>>>,
    running: Container<Vec<Container<Coroutine>>>,
    spawned: Container<Vec<Container<Coroutine>>>,
    to_wait: Container<Vec<(Container<Coroutine>, Container<Coroutine>)>>,

    /// In theory this does not need to be in a `Container` because it is not shared. However, we
    /// currently call `executor.call(..)` with a reference to the `Executor`, which does not allow
    /// itself to be borrowed as mutable. We may want to unwind this in the future.
    sleep_indicator: Container<Option<f64>>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    /// Create an `Executor`.
    pub fn new() -> Self {
        Self {
            current_coroutine: Container::new(None),
            running: Container::new(vec![]),
            spawned: Container::new(vec![]),
            to_wait: Container::new(vec![]),
            sleep_indicator: Container::new(None),
        }
    }

    fn set_current_coroutine(&self, coroutine: Container<Coroutine>) {
        *self.current_coroutine.borrow_mut() = Some(coroutine);
    }

    fn clear_current_coroutine(&self) {
        *self.current_coroutine.borrow_mut() = None;
    }

    /// Do the next piece of work on a given `Coroutine`. After its work is done, check to
    /// see if it was put to sleep and handle it accordingly.
    fn call(
        &self,
        interpreter: &Interpreter,
        coroutine: Container<Coroutine>,
    ) -> Result<ExprResult, InterpreterError> {
        self.set_current_coroutine(coroutine.clone());

        coroutine.run_until_pause(interpreter)?;

        if let Some(duration) = *self.sleep_indicator.borrow() {
            coroutine.borrow_mut().sleep(duration);
        }
        *self.sleep_indicator.borrow_mut() = None;

        self.clear_current_coroutine();
        Ok(ExprResult::None)
    }

    /// The main interface to the `Executor` event loop. An `ExprResult` will be returned once the
    /// coroutine has resolved.
    pub fn run(
        &self,
        interpreter: &Interpreter,
        coroutine: Container<Coroutine>,
    ) -> Result<ExprResult, InterpreterError> {
        let executor = Container::new(self);
        executor
            .borrow()
            .running
            .borrow_mut()
            .push(coroutine.clone());

        loop {
            // Run every coroutine on this event loop that has work to do.
            for c in executor.borrow().running.borrow_mut().iter() {
                if c.borrow().has_work() {
                    let _ = executor.borrow().call(interpreter, c.clone())?;
                }
            }

            for pair in executor.borrow().to_wait.borrow_mut().iter() {
                pair.0.borrow_mut().wait_on(pair.1.clone());
                if !pair.1.has_started() {
                    executor.borrow().spawn(pair.1.clone())?;
                }
            }
            executor.borrow().to_wait.borrow_mut().clear();

            // Call any coroutines spawned during this iteration and add them to the queue for the
            // next iteration.
            for c in executor.borrow().spawned.borrow_mut().iter() {
                let _ = executor.borrow().call(interpreter, c.clone())?;
                executor.borrow().running.borrow_mut().push(c.clone());
            }
            executor.borrow().spawned.borrow_mut().clear();

            // The event loop exits when its original coroutine has completed all its work. Other
            // spawned coroutines may or may not be finished by this time.
            if let Some(result) = coroutine.borrow().is_finished() {
                return Ok(result);
            }
        }
    }

    /// Launch a new `Coroutine`. This will be consumed at the end of the current iteration of the event loop.
    pub fn spawn(&self, coroutine: Container<Coroutine>) -> Result<ExprResult, InterpreterError> {
        coroutine.context().start();
        self.spawned.borrow_mut().push(coroutine.clone());
        Ok(ExprResult::Coroutine(coroutine))
    }

    pub fn sleep(&self, duration: f64) -> Result<ExprResult, InterpreterError> {
        *self.sleep_indicator.borrow_mut() = Some(duration);
        Err(InterpreterError::EncounteredSleep)
    }

    pub fn set_wait_on(&self, first: Container<Coroutine>, second: Container<Coroutine>) {
        self.to_wait
            .borrow_mut()
            .push((first.clone(), second.clone()));
    }
}

pub struct AsyncioRunBuiltin;
pub struct AsyncioSleepBuiltin;
pub struct AsyncioCreateTaskBuiltin;

impl Callable for AsyncioRunBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let coroutine =
            args.get_arg(0)
                .as_coroutine()
                .ok_or(InterpreterError::ExpectedCoroutine(
                    interpreter.state.call_stack(),
                ))?;

        let executor = interpreter.state.get_executor();
        let result = executor.borrow().run(interpreter, coroutine);
        drop(executor);
        result
    }

    fn name(&self) -> String {
        "run".into()
    }
}

impl Callable for AsyncioSleepBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let duration = args
            .get_arg(0)
            .as_fp()
            .ok_or(InterpreterError::ExpectedFloatingPoint(
                interpreter.state.call_stack(),
            ))?;

        interpreter.state.get_executor().borrow().sleep(duration)
    }

    fn name(&self) -> String {
        "sleep".into()
    }
}

impl Callable for AsyncioCreateTaskBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let coroutine =
            args.get_arg(0)
                .as_coroutine()
                .ok_or(InterpreterError::ExpectedCoroutine(
                    interpreter.state.call_stack(),
                ))?;

        interpreter.state.get_executor().borrow().spawn(coroutine)
    }

    fn name(&self) -> String {
        "create_task".into()
    }
}
