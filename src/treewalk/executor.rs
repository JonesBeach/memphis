use std::mem::take;

use crate::{
    core::Container,
    treewalk::{
        pausable::Pausable,
        protocols::Callable,
        types::Coroutine,
        utils::{check_args, Args},
        TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal, TreewalkValue,
    },
};

/// An event loop which runs `Coroutine` objects.
pub struct Executor {
    current_coroutine: Option<Container<Coroutine>>,
    running: Vec<Container<Coroutine>>,
    spawned: Vec<Container<Coroutine>>,
    to_wait: Vec<(Container<Coroutine>, Container<Coroutine>)>,
    sleep_indicator: Option<f64>,
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
            current_coroutine: None,
            running: vec![],
            spawned: vec![],
            to_wait: vec![],
            sleep_indicator: None,
        }
    }

    pub fn current_coroutine(&self) -> &Option<Container<Coroutine>> {
        &self.current_coroutine
    }

    pub fn set_wait_on(&mut self, first: Container<Coroutine>, second: Container<Coroutine>) {
        self.to_wait.push((first.clone(), second.clone()));
    }

    /// The main interface to the `Executor` event loop. An `TreewalkValue` will be returned once
    /// the coroutine has resolved.
    fn run(
        &mut self,
        interpreter: &TreewalkInterpreter,
        coroutine: Container<Coroutine>,
    ) -> TreewalkResult<TreewalkValue> {
        self.running.push(coroutine.clone());

        loop {
            // Take the current queue of running coroutines
            let to_run = take(&mut self.running);
            for c in &to_run {
                if c.borrow().has_work() {
                    self.step_coroutine(interpreter, c.clone())?;
                }
            }
            // Push them back in for the next round
            self.running.extend(to_run);

            // Same pattern for to_wait, except we don't need to push them back
            let to_wait = take(&mut self.to_wait);
            for (first, second) in &to_wait {
                first.borrow_mut().wait_on(second.clone());
                if !second.borrow().has_started() {
                    self.spawn(second.clone())?;
                }
            }

            // Same pattern for spawned, which we also don't need to push back
            let new_spawns = take(&mut self.spawned);
            for c in &new_spawns {
                self.step_coroutine(interpreter, c.clone())?;
                self.running.push(c.clone());
            }

            // The event loop exits when its original coroutine has completed all its work. Other
            // spawned coroutines may or may not be finished by this time.
            if let Some(result) = coroutine.borrow().is_finished_with() {
                return Ok(result);
            }
        }
    }

    /// Launch a new `Coroutine`. This will be consumed at the end of the current iteration of the
    /// event loop.
    fn spawn(&mut self, coroutine: Container<Coroutine>) -> TreewalkResult<TreewalkValue> {
        coroutine.borrow_mut().context_mut().start();
        self.spawned.push(coroutine.clone());
        Ok(TreewalkValue::Coroutine(coroutine))
    }

    fn sleep(&mut self, duration: f64) -> TreewalkResult<TreewalkValue> {
        self.sleep_indicator = Some(duration);
        Err(TreewalkDisruption::Signal(TreewalkSignal::Sleep))
    }

    /// Do the next piece of work on a given `Coroutine`. After its work is done, check to
    /// see if it was put to sleep and handle it accordingly.
    fn step_coroutine(
        &mut self,
        interpreter: &TreewalkInterpreter,
        coroutine: Container<Coroutine>,
    ) -> TreewalkResult<()> {
        self.current_coroutine = Some(coroutine.clone());
        coroutine.borrow_mut().run_until_pause(interpreter)?;

        if let Some(duration) = self.sleep_indicator.take() {
            coroutine.borrow_mut().sleep(duration);
        }

        self.current_coroutine = None;
        Ok(())
    }
}

#[derive(Clone)]
pub struct AsyncioRunBuiltin;
#[derive(Clone)]
pub struct AsyncioSleepBuiltin;
#[derive(Clone)]
pub struct AsyncioCreateTaskBuiltin;

impl Callable for AsyncioRunBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let coroutine = args.get_arg(0).expect_coroutine(interpreter)?;
        interpreter.with_executor(|exec| exec.run(interpreter, coroutine))
    }

    fn name(&self) -> String {
        "run".into()
    }
}

impl Callable for AsyncioSleepBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;
        let duration = args.get_arg(0).expect_float(interpreter)?;
        interpreter.with_executor(|exec| exec.sleep(duration))
    }

    fn name(&self) -> String {
        "sleep".into()
    }
}

impl Callable for AsyncioCreateTaskBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let coroutine = args.get_arg(0).expect_coroutine(interpreter)?;
        interpreter.with_executor(|exec| exec.spawn(coroutine))
    }

    fn name(&self) -> String {
        "create_task".into()
    }
}
