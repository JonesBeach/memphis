use std::{collections::VecDeque, time::Instant};

use crate::{
    bytecode_vm::{runtime::vm::StepResult, VmResult},
    core::{log, log_impure, Container, LogLevel},
};

use super::{types::CoroutineState, Coroutine, Reference, VirtualMachine};

#[derive(Default)]
pub struct VmExecutor {
    ready_queue: VecDeque<Container<Coroutine>>,
    sleeping: Vec<Container<Coroutine>>,
}

impl VmExecutor {
    pub fn spawn(&mut self, coroutine: Container<Coroutine>) {
        log(LogLevel::Debug, || {
            format!("Spawning: {:?}", coroutine.borrow())
        });

        self.enqueue(coroutine);
    }

    /// Runs the event loop until the given root coroutine completes, returning its final value.
    ///
    /// This method takes a raw pointer to the `VirtualMachine` instead of a mutable reference
    /// because the executor is part of the VM itself (`vm.executor`). Calling this function
    /// requires mutable access to both the executor and the VM as a whole, which would violate
    /// Rust's borrowing rules. By taking a raw pointer, we delay the borrow and manually enforce
    /// that only one mutable access occurs at a time.
    ///
    /// # Safety
    /// The `vm` pointer must be valid and point to a `VirtualMachine` that outlives the duration
    /// of this call. The caller must ensure exclusive access during the function's execution.
    pub fn run_until_complete(
        &mut self,
        vm: *mut VirtualMachine,
        root: Container<Coroutine>,
    ) -> VmResult<Reference> {
        self.enqueue(root.clone());

        while self.has_work() {
            log_impure(LogLevel::Debug, || self.dump_executor());

            self.awake_any_finished_sleepers();

            // if no ready tasks, just yield until something wakes up. This just tells the OS that
            // it is free to run another thread before us.
            // TODO we should probably use `sleep` rather than `yield_now`, given that we can tell
            // when the next coroutine will awake
            if self.ready_queue.is_empty() {
                std::thread::yield_now();
                continue;
            }

            let coroutine = self.dequeue()?;

            let step_result = get_vm_mut(vm).step_coroutine(coroutine.clone())?;
            self.handle_step_result(coroutine.clone(), step_result);

            if coroutine.same_identity(&root) {
                if let CoroutineState::Finished(val) = coroutine.borrow().state {
                    return Ok(val);
                }
            }
        }

        panic!("Executor queue exhausted")
    }

    fn dump_executor(&self) {
        if self.ready_queue.is_empty() {
            println!("    Queue is empty");
        } else {
            let items: Vec<String> = self
                .ready_queue
                .iter()
                .map(|c| format!("{:?}", c.borrow()))
                .collect();
            println!("    Queue contents: [{}]", items.join(", "))
        }

        if self.sleeping.is_empty() {
            println!("    Sleeping is empty");
        } else {
            let items: Vec<String> = self
                .sleeping
                .iter()
                .map(|c| format!("{:?}", c.borrow()))
                .collect();
            println!("    Sleeping contents: [{}]", items.join(", "))
        }
    }

    fn handle_step_result(&mut self, co: Container<Coroutine>, step_result: StepResult) {
        match step_result {
            StepResult::Await(awaited) => {
                // Suspend the parent
                co.borrow_mut().state = CoroutineState::WaitingOn(awaited.clone());

                // Record that the child has someone waiting on it
                awaited.borrow_mut().waiters.push(co.clone());

                // What if awaited hasn't ever been started? Is that possible?
                // self.queue.push(awaited);
            }
            StepResult::Sleep(dur) => {
                co.borrow_mut().state = CoroutineState::SleepingUntil(Instant::now() + dur);
                self.sleeping.push(co.clone());
            }
            StepResult::Return(val) => {
                co.borrow_mut().state = CoroutineState::Finished(val);

                // Wake any parents waiting on this coroutine
                let waiters = std::mem::take(&mut co.borrow_mut().waiters);
                for parent in waiters {
                    self.requeue(parent);
                }
            }
            StepResult::Continue => {
                self.enqueue(co.clone());
            }
            StepResult::Yield(_) => {
                unimplemented!("Async generators not currently supported in the bytecode VM.")
            }
            StepResult::Halt => panic!("Halt inside async runtime!"),
        }
    }

    /// Are there any outstanding Coroutines being managed by this executor?
    fn has_work(&self) -> bool {
        !self.ready_queue.is_empty() || !self.sleeping.is_empty()
    }

    /// Revive any sleepers that are ready
    fn awake_any_finished_sleepers(&mut self) {
        let now = Instant::now();
        let mut i = 0;
        while i < self.sleeping.len() {
            let sleeper = self.sleeping[i].clone();
            let state = { sleeper.borrow().state.clone() };
            if let CoroutineState::SleepingUntil(when) = state {
                if now >= when {
                    self.requeue(sleeper);
                    self.sleeping.remove(i);

                    // don't advance i, because we just shortened the length of the vector
                    continue;
                }
            }
            i += 1;
        }
    }

    /// Enqueue a `Coroutine` for execution, but first mark it as `Ready`.
    fn requeue(&mut self, coroutine: Container<Coroutine>) {
        coroutine.borrow_mut().state = CoroutineState::Ready;
        self.enqueue(coroutine);
    }

    /// Enqueue a coroutine that is in the `Ready` state and not already present.
    ///
    /// # Panics
    /// - If the coroutine is not in `Ready` state.
    /// - If it is already in the ready queue.
    fn enqueue(&mut self, coroutine: Container<Coroutine>) {
        assert!(
            matches!(coroutine.borrow().state, CoroutineState::Ready),
            "Only Ready coroutines should live on the ready_queue."
        );
        assert!(
            !self.ready_queue.iter().any(|c| c.same_identity(&coroutine)),
            "Coroutine was double-queued!"
        );
        self.ready_queue.push_back(coroutine);
    }

    /// Dequeue a coroutine that is in the `Ready` state.
    ///
    /// # Panics
    /// - If the ready queue is empty.
    /// - If the coroutine is not in `Ready` state.
    fn dequeue(&mut self) -> VmResult<Container<Coroutine>> {
        let coroutine = self
            .ready_queue
            .pop_front()
            .expect("Attempted to dequeue from an empty ready queue.");
        assert!(
            matches!(coroutine.borrow().state, CoroutineState::Ready),
            "Only Ready coroutines should live on the ready_queue."
        );
        log(LogLevel::Debug, || {
            format!("==> Begining work on {:?}", coroutine.borrow())
        });
        Ok(coroutine)
    }
}

/// # Safety
/// The pointer must be valid and point to a `VirtualMachine` that outlives the returned reference.
fn get_vm_mut<'a>(ptr: *mut VirtualMachine) -> &'a mut VirtualMachine {
    unsafe { &mut *ptr }
}
