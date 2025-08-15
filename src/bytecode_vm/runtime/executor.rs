use std::time::Instant;

use crate::{
    bytecode_vm::{runtime::vm::StepResult, VmResult},
    core::Container,
};

use super::{types::CoroutineState, Coroutine, Reference, VirtualMachine};

#[derive(Default)]
pub struct VmExecutor {
    current: Option<Container<Coroutine>>,
    queue: Vec<Container<Coroutine>>,
}

impl VmExecutor {
    pub fn run(
        &mut self,
        vm: &mut VirtualMachine,
        root: Container<Coroutine>,
    ) -> VmResult<Reference> {
        self.queue.push(root.clone());

        while let Some(co) = self.queue.pop() {
            self.current = Some(co.clone());

            match co.borrow().state {
                CoroutineState::Ready => {
                    let (status, frame) = vm.run_frame_and_capture(co.borrow().frame.clone())?;
                    co.borrow_mut().frame = frame;
                    match status {
                        StepResult::YieldTo(awaited) => {
                            co.borrow_mut().waiting_on = Some(awaited.clone());
                            self.queue.push(awaited);
                        }
                        StepResult::Sleep(dur) => {
                            co.borrow_mut().sleep_until = Some(Instant::now() + dur);
                            co.borrow_mut().state = CoroutineState::Sleeping;
                        }
                        StepResult::Return(val) => {
                            co.borrow_mut().state = CoroutineState::Finished;
                            co.borrow_mut().result = Some(val);
                        }
                        StepResult::Continue => {
                            self.queue.push(co.clone());
                        }
                        StepResult::Halt | StepResult::Yield(_) => todo!("Not sure!"),
                    }
                }
                CoroutineState::Sleeping => {
                    let now = Instant::now();
                    if let Some(when) = co.borrow().sleep_until {
                        if now >= when {
                            co.borrow_mut().state = CoroutineState::Ready;
                            self.queue.push(co.clone());
                        } else {
                            self.queue.push(co.clone()); // not ready yet
                        }
                    }
                }
                CoroutineState::Waiting => {
                    // skip — child coroutine will resume us later
                }
                CoroutineState::Finished => {
                    // noop
                }
            }

            if co.same_identity(&root) && matches!(co.borrow().state, CoroutineState::Finished) {
                return Ok(co.borrow().result.unwrap_or(vm.none()));
            }
        }

        panic!("Executor queue exhausted")
    }
}
