use crate::bytecode_vm::runtime::Frame;

#[derive(Clone, Debug)]
pub struct Generator {
    pub frame: Frame,
    #[allow(dead_code)]
    done: bool,
}

impl Generator {
    pub fn new(frame: Frame) -> Self {
        Self { frame, done: false }
    }
}
