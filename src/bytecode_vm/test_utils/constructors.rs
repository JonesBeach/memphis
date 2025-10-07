macro_rules! int {
    ($val:expr) => {
        $crate::bytecode_vm::VmValue::Int($val)
    };
}

macro_rules! float {
    ($val:expr) => {
        $crate::bytecode_vm::VmValue::Float($val)
    };
}

macro_rules! str {
    ($val:expr) => {
        $crate::bytecode_vm::VmValue::String($val.to_string())
    };
}

macro_rules! bool {
    ($val:expr) => {
        $crate::bytecode_vm::VmValue::Bool($val)
    };
}

pub(crate) use bool;
pub(crate) use float;
pub(crate) use int;
pub(crate) use str;
