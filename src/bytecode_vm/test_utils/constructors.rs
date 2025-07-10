macro_rules! int {
    ($val:expr) => {
        $crate::bytecode_vm::VmValue::Int($val)
    };
}

pub(crate) use int;
