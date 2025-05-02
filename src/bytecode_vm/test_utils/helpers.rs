use crate::{
    bytecode_vm::{runtime::Object, VmContext, VmValue},
    domain::{ExecutionError, Source},
    types::errors::MemphisError,
};

pub fn init(text: &str) -> VmContext {
    VmContext::new(Source::from_text(text.trim()))
}

pub fn init_path(path: &str) -> VmContext {
    VmContext::new(Source::from_path(path))
}

pub fn eval(text: &str) -> VmValue {
    init(text).run().expect("Failed to evaluate test string")
}

pub fn eval_expect_error(text: &str) -> ExecutionError {
    match init(text).run() {
        Ok(_) => panic!("Expected an error!"),
        Err(MemphisError::Execution(e)) => return e,
        Err(_) => panic!("Expected an execution error!"),
    };
}

pub fn run(text: &str) -> VmContext {
    let mut context = init(text);
    context.run().expect("VM run failed!");
    context
}

pub fn run_expect_error(context: &mut VmContext) -> ExecutionError {
    match context.run() {
        Ok(_) => panic!("Expected an error!"),
        Err(MemphisError::Execution(e)) => return e,
        Err(_) => panic!("Expected an execution error!"),
    };
}

pub fn read(context: &mut VmContext, name: &str) -> VmValue {
    context.read(name).expect("Failed to read variable.")
}

pub fn read_attr(context: &mut VmContext, object: Object, attr: &str) -> VmValue {
    let interpreter = context.interpreter_mut();
    let attr_ref = object
        .read(attr.into(), |reference| {
            interpreter.vm().dereference(reference)
        })
        .expect(&format!("Failed to read attr \"{}\" from object", attr));
    interpreter.vm_mut().take(attr_ref)
}
