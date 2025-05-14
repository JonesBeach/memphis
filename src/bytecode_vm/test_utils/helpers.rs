use crate::{
    bytecode_vm::{VmContext, VmValue},
    domain::{ExecutionError, Source},
    errors::MemphisError,
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

pub fn run_path(path: &str) -> VmContext {
    let mut context = init_path(path);
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

pub fn read_attr(context: &mut VmContext, name: &str, attr: &str) -> VmValue {
    let object = read(context, name);
    let interpreter = context.interpreter();
    let reference = interpreter
        .vm()
        .resolve_raw_attr(&object, attr)
        .expect("Failed to resolve");
    interpreter
        .vm()
        .dereference(reference)
        .expect("Failed to get owned value")
}
