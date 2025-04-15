use crate::{
    domain::{ExecutionError, Source},
    treewalk::{TreewalkContext, TreewalkValue},
    MemphisError,
};

pub fn init(text: &str) -> TreewalkContext {
    TreewalkContext::new(Source::from_text(text))
}

pub fn init_path(path: &str) -> TreewalkContext {
    TreewalkContext::new(Source::from_path(path))
}

pub fn eval(text: &str) -> TreewalkValue {
    init(text).run().expect("Failed to evaluate test string!")
}

pub fn eval_expect_error(text: &str) -> ExecutionError {
    match init(text).run() {
        Ok(_) => panic!("Expected an error!"),
        Err(MemphisError::Execution(e)) => return e,
        Err(_) => panic!("Expected an execution error!"),
    };
}

pub fn run(text: &str) -> TreewalkContext {
    let mut context = init(text);
    context.run().expect("Treewalk evaluation failed!");
    context
}

pub fn run_path(path: &str) -> TreewalkContext {
    let mut context = init_path(path);
    context.run().expect("Treewalk evaluation failed!");
    context
}

pub fn run_expect_error(context: &mut TreewalkContext) -> ExecutionError {
    match context.run() {
        Ok(_) => panic!("Expected an error!"),
        Err(MemphisError::Execution(e)) => return e,
        Err(_) => panic!("Expected an execution error!"),
    };
}

pub fn read_optional(ctx: &TreewalkContext, name: &str) -> Option<TreewalkValue> {
    ctx.read(name)
}

pub fn read(ctx: &TreewalkContext, name: &str) -> TreewalkValue {
    read_optional(&ctx, name).expect("Failed to read var")
}
