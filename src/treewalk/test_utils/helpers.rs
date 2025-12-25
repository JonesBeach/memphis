use crate::{
    domain::Source,
    treewalk::{RaisedException, TreewalkContext, TreewalkValue},
};

fn init(text: &str) -> TreewalkContext {
    TreewalkContext::new(Source::from_text(text))
}

fn init_path(path: &str) -> TreewalkContext {
    TreewalkContext::new(Source::from_path(path).expect("Failed to create Source"))
}

pub fn eval(text: &str) -> TreewalkValue {
    init(text)
        .run_inner()
        .expect("Failed to evaluate test string!")
}

pub fn eval_expect_error(text: &str) -> RaisedException {
    match init(text).run_inner() {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e,
    };
}

pub fn run(text: &str) -> TreewalkContext {
    let mut context = init(text);
    context.run_inner().expect("Treewalk evaluation failed!");
    context
}

pub fn run_path(path: &str) -> TreewalkContext {
    let mut context = init_path(path);
    context.run_inner().expect("Treewalk evaluation failed!");
    context
}

pub fn run_path_expect_error(path: &str) -> RaisedException {
    let mut context = init_path(path);
    match context.run_inner() {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e,
    };
}

pub fn read_optional(ctx: &TreewalkContext, name: &str) -> Option<TreewalkValue> {
    ctx.read_inner(name)
}

pub fn read(ctx: &TreewalkContext, name: &str) -> TreewalkValue {
    read_optional(&ctx, name).expect(&format!("Failed to read var: {}", name))
}
