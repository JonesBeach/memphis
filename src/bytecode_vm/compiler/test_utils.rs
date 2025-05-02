use crate::{
    domain::Source,
    parser::{
        test_utils::*,
        types::{Expr, Statement},
    },
};

use super::{Bytecode, Compiler};

fn init() -> Compiler {
    Compiler::new(Source::default())
}

pub fn compile_expr(expr: Expr) -> Bytecode {
    compile_stmt(stmt_expr!(expr))
}

pub fn compile_stmt(stmt: Statement) -> Bytecode {
    let mut compiler = init();
    compiler
        .compile_stmt(&stmt)
        .expect("Failed to compile test Statement!");
    compiler.bytecode()
}
