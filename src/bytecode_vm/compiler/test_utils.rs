use crate::{
    bytecode_vm::{compiler::Constant, indices::Index, VmContext},
    domain::{FunctionType, Source},
    parser::{
        test_utils::*,
        types::{Expr, Statement},
    },
};

use super::{Bytecode, CodeObject, Compiler, Opcode};

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

pub fn compile(text: &str) -> CodeObject {
    VmContext::new(Source::from_text(text))
        .compile()
        .expect("Failed to compile test program!")
}

pub fn wrap_top_level_function(name: &str, func: CodeObject) -> CodeObject {
    CodeObject {
        name: None,
        bytecode: vec![
            Opcode::LoadConst(Index::new(0)),
            Opcode::MakeFunction,
            Opcode::StoreGlobal(Index::new(0)),
            Opcode::Halt,
        ],
        arg_count: 0,
        varnames: vec![],
        freevars: vec![],
        names: vec![name.into()],
        constants: vec![Constant::Code(func)],
        source: Source::default(),
        line_map: vec![],
        function_type: FunctionType::Regular,
    }
}

pub fn wrap_top_level_class(name: &str, cls: CodeObject) -> CodeObject {
    CodeObject {
        name: None,
        bytecode: vec![
            Opcode::LoadBuildClass,
            Opcode::LoadConst(Index::new(0)),
            Opcode::Call(1),
            Opcode::StoreGlobal(Index::new(0)),
            Opcode::Halt,
        ],
        arg_count: 0,
        varnames: vec![],
        freevars: vec![],
        names: vec![name.into()],
        constants: vec![Constant::Code(cls)],
        source: Source::default(),
        line_map: vec![],
        function_type: FunctionType::Regular,
    }
}

macro_rules! assert_code_eq {
    ($actual:expr, $expected:expr) => {
        _assert_code_eq(&$actual, &$expected)
    };
}

macro_rules! compile_incremental {
        ( $( $line:expr ),* ) => {{
            let mut context = VmContext::default();
            $(
                context.add_line($line);
            )*
            context.compile().expect("Failed to compile")
        }};
    }

/// This is designed to confirm everything in a CodeObject matches besides the Source and
/// the line number mappings.
pub fn _assert_code_eq(actual: &CodeObject, expected: &CodeObject) {
    assert_eq!(actual.name, expected.name, "Code object names do not match");
    assert_eq!(
        actual.bytecode, expected.bytecode,
        "Code object bytecode does not match"
    );
    assert_eq!(
        actual.arg_count, expected.arg_count,
        "Code object arg_count does not match"
    );
    assert_eq!(
        actual.varnames, expected.varnames,
        "Code object varnames do not match"
    );
    assert_eq!(
        actual.freevars, expected.freevars,
        "Code object freevars do not match"
    );
    assert_eq!(
        actual.names, expected.names,
        "Code object names do not match"
    );
    assert_eq!(
        actual.function_type, expected.function_type,
        "Code object function types do not match"
    );

    assert_eq!(
        actual.constants.len(),
        expected.constants.len(),
        "Unequal number of code object constants"
    );

    for (i, (a_const, e_const)) in actual
        .constants
        .iter()
        .zip(expected.constants.iter())
        .enumerate()
    {
        match (a_const, e_const) {
            (Constant::Code(a_code), Constant::Code(e_code)) => {
                assert_code_eq!(a_code, e_code);
            }
            _ => {
                assert_eq!(
                    a_const, e_const,
                    "Code object constant at index {} does not match",
                    i
                );
            }
        }
    }
}

pub(crate) use assert_code_eq;
pub(crate) use compile_incremental;
