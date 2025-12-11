use crate::{
    bytecode_vm::{
        compiler::{Bytecode, CodeObject, Compiler, Constant, Opcode},
        indices::Index,
        CompilerError, VmContext,
    },
    domain::{FunctionType, ModuleName, Source},
    errors::MemphisError,
    parser::{
        test_utils::*,
        types::{ast, Expr, Statement},
    },
};

fn init() -> Compiler {
    Compiler::new(ModuleName::main(), "compiler_unit_test")
}

pub fn compile_expr(expr: Expr) -> Bytecode {
    let mut bytecode = compile_stmt(stmt_expr!(expr));

    // Some expressions end with a PopTop since we are not saving their value here, we are safe to
    // strip that away for tests.
    if let Some(&Opcode::PopTop) = bytecode.last() {
        bytecode.pop();
    }

    bytecode
}

pub fn compile_stmt(stmt: Statement) -> Bytecode {
    let mut compiler = init();
    let ast = ast![stmt];
    compiler
        .compile(&ast)
        .expect("Failed to compile test Statement!");
    compiler.bytecode()
}

pub fn compile(text: &str) -> CodeObject {
    VmContext::new(Source::from_text(text))
        .compile()
        .expect("Failed to compile test program!")
}

pub fn compile_at_module(text: &str, module_name: ModuleName) -> CodeObject {
    let mut ctx = VmContext::new(Source::from_text(text));
    ctx.set_module_name(module_name);
    ctx.compile().expect("Failed to compile test program!")
}

pub fn compile_err(text: &str) -> CompilerError {
    match VmContext::new(Source::from_text(text)).compile() {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(MemphisError::Compiler(e)) => e,
        Err(_) => panic!("Expected a CompilerError!"),
    }
}

pub fn compile_err_at_module(text: &str, module_name: ModuleName) -> CompilerError {
    let mut ctx = VmContext::new(Source::from_text(text));
    ctx.set_module_name(module_name);
    match ctx.compile() {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(MemphisError::Compiler(e)) => e,
        Err(_) => panic!("Expected a CompilerError!"),
    }
}

pub fn wrap_top_level_function(func: CodeObject) -> CodeObject {
    CodeObject {
        module_name: ModuleName::main(),
        name: "<module>".into(),
        filename: "<stdin>".into(),
        bytecode: vec![
            Opcode::LoadConst(Index::new(0)),
            Opcode::MakeFunction,
            Opcode::StoreGlobal(Index::new(0)),
            Opcode::Halt,
        ],
        arg_count: 0,
        varnames: vec![],
        freevars: vec![],
        names: vec![func.name().into()],
        constants: vec![Constant::Code(func)],
        line_map: vec![],
        function_type: FunctionType::Regular,
    }
}

pub fn wrap_top_level_class(name: &str, cls: CodeObject) -> CodeObject {
    CodeObject {
        module_name: ModuleName::main(),
        name: "<module>".into(),
        filename: "<stdin>".into(),
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
            let mut context = $crate::bytecode_vm::VmContext::default();
            $(
                context.add_line_inner($line);
            )*
            context.compile().expect("Failed to compile")
        }};
    }

/// This is designed to confirm everything in a CodeObject matches besides the Source and
/// the line number mappings.
pub fn _assert_code_eq(actual: &CodeObject, expected: &CodeObject) {
    assert_eq!(actual.name, expected.name, "Code object names do not match");
    assert_eq!(
        actual.filename, expected.filename,
        "Code object filenames do not match"
    );
    assert_eq!(
        actual.module_name, expected.module_name,
        "Code object module name do not match"
    );
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
