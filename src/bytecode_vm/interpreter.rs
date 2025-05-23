use crate::{
    bytecode_vm::{compiler::CodeObject, Compiler, Runtime, VirtualMachine, VmValue},
    core::{log, Container, Interpreter, LogLevel},
    domain::{MemphisValue, Source},
    errors::{MemphisError, MemphisResult},
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmInterpreter {
    compiler: Compiler,
    vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new(
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
        source: Source,
    ) -> Self {
        Self {
            compiler: Compiler::new(source.clone()),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn compile(&mut self, parser: &mut Parser) -> MemphisResult<CodeObject> {
        let mut ast = parser.parse().map_err(MemphisError::Parser)?;
        // This simulations CPython `eval` mode, which we assume to be for the tests and REPL.
        // TODO make this explicit with `CompileMode` or similar.
        ast.rewrite_last_expr_to_return();
        self.compiler.compile(&ast).map_err(MemphisError::Compiler)
    }

    pub fn execute(&mut self, parser: &mut Parser) -> MemphisResult<VmValue> {
        let code = self.compile(parser)?;
        log(LogLevel::Trace, || format!("{}", code));
        self.vm.execute(code).map_err(MemphisError::Execution)
    }

    pub fn read_global(&mut self, name: &str) -> Option<VmValue> {
        self.vm.read_global(name)
    }
}

impl Interpreter for VmInterpreter {
    fn run(&mut self, parser: &mut Parser) -> MemphisResult<MemphisValue> {
        self.execute(parser).map(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_global(name).map(Into::into)
    }
}

#[cfg(test)]
mod tests_vm_interpreter {
    use super::*;

    use crate::{
        bytecode_vm::{
            runtime::{List, Reference},
            test_utils::*,
            VmResult,
        },
        domain::test_utils::*,
    };

    impl VmInterpreter {
        pub fn vm(&self) -> &VirtualMachine {
            &self.vm
        }
    }

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        assert_eval_eq!(text, VmValue::Int(20));

        let text = "4 > x";
        let e = eval_expect_error(text);
        assert_name_error!(e, "x");

        let text = "y()";
        let e = eval_expect_error(text);
        assert_name_error!(e, "y");
    }

    #[test]
    fn binary_addition() {
        let text = "4 + 3";
        assert_eval_eq!(text, VmValue::Int(7));

        let text = "4.1 + 3.01";
        assert_eval_eq!(text, VmValue::Float(7.11));

        let text = "4 + 3.01";
        assert_eval_eq!(text, VmValue::Float(7.01));

        let text = "4.1 + 3";
        assert_eval_eq!(text, VmValue::Float(7.1));

        let text = "-4 + 3";
        assert_eval_eq!(text, VmValue::Int(-1));

        let text = "4.1 + 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for +");
    }

    #[test]
    fn binary_subtraction() {
        let text = "4 - 3";
        assert_eval_eq!(text, VmValue::Int(1));

        let text = "4.1 - 3.01";
        assert_eval_eq!(text, VmValue::Float(1.09));

        let text = "4 - 3.01";
        assert_eval_eq!(text, VmValue::Float(0.99));

        let text = "4.1 - 3";
        assert_eval_eq!(text, VmValue::Float(1.1));

        let text = "-4 - 3";
        assert_eval_eq!(text, VmValue::Int(-7));

        let text = "4.1 - 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for -");
    }

    #[test]
    fn binary_multiplication() {
        let text = "4 * 3";
        assert_eval_eq!(text, VmValue::Int(12));

        let text = "4.1 * 3.01";
        assert_eval_eq!(text, VmValue::Float(12.341));

        let text = "4 * 3.01";
        assert_eval_eq!(text, VmValue::Float(12.04));

        let text = "4.1 * 3";
        assert_eval_eq!(text, VmValue::Float(12.3));

        let text = "-4 * 3";
        assert_eval_eq!(text, VmValue::Int(-12));

        let text = "4 * 'a'";
        assert_eval_eq!(text, VmValue::String("aaaa".to_string()));

        let text = "'b' * 3";
        assert_eval_eq!(text, VmValue::String("bbb".to_string()));

        let text = "-4 * 'a'";
        assert_eval_eq!(text, VmValue::String("".to_string()));

        let text = "'b' * -3";
        assert_eval_eq!(text, VmValue::String("".to_string()));

        let text = "4.1 * 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for *");
    }

    #[test]
    fn binary_division() {
        let text = "4 / 3";
        assert_eval_eq!(text, VmValue::Float(1.33333333333));

        let text = "-4 / 3";
        assert_eval_eq!(text, VmValue::Float(-1.33333333333));

        let text = "4.1 / 3.01";
        assert_eval_eq!(text, VmValue::Float(1.36212624585));

        let text = "4 / 3.01";
        assert_eval_eq!(text, VmValue::Float(1.32890365449));

        let text = "4.1 / 3";
        assert_eval_eq!(text, VmValue::Float(1.36666666667));

        let text = "4.1 / 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for /");
    }

    #[test]
    fn comparison_eq() {
        let text = "4 == 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4 == 4";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4.1 == 4.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4 == 4.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = r#""a" == "a""#;
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = r#""a" == "b""#;
        assert_eval_eq!(text, VmValue::Bool(false));
    }

    #[test]
    fn comparison_less_than() {
        let text = "4 < 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6 < 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "5 < 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4.1 < 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 < 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4 < 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6 < 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4.1 < 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 < 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 < 6.1";
        assert_eval_eq!(text, VmValue::Bool(false));
    }

    #[test]
    fn comparison_less_than_or_equal() {
        let text = "4 <= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6 <= 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "5 <= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4.1 <= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 <= 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4 <= 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6 <= 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4.1 <= 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 <= 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 <= 6.1";
        assert_eval_eq!(text, VmValue::Bool(true));
    }

    #[test]
    fn comparison_greater_than() {
        let text = "4 > 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6 > 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "5 > 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "4.1 > 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 > 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4 > 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6 > 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4.1 > 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 > 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 > 6.1";
        assert_eval_eq!(text, VmValue::Bool(false));
    }

    #[test]
    fn comparison_greater_than_or_equal() {
        let text = "4 >= 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6 >= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "5 >= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4.1 >= 5";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 >= 5";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4 >= 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6 >= 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "4.1 >= 5.1";
        assert_eval_eq!(text, VmValue::Bool(false));

        let text = "6.1 >= 5.1";
        assert_eval_eq!(text, VmValue::Bool(true));

        let text = "6.1 >= 6.1";
        assert_eval_eq!(text, VmValue::Bool(true));
    }

    #[test]
    fn lists() {
        let text = "[2,3]";
        assert_eval_eq!(
            text,
            VmValue::List(List::new(vec![Reference::Int(2), Reference::Int(3)]))
        );

        let text = r#"
x = [2,"Hello"]
"#;
        let mut ctx = run(text);
        let binding = read(&mut ctx, "x");
        let list = binding.as_list().unwrap();
        let values = list
            .items
            .iter()
            .map(|r| ctx.interpreter().vm().deref(*r))
            .collect::<VmResult<Vec<_>>>()
            .unwrap();
        assert_eq!(
            values,
            vec![VmValue::Int(2), VmValue::String("Hello".to_string())]
        );
    }

    #[test]
    fn assignment_int() {
        let text = r#"
a = 5 - 3
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Int(2));
    }

    #[test]
    fn assignment_str() {
        let text = r#"
a = "Hello World"
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::String("Hello World".into()));
    }

    #[test]
    fn assignment_none() {
        let text = r#"
a = 5 - 3
b = 10
c = None
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Int(2));
        assert_read_eq!(ctx, "b", VmValue::Int(10));
        assert_read_eq!(ctx, "c", VmValue::None);
    }

    #[test]
    fn assignment_var() {
        let text = r#"
a = 5 - 3
b = 10 + a
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Int(2));
        assert_read_eq!(ctx, "b", VmValue::Int(12));
    }

    #[test]
    fn while_loop() {
        let text = r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "i", VmValue::Int(4));
    }

    #[test]
    fn function_call_with_parameters() {
        let text = r#"
def foo(a, b):
    return a + b

c = foo(2, 9)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "c", VmValue::Int(11));
    }

    #[test]
    fn function_call_with_local_var() {
        let text = r#"
def foo(a, b):
    c = 9
    return a + b + c

d = foo(2, 9)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "d", VmValue::Int(20));
    }

    #[test]
    fn function_call_with_no_return() {
        let text = r#"
def hello():
    print("Hello")

def world():
    print("World")

hello()
world()
"#;
        // TODO should this do something?
        let _ = run(text);
    }

    #[test]
    fn function_call_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner(c, d):
        return c * d
    return a + b + inner(a, b)

c = foo(2, 9)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "c", VmValue::Int(29));
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar():
        return 4
"#;
        let mut ctx = run(text);
        let class = extract!(ctx, "Foo", Class);
        assert_eq!(class.name(), "Foo");
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 4

f = Foo()
"#;
        let mut ctx = run(text);
        let _ = extract!(ctx, "f", Object);
    }

    #[test]
    fn class_with_method_call() {
        let text = r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "b", VmValue::Int(4));

        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
b = f.bar(11)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "b", VmValue::Int(15));
    }

    #[test]
    fn class_with_member_access() {
        let text = r#"
class Foo:
    pass

f = Foo()
f.x = 4
"#;
        let mut ctx = run(text);
        assert_member_eq!(ctx, "f", "x", VmValue::Int(4));
    }

    #[test]
    fn class_with_bound_method() {
        let text = r#"
class Foo:
    def bar(self):
        self.x = 4

f = Foo()
f.bar()
"#;
        let mut ctx = run(text);
        assert_member_eq!(ctx, "f", "x", VmValue::Int(4));
    }

    #[test]
    fn class_instantiation_with_constructor() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
"#;
        let mut ctx = run(text);
        assert_member_eq!(ctx, "f", "x", VmValue::Int(44));
    }

    #[test]
    fn class_instantiation_with_constructor_and_args() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.x = val

f = Foo(33)
"#;
        let mut ctx = run(text);
        assert_member_eq!(ctx, "f", "x", VmValue::Int(33));
    }

    #[test]
    fn class_instantiation_with_method_call() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "b", VmValue::Int(10));
    }

    #[test]
    fn regular_import_same_file() {
        let mut ctx = run_path("src/bytecode_vm/fixtures/imports/one/main.py");
        assert_read_eq!(ctx, "x", VmValue::Int(5));
    }

    #[test]
    fn regular_import_function_in_other_file() {
        let mut ctx = run_path("src/bytecode_vm/fixtures/imports/two/main.py");
        assert_read_eq!(ctx, "x", VmValue::Int(33));
    }

    #[test]
    fn regular_import_error() {
        let text = r#"
import not_found
"#;
        let e = run_expect_error(text);
        assert_import_error!(e, "not_found");
    }

    #[test]
    fn stack_trace() {
        let text = r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#;
        let e = run_expect_error(text);
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert_eq!(call_stack.get(1).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert_eq!(call_stack.get(2).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_from_file() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack_one_file.py");
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_multiple_files() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack.py");
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 2);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }
}
