use crate::{
    bytecode_vm::{compiler::CodeObject, Compiler, VirtualMachine, VmValue},
    core::{log, Container, Interpreter, LogLevel},
    domain::{MemphisValue, Source},
    parser::Parser,
    runtime::MemphisState,
    types::errors::MemphisError,
};

pub struct VmInterpreter {
    compiler: Compiler,
    vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new(state: Container<MemphisState>, source: Source) -> Self {
        Self {
            compiler: Compiler::new(source),
            vm: VirtualMachine::new(state),
        }
    }

    pub fn compile(&mut self, parser: &mut Parser) -> Result<CodeObject, MemphisError> {
        let ast = parser.parse().map_err(MemphisError::Parser)?;
        self.compiler.compile(&ast).map_err(MemphisError::Compiler)
    }

    pub fn execute(&mut self, parser: &mut Parser) -> Result<VmValue, MemphisError> {
        let code = self.compile(parser)?;
        log(LogLevel::Trace, || format!("{}", code));
        self.vm.load(code);
        self.vm.run_loop().map_err(MemphisError::Execution)
    }

    pub fn read_global(&mut self, name: &str) -> Option<VmValue> {
        let reference = self.vm.load_global_by_name(name).ok()?;
        Some(self.vm.take(reference))
    }
}

#[cfg(test)]
impl VmInterpreter {
    pub fn vm(&self) -> &VirtualMachine {
        &self.vm
    }

    pub fn vm_mut(&mut self) -> &mut VirtualMachine {
        &mut self.vm
    }
}

impl Default for VmInterpreter {
    fn default() -> Self {
        Self::new(Container::new(MemphisState::default()), Source::default())
    }
}

impl Interpreter for VmInterpreter {
    fn run(&mut self, parser: &mut Parser) -> Result<MemphisValue, MemphisError> {
        self.execute(parser).map(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_global(name).map(Into::into)
    }
}

#[cfg(test)]
mod tests_vm_interpreter {
    use super::*;

    use crate::{bytecode_vm::test_utils::*, domain::test_utils::*};

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        assert_eval_eq!(text, VmValue::Integer(20));

        let text = "4 < 5";
        assert_eval_eq!(text, VmValue::Boolean(true));

        let text = "4 > 5";
        assert_eval_eq!(text, VmValue::Boolean(false));

        let text = "4 == 5";
        assert_eval_eq!(text, VmValue::Boolean(false));

        let text = "4 == 4";
        assert_eval_eq!(text, VmValue::Boolean(true));

        let text = "4.1 == 4.1";
        assert_eval_eq!(text, VmValue::Boolean(true));

        let text = "4 == 4.1";
        assert_eval_eq!(text, VmValue::Boolean(false));

        let text = r#""a" == "a""#;
        assert_eval_eq!(text, VmValue::Boolean(true));

        let text = r#""a" == "b""#;
        assert_eval_eq!(text, VmValue::Boolean(false));

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
        assert_eval_eq!(text, VmValue::Integer(7));

        let text = "4.1 + 3.01";
        assert_eval_eq!(text, VmValue::Float(7.11));

        let text = "4 + 3.01";
        assert_eval_eq!(text, VmValue::Float(7.01));

        let text = "4.1 + 3";
        assert_eval_eq!(text, VmValue::Float(7.1));

        let text = "-4 + 3";
        assert_eval_eq!(text, VmValue::Integer(-1));

        let text = "4.1 + 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for +");
    }

    #[test]
    fn binary_subtraction() {
        let text = "4 - 3";
        assert_eval_eq!(text, VmValue::Integer(1));

        let text = "4.1 - 3.01";
        assert_eval_eq!(text, VmValue::Float(1.09));

        let text = "4 - 3.01";
        assert_eval_eq!(text, VmValue::Float(0.99));

        let text = "4.1 - 3";
        assert_eval_eq!(text, VmValue::Float(1.1));

        let text = "-4 - 3";
        assert_eval_eq!(text, VmValue::Integer(-7));

        let text = "4.1 - 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for -");
    }

    #[test]
    fn binary_multiplication() {
        let text = "4 * 3";
        assert_eval_eq!(text, VmValue::Integer(12));

        let text = "4.1 * 3.01";
        assert_eval_eq!(text, VmValue::Float(12.341));

        let text = "4 * 3.01";
        assert_eval_eq!(text, VmValue::Float(12.04));

        let text = "4.1 * 3";
        assert_eval_eq!(text, VmValue::Float(12.3));

        let text = "-4 * 3";
        assert_eval_eq!(text, VmValue::Integer(-12));

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
    fn assignment() {
        let text = r#"
a = 5 - 3
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Integer(2));

        let text = r#"
a = "Hello World"
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::String("Hello World".into()));

        let text = r#"
a = 5 - 3
b = 10
c = None
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Integer(2));
        assert_read_eq!(ctx, "b", VmValue::Integer(10));
        assert_read_eq!(ctx, "c", VmValue::None);

        let text = r#"
a = 5 - 3
b = 10 + a
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "a", VmValue::Integer(2));
        assert_read_eq!(ctx, "b", VmValue::Integer(12));
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
        assert_read_eq!(ctx, "i", VmValue::Integer(4));
    }

    #[test]
    fn function_call_with_parameters() {
        let text = r#"
def foo(a, b):
    return a + b

c = foo(2, 9)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "c", VmValue::Integer(11));
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
        assert_read_eq!(ctx, "d", VmValue::Integer(20));
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
        assert_read_eq!(ctx, "c", VmValue::Integer(29));
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
        assert_read_eq!(ctx, "b", VmValue::Integer(4));

        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
b = f.bar(11)
"#;
        let mut ctx = run(text);
        assert_read_eq!(ctx, "b", VmValue::Integer(15));
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
        assert_member_eq!(ctx, "f", "x", VmValue::Integer(4));
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
        assert_member_eq!(ctx, "f", "x", VmValue::Integer(4));
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
        assert_member_eq!(ctx, "f", "x", VmValue::Integer(44));
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
        assert_member_eq!(ctx, "f", "x", VmValue::Integer(33));
    }

    #[test]
    fn class_instantiation_with_constructor_and_args_again() {
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
        assert_read_eq!(ctx, "b", VmValue::Integer(10));
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
        let mut ctx = init(text);
        let e = run_expect_error(&mut ctx);
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
        let mut ctx = init_path("src/fixtures/call_stack/call_stack_one_file.py");
        let e = run_expect_error(&mut ctx);
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
}
