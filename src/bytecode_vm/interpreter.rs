use crate::{
    bytecode_vm::{Compiler, VirtualMachine, VmValue},
    core::{log, Container, InterpreterEntrypoint, LogLevel},
    domain::Source,
    parser::Parser,
    runtime::MemphisState,
    types::errors::MemphisError,
};

use super::compiler::types::CompiledProgram;

pub struct VmInterpreter {
    compiler: Compiler,
    // TODO this shouldn't need to be public, we're using it inside a few tests right now
    pub vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new(state: Container<MemphisState>, source: Source) -> Self {
        Self {
            compiler: Compiler::new(source),
            vm: VirtualMachine::new(state),
        }
    }

    pub fn take(&mut self, name: &str) -> Option<VmValue> {
        let reference = self.vm.load_global_by_name(name).ok()?;
        Some(self.vm.take(reference))
    }

    pub fn compile(&mut self, parser: &mut Parser) -> Result<CompiledProgram, MemphisError> {
        let ast = parser.parse().map_err(MemphisError::Parser)?;
        self.compiler.compile(&ast).map_err(MemphisError::Compiler)
    }
}

impl Default for VmInterpreter {
    fn default() -> Self {
        Self::new(Container::new(MemphisState::default()), Source::default())
    }
}

impl InterpreterEntrypoint for VmInterpreter {
    type Return = VmValue;

    fn run(&mut self, parser: &mut Parser) -> Result<Self::Return, MemphisError> {
        let program = self.compile(parser)?;
        log(LogLevel::Trace, || format!("{}", program));
        self.vm.load(program);
        self.vm.run_loop().map_err(MemphisError::Execution)
    }
}

#[cfg(test)]
mod vm_interpreter_tests {
    use super::*;

    use crate::{
        bytecode_vm::vm::types::Object,
        domain::{test_utils, ExecutionError, ExecutionErrorKind},
        init::MemphisContext,
    };

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text.trim())
    }

    fn init_path(path: &str) -> MemphisContext {
        MemphisContext::from_path(path)
    }

    fn take_obj_attr(interpreter: &mut VmInterpreter, object: Object, attr: &str) -> VmValue {
        interpreter.vm.take(
            object
                .read(attr.into(), |reference| {
                    interpreter.vm.dereference(reference)
                })
                .expect(&format!("Failed to read attr \"{}\" from object", attr)),
        )
    }

    fn eval_inner(text: &str) -> Result<VmValue, MemphisError> {
        let mut context = init(text);
        context.run_vm()
    }

    fn evaluate(text: &str) -> VmValue {
        eval_inner(text).expect("Failed to evaluate test string")
    }

    fn eval_expect_error(text: &str) -> ExecutionError {
        match eval_inner(text) {
            Ok(_) => panic!("Expected an error!"),
            Err(MemphisError::Execution(e)) => return e,
            Err(_) => panic!("Expected an execution error!"),
        };
    }

    fn run(context: &mut MemphisContext) -> &mut VmInterpreter {
        context.run_vm().expect("VM run failed!");
        context.ensure_vm()
    }

    fn run_expect_error(context: &mut MemphisContext) -> ExecutionError {
        match context.run_vm() {
            Ok(_) => panic!("Expected an error!"),
            Err(MemphisError::Execution(e)) => return e,
            Err(_) => panic!("Expected an execution error!"),
        };
    }

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        assert_eq!(evaluate(text), VmValue::Integer(20));

        let text = "4 < 5";
        assert_eq!(evaluate(text), VmValue::Boolean(true));

        let text = "4 > 5";
        assert_eq!(evaluate(text), VmValue::Boolean(false));

        let text = "4 > x";
        let e = eval_expect_error(text);
        test_utils::assert_error_kind(&e, ExecutionErrorKind::NameError("x".to_string()));

        let text = "y()";
        let e = eval_expect_error(text);
        test_utils::assert_error_kind(&e, ExecutionErrorKind::NameError("y".to_string()));
    }

    #[test]
    fn assignment() {
        let text = r#"
a = 5 - 3
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("a"), Some(VmValue::Integer(2)));

        let text = r#"
a = "Hello World"
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(
            interpreter.take("a"),
            Some(VmValue::String("Hello World".into()))
        );

        let text = r#"
a = 5 - 3
b = 10
c = None
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("a"), Some(VmValue::Integer(2)));
        assert_eq!(interpreter.take("b"), Some(VmValue::Integer(10)));
        assert_eq!(interpreter.take("c"), Some(VmValue::None));

        let text = r#"
a = 5 - 3
b = 10 + a
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("a"), Some(VmValue::Integer(2)));
        assert_eq!(interpreter.take("b"), Some(VmValue::Integer(12)));
    }

    #[test]
    fn while_loop() {
        let text = r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("i"), Some(VmValue::Integer(4)));
    }

    #[test]
    fn function_call_with_parameters() {
        let text = r#"
def foo(a, b):
    return a + b

c = foo(2, 9)
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("c"), Some(VmValue::Integer(11)));
    }

    #[test]
    fn function_call_with_local_var() {
        let text = r#"
def foo(a, b):
    c = 9
    return a + b + c

d = foo(2, 9)
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("d"), Some(VmValue::Integer(20)));
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
        let mut context = init(text);
        // TODO should this do something?
        let _ = run(&mut context);
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
        let mut context = init(text);
        let interpreter = run(&mut context);
        assert_eq!(interpreter.take("c"), Some(VmValue::Integer(29)));
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar():
        return 4
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        let Some(VmValue::Class(class)) = interpreter.take("Foo") else {
            panic!("Did not find class Foo")
        };
        assert_eq!(class.name(), "Foo");
        let VmValue::Function(ref function) =
            *interpreter.vm.dereference(class.read("bar").unwrap())
        else {
            panic!("Did not find function bar")
        };
        assert_eq!(function.code_object.name(), "bar");
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 4

f = Foo()
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        let Some(VmValue::Object(object)) = interpreter.take("f") else {
            panic!("Did not find object f")
        };
        assert_eq!(
            interpreter
                .vm
                .dereference(object.class_ref())
                .as_class()
                .name(),
            "Foo"
        );
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
        let mut context = init(text);
        let interpreter = run(&mut context);
        let Some(VmValue::Integer(b)) = interpreter.take("b") else {
            panic!("Did not find object f")
        };
        assert_eq!(b, 4);

        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
b = f.bar(11)
"#;
        let mut context = init(text);
        let interpreter = run(&mut context);
        let Some(VmValue::Integer(b)) = interpreter.take("b") else {
            panic!("Did not find object f")
        };
        assert_eq!(b, 15);
    }

    #[test]
    fn class_with_member_access() {
        let text = r#"
class Foo:
    pass

f = Foo()
f.x = 4
"#;
        let mut context = init(text);
        let mut interpreter = run(&mut context);
        let Some(VmValue::Object(f)) = interpreter.take("f") else {
            panic!("Did not find object f")
        };
        assert_eq!(take_obj_attr(&mut interpreter, f, "x"), VmValue::Integer(4));
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
        let mut context = init(text);
        let mut interpreter = run(&mut context);
        let Some(VmValue::Object(f)) = interpreter.take("f") else {
            panic!("Did not find object f")
        };
        assert_eq!(take_obj_attr(&mut interpreter, f, "x"), VmValue::Integer(4));
    }

    #[test]
    fn class_instantiation_with_constructor() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
"#;
        let mut context = init(text);
        let mut interpreter = run(&mut context);
        let Some(VmValue::Object(f)) = interpreter.take("f") else {
            panic!("Did not find object f")
        };
        assert_eq!(
            take_obj_attr(&mut interpreter, f, "x"),
            VmValue::Integer(44)
        );
    }

    #[test]
    fn class_instantiation_with_constructor_and_args() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.x = val

f = Foo(33)
"#;
        let mut context = init(text);
        let mut interpreter = run(&mut context);
        let Some(VmValue::Object(f)) = interpreter.take("f") else {
            panic!("Did not find object f")
        };
        assert_eq!(
            take_obj_attr(&mut interpreter, f, "x"),
            VmValue::Integer(33)
        );
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
        let mut context = init(text);
        let interpreter = run(&mut context);
        let Some(VmValue::Integer(b)) = interpreter.take("b") else {
            panic!("Did not find object f")
        };
        assert_eq!(b, 10);
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
        let mut context = init(text);
        let e = run_expect_error(&mut context);
        test_utils::assert_name_error(&e, "unknown");

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
        let mut context = init_path("src/fixtures/call_stack/call_stack_one_file.py");
        let e = run_expect_error(&mut context);
        test_utils::assert_name_error(&e, "unknown");

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
