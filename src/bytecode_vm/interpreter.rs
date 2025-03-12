use crate::{
    bytecode_vm::{types::Value, Compiler, VirtualMachine},
    core::{log, Container, InterpreterEntrypoint, LogLevel},
    parser::Parser,
    treewalk::State,
    types::errors::MemphisError,
};

use super::compiler::types::CompiledProgram;

pub struct VmInterpreter {
    compiler: Compiler,
    // TODO this shouldn't need to be public, we're using it inside a few tests right now
    pub vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new(state: Container<State>) -> Self {
        Self {
            compiler: Compiler::new(state.clone()),
            vm: VirtualMachine::new(state),
        }
    }

    pub fn take(&mut self, name: &str) -> Option<Value> {
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
        Self::new(Container::new(State::default()))
    }
}

impl InterpreterEntrypoint for VmInterpreter {
    type Return = Value;

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
        domain::{ExecutionError, ExecutionErrorKind},
        init::MemphisContext,
    };

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    fn init_path(path: &str) -> MemphisContext {
        MemphisContext::from_path(path)
    }

    fn take_obj_attr(interpreter: &mut VmInterpreter, object: Object, attr: &str) -> Value {
        interpreter.vm.take(
            object
                .read(attr.into(), |reference| {
                    interpreter.vm.dereference(reference)
                })
                .unwrap(),
        )
    }

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(result) => {
                assert_eq!(result, Value::Integer(20));
            }
        }

        let text = "4 < 5";
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(result) => {
                assert_eq!(result, Value::Boolean(true));
            }
        }

        let text = "4 > 5";
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(result) => {
                assert_eq!(result, Value::Boolean(false));
            }
        }

        let text = "4 > x";
        let mut context = init(text);

        match context.run_vm() {
            Err(MemphisError::Execution(e)) => {
                let interpreter = context.ensure_vm();
                assert_eq!(
                    e,
                    ExecutionError::new(
                        interpreter.vm.state.debug_call_stack(),
                        ExecutionErrorKind::NameError("x".to_string())
                    )
                )
            }
            _ => panic!("Expected an exception!"),
        }

        let text = "x()";
        let mut context = init(text);

        match context.run_vm() {
            Err(MemphisError::Execution(e)) => {
                let interpreter = context.ensure_vm();
                assert_eq!(
                    e,
                    ExecutionError::new(
                        interpreter.vm.state.debug_call_stack(),
                        ExecutionErrorKind::NameError("x".to_string())
                    )
                )
            }
            _ => panic!("Expected an exception!"),
        }
    }

    #[test]
    fn assignment() {
        let text = r#"
a = 5 - 3
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("a"), Some(Value::Integer(2)));
            }
        }

        let text = r#"
a = "Hello World"
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(
                    interpreter.take("a"),
                    Some(Value::String("Hello World".into()))
                );
            }
        }

        let text = r#"
a = 5 - 3
b = 10
c = None
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("a"), Some(Value::Integer(2)));
                assert_eq!(interpreter.take("b"), Some(Value::Integer(10)));
                assert_eq!(interpreter.take("c"), Some(Value::None));
            }
        }

        let text = r#"
a = 5 - 3
b = 10 + a
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("a"), Some(Value::Integer(2)));
                assert_eq!(interpreter.take("b"), Some(Value::Integer(12)));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("i"), Some(Value::Integer(4)));
            }
        }
    }

    #[test]
    fn function_call_with_parameters() {
        let text = r#"
def foo(a, b):
    return a + b

c = foo(2, 9)
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("c"), Some(Value::Integer(11)));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("d"), Some(Value::Integer(20)));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {}
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                assert_eq!(interpreter.take("c"), Some(Value::Integer(29)));
            }
        }
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar():
        return 4
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                let Some(Value::Class(class)) = interpreter.take("Foo") else {
                    panic!("Did not find class Foo")
                };
                assert_eq!(class.name(), "Foo");
                let Value::Function(ref function) =
                    *interpreter.vm.dereference(class.read("bar").unwrap())
                else {
                    panic!("Did not find function bar")
                };
                assert_eq!(function.code_object.name(), "bar");
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                let Some(Value::Object(object)) = interpreter.take("f") else {
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
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                let Some(Value::Integer(b)) = interpreter.take("b") else {
                    panic!("Did not find object f")
                };
                assert_eq!(b, 4);
            }
        }

        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
b = f.bar(11)
"#;
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                let Some(Value::Integer(b)) = interpreter.take("b") else {
                    panic!("Did not find object f")
                };
                assert_eq!(b, 15);
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let mut interpreter = context.ensure_vm();
                let Some(Value::Object(f)) = interpreter.take("f") else {
                    panic!("Did not find object f")
                };
                assert_eq!(take_obj_attr(&mut interpreter, f, "x"), Value::Integer(4));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let mut interpreter = context.ensure_vm();
                let Some(Value::Object(f)) = interpreter.take("f") else {
                    panic!("Did not find object f")
                };
                assert_eq!(take_obj_attr(&mut interpreter, f, "x"), Value::Integer(4));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let mut interpreter = context.ensure_vm();
                let Some(Value::Object(f)) = interpreter.take("f") else {
                    panic!("Did not find object f")
                };
                assert_eq!(take_obj_attr(&mut interpreter, f, "x"), Value::Integer(44));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let mut interpreter = context.ensure_vm();
                let Some(Value::Object(f)) = interpreter.take("f") else {
                    panic!("Did not find object f")
                };
                assert_eq!(take_obj_attr(&mut interpreter, f, "x"), Value::Integer(33));
            }
        }
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

        match context.run_vm() {
            Err(e) => panic!("Interpreter error: {:?}", e),
            Ok(_) => {
                let interpreter = context.ensure_vm();
                let Some(Value::Integer(b)) = interpreter.take("b") else {
                    panic!("Did not find object f")
                };
                assert_eq!(b, 10);
            }
        }
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

        match context.run_vm() {
            Err(MemphisError::Execution(e)) => {
                let interpreter = context.ensure_vm();
                assert_eq!(
                    e,
                    ExecutionError::new(
                        interpreter.vm.state.debug_call_stack(),
                        ExecutionErrorKind::NameError("unknown".to_string())
                    )
                );
                let call_stack = e.debug_call_stack;
                assert_eq!(call_stack.len(), 3);
                assert_eq!(call_stack.get(0).name(), "<module>");
                assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
                assert_eq!(call_stack.get(0).line_number(), 8);
                assert_eq!(call_stack.get(1).name(), "middle_call");
                assert_eq!(call_stack.get(1).file_path_str(), "<stdin>");
                assert_eq!(call_stack.get(1).line_number(), 3);
                assert_eq!(call_stack.get(2).name(), "last_call");
                assert_eq!(call_stack.get(2).file_path_str(), "<stdin>");
                assert_eq!(call_stack.get(2).line_number(), 6);
            }
            _ => panic!("Expected an exception!"),
        }

        let mut context = init_path("src/fixtures/call_stack/call_stack_one_file.py");

        match context.run_vm() {
            Err(MemphisError::Execution(e)) => {
                let interpreter = context.ensure_vm();
                assert_eq!(
                    e,
                    ExecutionError::new(
                        interpreter.vm.state.debug_call_stack(),
                        ExecutionErrorKind::NameError("unknown".to_string())
                    )
                );

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
            _ => panic!("Expected an exception!"),
        }
    }
}
