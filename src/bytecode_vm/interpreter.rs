use crate::{
    bytecode_vm::{types::Value, Compiler, VirtualMachine},
    core::{log, InterpreterEntrypoint, LogLevel},
    parser::Parser,
    types::errors::MemphisError,
};

use super::compiler::types::CompiledProgram;

pub struct VmInterpreter {
    compiler: Compiler,
    // TODO this shouldn't need to be public, we're using it inside a few tests right now
    pub vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new() -> Self {
        Self {
            compiler: Compiler::new(),
            vm: VirtualMachine::new(),
        }
    }

    pub fn take(&mut self, name: &str) -> Option<Value> {
        let reference = self.vm.load_global_by_name(name)?;
        Some(self.vm.take(reference))
    }

    pub fn compile(&mut self, parser: &mut Parser) -> Result<CompiledProgram, MemphisError> {
        let ast = parser.parse().map_err(MemphisError::Parser)?;
        self.compiler.compile(ast).map_err(MemphisError::Compiler)
    }
}

impl Default for VmInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl InterpreterEntrypoint for VmInterpreter {
    type Return = Value;

    fn run(&mut self, parser: &mut Parser) -> Result<Self::Return, MemphisError> {
        let program = self.compile(parser)?;
        log(LogLevel::Trace, || format!("{}", program));
        self.vm.load(program);
        self.vm.run_loop().map_err(MemphisError::Vm)
    }
}

#[cfg(test)]
mod vm_interpreter_tests {
    use super::*;

    use crate::{
        bytecode_vm::{types::VmError, vm::types::Object},
        init::MemphisContext,
    };

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
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
            Err(e) => {
                assert_eq!(e, MemphisError::Vm(VmError::NameError("x".to_string())));
            }
            Ok(_) => panic!("Expected an error!"),
        }

        let text = "x()";
        let mut context = init(text);

        match context.run_vm() {
            Err(e) => {
                assert_eq!(e, MemphisError::Vm(VmError::NameError("x".to_string())));
            }
            Ok(_) => panic!("Expected an error!"),
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
                assert_eq!(class.name, "Foo");
                let Value::Function(ref function) =
                    *interpreter.vm.dereference(class.read("bar").unwrap())
                else {
                    panic!("Did not find function bar")
                };
                assert_eq!(function.code_object.name, "bar");
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
                        .name,
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
                let Some(Value::Integer(i)) = interpreter.take("b") else {
                    panic!("Did not find object f")
                };
                assert_eq!(i, 10);
            }
        }
    }
}
