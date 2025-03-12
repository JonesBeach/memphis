use std::{borrow::Cow, collections::HashMap, mem};

use crate::{
    bytecode_vm::{
        compiler::{
            find_index,
            types::{CompiledProgram, Constant},
        },
        indices::{ConstantIndex, Index, LocalIndex, NonlocalIndex, ObjectTableIndex},
        types::Value,
        Opcode,
    },
    core::{log, log_impure, Container, LogLevel},
    domain::{Dunder, ExecutionError, ExecutionErrorKind, ToDebugStackFrame},
    treewalk::State,
};

mod frame;
pub mod types;

use self::{
    frame::Frame,
    types::{Class, FunctionObject, Method, Object, Reference},
};

type VmResult<T> = Result<T, ExecutionError>;

pub struct VirtualMachine {
    pub state: Container<State>,

    /// All code which is executed lives inside a [`Frame`] on this call stack.
    call_stack: Vec<Frame>,

    /// Constants handed to us by the compiler as part of the [`CompiledProgram`].
    constant_pool: Vec<Constant>,

    /// The runtime mapping of global variables to their values.
    global_store: HashMap<String, Reference>,

    /// This is kind of similar to the heap. When an object is created, it will live here and a
    /// reference to it will be placed on the stack. Objects here can be from any function context.
    /// This store retains ownership of the Rust objects throughout the runtime. When non-primitive
    /// objects are pushed onto the stack, a referenced is used so as to not take ownership of the
    /// objects.
    object_table: Vec<Value>,

    /// We must keep a stack of class definitions that we have begun so that when they finish, we
    /// know with which name to associate the namespace. The index here references a name from the
    /// constant pool.
    class_stack: Vec<String>,
}

impl VirtualMachine {
    pub fn new(state: Container<State>) -> Self {
        Self {
            state,
            call_stack: vec![],
            constant_pool: vec![],
            global_store: HashMap::new(),
            object_table: vec![],
            class_stack: vec![],
        }
    }

    pub fn load(&mut self, program: CompiledProgram) {
        log(LogLevel::Debug, || format!("{}", program));
        self.constant_pool = program.constant_pool;

        let function = FunctionObject::new(program.code);
        let frame = Frame::new(function, vec![]);

        // We used to do this, but we no longer need to because the MemphisState handles it (kind
        // of)
        // self.enter_context(frame);
        self.call_stack.push(frame);
    }

    pub fn current_frame_index(&self) -> VmResult<usize> {
        self.call_stack
            .len()
            .checked_sub(1)
            .ok_or_else(|| self.runtime_error())
    }

    pub fn current_frame(&self) -> VmResult<&Frame> {
        let index = self.current_frame_index()?;
        Ok(&self.call_stack[index])
    }

    fn current_frame_mut(&mut self) -> VmResult<&mut Frame> {
        let index = self.current_frame_index()?;
        Ok(&mut self.call_stack[index])
    }

    pub fn read_constant(&self, index: ConstantIndex) -> Option<Value> {
        self.constant_pool.get(*index).map(|c| c.into())
    }

    fn runtime_error(&self) -> ExecutionError {
        ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::RuntimeError,
        )
    }

    fn name_error(&self, name: &str) -> ExecutionError {
        ExecutionError::new(
            self.state.debug_call_stack(),
            ExecutionErrorKind::NameError(name.to_string()),
        )
    }

    fn update_fn<F>(&mut self, index: ObjectTableIndex, function: F)
    where
        F: FnOnce(&mut Value),
    {
        if let Some(object_value) = self.object_table.get_mut(*index) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) -> VmResult<()> {
        let name = self.lookup_global_name(index)?;
        self.global_store.insert(name.to_string(), value);
        Ok(())
    }

    fn store_local(&mut self, index: LocalIndex, value: Reference) -> VmResult<()> {
        let current_frame = self.current_frame_mut()?;
        if current_frame.locals.len() == *index {
            current_frame.locals.push(value);
        } else {
            current_frame.locals[*index] = value;
        }
        Ok(())
    }

    // this should only be used by the tests and/or after the VM has run. we should probably move
    // this somewhere else.
    pub fn load_global_by_name(&self, name: &str) -> VmResult<Reference> {
        let current_frame = self.current_frame()?;
        let index = find_index(&current_frame.function.code_object.names, name)
            .map(Index::new)
            .ok_or_else(|| self.runtime_error())?;
        self.load_global(index)
    }

    fn lookup_global_name(&self, index: NonlocalIndex) -> VmResult<&str> {
        let current_frame = self.current_frame()?;
        Ok(&current_frame.function.code_object.names[*index])
    }

    fn load_global(&self, index: NonlocalIndex) -> VmResult<Reference> {
        let name = self.lookup_global_name(index)?;
        self.global_store
            .get(name)
            .copied()
            .ok_or_else(|| self.name_error(name))
    }

    fn pop(&mut self) -> VmResult<Reference> {
        if let Some(frame) = self.call_stack.last_mut() {
            if let Some(value) = frame.locals.pop() {
                log_impure(LogLevel::Trace, || {
                    if let Some(frame) = self.call_stack.last() {
                        println!("After pop:");
                        for (index, local) in frame.locals.iter().rev().enumerate() {
                            println!("{}: {}", index, local);
                        }
                    }
                });
                return Ok(value);
            }
        }

        Err(self.runtime_error())
    }

    fn push(&mut self, value: Reference) -> VmResult<()> {
        if let Some(frame) = self.call_stack.last_mut() {
            frame.locals.push(value);
        }
        log_impure(LogLevel::Trace, || {
            if let Some(frame) = self.call_stack.last() {
                println!("After push:");
                for (index, local) in frame.locals.iter().rev().enumerate() {
                    println!("{}: {}", index, local);
                }
            }
        });

        Ok(())
    }

    fn return_val(&mut self) -> Value {
        if let Some(frame) = self.call_stack.last() {
            if let Some(value) = frame.locals.last() {
                return self.take(*value);
            }
        }

        Value::None
    }

    /// This is intended to be functionally equivalent to `__build_class__` in CPython.
    fn build_class(&mut self, args: Vec<Reference>) -> Frame {
        let code = self.dereference(args[0]).as_code().clone();
        let name = code.name().to_string();
        let function = FunctionObject::new(code);

        self.class_stack.push(name);

        Frame::new(function, vec![])
    }

    fn convert_method_to_frame(&mut self, method: Method, args: Vec<Reference>) -> Frame {
        let mut bound_args = vec![method.receiver];
        bound_args.extend(args);

        Frame::new(method.function.clone(), bound_args)
    }

    /// This does not kick off a separate loop; instead, `run_loop` continues execution with the
    /// new frame.
    fn enter_context(&mut self, frame: Frame) {
        let current_frame = self.current_frame().unwrap();
        // This is a bit counterintuitive: when we enter a new frame, we will begin tracking its
        // latest opcode/statements in `run_loop`. To keep track of where we came from, we push the
        // previous frame onto the stack.
        self.state.push_stack_frame(current_frame.to_stack_frame());
        self.call_stack.push(frame);
    }

    fn exit_context(&mut self) -> Frame {
        self.state.pop_stack_frame();
        self.call_stack.pop().expect("Empty call stack!")
    }

    /// Extract primitives and resolve any references to a [`Value`]. A [`Cow`] is returned to make
    /// it difficult to accidentally mutate an object. All modifications should occur through VM
    /// instructions.
    pub fn dereference(&self, reference: Reference) -> Cow<'_, Value> {
        match reference {
            Reference::ObjectRef(index) => Cow::Borrowed(self.object_table.get(*index).unwrap()),
            Reference::ConstantRef(index) => Cow::Owned(self.read_constant(index).unwrap()),
            // convert primitives directly
            _ => Cow::Owned(reference.into()),
        }
    }

    /// Convert a [`Reference`] to a [`Value`] taking full ownership. This will remove any objects
    /// from the VM's management and should only be used at the end of execution (i.e. the final
    /// return value, in tests, etc).
    pub fn take(&mut self, reference: Reference) -> Value {
        match reference {
            Reference::ObjectRef(index) => {
                mem::replace(&mut self.object_table[*index], Value::None)
            }
            Reference::ConstantRef(index) => self.constant_pool.get(*index).unwrap().into(),
            _ => reference.into(),
        }
    }

    /// Primitives are stored inline on the stack, we create a reference to the global store for
    /// all other types.
    fn create(&mut self, value: Value) -> Reference {
        match value {
            Value::Integer(_) | Value::Boolean(_) => value.into(),
            _ => {
                let index = Index::new(self.object_table.len());
                self.object_table.push(value);
                Reference::ObjectRef(index)
            }
        }
    }

    pub fn run_loop(&mut self) -> VmResult<Value> {
        // If we call a function or something that requires us to enter a new frame, we do not want
        // to do so until the end of this loop.
        let mut deferred_frame: Option<Frame> = None;

        while let Some(current_frame_index) = self.call_stack.len().checked_sub(1) {
            let frame = self.current_frame()?;
            let opcode = frame.get_inst();

            log(LogLevel::Debug, || {
                let code_name = &frame.function.code_object.name();
                format!("{}: {:?}", code_name, opcode)
            });
            self.state.push_stack_frame(frame.to_stack_frame());

            match opcode {
                Opcode::Iadd => {
                    let reference = self.pop()?;
                    let b = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let a = self.dereference(reference).as_integer();
                    self.push(Reference::Int(a + b))?;
                }
                Opcode::Isub => {
                    let reference = self.pop()?;
                    let b = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let a = self.dereference(reference).as_integer();
                    self.push(Reference::Int(a - b))?;
                }
                Opcode::Imul => {
                    let reference = self.pop()?;
                    let b = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let a = self.dereference(reference).as_integer();
                    self.push(Reference::Int(a * b))?;
                }
                Opcode::Idiv => {
                    let reference = self.pop()?;
                    let b = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let a = self.dereference(reference).as_integer();
                    self.push(Reference::Int(a / b))?;
                }
                Opcode::LessThan => {
                    let reference = self.pop()?;
                    let right = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let left = self.dereference(reference).as_integer();
                    self.push(Reference::Bool(left < right))?;
                }
                Opcode::GreaterThan => {
                    let reference = self.pop()?;
                    let right = self.dereference(reference).as_integer();
                    let reference = self.pop()?;
                    let left = self.dereference(reference).as_integer();
                    self.push(Reference::Bool(left > right))?;
                }
                Opcode::UnaryNegative => {
                    let reference = self.pop()?;
                    let right = self.dereference(reference).as_integer();
                    self.push(Reference::Int(-right))?;
                }
                Opcode::UnaryNot => {
                    let reference = self.pop()?;
                    let right = self.dereference(reference).as_boolean();
                    self.push(Reference::Bool(!right))?;
                }
                Opcode::UnaryInvert => {
                    let reference = self.pop()?;
                    let right = self.dereference(reference).as_integer();
                    self.push(Reference::Int(!right))?;
                }
                Opcode::Push(val) => self.push(Reference::Int(val))?,
                Opcode::LoadConst(index) => {
                    self.push(Reference::ConstantRef(index))?;
                }
                Opcode::StoreFast(index) => {
                    let reference = self.pop()?;
                    self.store_local(index, reference)?;
                }
                Opcode::StoreGlobal(index) => {
                    let reference = self.pop()?;
                    self.store_global(index, reference)?;
                }
                Opcode::LoadFast(index) => {
                    if let Some(frame) = self.call_stack.last() {
                        let value = frame.locals[*index];
                        self.push(value)?;
                    }
                }
                Opcode::LoadGlobal(index) => {
                    let reference = self.load_global(index)?;
                    self.push(reference)?;
                }
                Opcode::LoadAttr(index) => {
                    let reference = self.pop()?;
                    let object = self.dereference(reference);

                    let name = self.call_stack[current_frame_index]
                        .function
                        .code_object
                        .names[*index]
                        .clone();
                    let attr = object
                        .as_object()
                        .read(&name, |reference| self.dereference(reference))
                        .unwrap();
                    let attr_val = self.dereference(attr);
                    let bound_attr = if let Value::Function(ref function) = *attr_val {
                        self.create(Value::Method(Method::new(reference, function.clone())))
                    } else {
                        attr
                    };
                    self.push(bound_attr)?;
                }
                Opcode::SetAttr(index) => {
                    let value = self.pop()?;
                    let Reference::ObjectRef(obj_index) = self.pop()? else {
                        todo!()
                    };

                    let name = self.call_stack[current_frame_index]
                        .function
                        .code_object
                        .names[*index]
                        .clone();
                    self.update_fn(obj_index, |object_value| {
                        let Value::Object(object) = object_value else {
                            todo!()
                        };
                        object.write(&name, value);
                    });
                }
                Opcode::LoadBuildClass => {
                    let reference = self.create(Value::BuiltinFunction);
                    self.push(reference)?;
                }
                Opcode::Jump(offset) => {
                    let new_pc =
                        (self.call_stack[current_frame_index].pc as isize + offset) as usize;
                    self.call_stack[current_frame_index].pc = new_pc;
                }
                Opcode::JumpIfFalse(offset) => {
                    let reference = self.pop()?;
                    let condition = self.dereference(reference).as_boolean();
                    if !condition {
                        let new_pc =
                            (self.call_stack[current_frame_index].pc as isize + offset) as usize;
                        self.call_stack[current_frame_index].pc = new_pc;
                    }
                }
                Opcode::PrintConst(index) => {
                    let value = self.read_constant(index).unwrap();
                    println!("{}", value);
                }
                Opcode::MakeFunction => {
                    let reference = self.pop()?;
                    let code = self.dereference(reference).as_code().clone();
                    let function = FunctionObject::new(code);
                    let reference = self.create(Value::Function(function));
                    self.push(reference)?;
                }
                Opcode::Call(argc) => {
                    let args = (0..argc)
                        .map(|_| self.pop())
                        .collect::<Result<Vec<_>, _>>()?;
                    let reference = self.pop()?;
                    let callable = self.dereference(reference);
                    match *callable {
                        // this is the placeholder for __build_class__ at the moment
                        Value::BuiltinFunction => {
                            deferred_frame = Some(self.build_class(args));
                        }
                        Value::Function(ref function) => {
                            let function = function.clone();
                            deferred_frame = Some(Frame::new(function, args));
                        }
                        Value::Class(ref class) => {
                            let init_method = class.read(Dunder::Init);
                            let object = Object::new(reference);
                            let reference = self.create(Value::Object(object));

                            if let Some(init_method) = init_method {
                                let init = self.dereference(init_method).as_function().clone();
                                let method = Method::new(reference, init);

                                // The object reference must be on the stack for
                                // after the constructor executes.
                                self.push(reference)?;

                                deferred_frame = Some(self.convert_method_to_frame(method, args));
                            } else {
                                self.push(reference)?;
                            }
                        }
                        _ => unimplemented!(),
                    };
                }
                Opcode::CallMethod(argc) => {
                    let args = (0..argc)
                        .map(|_| self.pop())
                        .collect::<Result<Vec<_>, _>>()?;
                    let reference = self.pop()?;
                    let method = self.dereference(reference);
                    deferred_frame =
                        Some(self.convert_method_to_frame(method.as_method().clone(), args));
                }
                Opcode::ReturnValue => {
                    // TODO is reference of 0 a safe default here?
                    let return_value = self.pop().unwrap_or(Reference::Int(0));

                    // Exit the loop if there are no more frames
                    if self.call_stack.is_empty() {
                        break;
                    }

                    let _ = self.exit_context();
                    // Push the return value to the caller's frame
                    self.push(return_value)?;

                    // Because we have already manipulated the call stack, we can skip the
                    // end-of-iteration checks and handling.
                    continue;
                }
                Opcode::EndClass => {
                    // Grab the frame before it gets popped off the call stack below. Its locals
                    // are the class namespace for the class we just finished defining.
                    let frame = self.exit_context();

                    let name = self.class_stack.pop().expect("Failed to get class name");
                    let class = Class::new(name.clone(), frame.namespace());
                    let reference = self.create(Value::Class(class));

                    self.push(reference)?;
                    continue;
                }
                Opcode::Halt => break,
                // This is in an internal error that indicates a jump offset was not properly set
                // by the compiler. This opcode should not leak into the VM.
                Opcode::Placeholder => return Err(self.runtime_error()),
            }

            // Increment PC for all instructions.
            self.call_stack[current_frame_index].pc += 1;
            self.state.pop_stack_frame();

            // Check if we need to enter a new function frame
            if let Some(frame) = deferred_frame.take() {
                self.enter_context(frame);
                continue; // Restart loop immediately in new frame
            }

            // Handle functions that complete without explicit return
            if self.call_stack[current_frame_index].is_finished() {
                let _ = self.exit_context();
            }
        }

        Ok(self.return_val())
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new(Container::new(State::default()))
    }
}

#[cfg(test)]
/// These test conditions related to the VM's internal state.
mod tests {
    use super::*;

    use crate::init::MemphisContext;

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    #[test]
    /// We're testing for basic memory-efficiency here. The original implementation created
    /// unnecessary copies of the object.
    fn object_store_duplicates() {
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
                let interpreter = context.ensure_vm();
                let objects: Vec<&Value> = interpreter
                    .vm
                    .object_table
                    .iter()
                    .filter(|object| matches!(object, Value::Object(_)))
                    .collect();
                assert_eq!(objects.len(), 1);
            }
        }
    }

    #[test]
    /// We want to confirm the stack is empty at the end of execution.
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
                assert_eq!(interpreter.vm.current_frame().unwrap().locals.len(), 0);
            }
        }
    }
}
