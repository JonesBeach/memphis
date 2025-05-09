use std::{borrow::Cow, collections::HashMap, mem};

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Opcode},
        find_index,
        indices::{ConstantIndex, Index, LocalIndex, NonlocalIndex, ObjectTableIndex},
        VmResult, VmValue,
    },
    core::{log, log_impure, Container, LogLevel},
    domain::{Dunder, ExecutionError, ExecutionErrorKind},
    runtime::MemphisState,
};

use super::{
    frame::Frame,
    types::{Class, FunctionObject, Method, Object, Reference},
};

pub struct VirtualMachine {
    state: Container<MemphisState>,

    /// All code which is executed lives inside a [`Frame`] on this call stack.
    call_stack: Vec<Frame>,

    /// The runtime mapping of global variables to their values.
    global_store: HashMap<String, Reference>,

    /// This is kind of similar to the heap. When an object is created, it will live here and a
    /// reference to it will be placed on the stack. Objects here can be from any function context.
    /// This store retains ownership of the Rust objects throughout the runtime. When non-primitive
    /// objects are pushed onto the stack, a referenced is used so as to not take ownership of the
    /// objects.
    object_table: Vec<VmValue>,

    /// We must keep a stack of class definitions that we have begun so that when they finish, we
    /// know with which name to associate the namespace. The index here references a name from the
    /// constant pool.
    class_stack: Vec<String>,
}

impl VirtualMachine {
    pub fn new(state: Container<MemphisState>) -> Self {
        Self {
            state,
            call_stack: vec![],
            global_store: HashMap::new(),
            object_table: vec![],
            class_stack: vec![],
        }
    }

    pub fn load(&mut self, code: CodeObject) {
        log(LogLevel::Debug, || format!("{}", code));
        let function = FunctionObject::new(code);
        let frame = Frame::new(function, vec![]);

        // We used to do this, but we no longer need to because the MemphisState handles it (kind
        // of)
        // self.enter_context(frame);
        self.call_stack.push(frame);
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

    pub fn read_constant(&self, index: ConstantIndex) -> Option<VmValue> {
        self.current_frame()
            .ok()?
            .function
            .code_object
            .constants
            .get(*index)
            .map(|c| c.into())
    }

    fn error(&self, error_kind: ExecutionErrorKind) -> ExecutionError {
        self.state.save_line_number();
        ExecutionError::new(self.state.debug_call_stack(), error_kind)
    }

    fn runtime_error(&self) -> ExecutionError {
        self.error(ExecutionErrorKind::RuntimeError)
    }

    fn name_error(&self, name: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::NameError(name.to_string()))
    }

    fn type_error(&self, msg: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::TypeError(Some(msg.to_string())))
    }

    fn update_fn<F>(&mut self, index: ObjectTableIndex, function: F)
    where
        F: FnOnce(&mut VmValue),
    {
        if let Some(object_value) = self.object_table.get_mut(*index) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) -> VmResult<()> {
        let name = self.resolve_name(index)?;
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

    fn load_local(&self, index: LocalIndex) -> VmResult<Reference> {
        Ok(self.current_frame()?.locals[*index])
    }

    fn load_global(&self, index: NonlocalIndex) -> VmResult<Reference> {
        let name = self.resolve_name(index)?;
        self.global_store
            .get(name)
            .copied()
            .ok_or_else(|| self.name_error(name))
    }

    fn resolve_name(&self, index: NonlocalIndex) -> VmResult<&str> {
        Ok(&self.current_frame()?.function.code_object.names[*index])
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

    fn return_val(&mut self) -> VmValue {
        if let Some(frame) = self.call_stack.last() {
            if let Some(value) = frame.locals.last() {
                return self.take(*value);
            }
        }

        VmValue::None
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
    fn enter_context(&mut self, frame: Frame) -> VmResult<()> {
        // If we don't save the current line number, we won't properly record where in the current
        // file we called the next function from. We do something similar in the treewalk
        // interpreter.
        self.state.save_line_number();
        self.state.push_stack_frame(&frame);
        self.call_stack.push(frame);
        Ok(())
    }

    fn exit_context(&mut self) -> VmResult<Frame> {
        self.state.pop_stack_frame();
        self.call_stack.pop().ok_or_else(|| self.runtime_error())
    }

    fn advance_pc(&mut self) -> VmResult<()> {
        let frame = self.current_frame_mut()?;
        frame.pc += 1;
        Ok(())
    }

    fn jump_offset(&mut self, offset: isize) -> VmResult<()> {
        let frame = self.current_frame_mut()?;
        frame.pc = (frame.pc as isize + offset) as usize;
        Ok(())
    }

    fn is_finished(&self) -> bool {
        self.call_stack.is_empty()
    }

    /// Extract primitives and resolve any references to a [`VmValue`]. A [`Cow`] is returned to make
    /// it difficult to accidentally mutate an object. All modifications should occur through VM
    /// instructions.
    pub fn dereference(&self, reference: Reference) -> Cow<'_, VmValue> {
        match reference {
            Reference::ObjectRef(index) => Cow::Borrowed(self.object_table.get(*index).unwrap()),
            Reference::ConstantRef(index) => Cow::Owned(self.read_constant(index).unwrap()),
            // convert primitives directly
            _ => Cow::Owned(reference.into()),
        }
    }

    /// Convert a [`Reference`] to a [`VmValue`] taking full ownership. This will remove any objects
    /// from the VM's management and should only be used at the end of execution (i.e. the final
    /// return value, in tests, etc).
    pub fn take(&mut self, reference: Reference) -> VmValue {
        match reference {
            Reference::ObjectRef(index) => {
                mem::replace(&mut self.object_table[*index], VmValue::None)
            }
            Reference::ConstantRef(index) => self.read_constant(index).unwrap(),
            _ => reference.into(),
        }
    }

    /// Primitives are stored inline on the stack, we create a reference to the global store for
    /// all other types.
    fn as_ref(&mut self, value: VmValue) -> Reference {
        match value {
            VmValue::Integer(_) | VmValue::Float(_) | VmValue::Boolean(_) => value.into_ref(),
            _ => {
                let index = Index::new(self.object_table.len());
                self.object_table.push(value);
                Reference::ObjectRef(index)
            }
        }
    }

    /// Pops and dereferences a value.
    fn pop_value(&mut self) -> VmResult<VmValue> {
        let reference = self.pop()?;
        Ok(self.dereference(reference).into_owned())
    }

    /// Pops two dereferenced values.
    fn pop_two(&mut self) -> VmResult<(VmValue, VmValue)> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        Ok((a, b))
    }

    fn binary_op<F>(&mut self, opcode: Opcode, op: F, force_float: bool) -> VmResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let (a, b) = self.pop_two()?;
        let result = match opcode {
            Opcode::Mul => match (&a, &b) {
                (VmValue::String(s), VmValue::Integer(n))
                | (VmValue::Integer(n), VmValue::String(s)) => {
                    if *n < 0 {
                        VmValue::String("".to_string())
                    } else {
                        VmValue::String(s.repeat(*n as usize))
                    }
                }
                _ => self.binary_numeric_op(op, a, b, force_float)?,
            },
            _ => self.binary_numeric_op(op, a, b, force_float)?,
        };

        let reference = self.as_ref(result);
        self.push(reference)?;
        Ok(())
    }

    fn binary_numeric_op<F>(
        &mut self,
        op: F,
        a: VmValue,
        b: VmValue,
        force_float: bool,
    ) -> VmResult<VmValue>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let result = match (a, b) {
            (VmValue::Integer(x), VmValue::Integer(y)) => {
                let res = op(x as f64, y as f64);
                if force_float {
                    VmValue::Float(res)
                } else {
                    VmValue::Integer(res as i64)
                }
            }
            (VmValue::Float(x), VmValue::Float(y)) => VmValue::Float(op(x, y)),
            (VmValue::Integer(x), VmValue::Float(y)) => VmValue::Float(op(x as f64, y)),
            (VmValue::Float(x), VmValue::Integer(y)) => VmValue::Float(op(x, y as f64)),
            _ => return Err(self.type_error("Unsupported operand types for binary operation")),
        };

        Ok(result)
    }

    pub fn run_loop(&mut self) -> VmResult<VmValue> {
        // If we call a function or something that requires us to enter a new frame, we do not want
        // to do so until the end of this loop.
        let mut deferred_frame: Option<Frame> = None;

        while !self.is_finished() {
            let frame = self.current_frame()?;
            // Save this in case we encounter a runtime exception and need to record this info in
            // the stack trace
            self.state.set_line_number(frame.current_line());

            let opcode = frame.get_inst();

            log(LogLevel::Debug, || {
                let code_name = &frame.function.code_object.name();
                format!("{}: {:?}", code_name, opcode)
            });

            match opcode {
                Opcode::Add => {
                    self.binary_op(opcode, |a, b| a + b, false)
                        .map_err(|_| self.type_error("Unsupported operand types for +"))?;
                }
                Opcode::Sub => {
                    self.binary_op(opcode, |a, b| a - b, false)
                        .map_err(|_| self.type_error("Unsupported operand types for -"))?;
                }
                Opcode::Mul => {
                    self.binary_op(opcode, |a, b| a * b, false)
                        .map_err(|_| self.type_error("Unsupported operand types for *"))?;
                }
                Opcode::Div => {
                    self.binary_op(opcode, |a, b| a / b, true)
                        .map_err(|_| self.type_error("Unsupported operand types for /"))?;
                }
                Opcode::Eq => {
                    let right = self.pop_value()?;
                    let left = self.pop_value()?;
                    self.push(Reference::Bool(left == right))?;
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
                Opcode::PushInt(val) => self.push(Reference::Int(val))?,
                Opcode::PushFloat(val) => self.push(Reference::Float(val))?,
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
                    let reference = self.load_local(index)?;
                    self.push(reference)?;
                }
                Opcode::LoadGlobal(index) => {
                    let reference = self.load_global(index)?;
                    self.push(reference)?;
                }
                Opcode::LoadAttr(index) => {
                    let reference = self.pop()?;
                    let object = self.dereference(reference);

                    let name = self.resolve_name(index)?;
                    let attr = object
                        .as_object()
                        .read(name, |reference| self.dereference(reference))
                        .unwrap();
                    let attr_val = self.dereference(attr);
                    let bound_attr = if let VmValue::Function(ref function) = *attr_val {
                        self.as_ref(VmValue::Method(Method::new(reference, function.clone())))
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

                    let name = self.resolve_name(index)?.to_owned();
                    self.update_fn(obj_index, |object_value| {
                        let VmValue::Object(object) = object_value else {
                            todo!()
                        };
                        object.write(&name, value);
                    });
                }
                Opcode::LoadBuildClass => {
                    let reference = self.as_ref(VmValue::BuiltinFunction);
                    self.push(reference)?;
                }
                Opcode::Jump(offset) => {
                    self.jump_offset(offset)?;
                }
                Opcode::JumpIfFalse(offset) => {
                    let reference = self.pop()?;
                    let condition = self.dereference(reference).as_boolean();
                    if !condition {
                        self.jump_offset(offset)?;
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
                    let reference = self.as_ref(VmValue::Function(function));
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
                        VmValue::BuiltinFunction => {
                            deferred_frame = Some(self.build_class(args));
                        }
                        VmValue::Function(ref function) => {
                            let function = function.clone();
                            deferred_frame = Some(Frame::new(function, args));
                        }
                        VmValue::Class(ref class) => {
                            let init_method = class.read(Dunder::Init);
                            let object = Object::new(reference);
                            let reference = self.as_ref(VmValue::Object(object));

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
                    let return_value = self.pop()?;

                    // Exit the loop if there are no more frames
                    if self.is_finished() {
                        break;
                    }

                    self.exit_context()?;
                    // Push the return value to the caller's frame
                    self.push(return_value)?;

                    // Because we have already manipulated the call stack, we can skip the
                    // end-of-iteration checks and handling.
                    continue;
                }
                Opcode::EndClass => {
                    // Grab the frame before it gets popped off the call stack below. Its locals
                    // are the class namespace for the class we just finished defining.
                    let frame = self.exit_context()?;

                    let name = self.class_stack.pop().ok_or_else(|| self.runtime_error())?;
                    let class = Class::new(name.clone(), frame.namespace());
                    let reference = self.as_ref(VmValue::Class(class));

                    self.push(reference)?;
                    continue;
                }
                Opcode::Halt => break,
                // This is in an internal error that indicates a jump offset was not properly set
                // by the compiler. This opcode should not leak into the VM.
                Opcode::Placeholder => return Err(self.runtime_error()),
            }

            // Increment PC for all instructions.
            self.advance_pc()?;

            // Check if we need to enter a new function frame
            if let Some(frame) = deferred_frame.take() {
                self.enter_context(frame)?;
                continue; // Restart loop immediately in new frame
            }

            // Handle functions that complete without explicit return
            if self.current_frame()?.is_finished() {
                self.exit_context()?;
            }
        }

        Ok(self.return_val())
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new(Container::new(MemphisState::default()))
    }
}

#[cfg(test)]
/// These test conditions related to the VM's internal state.
mod tests {
    use super::*;

    use crate::bytecode_vm::test_utils::*;

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
        let ctx = run(text);
        let objects: Vec<&VmValue> = ctx
            .interpreter()
            .vm()
            .object_table
            .iter()
            .filter(|object| matches!(object, VmValue::Object(_)))
            .collect();
        assert_eq!(objects.len(), 1);
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
        let mut ctx = run(text);
        assert_read_eq!(ctx, "d", VmValue::Integer(20));
        let locals = &ctx
            .interpreter()
            .vm()
            .current_frame()
            .expect("Failed to get locals")
            .locals;
        assert_eq!(locals.len(), 0);
    }
}
