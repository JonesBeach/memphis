use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Opcode},
        indices::{ConstantIndex, LocalIndex, NonlocalIndex},
        runtime::{Class, FunctionObject, List, Method, Module, Object, Reference},
        VmResult, VmValue,
    },
    core::{log, log_impure, Container, LogLevel},
    domain::Dunder,
    runtime::MemphisState,
};

use super::{
    error_builder::ErrorBuilder, frame::Frame, module_loader::ModuleLoader, CallStack, Runtime,
};

mod errors;

pub struct VirtualMachine {
    runtime: Container<Runtime>,

    state: Container<MemphisState>,

    module_loader: ModuleLoader,

    error_builder: ErrorBuilder,

    call_stack: CallStack,
}

impl VirtualMachine {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        let module_loader = ModuleLoader::new(state.clone(), runtime.clone());
        let error_builder = ErrorBuilder::new(state.clone());
        let call_stack = CallStack::new(state.clone());

        Self {
            state,
            runtime,
            module_loader,
            error_builder,
            call_stack,
        }
    }

    pub fn execute(&mut self, code: CodeObject) -> VmResult<VmValue> {
        self.load(code);
        self.run_loop()
    }

    pub fn read_global(&self, name: &str) -> Option<VmValue> {
        let reference = self.load_global_by_name(name).ok()?;
        self.deref(reference).ok()
    }

    // TODO this should really only be available in test/repl mode, but we currently call this in
    // the Interpreter trait
    fn load_global_by_name(&self, name: &str) -> VmResult<Reference> {
        let module = self
            .runtime
            .borrow()
            .read_module(&Dunder::Main)
            .unwrap_or_else(|| panic!("Failed to read module: {}", Dunder::Main));

        let binding = module.borrow();
        Ok(binding
            .global_store
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("Failed to find var: {}", name)))
    }

    pub fn module(&self) -> VmResult<Container<Module>> {
        Ok(self.current_frame()?.module.clone())
    }

    fn resolve_module(&self, name: &str) -> VmResult<Container<Module>> {
        self.runtime
            .borrow()
            .read_module(name)
            .ok_or_else(|| self.runtime_error())
    }

    fn current_frame(&self) -> VmResult<&Frame> {
        self.call_stack.top_frame()
    }

    fn current_frame_mut(&mut self) -> VmResult<&mut Frame> {
        self.call_stack.top_frame_mut()
    }

    fn read_constant(&self, index: ConstantIndex) -> VmResult<Option<VmValue>> {
        Ok(self
            .current_frame()?
            .function
            .code_object
            .constants
            .get(*index)
            .map(|c| c.into()))
    }

    fn update_fn<F>(&mut self, obj_ref: Reference, function: F)
    where
        F: FnOnce(&mut VmValue),
    {
        if let Some(object_value) = self.runtime.borrow_mut().heap.get_mut(obj_ref) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) -> VmResult<()> {
        let name = self.resolve_name(index)?.to_owned();
        self.module()?.borrow_mut().global_store.insert(name, value);
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
        self.module()?
            .borrow()
            .global_store
            .get(name)
            .copied()
            .ok_or_else(|| self.name_error(name))
    }

    fn resolve_name(&self, index: NonlocalIndex) -> VmResult<&str> {
        Ok(&self.current_frame()?.function.code_object.names[*index])
    }

    fn pop(&mut self) -> VmResult<Reference> {
        let frame = self.current_frame_mut()?;

        if let Some(value) = frame.locals.pop() {
            log_impure(LogLevel::Trace, || {
                println!("After pop:");
                let frame = self.current_frame().expect("No frame!");
                for (index, local) in frame.locals.iter().rev().enumerate() {
                    println!("{}: {}", index, local);
                }
            });
            return Ok(value);
        }

        Err(self.runtime_error())
    }

    fn push(&mut self, value: Reference) -> VmResult<()> {
        let frame = self.current_frame_mut()?;
        frame.locals.push(value);

        log_impure(LogLevel::Trace, || {
            println!("After push:");
            let frame = self.current_frame().expect("No frame!");
            for (index, local) in frame.locals.iter().rev().enumerate() {
                println!("{}: {}", index, local);
            }
        });

        Ok(())
    }

    fn convert_method_to_frame(&mut self, method: Method, args: Vec<Reference>) -> VmResult<Frame> {
        let mut bound_args = vec![method.receiver];
        bound_args.extend(args);

        let module_name = method.function.code_object.source.name();
        let module = self.resolve_module(module_name)?;

        Ok(Frame::new(
            method.function.clone(),
            bound_args,
            module.clone(),
        ))
    }

    /// Extract primitives and resolve any references to a [`VmValue`]. All modifications should
    /// occur through VM instructions.
    pub fn deref(&self, reference: Reference) -> VmResult<VmValue> {
        let val = match reference {
            Reference::ObjectRef(_) => self
                .runtime
                .borrow()
                .heap
                .get(reference)
                .cloned()
                .ok_or_else(|| self.runtime_error())?,
            Reference::ConstantRef(index) => self
                .read_constant(index)?
                .ok_or_else(|| self.runtime_error())?,
            // convert primitives directly
            _ => reference.into(),
        };

        Ok(val)
    }

    /// Resolves an attribute without applying method binding (used in tests or low-level access).
    pub fn resolve_raw_attr(&self, object: &VmValue, name: &str) -> VmResult<Reference> {
        if let Some(object) = object.as_object() {
            object
                .read(name, |r| self.deref(r))
                .ok_or_else(|| self.attribute_error(name))
        } else if let Some(module) = object.as_module() {
            module
                .borrow()
                .read(name)
                .ok_or_else(|| self.attribute_error(name))
        } else {
            unimplemented!()
        }
    }

    /// Resolves an attribute and applies method binding if it is a function.
    pub fn resolve_attr(&mut self, object_ref: Reference, name: &str) -> VmResult<Reference> {
        let object = self.deref(object_ref)?;

        let attr_ref = self.resolve_raw_attr(&object, name)?;
        let attr_val = self.deref(attr_ref)?;

        let bound = match attr_val {
            VmValue::Function(f) => {
                self.as_ref(VmValue::Method(Method::new(object_ref, f.clone())))
            }
            _ => attr_ref,
        };

        Ok(bound)
    }

    /// Primitives are stored inline on the stack, we create a reference to the global store for
    /// all other types.
    fn as_ref(&mut self, value: VmValue) -> Reference {
        match value {
            VmValue::Int(_) | VmValue::Float(_) | VmValue::Bool(_) => value.into_ref(),
            _ => self.runtime.borrow_mut().heap.allocate(value),
        }
    }

    /// Pops and dereferences a value.
    fn pop_value(&mut self) -> VmResult<VmValue> {
        let reference = self.pop()?;
        self.deref(reference)
    }

    fn push_value(&mut self, value: VmValue) -> VmResult<()> {
        let reference = self.as_ref(value);
        self.push(reference)
    }

    /// Load and initialize a module, storing its reference in both the global scope and runtime
    /// module store.
    /// This ensures that newly created frames can resolve their originating module.
    fn load_and_register_module(&mut self, index: NonlocalIndex) -> VmResult<()> {
        let name = self.resolve_name(index)?.to_owned();
        let module = self.module_loader.import(&name, None)?;

        let module_ref = self.as_ref(VmValue::Module(module.clone()));
        self.store_global(index, module_ref)?;

        self.runtime.borrow_mut().store_module(module);

        Ok(())
    }

    fn try_string_multiplication(a: &VmValue, b: &VmValue) -> Option<VmValue> {
        match (a, b) {
            (VmValue::String(s), VmValue::Int(n)) | (VmValue::Int(n), VmValue::String(s)) => {
                let result = if *n < 0 {
                    "".to_string()
                } else {
                    s.repeat(*n as usize)
                };
                Some(VmValue::String(result))
            }
            _ => None,
        }
    }

    fn binary_op<F>(&mut self, opcode: Opcode, op: F, force_float: bool) -> VmResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result = if opcode == Opcode::Mul {
            if let Some(result) = Self::try_string_multiplication(&a, &b) {
                result
            } else {
                self.binary_numeric_op(op, &a, &b, force_float)?
            }
        } else {
            self.binary_numeric_op(op, &a, &b, force_float)?
        };

        self.push_value(result)?;
        Ok(())
    }

    fn cmp_op<F>(&mut self, op: F) -> VmResult<()>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result = self.dynamic_cmp(&a, &b, op)?;
        self.push_value(result)?;
        Ok(())
    }

    fn binary_numeric_op<F>(
        &mut self,
        op: F,
        a: &VmValue,
        b: &VmValue,
        force_float: bool,
    ) -> VmResult<VmValue>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => {
                let res = op(*x as f64, *y as f64);
                if force_float {
                    VmValue::Float(res)
                } else {
                    VmValue::Int(res as i64)
                }
            }
            (VmValue::Float(x), VmValue::Float(y)) => VmValue::Float(op(*x, *y)),
            (VmValue::Int(x), VmValue::Float(y)) => VmValue::Float(op(*x as f64, *y)),
            (VmValue::Float(x), VmValue::Int(y)) => VmValue::Float(op(*x, *y as f64)),
            _ => return Err(self.type_error("Unsupported operand types for binary operation")),
        };

        Ok(result)
    }

    fn dynamic_cmp<F>(&self, a: &VmValue, b: &VmValue, op: F) -> VmResult<VmValue>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => op(*x as f64, *y as f64),
            (VmValue::Float(x), VmValue::Float(y)) => op(*x, *y),
            (VmValue::Int(x), VmValue::Float(y)) => op(*x as f64, *y),
            (VmValue::Float(x), VmValue::Int(y)) => op(*x, *y as f64),
            _ => return Err(self.type_error("Unsupported operand types for comparison")),
        };

        Ok(VmValue::Bool(result))
    }

    /// This is intended to be functionally equivalent to `__build_class__` in CPython.
    fn build_class(&mut self, args: Vec<Reference>) -> VmResult<()> {
        let code = self.deref(args[0])?.as_code().clone();
        let name = code.name().to_string();
        let function = FunctionObject::new(code);

        // TODO do not use default module here
        let frame = Frame::new(function, vec![], Container::new(Module::default()));

        let frame = self.run_new_frame(frame)?;
        self.push_value(VmValue::Class(Class::new(name, frame.namespace())))
    }

    /// Run the top frame in the call stack until there are no more.
    fn run_loop(&mut self) -> VmResult<VmValue> {
        let mut result = VmValue::None;
        while !self.call_stack.is_finished() {
            let frame = self.run_frame()?;
            result = self.return_val_for(&frame)?;
        }

        Ok(result)
    }

    fn return_val_for(&self, frame: &Frame) -> VmResult<VmValue> {
        if let Some(v) = frame.return_val {
            self.deref(v)
        } else {
            Ok(VmValue::None)
        }
    }

    /// Push a new `Frame` to the call stack and immediately execute it to completion.
    fn run_new_frame_and_return(&mut self, frame: Frame) -> VmResult<VmValue> {
        let frame = self.run_new_frame(frame)?;
        self.return_val_for(&frame)
    }

    fn run_new_frame(&mut self, frame: Frame) -> VmResult<Frame> {
        self.call_stack.push(frame);
        self.run_frame()
    }

    /// This will run the top frame in the call stack to completion and then return.
    fn run_frame(&mut self) -> VmResult<Frame> {
        while let Some(frame) = self.call_stack.top() {
            if self.current_frame()?.is_finished() {
                return self.call_stack.pop().ok_or_else(|| self.runtime_error());
            }

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
                    self.cmp_op(|a, b| a < b)
                        .map_err(|_| self.type_error("Unsupported operand types for <"))?;
                }
                Opcode::LessThanOrEq => {
                    self.cmp_op(|a, b| a <= b)
                        .map_err(|_| self.type_error("Unsupported operand types for <="))?;
                }
                Opcode::GreaterThan => {
                    self.cmp_op(|a, b| a > b)
                        .map_err(|_| self.type_error("Unsupported operand types for >"))?;
                }
                Opcode::GreaterThanOrEq => {
                    self.cmp_op(|a, b| a >= b)
                        .map_err(|_| self.type_error("Unsupported operand types for >="))?;
                }
                Opcode::UnaryNegative => {
                    let right = self.pop_value()?.as_integer();
                    self.push(Reference::Int(-right))?;
                }
                Opcode::UnaryNot => {
                    let right = self.pop_value()?.as_boolean();
                    self.push(Reference::Bool(!right))?;
                }
                Opcode::UnaryInvert => {
                    let right = self.pop_value()?.as_integer();
                    self.push(Reference::Int(!right))?;
                }
                Opcode::PushInt(val) => self.push(Reference::Int(val))?,
                Opcode::PushFloat(val) => self.push(Reference::Float(val))?,
                Opcode::LoadConst(index) => {
                    // After loading a constant for the first time, it becomes an object managed by
                    // the heap like any other object.
                    let value = self
                        .read_constant(index)?
                        .ok_or_else(|| self.runtime_error())?;
                    self.push_value(value)?;
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
                    let attr_name = self.resolve_name(index)?.to_owned();
                    let object_ref = self.pop()?;

                    let bound_attr = self.resolve_attr(object_ref, &attr_name)?;
                    self.push(bound_attr)?;
                }
                Opcode::SetAttr(index) => {
                    let value = self.pop()?;
                    let obj_ref = self.pop()?;

                    let name = self.resolve_name(index)?.to_owned();
                    self.update_fn(obj_ref, |object_value| {
                        let VmValue::Object(object) = object_value else {
                            todo!()
                        };
                        object.write(&name, value);
                    });
                }
                Opcode::LoadBuildClass => {
                    self.push_value(VmValue::BuiltinFunction)?;
                }
                Opcode::BuildList(n) => {
                    let mut items = Vec::with_capacity(n);
                    for _ in 0..n {
                        let reference = self.pop()?;
                        items.push(reference);
                    }

                    // Reverse the items because we pop them off in reverse order
                    items.reverse();

                    self.push_value(VmValue::List(List::new(items)))?;
                }
                Opcode::Jump(offset) => {
                    self.call_stack.jump_to_offset(offset)?;
                }
                Opcode::JumpIfFalse(offset) => {
                    let condition = self.pop_value()?.as_boolean();
                    if !condition {
                        self.call_stack.jump_to_offset(offset)?;
                    }
                }
                Opcode::PrintConst(index) => {
                    let value = self
                        .read_constant(index)?
                        .ok_or_else(|| self.runtime_error())?;
                    println!("{}", value);
                }
                Opcode::MakeFunction => {
                    let code = self.pop_value()?.as_code().clone();
                    let function = FunctionObject::new(code);
                    self.push_value(VmValue::Function(function))?;
                }
                Opcode::Call(argc) => {
                    let args = (0..argc)
                        .map(|_| self.pop())
                        .collect::<Result<Vec<_>, _>>()?;
                    let callable_ref = self.pop()?;
                    let callable = self.deref(callable_ref)?;

                    match callable {
                        // TODO this is the placeholder for __build_class__ at the moment
                        VmValue::BuiltinFunction => {
                            self.build_class(args)?;
                        }
                        VmValue::Function(ref function) => {
                            let module_name = function.code_object.source.name();
                            let module = self.resolve_module(module_name)?;
                            let frame = Frame::new(function.clone(), args, module.clone());
                            let ret = self.run_new_frame_and_return(frame)?;
                            self.push_value(ret)?;
                        }
                        VmValue::Method(ref method) => {
                            let frame = self.convert_method_to_frame(method.clone(), args)?;
                            let ret = self.run_new_frame_and_return(frame)?;
                            self.push_value(ret)?;
                        }
                        VmValue::Class(ref class) => {
                            let object = VmValue::Object(Object::new(callable_ref));
                            let reference = self.as_ref(object);

                            if let Some(init_method) = class.read(Dunder::Init) {
                                let init = self.deref(init_method)?.as_function().clone();
                                let method = Method::new(reference, init);

                                // The object reference must be on the stack for
                                // after the constructor executes.
                                self.push(reference)?;

                                let frame = self.convert_method_to_frame(method, args)?;
                                self.run_new_frame(frame)?;
                            } else {
                                self.push(reference)?;
                            }
                        }
                        _ => unimplemented!(),
                    };
                }
                Opcode::ReturnValue => {
                    let return_val_ref = self.pop()?;
                    let frame = self.call_stack.top_frame_mut()?;
                    frame.return_val = Some(return_val_ref);
                    frame.finished = true;
                }
                Opcode::ImportName(index) => {
                    self.load_and_register_module(index)?;
                }
                Opcode::Halt => {
                    let frame = self.call_stack.top_frame_mut()?;
                    frame.finished = true;
                }
                // This is in an internal error that indicates a jump offset was not properly set
                // by the compiler. This opcode should not leak into the VM.
                Opcode::Placeholder => return Err(self.runtime_error()),
            }

            // Increment PC for all instructions.
            self.call_stack.advance_pc()?;
        }

        // We should never get here because we return inline select places in the loop which
        // indicate this frame has ended.
        unreachable!()
    }

    fn load(&mut self, code: CodeObject) {
        log(LogLevel::Debug, || format!("{:?}", code));
        let function = FunctionObject::new(code);
        let module = Container::new(Module::new(function.code_object.source.name()));
        let frame = Frame::new(function, vec![], module.clone());

        self.runtime.borrow_mut().store_module(module);

        self.call_stack.push(frame);
    }
}

pub fn _builtin_list(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => match vm.deref(args[0])? {
            VmValue::List(list) => list.items.clone(),
            _ => return Err(vm.type_error("list() expects an iterable")),
        },
        _ => return Err(vm.type_error("list() takes at most one argument")),
    };

    let list = List::new(items);
    Ok(vm.as_ref(VmValue::List(list)))
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
        let runtime = ctx.interpreter().vm().runtime.borrow();
        let objects: Vec<&VmValue> = runtime
            .heap
            .storage
            .iter()
            .filter(|object| matches!(object, VmValue::Object(_)))
            .collect();
        assert_eq!(objects.len(), 1);
    }
}
