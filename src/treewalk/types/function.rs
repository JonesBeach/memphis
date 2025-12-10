use std::{
    any::Any,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::{log, Container, LogLevel},
    domain::{DebugStackFrame, Dunder, FunctionType, ToDebugStackFrame, Type},
    parser::types::{Ast, Variable},
    treewalk::{
        macros::*,
        protocols::{Callable, DataDescriptor, MemberRead, MemberWrite, NonDataDescriptor},
        result::Raise,
        types::{Cell, Class, Dict, Module, Str, Tuple},
        utils::{bind_args, Args, EnvironmentFrame},
        Scope, SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkState, TreewalkValue,
    },
};

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeParam {
    pub arg: Variable,
    pub default: Option<TreewalkValue>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct RuntimeParams {
    pub args: Vec<RuntimeParam>,
    pub args_var: Option<Variable>,
    pub kwargs_var: Option<Variable>,
}

/// This is a placeholder for what is calcuated on a functions [`Dunder::Code`].
/// TODO this is a stub, we may need to flesh this out with bytecode if we ever want to support
/// self-modifying code or whatever this is used for.
#[derive(Clone)]
pub struct Code;

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    pub args: RuntimeParams,
    pub body: Ast,
    pub module: Container<Module>,
    pub class_context: Option<Container<Class>>,
    line_number: usize,
    function_type: FunctionType,
    pub captured_env: Container<EnvironmentFrame>,
    scope: Scope,
    free_vars: Vec<Variable>,
}

impl_typed!(Function, Type::Function);
impl_descriptor_provider!(
    Function,
    [
        CodeAttribute,
        DictDescriptor,
        GlobalsAttribute,
        ClosureAttribute,
        ModuleAttribute,
        DocAttribute,
        NameAttribute,
        QualnameAttribute,
        AnnotationsAttribute,
        TypeParamsAttribute,
    ]
);

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.module == other.module
    }
}

impl ToDebugStackFrame for Function {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(
            self.name(),
            self.module.borrow().path().to_path_buf(),
            self.line_number(),
        )
    }
}

impl Function {
    pub fn new(
        state: Container<TreewalkState>,
        name: &str,
        args: RuntimeParams,
        body: Ast,
        is_async: bool,
        line_number: usize,
    ) -> Self {
        let module = state.current_module();
        let class_context = state.current_class();
        let captured_env = state.get_environment_frame();

        let function_type = if is_async {
            FunctionType::Async
        } else if body.has_yield() {
            FunctionType::Generator
        } else {
            FunctionType::Regular
        };

        Self {
            free_vars: body.free_vars(),
            name: name.to_string(),
            args,
            body,
            module,
            class_context,
            line_number,
            function_type,
            captured_env,
            scope: Scope::default(),
        }
    }

    pub fn new_lambda(state: Container<TreewalkState>, args: RuntimeParams, body: Ast) -> Self {
        // TODO add line number
        Self::new(state, "<lambda>", args, body, false, 1)
    }

    pub fn new_anonymous_generator(state: Container<TreewalkState>, body: Ast) -> Self {
        // TODO add line number
        Self::new(
            state,
            "<anonymous_generator>",
            RuntimeParams::default(),
            body,
            false,
            1,
        )
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    fn get_globals(&self) -> TreewalkValue {
        TreewalkValue::Dict(Container::new(Dict::default()))
    }

    fn get_code(&self) -> TreewalkValue {
        TreewalkValue::Code(Code)
    }

    fn get_closure(&self) -> TreewalkValue {
        if self.free_vars.is_empty() {
            return TreewalkValue::None;
        }

        let mut items = vec![];
        for key in self.free_vars.iter() {
            let value = self.captured_env.borrow().read(key.as_str()).unwrap();
            items.push(TreewalkValue::Cell(Container::new(Cell::new(value))));
        }

        TreewalkValue::Tuple(Tuple::new(items))
    }

    /// Bind the provided `Args` to this `Function` signature, returning a `SymbolTable` which can
    /// be turned into a runtime `Scope`.
    pub fn bind_args(
        &self,
        args: &Args,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<SymbolTable> {
        bind_args(self.name(), args, &self.args, interpreter)
    }
}

impl MemberRead for Container<Function> {
    /// This is really the same logic as in Container<Object>::get_member. Maybe we can combine
    /// those at some point.
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        log(LogLevel::Debug, || format!("Searching for: {self}.{name}"));

        if let Some(attr) = self.borrow().scope.get(name) {
            log(LogLevel::Debug, || format!("Found: {self}.{name}"));
            return Ok(Some(attr));
        }

        let class = interpreter.state.class_of_type(&Type::Function);

        if let Some(attr) = class.get_from_class(name) {
            log(LogLevel::Debug, || format!("Found: {class}::{name}"));
            let instance = TreewalkValue::Function(self.clone());
            let owner = instance.get_class(interpreter);
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                Some(instance),
                owner,
            )?));
        }

        Ok(None)
    }
}

impl MemberWrite for Container<Function> {
    fn set_member(
        &mut self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        self.borrow_mut().scope.insert(name, value);
        Ok(())
    }

    fn delete_member(
        &mut self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<()> {
        self.borrow_mut().scope.delete(name);
        Ok(())
    }
}

impl Callable for Container<Function> {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        let symbol_table = self.borrow().bind_args(&args, interpreter)?;
        let scope = Container::new(Scope::new(symbol_table));
        interpreter.enter_function(self.clone(), scope)
    }

    fn name(&self) -> String {
        self.borrow().name.clone()
    }

    fn function_type(&self) -> FunctionType {
        self.borrow().function_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        // returning a reference to self, not self directly. This is required so that there is a
        // known size at compile-time.
        self
    }
}

impl Display for Container<Function> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<function {} at {:p}>", self.borrow().name, self)
    }
}

#[derive(Clone)]
struct ClosureAttribute;
#[derive(Clone)]
struct CodeAttribute;
#[derive(Clone)]
struct GlobalsAttribute;
#[derive(Clone)]
struct ModuleAttribute;
#[derive(Clone)]
struct DocAttribute;
#[derive(Clone)]
struct NameAttribute;
#[derive(Clone)]
struct QualnameAttribute;
#[derive(Clone)]
struct AnnotationsAttribute;
#[derive(Clone)]
struct TypeParamsAttribute;
#[derive(Clone)]
struct DictDescriptor;

impl NonDataDescriptor for ClosureAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .raise(interpreter)?
                .borrow()
                .get_closure(),
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Closure.into()
    }
}

impl NonDataDescriptor for CodeAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .raise(interpreter)?
                .borrow()
                .get_code(),
            None => TreewalkValue::DataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Code.into()
    }
}

impl DataDescriptor for CodeAttribute {
    fn set_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: TreewalkValue,
        _value: TreewalkValue,
    ) -> TreewalkResult<()> {
        todo!();
    }

    fn delete_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: TreewalkValue,
    ) -> TreewalkResult<()> {
        todo!();
    }
}

impl NonDataDescriptor for GlobalsAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => instance
                .as_function()
                .raise(interpreter)?
                .borrow()
                .get_globals(),
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Globals.into()
    }
}

impl NonDataDescriptor for ModuleAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .raise(interpreter)?
                    .borrow()
                    .module
                    .borrow()
                    .name()
                    .to_string();
                TreewalkValue::Str(Str::from(name))
            }
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Module.into()
    }
}

impl NonDataDescriptor for DocAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            // TODO store doc strings
            Some(_) => TreewalkValue::Str(Str::new("")),
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Doc.into()
    }
}

impl NonDataDescriptor for NameAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .raise(interpreter)?
                    .borrow()
                    .name
                    .clone();
                TreewalkValue::Str(Str::from(name))
            }
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Name.into()
    }
}

impl NonDataDescriptor for QualnameAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(instance) => {
                let name = instance
                    .as_function()
                    .raise(interpreter)?
                    .borrow()
                    .name
                    .clone();
                TreewalkValue::Str(Str::from(name))
            }
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Qualname.into()
    }
}

impl NonDataDescriptor for AnnotationsAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(_) => TreewalkValue::Dict(Container::new(Dict::default())),
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::Annotations.into()
    }
}

impl NonDataDescriptor for TypeParamsAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(_) => TreewalkValue::Tuple(Tuple::default()),
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        Dunder::TypeParams.into()
    }
}

/// This is really the same logic as in Container<Object>::get_attr. Maybe we can combine those at
/// some point.
impl NonDataDescriptor for DictDescriptor {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let scope = match instance {
            Some(i) => i.as_function().raise(interpreter)?.borrow().scope.clone(),
            None => owner.borrow().scope.clone(),
        };
        Ok(TreewalkValue::Dict(scope.as_dict(interpreter)))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}
