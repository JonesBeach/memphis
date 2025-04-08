use crate::{
    core::Container,
    treewalk::{
        types::{Class, Function},
        TreewalkValue,
    },
};

/// This struct stores data for operations related to function calls and class/instance contexts.
pub struct ExecutionContextManager {
    /// A stack to hold the current [`Class`] being defined (i.e. its lexical scope). We need this
    /// so we can associate a function with its class.
    lexical_class_stack: Vec<Container<Class>>,

    /// A stack to hold the current [`Function`] being evaluated. A method will push something onto
    /// this stack and the receiver stack below.
    current_function_stack: Vec<Container<Function>>,

    /// A stack to hold the current [`TreewalkValue`] being evaluated on. We need this for whenver
    /// `super()` is called.
    ///
    /// We do not need a container here because the [`Object`] and [`Class`] variants of
    /// [`TreewalkValue`] already are wrapped in a [`Container`].
    current_receiver_stack: Vec<TreewalkValue>,
}

impl ExecutionContextManager {
    pub fn new() -> Self {
        Self {
            lexical_class_stack: vec![],
            current_function_stack: vec![],
            current_receiver_stack: vec![],
        }
    }

    pub fn push_class(&mut self, class: Container<Class>) {
        self.lexical_class_stack.push(class);
    }

    pub fn pop_class(&mut self) -> Option<Container<Class>> {
        self.lexical_class_stack.pop()
    }

    pub fn push_function(&mut self, function: Container<Function>) {
        self.current_function_stack.push(function);
    }

    pub fn pop_function(&mut self) -> Option<Container<Function>> {
        self.current_function_stack.pop()
    }

    pub fn push_receiver(&mut self, receiver: TreewalkValue) {
        self.current_receiver_stack.push(receiver);
    }

    pub fn pop_receiver(&mut self) -> Option<TreewalkValue> {
        self.current_receiver_stack.pop()
    }

    /// Return the currently executing function.
    pub fn read_current_function(&self) -> Option<Container<Function>> {
        self.current_function_stack.last().cloned()
    }

    /// Return the currently executing receiver.
    pub fn read_current_receiver(&self) -> Option<TreewalkValue> {
        self.current_receiver_stack.last().cloned()
    }

    /// Return the current class according to lexical scoping rules.
    pub fn read_class(&self) -> Option<Container<Class>> {
        self.lexical_class_stack.last().cloned()
    }
}
