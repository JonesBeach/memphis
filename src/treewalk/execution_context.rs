use crate::{
    core::{Container, RwStack},
    treewalk::types::{Class, ExprResult, Function},
};

/// This struct stores data for operations related to function calls and class/instance contexts.
pub struct ExecutionContextManager {
    /// A stack to hold the current [`Class`] being defined (i.e. its lexical scope). We need this
    /// so we can associate a function with its class.
    lexical_class_stack: RwStack<Container<Class>>,

    /// A stack to hold the current [`Function`] being evaluated. A method will push something onto
    /// this stack and the receiver stack below.
    current_function_stack: RwStack<Container<Function>>,

    /// A stack to hold the current [`ExprResult`] being evaluated on. We need this for whenver
    /// `super()` is called.
    ///
    /// We do not need a container here because the [`Object`] and [`Class`] variants of
    /// [`ExprResult`] already are wrapped in a [`Container`].
    current_receiver_stack: RwStack<ExprResult>,
}

impl ExecutionContextManager {
    pub fn new() -> Self {
        Self {
            lexical_class_stack: RwStack::default(),
            current_function_stack: RwStack::default(),
            current_receiver_stack: RwStack::default(),
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

    pub fn push_receiver(&mut self, receiver: ExprResult) {
        self.current_receiver_stack.push(receiver);
    }

    pub fn pop_receiver(&mut self) -> Option<ExprResult> {
        self.current_receiver_stack.pop()
    }

    /// Return the currently executing function.
    pub fn read_current_function(&self) -> Option<Container<Function>> {
        self.current_function_stack.top()
    }

    /// Return the currently executing receiver.
    pub fn read_current_receiver(&self) -> Option<ExprResult> {
        self.current_receiver_stack.top()
    }

    /// Return the current class according to lexical scoping rules.
    pub fn read_class(&self) -> Option<Container<Class>> {
        self.lexical_class_stack.top()
    }
}
