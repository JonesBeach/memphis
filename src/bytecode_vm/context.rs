use crate::{
    bytecode_vm::{
        compiler::CodeObject, runtime::types::Exception, Compiler, Runtime, VirtualMachine,
        VmResult, VmValue,
    },
    core::{Container, Interpreter},
    domain::{MemphisValue, ModuleName, Source},
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmContext {
    lexer: Lexer,
    compiler: Compiler,
    vm: VirtualMachine,
}

impl Default for VmContext {
    fn default() -> Self {
        Self::new(Source::from_text(""))
    }
}

impl VmContext {
    pub fn new(source: Source) -> Self {
        let state = Self::init_state(source.clone());
        let runtime = Container::new(Runtime::new());
        Self::from_state(ModuleName::main(), source, state, runtime)
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(
        module_name: ModuleName,
        source: Source,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        Self {
            lexer: Lexer::new(&source),
            compiler: Compiler::new(
                module_name,
                source.path().to_str().expect("Failed to convert path."),
            ),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn run_inner(&mut self) -> VmResult<VmValue> {
        let code = self.compile()?;
        self.vm.execute(code)
    }

    pub fn compile(&mut self) -> VmResult<CodeObject> {
        let mut parser = Parser::new(&mut self.lexer);
        let mut ast = parser
            .parse()
            // TODO use a real syntax error here
            .map_err(|_e| self.vm.raise(Exception::syntax_error()))?;
        ast.rewrite_last_expr_to_return();
        self.compiler
            .compile(&ast)
            // TODO use a real syntax error here
            .map_err(|_e| self.vm.raise(Exception::syntax_error()))
    }

    pub fn read_inner(&self, name: &str) -> Option<VmValue> {
        self.vm.read_global(name).ok()
    }

    pub fn add_line_inner(&mut self, line: &str) {
        self.lexer.add_line(line);
    }

    fn init_state(source: Source) -> Container<MemphisState> {
        let state = Container::new(MemphisState::new());
        state.register_root(source.path());
        state
    }

    #[cfg(test)]
    pub fn vm(&self) -> &VirtualMachine {
        &self.vm
    }

    #[cfg(test)]
    pub fn set_module_name(&mut self, name: ModuleName) {
        self.compiler.set_module_name(name);
    }
}

impl Interpreter for VmContext {
    fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.run_inner().map(Into::into).map_err(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_inner(name).map(Into::into)
    }

    fn add_line(&mut self, line: &str) {
        self.add_line_inner(line);
    }
}
