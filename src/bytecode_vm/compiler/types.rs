use std::fmt::{Debug, Display, Error, Formatter};

use crate::bytecode_vm::opcode::Opcode;

pub type Bytecode = Vec<Opcode>;

/// The values which are passed to the VM are a subset of the types of [`Value`].
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    None,
    Boolean(bool),
    String(String),
    Code(CodeObject),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Constant::None => write!(f, "None"),
            Constant::Boolean(i) => write!(f, "{}", i),
            Constant::String(i) => write!(f, "{}", i),
            Constant::Code(i) => write!(f, "{}", i),
        }
    }
}

pub struct CompiledProgram {
    pub code: CodeObject,
    pub constant_pool: Vec<Constant>,
}

impl CompiledProgram {
    pub fn new(code: CodeObject, constant_pool: Vec<Constant>) -> Self {
        Self {
            code,
            constant_pool,
        }
    }
}

impl Display for CompiledProgram {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "CodeObject: {}", self.code.name)?;
        writeln!(f, "names:")?;
        for (index, name) in self.code.names.iter().enumerate() {
            writeln!(f, "{}: {:?}", name, index)?;
        }

        writeln!(f, "\nconstants:")?;
        for (index, constant) in self.constant_pool.iter().enumerate() {
            writeln!(f, "{}: {}", index, constant)?;
        }

        for constant in self.constant_pool.iter() {
            if let Constant::Code(code) = constant {
                writeln!(f, "\n{}:", code.name)?;
                for (index, opcode) in code.bytecode.iter().enumerate() {
                    writeln!(f, "{}: {}", index, opcode)?;
                }
            }
        }

        writeln!(f, "\n{}:", self.code.name)?;
        for (index, opcode) in self.code.bytecode.iter().enumerate() {
            writeln!(f, "{}: {}", index, opcode)?;
        }

        Ok(())
    }
}

impl Debug for CompiledProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

/// Represents the bytecode and associated metadata for a block of Python code. It's a compiled
/// version of the source code, containing instructions that the VM can execute. This is immutable
/// and does not know about the context in which it is executed, meaning it doesn't hold references
/// to the global or local variables it operates on.
#[derive(Clone, PartialEq, Debug)]
pub struct CodeObject {
    pub name: String,
    pub bytecode: Bytecode,
    pub arg_count: usize,
    /// Local variable names
    pub varnames: Vec<String>,
    /// Non-local identifiers
    pub names: Vec<String>,
}

impl CodeObject {
    pub fn new(name: String) -> Self {
        Self::with_args(name, vec![])
    }

    pub fn with_args(name: String, varnames: Vec<String>) -> Self {
        Self {
            name,
            bytecode: vec![],
            arg_count: varnames.len(),
            varnames,
            names: vec![],
        }
    }
}

impl Display for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<code {}>", self.name)
    }
}
