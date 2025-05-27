use std::{
    fmt::{Debug, Display, Error, Formatter},
    path::Path,
};

use crate::{
    bytecode_vm::compiler::{Bytecode, Constant},
    domain::{Dunder, Source},
};

/// Represents the bytecode and associated metadata for a block of Python code. It's a compiled
/// version of the source code, containing instructions that the VM can execute. This is immutable
/// and does not know about the context in which it is executed, meaning it doesn't hold references
/// to the global or local variables it operates on.
#[derive(Clone, PartialEq)]
pub struct CodeObject {
    pub name: Option<String>,
    pub bytecode: Bytecode,
    pub arg_count: usize,
    /// Local variable names
    pub varnames: Vec<String>,
    /// Free variable names
    pub freevars: Vec<String>,
    /// Non-local identifiers
    pub names: Vec<String>,
    pub constants: Vec<Constant>,

    // this may not be the right thing here? We don't really need a full source
    pub source: Source,
    pub line_map: Vec<(usize, usize)>,
}

impl CodeObject {
    const DEFAULT_NAME: Dunder = Dunder::Main;

    pub fn new_root(source: Source) -> Self {
        Self::with_args(None, &[], source)
    }

    pub fn new(name: &str, source: Source) -> Self {
        Self::with_args(Some(name.to_string()), &[], source)
    }

    pub fn with_args(name: Option<String>, varnames: &[String], source: Source) -> Self {
        Self {
            name,
            bytecode: vec![],
            arg_count: varnames.len(),
            varnames: varnames.to_vec(),
            freevars: vec![],
            names: vec![],
            constants: vec![],
            source,
            line_map: vec![],
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or(Self::DEFAULT_NAME.into())
    }

    pub fn context(&self) -> &str {
        self.name
            .as_deref()
            .unwrap_or_else(|| self.source.context())
    }

    pub fn path(&self) -> &Path {
        self.source.display_path()
    }

    pub fn get_line_number(&self, pc: usize) -> usize {
        match self
            .line_map
            .binary_search_by_key(&pc, |(offset, _)| *offset)
        {
            Ok(index) => self.line_map[index].1, // Exact match
            Err(index) => {
                if index == 0 {
                    0 // Default to first line if before first instruction
                } else {
                    self.line_map[index - 1].1 // Use the last known line number
                }
            }
        }
    }
}

impl Display for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<code {}>", self.name())
    }
}

impl Debug for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "CodeObject: {}", self.name())?;
        writeln!(f, "names:")?;
        for (index, name) in self.names.iter().enumerate() {
            writeln!(f, "{}: {:?}", name, index)?;
        }

        writeln!(f, "\nconstants:")?;
        for (index, constant) in self.constants.iter().enumerate() {
            writeln!(f, "{}: {}", index, constant)?;
        }

        for constant in self.constants.iter() {
            if let Constant::Code(code) = constant {
                writeln!(f, "\n{}:", code.name())?;
                for (index, opcode) in code.bytecode.iter().enumerate() {
                    writeln!(f, "{}: {}", index, opcode)?;
                }
            }
        }

        writeln!(f, "\n{}:", self.name())?;
        for (index, opcode) in self.bytecode.iter().enumerate() {
            writeln!(f, "{}: {}", index, opcode)?;
        }

        Ok(())
    }
}
