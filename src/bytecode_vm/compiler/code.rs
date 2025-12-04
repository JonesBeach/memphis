use std::fmt::{Debug, Display, Error, Formatter};

use crate::{
    bytecode_vm::compiler::{Bytecode, Constant},
    domain::{FunctionType, ModuleName},
};

/// Represents the bytecode and associated metadata for a block of Python code. It's a compiled
/// version of the source code, containing instructions that the VM can execute. This is immutable
/// and does not know about the context in which it is executed, meaning it doesn't hold references
/// to the global or local variables it operates on.
#[derive(Clone, PartialEq)]
pub struct CodeObject {
    pub module_name: ModuleName,
    pub name: String,
    pub filename: String,
    pub bytecode: Bytecode,
    pub arg_count: usize,
    /// Local variable names
    pub varnames: Vec<String>,
    /// Free variable names
    pub freevars: Vec<String>,
    /// Non-local identifiers
    pub names: Vec<String>,
    pub constants: Vec<Constant>,

    pub line_map: Vec<(usize, usize)>,
    pub function_type: FunctionType,
}

impl CodeObject {
    pub fn new(module_name: ModuleName, filename: &str) -> Self {
        Self::new_function(
            "<module>",
            module_name,
            filename,
            &[],
            FunctionType::Regular,
        )
    }

    pub fn new_function(
        name: &str,
        module_name: ModuleName,
        filename: &str,
        varnames: &[String],
        function_type: FunctionType,
    ) -> Self {
        Self {
            module_name,
            name: name.to_string(),
            filename: filename.to_string(),
            bytecode: vec![],
            arg_count: varnames.len(),
            varnames: varnames.to_vec(),
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    // This is a helper for debug output, this does _not_ have semantic meaning.
    pub fn dbg_context(&self) -> String {
        format!("{}.{}", self.module_name, self.name)
    }

    pub fn path(&self) -> &str {
        &self.filename
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
        writeln!(f, "CodeObject: {}", self.dbg_context())?;
        writeln!(f, "names:")?;
        for (index, name) in self.names.iter().enumerate() {
            writeln!(f, "[{index:?}]: {name}")?;
        }

        writeln!(f, "\nconstants:")?;
        for (index, constant) in self.constants.iter().enumerate() {
            writeln!(f, "[{index}]: {constant}")?;
        }

        for constant in self.constants.iter() {
            if let Constant::Code(code) = constant {
                writeln!(f, "\n{}:", code.name())?;
                for (index, opcode) in code.bytecode.iter().enumerate() {
                    writeln!(f, "{index}: {}", opcode.display_annotated(code))?;
                }
            }
        }

        writeln!(f, "\n{}:", self.name())?;
        for (index, opcode) in self.bytecode.iter().enumerate() {
            writeln!(f, "{index}: {}", opcode.display_annotated(self))?;
        }

        Ok(())
    }
}
