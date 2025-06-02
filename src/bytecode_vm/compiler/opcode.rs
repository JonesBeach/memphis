use std::fmt::{Display, Error, Formatter};

use crate::bytecode_vm::indices::{ConstantIndex, FreeIndex, LocalIndex, NonlocalIndex};

pub type Bytecode = Vec<Opcode>;
pub type UnsignedOffset = usize;
pub type SignedOffset = isize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    /// Pop the top two values off the stack, perform dynamic type conversion,
    /// and push their sum back onto the stack.
    Add,
    /// Pop the top two values off the stack, perform dynamic type conversion,
    /// and push their difference back onto the stack.
    Sub,
    /// Pop the top two values off the stack, perform dynamic type conversion,
    /// and push their product back onto the stack.
    Mul,
    /// Pop the top two values off the stack, perform dynamic type conversion,
    /// and push their quotient back onto the stack. This is NOT integer division.
    Div,
    Eq,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is less than the second value.
    LessThan,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is less than or equal to the second value.
    LessThanOrEq,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is greater than the second value.
    GreaterThan,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is greater than or equal to the second value.
    GreaterThanOrEq,
    /// Implements STACK[-1] = -STACK[-1].
    UnaryNegative,
    /// Implements STACK[-1] = not STACK[-1].
    UnaryNot,
    /// Implements STACK[-1] = ~STACK[-1].
    UnaryInvert,
    /// Write the top value of the stack into the local variable indicated by the specified index.
    StoreFast(LocalIndex),
    /// Write the top value of the stack into the global variable indicated by the specified index.
    StoreGlobal(NonlocalIndex),
    /// Push the value found at the specified index in the constant pool onto the stack.
    LoadConst(ConstantIndex),
    /// Read the local variable indicated by the specified index and push the value onto the stack.
    LoadFast(LocalIndex),
    /// Read the global variable indicated by the specified index and push the value onto the stack.
    LoadGlobal(NonlocalIndex),
    /// Read a variable from an enclosing environment.
    // TODO we may also need a level here?
    LoadFree(FreeIndex),
    /// Pop an object off the stack, find the attribute name specified by the given index, look up
    /// the attribute with that name off the object, and push it onto the stack.
    LoadAttr(NonlocalIndex),
    /// Pop a value and object off the stack and set the attribute of the object to that value. The
    /// attribute name is specified by the given index.
    SetAttr(NonlocalIndex),
    /// Pushes `__build_class__` onto the stack. It is later called by the VM to construct a class,
    /// NOT instantiate an object of that class. This is directly inspired by how CPython does it.
    LoadBuildClass,
    /// Pop the specified number of elements off the stack and built a list object.
    BuildList(usize),
    /// Uncomditional jump to an offset. This is signed because you can jump in reverse.
    Jump(SignedOffset),
    /// Conditional jump to an offset based on the value on the top of the stack. This is signed
    /// because you can jump in reverse.
    JumpIfFalse(SignedOffset),
    /// Create a function object from a code object, encapsulating the information needed to call
    /// the function later.
    MakeFunction,
    /// Create a function from the code object on the top of the stack, popping off the specified
    /// number of items from the stack to be free variables.
    MakeClosure(usize),
    /// Call the function from the top of the stack with the specified number of arguments.
    /// Binding, if needed, should happen at runtime.
    Call(usize),
    /// Return the value on the stack to the caller.
    ReturnValue,
    /// Import the module indicated by the specified index.
    ImportName(NonlocalIndex),
    /// Stop the VM
    Halt,
    /// Used internally to the compiler when constructing jump offsets.
    Placeholder,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Opcode::Add => write!(f, "ADD"),
            Opcode::Sub => write!(f, "SUB"),
            Opcode::Mul => write!(f, "MUL"),
            Opcode::Div => write!(f, "DIV"),
            Opcode::Eq => write!(f, "EQ"),
            Opcode::LessThan => write!(f, "LESS_THAN"),
            Opcode::LessThanOrEq => write!(f, "LESS_THAN_OR_EQ"),
            Opcode::GreaterThan => write!(f, "GREATER_THAN"),
            Opcode::GreaterThanOrEq => write!(f, "GREATER_THAN_OR_EQ"),
            Opcode::UnaryNegative => write!(f, "UNARY_NEGATIVE"),
            Opcode::UnaryNot => write!(f, "UNARY_NOT"),
            Opcode::UnaryInvert => write!(f, "UNARY_INVERT"),
            Opcode::LoadConst(i) => write!(f, "LOAD_CONST {}", i),
            Opcode::StoreFast(i) => write!(f, "STORE_FAST {}", i),
            Opcode::StoreGlobal(i) => write!(f, "STORE_GLOBAL {}", i),
            Opcode::LoadFast(i) => write!(f, "LOAD_FAST {}", i),
            Opcode::LoadGlobal(i) => write!(f, "LOAD_GLOBAL {}", i),
            Opcode::LoadFree(i) => write!(f, "LOAD_FREE {}", i),
            Opcode::LoadAttr(i) => write!(f, "LOAD_ATTR {}", i),
            Opcode::SetAttr(i) => write!(f, "SET_ATTR {}", i),
            Opcode::LoadBuildClass => write!(f, "LOAD_BUILD_CLASS"),
            Opcode::BuildList(i) => write!(f, "BUILD_LIST {}", i),
            Opcode::Jump(i) => write!(f, "JUMP {}", i),
            Opcode::JumpIfFalse(i) => write!(f, "JUMP_IF_FALSE {}", i),
            Opcode::MakeFunction => write!(f, "MAKE_FUNCTION"),
            Opcode::MakeClosure(i) => write!(f, "MAKE_CLOSURE {}", i),
            Opcode::Call(i) => write!(f, "CALL {}", i),
            Opcode::ReturnValue => write!(f, "RETURN_VALUE"),
            Opcode::ImportName(i) => write!(f, "IMPORT_NAME {}", i),
            Opcode::Halt => write!(f, "HALT"),
            Opcode::Placeholder => unreachable!(),
        }
    }
}
