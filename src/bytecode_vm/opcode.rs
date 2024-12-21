use std::fmt::{Display, Error, Formatter};

use super::indices::{BytecodeIndex, ConstantIndex};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    /// Treat the top two values on the stack as integers, add them together, and push their sum
    /// back onto the stack.
    Iadd,
    /// Treat the top two values on the stack as integers, subtract them from each other, and push
    /// their difference back onto the stack.
    Isub,
    /// Integer Multiply
    Imul,
    /// Integer Divide
    Idiv,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is less than the second value.
    LessThan,
    /// Compare two values on the stack and push a boolean result back onto the stack based on
    /// whether the first value is greater than the second value.
    GreaterThan,
    /// Implements STACK[-1] = -STACK[-1].
    UnaryNegative,
    /// Implements STACK[-1] = not STACK[-1].
    UnaryNot,
    /// Implements STACK[-1] = ~STACK[-1].
    UnaryInvert,
    /// Push an integer value onto the stack. This is in preparation for another instruction.
    Push(i64),
    /// Push the value found at the specified index in the constant pool onto the stack.
    LoadConst(ConstantIndex),
    /// Write the top value of the stack into the local variable indicated by the specified index.
    StoreFast(BytecodeIndex),
    /// Write the top value of the stack into the global variable indicated by the specified index.
    StoreGlobal(BytecodeIndex),
    /// Read the local variable indicated by the specified index and push the value onto the stack.
    LoadFast(BytecodeIndex),
    /// Read the global variable indicated by the specified index and push the value onto the stack.
    LoadGlobal(BytecodeIndex),
    /// Pop an object off the stack, find the attribute name specified by the given index, look up
    /// the attribute with that name off the object, and push it onto the stack.
    LoadAttr(BytecodeIndex),
    /// Pop a value and object off the stack and set the attribute of the object to that value. The
    /// attribute name is specified by the given index.
    SetAttr(BytecodeIndex),
    /// Pushes `__build_class__` onto the stack. It is later called by the VM to construct a class,
    /// NOT instantiate an object of that class. This is directly inspired by how CPython does it.
    LoadBuildClass,
    /// Uncomditional jump to an offset. This is signed because you can jump in reverse.
    Jump(isize),
    /// Conditional jump to an offset based on the value on the top of the stack. This is signed
    /// because you can jump in reverse.
    JumpIfFalse(isize),
    /// Create a function object from a code object, encapsulating the information needed to call
    /// the function later.
    MakeFunction,
    /// Call the function from the top of the stack with the specified number of arguments.
    Call(usize),
    /// Call the method from the top of the stack with the specified number of arguments.
    CallMethod(usize),
    /// Return the value on the stack to the caller.
    ReturnValue,
    /// Print a constant from the pool.
    PrintConst(ConstantIndex),
    /// Indicate that a we have reached the end of a class body definition.
    EndClass,
    /// Stop the VM
    Halt,
    /// Used internally to the compiler when constructing jump offsets.
    Placeholder,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Opcode::Iadd => write!(f, "IADD"),
            Opcode::Isub => write!(f, "ISUB"),
            Opcode::Imul => write!(f, "IMUL"),
            Opcode::Idiv => write!(f, "IDIV"),
            Opcode::LessThan => write!(f, "LESS_THAN"),
            Opcode::GreaterThan => write!(f, "GREATER_THAN"),
            Opcode::UnaryNegative => write!(f, "UNARY_NEGATIVE"),
            Opcode::UnaryNot => write!(f, "UNARY_NOT"),
            Opcode::UnaryInvert => write!(f, "UNARY_INVERT"),
            Opcode::Push(i) => write!(f, "PUSH {}", i),
            Opcode::LoadConst(i) => write!(f, "LOAD_CONST {}", i),
            Opcode::StoreFast(i) => write!(f, "STORE_FAST {}", i),
            Opcode::StoreGlobal(i) => write!(f, "STORE_GLOBAL {}", i),
            Opcode::LoadFast(i) => write!(f, "LOAD_FAST {}", i),
            Opcode::LoadGlobal(i) => write!(f, "LOAD_GLOBAL {}", i),
            Opcode::LoadAttr(i) => write!(f, "LOAD_ATTR {}", i),
            Opcode::SetAttr(i) => write!(f, "SET_ATTR {}", i),
            Opcode::LoadBuildClass => write!(f, "LOAD_BUILD_CLASS"),
            Opcode::Jump(i) => write!(f, "JUMP {}", i),
            Opcode::JumpIfFalse(i) => write!(f, "JUMP_IF_FALSE {}", i),
            Opcode::MakeFunction => write!(f, "MAKE_FUNCTION"),
            Opcode::Call(i) => write!(f, "CALL {}", i),
            Opcode::CallMethod(i) => write!(f, "CALL_METHOD {}", i),
            Opcode::ReturnValue => write!(f, "RETURN_VALUE"),
            Opcode::PrintConst(i) => write!(f, "PRINT_CONST {}", i),
            Opcode::EndClass => write!(f, "END_CLASS"),
            Opcode::Halt => write!(f, "HALT"),
            Opcode::Placeholder => unreachable!(),
        }
    }
}
