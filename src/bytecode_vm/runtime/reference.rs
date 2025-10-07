use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
    hash::{Hash, Hasher},
};

use crate::bytecode_vm::{indices::ObjectTableIndex, runtime::Heap};

pub type Namespace = HashMap<String, Reference>;

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    Int(i64),
    Float(f64),
    ObjectRef(ObjectTableIndex),
}

// This is a marker trait. We are confirming that PartialEq fully satisfies equality semantics.
// We cannot derive Eq because it is not implemented for f64 because of NaN weirdness.
impl Eq for Reference {}

// Needed for us to store a `Reference` inside a HashMap for dict objects.
impl Hash for Reference {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Reference::Int(i) => i.hash(state),
            Reference::Float(f) => f.to_bits().hash(state),
            Reference::ObjectRef(idx) => idx.hash(state),
        }
    }
}

impl Reference {
    pub fn display_annotated(&self, heap: &Heap) -> String {
        match self {
            Self::ObjectRef(index) => format!(
                "ObjectRef({}) => {}",
                index,
                heap.get(*self).expect("Heap lookup failed")
            ),
            _ => format!("{}", self),
        }
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{self:?}")
    }
}
