use std::{
    any,
    fmt::{Debug, Display, Error, Formatter},
    marker::PhantomData,
    ops::Deref,
};

/// An unsigned integer wrapper which provides type safety. This is particularly useful when
/// dealing with indices used across the bytecode compiler and the VM as common integer values such
/// as 0, 1, etc, can be interpreted many different ways.
#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct Index<T> {
    value: usize,
    _marker: PhantomData<T>,
}

impl<T> Index<T> {
    pub fn new(value: usize) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Index<T> {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Display for Index<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.value)
    }
}

impl<T> Debug for Index<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let full_type_name = any::type_name::<T>();
        let type_name = full_type_name.rsplit("::").next().unwrap();
        write!(f, "{}({})", type_name, self.value)
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct BytecodeMarker;
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct GlobalStoreMarker;
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct ObjectTableMarker;
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub struct ConstantMarker;

pub type BytecodeIndex = Index<BytecodeMarker>;
pub type GlobalStoreIndex = Index<GlobalStoreMarker>;
pub type ObjectTableIndex = Index<ObjectTableMarker>;
pub type ConstantIndex = Index<ConstantMarker>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug_output() {
        let index: BytecodeIndex = Index::new(4);
        assert_eq!(format!("{:?}", index), "BytecodeMarker(4)".to_string())
    }

    #[test]
    fn test_display_output() {
        let index: BytecodeIndex = Index::new(4);
        assert_eq!(format!("{}", index), "4".to_string())
    }

    #[test]
    fn test_dereference() {
        let index: BytecodeIndex = Index::new(4);
        assert_eq!(*index, 4)
    }
}
