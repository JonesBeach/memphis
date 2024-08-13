use std::{
    cell::RefCell,
    fmt::{Error, Formatter, Pointer},
    ops::Deref,
    rc::Rc,
};

#[derive(Debug, PartialEq)]
pub struct Container<T>(Rc<RefCell<T>>);

impl<T> Container<T> {
    pub fn new(value: T) -> Self {
        Container(Rc::new(RefCell::new(value)))
    }

    /// This compares that two `Container` objects have the same identity, meaning that they point
    /// to the same place in memory.
    ///
    /// The `==` operator uses the derived `PartialEq` trait to check whether two `Container<T>`
    /// objects have the same values `T`.
    pub fn same_identity(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn address(&self) -> usize {
        self.0.as_ptr() as usize
    }
}

/// When we print the pointer value of a `Container<T>` we are usually interested in the pointer
/// value of the `Rc<_>`.
impl<T> Pointer for Container<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{:p}", self.0)
    }
}

impl<T> Deref for Container<T> {
    type Target = RefCell<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// The intention when calling `clone()` on a `Container<T>` is that the reference count to this
/// object should be increased but the inner `T` should not be cloned. We implement the `clone`
/// method ourselves, rather than deriving it, so that the `Clone` trait is not required to be
/// implemented on type `T`.
impl<T> Clone for Container<T> {
    fn clone(&self) -> Self {
        Container(self.0.clone())
    }
}

/// Provide an alternative way to initialize a `Container`.
pub trait Storable {
    fn store(self) -> Container<Self>
    where
        Self: Sized;
}

impl Storable for i64 {
    fn store(self) -> Container<i64> {
        Container::new(self)
    }
}
