use crate::{
    domain::Type,
    treewalk::protocols::{Callable, DataDescriptor, Iterable, MemberRead, NonDataDescriptor},
};

/// A trait for enabling `Clone` on trait objects that implement `Callable`.
///
/// Rust trait objects (`dyn Trait`) cannot use `Clone` directly because the compiler
/// can't determine how to clone the underlying type. This trait provides a workaround:
/// by requiring implementors to define `clone_box()`, we can implement `Clone` for
/// `Box<dyn CloneableCallable>` using that method.
///
/// We then implement this for all types `T: Callable + Clone + 'static`. This is why all our
/// `Callable` structs contain `#[derive(Clone)]`.
///
/// This allows built-in methods and functions (which are stored as boxed trait objects)
/// to be duplicated cleanly—especially helpful when storing or returning them in
/// contexts like `TreewalkValue::BuiltinFunction`.
pub trait CloneableCallable: Callable {
    fn clone_box(&self) -> Box<dyn CloneableCallable>;
}

impl<T> CloneableCallable for T
where
    T: Callable + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableCallable> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableCallable> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

pub trait CloneableMemberRead: MemberRead {
    fn clone_box(&self) -> Box<dyn CloneableMemberRead>;
}

impl<T> CloneableMemberRead for T
where
    T: MemberRead + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableMemberRead> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableMemberRead> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
/// See `CloneableCallable` for description.
pub trait CloneableDataDescriptor: DataDescriptor {
    fn clone_box(&self) -> Box<dyn CloneableDataDescriptor>;
}

impl<T> CloneableDataDescriptor for T
where
    T: DataDescriptor + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableDataDescriptor> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableDataDescriptor> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

/// See `CloneableCallable` for description.
pub trait CloneableNonDataDescriptor: NonDataDescriptor {
    fn clone_box(&self) -> Box<dyn CloneableNonDataDescriptor>;
}

impl<T> CloneableNonDataDescriptor for T
where
    T: NonDataDescriptor + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableNonDataDescriptor> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableNonDataDescriptor> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

/// See `CloneableCallable` for description.
pub trait CloneableIterable: Iterable {
    fn clone_box(&self) -> Box<dyn CloneableIterable>;
}

impl<T> CloneableIterable for T
where
    T: Iterable + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn CloneableIterable> {
        // defensive: don't allow cloning Box<dyn Trait> directly
        if std::any::type_name::<T>().contains("CloneableIterable") {
            panic!("Attempted to clone a boxed trait object, which leads to infinite recursion");
        }
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn CloneableIterable> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

pub trait Typed {
    fn get_type() -> Type;
}

pub trait MethodProvider {
    fn get_methods() -> Vec<Box<dyn CloneableCallable>>;
}

pub trait DescriptorProvider {
    fn get_descriptors() -> Vec<Box<dyn CloneableNonDataDescriptor>>;
}

pub trait DataDescriptorProvider {
    fn get_data_descriptors() -> Vec<Box<dyn CloneableDataDescriptor>>;
}
