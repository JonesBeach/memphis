use std::sync::RwLock;

/// This is soon to be DEPRECATED.
pub struct RwStack<T> {
    elements: RwLock<Vec<T>>,
}

impl<T> RwStack<T>
where
    T: Clone,
{
    pub fn new(items: Vec<T>) -> Self {
        Self {
            elements: RwLock::new(items),
        }
    }

    pub fn with_initial(item: T) -> Self {
        Self::new(vec![item])
    }

    /// It is safe to call `unwrap()` after `write()` in our single-threaded context.
    ///
    /// If this is used in a multi-threaded context in the future, we will need to consider lock
    /// poisoning, which happens when a panic occurs in a thread while it holds a lock.
    pub fn push(&self, item: T) {
        self.elements.write().unwrap().push(item);
    }

    pub fn pop(&self) -> Option<T> {
        self.elements.write().unwrap().pop()
    }

    /// Return the top element of the stack. If a `Container<T>` is used, this can still be used in
    /// mutable contexts because of its interior mutability.
    pub fn top(&self) -> Option<T> {
        self.elements.read().unwrap().last().cloned()
    }

    /// Perform a mutable operation `F` on the top element while maintaining the safety of the
    /// `RwLockWriteGuard`.
    pub fn with_top_mut<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut guard = self.elements.write().unwrap();
        guard.last_mut().map(f)
    }

    /// We call clone to avoid holding a lock for a long time.
    ///
    /// We must guarantee `DoubleEndedIterator` is called so that this iterator may be used in
    /// reverse.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = T> {
        self.elements.read().unwrap().clone().into_iter()
    }

    /// We call clone to avoid holding a lock for a long time.
    ///
    /// We must guarantee `DoubleEndedIterator` is called so that this iterator may be used in
    /// reverse.
    pub fn iter_mut(&self) -> impl DoubleEndedIterator<Item = T> {
        self.elements.write().unwrap().clone().into_iter()
    }
}

impl<T> Default for RwStack<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self::new(vec![])
    }
}
