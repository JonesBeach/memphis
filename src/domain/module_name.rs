use std::fmt::{Display, Error, Formatter};

use crate::domain::Dunder;

/// A resolved, absolute module name used at runtime.
/// Always valid, never relative. Built by the resolver
/// or by the runtime for builtin modules.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub fn new(segments: Vec<String>) -> Self {
        Self(segments)
    }

    pub fn from_segments<S: AsRef<str>>(segments: &[S]) -> Self {
        Self(segments.iter().map(|s| s.as_ref().to_string()).collect())
    }

    pub fn main() -> Self {
        Self::from_segments(&[Dunder::Main])
    }

    pub fn as_str(&self) -> String {
        self.0.join(".")
    }

    pub fn segments(&self) -> &[String] {
        &self.0
    }

    pub fn head(&self) -> Option<&str> {
        self.0.first().map(|s| s.as_str())
    }

    pub fn tail(&self) -> Option<&str> {
        self.0.last().map(|s| s.as_str())
    }

    /// Removes `n` segments from the end.
    /// Returns None if it would underflow the module name (Illegal relative import).
    pub fn strip_last(&self, n: usize) -> Option<ModuleName> {
        if n > self.0.len() {
            return None;
        }
        let new_len = self.0.len() - n;
        Some(ModuleName(self.0[..new_len].to_vec()))
    }

    /// Joins additional segments onto the module name (used for relative imports).
    pub fn join<I>(&self, tail: I) -> ModuleName
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let mut segments = self.0.clone();
        for s in tail {
            segments.push(s.as_ref().to_string());
        }
        ModuleName(segments)
    }

    /// Iterate from the full module name downward to its parents,
    /// excluding the full name itself.
    ///
    /// Example:
    ///   "a.b.c" -> yields ["a.b", "a"]
    pub fn parents(&self) -> impl DoubleEndedIterator<Item = ModuleName> + '_ {
        (1..self.0.len()).filter_map(move |n| self.strip_last(n))
    }
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.as_str())
    }
}

impl From<&ModuleName> for String {
    fn from(value: &ModuleName) -> Self {
        value.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parents_of_three_segments() {
        let m = ModuleName::from_segments(&["a", "b", "c"]);
        let parents: Vec<_> = m.parents().collect();

        assert_eq!(
            parents,
            vec![
                ModuleName::from_segments(&["a", "b"]),
                ModuleName::from_segments(&["a"]),
            ]
        );
    }

    #[test]
    fn parents_of_two_segments() {
        let m = ModuleName::from_segments(&["a", "b"]);
        let parents: Vec<_> = m.parents().collect();

        assert_eq!(parents, vec![ModuleName::from_segments(&["a"]),]);
    }

    #[test]
    fn parents_of_one_segment_is_empty() {
        let m = ModuleName::from_segments(&["a"]);
        let parents: Vec<_> = m.parents().collect();

        assert!(parents.is_empty());
    }

    #[test]
    fn parents_is_double_ended_iterator() {
        // Verify that next_back works
        let m = ModuleName::from_segments(&["x", "y", "z"]);
        let mut it = m.parents();

        assert_eq!(it.next(), Some(ModuleName::from_segments(&["x", "y"])));
        assert_eq!(it.next_back(), Some(ModuleName::from_segments(&["x"])));
        assert_eq!(it.next(), None);
    }
}
