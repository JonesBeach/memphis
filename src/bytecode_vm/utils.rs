pub fn find_index<T, Q>(vec: &[T], query: &Q) -> Option<usize>
where
    T: PartialEq<Q>,
    Q: ?Sized,
{
    vec.iter()
        .enumerate()
        .find_map(|(index, value)| if value == query { Some(index) } else { None })
}
