pub fn format_comma_separated<I>(iter: I) -> String
where
    I: IntoIterator,
    I::Item: ToString,
{
    format_comma_separated_with(iter, |i| i.to_string())
}

pub fn format_comma_separated_with<I, F>(iter: I, format_fn: F) -> String
where
    I: IntoIterator,
    F: Fn(I::Item) -> String,
{
    iter.into_iter()
        .map(format_fn)
        .collect::<Vec<_>>()
        .join(", ")
}
