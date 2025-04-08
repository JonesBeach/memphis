pub struct MultilineString {
    pub raw: bool,
    pub literal: String,
    // Either ' or "
    pub end_char: char,
}

impl MultilineString {
    fn new_inner(raw: bool, end_char: char) -> Self {
        assert!(
            matches!(end_char, '\'' | '"'),
            "Multiline string must end with ' or \"."
        );

        Self {
            raw,
            end_char,
            literal: String::new(),
        }
    }

    pub fn new(end_char: char) -> Self {
        Self::new_inner(false, end_char)
    }

    pub fn new_raw(end_char: char) -> Self {
        Self::new_inner(true, end_char)
    }
}
