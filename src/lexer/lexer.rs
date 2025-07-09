use std::{
    collections::VecDeque,
    iter::{repeat_n, Peekable},
    str::Chars,
};

use crate::{
    core::{log, LogLevel},
    domain::Source,
    errors::LexerError,
};

use super::{MultilineString, Token};

type LexerResult<T> = Result<T, LexerError>;

#[derive(Default)]
pub struct Lexer {
    // Tokens we have produced but which have yet to be consumed
    pending_tokens: VecDeque<Token>,

    // Each input line, added incrementally
    source_lines: VecDeque<String>,

    /// When `None`, we are not inside a multiline string. When `Some`, contains the character we
    /// must see to end the multiline string. If a multiline string is not assigned to a variable,
    /// it can act as a multiline comment.
    multiline_string: Option<MultilineString>,

    /// How many nested data structures deep are we? Nested data structures here are those beginning
    /// with {, [, or (.
    multiline_context: usize,

    /// Internal `Lexer` state indicating whether we are tokenizing an expression between `{..}` in
    /// an f-string.
    in_f_string_expr: bool,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // 1. Return any pending token if available
            if let Some(token) = self.pending_tokens.pop_front() {
                return Some(token);
            }

            // 2. If there’s no more source to lex, stop
            if self.source_lines.is_empty() {
                return None;
            }

            // 3. Process the next line into tokens
            let line = self.source_lines.pop_front()?;
            match self.tokenize(&line) {
                Ok(()) => continue,
                Err(err) => self.handle_tokenize_error(err),
            }
        }
    }
}

impl Lexer {
    pub fn new(source: &Source) -> Lexer {
        let mut lexer = Lexer::default();

        // empty Source can occur in REPL mode
        if source.has_text() {
            lexer
                .add_line(source.text())
                .expect("Failed to add line to lexer");
        }

        lexer
    }

    pub fn add_line(&mut self, line: &str) -> LexerResult<()> {
        self.source_lines.push_back(line.to_string());
        Ok(())
    }

    /// Since we tokenize one line at a time, we must consider whether we are inside a multiline
    /// string or not when choosing to emit a `Newline` token or insert a `\n` char into ongoing
    /// multiline string literal.
    fn emit_newline(&mut self) {
        match self.multiline_string {
            None => {
                self.pending_tokens.push_back(Token::Newline);
            }
            Some(ref mut s) => {
                s.literal.push('\n');
            }
        }
    }

    fn handle_tokenize_error(&mut self, err: LexerError) {
        match err {
            LexerError::UnexpectedCharacter(c) => {
                self.pending_tokens.push_back(Token::InvalidCharacter(c));
            }
            _ => panic!("{}", err),
        }
    }

    /// Are we inside a multi-line string or a multi-line context (indicated by {}, (), or []). If
    /// so, our rules for checking and emitting Indent and Dedent tokens are different/disabled.
    fn check_in_block(&self) -> bool {
        self.multiline_context == 0 && self.multiline_string.is_none()
    }

    fn tokenize(&mut self, input: &str) -> LexerResult<()> {
        // Each element here indicates the number of spaces at the beginning of the column for this
        // indentation block. Python does not enforce a particular number of spaces, only that for
        // a given indentation, you are consistent with the number of spaces.
        let mut indentation_stack = vec![0];

        for line in input.lines() {
            if line.is_empty() {
                self.emit_newline();
                continue;
            }

            let num_spaces = count_leading_spaces(line);

            if self.check_in_block() {
                if num_spaces
                    > *indentation_stack
                        .last()
                        .ok_or_else(|| internal_error("Invalid indentation stack state"))?
                {
                    indentation_stack.push(num_spaces);
                    self.pending_tokens.push_back(Token::Indent);
                } else {
                    while num_spaces
                        < *indentation_stack
                            .last()
                            .ok_or_else(|| internal_error("Invalid indentation stack state"))?
                    {
                        indentation_stack.pop();
                        self.pending_tokens.push_back(Token::Dedent);
                    }
                }
            }

            self.tokenize_line(line.trim_start())?;
            self.emit_newline();
        }

        while indentation_stack.len() > 1 {
            indentation_stack.pop();
            self.pending_tokens.push_back(Token::Dedent);
        }

        Ok(())
    }

    /// While inside of an f-string, we do not know the end of a string literal until we hit
    /// another character.
    fn save_string_literal(&mut self, literal: &mut String) {
        if !literal.is_empty() {
            self.pending_tokens
                .push_back(Token::StringLiteral(literal.clone()));
            literal.clear();
        }
    }

    fn tokenize_f_string(&mut self, chars: &mut Peekable<Chars>) {
        let mut literal = String::new();
        while let Some(&c) = chars.peek() {
            if c == '{' {
                if chars.clone().nth(1) == Some('{') {
                    // Handle escape left brace {{
                    literal.push(c);
                    chars.next();
                    chars.next();
                } else {
                    self.save_string_literal(&mut literal);
                    chars.next();
                    self.pending_tokens.push_back(Token::LBrace);
                    self.in_f_string_expr = true;

                    // We are now inside an f-string expression and should use our main lexing loop
                    // to generate the tokens until we detect the end of the f-string expression.
                    return;
                }
            } else if matches!(c, '"' | '\'') {
                self.save_string_literal(&mut literal);
                chars.next();
                self.pending_tokens.push_back(Token::FStringEnd);

                // We are done with the f-string.
                return;
            } else if c == '}' {
                if chars.clone().nth(1) == Some('}') {
                    // Handle escape right brace }}
                    literal.push(c);
                    chars.next();
                    chars.next();
                } else {
                    chars.next();
                    self.pending_tokens.push_back(Token::RBrace);
                    self.in_f_string_expr = false;
                }
            } else {
                literal.push(c);
                chars.next();
            }
        }
    }

    fn tokenize_line(&mut self, input: &str) -> LexerResult<()> {
        let mut chars = input.chars().peekable();

        while let Some(&c) = chars.peek() {
            log(LogLevel::Trace, || format!("char: {c}"));
            if c == '#' {
                // Comments cause the rest of the line to be ignored
                break;
            } else if let Some(string) = &self.multiline_string {
                // When we see three of our end_char, our multiline string is ending
                let triple = repeat_n(string.end_char, 3).collect::<String>();
                if starts_with(&chars, &triple) {
                    chars.next();
                    chars.next();
                    chars.next();
                    if string.raw {
                        self.pending_tokens
                            .push_back(Token::RawStringLiteral(string.literal.clone()));
                    } else {
                        self.pending_tokens
                            .push_back(Token::StringLiteral(string.literal.clone()));
                    }
                    self.multiline_string = None;
                } else {
                    chars.next();
                    if let Some(ref mut s) = self.multiline_string {
                        s.literal.push(c);
                    } else {
                        return Err(internal_error(
                            "Expected a raw string literal, but found None",
                        ));
                    }
                }
            } else if starts_with_any(&chars, &["\"\"\"", "'''"]) {
                self.multiline_string = Some(MultilineString::new(c));
                chars.next();
                chars.next();
                chars.next();
            } else if starts_with_any(&chars, &["r\"\"\"", "r'''"]) {
                let end_char = chars
                    .clone()
                    .nth(1)
                    .ok_or_else(|| internal_error("Invalid multiline string"))?;
                self.multiline_string = Some(MultilineString::new_raw(end_char));
                chars.next();
                chars.next();
                chars.next();
                chars.next();
            } else if starts_with(&chars, "...") {
                chars.next();
                chars.next();
                chars.next();
                self.pending_tokens.push_back(Token::Ellipsis);
            } else if c.is_whitespace() {
                chars.next();
            } else if self.in_f_string_expr && c == '}' {
                self.tokenize_f_string(&mut chars);
            } else if starts_with_any(&chars, &["f\"", "f'", "F\"", "F'"]) {
                chars.next();
                chars.next();
                self.pending_tokens.push_back(Token::FStringStart);
                self.tokenize_f_string(&mut chars);
            } else if starts_with(&chars, "b\'") {
                chars.next();
                chars.next();
                let literal = consume_delimited(&mut chars, '\'');
                self.pending_tokens
                    .push_back(Token::ByteStringLiteral(literal));
            } else if starts_with_any(&chars, &["0b", "0B"]) {
                chars.next();
                chars.next();
                let literal = consume_literal_with_prefix(&mut chars, "0b", |c| c.is_digit(2));
                self.pending_tokens.push_back(Token::BinaryLiteral(literal));
            } else if starts_with_any(&chars, &["0o", "0O"]) {
                chars.next();
                chars.next();
                let literal = consume_literal_with_prefix(&mut chars, "0o", |c| c.is_digit(8));
                self.pending_tokens.push_back(Token::OctalLiteral(literal));
            } else if starts_with_any(&chars, &["0x", "0X"]) {
                chars.next();
                chars.next();
                let literal =
                    consume_literal_with_prefix(&mut chars, "0x", |c| c.is_ascii_hexdigit());
                self.pending_tokens.push_back(Token::HexLiteral(literal));
            } else if starts_with_any(&chars, &["r\"", "R\""]) {
                chars.next();
                chars.next();
                let literal = consume_delimited(&mut chars, '"');
                self.pending_tokens
                    .push_back(Token::RawStringLiteral(literal));
            } else if matches!(c, '"' | '\'') {
                chars.next();
                let literal = consume_delimited(&mut chars, c);
                self.pending_tokens.push_back(Token::StringLiteral(literal));
            } else if c.is_alphabetic() || c == '_' {
                let identifier = consume_literal(&mut chars, |c| c.is_alphanumeric() || c == '_');

                let token = match identifier.as_str() {
                    "def" => Token::Def,
                    "del" => Token::Del,
                    "lambda" => Token::Lambda,
                    "if" => Token::If,
                    "elif" => Token::Elif,
                    "else" => Token::Else,
                    "while" => Token::While,
                    "for" => Token::For,
                    "in" => Token::In,
                    "is" => Token::Is,
                    "return" => Token::Return,
                    "yield" => Token::Yield,
                    "pass" => Token::Pass,
                    "and" => Token::And,
                    "or" => Token::Or,
                    "not" => Token::Not,
                    "class" => Token::Class,
                    "try" => Token::Try,
                    "except" => Token::Except,
                    "finally" => Token::Finally,
                    "raise" => Token::Raise,
                    "from" => Token::From,
                    "as" => Token::As,
                    "with" => Token::With,
                    "import" => Token::Import,
                    "assert" => Token::Assert,
                    "None" => Token::None,
                    "Ellipsis" => Token::Ellipsis,
                    "NotImplemented" => Token::NotImplemented,
                    "True" => Token::BooleanLiteral(true),
                    "False" => Token::BooleanLiteral(false),
                    "async" => Token::Async,
                    "await" => Token::Await,
                    "continue" => Token::Continue,
                    "break" => Token::Break,
                    "nonlocal" => Token::Nonlocal,
                    "global" => Token::Global,
                    _ => Token::Identifier(identifier),
                };
                self.pending_tokens.push_back(token);
            } else if c.is_ascii_digit() {
                let mut value = String::new();
                let mut is_scientific = false;
                while let Some(&c) = chars.peek() {
                    if matches!(c, 'e' | 'E') {
                        is_scientific = true;
                    }

                    if c.is_ascii_digit()
                        || matches!(c, '.' | 'e' | 'E')
                        // We should only see a dash char '-' if we know we are in scientific
                        // notation.
                        || (is_scientific && c == '-')
                    {
                        value.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if value.contains('.') || value.contains('e') || value.contains('E') {
                    let f_value = value
                        .parse::<f64>()
                        .map_err(|_| LexerError::InvalidToken(value))?;
                    self.pending_tokens.push_back(Token::FloatingPoint(f_value));
                } else {
                    let i_value = value
                        .parse::<u64>()
                        .map_err(|_| LexerError::InvalidToken(value))?;
                    self.pending_tokens.push_back(Token::Integer(i_value));
                }
            } else if starts_with_any(
                &chars,
                &["+=", "-=", "*=", "/=", "&=", "^=", "|=", "%=", "@="],
            ) {
                chars.next();
                chars.next();
                let token = match c {
                    '+' => Token::PlusEquals,
                    '-' => Token::MinusEquals,
                    '*' => Token::AsteriskEquals,
                    '/' => Token::SlashEquals,
                    '&' => Token::BitwiseAndEquals,
                    '^' => Token::BitwiseXorEquals,
                    '|' => Token::BitwiseOrEquals,
                    '%' => Token::ModEquals,
                    '@' => Token::MatMulEquals,
                    _ => unreachable!(),
                };
                self.pending_tokens.push_back(token);
            } else if starts_with_any(&chars, &["//=", "**=", "<<=", ">>="]) {
                chars.next();
                chars.next();
                chars.next();

                let token = match c {
                    '/' => Token::DoubleSlashEquals,
                    '*' => Token::ExpoEquals,
                    '<' => Token::LeftShiftEquals,
                    '>' => Token::RightShiftEquals,
                    _ => unreachable!(),
                };
                self.pending_tokens.push_back(token);
            } else if starts_with(&chars, "->") {
                chars.next();
                chars.next();
                self.pending_tokens.push_back(Token::ReturnTypeArrow);
            } else if matches!(c, '=' | '!' | '<' | '>') {
                let operator = consume_literal(&mut chars, |c| matches!(c, '=' | '!' | '<' | '>'));

                let token = match operator.as_str() {
                    "==" => Token::Equal,
                    "!=" => Token::NotEqual,
                    "<" => Token::LessThan,
                    ">" => Token::GreaterThan,
                    "<=" => Token::LessThanOrEqual,
                    ">=" => Token::GreaterThanOrEqual,
                    "=" => Token::Assign,
                    "!" => Token::Exclamation,
                    "<<" => Token::LeftShift,
                    ">>" => Token::RightShift,
                    _ => return Err(LexerError::UnexpectedCharacter(c)),
                };
                self.pending_tokens.push_back(token);
            } else if starts_with(&chars, "**") {
                chars.next();
                chars.next();
                self.pending_tokens.push_back(Token::DoubleAsterisk);
            } else if starts_with(&chars, "//") {
                chars.next();
                chars.next();
                self.pending_tokens.push_back(Token::DoubleSlash);
            } else {
                let token = match c {
                    '.' => Token::Dot,
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '*' => Token::Asterisk,
                    '/' => Token::Slash,
                    '(' => Token::LParen,
                    ')' => Token::RParen,
                    '[' => Token::LBracket,
                    ']' => Token::RBracket,
                    '{' => Token::LBrace,
                    '}' => Token::RBrace,
                    ',' => Token::Comma,
                    ':' => Token::Colon,
                    '@' => Token::AtSign,
                    '&' => Token::BitwiseAnd,
                    '|' => Token::BitwiseOr,
                    '^' => Token::BitwiseXor,
                    '~' => Token::BitwiseNot,
                    '%' => Token::Modulo,
                    '\n' => Token::Newline,
                    _ => return Err(LexerError::UnexpectedCharacter(c)),
                };

                // Detect when we are inside multi-line data structures, which should not be
                // treated the same as blocks.
                if matches!(token, Token::LParen | Token::LBrace | Token::LBracket) {
                    self.multiline_context += 1;
                } else if matches!(token, Token::RParen | Token::RBrace | Token::RBracket) {
                    self.multiline_context -= 1;
                }

                self.pending_tokens.push_back(token);
                chars.next();
            }
        }

        Ok(())
    }
}

fn starts_with_any(chars: &Peekable<Chars>, prefixes: &[&str]) -> bool {
    prefixes.iter().any(|prefix| starts_with(chars, prefix))
}

fn starts_with(chars: &Peekable<Chars>, prefix: &str) -> bool {
    let mut clone = chars.clone();
    for expected in prefix.chars() {
        match clone.next() {
            Some(c) if c == expected => continue,
            _ => return false,
        }
    }
    true
}

fn internal_error(msg: &str) -> LexerError {
    LexerError::InternalError(msg.to_string())
}

fn count_leading_spaces(line: &str) -> usize {
    let mut chars = line.chars().peekable();
    let leading_spaces = consume_literal(&mut chars, |c| c == ' ');
    leading_spaces.len()
}

fn consume_literal<F>(chars: &mut Peekable<Chars>, valid_char: F) -> String
where
    F: Fn(char) -> bool,
{
    consume_literal_with_prefix(chars, "", valid_char)
}

fn consume_literal_with_prefix<F>(
    chars: &mut Peekable<Chars>,
    prefix: &str,
    valid_char: F,
) -> String
where
    F: Fn(char) -> bool,
{
    let mut literal = String::from(prefix);

    while let Some(&c) = chars.peek() {
        if valid_char(c) {
            literal.push(c);
            chars.next();
        } else {
            break;
        }
    }

    literal
}

fn consume_delimited(chars: &mut Peekable<Chars>, end_delim: char) -> String {
    let mut literal = String::new();

    while let Some(&c) = chars.peek() {
        if c == end_delim {
            chars.next();
            break;
        } else {
            literal.push(c);
            chars.next();
        }
    }

    literal
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        let trimmed = input.trim_matches('\n');

        let mut lexer = Lexer::default();
        lexer
            .add_line(trimmed)
            .expect("Failed to add input line to Lexer.");
        lexer.collect()
    }

    macro_rules! tokenize_incremental {
        ( $( $line:expr ),* ) => {{
            let mut lexer = Lexer::default();
            $(
                lexer.add_line($line).expect("Failed to add input line to Lexer.");
            )*
            lexer.collect::<Vec<Token>>()
        }};
    }

    #[test]
    fn incremental_tokenizing() {
        let first = r#"
def add(x, y):
"#;
        let second = r#"
    return x + y
"#;

        let tokens = tokenize_incremental![first, second];

        assert_eq!(
            tokens,
            vec![
                Token::Newline,
                Token::Def,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Identifier("x".to_string()),
                Token::Comma,
                Token::Identifier("y".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Newline,
                Token::Indent,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::Plus,
                Token::Identifier("y".to_string()),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn function_definition() {
        let input = r#"
def add(x, y):
    return x + y
"#;
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Def,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Identifier("x".to_string()),
                Token::Comma,
                Token::Identifier("y".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::Plus,
                Token::Identifier("y".to_string()),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn invalid_character() {
        let input = "2 + $";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Integer(2), Token::Plus, Token::InvalidCharacter('$'),]
        );
    }

    #[test]
    fn comparison_operators() {
        let input = "a > b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::GreaterThan,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a < b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LessThan,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a == b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Equal,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a != b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::NotEqual,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a >= b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::GreaterThanOrEqual,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a <= b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LessThanOrEqual,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn boolean_expressions() {
        let input = "a and b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::And,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a or b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Or,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a in b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::In,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "a is None";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Is,
                Token::None,
                Token::Newline,
            ]
        );

        let input = "not b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Not,
                Token::Identifier("b".to_string()),
                Token::Newline,
            ]
        );

        let input = "not (b or c)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Not,
                Token::LParen,
                Token::Identifier("b".to_string()),
                Token::Or,
                Token::Identifier("c".to_string()),
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn boolean_literals() {
        let input = "x = True";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::BooleanLiteral(true),
                Token::Newline,
            ]
        );

        let input = "x = False";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::BooleanLiteral(false),
                Token::Newline,
            ]
        );

        let input = "x = None";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::None,
                Token::Newline,
            ]
        );

        let input = "return None";
        let tokens = tokenize(input);
        assert_eq!(tokens, vec![Token::Return, Token::None, Token::Newline,]);
    }

    #[test]
    fn if_else() {
        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Middle")
else:
    print("Less")
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::Identifier("x".to_string()),
                Token::GreaterThan,
                Token::Integer(0),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral("Greater".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
                Token::Elif,
                Token::Identifier("x".to_string()),
                Token::GreaterThan,
                Token::Minus,
                Token::Integer(10),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral("Middle".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
                Token::Else,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral("Less".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn while_loop() {
        let input = r#"
while True:
    print("busy loop")
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::While,
                Token::BooleanLiteral(true),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral("busy loop".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn class_definition() {
        let input = r#"
class Foo:
    def __init__(self):
        self.x = 0

    def bar(self):
        return self.x
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Class,
                Token::Identifier("Foo".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Def,
                Token::Identifier("__init__".to_string()),
                Token::LParen,
                Token::Identifier("self".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("self".to_string()),
                Token::Dot,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::Integer(0),
                Token::Newline,
                Token::Newline,
                Token::Dedent,
                Token::Def,
                Token::Identifier("bar".to_string()),
                Token::LParen,
                Token::Identifier("self".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Return,
                Token::Identifier("self".to_string()),
                Token::Dot,
                Token::Identifier("x".to_string()),
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn class_instantiation() {
        let input = "foo = Foo()";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Assign,
                Token::Identifier("Foo".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn method_invocation() {
        let input = r#"
foo = Foo()
foo.bar()
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Assign,
                Token::Identifier("Foo".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Newline,
                Token::Identifier("foo".to_string()),
                Token::Dot,
                Token::Identifier("bar".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Import,
                Token::Identifier("other".to_string()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn selective_import() {
        let input = "from other import something";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::Newline,
            ]
        );

        let input = "from other import something as something_else";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::As,
                Token::Identifier("something_else".to_string()),
                Token::Newline,
            ]
        );

        let input = "from other import *";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Asterisk,
                Token::Newline,
            ]
        );

        let input = "from other import something, something_else";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::Comma,
                Token::Identifier("something_else".to_string()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn comment() {
        let input = r#"
foo = Foo(3) # new instance
# x = foo.baz()
foo.bar()
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Assign,
                Token::Identifier("Foo".to_string()),
                Token::LParen,
                Token::Integer(3),
                Token::RParen,
                Token::Newline,
                Token::Newline,
                Token::Identifier("foo".to_string()),
                Token::Dot,
                Token::Identifier("bar".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn floating_point() {
        let input = "x = 3.14";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(3.14),
                Token::Newline,
            ]
        );

        let input = "x = 2.5e-3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2.5e-3),
                Token::Newline,
            ]
        );

        let input = "x = 2.5E-3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2.5e-3),
                Token::Newline,
            ]
        );

        let input = "x = 2E-3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2e-3),
                Token::Newline,
            ]
        );

        let input = "x = 2E3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2e3),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn negative_numbers() {
        let input = "-3.14";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Minus, Token::FloatingPoint(3.14), Token::Newline,]
        );

        let input = "-3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Minus, Token::Integer(3), Token::Newline,]
        );

        let input = "2 - 3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Integer(2),
                Token::Minus,
                Token::Integer(3),
                Token::Newline,
            ]
        );

        let input = "-2e-3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Minus, Token::FloatingPoint(2e-3), Token::Newline,]
        );

        let input = "3-i";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Integer(3),
                Token::Minus,
                Token::Identifier("i".into()),
                Token::Newline,
            ]
        );

        let input = "2 + -3";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Integer(2),
                Token::Plus,
                Token::Minus,
                Token::Integer(3),
                Token::Newline,
            ]
        );

        let input = "-(3)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Minus,
                Token::LParen,
                Token::Integer(3),
                Token::RParen,
                Token::Newline,
            ]
        );

        let input = "-(2 + 3)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Minus,
                Token::LParen,
                Token::Integer(2),
                Token::Plus,
                Token::Integer(3),
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn lists() {
        let input = "[1,2,3]";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::Newline,
            ]
        );

        let input = "[1, 2, 3]";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::Newline,
            ]
        );

        let input = "a = [1, 2, 3]";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::Newline,
            ]
        );

        let input = "list([1, 2, 3])";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("list".to_string()),
                Token::LParen,
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn sets() {
        let input = "{1,2,3}";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::Newline,
            ]
        );

        let input = "{1, 2, 3}";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::Newline,
            ]
        );

        let input = "a = {1, 2, 3}";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::Newline,
            ]
        );

        let input = "set({1, 2, 3})";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("set".to_string()),
                Token::LParen,
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn index_access() {
        let input = "a[0]";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LBracket,
                Token::Integer(0),
                Token::RBracket,
                Token::Newline,
            ]
        );

        let input = "[0,1][1]";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Integer(0),
                Token::Comma,
                Token::Integer(1),
                Token::RBracket,
                Token::LBracket,
                Token::Integer(1),
                Token::RBracket,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
for i in a:
    print(a)
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::Identifier("a".to_string()),
                Token::RParen,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn list_comprehension() {
        let input = r#"
b = [ i * 2 for i in a ]
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("b".to_string()),
                Token::Assign,
                Token::LBracket,
                Token::Identifier("i".to_string()),
                Token::Asterisk,
                Token::Integer(2),
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::RBracket,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn tuples() {
        let input = r#"
(1,2)
print((1,2))
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::RParen,
                Token::Newline,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::LParen,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::RParen,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn generators() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n - 1
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Def,
                Token::Identifier("countdown".to_string()),
                Token::LParen,
                Token::Identifier("n".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::While,
                Token::Identifier("n".to_string()),
                Token::GreaterThan,
                Token::Integer(0),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Yield,
                Token::Identifier("n".to_string()),
                Token::Newline,
                Token::Identifier("n".to_string()),
                Token::Assign,
                Token::Identifier("n".to_string()),
                Token::Minus,
                Token::Integer(1),
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn inheritance() {
        let input = r#"
class Foo(Parent):
    def __init__(self):
        self.x = 0
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Class,
                Token::Identifier("Foo".to_string()),
                Token::LParen,
                Token::Identifier("Parent".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Def,
                Token::Identifier("__init__".to_string()),
                Token::LParen,
                Token::Identifier("self".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("self".to_string()),
                Token::Dot,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::Integer(0),
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn dictionaries() {
        let input = r#"
a = { "b": 4, 'c': 5 }
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::LBrace,
                Token::StringLiteral("b".to_string()),
                Token::Colon,
                Token::Integer(4),
                Token::Comma,
                Token::StringLiteral("c".to_string()),
                Token::Colon,
                Token::Integer(5),
                Token::RBrace,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn async_await() {
        let input = r#"
async def main():
    task_1 = asyncio.create_task(task1())
    await task_1
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Async,
                Token::Def,
                Token::Identifier("main".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("task_1".to_string()),
                Token::Assign,
                Token::Identifier("asyncio".to_string()),
                Token::Dot,
                Token::Identifier("create_task".to_string()),
                Token::LParen,
                Token::Identifier("task1".to_string()),
                Token::LParen,
                Token::RParen,
                Token::RParen,
                Token::Newline,
                Token::Await,
                Token::Identifier("task_1".to_string()),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn multiline_string() {
        let input = r#"
"""comment 5-lines
5-types
"""
a = 1
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral("comment 5-lines\n5-types\n".into()),
                Token::Newline,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = r#"
'''comment 5-lines
5-types
'''
a = 1
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::StringLiteral("comment 5-lines\n5-types\n".into()),
                Token::Newline,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Integer(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn assert() {
        let input = "assert True";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Assert, Token::BooleanLiteral(true), Token::Newline,]
        );
    }

    #[test]
    fn try_except_finally() {
        let input = r#"
try:
    4 / 0
except:
    a = 2
finally:
    a = 3
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Try,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Integer(4),
                Token::Slash,
                Token::Integer(0),
                Token::Newline,
                Token::Dedent,
                Token::Except,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("a".into()),
                Token::Assign,
                Token::Integer(2),
                Token::Newline,
                Token::Dedent,
                Token::Finally,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("a".into()),
                Token::Assign,
                Token::Integer(3),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn hex_literal() {
        let input = r#"
a = 0x0010
b
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::Assign,
                Token::HexLiteral("0x0010".into()),
                Token::Newline,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn octal_literal() {
        let input = "a = 0o0010";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::Assign,
                Token::OctalLiteral("0o0010".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn binary_literal() {
        let input = "a = 0b0010";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::Assign,
                Token::BinaryLiteral("0b0010".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn kwargs() {
        let input = r#"
def add(*args, **kwargs):
    pass
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Def,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Asterisk,
                Token::Identifier("args".to_string()),
                Token::Comma,
                Token::DoubleAsterisk,
                Token::Identifier("kwargs".to_string()),
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Pass,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn decorator() {
        let input = r#"
@test_decorator
def get_val():
    return 2
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::AtSign,
                Token::Identifier("test_decorator".to_string()),
                Token::Newline,
                Token::Def,
                Token::Identifier("get_val".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Return,
                Token::Integer(2),
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn raise() {
        let input = "raise Exception";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Raise,
                Token::Identifier("Exception".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn context_manager() {
        let input = r#"
with open('test.txt') as f:
    f.read()
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::With,
                Token::Identifier("open".into()),
                Token::LParen,
                Token::StringLiteral("test.txt".into()),
                Token::RParen,
                Token::As,
                Token::Identifier("f".into()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("f".into()),
                Token::Dot,
                Token::Identifier("read".into()),
                Token::LParen,
                Token::RParen,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn ellipsis() {
        let input = "type(...)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("type".into()),
                Token::LParen,
                Token::Ellipsis,
                Token::RParen,
                Token::Newline,
            ]
        );

        let input = "type(Ellipsis)";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("type".into()),
                Token::LParen,
                Token::Ellipsis,
                Token::RParen,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn delete() {
        let input = r#"del a"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Del, Token::Identifier("a".into()), Token::Newline,]
        );
    }

    #[test]
    fn byte_string() {
        let input = r#"b'hello'"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::ByteStringLiteral("hello".into()), Token::Newline,]
        );
    }

    #[test]
    fn compound_assignment() {
        let input = "a += 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::PlusEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a -= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::MinusEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a *= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::AsteriskEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a /= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::SlashEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a &= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseAndEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a ^= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseXorEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a |= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseOrEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a //= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::DoubleSlashEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a <<= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::LeftShiftEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a %= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::ModEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a @= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::MatMulEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a **= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::ExpoEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );

        let input = "a >>= 1";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::RightShiftEquals,
                Token::Integer(1),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn f_strings() {
        let input = r#"
f"Hello {name}"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("Hello ".into()),
                Token::LBrace,
                Token::Identifier("name".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f'Hello {name}'
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("Hello ".into()),
                Token::LBrace,
                Token::Identifier("name".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f"Hello"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("Hello".into()),
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f"Hello {name} goodbye {other}."
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("Hello ".into()),
                Token::LBrace,
                Token::Identifier("name".into()),
                Token::RBrace,
                Token::StringLiteral(" goodbye ".into()),
                Token::LBrace,
                Token::Identifier("other".into()),
                Token::RBrace,
                Token::StringLiteral(".".into()),
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f"{first}{last}"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::LBrace,
                Token::Identifier("first".into()),
                Token::RBrace,
                Token::LBrace,
                Token::Identifier("last".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f"environ({{{formatted_items}}})"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("environ({".into()),
                Token::LBrace,
                Token::Identifier("formatted_items".into()),
                Token::RBrace,
                Token::StringLiteral("})".into()),
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
f"environ({{{formatted_items}after}})"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::StringLiteral("environ({".into()),
                Token::LBrace,
                Token::Identifier("formatted_items".into()),
                Token::RBrace,
                Token::StringLiteral("after})".into()),
                Token::FStringEnd,
                Token::Newline,
            ]
        );

        let input = r#"
      def __repr__():
          return f"environ({{{formatted_items}}})"

      def copy():
          pass
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Indent,
                Token::Def,
                Token::Identifier("__repr__".into()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Return,
                Token::FStringStart,
                Token::StringLiteral("environ({".into()),
                Token::LBrace,
                Token::Identifier("formatted_items".into()),
                Token::RBrace,
                Token::StringLiteral("})".into()),
                Token::FStringEnd,
                Token::Newline,
                Token::Newline,
                Token::Dedent,
                Token::Def,
                Token::Identifier("copy".into()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Pass,
                Token::Newline,
                Token::Dedent,
                Token::Dedent,
            ]
        );

        let input = r#"f"{first}{last!r}""#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::FStringStart,
                Token::LBrace,
                Token::Identifier("first".into()),
                Token::RBrace,
                Token::LBrace,
                Token::Identifier("last".into()),
                Token::Exclamation,
                Token::Identifier("r".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Newline,
            ]
        );
    }

    #[test]
    fn raw_strings() {
        let input = r#"
r"hello"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::RawStringLiteral("hello".into()), Token::Newline,]
        );

        let input = r#"
R"hello"
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::RawStringLiteral("hello".into()), Token::Newline,]
        );

        let input = r#"
r"""OS routines for NT or Posix depending on what system we're on.

This exports:
"""
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::RawStringLiteral(
                "OS routines for NT or Posix depending on what system we're on.\n\nThis exports:\n"
                    .into()
            ),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn binary_operators() {
        let input = "a // b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::DoubleSlash,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "a & b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseAnd,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "a | b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseOr,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "a ^ b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseXor,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "a % b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::Modulo,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "~a";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::BitwiseNot,
                Token::Identifier("a".into()),
                Token::Newline,
            ]
        );

        let input = "a << b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::LeftShift,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );

        let input = "a >> b";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".into()),
                Token::RightShift,
                Token::Identifier("b".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn control_flow() {
        let input = r#"
for i in a:
    continue
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Continue,
                Token::Newline,
                Token::Dedent,
            ]
        );

        let input = r#"
for i in a:
    break
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Break,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn lambda() {
        let input = "lambda: 4";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Lambda,
                Token::Colon,
                Token::Integer(4),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn type_hints() {
        let input = r#"
def add(a: str, b: str) -> int:
    pass
"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Def,
                Token::Identifier("add".into()),
                Token::LParen,
                Token::Identifier("a".into()),
                Token::Colon,
                Token::Identifier("str".into()),
                Token::Comma,
                Token::Identifier("b".into()),
                Token::Colon,
                Token::Identifier("str".into()),
                Token::RParen,
                Token::ReturnTypeArrow,
                Token::Identifier("int".into()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Pass,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }

    #[test]
    fn scope_modifiers() {
        let input = "nonlocal var";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Nonlocal,
                Token::Identifier("var".into()),
                Token::Newline,
            ]
        );

        let input = "global var";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Global,
                Token::Identifier("var".into()),
                Token::Newline,
            ]
        );
    }

    #[test]
    fn not_implemented() {
        let input = "NotImplemented";
        let tokens = tokenize(input);
        assert_eq!(tokens, vec![Token::NotImplemented, Token::Newline,]);
    }

    #[test]
    fn blocks() {
        let input = r#"
for i in a:
    continue


"#;
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Continue,
                Token::Newline,
                Token::Dedent,
            ]
        );
    }
}
