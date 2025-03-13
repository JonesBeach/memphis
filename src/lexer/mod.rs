use crate::{
    core::{log, LogLevel},
    types::errors::LexerError,
};
use std::{iter::Peekable, str::Chars};

pub mod types;

use self::types::{MultilineString, Token};

pub struct Lexer {
    tokens: Vec<Token>,
    error: Option<LexerError>,

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

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            tokens: Vec::new(),
            error: None,
            multiline_string: None,
            multiline_context: 0,
            in_f_string_expr: false,
        };
        let _ = lexer.tokenize(&input);

        lexer
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// Since we tokenize one line at a time, we must consider whether we are inside a multiline
    /// string or not when choosing to emit a `Newline` token or insert a `\n` char into ongoing
    /// multiline string literal.
    fn emit_newline(&mut self) {
        match self.multiline_string {
            None => {
                self.tokens.push(Token::Newline);
            }
            Some(ref mut s) => {
                s.literal.push('\n');
            }
        }
    }

    /// Are we inside a multi-line string or a multi-line context (indicated by {}, (), or []). If
    /// so, our rules for checking and emitting Indent and Dedent tokens are different/disabled.
    fn check_in_block(&self) -> bool {
        self.multiline_context == 0 && self.multiline_string.is_none()
    }

    fn tokenize(&mut self, input: &str) -> Result<(), LexerError> {
        self.tokens.clear();

        // Each element here indicates the number of spaces at the beginning of the column for this
        // indentation block. Python does not enforce a particular number of spaces, only that for
        // a given indentation, you are consistent with the number of spaces.
        let mut indentation_stack = vec![0];

        for line in input.lines() {
            if line.is_empty() {
                self.emit_newline();
                continue;
            }

            let mut chars = line.chars().peekable();

            let mut spaces = 0;
            while let Some(&c) = chars.peek() {
                if c == ' ' {
                    spaces += 1;
                    chars.next();
                } else {
                    break;
                }
            }

            if self.check_in_block() {
                if spaces > *indentation_stack.last().unwrap() {
                    indentation_stack.push(spaces);
                    self.tokens.push(Token::Indent);
                } else {
                    while spaces < *indentation_stack.last().unwrap() {
                        indentation_stack.pop();
                        self.tokens.push(Token::Dedent);
                    }
                }
            }

            self.tokenize_line(line.trim_start())?;
            self.emit_newline();
        }
        if self.tokens.last() == Some(&Token::Newline) {
            self.tokens.remove(self.tokens.len() - 1);
        }

        while indentation_stack.len() > 1 {
            indentation_stack.pop();
            self.tokens.push(Token::Dedent);
        }

        self.tokens.push(Token::Eof);
        Ok(())
    }

    fn tokenize_binary_literal(&self, chars: &mut Peekable<Chars>) -> Token {
        let mut literal = String::from("0b");

        while let Some(&c) = chars.peek() {
            if c.is_digit(2) {
                literal.push(c);
                chars.next();
            } else {
                break;
            }
        }

        Token::BinaryLiteral(literal)
    }

    fn tokenize_octal_literal(&self, chars: &mut Peekable<Chars>) -> Token {
        let mut literal = String::from("0o");

        while let Some(&c) = chars.peek() {
            if c.is_digit(8) {
                literal.push(c);
                chars.next();
            } else {
                break;
            }
        }

        Token::OctalLiteral(literal)
    }

    fn tokenize_hex_literal(&self, chars: &mut Peekable<Chars>) -> Token {
        let mut literal = String::from("0x");

        while let Some(&c) = chars.peek() {
            if c.is_ascii_hexdigit() {
                literal.push(c);
                chars.next();
            } else {
                break;
            }
        }

        Token::HexLiteral(literal)
    }

    /// While inside of an f-string, we do not know the end of a string literal until we hit
    /// another character.
    fn save_string_literal(&mut self, literal: &mut String) {
        if !literal.is_empty() {
            self.tokens.push(Token::StringLiteral(literal.clone()));
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
                    self.tokens.push(Token::LBrace);
                    self.in_f_string_expr = true;

                    // We are now inside an f-string expression and should use our main lexing loop
                    // to generate the tokens until we detect the end of the f-string expression.
                    return;
                }
            } else if matches!(c, '"' | '\'') {
                self.save_string_literal(&mut literal);
                chars.next();
                self.tokens.push(Token::FStringEnd);

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
                    self.tokens.push(Token::RBrace);
                    self.in_f_string_expr = false;
                }
            } else {
                literal.push(c);
                chars.next();
            }
        }
    }

    fn tokenize_line(&mut self, input: &str) -> Result<(), LexerError> {
        let mut chars = input.chars().peekable();

        while let Some(&c) = chars.peek() {
            log(LogLevel::Trace, || format!("char: {}", c));
            if c == '#' {
                // Comments cause the rest of the line to be ignored
                break;
            } else if let Some(string) = &self.multiline_string {
                if c == string.end_char
                    && chars.clone().nth(1) == Some(string.end_char)
                    && chars.clone().nth(2) == Some(string.end_char)
                {
                    chars.next();
                    chars.next();
                    chars.next();
                    if string.raw {
                        self.tokens
                            .push(Token::RawStringLiteral(string.literal.clone()));
                    } else {
                        self.tokens
                            .push(Token::StringLiteral(string.literal.clone()));
                    }
                    self.multiline_string = None;
                } else {
                    chars.next();
                    if let Some(ref mut s) = self.multiline_string {
                        s.literal.push(c);
                    } else {
                        panic!("Expected a raw string literal, but found None");
                    }
                }
            } else if (c == '"'
                && chars.clone().nth(1) == Some('"')
                && chars.clone().nth(2) == Some('"'))
                || (c == '\''
                    && chars.clone().nth(1) == Some('\'')
                    && chars.clone().nth(2) == Some('\''))
            {
                self.multiline_string = Some(MultilineString::new(false, c));
                chars.next();
                chars.next();
                chars.next();
            } else if (c == 'r'
                && (chars.clone().nth(1) == Some('"')
                    && chars.clone().nth(2) == Some('"')
                    && chars.clone().nth(3) == Some('"')))
                || (chars.clone().nth(1) == Some('\'')
                    && chars.clone().nth(2) == Some('\'')
                    && chars.clone().nth(3) == Some('\''))
            {
                self.multiline_string =
                    Some(MultilineString::new(true, chars.clone().nth(1).unwrap()));
                chars.next();
                chars.next();
                chars.next();
                chars.next();
            } else if c == '.'
                && chars.clone().nth(1) == Some('.')
                && chars.clone().nth(2) == Some('.')
            {
                chars.next();
                chars.next();
                chars.next();
                self.tokens.push(Token::Ellipsis);
            } else if c.is_whitespace() {
                chars.next();
            } else if self.in_f_string_expr && c == '}' {
                self.tokenize_f_string(&mut chars);
            } else if matches!(c, 'f' | 'F') && matches!(chars.clone().nth(1), Some('"' | '\'')) {
                chars.next();
                chars.next();
                self.tokens.push(Token::FStringStart);
                self.tokenize_f_string(&mut chars);
            } else if c == 'b' && chars.clone().nth(1) == Some('\'') {
                chars.next();
                chars.next();
                let mut literal = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '\'' {
                        chars.next();
                        break;
                    } else {
                        literal.push(c);
                        chars.next();
                    }
                }
                self.tokens.push(Token::ByteStringLiteral(literal));
            } else if c == '0' && matches!(chars.clone().nth(1), Some('b' | 'B')) {
                chars.next();
                chars.next();
                self.tokens.push(self.tokenize_binary_literal(&mut chars));
            } else if c == '0' && matches!(chars.clone().nth(1), Some('o' | 'O')) {
                chars.next();
                chars.next();
                self.tokens.push(self.tokenize_octal_literal(&mut chars));
            } else if c == '0' && matches!(chars.clone().nth(1), Some('x' | 'X')) {
                chars.next();
                chars.next();
                self.tokens.push(self.tokenize_hex_literal(&mut chars));
            } else if matches!(c, 'r' | 'R') && chars.clone().nth(1) == Some('"') {
                chars.next();
                chars.next();
                let mut literal = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '"' {
                        chars.next();
                        break;
                    } else {
                        literal.push(c);
                        chars.next();
                    }
                }
                self.tokens.push(Token::RawStringLiteral(literal));
            } else if c == '"' {
                let mut literal = String::new();
                chars.next();
                while let Some(&c) = chars.peek() {
                    if c == '"' {
                        chars.next();
                        break;
                    } else {
                        literal.push(c);
                        chars.next();
                    }
                }
                self.tokens.push(Token::StringLiteral(literal));
            } else if c == '\'' {
                let mut literal = String::new();
                chars.next();
                while let Some(&c) = chars.peek() {
                    if c == '\'' {
                        chars.next();
                        break;
                    } else {
                        literal.push(c);
                        chars.next();
                    }
                }
                self.tokens.push(Token::StringLiteral(literal));
            } else if c.is_alphabetic() || c == '_' {
                let mut identifier = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        identifier.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

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
                self.tokens.push(token);
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
                    self.tokens
                        .push(Token::FloatingPoint(value.parse().unwrap()));
                } else {
                    self.tokens.push(Token::Integer(value.parse().unwrap()));
                }
            } else if matches!(c, '+' | '-' | '*' | '/' | '&' | '^' | '|' | '%' | '@')
                && chars.clone().nth(1) == Some('=')
            {
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
                self.tokens.push(token);
            } else if matches!(c, '/' | '*' | '<' | '>')
                // Look for a second character in a row which is the same
                && chars.clone().nth(1) == Some(c)
                && chars.clone().nth(2) == Some('=')
            {
                let mut identifier = String::new();
                while let Some(&c) = chars.peek() {
                    identifier.push(c);
                    chars.next();

                    if c == '=' {
                        break;
                    }
                }

                let token = match identifier.as_str() {
                    "//=" => Token::DoubleSlashEquals,
                    "**=" => Token::ExpoEquals,
                    "<<=" => Token::LeftShiftEquals,
                    ">>=" => Token::RightShiftEquals,
                    _ => unreachable!(),
                };
                self.tokens.push(token);
            } else if c == '-' && chars.clone().nth(1) == Some('>') {
                chars.next();
                chars.next();
                self.tokens.push(Token::ReturnTypeArrow);
            } else if matches!(c, '=' | '!' | '<' | '>') {
                let mut operator = String::new();
                while let Some(&c) = chars.peek() {
                    if matches!(c, '=' | '!' | '<' | '>') {
                        operator.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

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
                    _ => {
                        self.error = Some(LexerError::UnexpectedCharacter(c));
                        Token::InvalidCharacter(c)
                    }
                };
                self.tokens.push(token);
            } else if c == '*' && matches!(chars.clone().nth(1), Some('*')) {
                chars.next();
                chars.next();
                self.tokens.push(Token::DoubleAsterisk);
            } else if c == '/' && matches!(chars.clone().nth(1), Some('/')) {
                chars.next();
                chars.next();
                self.tokens.push(Token::DoubleSlash);
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
                    _ => {
                        self.error = Some(LexerError::UnexpectedCharacter(c));
                        Token::InvalidCharacter(c)
                    }
                };

                // Detect when we are inside multi-line data structures, which should not be
                // treated the same as blocks.
                if matches!(token, Token::LParen | Token::LBrace | Token::LBracket) {
                    self.multiline_context += 1;
                } else if matches!(token, Token::RParen | Token::RBrace | Token::RBracket) {
                    self.multiline_context -= 1;
                }

                self.tokens.push(token);
                chars.next();
            }
        }

        if let Some(error) = self.error.as_ref() {
            Err(error.clone())
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Lexer {
        fn from_str(input: &str) -> Self {
            Self::new(input.to_owned())
        }
    }

    #[test]
    fn function_definition() {
        let input = r#"
def add(x, y):
    return x + y
"#;
        let lexer = Lexer::from_str(input);

        assert_eq!(
            lexer.tokens,
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
                Token::Indent,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::Plus,
                Token::Identifier("y".to_string()),
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn invalid_character() {
        let input = "2 + $";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Integer(2), Token::Plus, Token::InvalidCharacter('$'),]
        );
    }

    #[test]
    fn comparison_operators() {
        let input = "a > b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::GreaterThan,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a < b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LessThan,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a == b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Equal,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a != b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::NotEqual,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a >= b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::GreaterThanOrEqual,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a <= b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LessThanOrEqual,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn boolean_expressions() {
        let input = "a and b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::And,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a or b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Or,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a in b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::In,
                Token::Identifier("b".to_string()),
                Token::Eof,
            ]
        );

        let input = "a is None";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::Is,
                Token::None,
                Token::Eof,
            ]
        );

        let input = "not b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Not, Token::Identifier("b".to_string()), Token::Eof,]
        );

        let input = "not (b or c)";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Not,
                Token::LParen,
                Token::Identifier("b".to_string()),
                Token::Or,
                Token::Identifier("c".to_string()),
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn boolean_literals() {
        let input = "x = True";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::BooleanLiteral(true),
                Token::Eof,
            ]
        );

        let input = "x = False";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::BooleanLiteral(false),
                Token::Eof,
            ]
        );

        let input = "x = None";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::None,
                Token::Eof,
            ]
        );

        let input = "return None";
        let lexer = Lexer::from_str(input);
        assert_eq!(lexer.tokens, vec![Token::Return, Token::None, Token::Eof,]);
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn while_loop() {
        let input = r#"
while True:
    print("busy loop")
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::While,
                Token::BooleanLiteral(true),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::StringLiteral("busy loop".to_string()),
                Token::RParen,
                Token::Dedent,
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn class_instantiation() {
        let input = "foo = Foo()\n";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Assign,
                Token::Identifier("Foo".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn method_invocation() {
        let input = "foo = Foo()\nfoo.bar()";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Import,
                Token::Identifier("other".to_string()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn selective_import() {
        let input = "from other import something";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::Eof,
            ]
        );

        let input = "from other import something as something_else";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::As,
                Token::Identifier("something_else".to_string()),
                Token::Eof,
            ]
        );

        let input = "from other import *";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Asterisk,
                Token::Eof,
            ]
        );

        let input = "from other import something, something_else";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::From,
                Token::Identifier("other".to_string()),
                Token::Import,
                Token::Identifier("something".to_string()),
                Token::Comma,
                Token::Identifier("something_else".to_string()),
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn floating_point() {
        let input = "x = 3.14";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(3.14),
                Token::Eof,
            ]
        );

        let input = "x = 2.5e-3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2.5e-3),
                Token::Eof,
            ]
        );

        let input = "x = 2.5E-3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2.5e-3),
                Token::Eof,
            ]
        );

        let input = "x = 2E-3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2e-3),
                Token::Eof,
            ]
        );

        let input = "x = 2E3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::FloatingPoint(2e3),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn negative_numbers() {
        let input = "-3.14";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Minus, Token::FloatingPoint(3.14), Token::Eof,]
        );

        let input = "-3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Minus, Token::Integer(3), Token::Eof,]
        );

        let input = "2 - 3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Integer(2),
                Token::Minus,
                Token::Integer(3),
                Token::Eof,
            ]
        );

        let input = "-2e-3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Minus, Token::FloatingPoint(2e-3), Token::Eof,]
        );

        let input = "3-i";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Integer(3),
                Token::Minus,
                Token::Identifier("i".into()),
                Token::Eof,
            ]
        );

        let input = "2 + -3";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Integer(2),
                Token::Plus,
                Token::Minus,
                Token::Integer(3),
                Token::Eof,
            ]
        );

        let input = "-(3)";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Minus,
                Token::LParen,
                Token::Integer(3),
                Token::RParen,
                Token::Eof,
            ]
        );

        let input = "-(2 + 3)";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Minus,
                Token::LParen,
                Token::Integer(2),
                Token::Plus,
                Token::Integer(3),
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn lists() {
        let input = "[1,2,3]";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::Eof,
            ]
        );

        let input = "[1, 2, 3]";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket,
                Token::Eof,
            ]
        );

        let input = "a = [1, 2, 3]";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
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
                Token::Eof,
            ]
        );

        let input = "list([1, 2, 3])";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn sets() {
        let input = "{1,2,3}";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::Eof,
            ]
        );

        let input = "{1, 2, 3}";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::LBrace,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBrace,
                Token::Eof,
            ]
        );

        let input = "a = {1, 2, 3}";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
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
                Token::Eof,
            ]
        );

        let input = "set({1, 2, 3})";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn index_access() {
        let input = "a[0]";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::LBracket,
                Token::Integer(0),
                Token::RBracket,
                Token::Eof,
            ]
        );

        let input = "[0,1][1]";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::LBracket,
                Token::Integer(0),
                Token::Comma,
                Token::Integer(1),
                Token::RBracket,
                Token::LBracket,
                Token::Integer(1),
                Token::RBracket,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
for i in a:
    print(a)
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn list_comprehension() {
        let input = r#"
b = [ i * 2 for i in a ]
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn tuples() {
        let input = r#"
(1,2)
print((1,2))
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Dedent,
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn dictionaries() {
        let input = r#"
a = { "b": 4, 'c': 5 }
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::StringLiteral("comment 5-lines\n5-types\n".into()),
                Token::Newline,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
'''comment 5-lines
5-types
'''
a = 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::StringLiteral("comment 5-lines\n5-types\n".into()),
                Token::Newline,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Integer(1),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Assert,
                Token::BooleanLiteral(true),
                Token::Eof,
            ]
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn hex_literal() {
        let input = r#"
a = 0x0010
b
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::Assign,
                Token::HexLiteral("0x0010".into()),
                Token::Newline,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn octal_literal() {
        let input = r#"
a = 0o0010
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::Assign,
                Token::OctalLiteral("0o0010".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn binary_literal() {
        let input = r#"
a = 0b0010
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::Assign,
                Token::BinaryLiteral("0b0010".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn kwargs() {
        let input = r#"
def add(*args, **kwargs):
    pass
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
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
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn raise() {
        let input = r#"
raise Exception
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Raise,
                Token::Identifier("Exception".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn context_manager() {
        let input = r#"
with open('test.txt') as f:
    f.read()
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn ellipsis() {
        let input = r#"
type(...)
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("type".into()),
                Token::LParen,
                Token::Ellipsis,
                Token::RParen,
                Token::Eof,
            ]
        );

        let input = r#"
type(Ellipsis)
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("type".into()),
                Token::LParen,
                Token::Ellipsis,
                Token::RParen,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn delete() {
        let input = r#"
del a
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Del,
                Token::Identifier("a".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn byte_string() {
        let input = r#"
b'hello'
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::ByteStringLiteral("hello".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn compound_assignment() {
        let input = r#"
a += 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::PlusEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a -= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::MinusEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a *= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::AsteriskEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a /= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::SlashEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a &= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::BitwiseAndEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a ^= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::BitwiseXorEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a |= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::BitwiseOrEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a //= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::DoubleSlashEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a <<= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::LeftShiftEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a %= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::ModEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a @= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::MatMulEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a **= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::ExpoEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );

        let input = r#"
a >>= 1
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Identifier("a".into()),
                Token::RightShiftEquals,
                Token::Integer(1),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn f_strings() {
        let input = r#"
f"Hello {name}"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::StringLiteral("Hello ".into()),
                Token::LBrace,
                Token::Identifier("name".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
f'Hello {name}'
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::StringLiteral("Hello ".into()),
                Token::LBrace,
                Token::Identifier("name".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
f"Hello"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::StringLiteral("Hello".into()),
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
f"Hello {name} goodbye {other}."
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
            ]
        );

        let input = r#"
f"{first}{last}"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::LBrace,
                Token::Identifier("first".into()),
                Token::RBrace,
                Token::LBrace,
                Token::Identifier("last".into()),
                Token::RBrace,
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
f"environ({{{formatted_items}}})"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::StringLiteral("environ({".into()),
                Token::LBrace,
                Token::Identifier("formatted_items".into()),
                Token::RBrace,
                Token::StringLiteral("})".into()),
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
f"environ({{{formatted_items}after}})"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::FStringStart,
                Token::StringLiteral("environ({".into()),
                Token::LBrace,
                Token::Identifier("formatted_items".into()),
                Token::RBrace,
                Token::StringLiteral("after})".into()),
                Token::FStringEnd,
                Token::Eof,
            ]
        );

        let input = r#"
      def __repr__():
          return f"environ({{{formatted_items}}})"

      def copy():
          pass
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Dedent,
                Token::Eof,
            ]
        );

        let input = r#"
f"{first}{last!r}"
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn raw_strings() {
        let input = r#"r"hello""#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::RawStringLiteral("hello".into()), Token::Eof,]
        );

        let input = r#"
r"""OS routines for NT or Posix depending on what system we're on.

This exports:
"""
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::RawStringLiteral("OS routines for NT or Posix depending on what system we're on.\n\nThis exports:\n".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn binary_operators() {
        let input = "a // b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::DoubleSlash,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "a & b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseAnd,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "a | b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseOr,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "a ^ b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::BitwiseXor,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "a % b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::Modulo,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "~a";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::BitwiseNot, Token::Identifier("a".into()), Token::Eof,]
        );

        let input = "a << b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::LeftShift,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );

        let input = "a >> b";
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Identifier("a".into()),
                Token::RightShift,
                Token::Identifier("b".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn control_flow() {
        let input = r#"
for i in a:
    continue
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Continue,
                Token::Dedent,
                Token::Eof,
            ]
        );

        let input = r#"
for i in a:
    break
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::For,
                Token::Identifier("i".to_string()),
                Token::In,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Break,
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn lambda() {
        let input = r#"
lambda: 4
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Lambda,
                Token::Colon,
                Token::Integer(4),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn type_hints() {
        let input = r#"
def add(a: str, b: str) -> int:
    pass
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
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
                Token::Dedent,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn scope_modifiers() {
        let input = r#"
nonlocal var
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Nonlocal,
                Token::Identifier("var".into()),
                Token::Eof,
            ]
        );

        let input = r#"
global var
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![
                Token::Newline,
                Token::Global,
                Token::Identifier("var".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn not_implemented() {
        let input = r#"
NotImplemented
"#;
        let lexer = Lexer::from_str(input);
        assert_eq!(
            lexer.tokens,
            vec![Token::Newline, Token::NotImplemented, Token::Eof,]
        );
    }
}
