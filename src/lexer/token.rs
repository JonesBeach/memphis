use crate::parser::types::{BinOp, CompareOp, LogicalOp};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Def,
    Del,
    Lambda,
    If,
    Else,
    Elif,
    While,
    Return,
    For,
    In,
    Yield,
    Pass,
    Class,
    Try,
    Except,
    Finally,
    Raise,
    From,
    As,
    Import,
    With,
    Assert,
    Dot,
    And,
    Or,
    Not,
    Is,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
    Modulo,
    ReturnTypeArrow,
    Indent,
    Dedent,
    Identifier(String),
    StringLiteral(String),
    RawStringLiteral(String),
    BytesLiteral(Vec<u8>),
    None,
    Ellipsis,
    NotImplemented,
    BooleanLiteral(bool),
    // This is unsigned because the minus unary operator is not handled by
    // this lexer, but rather left for the parser which has better context.
    Integer(u64),
    FloatingPoint(f64),
    HexLiteral(String),
    OctalLiteral(String),
    BinaryLiteral(String),
    DoubleAsterisk,
    Plus,
    Minus,
    Asterisk,
    Slash,
    DoubleSlash,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    BitwiseAndEquals,
    BitwiseXorEquals,
    BitwiseOrEquals,
    DoubleSlashEquals,
    ModEquals,
    MatMulEquals,
    ExpoEquals,
    LeftShiftEquals,
    RightShiftEquals,
    GreaterThan,
    LessThan,
    Equal,
    NotEqual,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Assign,
    Comma,
    Colon,
    Exclamation,
    AtSign,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Break,
    Continue,
    Async,
    Await,
    FStringStart,
    FStringEnd,
    Nonlocal,
    Global,
    Newline,
    Eof,
    InvalidCharacter(char),
}

impl TryFrom<&Token> for BinOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        let op = match value {
            Token::BitwiseAnd => BinOp::BitwiseAnd,
            Token::BitwiseOr => BinOp::BitwiseOr,
            Token::BitwiseXor => BinOp::BitwiseXor,
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub,
            Token::LeftShift => BinOp::LeftShift,
            Token::RightShift => BinOp::RightShift,
            Token::Asterisk => BinOp::Mul,
            Token::Slash => BinOp::Div,
            Token::DoubleSlash => BinOp::IntegerDiv,
            Token::Modulo => BinOp::Mod,
            Token::AtSign => BinOp::MatMul,
            _ => return Err(()),
        };

        Ok(op)
    }
}

impl TryFrom<&Token> for CompareOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        let op = match value {
            Token::LessThan => CompareOp::LessThan,
            Token::LessThanOrEqual => CompareOp::LessThanOrEqual,
            Token::GreaterThan => CompareOp::GreaterThan,
            Token::GreaterThanOrEqual => CompareOp::GreaterThanOrEqual,
            Token::Equal => CompareOp::Equals,
            Token::NotEqual => CompareOp::NotEquals,
            Token::In => CompareOp::In,
            Token::Is => CompareOp::Is,
            _ => return Err(()),
        };

        Ok(op)
    }
}

impl TryFrom<&Token> for LogicalOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        let op = match value {
            Token::And => LogicalOp::And,
            Token::Or => LogicalOp::Or,
            _ => return Err(()),
        };

        Ok(op)
    }
}

impl Token {
    /// These types, when called with type(..), are considered type aliases. These were introduced
    /// in Python 3.9 from PEP 613: https://peps.python.org/pep-0613/
    pub fn is_type(&self) -> bool {
        match self {
            Token::Identifier(i) => matches!(i.as_str(), "list" | "dict" | "int" | "str"),
            Token::Ellipsis => true,
            _ => false,
        }
    }

    /// Checks if this token is one of the `+=`, `-=`, etc. variants.
    pub fn is_compound_assign(&self) -> bool {
        matches!(
            self,
            Token::PlusEquals
                | Token::MinusEquals
                | Token::AsteriskEquals
                | Token::SlashEquals
                | Token::BitwiseAndEquals
                | Token::BitwiseOrEquals
                | Token::BitwiseXorEquals
                | Token::DoubleSlashEquals
                | Token::LeftShiftEquals
                | Token::RightShiftEquals
                | Token::ModEquals
                | Token::MatMulEquals
                | Token::ExpoEquals
        )
    }
}
