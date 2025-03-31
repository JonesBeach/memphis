use std::collections::HashSet;

pub mod static_analysis;
pub mod token_buffer;
pub mod types;
#[cfg(test)]
#[macro_use]
pub mod test_utils;

use token_buffer::TokenBuffer;
use types::Statement;

use crate::{
    ast,
    core::{log, LogLevel},
    domain::ExceptionLiteral,
    lexer::{types::Token, Lexer},
    parser::types::{
        Alias, Ast, BinOp, CallArg, CallArgs, CompoundOperator, ConditionalBlock, DictOperation,
        ExceptClause, ExceptionInstance, Expr, ExprFormat, FStringPart, ForClause, FormatOption,
        ImportPath, ImportedItem, KwargsOperation, LogicalOp, LoopIndex, Param, Params,
        RegularImport, SliceParams, StatementKind, TypeNode, UnaryOp, Variable,
    },
    types::errors::ParserError,
};

/// A recursive-descent parser which attempts to encode the full Python grammar.
pub struct Parser<'a> {
    tokens: TokenBuffer<'a>,
    line_number: usize,
    delimiter_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Parser {
            tokens: TokenBuffer::new(lexer),
            line_number: 1,
            delimiter_depth: 0,
        }
    }

    fn current_token(&mut self) -> &Token {
        self.tokens.peek(0)
    }

    pub fn is_finished(&mut self) -> bool {
        self.current_token() == &Token::Eof
    }

    fn end_of_statement(&mut self) -> bool {
        self.is_finished() || self.current_token() == &Token::Newline
    }

    fn inside_delimiter(&self) -> bool {
        self.delimiter_depth > 0
    }

    /// If we are inside a string literal, we must check for newline characters rather than
    /// tokens. These are produced by `Lexer::emit_newline`.
    fn advance_line_number_if_needed(&mut self) {
        if self.current_token() == &Token::Newline {
            self.line_number += 1;
        } else if let Token::StringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        } else if let Token::RawStringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        }
    }

    fn consume_current(&mut self) -> Result<(), ParserError> {
        let token = self.tokens.peek(0).clone();
        self.consume(&token)
    }

    fn consume(&mut self, expected: &Token) -> Result<(), ParserError> {
        let current = self.tokens.peek(0);

        log(LogLevel::Trace, || format!("Token: {:?}", current));

        if current != expected {
            return Err(ParserError::ExpectedToken(
                expected.clone(),
                current.clone(),
            ));
        }

        if matches!(current, Token::LParen | Token::LBracket | Token::LBrace) {
            self.delimiter_depth += 1;
        } else if matches!(current, Token::RParen | Token::RBracket | Token::RBrace) {
            self.delimiter_depth -= 1;
        }

        self.advance_line_number_if_needed();

        self.tokens.consume();

        if self.inside_delimiter() {
            self.consume_optional_many(&Token::Newline);
        }

        Ok(())
    }

    fn consume_optional(&mut self, expected: &Token) {
        if self.current_token() == expected {
            let _ = self.consume(expected);
        }
    }

    fn consume_optional_many(&mut self, expected: &Token) {
        while self.current_token() == expected {
            let _ = self.consume(expected);
        }
    }

    /// Parse an expression in a context where tuples may be expected. A good option if you're not
    /// sure. By tuples here, we mean those that are not indicated by parentheses (those are
    /// handled by detecting a LParen in `parse_factor`).
    ///
    /// ```python
    /// 4, 5
    /// a = 4, 5
    /// a = 1,
    /// ```
    ///
    /// All other expression parsing is immediately delegated to `parse_simple_expr`.
    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_expr".to_string());
        let left = self.parse_simple_expr()?;

        if self.current_token() == &Token::Comma {
            let mut items = vec![left];
            while self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;

                // We need this for the case of a trailing comma, which is most often used for a
                // tuple with a single element.
                //
                // The [`Token::Assign`] is when this happens on the LHS.
                if self.end_of_statement() || self.current_token() == &Token::Assign {
                    break;
                }
                items.push(self.parse_simple_expr()?);
            }

            Ok(Expr::Tuple(items))
        } else {
            Ok(left)
        }
    }

    /// Parse an expression where open tuples are not expected. If you need to support this in a
    /// given context (i.e. a = 4, 5), try `parse_expr`.
    fn parse_simple_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_simple_expr".to_string());
        if self.current_token() == &Token::Await {
            self.parse_await_expr()
        } else {
            self.parse_ternary_expr()
        }
    }

    fn parse_await_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_await_expr".to_string());
        self.consume(&Token::Await)?;
        let right = self.parse_ternary_expr()?;
        Ok(Expr::Await(Box::new(right)))
    }

    /// Implements the Python precedence order in reverse call stack order, meaning the operators
    /// evaluated last will be detected first during this recursive descent.
    ///
    /// Python precedence order is:
    /// - Exponentiation (**) - `parse_exponentiation`
    /// - Literals, Identifiers - `parse_factor`
    /// - Member Access, Index Access - `parse_access_operations`
    /// - Multiplication, Division, Modulo, and Comparison Operators - `parse_term`
    /// - Logical operators (AND/OR) - `parse_logical_term`
    /// - Addition, Subtraction - `parse_add_sub`
    /// - Bitwise Shifts (<<, >>) - `parse_bitwise_shift`
    /// - Bitwise AND (&), OR (|), XOR (^) - `parse_binary_expr`
    /// - Ternary Expression (inline-if) - `parse_ternary_expr`
    fn parse_ternary_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_ternary_expr".to_string());
        let if_value = self.parse_binary_expr()?;

        if self.current_token() == &Token::If {
            self.consume(&Token::If)?;
            let condition = self.parse_binary_expr()?;
            self.consume(&Token::Else)?;
            let else_value = self.parse_binary_expr()?;

            return Ok(Expr::TernaryOp {
                condition: Box::new(condition),
                if_value: Box::new(if_value),
                else_value: Box::new(else_value),
            });
        }

        Ok(if_value)
    }

    fn parse_binary_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_binary_expr".to_string());
        let mut left = self.parse_bitwise_shift()?;

        while matches!(
            self.current_token(),
            Token::BitwiseAnd | Token::BitwiseOr | Token::BitwiseXor
        ) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_bitwise_shift()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_add_sub(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_add_sub".to_string());
        let mut left = self.parse_logical_term()?;

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_logical_term()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_shift(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_bitwise_shift".to_string());
        let mut left = self.parse_add_sub()?;

        while matches!(self.current_token(), Token::LeftShift | Token::RightShift) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_add_sub()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_member_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_member_access".to_string());
        self.consume(&Token::Dot)?;
        let field = self.parse_identifier()?;

        if self.current_token() == &Token::LParen {
            let args = self.parse_function_call_args()?;

            Ok(Expr::MethodCall {
                object: Box::new(left),
                name: field.clone(),
                args,
            })
        } else {
            Ok(Expr::MemberAccess {
                object: Box::new(left),
                field: field.clone(),
            })
        }
    }

    fn parse_index_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_index_access".to_string());
        self.consume(&Token::LBracket)?;
        // [::2]
        let params = if self
            .tokens
            .peek_ahead_contains(&[Token::Colon, Token::Colon])
        {
            self.consume(&Token::Colon)?;
            self.consume(&Token::Colon)?;
            let step = Some(Box::new(self.parse_simple_expr()?));
            (true, None, None, step)
            // [:] - this syntax is useful to replace the items in a list without changing the
            // list's reference
        } else if self
            .tokens
            .peek_ahead_contains(&[Token::Colon, Token::RBracket])
        {
            self.consume(&Token::Colon)?;
            (true, None, None, None)
            // [:2]
        } else if self.tokens.peek_ahead_contains(&[Token::Colon]) {
            self.consume(&Token::Colon)?;
            let stop = Some(Box::new(self.parse_simple_expr()?));
            (true, None, stop, None)
            // [2:]
            // if there is a Colon immediately before the next RBracket
        } else if self.tokens.has(&Token::Colon)
            && self.tokens.num_away(&Token::Colon)? + 1 == self.tokens.num_away(&Token::RBracket)?
        {
            let start = Some(Box::new(self.parse_simple_expr()?));
            self.consume(&Token::Colon)?;
            (true, start, None, None)
            // [1:1:1] or [2:5]
            // if there is a Colon before the next RBracket
        } else if self.tokens.has(&Token::Colon)
            && self.tokens.num_away(&Token::Colon)? < self.tokens.num_away(&Token::RBracket)?
        {
            let start = Some(Box::new(self.parse_simple_expr()?));
            self.consume(&Token::Colon)?;
            let stop = Some(Box::new(self.parse_simple_expr()?));
            let step = if self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                Some(Box::new(self.parse_simple_expr()?))
            } else {
                None
            };
            (true, start, stop, step)
            // [1]
        } else {
            let index = Some(Box::new(self.parse_simple_expr()?));
            (false, index, None, None)
        };
        self.consume(&Token::RBracket)?;

        if !params.0 {
            Ok(Expr::IndexAccess {
                object: Box::new(left),
                index: params.1.unwrap(),
            })
        } else {
            Ok(Expr::SliceOperation {
                object: Box::new(left),
                params: SliceParams {
                    start: params.1,
                    stop: params.2,
                    step: params.3,
                },
            })
        }
    }

    /// This is recursive to the right to create a right-associativity binary operator.
    fn parse_exponentiation(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_exponentiation".to_string());
        let mut left = self.parse_factor()?;

        while self.current_token() == &Token::DoubleAsterisk {
            self.consume(&Token::DoubleAsterisk)?;
            let right = self.parse_exponentiation()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op: BinOp::Expo,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_access_operations(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_access_operations".to_string());
        let mut left = self.parse_exponentiation()?;

        while matches!(self.current_token(), Token::Dot | Token::LBracket) {
            left = match self.current_token() {
                Token::Dot => self.parse_member_access(left)?,
                Token::LBracket => self.parse_index_access(left)?,
                _ => unreachable!(),
            };
        }

        if self.current_token() == &Token::LParen {
            let args = self.parse_function_call_args()?;
            left = Expr::FunctionCall {
                name: "<anonymous_from_callee>".into(),
                args,
                callee: Some(Box::new(left)),
            }
        }

        Ok(left)
    }

    fn parse_logical_term(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_logical_term".to_string());
        let mut left = self.parse_term()?;

        while matches!(self.current_token(), Token::And | Token::Or) {
            let op = LogicalOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_term()?;
            left = Expr::LogicalOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_term".to_string());
        let mut left = self.parse_access_operations()?;

        while matches!(
            self.current_token(),
            Token::Asterisk | Token::Slash | Token::DoubleSlash | Token::Modulo | Token::AtSign
        ) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_access_operations()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        while matches!(
            self.current_token(),
            Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::Equal
                | Token::NotEqual
                | Token::In
                | Token::Is
        ) || self.tokens.peek_ahead_contains(&[Token::Not, Token::In])
            || self.tokens.peek_ahead_contains(&[Token::Is, Token::Not])
        {
            // Handle two tokens to produce one `BinOp::NotIn` operation. If this gets too messy,
            // we could look to move multi-word tokens into the lexer.
            let op = if self.tokens.peek_ahead_contains(&[Token::Not, Token::In]) {
                self.consume(&Token::Not)?;
                self.consume(&Token::In)?;
                BinOp::NotIn
            } else if self.tokens.peek_ahead_contains(&[Token::Is, Token::Not]) {
                self.consume(&Token::Is)?;
                self.consume(&Token::Not)?;
                BinOp::IsNot
            } else {
                let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
                self.consume_current()?;
                op
            };

            let right = self.parse_term()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_minus(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Minus)?;
        match self.current_token().clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(-(i as i64)))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::FloatingPoint(-i))
            }
            _ => {
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Minus,
                    right: Box::new(right),
                })
            }
        }
    }

    /// The unary plus operator is a no-op for integers and floats, but exists to provide custom
    /// behaviors using `Dunder::Pos`.
    fn parse_plus(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Plus)?;
        match self.current_token().clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(i as i64))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::FloatingPoint(i))
            }
            _ => {
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Plus,
                    right: Box::new(right),
                })
            }
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || {
            format!("parse_factor: {:?}", self.current_token())
        });
        match self.current_token().clone() {
            Token::Minus => self.parse_minus(),
            Token::Plus => self.parse_plus(),
            Token::Asterisk => {
                self.consume(&Token::Asterisk)?;
                let right = self.parse_simple_expr()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Unpack,
                    right: Box::new(right),
                })
            }
            Token::DoubleAsterisk => {
                self.consume(&Token::DoubleAsterisk)?;
                let right = self.parse_simple_expr()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::DictUnpack,
                    right: Box::new(right),
                })
            }
            Token::Yield => {
                self.consume(&Token::Yield)?;

                if self.current_token() == &Token::From {
                    self.consume(&Token::From)?;
                    let expr = self.parse_simple_expr()?;
                    Ok(Expr::YieldFrom(Box::new(expr)))
                // The [`Token::RParen`] can be found on generator lambdas.
                } else if self.end_of_statement() || self.current_token() == &Token::RParen {
                    Ok(Expr::Yield(None))
                } else {
                    let expr = self.parse_simple_expr()?;
                    Ok(Expr::Yield(Some(Box::new(expr))))
                }
            }
            Token::Not => {
                self.consume(&Token::Not)?;
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Not,
                    right: Box::new(right),
                })
            }
            Token::BitwiseNot => {
                self.consume(&Token::BitwiseNot)?;
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::BitwiseNot,
                    right: Box::new(right),
                })
            }
            Token::None => {
                self.consume(&Token::None)?;
                Ok(Expr::None)
            }
            Token::NotImplemented => {
                self.consume(&Token::NotImplemented)?;
                Ok(Expr::NotImplemented)
            }
            Token::Ellipsis => {
                self.consume(&Token::Ellipsis)?;
                Ok(Expr::Ellipsis)
            }
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(i as i64))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::FloatingPoint(i))
            }
            Token::BooleanLiteral(b) => {
                self.consume(&Token::BooleanLiteral(b))?;
                Ok(Expr::Boolean(b))
            }
            Token::Identifier(_) => {
                if self.tokens.peek(1) == &Token::LParen {
                    let name = self.parse_identifier()?;
                    let args = self.parse_function_call_args()?;

                    Ok(Expr::FunctionCall {
                        name,
                        args,
                        callee: None,
                    })
                } else if self.current_token().is_type() {
                    let type_node = self.parse_type_node()?;

                    match type_node {
                        TypeNode::Basic(type_) => Ok(Expr::Variable(type_)),
                        _ => Ok(Expr::TypeNode(type_node)),
                    }
                } else {
                    Ok(Expr::Variable(self.parse_identifier()?))
                }
            }
            Token::LParen => self.parse_tuple(),
            Token::LBracket => self.parse_list(),
            Token::LBrace => self.parse_set(),
            Token::Lambda => self.parse_lambda(),
            Token::StringLiteral(literal) => {
                self.consume(&Token::StringLiteral(literal.clone()))?;
                Ok(Expr::StringLiteral(literal))
            }
            Token::RawStringLiteral(literal) => {
                // TODO store the raw-ness here so that we do not escape characters
                self.consume(&Token::RawStringLiteral(literal.clone()))?;
                Ok(Expr::StringLiteral(literal))
            }
            Token::ByteStringLiteral(literal) => {
                self.consume(&Token::ByteStringLiteral(literal.clone()))?;
                Ok(Expr::ByteStringLiteral(literal.as_bytes().to_vec()))
            }
            Token::BinaryLiteral(literal) => self.parse_binary_literal(literal),
            Token::OctalLiteral(literal) => self.parse_octal_literal(literal),
            Token::HexLiteral(literal) => self.parse_hex_literal(literal),
            Token::FStringStart => self.parse_f_string(),
            _ => Err(ParserError::UnexpectedToken(self.current_token().clone())),
        }
    }

    fn parse_indented_block(&mut self) -> Result<Ast, ParserError> {
        self.consume_optional_many(&Token::Newline);
        self.consume(&Token::Indent)?;

        let mut statements = Vec::new();
        while self.current_token() != &Token::Dedent {
            statements.push(self.parse_statement()?);
        }
        self.consume(&Token::Dedent)?;
        self.consume_optional_many(&Token::Newline);

        Ok(Ast::new(statements))
    }

    fn parse_import_path(&mut self) -> Result<ImportPath, ParserError> {
        match self.current_token() {
            Token::Dot => {
                self.consume(&Token::Dot)?;
                let mut levels = 0;
                while self.current_token() == &Token::Dot {
                    self.consume(&Token::Dot)?;
                    levels += 1;
                }

                let path = if matches!(self.current_token(), Token::Identifier(_)) {
                    let mut path = vec![self.parse_identifier()?];
                    while self.current_token() == &Token::Dot {
                        self.consume(&Token::Dot)?;
                        path.push(self.parse_identifier()?);
                    }
                    path
                } else {
                    vec![]
                };

                Ok(ImportPath::Relative(levels, path))
            }
            _ => {
                let mut path = vec![self.parse_identifier()?];
                while self.current_token() == &Token::Dot {
                    self.consume(&Token::Dot)?;
                    path.push(self.parse_identifier()?);
                }

                Ok(ImportPath::Absolute(path))
            }
        }
    }

    fn parse_alias(&mut self) -> Result<Option<String>, ParserError> {
        if self.current_token() == &Token::As {
            self.consume(&Token::As)?;
            let alias = self.parse_identifier()?;
            Ok(Some(alias))
        } else {
            Ok(None)
        }
    }

    fn parse_regular_import(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Import)?;

        let mut items = vec![];
        loop {
            let import_path = self.parse_import_path()?;
            let alias = self.parse_alias()?;
            items.push(RegularImport { import_path, alias });

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;
            } else {
                break;
            }
        }

        Ok(StatementKind::RegularImport(items))
    }

    fn parse_selective_import(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::From)?;
        let import_path = self.parse_import_path()?;

        self.consume(&Token::Import)?;
        let stmt = match self.current_token() {
            Token::Asterisk => {
                self.consume(&Token::Asterisk)?;
                StatementKind::SelectiveImport {
                    import_path,
                    items: vec![],
                    wildcard: true,
                }
            }
            _ => {
                self.consume_optional(&Token::LParen);

                let mut items = Vec::new();
                loop {
                    let symbol = self.parse_identifier()?;
                    let alias = self.parse_alias()?;

                    let item = alias.map_or(ImportedItem::Direct(symbol.clone()), |a| {
                        ImportedItem::Alias(Alias {
                            symbol: symbol.clone(),
                            alias_symbol: Some(a),
                        })
                    });
                    items.push(item);

                    if self.end_of_statement() {
                        break;
                    }

                    match self.current_token() {
                        Token::Comma => {
                            self.consume(&Token::Comma)?;
                            continue;
                        }
                        Token::RParen => {
                            self.consume(&Token::RParen)?;
                            break;
                        }
                        _ => {
                            return Err(ParserError::ExpectedToken(
                                Token::Comma,
                                self.current_token().clone(),
                            ));
                        }
                    }
                }

                StatementKind::SelectiveImport {
                    import_path,
                    items,
                    wildcard: false,
                }
            }
        };

        Ok(stmt)
    }

    fn parse_binary_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::BinaryLiteral(literal.clone()))?;

        let result = i64::from_str_radix(&literal[2..], 2).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_octal_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::OctalLiteral(literal.clone()))?;

        let result = i64::from_str_radix(&literal[2..], 8).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_hex_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::HexLiteral(literal.clone()))?;

        let result =
            i64::from_str_radix(&literal[2..], 16).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_type_node(&mut self) -> Result<TypeNode, ParserError> {
        let mut nodes = vec![];

        loop {
            let node = match self.current_token() {
                Token::Identifier(ref identifier) => match identifier.as_str() {
                    "int" => {
                        self.consume(&Token::Identifier("int".into()))?;
                        TypeNode::Basic("int".into())
                    }
                    "str" => {
                        self.consume(&Token::Identifier("str".into()))?;
                        TypeNode::Basic("str".into())
                    }
                    "dict" => {
                        self.consume(&Token::Identifier("dict".into()))?;
                        TypeNode::Basic("dict".into())
                    }
                    "list" => {
                        self.consume(&Token::Identifier("list".into()))?;

                        if self.current_token() == &Token::LBracket {
                            self.consume(&Token::LBracket)?;
                            let parameters = self.parse_type_node()?;
                            self.consume(&Token::RBracket)?;

                            TypeNode::Generic {
                                base_type: "list".into(),
                                parameters: vec![parameters],
                            }
                        } else {
                            TypeNode::Basic("list".into())
                        }
                    }
                    _ => unimplemented!(),
                },
                Token::Ellipsis => {
                    self.consume(&Token::Ellipsis)?;
                    // should this be modeled in a better way?
                    // this is from _collections_abc.py: EllipsisType = type(...)
                    TypeNode::Basic("...".into())
                }
                _ => unimplemented!(),
            };

            nodes.push(node);

            if self.current_token() != &Token::BitwiseOr {
                break;
            }
            self.consume(&Token::BitwiseOr)?;
        }

        if nodes.len() == 1 {
            Ok(nodes[0].clone())
        } else {
            Ok(TypeNode::Union(nodes))
        }
    }

    fn parse_exception_literal(&mut self) -> Result<ExceptionLiteral, ParserError> {
        let symbol = self.parse_identifier()?;
        Ok(symbol.into())
    }

    fn parse_context_manager(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::With)?;
        let expr = self.parse_simple_expr()?;

        let variable = if self.current_token() == &Token::As {
            self.consume(&Token::As)?;
            Some(self.parse_identifier()?)
        } else {
            None
        };
        self.consume(&Token::Colon)?;
        let block = self.parse_indented_block()?;

        Ok(StatementKind::ContextManager {
            expr,
            variable,
            block,
        })
    }

    fn parse_raise(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Raise)?;

        let instance = if matches!(self.current_token(), Token::Identifier(_)) {
            let literal = self.parse_exception_literal()?;

            let args = if self.current_token() == &Token::LParen {
                self.parse_function_call_args()?
            } else {
                CallArgs::default()
            };

            // TODO support exception chaining here and in the interpreter
            if self.current_token() == &Token::From {
                self.consume(&Token::From)?;
                let _from = self.parse_simple_expr()?;
            }
            Some(ExceptionInstance { literal, args })
        } else {
            None
        };

        Ok(StatementKind::Raise(instance))
    }

    fn parse_try_except(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Try)?;
        self.consume(&Token::Colon)?;
        let try_block = self.parse_indented_block()?;

        let mut except_clauses: Vec<ExceptClause> = vec![];
        while self.current_token() == &Token::Except {
            self.consume(&Token::Except)?;
            if self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                let except_block = self.parse_indented_block()?;
                except_clauses.push(ExceptClause {
                    exception_types: vec![],
                    alias: None,
                    block: except_block,
                });
            } else if self.current_token() == &Token::LParen {
                self.consume(&Token::LParen)?;
                let mut literals = vec![];
                while self.current_token() != &Token::RParen {
                    let literal = self.parse_exception_literal()?;
                    literals.push(literal);
                    self.consume_optional(&Token::Comma);
                }

                self.consume(&Token::RParen)?;
                let alias = self.parse_alias()?;
                self.consume(&Token::Colon)?;
                let except_block = self.parse_indented_block()?;
                except_clauses.push(ExceptClause {
                    exception_types: literals,
                    alias,
                    block: except_block,
                });
            } else {
                let literal = self.parse_exception_literal()?;
                let alias = self.parse_alias()?;
                self.consume(&Token::Colon)?;
                let except_block = self.parse_indented_block()?;
                except_clauses.push(ExceptClause {
                    exception_types: vec![literal],
                    alias,
                    block: except_block,
                });
            }
        }

        let else_block = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        let finally_block = if self.current_token() == &Token::Finally {
            self.consume(&Token::Finally)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        if except_clauses.is_empty() && finally_block.is_none() {
            return Err(ParserError::SyntaxError);
        }

        Ok(StatementKind::TryExcept {
            try_block,
            except_clauses,
            else_block,
            finally_block,
        })
    }

    fn parse_if_else(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::If)?;
        let condition = self.parse_simple_expr()?;
        self.consume(&Token::Colon)?;
        let if_part = ConditionalBlock {
            condition,
            block: self.parse_block()?,
        };

        let mut elif_parts: Vec<ConditionalBlock> = vec![];
        while self.current_token() == &Token::Elif {
            self.consume(&Token::Elif)?;
            let condition = self.parse_simple_expr()?;
            self.consume(&Token::Colon)?;
            let elif_parts_part = ConditionalBlock {
                condition,
                block: self.parse_indented_block()?,
            };

            // We must use push because these will be evaluated in order
            elif_parts.push(elif_parts_part);
        }

        let else_part = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        Ok(StatementKind::IfElse {
            if_part,
            elif_parts,
            else_part,
        })
    }

    fn parse_for_in_loop(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::For)?;

        let index_a = self.parse_identifier()?;
        let index = if self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            let index_b = self.parse_identifier()?;
            LoopIndex::Tuple(vec![index_a, index_b])
        } else {
            LoopIndex::Variable(index_a)
        };

        self.consume(&Token::In)?;
        let range = self.parse_simple_expr()?;
        self.consume(&Token::Colon)?;
        let body = self.parse_indented_block()?;

        let else_block = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        Ok(StatementKind::ForInLoop {
            index,
            iterable: range,
            body,
            else_block,
        })
    }

    fn parse_block(&mut self) -> Result<Ast, ParserError> {
        if self.current_token() == &Token::Newline {
            self.parse_indented_block()
        } else {
            // Support single-line functions or classes
            // Examples:
            // def _f() : pass
            // def four(): return 4
            // class Foo: pass
            Ok(ast![self.parse_statement()?])
        }
    }

    /// Parse a parent class looking for one of the following syntaxes:
    /// ```python
    /// class Foo(Bar): pass
    /// class Foo(module.Baz): pass
    /// ```
    ///
    /// We use `parse_simple_expr` here because we do not want to catch any Expr::Tuple, which
    /// would be returned for multiple inheritance if we used `parse_expr`.
    fn parse_parent_class(&mut self) -> Result<Expr, ParserError> {
        let parent = self.parse_simple_expr()?;

        if !matches!(parent, Expr::Variable(_) | Expr::MemberAccess { .. }) {
            Err(ParserError::SyntaxError)
        } else {
            Ok(parent)
        }
    }

    fn parse_class_definition(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Class)?;
        let name = self.parse_identifier()?;

        let mut parents = vec![];
        let mut metaclass = None;

        if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            while self.current_token() != &Token::RParen {
                if self
                    .tokens
                    .peek_ahead_contains(&[Token::Identifier("metaclass".into()), Token::Assign])
                {
                    // Support for metaclasses, i.e. the `__new__` method which constructs a class
                    // (instead of an object like the normal `__new__` method).
                    //
                    // Context: PEP 3115 (https://peps.python.org/pep-3115/)
                    // ```
                    // class ABC(metaclass=ABCMeta):
                    //     pass
                    // ```
                    self.consume(&Token::Identifier("metaclass".into()))?;
                    self.consume(&Token::Assign)?;
                    metaclass = Some(self.parse_identifier()?);
                    break;
                }

                parents.push(self.parse_parent_class()?);

                self.consume_optional(&Token::Comma);
            }
            self.consume(&Token::RParen)?;
        }

        self.consume(&Token::Colon)?;
        let body = self.parse_block()?;

        Ok(StatementKind::ClassDef {
            name,
            parents,
            metaclass,
            body,
        })
    }

    fn parse_function_definition(&mut self) -> Result<StatementKind, ParserError> {
        let mut decorators: Vec<Expr> = vec![];

        while self.current_token() == &Token::AtSign {
            self.consume(&Token::AtSign)?;
            decorators.push(self.parse_simple_expr()?);

            // Each decorator must be ended by 1 and only 1 newline
            self.consume(&Token::Newline)?;
        }

        let is_async = if self.current_token() == &Token::Async {
            self.consume(&Token::Async)?;
            true
        } else {
            false
        };

        self.consume(&Token::Def)?;
        let name = self.parse_identifier()?;
        self.consume(&Token::LParen)?;
        let args = self.parse_function_def_args(Token::RParen)?;
        self.consume(&Token::RParen)?;

        // Support type hints in the return type
        if self.current_token() == &Token::ReturnTypeArrow {
            self.consume(&Token::ReturnTypeArrow)?;
            let _ = self.parse_simple_expr()?;
        }

        self.consume(&Token::Colon)?;
        let body = self.parse_block()?;

        Ok(StatementKind::FunctionDef {
            name,
            args,
            body,
            decorators,
            is_async,
        })
    }

    fn parse_comma_separated_expr(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = vec![];
        loop {
            let expr = self.parse_simple_expr()?;
            exprs.push(expr);

            if self.current_token() != &Token::Comma {
                break;
            }
            self.consume(&Token::Comma)?;
        }

        Ok(exprs)
    }

    fn parse_delete(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Del)?;
        let exprs = self.parse_comma_separated_expr()?;
        Ok(StatementKind::Delete(exprs))
    }

    fn parse_return(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Return)?;
        let exprs = if self.end_of_statement() {
            vec![]
        } else {
            self.parse_comma_separated_expr()?
        };
        Ok(StatementKind::Return(exprs))
    }

    /// Return the full AST. This will consume all the tokens.
    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut stmts = vec![];
        while !self.is_finished() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        Ok(Ast::new(stmts))
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        self.consume_optional_many(&Token::Newline);
        let start_line = self.line_number;
        let stmt = match self.current_token().clone() {
            Token::Del => self.parse_delete(),
            Token::Def => self.parse_function_definition(),
            Token::AtSign => self.parse_function_definition(),
            Token::Async => self.parse_function_definition(),
            Token::Assert => {
                self.consume(&Token::Assert)?;
                let expr = self.parse_simple_expr()?;
                Ok(StatementKind::Assert(expr))
            }
            Token::Class => self.parse_class_definition(),
            Token::Return => self.parse_return(),
            Token::Pass => {
                self.consume(&Token::Pass)?;
                Ok(StatementKind::Pass)
            }
            Token::Break => {
                self.consume(&Token::Break)?;
                Ok(StatementKind::Break)
            }
            Token::Continue => {
                self.consume(&Token::Continue)?;
                Ok(StatementKind::Continue)
            }
            Token::Nonlocal => {
                self.consume(&Token::Nonlocal)?;
                let identifiers = self.parse_identifiers()?;
                Ok(StatementKind::Nonlocal(identifiers))
            }
            Token::Global => {
                self.consume(&Token::Global)?;
                let identifiers = self.parse_identifiers()?;
                Ok(StatementKind::Global(identifiers))
            }
            Token::If => self.parse_if_else(),
            Token::While => {
                self.consume(&Token::While)?;
                let condition = self.parse_simple_expr()?;
                self.consume(&Token::Colon)?;
                let body = self.parse_indented_block()?;
                Ok(StatementKind::WhileLoop { condition, body })
            }
            Token::For => self.parse_for_in_loop(),
            Token::Import => self.parse_regular_import(),
            Token::From => self.parse_selective_import(),
            Token::Try => self.parse_try_except(),
            Token::Raise => self.parse_raise(),
            Token::With => self.parse_context_manager(),
            _ => self.parse_statement_without_starting_keyword(),
        }?;

        self.consume_optional_many(&Token::Newline);
        Ok(Statement::new(start_line, stmt))
    }

    fn parse_statement_without_starting_keyword(&mut self) -> Result<StatementKind, ParserError> {
        let left = self.parse_expr()?;

        if self.current_token() == &Token::Assign {
            self.consume(&Token::Assign)?;
            match left {
                Expr::Tuple(vars) => Ok(StatementKind::UnpackingAssignment {
                    left: vars,
                    right: self.parse_expr()?,
                }),
                _ => {
                    let mut left_items = vec![left];

                    let mut right = self.parse_expr()?;
                    while self.current_token() == &Token::Assign {
                        self.consume(&Token::Assign)?;
                        left_items.push(right);
                        right = self.parse_expr()?;
                    }

                    if left_items.len() > 1 {
                        Ok(StatementKind::MultipleAssignment {
                            left: left_items,
                            right,
                        })
                    } else {
                        Ok(StatementKind::Assignment {
                            left: left_items[0].clone(),
                            right,
                        })
                    }
                }
            }
        } else if self.current_token().is_compound_assign() {
            let operator = match self.current_token() {
                Token::PlusEquals => CompoundOperator::Add,
                Token::MinusEquals => CompoundOperator::Subtract,
                Token::AsteriskEquals => CompoundOperator::Multiply,
                Token::SlashEquals => CompoundOperator::Divide,
                Token::BitwiseAndEquals => CompoundOperator::BitwiseAnd,
                Token::BitwiseOrEquals => CompoundOperator::BitwiseOr,
                Token::BitwiseXorEquals => CompoundOperator::BitwiseXor,
                Token::DoubleSlashEquals => CompoundOperator::IntegerDiv,
                Token::LeftShiftEquals => CompoundOperator::LeftShift,
                Token::RightShiftEquals => CompoundOperator::RightShift,
                Token::ModEquals => CompoundOperator::Mod,
                Token::MatMulEquals => CompoundOperator::MatMul,
                Token::ExpoEquals => CompoundOperator::Expo,
                _ => unreachable!(),
            };
            self.consume_current()?;

            let value = self.parse_simple_expr()?;
            Ok(StatementKind::CompoundAssignment {
                operator,
                target: Box::new(left),
                value: Box::new(value),
            })
        } else {
            Ok(StatementKind::Expression(left))
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Lambda)?;
        let args = self.parse_function_def_args(Token::Colon)?;
        self.consume(&Token::Colon)?;

        let expr = if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            let expr = self.parse_simple_expr()?;
            self.consume(&Token::RParen)?;
            expr
        } else {
            self.parse_simple_expr()?
        };

        Ok(Expr::Lambda {
            args,
            expr: Box::new(expr),
        })
    }

    fn parse_list(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_list".to_string());
        let mut items = Vec::new();

        self.consume(&Token::LBracket)?;
        while self.current_token() != &Token::RBracket {
            let expr = self.parse_simple_expr()?;
            items.push(expr.clone());

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;

                // Handle trailing comma
                if self.current_token() == &Token::RBracket {
                    self.consume(&Token::RBracket)?;
                    return Ok(Expr::List(items));
                }
            } else if self.current_token() == &Token::RBracket {
                self.consume(&Token::RBracket)?;
                return Ok(Expr::List(items));
            }

            if self.current_token() == &Token::For {
                let clauses = self.parse_comprehension_clauses()?;
                self.consume(&Token::RBracket)?;

                return Ok(Expr::ListComprehension {
                    body: Box::new(expr),
                    clauses,
                });
            }
        }

        // You should only get here if this was an empty literal.
        assert_eq!(items.len(), 0);
        self.consume(&Token::RBracket)?;
        Ok(Expr::List(vec![]))
    }

    fn parse_f_string(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::FStringStart)?;

        let mut parts = vec![];
        while self.current_token() != &Token::FStringEnd {
            match self.current_token().clone() {
                Token::StringLiteral(s) => {
                    self.consume(&Token::StringLiteral(s.to_string()))?;
                    parts.push(FStringPart::String(s.to_string()));
                }
                Token::LBrace => {
                    // Start consuming the expression within braces
                    self.consume(&Token::LBrace)?;
                    let expr = self.parse_simple_expr()?;

                    let format = if self.current_token() == &Token::Exclamation {
                        self.consume(&Token::Exclamation)?;
                        if let Token::Identifier(token) = self.current_token().clone() {
                            self.consume(&Token::Identifier(token.to_string()))?;
                            match token.as_str() {
                                "r" => FormatOption::Repr,
                                "s" => FormatOption::Str,
                                "a" => FormatOption::Ascii,
                                _ => {
                                    return Err(ParserError::UnexpectedToken(
                                        self.current_token().clone(),
                                    ));
                                }
                            }
                        } else {
                            return Err(ParserError::UnexpectedToken(self.current_token().clone()));
                        }
                    } else {
                        FormatOption::Str
                    };

                    self.consume(&Token::RBrace)?;
                    parts.push(FStringPart::Expr(ExprFormat {
                        expr: Box::new(expr),
                        format,
                    }));
                }
                _ => {
                    return Err(ParserError::UnexpectedToken(self.current_token().clone()));
                }
            }
        }

        self.consume(&Token::FStringEnd)?;
        Ok(Expr::FString(parts))
    }

    fn parse_set(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_set".to_string());
        let mut pairs = vec![];
        let mut set = HashSet::new();

        self.consume(&Token::LBrace)?;
        while self.current_token() != &Token::RBrace {
            let key = self.parse_simple_expr()?;

            match self.current_token() {
                // A Comma or an RBrace indicates the end of an element, which means this element
                // will not be a key-value (i.e. have a colon). However, only an RBrace indicates
                // an end to the entire literal.
                Token::Comma | Token::RBrace => {
                    match key {
                        Expr::UnaryOperation {
                            op: UnaryOp::DictUnpack,
                            right,
                        } => {
                            pairs.push(DictOperation::Unpack(*right));
                        }
                        _ => {
                            set.insert(key);
                        }
                    };
                    self.consume_optional(&Token::Comma);
                    if self.current_token() == &Token::RBrace {
                        break;
                    }
                }
                Token::For => {
                    let clauses = self.parse_comprehension_clauses()?;
                    self.consume(&Token::RBrace)?;
                    return Ok(Expr::SetComprehension {
                        body: Box::new(key),
                        clauses,
                    });
                }
                Token::Colon => {
                    self.consume(&Token::Colon)?;
                    let value = self.parse_simple_expr()?;

                    match self.current_token() {
                        // A Comma or an RBrace indicates the end of an element, which means this
                        // element _will_ be a key-value (i.e. have a colon). Only an RBrace
                        // indicates an end to the entire literal.
                        Token::Comma | Token::RBrace => {
                            pairs.push(DictOperation::Pair(key, value));
                            self.consume_optional(&Token::Comma);
                            if self.current_token() == &Token::RBrace {
                                break;
                            }
                        }
                        Token::For => {
                            let clauses = self.parse_comprehension_clauses()?;
                            self.consume(&Token::RBrace)?;
                            return Ok(Expr::DictComprehension {
                                clauses,
                                key_body: Box::new(key),
                                value_body: Box::new(value),
                            });
                        }
                        _ => {
                            return Err(ParserError::UnexpectedToken(self.current_token().clone()))
                        }
                    }
                }
                _ => return Err(ParserError::UnexpectedToken(self.current_token().clone())),
            }
        }

        self.consume(&Token::RBrace)?;
        if set.is_empty() {
            Ok(Expr::Dict(pairs))
        } else if pairs.is_empty() {
            Ok(Expr::Set(set))
        } else {
            Err(ParserError::SyntaxError)
        }
    }

    fn parse_function_def_args(&mut self, end_token: Token) -> Result<Params, ParserError> {
        let mut args = Vec::new();
        let mut args_var = None;
        let mut kwargs_var = None;
        while self.current_token() != &end_token {
            // This is to support positional-only parameters.
            // Context: PEP 570 (https://peps.python.org/pep-0570/)
            // TODO test positional-only parameters now that we support args/kwargs
            if self.current_token() == &Token::Slash {
                self.consume(&Token::Slash)?;

                // We will only see a comma if the slash isn't the last "parameter".
                // We test this is the "slash_args" interpreter test. This is also found in
                // types.py in the standard lib.
                if self.current_token() == &Token::Comma {
                    self.consume(&Token::Comma)?;
                } else {
                    break;
                }
            }

            if self.current_token() == &Token::Asterisk {
                self.consume(&Token::Asterisk)?;

                // We will see an asterisk without a trailing identifier for keyword-only
                // parameters. TODO we do not yet enforce this.
                // Context: PEP 3102 (https://peps.python.org/pep-3102/)
                if matches!(self.current_token(), Token::Identifier(_)) {
                    args_var = Some(self.parse_identifier()?);
                }

                // If *args is not at the end of the args (only kwargs can come after), we must
                // allow for a comma. This is similar to how we optionally consume a comma as the
                // last step of each loop iteration.
                self.consume_optional(&Token::Comma);
                continue;
            }

            if self.current_token() == &Token::DoubleAsterisk {
                self.consume(&Token::DoubleAsterisk)?;
                kwargs_var = Some(self.parse_identifier()?);
                break;
            }

            let arg = self.parse_identifier()?;
            let default = if self.current_token() == &Token::Assign {
                self.consume(&Token::Assign)?;
                Some(self.parse_simple_expr()?)
            } else {
                None
            };

            args.push(Param { arg, default });

            // Support for type hints. Will there be reason to store these alongside the params
            // themselves? Perhaps for future toolings like memphis-lsp.
            //
            // Not sure if the check for end_token here is correct or not. This is for lambdas.
            if end_token != Token::Colon && self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                let _type = self.parse_simple_expr()?;
            }

            self.consume_optional(&Token::Comma);
        }

        Ok(Params {
            args,
            args_var,
            kwargs_var,
        })
    }

    fn parse_function_call_args(&mut self) -> Result<CallArgs, ParserError> {
        self.consume(&Token::LParen)?;

        let mut args = Vec::new();
        let mut kwargs = vec![];
        let mut args_var = None;
        while self.current_token() != &Token::RParen {
            if self.current_token() == &Token::Asterisk {
                self.consume(&Token::Asterisk)?;
                args_var = Some(Box::new(self.parse_simple_expr()?));

                // If *args is not at the end of the args (only kwargs can come after), we must
                // allow for a comma. This is similar to how we optionally consume a comma as the
                // last step of each loop iteration.
                self.consume_optional(&Token::Comma);
                continue;
            }

            // This is to support the formats
            // - foo(**{'a': 2, 'b': 1})
            // - foo(**args)
            if self.current_token() == &Token::DoubleAsterisk {
                self.consume(&Token::DoubleAsterisk)?;
                let kwargs_expr = self.parse_simple_expr()?;
                match kwargs_expr {
                    Expr::Dict(dict_ops) => {
                        for op in dict_ops {
                            match op {
                                DictOperation::Pair(key, value) => {
                                    let key_name =
                                        key.as_string().ok_or(ParserError::SyntaxError)?;
                                    kwargs.push(KwargsOperation::Pair(key_name, value));
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                    Expr::Variable(_) | Expr::MemberAccess { .. } => {
                        kwargs.push(KwargsOperation::Unpacking(kwargs_expr));
                    }
                    _ => return Err(ParserError::SyntaxError),
                };
                self.consume_optional(&Token::Comma);
                continue;
            }

            match self.parse_function_call_arg()? {
                // This is to support the format foo(a=2, b=1)
                CallArg::Keyword { arg, expr } => {
                    kwargs.push(KwargsOperation::Pair(arg, expr));
                }
                CallArg::Positional(expr) => {
                    args.push(expr);
                }
            }

            self.consume_optional(&Token::Comma);
        }

        self.consume(&Token::RParen)?;

        Ok(CallArgs {
            args,
            kwargs,
            args_var,
        })
    }

    /// An argument in a function call can be either variable `a` or contain an equals such as
    /// `a = 4`. We originally (and ignorantly) called `parse_statement` but that contains too many
    /// other cases to be safely used inside function call parsing.
    fn parse_function_call_arg(&mut self) -> Result<CallArg, ParserError> {
        let expr = self.parse_simple_expr()?;
        match self.current_token() {
            Token::Assign => {
                self.consume(&Token::Assign)?;
                let arg = expr.as_variable().ok_or(ParserError::SyntaxError)?;
                Ok(CallArg::Keyword {
                    arg,
                    expr: self.parse_simple_expr()?,
                })
            }
            Token::For => Ok(CallArg::Positional(
                self.parse_generator_comprehension(&expr)?,
            )),
            _ => Ok(CallArg::Positional(expr)),
        }
    }

    fn parse_generator_comprehension(&mut self, body: &Expr) -> Result<Expr, ParserError> {
        let clauses = self.parse_comprehension_clauses()?;
        Ok(Expr::GeneratorComprehension {
            body: Box::new(body.clone()),
            clauses,
        })
    }

    fn parse_comprehension_clauses(&mut self) -> Result<Vec<ForClause>, ParserError> {
        let mut clauses = vec![];
        while self.current_token() == &Token::For {
            clauses.push(self.parse_comprehension_clause()?);
        }
        Ok(clauses)
    }

    fn parse_comprehension_clause(&mut self) -> Result<ForClause, ParserError> {
        self.consume(&Token::For)?;

        // The parentheses are optional here, but if one is present, both must be present
        let mut need_rparen = false;
        if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            need_rparen = true;
        }

        let mut indices = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            indices.push(self.parse_identifier()?);
        }

        if need_rparen {
            self.consume(&Token::RParen)?;
        }

        self.consume(&Token::In)?;

        // We do not use `parse_expr` here because it can think that an expression of the
        // form `a if True` is the start of a ternary operation and expect an `else` token
        // next. By calling `parse_binary_expr`, we enter the parse tree below where
        // ternary operations are handled.
        let iterable = self.parse_binary_expr()?;

        let condition = if self.current_token() == &Token::If {
            self.consume(&Token::If)?;
            Some(self.parse_simple_expr()?)
        } else {
            None
        };

        Ok(ForClause {
            indices,
            iterable,
            condition,
        })
    }

    /// Single elements without a comma will be returned as is, everything else will be wrapped in
    /// `Expr::Tuple`.
    ///
    /// For example:
    ///
    /// (4) => int(4)
    /// (4,) => Expr::Tuple(vec!\[int(4)\])
    ///
    fn parse_tuple(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_tuple".to_string());
        self.consume(&Token::LParen)?;

        let mut args = Vec::new();
        let mut is_single_element = true;
        while self.current_token() != &Token::RParen {
            let expr = self.parse_simple_expr()?;
            args.push(expr.clone());

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;
                is_single_element = false;
            }

            if self.current_token() == &Token::For {
                // If you saw a For token, we must be in list comprehension.
                assert_eq!(args.len(), 1);
                let gen_comp = self.parse_generator_comprehension(&expr)?;

                self.consume(&Token::RParen)?;
                return Ok(gen_comp);
            }
        }

        self.consume(&Token::RParen)?;

        if args.len() == 1 && is_single_element {
            Ok(args.into_iter().next().unwrap())
        } else {
            Ok(Expr::Tuple(args))
        }
    }

    fn parse_identifiers(&mut self) -> Result<Vec<Variable>, ParserError> {
        let mut items = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            items.push(self.parse_identifier()?);
        }
        Ok(items)
    }

    /// Parse a `Token::Identifier` without any semantic analysis.
    fn parse_identifier(&mut self) -> Result<String, ParserError> {
        match self.current_token().clone() {
            Token::Identifier(name) => {
                let name_clone = name.clone();
                self.consume(&Token::Identifier(name_clone.clone()))?;
                Ok(name_clone)
            }
            _ => Err(ParserError::UnexpectedToken(self.current_token().clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_utils::*;
    use super::{types::KwargsOperation, *};
    use crate::init::MemphisContext;

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    fn parse_all(input: &str) -> Ast {
        let mut context = init(input);
        context
            .parse_all_statements()
            .expect("Failed to parse all statements!")
    }

    macro_rules! expect_error {
        ($input:expr, $pattern:ident) => {
            match init($input).parse_oneshot::<$pattern>() {
                Ok(_) => panic!("Expected a ParserError!"),
                Err(e) => e,
            }
        };
    }

    macro_rules! parse {
        ($input:expr, $pattern:ident) => {
            match init($input).parse_oneshot::<$pattern>() {
                Err(e) => panic!("Parser error: {:?}", e),
                Ok(ast) => ast,
            }
        };
    }

    macro_rules! assert_ast_eq {
        ($input:expr, $expected:expr) => {
            let ast = parse!($input, Statement);
            assert_stmt_eq!(ast, $expected);
        };
        ($input:expr, $expected:expr, $pattern:ident) => {
            let ast = parse!($input, $pattern);
            assert_eq!(ast, $expected);
        };
    }

    macro_rules! assert_stmt_eq {
        ($actual:expr, $expected:expr) => {
            assert_stmt_eq(&$actual, &$expected)
        };
    }

    fn assert_stmt_eq(actual: &Statement, expected: &Statement) {
        match (&actual.kind, &expected.kind) {
            // Function definitions (compare nested body statements)
            (
                StatementKind::FunctionDef {
                    name: actual_name,
                    args: actual_args,
                    body: actual_body,
                    decorators: actual_decorators,
                    is_async: actual_is_async,
                },
                StatementKind::FunctionDef {
                    name: expected_name,
                    args: expected_args,
                    body: expected_body,
                    decorators: expected_decorators,
                    is_async: expected_is_async,
                },
            ) => {
                assert_eq!(
                    actual_body.len(),
                    expected_body.len(),
                    "Function body length mismatch"
                );
                for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                    assert_stmt_eq(a, e);
                }

                assert_eq!(actual_name, expected_name, "Function name mismatch");
                assert_eq!(actual_args, expected_args, "Function args mismatch");
                assert_eq!(
                    actual_decorators, expected_decorators,
                    "Function decorators mismatch"
                );
                assert_eq!(
                    actual_is_async, expected_is_async,
                    "Function is_async mismatch"
                );
            }

            (
                StatementKind::ClassDef {
                    name: actual_name,
                    parents: actual_parents,
                    metaclass: actual_metaclass,
                    body: actual_body,
                },
                StatementKind::ClassDef {
                    name: expected_name,
                    parents: expected_parents,
                    metaclass: expected_metaclass,
                    body: expected_body,
                },
            ) => {
                assert_eq!(actual_name, expected_name, "Class name mismatch");
                assert_eq!(actual_parents, expected_parents, "Class parents mismatch");
                assert_eq!(
                    actual_metaclass, expected_metaclass,
                    "Class metaclass mismatch"
                );

                assert_eq!(
                    actual_body.len(),
                    expected_body.len(),
                    "Class body length mismatch"
                );
                for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                    assert_stmt_eq(a, e);
                }
            }

            (
                StatementKind::ContextManager {
                    expr: actual_expr,
                    variable: actual_variable,
                    block: actual_block,
                },
                StatementKind::ContextManager {
                    expr: expected_expr,
                    variable: expected_variable,
                    block: expected_block,
                },
            ) => {
                assert_eq!(actual_expr, expected_expr, "Context manager expr mismatch");
                assert_eq!(
                    actual_variable, expected_variable,
                    "Context manager variable mismatch"
                );

                assert_eq!(
                    actual_block.len(),
                    expected_block.len(),
                    "Context manager block length mismatch"
                );
                for (a, e) in actual_block.iter().zip(expected_block.iter()) {
                    assert_stmt_eq(a, e);
                }
            }

            (
                StatementKind::TryExcept {
                    try_block: actual_try_block,
                    except_clauses: actual_except_clauses,
                    else_block: actual_else_block,
                    finally_block: actual_finally_block,
                },
                StatementKind::TryExcept {
                    try_block: expected_try_block,
                    except_clauses: expected_except_clauses,
                    else_block: expected_else_block,
                    finally_block: expected_finally_block,
                },
            ) => {
                assert_eq!(
                    actual_try_block.len(),
                    expected_try_block.len(),
                    "Try block length mismatch"
                );
                for (a, e) in actual_try_block.iter().zip(expected_try_block.iter()) {
                    assert_stmt_eq(a, e);
                }

                assert_eq!(
                    actual_except_clauses.len(),
                    expected_except_clauses.len(),
                    "Except clauses length mismatch"
                );
                for (actual_except_clause, expected_except_clause) in actual_except_clauses
                    .iter()
                    .zip(expected_except_clauses.iter())
                {
                    assert_eq!(
                        actual_except_clause.exception_types,
                        expected_except_clause.exception_types,
                        "Except clause types mismatch"
                    );
                    assert_eq!(
                        actual_except_clause.alias, expected_except_clause.alias,
                        "Except clause alias mismatch"
                    );
                    assert_eq!(
                        actual_except_clause.block.len(),
                        expected_except_clause.block.len(),
                        "Except_clause.block length mismatch"
                    );
                    for (a, e) in actual_except_clause
                        .block
                        .iter()
                        .zip(expected_except_clause.block.iter())
                    {
                        assert_stmt_eq(a, e);
                    }
                }

                match (actual_else_block, expected_else_block) {
                    (Some(actual), Some(expected)) => {
                        assert_eq!(actual.len(), expected.len(), "Else block length mismatch");
                        for (a, e) in actual.iter().zip(expected.iter()) {
                            assert_stmt_eq(a, e);
                        }
                    }
                    (None, None) => {} // Both bodies are None—nothing to compare
                    (None, Some(_)) | (Some(_), None) => {
                        panic!("Else block mismatch: one body is None while the other is Some");
                    }
                }

                match (actual_finally_block, expected_finally_block) {
                    (Some(actual), Some(expected)) => {
                        assert_eq!(
                            actual.len(),
                            expected.len(),
                            "Finally block length mismatch"
                        );
                        for (a, e) in actual.iter().zip(expected.iter()) {
                            assert_stmt_eq(a, e);
                        }
                    }
                    (None, None) => {} // Both bodies are None—nothing to compare
                    (None, Some(_)) | (Some(_), None) => {
                        panic!("Finally block mismatch: one body is None while the other is Some");
                    }
                }
            }

            (
                StatementKind::IfElse {
                    if_part: actual_if_part,
                    elif_parts: actual_elif_parts,
                    else_part: actual_else_part,
                },
                StatementKind::IfElse {
                    if_part: expected_if_part,
                    elif_parts: expected_elif_parts,
                    else_part: expected_else_part,
                },
            ) => {
                assert_eq!(
                    actual_if_part.condition, expected_if_part.condition,
                    "If/else condition mismatch"
                );
                assert_eq!(
                    actual_if_part.block.len(),
                    expected_if_part.block.len(),
                    "If/else if_part.block length mismatch"
                );
                for (a, e) in actual_if_part
                    .block
                    .iter()
                    .zip(expected_if_part.block.iter())
                {
                    assert_stmt_eq(a, e);
                }

                assert_eq!(
                    actual_elif_parts.len(),
                    expected_elif_parts.len(),
                    "Elif parts length mismatch"
                );
                for (actual_elif_part, expected_elif_part) in
                    actual_elif_parts.iter().zip(expected_elif_parts.iter())
                {
                    assert_eq!(
                        actual_elif_part.condition, expected_elif_part.condition,
                        "If/else condition mismatch"
                    );
                    assert_eq!(
                        actual_elif_part.block.len(),
                        expected_elif_part.block.len(),
                        "If/else elif_part.block length mismatch"
                    );
                    for (a, e) in actual_elif_part
                        .block
                        .iter()
                        .zip(expected_elif_part.block.iter())
                    {
                        assert_stmt_eq(a, e);
                    }
                }

                match (actual_else_part, expected_else_part) {
                    (Some(actual), Some(expected)) => {
                        assert_eq!(actual.len(), expected.len(), "Else part length mismatch");
                        for (a, e) in actual.iter().zip(expected.iter()) {
                            assert_stmt_eq(a, e);
                        }
                    }
                    (None, None) => {} // Both bodies are None—nothing to compare
                    (None, Some(_)) | (Some(_), None) => {
                        panic!("Else part mismatch: one body is None while the other is Some");
                    }
                }
            }

            (
                StatementKind::WhileLoop {
                    condition: actual_cond,
                    body: actual_body,
                },
                StatementKind::WhileLoop {
                    condition: expected_cond,
                    body: expected_body,
                },
            ) => {
                assert_eq!(actual_cond, expected_cond, "Loop condition mismatch");

                assert_eq!(
                    actual_body.len(),
                    expected_body.len(),
                    "Loop body length mismatch"
                );
                for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                    assert_stmt_eq(a, e);
                }
            }

            (
                StatementKind::ForInLoop {
                    index: actual_index,
                    iterable: actual_iterable,
                    body: actual_body,
                    else_block: actual_else_block,
                },
                StatementKind::ForInLoop {
                    index: expected_index,
                    iterable: expected_iterable,
                    body: expected_body,
                    else_block: expected_else_block,
                },
            ) => {
                assert_eq!(actual_index, expected_index, "Loop index mismatch");
                assert_eq!(actual_iterable, expected_iterable, "Loop iterable mismatch");

                assert_eq!(
                    actual_body.len(),
                    expected_body.len(),
                    "Loop body length mismatch"
                );
                for (a, e) in actual_body.iter().zip(expected_body.iter()) {
                    assert_stmt_eq(a, e);
                }

                match (actual_else_block, expected_else_block) {
                    (Some(actual), Some(expected)) => {
                        assert_eq!(actual.len(), expected.len(), "Loop body length mismatch");
                        for (a, e) in actual.iter().zip(expected.iter()) {
                            assert_stmt_eq(a, e);
                        }
                    }
                    (None, None) => {} // Both bodies are None—nothing to compare
                    (None, Some(_)) | (Some(_), None) => {
                        panic!("Loop body mismatch: one body is None while the other is Some");
                    }
                }
            }

            // Default case (compare only kinds, ignoring start_line)
            _ => {
                assert_eq!(actual.kind, expected.kind, "AST nodes do not match");
            }
        }
    }

    #[test]
    fn expression() {
        let input = "2 + 3 * (4 - 1)";
        let expected_ast = bin_op!(
            int!(2),
            Add,
            bin_op!(int!(3), Mul, bin_op!(int!(4), Sub, int!(1)))
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 // 3";
        let expected_ast = bin_op!(int!(2), IntegerDiv, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn string_literal() {
        let input = "\"Hello\"";
        let expected_ast = str!("Hello");

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "\"\".join([])";
        let expected_ast = method_call!(str!(""), "join", call_args![list![]]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn variable_assignment() {
        let input = "a = 2";
        let expected_ast = stmt_assign!(var!("a"), int!(2));

        assert_ast_eq!(input, expected_ast);

        let input = "b = a + 3";
        let expected_ast = stmt_assign!(var!("b"), bin_op!(var!("a"), Add, int!(3)));

        assert_ast_eq!(input, expected_ast);

        let input = "a, b = (1, 2)";
        let expected_ast = stmt(StatementKind::UnpackingAssignment {
            left: vec![var!("a"), var!("b")],
            right: tuple![int!(1), int!(2)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn function_call() {
        let input = "print(\"Hello, World!\")";
        let expected_ast = func_call!("print", call_args![str!("Hello, World!")]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a(*self.args, **self.kwargs)";
        let expected_ast = func_call!(
            "a",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(member_access!(
                    var!("self"),
                    "kwargs"
                ))],
                args_var: Some(Box::new(member_access!(var!("self"), "args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn function_definition() {
        let input = "
def add(x, y):
    return x + y
";
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "add".to_string(),
            args: params![param!("x"), param!("y")],
            body: ast![stmt_return![bin_op!(var!("x"), Add, var!("y"))]],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "def _f(): pass";
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "_f".to_string(),
            args: params![],
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "lambda: 4";
        let expected_ast = lambda!(params![], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda index: 4";
        let expected_ast = lambda!(params![param!("index")], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda index, val: 4";
        let expected_ast = lambda!(params![param!("index"), param!("val")], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda: (yield)";
        let expected_ast = lambda!(params![], Expr::Yield(None));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "(lambda: (yield))()";
        let expected_ast = func_call!(
            "<anonymous_from_callee>",
            call_args![],
            lambda!(params![], Expr::Yield(None))
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
def __init__(
    self, *, indent=None,
):
    pass
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "__init__".into(),
            args: params![param!("self"), param!("indent", Expr::None)],
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "return a, b";
        let expected_ast = stmt_return![var!("a"), var!("b")];

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn boolean_expressions() {
        let input = "x and y";
        let expected_ast = logic_op!(var!("x"), And, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x or y";
        let expected_ast = logic_op!(var!("x"), Or, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x or not y";
        let expected_ast = logic_op!(var!("x"), Or, unary_op!(Not, var!("y")));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "not (x or y)";
        let expected_ast = unary_op!(Not, logic_op!(var!("x"), Or, var!("y")));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
if (a
    or b):
    pass
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: logic_op!(var!("a"), Or, var!("b")),
                block: ast![stmt(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn comparison_operators() {
        let input = "x == y";
        let expected_ast = bin_op!(var!("x"), Equals, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x != y";
        let expected_ast = bin_op!(var!("x"), NotEquals, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x < y";
        let expected_ast = bin_op!(var!("x"), LessThan, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x > y";
        let expected_ast = bin_op!(var!("x"), GreaterThan, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x >= y";
        let expected_ast = bin_op!(var!("x"), GreaterThanOrEqual, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x <= y";
        let expected_ast = bin_op!(var!("x"), LessThanOrEqual, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x in y";
        let expected_ast = bin_op!(var!("x"), In, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x not in y";
        let expected_ast = bin_op!(var!("x"), NotIn, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x is None";
        let expected_ast = bin_op!(var!("x"), Is, Expr::None);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x is not None";
        let expected_ast = bin_op!(var!("x"), IsNot, Expr::None);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn boolean_operators() {
        let input = "x = True";
        let expected_ast = stmt_assign!(var!("x"), bool!(true));

        assert_ast_eq!(input, expected_ast);

        let input = "True or False";
        let expected_ast = logic_op!(bool!(true), Or, bool!(false));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x = None";
        let expected_ast = stmt_assign!(var!("x"), Expr::None);

        assert_ast_eq!(input, expected_ast);

        let input = "return None";
        let expected_ast = stmt_return![Expr::None];

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn if_else() {
        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Medium")
else:
    print("Less")
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bin_op!(var!("x"), GreaterThan, int!(0)),
                block: ast![stmt(StatementKind::Expression(func_call!(
                    "print",
                    call_args![str!("Greater")]
                )))],
            },
            elif_parts: vec![ConditionalBlock {
                condition: bin_op!(var!("x"), GreaterThan, int!(-10)),
                block: ast![stmt(StatementKind::Expression(func_call!(
                    "print",
                    call_args![str!("Medium")]
                )))],
            }],
            else_part: Some(ast![stmt(StatementKind::Expression(func_call!(
                "print",
                call_args![str!("Less")]
            )))]),
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Medium")
elif x > -20:
    print("Less")
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bin_op!(var!("x"), GreaterThan, int!(0)),
                block: ast![stmt(StatementKind::Expression(func_call!(
                    "print",
                    call_args![str!("Greater")]
                )))],
            },
            elif_parts: vec![
                ConditionalBlock {
                    condition: bin_op!(var!("x"), GreaterThan, int!(-10)),
                    block: ast![stmt(StatementKind::Expression(func_call!(
                        "print",
                        call_args![str!("Medium")]
                    )))],
                },
                ConditionalBlock {
                    condition: bin_op!(var!("x"), GreaterThan, int!(-20)),
                    block: ast![stmt(StatementKind::Expression(func_call!(
                        "print",
                        call_args![str!("Less")]
                    )))],
                },
            ],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
if x > 0:
    print("Greater")
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bin_op!(var!("x"), GreaterThan, int!(0)),
                block: ast![stmt(StatementKind::Expression(func_call!(
                    "print",
                    call_args![str!("Greater")]
                )))],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
if True: return False
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bool!(true),
                block: ast![stmt_return![bool!(false)]],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
if (a == 1
        and b
        and c):
    pass
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: logic_op!(
                    logic_op!(bin_op!(var!("a"), Equals, int!(1)), And, var!("b")),
                    And,
                    var!("c")
                ),
                block: ast![stmt(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn while_loop() {
        let input = "
while True:
    print(\"busy loop\")
";
        let expected_ast = stmt(StatementKind::WhileLoop {
            condition: bool!(true),
            body: ast![stmt(StatementKind::Expression(func_call!(
                "print",
                call_args![str!("busy loop")]
            )))],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn class_definition() {
        let input = r#"
class Foo:
    def __init__(self):
        self.x = 0

    def bar(self):
        print(self.x)
"#;
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![],
            metaclass: None,
            body: ast![
                stmt(StatementKind::FunctionDef {
                    name: "__init__".to_string(),
                    args: params![param!("self")],
                    body: ast![stmt_assign!(member_access!(var!("self"), "x"), int!(0))],
                    decorators: vec![],
                    is_async: false,
                }),
                stmt(StatementKind::FunctionDef {
                    name: "bar".to_string(),
                    args: params![param!("self")],
                    body: ast![stmt(StatementKind::Expression(func_call!(
                        "print",
                        call_args![member_access!(var!("self"), "x")]
                    )))],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
        });

        assert_ast_eq!(input, expected_ast);

        let input = "class Foo(Bar, Baz): pass";
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![var!("Bar"), var!("Baz")],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = "class Foo(module.Bar): pass";
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![member_access!(var!("module"), "Bar")],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn class_instantiation() {
        let input = "foo = Foo()";
        let expected_ast = stmt_assign!(var!("foo"), func_call!("Foo", call_args![]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn method_invocation() {
        let input = "foo.bar()";
        let expected_ast = method_call!(var!("foo"), "bar");

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let expected_ast = stmt(StatementKind::RegularImport(vec![RegularImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            alias: None,
        }]));

        assert_ast_eq!(input, expected_ast);

        let input = r#"
def foo():
    import other, second as third
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "foo".to_string(),
            args: params![],
            body: ast![stmt(StatementKind::RegularImport(vec![
                RegularImport {
                    import_path: ImportPath::Absolute(vec!["other".into()]),
                    alias: None,
                },
                RegularImport {
                    import_path: ImportPath::Absolute(vec!["second".into()]),
                    alias: Some("third".into()),
                },
            ]))],
            decorators: vec![],
            is_async: false,
        });

        // We test this inside a function so that it will attempt to parse more than one statement,
        // which is what originally caught the bug related to parsing the comma and beyond.
        assert_ast_eq!(input, expected_ast);

        let input = r#"
import other as b
pass
"#;
        let expected_ast = stmt(StatementKind::RegularImport(vec![RegularImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            alias: Some("b".into()),
        }]));

        // Before we handling Token::As processing, this test would fail, but only once it began
        // parsing the next statement. We needed to parse two statements here to produce the
        // failing test.
        let asts = parse_all(input);
        assert_eq!(asts.len(), 2);
        assert_stmt_eq!(asts.get(0).unwrap(), expected_ast);
        assert_stmt_eq!(asts.get(1).unwrap(), stmt(StatementKind::Pass));

        let input = "mypackage.myothermodule.add('1', '1')";
        let expected_ast = method_call!(
            member_access!(var!("mypackage"), "myothermodule"),
            "add",
            call_args![str!("1"), str!("1")]
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "cls._abc_registry.add(subclass)";
        let expected_ast = method_call!(
            member_access!(var!("cls"), "_abc_registry"),
            "add",
            call_args![var!("subclass")]
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn selective_import() {
        let input = "from other import something";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![ImportedItem::Direct("something".to_string())],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from other import something, something_else";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Direct("something_else".to_string()),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from other import *";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![],
            wildcard: true,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from other import something, something_else as imported_name";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Alias(Alias {
                    symbol: "something_else".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from other.module import something, something_else as imported_name";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into(), "module".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Alias(Alias {
                    symbol: "something_else".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from . import something";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(0, vec![]),
            items: vec![ImportedItem::Direct("something".to_string())],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from .other.module import something, something_else as imported_name";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(0, vec!["other".into(), "module".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Alias(Alias {
                    symbol: "something_else".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "from ..other.module import something, something_else as imported_name";
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(1, vec!["other".into(), "module".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Alias(Alias {
                    symbol: "something_else".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (something,
                            something_else as imported_name)
"#;
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(1, vec!["other".into(), "module".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Alias(Alias {
                    symbol: "something_else".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (something as imported_name,
                            something_else)
"#;
        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(1, vec!["other".into(), "module".into()]),
            items: vec![
                ImportedItem::Alias(Alias {
                    symbol: "something".to_string(),
                    alias_symbol: Some("imported_name".to_string()),
                }),
                ImportedItem::Direct("something_else".to_string()),
            ],
            wildcard: false,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn floating_point() {
        let input = "a = 3.14";
        let expected_ast = stmt_assign!(var!("a"), Expr::FloatingPoint(3.14));

        assert_ast_eq!(input, expected_ast);

        let input = "b = a + 2.5e-3";
        let expected_ast = stmt_assign!(
            var!("b"),
            bin_op!(var!("a"), Add, Expr::FloatingPoint(2.5e-3))
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn negative_numbers() {
        let input = "-3.14";
        let expected_ast = Expr::FloatingPoint(-3.14);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-3";
        let expected_ast = int!(-3);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 - 3";
        let expected_ast = bin_op!(int!(2), Sub, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-2e-3";
        let expected_ast = Expr::FloatingPoint(-2e-3);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 + -3";
        let expected_ast = bin_op!(int!(2), Add, int!(-3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-(3)";
        let expected_ast = unary_op!(Minus, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "+(3)";
        let expected_ast = unary_op!(Plus, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-(2 + 3)";
        let expected_ast = unary_op!(Minus, bin_op!(int!(2), Add, int!(3)));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn lists() {
        let input = "[1,2,3]";
        let expected_ast = list![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[1, 2, 3]";
        let expected_ast = list![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
a = [1,
    2,
    3
]"#;
        let expected_ast = stmt_assign!(var!("a"), list![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "a = [1, 2, 3]";
        let expected_ast = stmt_assign!(var!("a"), list![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "list([1, 2, 3])";
        let expected_ast = func_call!("list", call_args![list![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn sets() {
        let input = "{1,2,3}";
        let expected_ast = set![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "{1, 2, 3}";
        let expected_ast = set![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a = {1, 2, 3}";
        let expected_ast = stmt_assign!(var!("a"), set![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "set({1, 2, 3})";
        let expected_ast = func_call!("set", call_args![set![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
{
    1,
    2,
    3
}"#;
        let expected_ast = stmt(StatementKind::Expression(set![int!(1), int!(2), int!(3),]));

        assert_ast_eq!(input, expected_ast);

        let input = r#"
{
    1,
    2,
    3,
}"#;
        let expected_ast = stmt(StatementKind::Expression(set![int!(1), int!(2), int!(3),]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn tuples() {
        let input = "(1,2,3)";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "(1, 2, 3)";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "1, 2, 3";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "1,";
        let expected_ast = tuple![int!(1)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a = (1, 2, 3)";
        let expected_ast = stmt_assign!(var!("a"), tuple![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "a = 1, 2, 3";
        let expected_ast = stmt_assign!(var!("a"), tuple![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "tuple((1, 2, 3))";
        let expected_ast = func_call!("tuple", call_args![tuple![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
tuple((1,
       2,
       3))
"#;
        let expected_ast = stmt(StatementKind::Expression(func_call!(
            "tuple",
            call_args![tuple![int!(1), int!(2), int!(3)]]
        )));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn index_access() {
        let input = "a[0]";
        let expected_ast = Expr::IndexAccess {
            object: Box::new(var!("a")),
            index: Box::new(int!(0)),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[0,1][1]";
        let expected_ast = Expr::IndexAccess {
            object: Box::new(list![int!(0), int!(1)]),
            index: Box::new(int!(1)),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[1] = 0";
        let expected_ast = stmt_assign!(
            Expr::IndexAccess {
                object: Box::new(var!("a")),
                index: Box::new(int!(1)),
            },
            int!(0)
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
for i in a:
    print(i)
"#;
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var!("a"),
            body: ast![stmt(StatementKind::Expression(func_call!(
                "print",
                call_args![var!("i")]
            )))],
            else_block: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
for k, v in a.items():
    print(v)
"#;
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Tuple(vec!["k".into(), "v".into()]),
            iterable: method_call!(var!("a"), "items"),
            body: ast![stmt(StatementKind::Expression(func_call!(
                "print",
                call_args![var!("v")]
            )))],
            else_block: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn list_comprehension() {
        let input = "[ i * 2 for i in a ]";
        let expected_ast = Expr::ListComprehension {
            body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
            clauses: vec![ForClause {
                indices: vec!["i".to_string()],
                iterable: var!("a"),
                condition: None,
            }],
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[i*2 for i in a if True]";
        let expected_ast = Expr::ListComprehension {
            body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
            clauses: vec![ForClause {
                indices: vec!["i".to_string()],
                iterable: var!("a"),
                condition: Some(bool!(true)),
            }],
        };

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn generators() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n - 1
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "countdown".to_string(),
            args: params![param!("n")],
            body: ast![stmt(StatementKind::WhileLoop {
                condition: bin_op!(var!("n"), GreaterThan, int!(0)),
                body: ast![
                    stmt(StatementKind::Expression(Expr::Yield(Some(Box::new(
                        var!("n")
                    ))))),
                    stmt_assign!(var!("n"), bin_op!(var!("n"), Sub, int!(1))),
                ],
            })],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "yield from a";
        let expected_ast = Expr::YieldFrom(Box::new(var!("a")));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn inheritance() {
        let input = r#"
class Foo(Parent):
    def __init__(self):
        self.x = 0
"#;
        let ast = parse!(input, Statement);
        let expected_parent = vec![var!("Parent")];

        let StatementKind::ClassDef { parents, .. } = ast.kind else {
            panic!("Expected a class def!")
        };
        assert_eq!(parents, expected_parent);

        let input = r#"
class Foo(metaclass=Parent):
    pass
"#;
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![],
            metaclass: Some("Parent".to_string()),
            body: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
class Foo(Bar, metaclass=Parent):
    pass
"#;
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![var!("Bar")],
            metaclass: Some("Parent".to_string()),
            body: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
class InterfaceMeta(type):
    pass
"#;
        let expected_ast = stmt(StatementKind::ClassDef {
            name: "InterfaceMeta".to_string(),
            parents: vec![var!("type")],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn dictionaries() {
        let input = r#"a = { "b": 4, 'c': 5 }"#;
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::Dict(vec![
                DictOperation::Pair(str!("b"), int!(4)),
                DictOperation::Pair(str!("c"), int!(5)),
            ])
        );

        assert_ast_eq!(input, expected_ast);

        let input = r#"
namespace = {
    '__name__': 4,
}
"#;
        let expected_ast = stmt_assign!(
            var!("namespace"),
            Expr::Dict(vec![DictOperation::Pair(str!("__name__"), int!(4))])
        );

        assert_ast_eq!(input, expected_ast);

        let input = r#"{ **first, **second }"#;
        let expected_ast = Expr::Dict(vec![
            DictOperation::Unpack(var!("first")),
            DictOperation::Unpack(var!("second")),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"{ **first, **second, }"#;
        let expected_ast = Expr::Dict(vec![
            DictOperation::Unpack(var!("first")),
            DictOperation::Unpack(var!("second")),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "{ 2, **second }";
        let e = expect_error!(input, Expr);
        assert_eq!(e, ParserError::SyntaxError);

        let input = "{ 2, **second, }";
        let e = expect_error!(input, Expr);
        assert_eq!(e, ParserError::SyntaxError);

        let input = r#"{ key: val * 2 for key, val in d }"#;
        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec!["key".to_string(), "val".to_string()],
                iterable: var!("d"),
                condition: None,
            }],
            key_body: Box::new(var!("key")),
            value_body: Box::new(bin_op!(var!("val"), Mul, int!(2))),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"{ key: val * 2 for (key, val) in d }"#;
        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec!["key".to_string(), "val".to_string()],
                iterable: var!("d"),
                condition: None,
            }],
            key_body: Box::new(var!("key")),
            value_body: Box::new(bin_op!(var!("val"), Mul, int!(2))),
        };

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn async_await() {
        let input = r#"
async def main():
    task_1 = asyncio.create_task(task1())
    return await task_1
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "main".to_string(),
            args: params![],
            body: ast![
                stmt_assign!(
                    var!("task_1"),
                    method_call!(
                        var!("asyncio"),
                        "create_task",
                        call_args![func_call!("task1")]
                    )
                ),
                stmt_return![Expr::Await(Box::new(var!("task_1")))],
            ],
            decorators: vec![],
            is_async: true,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let expected_ast = stmt(StatementKind::Assert(bool!(true)));

        assert_ast_eq!(input, expected_ast);
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
        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(bin_op!(
                int!(4),
                Div,
                int!(0)
            )))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![],
                alias: None,
                block: ast![stmt_assign!(var!("a"), int!(2))],
            }],
            else_block: None,
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except ZeroDivisionError as e:
    a = 2
finally:
    a = 3
"#;
        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(bin_op!(
                int!(4),
                Div,
                int!(0)
            )))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![ExceptionLiteral::ZeroDivisionError],
                alias: Some("e".into()),
                block: ast![stmt_assign!(var!("a"), int!(2))],
            }],
            else_block: None,
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except (ZeroDivisionError, IOError) as e:
    a = 2
"#;
        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(bin_op!(
                int!(4),
                Div,
                int!(0)
            )))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![
                    ExceptionLiteral::ZeroDivisionError,
                    ExceptionLiteral::IOError,
                ],
                alias: Some("e".into()),
                block: ast![stmt_assign!(var!("a"), int!(2))],
            }],
            else_block: None,
            finally_block: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except ZeroDivisionError as e:
    a = 2
else:
    a = 4
finally:
    a = 3
"#;
        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(bin_op!(
                int!(4),
                Div,
                int!(0)
            )))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![ExceptionLiteral::ZeroDivisionError],
                alias: Some("e".into()),
                block: ast![stmt_assign!(var!("a"), int!(2))],
            }],
            else_block: Some(ast![stmt_assign!(var!("a"), int!(4))]),
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
try:
    pass
except:
    return
a = 1
"#;
        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Pass)],
            except_clauses: vec![ExceptClause {
                exception_types: vec![],
                alias: None,
                block: ast![stmt_return![]],
            }],
            else_block: None,
            finally_block: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn binary_literal() {
        let input = "a = 0b0010";
        let expected_ast = stmt_assign!(var!("a"), int!(2));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn octal_literal() {
        let input = "a = 0o0010";
        let expected_ast = stmt_assign!(var!("a"), int!(8));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn hex_literal() {
        let input = "a = 0x0010";
        let expected_ast = stmt_assign!(var!("a"), int!(16));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"
def test_args(*args):
    pass
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_args".into(),
            args: Params {
                args: vec![],
                args_var: Some("args".into()),
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
def test_args(*args, **kwargs):
    pass
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_args".into(),
            args: Params {
                args: vec![],
                args_var: Some("args".into()),
                kwargs_var: Some("kwargs".into()),
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
def test_kwargs(**kwargs):
    print(kwargs['a'])
"#;
        let ast = parse!(input, Statement);
        let expected_args = Params {
            args: vec![],
            args_var: None,
            kwargs_var: Some("kwargs".into()),
        };
        let StatementKind::FunctionDef { args, .. } = ast.kind else {
            panic!("Expected function def")
        };
        assert_eq!(expected_args, args);

        let input = r#"
def test_default(file=None):
    pass
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_default".into(),
            args: params![param!("file", Expr::None)],
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "test_kwargs(a=1, b=2)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int!(1)),
                    KwargsOperation::Pair("b".into(), int!(2)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**{'a':1, 'b':2})";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int!(1)),
                    KwargsOperation::Pair("b".into(), int!(2)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**{'a':1, 'b':2}, **{'c': 3})";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int!(1)),
                    KwargsOperation::Pair("b".into(), int!(2)),
                    KwargsOperation::Pair("c".into(), int!(3)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**first, **second)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Unpacking(var!("first")),
                    KwargsOperation::Unpacking(var!("second")),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**kwargs)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var!("kwargs"))],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(*args)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![],
                args_var: Some(Box::new(var!("args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(*args, **kwargs)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var!("kwargs"))],
                args_var: Some(Box::new(var!("args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
deprecated("collections.abc.ByteString",
)
"#;
        // TODO we have to use StatementKind::Expression here because it is on multiple lines, I
        // don't think this should technically be required
        let expected_ast = stmt(StatementKind::Expression(func_call!(
            "deprecated",
            call_args![str!("collections.abc.ByteString")]
        )));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn decorator() {
        let input = r#"
@test_decorator
def get_val():
    return 2
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "get_val".into(),
            args: params![],
            body: ast![stmt_return![int!(2)]],
            decorators: vec![var!("test_decorator")],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "test_decorator(get_val_undecorated)()";
        let expected_ast = func_call!(
            "<anonymous_from_callee>",
            call_args![],
            func_call!("test_decorator", call_args![var!("get_val_undecorated")])
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn raise() {
        let input = "raise Exception";
        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: call_args![],
        })));

        assert_ast_eq!(input, expected_ast);

        let input = r#"raise Exception("message")"#;
        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: call_args![str!("message")],
        })));

        assert_ast_eq!(input, expected_ast);

        let input = "raise";
        let expected_ast = stmt(StatementKind::Raise(None));

        assert_ast_eq!(input, expected_ast);

        let input = r#"raise Exception("message") from None"#;
        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: call_args![str!("message")],
        })));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn context_manager() {
        let input = r#"
with open('test.txt') as f:
    pass
"#;
        let expected_ast = stmt(StatementKind::ContextManager {
            expr: func_call!("open", call_args![str!("test.txt")]),
            variable: Some("f".into()),
            block: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
with open('test.txt'):
    pass
"#;
        let expected_ast = stmt(StatementKind::ContextManager {
            expr: func_call!("open", call_args![str!("test.txt")]),
            variable: None,
            block: ast![stmt(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn type_alias() {
        let input = "a = list[int]";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TypeNode(TypeNode::Generic {
                base_type: "list".into(),
                parameters: vec![TypeNode::Basic("int".into())],
            })
        );

        assert_ast_eq!(input, expected_ast);

        let input = "u = int | str";
        let expected_ast = stmt_assign!(
            var!("u"),
            Expr::TypeNode(TypeNode::Union(vec![
                TypeNode::Basic("int".into()),
                TypeNode::Basic("str".into()),
            ]))
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn delete() {
        let input = "del a";
        let expected_ast = stmt(StatementKind::Delete(vec![var!("a")]));

        assert_ast_eq!(input, expected_ast);

        let input = "del a, b, c";
        let expected_ast = stmt(StatementKind::Delete(vec![var!("a"), var!("b"), var!("c")]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn byte_string() {
        let input = "a = b'hello'";
        let expected_ast = stmt_assign!(var!("a"), Expr::ByteStringLiteral("hello".into()));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn compound_operator() {
        let input = "a += 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Add,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a -= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Subtract,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a *= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Multiply,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a /= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Divide,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_ast_eq!(input, expected_ast);

        let input = "a &= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseAnd,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a |= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseOr,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a ^= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseXor,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a //= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::IntegerDiv,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a <<= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::LeftShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a >>= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::RightShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a %= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Mod,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a @= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::MatMul,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a **= 1";
        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Expo,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "~a";
        let expected_ast = unary_op!(BitwiseNot, var!("a"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a % b";
        let expected_ast = bin_op!(var!("a"), Mod, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a @ b";
        let expected_ast = bin_op!(var!("a"), MatMul, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn f_strings() {
        let input = r#"f"Hello {name}.""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(".".into()),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"{first}{last}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("first")),
                format: FormatOption::Str,
            }),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("last")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello""#;
        let expected_ast = Expr::FString(vec![FStringPart::String("Hello".into())]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello {name} goodbye {other}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("other")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Age: {num + 1}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Age: ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(bin_op!(var!("num"), Add, int!(1))),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"environ({{{formatted_items}}})""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("environ({".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("formatted_items")),
                format: FormatOption::Str,
            }),
            FStringPart::String("})".into()),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello {name!r} goodbye {other}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Repr,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("other")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn binary_operators() {
        let input = "a & b";
        let expected_ast = bin_op!(var!("a"), BitwiseAnd, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a | b";
        let expected_ast = bin_op!(var!("a"), BitwiseOr, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a ^ b";
        let expected_ast = bin_op!(var!("a"), BitwiseXor, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a << b";
        let expected_ast = bin_op!(var!("a"), LeftShift, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a >> b";
        let expected_ast = bin_op!(var!("a"), RightShift, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a ** b";
        let expected_ast = bin_op!(var!("a"), Expo, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 * 3 << 2 + 4 & 205";
        let expected_ast = bin_op!(
            bin_op!(
                bin_op!(int!(2), Mul, int!(3)),
                LeftShift,
                bin_op!(int!(2), Add, int!(4))
            ),
            BitwiseAnd,
            int!(205)
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn control_flow() {
        let input = r#"
for i in a:
    break
"#;
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var!("a"),
            body: ast![stmt(StatementKind::Break)],
            else_block: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
for i in a:
    continue
"#;
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var!("a"),
            body: ast![stmt(StatementKind::Continue)],
            else_block: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
for i in a:
    break
else:
    pass
"#;
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var!("a"),
            body: ast![stmt(StatementKind::Break)],
            else_block: Some(ast![stmt(StatementKind::Pass)]),
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn type_hints() {
        let input = "
def add(x: str, y: str) -> str:
    return x + y
";
        let ast = parse!(input, Statement);
        let expected_args = params![param!("x"), param!("y")];

        let StatementKind::FunctionDef { args, .. } = ast.kind else {
            panic!("Expected function def!")
        };

        assert_eq!(args, expected_args)
    }

    #[test]
    fn slices() {
        let input = "a[1:1:1]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: Some(Box::new(int!(1))),
                stop: Some(Box::new(int!(1))),
                step: Some(Box::new(int!(1))),
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[2:5]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: Some(Box::new(int!(2))),
                stop: Some(Box::new(int!(5))),
                step: None,
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[:5]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: None,
                stop: Some(Box::new(int!(5))),
                step: None,
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[3:]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: Some(Box::new(int!(3))),
                stop: None,
                step: None,
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[::2]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: None,
                stop: None,
                step: Some(Box::new(int!(2))),
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[:]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("a")),
            params: SliceParams {
                start: None,
                stop: None,
                step: None,
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "new_bases[i+shift:shift+1]";
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var!("new_bases")),
            params: SliceParams {
                start: Some(Box::new(bin_op!(var!("i"), Add, var!("shift")))),
                stop: Some(Box::new(bin_op!(var!("shift"), Add, int!(1)))),
                step: None,
            },
        };

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn closures() {
        let input = "
def outer():
    a = 1
    b = 2
    def inner():
        b = 3
        print(a)
";
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "outer".into(),
            args: params![],
            body: ast![
                stmt_assign!(var!("a"), int!(1)),
                stmt_assign!(var!("b"), int!(2)),
                stmt(StatementKind::FunctionDef {
                    name: "inner".into(),
                    args: params![],
                    body: ast![
                        stmt_assign!(var!("b"), int!(3)),
                        stmt(StatementKind::Expression(func_call!(
                            "print",
                            call_args![var!("a")]
                        ))),
                    ],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn scope_modifiers() {
        let input = "nonlocal var";
        let expected_ast = stmt(StatementKind::Nonlocal(vec!["var".into()]));

        assert_ast_eq!(input, expected_ast);

        let input = "nonlocal var, var2";
        let expected_ast = stmt(StatementKind::Nonlocal(vec!["var".into(), "var2".into()]));

        assert_ast_eq!(input, expected_ast);

        let input = "global var";
        let expected_ast = stmt(StatementKind::Global(vec!["var".into()]));

        assert_ast_eq!(input, expected_ast);

        let input = "global var, var2";
        let expected_ast = stmt(StatementKind::Global(vec!["var".into(), "var2".into()]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn ternary_operation() {
        let input = "a = 4 if True else 5";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TernaryOp {
                condition: Box::new(bool!(true)),
                if_value: Box::new(int!(4)),
                else_value: Box::new(int!(5)),
            }
        );

        assert_ast_eq!(input, expected_ast);

        let input = "a = 4 + x if b == 6 else 5 << 2";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TernaryOp {
                condition: Box::new(bin_op!(var!("b"), Equals, int!(6))),
                if_value: Box::new(bin_op!(int!(4), Add, var!("x"))),
                else_value: Box::new(bin_op!(int!(5), LeftShift, int!(2))),
            }
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn more_tokens() {
        let input = "Ellipsis";
        let expected_ast = Expr::Ellipsis;

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn generator_comprehension() {
        let input = "a = (i * 2 for i in b)";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::GeneratorComprehension {
                body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
                clauses: vec![ForClause {
                    indices: vec!["i".into()],
                    iterable: var!("b"),
                    condition: None,
                }],
            }
        );

        assert_ast_eq!(input, expected_ast);

        let input = "foo(i * 2 for i in b)";
        let expected_ast = func_call!(
            "foo",
            call_args![Expr::GeneratorComprehension {
                body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
                clauses: vec![ForClause {
                    indices: vec!["i".into()],
                    iterable: var!("b"),
                    condition: None,
                }],
            }]
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn default_args() {
        let input = r#"
def foo(data=None):
    pass
"#;
        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "foo".into(),
            args: params![param!("data", Expr::None)],
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn unpacking() {
        let input = "(*l,)";
        let expected_ast = tuple![unary_op!(Unpack, var!("l"))];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "foo(a, *b[1:])";
        let expected_ast = func_call!(
            "foo",
            CallArgs {
                args: vec![var!("a")],
                kwargs: vec![],
                args_var: Some(Box::new(Expr::SliceOperation {
                    object: Box::new(var!("b")),
                    params: SliceParams {
                        start: Some(Box::new(int!(1))),
                        stop: None,
                        step: None,
                    },
                })),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
if True:
    a, b = b, a
"#;
        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: bool!(true),
                block: ast![stmt(StatementKind::UnpackingAssignment {
                    left: vec![var!("a"), var!("b")],
                    right: tuple![var!("b"), var!("a"),],
                })],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a, = b,";
        let expected_ast = stmt(StatementKind::UnpackingAssignment {
            left: vec![var!("a")],
            right: tuple![var!("b")],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn multiple_assignment() {
        let input = "a = b = True";
        let expected_ast = stmt(StatementKind::MultipleAssignment {
            left: vec![var!("a"), var!("b")],
            right: bool!(true),
        });

        assert_ast_eq!(input, expected_ast);
    }
}
