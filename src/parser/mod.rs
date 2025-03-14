use std::collections::HashSet;

pub mod static_analysis;
pub mod types;

use types::Statement;

use crate::{
    ast,
    core::{log, Container, LogLevel},
    domain::ExceptionLiteral,
    lexer::types::Token,
    parser::types::{
        Alias, Ast, BinOp, CompoundOperator, ConditionalBlock, DictOperation, ExceptClause,
        ExceptionInstance, Expr, ExprFormat, FStringPart, ForClause, FormatOption, ImportPath,
        ImportedItem, KwargsOperation, LogicalOp, LoopIndex, ParsedArgDefinition,
        ParsedArgDefinitions, ParsedArgument, ParsedArguments, ParsedSliceParams, RegularImport,
        StatementKind, TypeNode, UnaryOp, Variable,
    },
    treewalk::State,
    types::errors::ParserError,
};

fn int(val: i64) -> Expr {
    Expr::Integer(val)
}

static EOF: Token = Token::Eof;

/// A recursive-descent parser which attempts to encode the full Python grammar.
pub struct Parser<'a> {
    state: Container<State>,
    tokens: &'a [Token],
    current_token: &'a Token,
    position: usize,
    line_number: usize,
    delimiter_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], state: Container<State>) -> Self {
        let current_token = tokens.first().unwrap_or(&EOF);
        Parser {
            state,
            tokens,
            current_token,
            position: 0,
            line_number: 1,
            delimiter_depth: 0,
        }
    }

    pub fn is_finished(&self) -> bool {
        self.current_token == &EOF
    }

    fn end_of_statement(&self) -> bool {
        self.is_finished() || self.current_token == &Token::Newline
    }

    fn inside_delimiter(&self) -> bool {
        self.delimiter_depth > 0
    }

    /// Return the token a given number ahead of the current position.
    fn peek(&self, ahead: usize) -> &Token {
        self.tokens.get(self.position + ahead).unwrap_or(&EOF)
    }

    /// Get a reference to a slice of the remaining tokens.
    fn remaining_tokens(&self) -> &[Token] {
        &self.tokens[self.position..]
    }

    /// Does the slice of reamining tokens contain the sought after token?
    fn has(&self, target: &Token) -> bool {
        self.remaining_tokens().contains(target)
    }

    /// How far away is the given token, if it exists in the reaming tokens? This is useful on
    /// slice operations.
    fn num_away(&self, target: &Token) -> Result<usize, ParserError> {
        self.remaining_tokens()
            .iter()
            .position(|token| token == target)
            .ok_or(ParserError::ExpectedToken(target.clone(), Token::Eof))
    }

    /// Check whether the next `tokens.len()` tokens matches those provided, without consuming any
    /// tokens. This is useful for multi-token operations or where extra context is needed.
    fn peek_ahead_contains(&self, tokens: &[Token]) -> bool {
        for (index, token) in tokens.iter().enumerate() {
            if self.peek(index) != token {
                return false;
            }
        }

        true
    }

    /// If we are inside a string literal, we must check for newline characters rather than
    /// tokens. These are produced by `Lexer::emit_newline`.
    fn advance_line_number_if_needed(&mut self) {
        if self.current_token == &Token::Newline {
            self.line_number += 1;
        } else if let Token::StringLiteral(string) = &self.current_token {
            self.line_number += string.matches('\n').count();
        } else if let Token::RawStringLiteral(string) = &self.current_token {
            self.line_number += string.matches('\n').count();
        }
    }

    fn consume(&mut self, expected: &Token) -> Result<(), ParserError> {
        log(LogLevel::Trace, || {
            format!("Token: {:?}", self.current_token)
        });

        if self.current_token != expected {
            return Err(ParserError::ExpectedToken(
                expected.clone(),
                self.current_token.clone(),
            ));
        }

        self.advance_line_number_if_needed();

        if matches!(
            self.current_token,
            Token::LParen | Token::LBracket | Token::LBrace
        ) {
            // We don't treat this as an actual stack because the comparison against expected
            // above handles this for us.
            self.delimiter_depth += 1;
        } else if matches!(
            self.current_token,
            Token::RParen | Token::RBracket | Token::RBrace
        ) {
            self.delimiter_depth -= 1;
        }

        self.position += 1;
        self.current_token = self.tokens.get(self.position).unwrap_or(&EOF);

        // Newlines are allowed freely inside delimiters (), [], {} in Python because the parser
        // expects there to be an ending coming.
        if self.inside_delimiter() {
            self.consume_optional_many(&Token::Newline);
        }

        Ok(())
    }

    fn consume_optional(&mut self, expected: &Token) {
        if self.current_token == expected {
            let _ = self.consume(expected);
        }
    }

    fn consume_optional_many(&mut self, expected: &Token) {
        while self.current_token == expected {
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

        if self.current_token == &Token::Comma {
            let mut items = vec![left];
            while self.current_token == &Token::Comma {
                self.consume(&Token::Comma)?;

                // We need this for the case of a trailing comma, which is most often used for a
                // tuple with a single element.
                //
                // The [`Token::Assign`] is when this happens on the LHS.
                if self.end_of_statement() || self.current_token == &Token::Assign {
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
        if self.current_token == &Token::Await {
            self.parse_await_expr()
        } else {
            self.parse_ternary_expr()
        }
    }

    fn parse_await_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_await_expr".to_string());
        self.consume(&Token::Await)?;
        let right = self.parse_ternary_expr()?;
        Ok(Expr::Await {
            right: Box::new(right),
        })
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

        if self.current_token == &Token::If {
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
            self.current_token,
            Token::BitwiseAnd | Token::BitwiseOr | Token::BitwiseXor
        ) {
            let op = BinOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
            self.consume(self.current_token)?;
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

        while matches!(self.current_token, Token::Plus | Token::Minus) {
            let op = BinOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
            self.consume(self.current_token)?;
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

        while matches!(self.current_token, Token::LeftShift | Token::RightShift) {
            let op = BinOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
            self.consume(self.current_token)?;
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

        if self.current_token == &Token::LParen {
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
        let params = if self.peek_ahead_contains(&[Token::Colon, Token::Colon]) {
            self.consume(&Token::Colon)?;
            self.consume(&Token::Colon)?;
            let step = Some(Box::new(self.parse_simple_expr()?));
            (true, None, None, step)
            // [:] - this syntax is useful to replace the items in a list without changing the
            // list's reference
        } else if self.peek_ahead_contains(&[Token::Colon, Token::RBracket]) {
            self.consume(&Token::Colon)?;
            (true, None, None, None)
            // [:2]
        } else if self.peek_ahead_contains(&[Token::Colon]) {
            self.consume(&Token::Colon)?;
            let stop = Some(Box::new(self.parse_simple_expr()?));
            (true, None, stop, None)
            // [2:]
            // if there is a Colon immediately before the next RBracket
        } else if self.has(&Token::Colon)
            && self.num_away(&Token::Colon)? + 1 == self.num_away(&Token::RBracket)?
        {
            let start = Some(Box::new(self.parse_simple_expr()?));
            self.consume(&Token::Colon)?;
            (true, start, None, None)
            // [1:1:1] or [2:5]
            // if there is a Colon before the next RBracket
        } else if self.has(&Token::Colon)
            && self.num_away(&Token::Colon)? < self.num_away(&Token::RBracket)?
        {
            let start = Some(Box::new(self.parse_simple_expr()?));
            self.consume(&Token::Colon)?;
            let stop = Some(Box::new(self.parse_simple_expr()?));
            let step = if self.current_token == &Token::Colon {
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
                params: ParsedSliceParams {
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

        while self.current_token == &Token::DoubleAsterisk {
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

        while matches!(self.current_token, Token::Dot | Token::LBracket) {
            left = match self.current_token {
                Token::Dot => self.parse_member_access(left)?,
                Token::LBracket => self.parse_index_access(left)?,
                _ => unreachable!(),
            };
        }

        if self.current_token == &Token::LParen {
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

        while matches!(self.current_token, Token::And | Token::Or) {
            let op = LogicalOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
            self.consume(self.current_token)?;
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
            self.current_token,
            Token::Asterisk | Token::Slash | Token::DoubleSlash | Token::Modulo | Token::AtSign
        ) {
            let op = BinOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
            self.consume(self.current_token)?;
            let right = self.parse_access_operations()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        while matches!(
            self.current_token,
            Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::Equal
                | Token::NotEqual
                | Token::In
                | Token::Is
        ) || self.peek_ahead_contains(&[Token::Not, Token::In])
            || self.peek_ahead_contains(&[Token::Is, Token::Not])
        {
            // Handle two tokens to produce one `BinOp::NotIn` operation. If this gets too messy,
            // we could look to move multi-word tokens into the lexer.
            let op = if self.peek_ahead_contains(&[Token::Not, Token::In]) {
                self.consume(&Token::Not)?;
                self.consume(&Token::In)?;
                BinOp::NotIn
            } else if self.peek_ahead_contains(&[Token::Is, Token::Not]) {
                self.consume(&Token::Is)?;
                self.consume(&Token::Not)?;
                BinOp::IsNot
            } else {
                let op = BinOp::try_from(self.current_token).unwrap_or_else(|_| unreachable!());
                self.consume(self.current_token)?;
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
        match self.current_token.clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(int(-(i as i64)))
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
        match self.current_token.clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(int(i as i64))
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
            format!("parse_factor: {:?}", self.current_token)
        });
        match self.current_token.clone() {
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

                if self.current_token == &Token::From {
                    self.consume(&Token::From)?;
                    let expr = self.parse_simple_expr()?;
                    Ok(Expr::YieldFrom(Box::new(expr)))
                // The [`Token::RParen`] can be found on generator lambdas.
                } else if self.end_of_statement() || self.current_token == &Token::RParen {
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
                Ok(int(i as i64))
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
                if self.peek(1) == &Token::LParen {
                    let name = self.parse_identifier()?;
                    let args = self.parse_function_call_args()?;

                    let first_call = if self.state.is_class(&name) {
                        Expr::ClassInstantiation { name, args }
                    } else {
                        Expr::FunctionCall {
                            name,
                            args,
                            callee: None,
                        }
                    };

                    Ok(first_call)
                } else if self.current_token.is_type() {
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
            _ => Err(ParserError::UnexpectedToken(self.current_token.clone())),
        }
    }

    fn parse_indented_block(&mut self) -> Result<Ast, ParserError> {
        self.consume_optional_many(&Token::Newline);
        self.consume(&Token::Indent)?;

        let mut statements = Vec::new();
        while self.current_token != &Token::Dedent {
            statements.push(self.parse_statement()?);
        }
        self.consume(&Token::Dedent)?;
        self.consume_optional_many(&Token::Newline);

        Ok(Ast::new(statements))
    }

    fn parse_import_path(&mut self) -> Result<ImportPath, ParserError> {
        match self.current_token {
            Token::Dot => {
                self.consume(&Token::Dot)?;
                let mut levels = 0;
                while self.current_token == &Token::Dot {
                    self.consume(&Token::Dot)?;
                    levels += 1;
                }

                let path = if matches!(self.current_token, Token::Identifier(_)) {
                    let mut path = vec![self.parse_identifier()?];
                    while self.current_token == &Token::Dot {
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
                while self.current_token == &Token::Dot {
                    self.consume(&Token::Dot)?;
                    path.push(self.parse_identifier()?);
                }

                Ok(ImportPath::Absolute(path))
            }
        }
    }

    fn parse_alias(&mut self) -> Result<Option<String>, ParserError> {
        if self.current_token == &Token::As {
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

            if self.current_token == &Token::Comma {
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
        let stmt = match self.current_token {
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

                    match self.current_token {
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
                                self.current_token.clone(),
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
        Ok(int(result))
    }

    fn parse_octal_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::OctalLiteral(literal.clone()))?;

        let result = i64::from_str_radix(&literal[2..], 8).map_err(|_| ParserError::SyntaxError)?;
        Ok(int(result))
    }

    fn parse_hex_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::HexLiteral(literal.clone()))?;

        let result =
            i64::from_str_radix(&literal[2..], 16).map_err(|_| ParserError::SyntaxError)?;
        Ok(int(result))
    }

    fn parse_type_node(&mut self) -> Result<TypeNode, ParserError> {
        let mut nodes = vec![];

        loop {
            let node = match self.current_token {
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

                        if self.current_token == &Token::LBracket {
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

            if self.current_token != &Token::BitwiseOr {
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

        let variable = if self.current_token == &Token::As {
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

        let instance = if matches!(self.current_token, Token::Identifier(_)) {
            let literal = self.parse_exception_literal()?;

            let args = if self.current_token == &Token::LParen {
                self.parse_function_call_args()?
            } else {
                ParsedArguments::default()
            };

            // TODO support exception chaining here and in the interpreter
            if self.current_token == &Token::From {
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
        while self.current_token == &Token::Except {
            self.consume(&Token::Except)?;
            if self.current_token == &Token::Colon {
                self.consume(&Token::Colon)?;
                let except_block = self.parse_indented_block()?;
                except_clauses.push(ExceptClause {
                    exception_types: vec![],
                    alias: None,
                    block: except_block,
                });
            } else if self.current_token == &Token::LParen {
                self.consume(&Token::LParen)?;
                let mut literals = vec![];
                while self.current_token != &Token::RParen {
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

        let else_block = if self.current_token == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        let finally_block = if self.current_token == &Token::Finally {
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
        while self.current_token == &Token::Elif {
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

        let else_part = if self.current_token == &Token::Else {
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
        let index = if self.current_token == &Token::Comma {
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

        let else_block = if self.current_token == &Token::Else {
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
        if self.current_token == &Token::Newline {
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

        if self.current_token == &Token::LParen {
            self.consume(&Token::LParen)?;
            while self.current_token != &Token::RParen {
                if self.peek_ahead_contains(&[Token::Identifier("metaclass".into()), Token::Assign])
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

        while self.current_token == &Token::AtSign {
            self.consume(&Token::AtSign)?;
            decorators.push(self.parse_simple_expr()?);

            // Each decorator must be ended by 1 and only 1 newline
            self.consume(&Token::Newline)?;
        }

        let is_async = if self.current_token == &Token::Async {
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
        if self.current_token == &Token::ReturnTypeArrow {
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

            if self.current_token != &Token::Comma {
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
        let stmt = match self.current_token.clone() {
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

        if self.current_token == &Token::Assign {
            self.consume(&Token::Assign)?;
            match left {
                Expr::Tuple(vars) => Ok(StatementKind::UnpackingAssignment {
                    left: vars,
                    right: self.parse_expr()?,
                }),
                _ => {
                    let mut left_items = vec![left];

                    let mut right = self.parse_expr()?;
                    while self.current_token == &Token::Assign {
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
        } else if self.current_token.is_compound_assign() {
            let operator = match self.current_token {
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
            self.consume(self.current_token)?;

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

        let expr = if self.current_token == &Token::LParen {
            self.consume(&Token::LParen)?;
            let expr = self.parse_simple_expr()?;
            self.consume(&Token::RParen)?;
            expr
        } else {
            self.parse_simple_expr()?
        };

        Ok(Expr::Lambda {
            args: Box::new(args),
            expr: Box::new(expr),
        })
    }

    fn parse_list(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_list".to_string());
        let mut items = Vec::new();

        self.consume(&Token::LBracket)?;
        while self.current_token != &Token::RBracket {
            let expr = self.parse_simple_expr()?;
            items.push(expr.clone());

            if self.current_token == &Token::Comma {
                self.consume(&Token::Comma)?;

                // Handle trailing comma
                if self.current_token == &Token::RBracket {
                    self.consume(&Token::RBracket)?;
                    return Ok(Expr::List(items));
                }
            } else if self.current_token == &Token::RBracket {
                self.consume(&Token::RBracket)?;
                return Ok(Expr::List(items));
            }

            if self.current_token == &Token::For {
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
        while self.current_token != &Token::FStringEnd {
            match self.current_token.clone() {
                Token::StringLiteral(s) => {
                    self.consume(&Token::StringLiteral(s.to_string()))?;
                    parts.push(FStringPart::String(s.to_string()));
                }
                Token::LBrace => {
                    // Start consuming the expression within braces
                    self.consume(&Token::LBrace)?;
                    let expr = self.parse_simple_expr()?;

                    let format = if self.current_token == &Token::Exclamation {
                        self.consume(&Token::Exclamation)?;
                        if let Token::Identifier(token) = self.current_token.clone() {
                            self.consume(&Token::Identifier(token.to_string()))?;
                            match token.as_str() {
                                "r" => FormatOption::Repr,
                                "s" => FormatOption::Str,
                                "a" => FormatOption::Ascii,
                                _ => {
                                    return Err(ParserError::UnexpectedToken(
                                        self.current_token.clone(),
                                    ));
                                }
                            }
                        } else {
                            return Err(ParserError::UnexpectedToken(self.current_token.clone()));
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
                    return Err(ParserError::UnexpectedToken(self.current_token.clone()));
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
        while self.current_token != &Token::RBrace {
            let key = self.parse_simple_expr()?;

            match self.current_token {
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
                    if self.current_token == &Token::RBrace {
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

                    match self.current_token {
                        // A Comma or an RBrace indicates the end of an element, which means this
                        // element _will_ be a key-value (i.e. have a colon). Only an RBrace
                        // indicates an end to the entire literal.
                        Token::Comma | Token::RBrace => {
                            pairs.push(DictOperation::Pair(key, value));
                            self.consume_optional(&Token::Comma);
                            if self.current_token == &Token::RBrace {
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
                        _ => return Err(ParserError::UnexpectedToken(self.current_token.clone())),
                    }
                }
                _ => return Err(ParserError::UnexpectedToken(self.current_token.clone())),
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

    fn parse_function_def_args(
        &mut self,
        end_token: Token,
    ) -> Result<ParsedArgDefinitions, ParserError> {
        let mut args = Vec::new();
        let mut args_var = None;
        let mut kwargs_var = None;
        while self.current_token != &end_token {
            // This is to support positional-only parameters.
            // Context: PEP 570 (https://peps.python.org/pep-0570/)
            // TODO test positional-only parameters now that we support args/kwargs
            if self.current_token == &Token::Slash {
                self.consume(&Token::Slash)?;

                // We will only see a comma if the slash isn't the last "parameter".
                // We test this is the "slash_args" interpreter test. This is also found in
                // types.py in the standard lib.
                if self.current_token == &Token::Comma {
                    self.consume(&Token::Comma)?;
                } else {
                    break;
                }
            }

            if self.current_token == &Token::Asterisk {
                self.consume(&Token::Asterisk)?;

                // We will see an asterisk without a trailing identifier for keyword-only
                // parameters. TODO we do not yet enforce this.
                // Context: PEP 3102 (https://peps.python.org/pep-3102/)
                if matches!(self.current_token, Token::Identifier(_)) {
                    args_var = Some(self.parse_identifier()?);
                }

                // If *args is not at the end of the args (only kwargs can come after), we must
                // allow for a comma. This is similar to how we optionally consume a comma as the
                // last step of each loop iteration.
                self.consume_optional(&Token::Comma);
                continue;
            }

            if self.current_token == &Token::DoubleAsterisk {
                self.consume(&Token::DoubleAsterisk)?;
                kwargs_var = Some(self.parse_identifier()?);
                break;
            }

            let arg = self.parse_identifier()?;
            let default = if self.current_token == &Token::Assign {
                self.consume(&Token::Assign)?;
                Some(self.parse_simple_expr()?)
            } else {
                None
            };

            args.push(ParsedArgDefinition { arg, default });

            // Support for type hints. Will there be reason to store these alongside the params
            // themselves? Perhaps for future toolings like memphis-lsp.
            //
            // Not sure if the check for end_token here is correct or not. This is for lambdas.
            if end_token != Token::Colon && self.current_token == &Token::Colon {
                self.consume(&Token::Colon)?;
                let _type = self.parse_simple_expr()?;
            }

            self.consume_optional(&Token::Comma);
        }

        Ok(ParsedArgDefinitions {
            args,
            args_var,
            kwargs_var,
        })
    }

    fn parse_function_call_args(&mut self) -> Result<ParsedArguments, ParserError> {
        self.consume(&Token::LParen)?;

        let mut args = Vec::new();
        let mut kwargs = vec![];
        let mut args_var = None;
        while self.current_token != &Token::RParen {
            if self.current_token == &Token::Asterisk {
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
            if self.current_token == &Token::DoubleAsterisk {
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
                ParsedArgument::Keyword { arg, expr } => {
                    kwargs.push(KwargsOperation::Pair(arg, expr));
                }
                ParsedArgument::Positional(expr) => {
                    args.push(expr);
                }
            }

            self.consume_optional(&Token::Comma);
        }

        self.consume(&Token::RParen)?;

        Ok(ParsedArguments {
            args,
            kwargs,
            args_var,
        })
    }

    /// An argument in a function call can be either variable `a` or contain an equals such as
    /// `a = 4`. We originally (and ignorantly) called `parse_statement` but that contains too many
    /// other cases to be safely used inside function call parsing.
    fn parse_function_call_arg(&mut self) -> Result<ParsedArgument, ParserError> {
        let expr = self.parse_simple_expr()?;
        match self.current_token {
            Token::Assign => {
                self.consume(&Token::Assign)?;
                let arg = expr.as_variable().ok_or(ParserError::SyntaxError)?;
                Ok(ParsedArgument::Keyword {
                    arg,
                    expr: self.parse_simple_expr()?,
                })
            }
            Token::For => Ok(ParsedArgument::Positional(
                self.parse_generator_comprehension(&expr)?,
            )),
            _ => Ok(ParsedArgument::Positional(expr)),
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
        while self.current_token == &Token::For {
            clauses.push(self.parse_comprehension_clause()?);
        }
        Ok(clauses)
    }

    fn parse_comprehension_clause(&mut self) -> Result<ForClause, ParserError> {
        self.consume(&Token::For)?;

        // The parentheses are optional here, but if one is present, both must be present
        let mut need_rparen = false;
        if self.current_token == &Token::LParen {
            self.consume(&Token::LParen)?;
            need_rparen = true;
        }

        let mut indices = vec![self.parse_identifier()?];
        while self.current_token == &Token::Comma {
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

        let condition = if self.current_token == &Token::If {
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
        while self.current_token != &Token::RParen {
            let expr = self.parse_simple_expr()?;
            args.push(expr.clone());

            if self.current_token == &Token::Comma {
                self.consume(&Token::Comma)?;
                is_single_element = false;
            }

            if self.current_token == &Token::For {
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
        while self.current_token == &Token::Comma {
            self.consume(&Token::Comma)?;
            items.push(self.parse_identifier()?);
        }
        Ok(items)
    }

    /// Parse a `Token::Identifier` without any semantic analysis.
    fn parse_identifier(&mut self) -> Result<String, ParserError> {
        match self.current_token.clone() {
            Token::Identifier(name) => {
                let name_clone = name.clone();
                self.consume(&Token::Identifier(name_clone.clone()))?;
                Ok(name_clone)
            }
            _ => Err(ParserError::UnexpectedToken(self.current_token.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{types::KwargsOperation, *};
    use crate::{
        init::MemphisContext,
        treewalk::{
            types::{Class, ExprResult},
            Scope,
        },
    };

    fn init(text: &str) -> MemphisContext {
        MemphisContext::from_text(text)
    }

    fn stmt(kind: StatementKind) -> Statement {
        Statement::new(1, kind)
    }

    fn var(name: &str) -> Expr {
        Expr::Variable(name.to_string())
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
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(int(2)),
            op: BinOp::Add,
            right: Box::new(Expr::BinaryOperation {
                left: Box::new(int(3)),
                op: BinOp::Mul,
                right: Box::new(Expr::BinaryOperation {
                    left: Box::new(int(4)),
                    op: BinOp::Sub,
                    right: Box::new(int(1)),
                }),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "2 // 3";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(int(2)),
            op: BinOp::IntegerDiv,
            right: Box::new(int(3)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn string_literal() {
        let input = "\"Hello\"";
        let context = init(input);

        let expected_ast = Expr::StringLiteral("Hello".into());

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "\"\".join([])";
        let context = init(input);

        let expected_ast = Expr::MethodCall {
            object: Box::new(Expr::StringLiteral("".into())),
            name: "join".into(),
            args: ParsedArguments {
                args: vec![Expr::List(vec![])],
                kwargs: vec![],
                args_var: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn variable_assignment() {
        let input = "
a = 2
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: int(2),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
b = a + 3
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("b"),
            right: Expr::BinaryOperation {
                left: Box::new(var("a")),
                op: BinOp::Add,
                right: Box::new(int(3)),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
a, b = (1, 2)
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::UnpackingAssignment {
            left: vec![var("a"), var("b")],
            right: Expr::Tuple(vec![int(1), Expr::Integer(2)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn function_call() {
        let input = "print(\"Hello, World!\")";
        let context = init(input);

        let expected_ast = Expr::FunctionCall {
            name: "print".to_string(),
            args: ParsedArguments {
                args: vec![Expr::StringLiteral("Hello, World!".to_string())],
                kwargs: vec![],
                args_var: None,
            },
            callee: None,
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "
a(*self.args, **self.kwargs)
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "a".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(Expr::MemberAccess {
                    object: Box::new(var("self")),
                    field: "kwargs".into(),
                })],
                args_var: Some(Box::new(Expr::MemberAccess {
                    object: Box::new(var("self")),
                    field: "args".into(),
                })),
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn function_definition() {
        let input = "
def add(x, y):
    return x + y
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "add".to_string(),
            args: ParsedArgDefinitions {
                args: vec![
                    ParsedArgDefinition {
                        arg: "x".into(),
                        default: None,
                    },
                    ParsedArgDefinition {
                        arg: "y".into(),
                        default: None,
                    },
                ],
                args_var: None,
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Return(vec![Expr::BinaryOperation {
                left: Box::new(var("x")),
                op: BinOp::Add,
                right: Box::new(var("y")),
            }]))],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
def _f(): pass
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "_f".to_string(),
            args: ParsedArgDefinitions::default(),
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
lambda: 4
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Lambda {
            args: Box::new(ParsedArgDefinitions::default()),
            expr: Box::new(int(4)),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
lambda index: 4
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Lambda {
            args: Box::new(ParsedArgDefinitions {
                args: vec![ParsedArgDefinition {
                    arg: "index".into(),
                    default: None,
                }],
                args_var: None,
                kwargs_var: None,
            }),
            expr: Box::new(int(4)),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
lambda index, val: 4
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Lambda {
            args: Box::new(ParsedArgDefinitions {
                args: vec![
                    ParsedArgDefinition {
                        arg: "index".into(),
                        default: None,
                    },
                    ParsedArgDefinition {
                        arg: "val".into(),
                        default: None,
                    },
                ],
                args_var: None,
                kwargs_var: None,
            }),
            expr: Box::new(int(4)),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
lambda: (yield)
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Lambda {
            args: Box::new(ParsedArgDefinitions::default()),
            expr: Box::new(Expr::Yield(None)),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
(lambda: (yield))()
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "<anonymous_from_callee>".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![],
                args_var: None,
            },
            callee: Some(Box::new(Expr::Lambda {
                args: Box::new(ParsedArgDefinitions::default()),
                expr: Box::new(Expr::Yield(None)),
            })),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
def __init__(
    self, *, indent=None,
):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "__init__".into(),
            args: ParsedArgDefinitions {
                args: vec![
                    ParsedArgDefinition {
                        arg: "self".into(),
                        default: None,
                    },
                    ParsedArgDefinition {
                        arg: "indent".into(),
                        default: Some(Expr::None),
                    },
                ],
                args_var: None,
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
return a, b
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Return(vec![var("a"), var("b")]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn boolean_expressions() {
        let input = "x and y\n";
        let context = init(input);

        let expected_ast = Expr::LogicalOperation {
            left: Box::new(var("x")),
            op: LogicalOp::And,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x or y\n";
        let context = init(input);

        let expected_ast = Expr::LogicalOperation {
            left: Box::new(var("x")),
            op: LogicalOp::Or,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x or not y\n";
        let context = init(input);

        let expected_ast = Expr::LogicalOperation {
            left: Box::new(var("x")),
            op: LogicalOp::Or,
            right: Box::new(Expr::UnaryOperation {
                op: UnaryOp::Not,
                right: Box::new(var("y")),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "not (x or y)\n";
        let context = init(input);

        let expected_ast = Expr::UnaryOperation {
            op: UnaryOp::Not,
            right: Box::new(Expr::LogicalOperation {
                left: Box::new(var("x")),
                op: LogicalOp::Or,
                right: Box::new(var("y")),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"
if (a
    or b):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::LogicalOperation {
                    left: Box::new(var("a")),
                    op: LogicalOp::Or,
                    right: Box::new(var("b")),
                },
                block: ast![stmt(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn comparison_operators() {
        let input = "x == y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::Equals,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x != y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::NotEquals,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x < y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::LessThan,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x > y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::GreaterThan,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x >= y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::GreaterThanOrEqual,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x <= y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::LessThanOrEqual,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x in y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::In,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x not in y";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::NotIn,
            right: Box::new(var("y")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x is None";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::Is,
            right: Box::new(Expr::None),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x is not None";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("x")),
            op: BinOp::IsNot,
            right: Box::new(Expr::None),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn boolean_operators() {
        let input = "x = True\n";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("x"),
            right: Expr::Boolean(true),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "True or False\n";
        let context = init(input);

        let expected_ast = Expr::LogicalOperation {
            left: Box::new(Expr::Boolean(true)),
            op: LogicalOp::Or,
            right: Box::new(Expr::Boolean(false)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "x = None\n";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("x"),
            right: Expr::None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "return None\n";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Return(vec![Expr::None]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
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
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(var("x")),
                    op: BinOp::GreaterThan,
                    right: Box::new(int(0)),
                },
                block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                    name: "print".to_string(),
                    args: ParsedArguments {
                        args: vec![Expr::StringLiteral("Greater".to_string())],
                        kwargs: vec![],
                        args_var: None,
                    },
                    callee: None,
                }))],
            },
            elif_parts: vec![ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(var("x")),
                    op: BinOp::GreaterThan,
                    right: Box::new(int(-10)),
                },
                block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                    name: "print".to_string(),
                    args: ParsedArguments {
                        args: vec![Expr::StringLiteral("Medium".to_string())],
                        kwargs: vec![],
                        args_var: None,
                    },
                    callee: None,
                }))],
            }],
            else_part: Some(ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                name: "print".to_string(),
                args: ParsedArguments {
                    args: vec![Expr::StringLiteral("Less".to_string())],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            }))]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Medium")
elif x > -20:
    print("Less")
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(var("x")),
                    op: BinOp::GreaterThan,
                    right: Box::new(int(0)),
                },
                block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                    name: "print".to_string(),
                    args: ParsedArguments {
                        args: vec![Expr::StringLiteral("Greater".to_string())],
                        kwargs: vec![],
                        args_var: None,
                    },
                    callee: None,
                }))],
            },
            elif_parts: vec![
                ConditionalBlock {
                    condition: Expr::BinaryOperation {
                        left: Box::new(var("x")),
                        op: BinOp::GreaterThan,
                        right: Box::new(int(-10)),
                    },
                    block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                        name: "print".to_string(),
                        args: ParsedArguments {
                            args: vec![Expr::StringLiteral("Medium".to_string())],
                            kwargs: vec![],
                            args_var: None,
                        },
                        callee: None,
                    }))],
                },
                ConditionalBlock {
                    condition: Expr::BinaryOperation {
                        left: Box::new(var("x")),
                        op: BinOp::GreaterThan,
                        right: Box::new(int(-20)),
                    },
                    block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                        name: "print".to_string(),
                        args: ParsedArguments {
                            args: vec![Expr::StringLiteral("Less".to_string())],
                            kwargs: vec![],
                            args_var: None,
                        },
                        callee: None,
                    }))],
                },
            ],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
if x > 0:
    print("Greater")
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::BinaryOperation {
                    left: Box::new(var("x")),
                    op: BinOp::GreaterThan,
                    right: Box::new(int(0)),
                },
                block: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                    name: "print".to_string(),
                    args: ParsedArguments {
                        args: vec![Expr::StringLiteral("Greater".to_string())],
                        kwargs: vec![],
                        args_var: None,
                    },
                    callee: None,
                }))],
            },
            elif_parts: vec![],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
if True: return False
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::Boolean(true),
                block: ast![stmt(StatementKind::Return(vec![Expr::Boolean(false)]))],
            },
            elif_parts: vec![],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
if (a == 1
        and b
        and c):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::LogicalOperation {
                    left: Box::new(Expr::LogicalOperation {
                        left: Box::new(Expr::BinaryOperation {
                            left: Box::new(var("a")),
                            op: BinOp::Equals,
                            right: Box::new(int(1)),
                        }),
                        op: LogicalOp::And,
                        right: Box::new(var("b")),
                    }),
                    op: LogicalOp::And,
                    right: Box::new(var("c")),
                },
                block: ast![stmt(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn while_loop() {
        let input = "
while True:
    print(\"busy loop\")
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::WhileLoop {
            condition: Expr::Boolean(true),
            body: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                name: "print".to_string(),
                args: ParsedArguments {
                    args: vec![Expr::StringLiteral("busy loop".to_string())],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            }))],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
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
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![],
            metaclass: None,
            body: ast![
                stmt(StatementKind::FunctionDef {
                    name: "__init__".to_string(),
                    args: ParsedArgDefinitions {
                        args: vec![ParsedArgDefinition {
                            arg: "self".to_string(),
                            default: None,
                        }],
                        args_var: None,
                        kwargs_var: None,
                    },
                    body: ast![stmt(StatementKind::Assignment {
                        left: Expr::MemberAccess {
                            object: Box::new(var("self")),
                            field: "x".to_string(),
                        },
                        right: int(0),
                    })],
                    decorators: vec![],
                    is_async: false,
                }),
                stmt(StatementKind::FunctionDef {
                    name: "bar".to_string(),
                    args: ParsedArgDefinitions {
                        args: vec![ParsedArgDefinition {
                            arg: "self".to_string(),
                            default: None,
                        }],
                        args_var: None,
                        kwargs_var: None,
                    },
                    body: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                        name: "print".to_string(),
                        args: ParsedArguments {
                            args: vec![Expr::MemberAccess {
                                object: Box::new(var("self")),
                                field: "x".to_string(),
                            }],
                            kwargs: vec![],
                            args_var: None,
                        },
                        callee: None,
                    }))],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
class Foo(Bar, Baz): pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![var("Bar"), var("Baz")],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
class Foo(module.Bar): pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![Expr::MemberAccess {
                object: Box::new(var("module")),
                field: "Bar".into(),
            }],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn class_instantiation() {
        let input = r#"
foo = Foo()
"#;

        let mut scope = Scope::default();
        let foo = Class::new_base("Foo".to_string(), vec![], None, Scope::default());
        scope.insert("Foo", ExprResult::Class(foo));

        let context = init(input);
        context.state.push_local(Container::new(scope));

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("foo"),
            right: Expr::ClassInstantiation {
                name: "Foo".to_string(),
                args: ParsedArguments::default(),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn method_invocation() {
        let mut scope = Scope::default();
        let foo = Class::new_base("Foo".to_string(), vec![], None, Scope::default());
        scope.insert("Foo", ExprResult::Class(foo));

        let input = r#"
foo = Foo()
"#;
        let context = init(input);
        context.state.push_local(Container::new(scope.clone()));

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("foo"),
            right: Expr::ClassInstantiation {
                name: "Foo".to_string(),
                args: ParsedArguments::default(),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
foo.bar()
"#;
        let context = init(input);
        context.state.push_local(Container::new(scope));

        let expected_ast = stmt(StatementKind::Expression(Expr::MethodCall {
            object: Box::new(var("foo")),
            name: "bar".to_string(),
            args: ParsedArguments::default(),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let context = init(input);

        let expected_ast = stmt(StatementKind::RegularImport(vec![RegularImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            alias: None,
        }]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
def foo():
    import other, second as third
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "foo".to_string(),
            args: ParsedArgDefinitions::default(),
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
        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
import other as b
pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::RegularImport(vec![RegularImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            alias: Some("b".into()),
        }]));

        // Before we handling Token::As processing, this test would fail, but only once it began
        // parsing the next statement. We needed to parse two statements here to produce the
        // failing test.
        let mut parser = context.init_parser();
        match parser.parse_statement() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
        match parser.parse_statement() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, stmt(StatementKind::Pass)),
        }

        let input = "mypackage.myothermodule.add('1', '1')";
        let context = init(input);

        let expected_ast = Expr::MethodCall {
            object: Box::new(Expr::MemberAccess {
                object: Box::new(var("mypackage")),
                field: "myothermodule".into(),
            }),
            name: "add".into(),
            args: ParsedArguments {
                args: vec![
                    Expr::StringLiteral("1".into()),
                    Expr::StringLiteral("1".into()),
                ],
                kwargs: vec![],
                args_var: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"
cls._abc_registry.add(subclass)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::MethodCall {
            object: Box::new(Expr::MemberAccess {
                object: Box::new(var("cls")),
                field: "_abc_registry".into(),
            }),
            name: "add".into(),
            args: ParsedArguments {
                args: vec![var("subclass")],
                kwargs: vec![],
                args_var: None,
            },
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn selective_import() {
        let input = "from other import something";
        let context = init(input);

        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![ImportedItem::Direct("something".to_string())],
            wildcard: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from other import something, something_else";
        let context = init(input);

        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![
                ImportedItem::Direct("something".to_string()),
                ImportedItem::Direct("something_else".to_string()),
            ],
            wildcard: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from other import *";
        let context = init(input);

        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Absolute(vec!["other".into()]),
            items: vec![],
            wildcard: true,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from other import something, something_else as imported_name";
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from other.module import something, something_else as imported_name";
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from . import something";
        let context = init(input);

        let expected_ast = stmt(StatementKind::SelectiveImport {
            import_path: ImportPath::Relative(0, vec![]),
            items: vec![ImportedItem::Direct("something".to_string())],
            wildcard: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from .other.module import something, something_else as imported_name";
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "from ..other.module import something, something_else as imported_name";
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
from ..other.module import (something,
                            something_else as imported_name)
"#;
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
from ..other.module import (something as imported_name,
                            something_else)
"#;
        let context = init(input);

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

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn floating_point() {
        let input = r#"
a = 3.14
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::FloatingPoint(3.14),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
b = a + 2.5e-3
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("b"),
            right: Expr::BinaryOperation {
                left: Box::new(var("a")),
                op: BinOp::Add,
                right: Box::new(Expr::FloatingPoint(2.5e-3)),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn negative_numbers() {
        let input = "-3.14";
        let context = init(input);
        let expected_ast = Expr::FloatingPoint(-3.14);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "-3";
        let context = init(input);
        let expected_ast = int(-3);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "2 - 3";
        let context = init(input);
        let expected_ast = Expr::BinaryOperation {
            left: Box::new(int(2)),
            op: BinOp::Sub,
            right: Box::new(int(3)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "-2e-3";
        let context = init(input);
        let expected_ast = Expr::FloatingPoint(-2e-3);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "2 + -3";
        let context = init(input);
        let expected_ast = Expr::BinaryOperation {
            left: Box::new(int(2)),
            op: BinOp::Add,
            right: Box::new(int(-3)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "-(3)";
        let context = init(input);
        let expected_ast = Expr::UnaryOperation {
            op: UnaryOp::Minus,
            right: Box::new(int(3)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "+(3)";
        let context = init(input);
        let expected_ast = Expr::UnaryOperation {
            op: UnaryOp::Plus,
            right: Box::new(int(3)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "-(2 + 3)";
        let context = init(input);
        let expected_ast = Expr::UnaryOperation {
            op: UnaryOp::Minus,
            right: Box::new(Expr::BinaryOperation {
                left: Box::new(int(2)),
                op: BinOp::Add,
                right: Box::new(int(3)),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn lists() {
        let input = "[1,2,3]";
        let context = init(input);
        let expected_ast = Expr::List(vec![int(1), Expr::Integer(2), Expr::Integer(3)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "[1, 2, 3]";
        let context = init(input);
        let expected_ast = Expr::List(vec![int(1), Expr::Integer(2), Expr::Integer(3)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"
a = [1,
    2,
    3
]"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::List(vec![int(1), Expr::Integer(2), Expr::Integer(3)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
a = [1, 2, 3]
";
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::List(vec![int(1), Expr::Integer(2), Expr::Integer(3)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "list([1, 2, 3])";
        let context = init(input);
        let expected_ast = Expr::ClassInstantiation {
            name: "list".to_string(),
            args: ParsedArguments {
                args: vec![Expr::List(vec![int(1), int(2), int(3)])],
                kwargs: vec![],
                args_var: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn sets() {
        let input = "{1,2,3}";
        let context = init(input);
        let expected_ast = Expr::Set(HashSet::from([int(1), int(2), int(3)]));

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "{1, 2, 3}";
        let context = init(input);
        let expected_ast = Expr::Set(HashSet::from([int(1), int(2), int(3)]));

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a = {1, 2, 3}";
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::Set(HashSet::from([int(1), int(2), int(3)])),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "set({1, 2, 3})";
        let context = init(input);
        let expected_ast = Expr::ClassInstantiation {
            name: "set".to_string(),
            args: ParsedArguments {
                args: vec![Expr::Set(HashSet::from([int(1), int(2), int(3)]))],
                kwargs: vec![],
                args_var: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"
{
    1,
    2,
    3
}"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::Expression(Expr::Set(HashSet::from([
            int(1),
            int(2),
            int(3),
        ]))));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
{
    1,
    2,
    3,
}"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::Expression(Expr::Set(HashSet::from([
            int(1),
            int(2),
            int(3),
        ]))));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn tuples() {
        let input = "(1,2,3)";
        let context = init(input);
        let expected_ast = Expr::Tuple(vec![int(1), Expr::Integer(2), Expr::Integer(3)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "(1, 2, 3)";
        let context = init(input);
        let expected_ast = Expr::Tuple(vec![int(1), Expr::Integer(2), Expr::Integer(3)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "1, 2, 3";
        let context = init(input);
        let expected_ast = Expr::Tuple(vec![int(1), Expr::Integer(2), Expr::Integer(3)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "1,";
        let context = init(input);
        let expected_ast = Expr::Tuple(vec![int(1)]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a = (1, 2, 3)";
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::Tuple(vec![int(1), Expr::Integer(2), Expr::Integer(3)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "a = 1, 2, 3";
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::Tuple(vec![int(1), Expr::Integer(2), Expr::Integer(3)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "tuple((1, 2, 3))";
        let context = init(input);
        let expected_ast = Expr::ClassInstantiation {
            name: "tuple".to_string(),
            args: ParsedArguments {
                args: vec![Expr::Tuple(vec![int(1), int(2), int(3)])],
                kwargs: vec![],
                args_var: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"
tuple((1,
       2,
       3))
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::Expression(Expr::ClassInstantiation {
            name: "tuple".to_string(),
            args: ParsedArguments {
                args: vec![Expr::Tuple(vec![int(1), int(2), int(3)])],
                kwargs: vec![],
                args_var: None,
            },
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn index_access() {
        let input = "a[0]";
        let context = init(input);
        let expected_ast = Expr::IndexAccess {
            object: Box::new(var("a")),
            index: Box::new(int(0)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "[0,1][1]";
        let context = init(input);
        let expected_ast = Expr::IndexAccess {
            object: Box::new(Expr::List(vec![int(0), Expr::Integer(1)])),
            index: Box::new(int(1)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[1] = 0";
        let context = init(input);
        let expected_ast = stmt(StatementKind::Assignment {
            left: Expr::IndexAccess {
                object: Box::new(var("a")),
                index: Box::new(int(1)),
            },
            right: int(0),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
for i in a:
    print(i)
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var("a"),
            body: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                name: "print".into(),
                args: ParsedArguments {
                    args: vec![var("i")],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            }))],
            else_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
for k, v in a.items():
    print(v)
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Tuple(vec!["k".into(), "v".into()]),
            iterable: Expr::MethodCall {
                object: Box::new(var("a")),
                name: "items".into(),
                args: ParsedArguments::default(),
            },
            body: ast![stmt(StatementKind::Expression(Expr::FunctionCall {
                name: "print".into(),
                args: ParsedArguments {
                    args: vec![var("v")],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            }))],
            else_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn list_comprehension() {
        let input = "[ i * 2 for i in a ]";
        let context = init(input);
        let expected_ast = Expr::ListComprehension {
            body: Box::new(Expr::BinaryOperation {
                left: Box::new(var("i")),
                op: BinOp::Mul,
                right: Box::new(int(2)),
            }),
            clauses: vec![ForClause {
                indices: vec!["i".to_string()],
                iterable: var("a"),
                condition: None,
            }],
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "[i*2 for i in a if True]";
        let context = init(input);
        let expected_ast = Expr::ListComprehension {
            body: Box::new(Expr::BinaryOperation {
                left: Box::new(var("i")),
                op: BinOp::Mul,
                right: Box::new(int(2)),
            }),
            clauses: vec![ForClause {
                indices: vec!["i".to_string()],
                iterable: var("a"),
                condition: Some(Expr::Boolean(true)),
            }],
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn generators() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n - 1
"#;
        let context = init(input);

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => {
                let expected_ast = stmt(StatementKind::FunctionDef {
                    name: "countdown".to_string(),
                    args: ParsedArgDefinitions {
                        args: vec![ParsedArgDefinition {
                            arg: "n".into(),
                            default: None,
                        }],
                        args_var: None,
                        kwargs_var: None,
                    },
                    body: ast![stmt(StatementKind::WhileLoop {
                        condition: Expr::BinaryOperation {
                            left: Box::new(var("n")),
                            op: BinOp::GreaterThan,
                            right: Box::new(int(0)),
                        },
                        body: ast![
                            stmt(StatementKind::Expression(Expr::Yield(Some(Box::new(var(
                                "n"
                            )))))),
                            stmt(StatementKind::Assignment {
                                left: var("n"),
                                right: Expr::BinaryOperation {
                                    left: Box::new(var("n")),
                                    op: BinOp::Sub,
                                    right: Box::new(int(1)),
                                },
                            }),
                        ],
                    })],
                    decorators: vec![],
                    is_async: false,
                });

                assert_stmt_eq!(ast, expected_ast)
            }
        }

        let input = r#"
yield from a
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::YieldFrom(Box::new(var(
            "a",
        )))));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn inheritance() {
        let input = r#"
class Foo(Parent):
    def __init__(self):
        self.x = 0
"#;
        let context = init(input);

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => {
                let expected_parent = vec![var("Parent")];

                let StatementKind::ClassDef { parents, .. } = ast.kind else {
                    panic!("Expected a class def!")
                };
                assert_eq!(parents, expected_parent)
            }
        }

        let input = r#"
class Foo(metaclass=Parent):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![],
            metaclass: Some("Parent".to_string()),
            body: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
class Foo(Bar, metaclass=Parent):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "Foo".to_string(),
            parents: vec![var("Bar")],
            metaclass: Some("Parent".to_string()),
            body: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
class InterfaceMeta(type):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ClassDef {
            name: "InterfaceMeta".to_string(),
            parents: vec![var("type")],
            metaclass: None,
            body: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn dictionaries() {
        let input = r#"
a = { "b": 4, 'c': 5 }
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::Dict(vec![
                DictOperation::Pair(Expr::StringLiteral("b".to_string()), int(4)),
                DictOperation::Pair(Expr::StringLiteral("c".to_string()), int(5)),
            ]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
namespace = {
    '__name__': 4,
}
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("namespace"),
            right: Expr::Dict(vec![DictOperation::Pair(
                Expr::StringLiteral("__name__".to_string()),
                int(4),
            )]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"{ **first, **second }"#;
        let context = init(input);

        let expected_ast = Expr::Dict(vec![
            DictOperation::Unpack(var("first")),
            DictOperation::Unpack(var("second")),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"{ **first, **second, }"#;
        let context = init(input);

        let expected_ast = Expr::Dict(vec![
            DictOperation::Unpack(var("first")),
            DictOperation::Unpack(var("second")),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"{ 2, **second }"#;
        let context = init(input);

        match context.parse_oneshot::<Expr>() {
            Err(e) => assert_eq!(e, ParserError::SyntaxError),
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"{ 2, **second, }"#;
        let context = init(input);

        match context.parse_oneshot::<Expr>() {
            Err(e) => assert_eq!(e, ParserError::SyntaxError),
            Ok(_) => panic!("Expected an error!"),
        }

        let input = r#"{ key: val * 2 for key, val in d }"#;
        let context = init(input);

        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec!["key".to_string(), "val".to_string()],
                iterable: var("d"),
                condition: None,
            }],
            key_body: Box::new(var("key")),
            value_body: Box::new(Expr::BinaryOperation {
                left: Box::new(var("val")),
                op: BinOp::Mul,
                right: Box::new(int(2)),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"{ key: val * 2 for (key, val) in d }"#;
        let context = init(input);

        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec!["key".to_string(), "val".to_string()],
                iterable: var("d"),
                condition: None,
            }],
            key_body: Box::new(var("key")),
            value_body: Box::new(Expr::BinaryOperation {
                left: Box::new(var("val")),
                op: BinOp::Mul,
                right: Box::new(int(2)),
            }),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn async_await() {
        let input = r#"
async def main():
    task_1 = asyncio.create_task(task1())
    return await task_1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "main".to_string(),
            args: ParsedArgDefinitions::default(),
            body: ast![
                stmt(StatementKind::Assignment {
                    left: var("task_1"),
                    right: Expr::MethodCall {
                        object: Box::new(var("asyncio")),
                        name: "create_task".to_string(),
                        args: ParsedArguments {
                            args: vec![Expr::FunctionCall {
                                name: "task1".to_string(),
                                args: ParsedArguments::default(),
                                callee: None,
                            }],
                            kwargs: vec![],
                            args_var: None,
                        },
                    },
                }),
                stmt(StatementKind::Return(vec![Expr::Await {
                    right: Box::new(var("task_1"))
                }])),
            ],
            decorators: vec![],
            is_async: true,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assert(Expr::Boolean(true)));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
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
        let context = init(input);

        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(Expr::BinaryOperation {
                left: Box::new(int(4)),
                op: BinOp::Div,
                right: Box::new(int(0)),
            }))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![],
                alias: None,
                block: ast![stmt(StatementKind::Assignment {
                    left: var("a"),
                    right: int(2)
                })],
            }],
            else_block: None,
            finally_block: Some(ast![stmt(StatementKind::Assignment {
                left: var("a"),
                right: int(3)
            })]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
try:
    4 / 0
except ZeroDivisionError as e:
    a = 2
finally:
    a = 3
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(Expr::BinaryOperation {
                left: Box::new(int(4)),
                op: BinOp::Div,
                right: Box::new(int(0)),
            }))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![ExceptionLiteral::ZeroDivisionError],
                alias: Some("e".into()),
                block: ast![stmt(StatementKind::Assignment {
                    left: var("a"),
                    right: int(2)
                })],
            }],
            else_block: None,
            finally_block: Some(ast![stmt(StatementKind::Assignment {
                left: var("a"),
                right: int(3)
            })]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
try:
    4 / 0
except (ZeroDivisionError, IOError) as e:
    a = 2
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(Expr::BinaryOperation {
                left: Box::new(int(4)),
                op: BinOp::Div,
                right: Box::new(int(0)),
            }))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![
                    ExceptionLiteral::ZeroDivisionError,
                    ExceptionLiteral::IOError,
                ],
                alias: Some("e".into()),
                block: ast![stmt(StatementKind::Assignment {
                    left: var("a"),
                    right: int(2)
                })],
            }],
            else_block: None,
            finally_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

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
        let context = init(input);

        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Expression(Expr::BinaryOperation {
                left: Box::new(int(4)),
                op: BinOp::Div,
                right: Box::new(int(0)),
            }))],
            except_clauses: vec![ExceptClause {
                exception_types: vec![ExceptionLiteral::ZeroDivisionError],
                alias: Some("e".into()),
                block: ast![stmt(StatementKind::Assignment {
                    left: var("a"),
                    right: int(2)
                })],
            }],
            else_block: Some(ast![stmt(StatementKind::Assignment {
                left: var("a"),
                right: int(4)
            })]),
            finally_block: Some(ast![stmt(StatementKind::Assignment {
                left: var("a"),
                right: int(3)
            })]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
try:
    pass
except:
    return
a = 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::TryExcept {
            try_block: ast![stmt(StatementKind::Pass)],
            except_clauses: vec![ExceptClause {
                exception_types: vec![],
                alias: None,
                block: ast![stmt(StatementKind::Return(vec![]))],
            }],
            else_block: None,
            finally_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn binary_literal() {
        let input = r#"
a = 0b0010
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: int(2),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn octal_literal() {
        let input = r#"
a = 0o0010
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: int(8),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn hex_literal() {
        let input = r#"
a = 0x0010
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: int(16),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"
def test_args(*args):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_args".into(),
            args: ParsedArgDefinitions {
                args: vec![],
                args_var: Some("args".into()),
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
def test_args(*args, **kwargs):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_args".into(),
            args: ParsedArgDefinitions {
                args: vec![],
                args_var: Some("args".into()),
                kwargs_var: Some("kwargs".into()),
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
def test_kwargs(**kwargs):
    print(kwargs['a'])
"#;
        let context = init(input);

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => {
                let expected_args = ParsedArgDefinitions {
                    args: vec![],
                    args_var: None,
                    kwargs_var: Some("kwargs".into()),
                };
                let StatementKind::FunctionDef { args, .. } = ast.kind else {
                    panic!("Expected function def")
                };
                assert_eq!(expected_args, args)
            }
        }

        let input = r#"
def test_default(file=None):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "test_default".into(),
            args: ParsedArgDefinitions {
                args: vec![ParsedArgDefinition {
                    arg: "file".into(),
                    default: Some(Expr::None),
                }],
                args_var: None,
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(a=1, b=2)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int(1)),
                    KwargsOperation::Pair("b".into(), int(2)),
                ],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(**{'a':1, 'b':2})
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int(1)),
                    KwargsOperation::Pair("b".into(), int(2)),
                ],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(**{'a':1, 'b':2}, **{'c': 3})
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair("a".into(), int(1)),
                    KwargsOperation::Pair("b".into(), int(2)),
                    KwargsOperation::Pair("c".into(), int(3)),
                ],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(**first, **second)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Unpacking(var("first")),
                    KwargsOperation::Unpacking(var("second")),
                ],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(**kwargs)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var("kwargs"))],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(*args)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![],
                args_var: Some(Box::new(var("args"))),
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_kwargs(*args, **kwargs)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "test_kwargs".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var("kwargs"))],
                args_var: Some(Box::new(var("args"))),
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
deprecated("collections.abc.ByteString",
)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "deprecated".into(),
            args: ParsedArguments {
                args: vec![Expr::StringLiteral("collections.abc.ByteString".into())],
                kwargs: vec![],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn decorator() {
        let input = r#"
@test_decorator
def get_val():
    return 2
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "get_val".into(),
            args: ParsedArgDefinitions::default(),
            body: ast![stmt(StatementKind::Return(vec![int(2)]))],
            decorators: vec![var("test_decorator")],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
test_decorator(get_val_undecorated)()
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "<anonymous_from_callee>".into(),
            args: ParsedArguments {
                args: vec![],
                kwargs: vec![],
                args_var: None,
            },
            callee: Some(Box::new(Expr::FunctionCall {
                name: "test_decorator".into(),
                args: ParsedArguments {
                    args: vec![var("get_val_undecorated")],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            })),
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn raise() {
        let input = r#"
raise Exception
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: ParsedArguments::default(),
        })));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
raise Exception("message")
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: ParsedArguments {
                args: vec![Expr::StringLiteral("message".into())],
                kwargs: vec![],
                args_var: None,
            },
        })));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
raise
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Raise(None));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
raise Exception("message") from None
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Raise(Some(ExceptionInstance {
            literal: ExceptionLiteral::Exception,
            args: ParsedArguments {
                args: vec![Expr::StringLiteral("message".into())],
                kwargs: vec![],
                args_var: None,
            },
        })));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn context_manager() {
        let input = r#"
with open('test.txt') as f:
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ContextManager {
            expr: Expr::FunctionCall {
                name: "open".into(),
                args: ParsedArguments {
                    args: vec![Expr::StringLiteral("test.txt".into())],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            },
            variable: Some("f".into()),
            block: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
with open('test.txt'):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::ContextManager {
            expr: Expr::FunctionCall {
                name: "open".into(),
                args: ParsedArguments {
                    args: vec![Expr::StringLiteral("test.txt".into())],
                    kwargs: vec![],
                    args_var: None,
                },
                callee: None,
            },
            variable: None,
            block: ast![stmt(StatementKind::Pass)],
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn type_alias() {
        let input = r#"
a = list[int]
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::TypeNode(TypeNode::Generic {
                base_type: "list".into(),
                parameters: vec![TypeNode::Basic("int".into())],
            }),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
u = int | str
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("u"),
            right: Expr::TypeNode(TypeNode::Union(vec![
                TypeNode::Basic("int".into()),
                TypeNode::Basic("str".into()),
            ])),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn delete() {
        let input = r#"
del a
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Delete(vec![var("a")]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
del a, b, c
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Delete(vec![var("a"), var("b"), var("c")]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn byte_string() {
        let input = r#"
a = b'hello'
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::ByteStringLiteral("hello".into()),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn compound_operator() {
        let input = r#"
a += 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Add,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a -= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Subtract,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a *= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Multiply,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a /= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Divide,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a &= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseAnd,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a |= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseOr,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a ^= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseXor,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a //= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::IntegerDiv,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a <<= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::LeftShift,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a >>= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::RightShift,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a %= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Mod,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a @= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::MatMul,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a **= 1
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Expo,
            target: Box::new(var("a")),
            value: Box::new(int(1)),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "~a";
        let context = init(input);
        let expected_ast = Expr::UnaryOperation {
            op: UnaryOp::BitwiseNot,
            right: Box::new(var("a")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a % b";
        let context = init(input);
        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::Mod,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a @ b";
        let context = init(input);
        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::MatMul,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn f_strings() {
        let input = r#"f"Hello {name}.""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(".".into()),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"{first}{last}""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("first")),
                format: FormatOption::Str,
            }),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("last")),
                format: FormatOption::Str,
            }),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"Hello""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![FStringPart::String("Hello".into())]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"Hello {name} goodbye {other}""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("other")),
                format: FormatOption::Str,
            }),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"Age: {num + 1}""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::String("Age: ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(Expr::BinaryOperation {
                    left: Box::new(var("num")),
                    op: BinOp::Add,
                    right: Box::new(int(1)),
                }),
                format: FormatOption::Str,
            }),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"environ({{{formatted_items}}})""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::String("environ({".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("formatted_items")),
                format: FormatOption::Str,
            }),
            FStringPart::String("})".into()),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = r#"f"Hello {name!r} goodbye {other}""#;
        let context = init(input);

        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("name")),
                format: FormatOption::Repr,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var("other")),
                format: FormatOption::Str,
            }),
        ]);

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn binary_operators() {
        let input = "a & b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::BitwiseAnd,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a | b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::BitwiseOr,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a ^ b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::BitwiseXor,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a << b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::LeftShift,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a >> b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::RightShift,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a ** b";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(var("a")),
            op: BinOp::Expo,
            right: Box::new(var("b")),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "2 * 3 << 2 + 4 & 205";
        let context = init(input);

        let expected_ast = Expr::BinaryOperation {
            left: Box::new(Expr::BinaryOperation {
                left: Box::new(Expr::BinaryOperation {
                    left: Box::new(int(2)),
                    op: BinOp::Mul,
                    right: Box::new(int(3)),
                }),
                op: BinOp::LeftShift,
                right: Box::new(Expr::BinaryOperation {
                    left: Box::new(int(2)),
                    op: BinOp::Add,
                    right: Box::new(int(4)),
                }),
            }),
            op: BinOp::BitwiseAnd,
            right: Box::new(int(205)),
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn control_flow() {
        let input = r#"
for i in a:
    break
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var("a"),
            body: ast![stmt(StatementKind::Break)],
            else_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
for i in a:
    continue
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var("a"),
            body: ast![stmt(StatementKind::Continue)],
            else_block: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
for i in a:
    break
else:
    pass
"#;
        let context = init(input);
        let expected_ast = stmt(StatementKind::ForInLoop {
            index: LoopIndex::Variable("i".into()),
            iterable: var("a"),
            body: ast![stmt(StatementKind::Break)],
            else_block: Some(ast![stmt(StatementKind::Pass)]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn type_hints() {
        let input = "
def add(x: str, y: str) -> str:
    return x + y
";
        let context = init(input);

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => {
                let expected_args = ParsedArgDefinitions {
                    args: vec![
                        ParsedArgDefinition {
                            arg: "x".into(),
                            default: None,
                        },
                        ParsedArgDefinition {
                            arg: "y".into(),
                            default: None,
                        },
                    ],
                    args_var: None,
                    kwargs_var: None,
                };

                let StatementKind::FunctionDef { args, .. } = ast.kind else {
                    panic!("Expected function def!")
                };

                assert_eq!(args, expected_args)
            }
        }
    }

    #[test]
    fn slices() {
        let input = "a[1:1:1]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: Some(Box::new(int(1))),
                stop: Some(Box::new(int(1))),
                step: Some(Box::new(int(1))),
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[2:5]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: Some(Box::new(int(2))),
                stop: Some(Box::new(int(5))),
                step: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[:5]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: None,
                stop: Some(Box::new(int(5))),
                step: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[3:]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: Some(Box::new(int(3))),
                stop: None,
                step: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[::2]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: None,
                stop: None,
                step: Some(Box::new(int(2))),
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "a[:]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("a")),
            params: ParsedSliceParams {
                start: None,
                stop: None,
                step: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }

        let input = "new_bases[i+shift:shift+1]";
        let context = init(input);
        let expected_ast = Expr::SliceOperation {
            object: Box::new(var("new_bases")),
            params: ParsedSliceParams {
                start: Some(Box::new(Expr::BinaryOperation {
                    left: Box::new(var("i")),
                    op: BinOp::Add,
                    right: Box::new(var("shift")),
                })),
                stop: Some(Box::new(Expr::BinaryOperation {
                    left: Box::new(var("shift")),
                    op: BinOp::Add,
                    right: Box::new(int(1)),
                })),
                step: None,
            },
        };

        match context.parse_oneshot::<Expr>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_eq!(ast, expected_ast),
        }
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
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "outer".into(),
            args: ParsedArgDefinitions::default(),
            body: ast![
                stmt(StatementKind::Assignment {
                    left: var("a"),
                    right: int(1)
                }),
                stmt(StatementKind::Assignment {
                    left: var("b"),
                    right: int(2)
                }),
                stmt(StatementKind::FunctionDef {
                    name: "inner".into(),
                    args: ParsedArgDefinitions::default(),
                    body: ast![
                        stmt(StatementKind::Assignment {
                            left: var("b"),
                            right: int(3)
                        }),
                        stmt(StatementKind::Expression(Expr::FunctionCall {
                            name: "print".into(),
                            args: ParsedArguments {
                                args: vec![var("a")],
                                kwargs: vec![],
                                args_var: None,
                            },
                            callee: None,
                        })),
                    ],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn scope_modifiers() {
        let input = "
nonlocal var
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Nonlocal(vec!["var".into()]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
nonlocal var, var2
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Nonlocal(vec!["var".into(), "var2".into()]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
global var
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Global(vec!["var".into()]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = "
global var, var2
";
        let context = init(input);

        let expected_ast = stmt(StatementKind::Global(vec!["var".into(), "var2".into()]));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn ternary_operation() {
        let input = r#"
a = 4 if True else 5
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::TernaryOp {
                condition: Box::new(Expr::Boolean(true)),
                if_value: Box::new(int(4)),
                else_value: Box::new(int(5)),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a = 4 + x if b == 6 else 5 << 2
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::TernaryOp {
                condition: Box::new(Expr::BinaryOperation {
                    left: Box::new(var("b")),
                    op: BinOp::Equals,
                    right: Box::new(int(6)),
                }),
                if_value: Box::new(Expr::BinaryOperation {
                    left: Box::new(int(4)),
                    op: BinOp::Add,
                    right: Box::new(var("x")),
                }),
                else_value: Box::new(Expr::BinaryOperation {
                    left: Box::new(int(5)),
                    op: BinOp::LeftShift,
                    right: Box::new(int(2)),
                }),
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn more_tokens() {
        let input = r#"
Ellipsis
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Ellipsis));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn generator_comprehension() {
        let input = r#"
a = (i * 2 for i in b)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Assignment {
            left: var("a"),
            right: Expr::GeneratorComprehension {
                body: Box::new(Expr::BinaryOperation {
                    left: Box::new(var("i")),
                    op: BinOp::Mul,
                    right: Box::new(int(2)),
                }),
                clauses: vec![ForClause {
                    indices: vec!["i".into()],
                    iterable: var("b"),
                    condition: None,
                }],
            },
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
foo(i * 2 for i in b)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "foo".to_string(),
            args: ParsedArguments {
                args: vec![Expr::GeneratorComprehension {
                    body: Box::new(Expr::BinaryOperation {
                        left: Box::new(var("i")),
                        op: BinOp::Mul,
                        right: Box::new(int(2)),
                    }),
                    clauses: vec![ForClause {
                        indices: vec!["i".into()],
                        iterable: var("b"),
                        condition: None,
                    }],
                }],
                kwargs: vec![],
                args_var: None,
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn default_args() {
        let input = r#"
def foo(data=None):
    pass
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::FunctionDef {
            name: "foo".into(),
            args: ParsedArgDefinitions {
                args: vec![ParsedArgDefinition {
                    arg: "data".into(),
                    default: Some(Expr::None),
                }],
                args_var: None,
                kwargs_var: None,
            },
            body: ast![stmt(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn unpacking() {
        let input = r#"
(*l,)
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::Tuple(vec![
            Expr::UnaryOperation {
                op: UnaryOp::Unpack,
                right: Box::new(var("l")),
            },
        ])));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
foo(a, *b[1:])
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::Expression(Expr::FunctionCall {
            name: "foo".into(),
            args: ParsedArguments {
                args: vec![var("a")],
                kwargs: vec![],
                args_var: Some(Box::new(Expr::SliceOperation {
                    object: Box::new(var("b")),
                    params: ParsedSliceParams {
                        start: Some(Box::new(int(1))),
                        stop: None,
                        step: None,
                    },
                })),
            },
            callee: None,
        }));

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
if True:
    a, b = b, a
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::IfElse {
            if_part: ConditionalBlock {
                condition: Expr::Boolean(true),
                block: ast![stmt(StatementKind::UnpackingAssignment {
                    left: vec![var("a"), var("b")],
                    right: Expr::Tuple(vec![var("b"), var("a"),]),
                })],
            },
            elif_parts: vec![],
            else_part: None,
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }

        let input = r#"
a, = b,
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::UnpackingAssignment {
            left: vec![var("a")],
            right: Expr::Tuple(vec![var("b")]),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }

    #[test]
    fn multiple_assignment() {
        let input = r#"
a = b = True
"#;
        let context = init(input);

        let expected_ast = stmt(StatementKind::MultipleAssignment {
            left: vec![var("a"), var("b")],
            right: Expr::Boolean(true),
        });

        match context.parse_oneshot::<Statement>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => assert_stmt_eq!(ast, expected_ast),
        }
    }
}
