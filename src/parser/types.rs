use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    parser::static_analysis::{FunctionAnalysisVisitor, Visitor},
    treewalk::types::utils::Dunder,
    types::errors::{InterpreterError, ParserError},
};

use super::Parser;

/// There are a handful of places where we reference a variable and it must be a variable name
/// only, not an expression. There is nothing to resolve or evaluate on these Using [`String`]
/// here works, but we create a [`Variable`] to be more expressive and add type-safety.
pub type Variable = String;

#[derive(Clone, PartialEq, Debug)]
pub enum DictOperation {
    Pair(Expr, Expr),
    Unpack(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeNode {
    Generic {
        base_type: String,
        parameters: Vec<TypeNode>,
    },
    Union(Vec<TypeNode>),
    Basic(String),
}

/// The three conversion modes supported in Python f-strings. These are specified by !s (the
/// default), !r, and !a, respectively.
/// Reference: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
#[derive(Debug, PartialEq, Clone)]
pub enum FormatOption {
    Str,
    Repr,
    Ascii,
}

/// A container for an [`Expr`] inside braces in an f-string and an optional conversion identifier
/// `FormatOption`. It's not optional in this struct because the parser defaults to
/// `FormatOption::Str`.
#[derive(Debug, PartialEq, Clone)]
pub struct ExprFormat {
    pub expr: Box<Expr>,
    pub format: FormatOption,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FStringPart {
    String(String),
    Expr(ExprFormat),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RegularImport {
    pub import_path: ImportPath,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ImportPath {
    Absolute(Vec<String>),
    /// Any import path which _starts_ with a dot. The `usize` represents the number of levels up
    /// to look for the path. This means that this example is in the same directory.
    ///
    /// ```python
    /// from .package.module import symbol
    /// ```
    ///
    /// While this would look one directory up
    ///
    /// ```python
    /// from ..package.module import symbol
    /// ```
    Relative(usize, Vec<String>),
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.as_str())
    }
}

impl ImportPath {
    pub fn as_str(&self) -> String {
        match self {
            ImportPath::Absolute(path) => path.join("."),
            ImportPath::Relative(levels, path) => ".".repeat(*levels) + &path.join("."),
        }
    }

    pub fn segments(&self) -> Vec<String> {
        match self {
            ImportPath::Absolute(path) => path.clone(),
            ImportPath::Relative(_, path) => path.clone(),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntegerDiv,
    In,
    NotIn,
    Is,
    IsNot,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equals,
    NotEquals,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Mod,
    MatMul,
    Expo,
}

impl TryFrom<&BinOp> for Dunder {
    type Error = ();

    fn try_from(op: &BinOp) -> Result<Self, Self::Error> {
        match op {
            BinOp::Equals => Ok(Dunder::Eq),
            BinOp::NotEquals => Ok(Dunder::Ne),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    Minus,
    Plus,
    BitwiseNot,
    Unpack,     // single asterisk *
    DictUnpack, // double asterisk **
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum LogicalOp {
    And,
    Or,
}

/// A `FunctionContext` is needed to determine what variables are accessed from an outer scope as
/// part of a closure.
#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    free_vars: Vec<Variable>,
}

impl Closure {
    pub fn new(free_vars: Vec<Variable>) -> Self {
        Self { free_vars }
    }

    pub fn get_free_vars(&self) -> Vec<Variable> {
        self.free_vars.clone()
    }
}

impl From<FunctionAnalysisVisitor> for Closure {
    fn from(visitor: FunctionAnalysisVisitor) -> Self {
        Self::new(visitor.get_free_vars())
    }
}

/// An individual function argument and its optional default.
#[derive(Clone, PartialEq, Debug)]
pub struct ParsedArgDefinition {
    pub arg: Variable,
    pub default: Option<Expr>,
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParsedArgDefinitions {
    /// The variables for all the positional arguments.
    /// ```python
    /// def foo(a, b):
    ///     ...
    /// ```
    pub args: Vec<ParsedArgDefinition>,

    /// An optional variable to hold arguments passed in for variable arity.
    /// ```python
    /// def foo(*args):
    ///     ...
    /// ```
    pub args_var: Option<Variable>,

    /// An optional variable to hold arguments passed in by keyword.
    /// ```python
    /// def foo(**kwargs):
    ///     ...
    /// ```
    pub kwargs_var: Option<Variable>,
}

pub enum ParsedArgument {
    Keyword { arg: Variable, expr: Expr },
    Positional(Expr),
}

#[derive(Clone, PartialEq, Debug)]
// This is similar to DictOperation, but where the keys are required to be strings. This matches
// the Python rules and simplifies usage.
pub enum KwargsOperation {
    Unpacking(Expr),    // Represents **kwargs_var
    Pair(String, Expr), // Represents a key-value pair
}

/// All the information provided when a function is called (besides of the name of the function).
#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParsedArguments {
    /// Any args passed in positionally.
    /// ```python
    /// foo(1, 2)
    /// ```
    pub args: Vec<Expr>,

    /// Any keyword arguments passed in as literals or variables. For example,
    /// ```python
    /// foo(a=1, b=2)
    /// foo(**{'a': 1, 'b': 2})
    /// foo(**kwargs)
    /// ```
    pub kwargs: Vec<KwargsOperation>,

    /// Any variable-arity arguments passed in through a variable. For example,
    /// ```python
    /// args = [1, 2]
    /// foo(**args)
    /// ```
    /// The `Expr` here references a variable which will be read during the interpreter stage.
    pub args_var: Option<Box<Expr>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParsedSliceParams {
    pub start: Option<Box<Expr>>,
    pub stop: Option<Box<Expr>>,
    pub step: Option<Box<Expr>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ForClause {
    pub indices: Vec<String>,
    pub iterable: Box<Expr>,
    pub condition: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    None,
    NotImplemented,
    Ellipsis,
    Integer(i64),
    FloatingPoint(f64),
    Boolean(bool),
    Variable(String),
    StringLiteral(String),
    ByteStringLiteral(Vec<u8>),
    List(Vec<Expr>),
    Set(HashSet<Expr>),
    Dict(Vec<DictOperation>),
    Tuple(Vec<Expr>),
    FString(Vec<FStringPart>),
    Yield(Option<Box<Expr>>),
    YieldFrom(Box<Expr>),
    BinaryOperation {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnaryOperation {
        op: UnaryOp,
        right: Box<Expr>,
    },
    LogicalOperation {
        left: Box<Expr>,
        op: LogicalOp,
        right: Box<Expr>,
    },
    TernaryOp {
        condition: Box<Expr>,
        if_value: Box<Expr>,
        else_value: Box<Expr>,
    },
    MemberAccess {
        object: Box<Expr>,
        field: String,
    },
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    SliceOperation {
        object: Box<Expr>,
        params: ParsedSliceParams,
    },
    Await {
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: ParsedArguments,
        callee: Option<Box<Expr>>,
    },
    ClassInstantiation {
        name: String,
        args: ParsedArguments,
    },
    MethodCall {
        object: Box<Expr>,
        name: String,
        args: ParsedArguments,
    },
    GeneratorComprehension {
        clauses: Vec<ForClause>,
        body: Box<Expr>,
    },
    ListComprehension {
        clauses: Vec<ForClause>,
        body: Box<Expr>,
    },
    SetComprehension {
        clauses: Vec<ForClause>,
        body: Box<Expr>,
    },
    DictComprehension {
        clauses: Vec<ForClause>,
        key_body: Box<Expr>,
        value_body: Box<Expr>,
    },
    Lambda {
        args: Box<ParsedArgDefinitions>,
        expr: Box<Expr>,
    },
    TypeNode(TypeNode),
}

impl Expr {
    pub fn as_variable(&self) -> Option<String> {
        match self {
            Expr::Variable(name) => Some(name.to_string()),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            Expr::StringLiteral(name) => Some(name.to_string()),
            _ => None,
        }
    }
}

// For some reason, we have to create this here for the Eq trait to be
// satisfied for f64.
impl Eq for Expr {}

// Is the empty function body going to cause weirdness on HashSet?
impl Hash for Expr {
    fn hash<H>(&self, _state: &mut H)
    where
        H: Hasher,
    {
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExceptionLiteral {
    Exception,
    ZeroDivisionError,
    IOError,
    ImportError,
    StopIteration,
    TypeError,
    AttributeError,
    NameError,
    Custom(String),
}

impl From<String> for ExceptionLiteral {
    fn from(value: String) -> Self {
        match value.as_str() {
            "ZeroDivisionError" => ExceptionLiteral::ZeroDivisionError,
            "Exception" => ExceptionLiteral::Exception,
            "IOError" => ExceptionLiteral::IOError,
            "ImportError" => ExceptionLiteral::ImportError,
            "StopIteration" => ExceptionLiteral::StopIteration,
            "TypeError" => ExceptionLiteral::TypeError,
            "AttributeError" => ExceptionLiteral::AttributeError,
            "NameError" => ExceptionLiteral::NameError,
            // TODO we don't handle ExceptionLiteral::Custom in the interpreter yet
            _ => ExceptionLiteral::Custom(value.to_owned()),
        }
    }
}

impl TryFrom<InterpreterError> for ExceptionLiteral {
    type Error = InterpreterError;

    fn try_from(value: InterpreterError) -> Result<Self, Self::Error> {
        match value {
            InterpreterError::DivisionByZero(..) => Ok(ExceptionLiteral::ZeroDivisionError),
            InterpreterError::ModuleNotFound(..) => Ok(ExceptionLiteral::ImportError),
            InterpreterError::TypeError(..) => Ok(ExceptionLiteral::TypeError),
            InterpreterError::AttributeError(..) => Ok(ExceptionLiteral::AttributeError),
            InterpreterError::NameError(..) => Ok(ExceptionLiteral::NameError),
            _ => Err(InterpreterError::RuntimeError),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExceptionInstance {
    pub literal: ExceptionLiteral,
    pub args: ParsedArguments,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExceptClause {
    pub exception_types: Vec<ExceptionLiteral>,
    pub alias: Option<String>,
    pub block: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ConditionalBlock {
    pub condition: Expr,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn from_expr(expr: Expr) -> Self {
        Self::new(vec![Statement::Expression(expr)])
    }

    pub fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_block(self);
        for statement in &self.statements {
            statement.accept(visitor);
        }
    }
}

/// This represents one of the comma-separated values being imported. This is only used in
/// selective imports right now.
///
/// ```python
/// from module_a import one, two as three, four
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum ImportedItem {
    Direct(String),
    Alias(Alias),
}

impl ImportedItem {
    pub fn as_imported_symbol(&self) -> String {
        match self {
            ImportedItem::Direct(name) => name.clone(),
            ImportedItem::Alias(alias) => alias.remap(),
        }
    }

    pub fn as_original_symbol(&self) -> String {
        match self {
            ImportedItem::Direct(name) => name.clone(),
            ImportedItem::Alias(alias) => alias.original(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Alias {
    pub symbol: String,
    pub alias_symbol: Option<String>,
}

impl Alias {
    fn original(&self) -> String {
        self.symbol.to_owned()
    }

    fn remap(&self) -> String {
        if let Some(s) = &self.alias_symbol {
            return s.to_string();
        }

        self.symbol.to_owned()
    }
}

/// Indicate whether a single variable or a `Tuple` of variable should be unpacked on each
/// iteration of a `for` loop.
#[derive(Debug, PartialEq, Clone)]
pub enum LoopIndex {
    /// Used when the range returns a single value.
    /// ```python
    /// for i in a:
    ///     ...
    /// ```
    Variable(String),

    /// Used when the range returns a tuple of values.
    /// ```python
    /// for k, v in a.items()
    ///     ...
    /// ```
    Tuple(Vec<String>),
}

/// Perform the listed operation before assigning the result.
#[derive(Debug, PartialEq, Clone)]
pub enum CompoundOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    IntegerDiv,
    LeftShift,
    RightShift,
    Mod,
    MatMul,
    Expo,
}

impl From<&CompoundOperator> for BinOp {
    fn from(value: &CompoundOperator) -> Self {
        match value {
            CompoundOperator::Add => BinOp::Add,
            CompoundOperator::Subtract => BinOp::Sub,
            CompoundOperator::Multiply => BinOp::Mul,
            CompoundOperator::Divide => BinOp::Div,
            CompoundOperator::BitwiseAnd => BinOp::BitwiseAnd,
            CompoundOperator::BitwiseOr => BinOp::BitwiseOr,
            CompoundOperator::BitwiseXor => BinOp::BitwiseXor,
            CompoundOperator::IntegerDiv => BinOp::IntegerDiv,
            CompoundOperator::LeftShift => BinOp::LeftShift,
            CompoundOperator::RightShift => BinOp::RightShift,
            CompoundOperator::Mod => BinOp::Mod,
            CompoundOperator::MatMul => BinOp::MatMul,
            CompoundOperator::Expo => BinOp::Expo,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Expr),
    Pass,
    Break,
    Continue,
    Assert(Expr),
    Delete(Vec<Expr>),
    Assignment {
        left: Expr,
        right: Expr,
    },
    /// This is different than `UnpackingAssignment` in that every value on the LHS is assigned the
    /// valued of the RHS.
    ///
    /// Will these two ever happen at the same time? Perhaps.
    MultipleAssignment {
        left: Vec<Expr>,
        right: Expr,
    },
    UnpackingAssignment {
        left: Vec<Expr>,
        right: Expr,
    },
    CompoundAssignment {
        operator: CompoundOperator,
        target: Box<Expr>,
        value: Box<Expr>,
    },
    FunctionDef {
        name: String,
        args: ParsedArgDefinitions,
        body: Block,
        decorators: Vec<Expr>,
        is_async: bool,
    },
    ClassDef {
        name: String,
        parents: Vec<Expr>,
        metaclass: Option<String>,
        body: Block,
    },
    Return(Vec<Expr>),
    Nonlocal(Vec<Variable>),
    Global(Vec<Variable>),
    IfElse {
        if_part: ConditionalBlock,
        elif_parts: Vec<ConditionalBlock>,
        else_part: Option<Block>,
    },
    WhileLoop {
        condition: Expr,
        body: Block,
    },
    ForInLoop {
        index: LoopIndex,
        iterable: Expr,
        body: Block,
        else_block: Option<Block>,
    },
    RegularImport(Vec<RegularImport>),
    SelectiveImport {
        import_path: ImportPath,
        items: Vec<ImportedItem>,
        wildcard: bool,
    },
    TryExcept {
        try_block: Block,
        except_clauses: Vec<ExceptClause>,
        else_block: Option<Block>,
        finally_block: Option<Block>,
    },
    Raise(Option<ExceptionInstance>),
    ContextManager {
        expr: Expr,
        variable: Option<String>,
        block: Block,
    },
}

impl Statement {
    /// Visit this statement, then walk the AST to any nested blocks.
    pub fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_statement(self);

        match self {
            Statement::FunctionDef { body, .. } => {
                body.accept(visitor);
            }
            Statement::WhileLoop { body, .. } => {
                body.accept(visitor);
            }
            Statement::ForInLoop { body, .. } => {
                body.accept(visitor);
            }
            Statement::ContextManager { block, .. } => {
                block.accept(visitor);
            }
            Statement::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                if_part.block.accept(visitor);
                for part in elif_parts {
                    part.block.accept(visitor);
                }

                if let Some(else_part) = else_part {
                    else_part.accept(visitor);
                }
            }
            _ => {}
        }
    }
}

pub trait ParseNode {
    fn parse_oneshot(parser: Parser) -> Result<Self, ParserError>
    where
        Self: Sized;
}

impl ParseNode for Expr {
    fn parse_oneshot(mut parser: Parser) -> Result<Self, ParserError> {
        parser.parse_expr()
    }
}

impl ParseNode for Statement {
    fn parse_oneshot(mut parser: Parser) -> Result<Self, ParserError> {
        parser.parse_statement()
    }
}
