//! JOVIAL J73 Abstract Syntax Tree
//!
//! Defines the AST nodes for JOVIAL programs.

use crate::error::Span;

/// A complete JOVIAL program
#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

// =============================================================================
// Types
// =============================================================================

/// JOVIAL base types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    Signed,     // S n
    Unsigned,   // U n
    Float,      // F n
    Bit,        // B n
    Character,  // C n
    Hollerith,  // H n
    Fixed,      // A n (fixed-point)
    Status,     // STATUS
    Pointer,    // POINTER
    Void,       // For type references
}

/// Type specification
#[derive(Debug, Clone)]
pub struct TypeSpec {
    pub base: BaseType,
    pub size: Option<u32>,           // Bit width
    pub scale: Option<i32>,          // For fixed-point
    pub status_values: Vec<String>,  // For STATUS types
    pub element_type: Option<Box<TypeSpec>>,  // For POINTER
    pub referenced_type: Option<String>,  // For type references
    pub span: Span,
}

impl TypeSpec {
    pub fn signed(size: u32, span: Span) -> Self {
        Self {
            base: BaseType::Signed,
            size: Some(size),
            scale: None,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: None,
            span,
        }
    }

    pub fn unsigned(size: u32, span: Span) -> Self {
        Self {
            base: BaseType::Unsigned,
            size: Some(size),
            scale: None,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: None,
            span,
        }
    }

    pub fn float(size: u32, span: Span) -> Self {
        Self {
            base: BaseType::Float,
            size: Some(size),
            scale: None,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: None,
            span,
        }
    }

    pub fn void(span: Span) -> Self {
        Self {
            base: BaseType::Void,
            size: None,
            scale: None,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: None,
            span,
        }
    }

    pub fn type_ref(name: String, span: Span) -> Self {
        Self {
            base: BaseType::Void,
            size: None,
            scale: None,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: Some(name),
            span,
        }
    }
}

// =============================================================================
// Expressions
// =============================================================================

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod, Power,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor, Eqv,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,    // -
    Pos,    // +
    Not,    // NOT
}

/// Expression node
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    Integer { value: i64, span: Span },

    /// Float literal
    Float { value: f64, span: Span },

    /// String literal
    String { value: String, span: Span },

    /// Status literal: V(NAME)
    Status { value: String, span: Span },

    /// Identifier
    Ident { name: String, span: Span },

    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },

    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },

    /// Function/procedure call or table access
    Call {
        name: String,
        args: Vec<Expr>,
        output_args: Vec<Expr>,
        span: Span,
    },

    /// Array indexing
    Index {
        base: Box<Expr>,
        indices: Vec<Expr>,
        span: Span,
    },

    /// Member access: expr.member
    Member {
        base: Box<Expr>,
        member: String,
        span: Span,
    },

    /// Pointer dereference: @ptr
    Deref {
        operand: Box<Expr>,
        span: Span,
    },

    /// Address-of: LOC(expr)
    AddrOf {
        operand: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Integer { span, .. } => *span,
            Expr::Float { span, .. } => *span,
            Expr::String { span, .. } => *span,
            Expr::Status { span, .. } => *span,
            Expr::Ident { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Member { span, .. } => *span,
            Expr::Deref { span, .. } => *span,
            Expr::AddrOf { span, .. } => *span,
        }
    }
}

// =============================================================================
// Statements
// =============================================================================

/// Statement node
#[derive(Debug, Clone)]
pub enum Statement {
    /// Null statement (just semicolon)
    Null { span: Span },

    /// Assignment: target := value
    Assign {
        target: Expr,
        value: Expr,
        span: Span,
    },

    /// Procedure call
    Call {
        expr: Expr,
        span: Span,
    },

    /// IF statement
    If {
        condition: Expr,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
        span: Span,
    },

    /// CASE statement
    Case {
        selector: Expr,
        branches: Vec<CaseBranch>,
        default: Vec<Statement>,
        span: Span,
    },

    /// WHILE loop
    While {
        condition: Expr,
        body: Vec<Statement>,
        span: Span,
    },

    /// FOR loop
    For {
        variable: String,
        start: Expr,
        end: Expr,
        step: Option<Expr>,
        body: Vec<Statement>,
        span: Span,
    },

    /// GOTO
    Goto { label: String, span: Span },

    /// Label definition
    Label { name: String, span: Span },

    /// RETURN
    Return { value: Option<Expr>, span: Span },

    /// EXIT (break)
    Exit { span: Span },

    /// ABORT
    Abort { span: Span },

    /// STOP
    Stop { span: Span },

    /// BEGIN ... END block
    Block {
        statements: Vec<Statement>,
        span: Span,
    },
}

/// Case branch
#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub values: Vec<Expr>,
    pub body: Vec<Statement>,
    pub fallthru: bool,
    pub span: Span,
}

// =============================================================================
// Declarations
// =============================================================================

/// Declaration node
#[derive(Debug, Clone)]
pub enum Declaration {
    /// DEFINE name = value
    Define {
        name: String,
        value: Expr,
        span: Span,
    },

    /// TYPE name type-spec
    Type {
        name: String,
        type_spec: TypeSpec,
        span: Span,
    },

    /// ITEM declaration
    Item {
        name: String,
        type_spec: TypeSpec,
        is_static: bool,
        is_constant: bool,
        is_parallel: bool,
        initial_value: Option<Expr>,
        like_source: Option<String>,      // For LIKE modifier - copies type from another item
        overlay_target: Option<String>,   // For OVERLAY - shares memory with target
        pos_target: Option<(String, u32)>, // For POS - (target, bit_offset) into target
        span: Span,
    },

    /// TABLE declaration
    Table {
        name: String,
        dimensions: Vec<(Expr, Expr)>,  // (lower, upper) bounds
        entries: Vec<Declaration>,
        is_parallel: bool,
        wordsize: Option<u32>,
        span: Span,
    },

    /// PROC declaration
    Proc {
        name: String,
        params: Vec<Declaration>,
        output_params: Vec<Declaration>,
        return_type: Option<TypeSpec>,
        is_recursive: bool,
        is_reentrant: bool,
        is_inline: bool,
        locals: Vec<Declaration>,
        body: Vec<Statement>,
        span: Span,
    },

    /// BLOCK declaration
    Block {
        name: Option<String>,
        declarations: Vec<Declaration>,
        span: Span,
    },

    /// COMPOOL declaration
    Compool {
        name: String,
        declarations: Vec<Declaration>,
        span: Span,
    },
}

impl Declaration {
    pub fn span(&self) -> Span {
        match self {
            Declaration::Define { span, .. } => *span,
            Declaration::Type { span, .. } => *span,
            Declaration::Item { span, .. } => *span,
            Declaration::Table { span, .. } => *span,
            Declaration::Proc { span, .. } => *span,
            Declaration::Block { span, .. } => *span,
            Declaration::Compool { span, .. } => *span,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Declaration::Define { name, .. } => Some(name),
            Declaration::Type { name, .. } => Some(name),
            Declaration::Item { name, .. } => Some(name),
            Declaration::Table { name, .. } => Some(name),
            Declaration::Proc { name, .. } => Some(name),
            Declaration::Block { name, .. } => name.as_deref(),
            Declaration::Compool { name, .. } => Some(name),
        }
    }
}
