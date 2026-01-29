//! Error types for the JOVIAL compiler

use thiserror::Error;

/// Source location
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

/// Compiler error
#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Lexer error at {span:?}: {message}")]
    Lexer { span: Span, message: String },

    #[error("Parse error at {span:?}: {message}")]
    Parse { span: Span, message: String },

    #[error("Semantic error at {span:?}: {message}")]
    Semantic { span: Span, message: String },

    #[error("Codegen error: {message}")]
    Codegen { message: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

impl CompileError {
    pub fn lexer(span: impl Into<Span>, message: impl Into<String>) -> Self {
        Self::Lexer {
            span: span.into(),
            message: message.into(),
        }
    }

    pub fn parse(span: impl Into<Span>, message: impl Into<String>) -> Self {
        Self::Parse {
            span: span.into(),
            message: message.into(),
        }
    }

    pub fn semantic(span: impl Into<Span>, message: impl Into<String>) -> Self {
        Self::Semantic {
            span: span.into(),
            message: message.into(),
        }
    }

    pub fn codegen(message: impl Into<String>) -> Self {
        Self::Codegen {
            message: message.into(),
        }
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;
