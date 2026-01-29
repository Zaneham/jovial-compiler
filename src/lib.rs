//! JOVIAL J73 Compiler
//!
//! A compiler for JOVIAL J73 (MIL-STD-1589C) targeting LLVM IR.
//!
//! JOVIAL (Jules Own Version of the International Algebraic Language) is a
//! programming language used in military avionics systems since 1959. This
//! compiler aims to preserve the ability to compile and study historical
//! JOVIAL code.

pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod llvm_codegen;
pub mod parser;
pub mod semantic;

pub use error::{CompileError, Result};
