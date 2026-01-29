//! Semantic Analysis for JOVIAL J73
//!
//! Type checking, symbol resolution, and semantic validation.

use indexmap::IndexMap;

use crate::ast::*;
use crate::error::{CompileError, Result, Span};

/// Symbol information
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub type_spec: TypeSpec,
    pub span: Span,
}

/// Symbol kinds
#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable,
    Constant,
    Procedure,
    Type,
    Define,
    Table,
}

/// Symbol table with scoping
pub struct SymbolTable {
    scopes: Vec<IndexMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![IndexMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(IndexMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, symbol: Symbol) -> Result<()> {
        let scope = self.scopes.last_mut().unwrap();
        let name = symbol.name.to_uppercase();
        if scope.contains_key(&name) {
            return Err(CompileError::semantic(
                symbol.span,
                format!("Symbol '{}' already defined in this scope", symbol.name),
            ));
        }
        scope.insert(name, symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let name = name.to_uppercase();
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(&name) {
                return Some(symbol);
            }
        }
        None
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Semantic analyser
pub struct Analyser {
    symbols: SymbolTable,
}

impl Analyser {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
        }
    }

    /// Analyse a program
    pub fn analyse(&mut self, program: &Program) -> Result<()> {
        // First pass: collect all declarations
        for decl in &program.declarations {
            self.collect_declaration(decl)?;
        }

        // Second pass: type check everything
        for decl in &program.declarations {
            self.check_declaration(decl)?;
        }

        for stmt in &program.statements {
            self.check_statement(stmt)?;
        }

        Ok(())
    }

    fn collect_declaration(&mut self, decl: &Declaration) -> Result<()> {
        match decl {
            Declaration::Define { name, span, .. } => {
                self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Define,
                    type_spec: TypeSpec::void(*span),
                    span: *span,
                })?;
            }
            Declaration::Type { name, type_spec, span } => {
                self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Type,
                    type_spec: type_spec.clone(),
                    span: *span,
                })?;
            }
            Declaration::Item { name, type_spec, is_constant, span, .. } => {
                self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: if *is_constant { SymbolKind::Constant } else { SymbolKind::Variable },
                    type_spec: type_spec.clone(),
                    span: *span,
                })?;
            }
            Declaration::Table { name, span, .. } => {
                self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Table,
                    type_spec: TypeSpec::void(*span),
                    span: *span,
                })?;
            }
            Declaration::Proc { name, return_type, span, .. } => {
                self.symbols.define(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Procedure,
                    type_spec: return_type.clone().unwrap_or_else(|| TypeSpec::void(*span)),
                    span: *span,
                })?;
            }
            Declaration::Block { declarations, .. } => {
                for d in declarations {
                    self.collect_declaration(d)?;
                }
            }
            Declaration::Compool { declarations, .. } => {
                for d in declarations {
                    self.collect_declaration(d)?;
                }
            }
        }
        Ok(())
    }

    fn check_declaration(&mut self, decl: &Declaration) -> Result<()> {
        match decl {
            Declaration::Proc { params, output_params, locals, body, .. } => {
                self.symbols.push_scope();

                // Collect parameter names (they may be re-declared as ITEMs in body for type info)
                let param_names: std::collections::HashSet<String> = params
                    .iter()
                    .chain(output_params.iter())
                    .filter_map(|p| {
                        if let Declaration::Item { name, .. } = p {
                            Some(name.to_uppercase())
                        } else {
                            None
                        }
                    })
                    .collect();

                // Add parameters to scope (get type from locals if available)
                for param in params.iter().chain(output_params.iter()) {
                    if let Declaration::Item { name, span, .. } = param {
                        // Look for matching ITEM in locals to get the real type
                        let type_spec = locals
                            .iter()
                            .find_map(|local| {
                                if let Declaration::Item { name: local_name, type_spec, .. } = local {
                                    if local_name.to_uppercase() == name.to_uppercase() {
                                        return Some(type_spec.clone());
                                    }
                                }
                                None
                            })
                            .unwrap_or_else(|| TypeSpec::void(*span));

                        self.symbols.define(Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            type_spec,
                            span: *span,
                        })?;
                    }
                }

                // Add locals (skip params - they're already defined)
                for local in locals {
                    if let Declaration::Item { name, .. } = local {
                        if param_names.contains(&name.to_uppercase()) {
                            continue; // Skip - this is a parameter type declaration
                        }
                    }
                    self.collect_declaration(local)?;
                }

                // Check body
                for stmt in body {
                    self.check_statement(stmt)?;
                }

                self.symbols.pop_scope();
            }
            Declaration::Item { initial_value, .. } => {
                if let Some(expr) = initial_value {
                    self.check_expression(expr)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn check_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Assign { target, value, .. } => {
                self.check_expression(target)?;
                self.check_expression(value)?;
            }
            Statement::Call { expr, .. } => {
                self.check_expression(expr)?;
            }
            Statement::If { condition, then_body, else_body, .. } => {
                self.check_expression(condition)?;
                for s in then_body {
                    self.check_statement(s)?;
                }
                for s in else_body {
                    self.check_statement(s)?;
                }
            }
            Statement::Case { selector, branches, default, .. } => {
                self.check_expression(selector)?;
                for branch in branches {
                    for val in &branch.values {
                        self.check_expression(val)?;
                    }
                    for s in &branch.body {
                        self.check_statement(s)?;
                    }
                }
                for s in default {
                    self.check_statement(s)?;
                }
            }
            Statement::While { condition, body, .. } => {
                self.check_expression(condition)?;
                for s in body {
                    self.check_statement(s)?;
                }
            }
            Statement::For { start, end, step, body, .. } => {
                self.check_expression(start)?;
                self.check_expression(end)?;
                if let Some(s) = step {
                    self.check_expression(s)?;
                }
                for s in body {
                    self.check_statement(s)?;
                }
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.check_expression(expr)?;
                }
            }
            Statement::Block { statements, .. } => {
                for s in statements {
                    self.check_statement(s)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Ident { name, span } => {
                if self.symbols.lookup(name).is_none() {
                    return Err(CompileError::semantic(
                        *span,
                        format!("Undefined symbol '{}'", name),
                    ));
                }
            }
            Expr::Binary { left, right, .. } => {
                self.check_expression(left)?;
                self.check_expression(right)?;
            }
            Expr::Unary { operand, .. } => {
                self.check_expression(operand)?;
            }
            Expr::Call { args, output_args, .. } => {
                for arg in args {
                    self.check_expression(arg)?;
                }
                for arg in output_args {
                    self.check_expression(arg)?;
                }
            }
            Expr::Index { base, indices, .. } => {
                self.check_expression(base)?;
                for idx in indices {
                    self.check_expression(idx)?;
                }
            }
            Expr::Member { base, .. } => {
                self.check_expression(base)?;
            }
            Expr::Deref { operand, .. } => {
                self.check_expression(operand)?;
            }
            Expr::AddrOf { operand, .. } => {
                self.check_expression(operand)?;
            }
            _ => {}
        }
        Ok(())
    }

    // =========================================================================
    // AST Transformation (Call -> Index for TABLEs)
    // =========================================================================

    fn transform_declaration(&self, decl: &mut Declaration) {
        match decl {
            Declaration::Item { initial_value, .. } => {
                if let Some(expr) = initial_value {
                    self.transform_expression(expr);
                }
            }
            Declaration::Proc { locals, body, .. } => {
                for local in locals {
                    self.transform_declaration(local);
                }
                for stmt in body {
                    self.transform_statement(stmt);
                }
            }
            Declaration::Block { declarations, .. } => {
                for d in declarations {
                    self.transform_declaration(d);
                }
            }
            Declaration::Compool { declarations, .. } => {
                for d in declarations {
                    self.transform_declaration(d);
                }
            }
            Declaration::Table { entries, .. } => {
                for entry in entries {
                    self.transform_declaration(entry);
                }
            }
            _ => {}
        }
    }

    fn transform_statement(&self, stmt: &mut Statement) {
        match stmt {
            Statement::Assign { target, value, .. } => {
                self.transform_expression(target);
                self.transform_expression(value);
            }
            Statement::Call { expr, .. } => {
                self.transform_expression(expr);
            }
            Statement::If { condition, then_body, else_body, .. } => {
                self.transform_expression(condition);
                for s in then_body {
                    self.transform_statement(s);
                }
                for s in else_body {
                    self.transform_statement(s);
                }
            }
            Statement::Case { selector, branches, default, .. } => {
                self.transform_expression(selector);
                for branch in branches {
                    for val in &mut branch.values {
                        self.transform_expression(val);
                    }
                    for s in &mut branch.body {
                        self.transform_statement(s);
                    }
                }
                for s in default {
                    self.transform_statement(s);
                }
            }
            Statement::While { condition, body, .. } => {
                self.transform_expression(condition);
                for s in body {
                    self.transform_statement(s);
                }
            }
            Statement::For { start, end, step, body, .. } => {
                self.transform_expression(start);
                self.transform_expression(end);
                if let Some(s) = step {
                    self.transform_expression(s);
                }
                for s in body {
                    self.transform_statement(s);
                }
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.transform_expression(expr);
                }
            }
            Statement::Block { statements, .. } => {
                for s in statements {
                    self.transform_statement(s);
                }
            }
            _ => {}
        }
    }

    fn transform_expression(&self, expr: &mut Expr) {
        // First, recursively transform children
        match expr {
            Expr::Binary { left, right, .. } => {
                self.transform_expression(left);
                self.transform_expression(right);
            }
            Expr::Unary { operand, .. } => {
                self.transform_expression(operand);
            }
            Expr::Call { args, output_args, .. } => {
                for arg in args {
                    self.transform_expression(arg);
                }
                for arg in output_args {
                    self.transform_expression(arg);
                }
            }
            Expr::Index { base, indices, .. } => {
                self.transform_expression(base);
                for idx in indices {
                    self.transform_expression(idx);
                }
            }
            Expr::Member { base, .. } => {
                self.transform_expression(base);
            }
            Expr::Deref { operand, .. } => {
                self.transform_expression(operand);
            }
            Expr::AddrOf { operand, .. } => {
                self.transform_expression(operand);
            }
            _ => {}
        }

        // Check if this Call should be transformed to Index
        if let Expr::Call { name, args, output_args, span } = expr {
            // Only transform if there are no output args (arrays don't have output params)
            if output_args.is_empty() {
                if let Some(symbol) = self.symbols.lookup(name) {
                    if matches!(symbol.kind, SymbolKind::Table) {
                        // Transform Call to Index
                        let base = Box::new(Expr::Ident {
                            name: name.clone(),
                            span: *span,
                        });
                        *expr = Expr::Index {
                            base,
                            indices: std::mem::take(args),
                            span: *span,
                        };
                    }
                }
            }
        }
    }
}

impl Default for Analyser {
    fn default() -> Self {
        Self::new()
    }
}

/// Analyse a program
pub fn analyse(program: &Program) -> Result<()> {
    Analyser::new().analyse(program)
}

/// Transform and analyse a program (converts Call to Index for TABLEs)
pub fn transform(program: &mut Program) -> Result<()> {
    let mut analyser = Analyser::new();

    // First pass: collect all declarations to build symbol table
    for decl in &program.declarations {
        analyser.collect_declaration(decl)?;
    }

    // Second pass: transform expressions (Call -> Index for TABLEs)
    for decl in &mut program.declarations {
        analyser.transform_declaration(decl);
    }
    for stmt in &mut program.statements {
        analyser.transform_statement(stmt);
    }

    Ok(())
}
