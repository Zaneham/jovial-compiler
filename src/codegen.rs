//! LLVM IR Code Generation for JOVIAL J73
//!
//! Generates LLVM IR from the JOVIAL AST using inkwell.

// TODO: Enable when LLVM is configured
// use inkwell::builder::Builder;
// use inkwell::context::Context;
// use inkwell::module::Module;
// use inkwell::types::*;
// use inkwell::values::*;

use crate::ast::*;
use crate::error::{CompileError, Result};

/// Code generator (stub - LLVM integration pending)
pub struct CodeGenerator {
    // context: Context,
    // module: Module<'ctx>,
    // builder: Builder<'ctx>,
    /// Current function's output parameters (for dereference in assignments)
    current_output_params: std::collections::HashSet<String>,
}

impl CodeGenerator {
    pub fn new(_module_name: &str) -> Self {
        Self {
            current_output_params: std::collections::HashSet::new(),
        }
    }

    /// Generate LLVM IR for a program
    pub fn generate(&mut self, _program: &Program) -> Result<String> {
        // TODO: Implement LLVM IR generation
        //
        // The general approach:
        // 1. Create LLVM context, module, and builder
        // 2. Declare all global variables
        // 3. Generate function declarations
        // 4. Generate function bodies
        // 5. Run optimisation passes
        // 6. Return IR or emit object code

        Err(CompileError::codegen(
            "LLVM codegen not yet implemented - use C transpiler for now",
        ))
    }

    /// Generate C code as a fallback (using the Python transpiler approach)
    pub fn generate_c(&mut self, program: &Program) -> Result<String> {
        let mut output = String::new();

        // Header
        output.push_str(&format!(
            "/* Generated from JOVIAL program: {} */\n",
            program.name
        ));
        output.push_str("/* JOVIAL J73 to C transpiler */\n\n");
        output.push_str("#include <stdint.h>\n");
        output.push_str("#include <stdbool.h>\n");
        output.push_str("#include <math.h>\n");
        output.push_str("#include <string.h>\n");
        output.push_str("#include <stdlib.h>\n\n");

        // Generate declarations
        for decl in &program.declarations {
            self.generate_c_declaration(&mut output, decl, 0)?;
        }

        // Generate main if there are statements
        if !program.statements.is_empty() {
            output.push_str("\nint main(void) {\n");
            for stmt in &program.statements {
                self.generate_c_statement(&mut output, stmt, 1)?;
            }
            output.push_str("    return 0;\n");
            output.push_str("}\n");
        }

        Ok(output)
    }

    fn generate_c_declaration(
        &mut self,
        output: &mut String,
        decl: &Declaration,
        indent: usize,
    ) -> Result<()> {
        let ind = "    ".repeat(indent);

        match decl {
            Declaration::Define { name, value, .. } => {
                let val = self.expr_to_c(value);
                output.push_str(&format!(
                    "#define {} {}\n",
                    Self::translate_name(name),
                    val
                ));
            }
            Declaration::Type { name, type_spec, .. } => {
                let c_type = self.type_to_c(type_spec);
                output.push_str(&format!(
                    "typedef {} {};\n",
                    c_type,
                    Self::translate_name(name)
                ));
            }
            Declaration::Item {
                name,
                type_spec,
                is_static,
                is_constant,
                initial_value,
                ..
            } => {
                let mut prefix = String::new();
                if *is_static && indent == 0 {
                    prefix.push_str("static ");
                }
                if *is_constant {
                    prefix.push_str("const ");
                }

                let c_name = Self::translate_name(name);

                // Check if this is a STATUS type with values - generate enum
                let c_type = if type_spec.base == BaseType::Status && !type_spec.status_values.is_empty() {
                    let enum_name = format!("{}_t", c_name);
                    // Generate enum typedef
                    output.push_str(&format!("{}typedef enum {{\n", ind));
                    for (i, val) in type_spec.status_values.iter().enumerate() {
                        let comma = if i < type_spec.status_values.len() - 1 { "," } else { "" };
                        output.push_str(&format!("{}    {}{}\n", ind, Self::translate_name(val), comma));
                    }
                    output.push_str(&format!("{}}} {};\n", ind, enum_name));
                    enum_name
                } else {
                    self.type_to_c(type_spec)
                };

                let init = initial_value
                    .as_ref()
                    .map(|v| format!(" = {}", self.expr_to_c(v)))
                    .unwrap_or_default();

                output.push_str(&format!("{}{}{} {}{};\n", ind, prefix, c_type, c_name, init));
            }
            Declaration::Table {
                name,
                dimensions,
                entries,
                ..
            } => {
                let c_name = Self::translate_name(name);

                // Generate struct
                output.push_str(&format!("{}typedef struct {{\n", ind));
                for entry in entries {
                    self.generate_c_declaration(output, entry, indent + 1)?;
                }
                output.push_str(&format!("{}}} {}_entry_t;\n\n", ind, c_name));

                // Generate array
                let dims: Vec<String> = dimensions
                    .iter()
                    .map(|(_, upper)| format!("[{}]", self.expr_to_c(upper)))
                    .collect();
                output.push_str(&format!(
                    "{}{}_entry_t {}{};\n\n",
                    ind,
                    c_name,
                    c_name,
                    dims.join("")
                ));
            }
            Declaration::Proc {
                name,
                params,
                output_params,
                return_type,
                locals,
                body,
                ..
            } => {
                let c_name = Self::translate_name(name);
                let ret_type = return_type
                    .as_ref()
                    .map(|t| self.type_to_c(t))
                    .unwrap_or_else(|| "void".to_string());

                // Build parameter types from locals
                let mut param_types = std::collections::HashMap::new();
                for local in locals {
                    if let Declaration::Item { name, type_spec, .. } = local {
                        param_types.insert(name.to_uppercase(), type_spec.clone());
                    }
                }

                // Track output params for dereference in assignments
                self.current_output_params.clear();
                for p in output_params {
                    if let Declaration::Item { name, .. } = p {
                        self.current_output_params.insert(name.to_uppercase());
                    }
                }

                // Generate parameter list
                let mut params_str = Vec::new();
                for p in params {
                    if let Declaration::Item { name, type_spec, .. } = p {
                        let p_type = param_types
                            .get(&name.to_uppercase())
                            .map(|t| self.type_to_c(t))
                            .unwrap_or_else(|| self.type_to_c(type_spec));
                        params_str.push(format!("{} {}", p_type, Self::translate_name(name)));
                    }
                }
                for p in output_params {
                    if let Declaration::Item { name, type_spec, .. } = p {
                        let p_type = param_types
                            .get(&name.to_uppercase())
                            .map(|t| self.type_to_c(t))
                            .unwrap_or_else(|| self.type_to_c(type_spec));
                        params_str.push(format!("{}* {}", p_type, Self::translate_name(name)));
                    }
                }

                let params_list = if params_str.is_empty() {
                    "void".to_string()
                } else {
                    params_str.join(", ")
                };

                output.push_str(&format!(
                    "{}{} {}({}) {{\n",
                    ind, ret_type, c_name, params_list
                ));

                // Collect param names to skip
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

                // Generate locals (skip params)
                for local in locals {
                    if let Declaration::Item { name, .. } = local {
                        if !param_names.contains(&name.to_uppercase()) {
                            self.generate_c_declaration(output, local, indent + 1)?;
                        }
                    }
                }

                // Generate body
                for stmt in body {
                    self.generate_c_statement(output, stmt, indent + 1)?;
                }

                // Clear output params after function
                self.current_output_params.clear();

                output.push_str(&format!("{}}}\n\n", ind));
            }
            Declaration::Block { declarations, .. } => {
                for d in declarations {
                    self.generate_c_declaration(output, d, indent)?;
                }
            }
            Declaration::Compool { name, declarations, .. } => {
                output.push_str(&format!("/* COMPOOL {} */\n", Self::translate_name(name)));
                for d in declarations {
                    self.generate_c_declaration(output, d, indent)?;
                }
            }
        }

        Ok(())
    }

    fn generate_c_statement(&self, output: &mut String, stmt: &Statement, indent: usize) -> Result<()> {
        let ind = "    ".repeat(indent);

        match stmt {
            Statement::Null { .. } => {
                output.push_str(&format!("{};\n", ind));
            }
            Statement::Assign { target, value, .. } => {
                let v = self.expr_to_c(value);
                // Check if target is an output param (needs dereference)
                let t = if let Expr::Ident { name, .. } = target {
                    if self.current_output_params.contains(&name.to_uppercase()) {
                        format!("*{}", Self::translate_name(name))
                    } else {
                        self.expr_to_c(target)
                    }
                } else {
                    self.expr_to_c(target)
                };
                output.push_str(&format!("{}{} = {};\n", ind, t, v));
            }
            Statement::Call { expr, .. } => {
                // Add () for simple identifier calls (procedures without args)
                let call_str = match expr {
                    Expr::Ident { name, .. } => format!("{}()", Self::translate_name(name)),
                    _ => self.expr_to_c(expr),
                };
                output.push_str(&format!("{}{};\n", ind, call_str));
            }
            Statement::If { condition, then_body, else_body, .. } => {
                output.push_str(&format!("{}if ({}) {{\n", ind, self.expr_to_c(condition)));
                for s in then_body {
                    self.generate_c_statement(output, s, indent + 1)?;
                }
                if !else_body.is_empty() {
                    output.push_str(&format!("{}}} else {{\n", ind));
                    for s in else_body {
                        self.generate_c_statement(output, s, indent + 1)?;
                    }
                }
                output.push_str(&format!("{}}}\n", ind));
            }
            Statement::Case { selector, branches, default, .. } => {
                output.push_str(&format!("{}switch ({}) {{\n", ind, self.expr_to_c(selector)));
                for branch in branches {
                    for val in &branch.values {
                        output.push_str(&format!("{}case {}:\n", ind, self.expr_to_c(val)));
                    }
                    for s in &branch.body {
                        self.generate_c_statement(output, s, indent + 1)?;
                    }
                    if !branch.fallthru {
                        output.push_str(&format!("{}    break;\n", ind));
                    }
                }
                if !default.is_empty() {
                    output.push_str(&format!("{}default:\n", ind));
                    for s in default {
                        self.generate_c_statement(output, s, indent + 1)?;
                    }
                    output.push_str(&format!("{}    break;\n", ind));
                }
                output.push_str(&format!("{}}}\n", ind));
            }
            Statement::While { condition, body, .. } => {
                output.push_str(&format!("{}while ({}) {{\n", ind, self.expr_to_c(condition)));
                for s in body {
                    self.generate_c_statement(output, s, indent + 1)?;
                }
                output.push_str(&format!("{}}}\n", ind));
            }
            Statement::For { variable, start, end, step, body, .. } => {
                let var = Self::translate_name(variable);
                let st = self.expr_to_c(start);
                let en = self.expr_to_c(end);
                let stp = step.as_ref().map(|s| self.expr_to_c(s)).unwrap_or_else(|| "1".to_string());
                output.push_str(&format!(
                    "{}for ({} = {}; {} <= {}; {} += {}) {{\n",
                    ind, var, st, var, en, var, stp
                ));
                for s in body {
                    self.generate_c_statement(output, s, indent + 1)?;
                }
                output.push_str(&format!("{}}}\n", ind));
            }
            Statement::Goto { label, .. } => {
                output.push_str(&format!("{}goto {};\n", ind, Self::translate_name(label)));
            }
            Statement::Label { name, .. } => {
                output.push_str(&format!("{}:\n", Self::translate_name(name)));
            }
            Statement::Return { value, .. } => {
                if let Some(v) = value {
                    output.push_str(&format!("{}return {};\n", ind, self.expr_to_c(v)));
                } else {
                    output.push_str(&format!("{}return;\n", ind));
                }
            }
            Statement::Exit { .. } => {
                output.push_str(&format!("{}break;\n", ind));
            }
            Statement::Abort { .. } => {
                output.push_str(&format!("{}abort();\n", ind));
            }
            Statement::Stop { .. } => {
                output.push_str(&format!("{}exit(0);\n", ind));
            }
            Statement::Block { statements, .. } => {
                output.push_str(&format!("{}{{\n", ind));
                for s in statements {
                    self.generate_c_statement(output, s, indent + 1)?;
                }
                output.push_str(&format!("{}}}\n", ind));
            }
        }

        Ok(())
    }

    fn expr_to_c(&self, expr: &Expr) -> String {
        match expr {
            Expr::Integer { value, .. } => value.to_string(),
            Expr::Float { value, .. } => value.to_string(),
            Expr::String { value, .. } => format!("\"{}\"", value.replace('"', "\\\"")),
            Expr::Status { value, .. } => Self::translate_name(value),
            Expr::Ident { name, .. } => Self::translate_name(name),
            Expr::Binary { op, left, right, .. } => {
                let l = self.expr_to_c(left);
                let r = self.expr_to_c(right);
                match op {
                    BinaryOp::Power => format!("pow({}, {})", l, r),
                    _ => {
                        let op_str = match op {
                            BinaryOp::Add => "+",
                            BinaryOp::Sub => "-",
                            BinaryOp::Mul => "*",
                            BinaryOp::Div => "/",
                            BinaryOp::Mod => "%",
                            BinaryOp::Eq => "==",
                            BinaryOp::Ne => "!=",
                            BinaryOp::Lt => "<",
                            BinaryOp::Le => "<=",
                            BinaryOp::Gt => ">",
                            BinaryOp::Ge => ">=",
                            BinaryOp::And => "&&",
                            BinaryOp::Or => "||",
                            BinaryOp::Xor => "^",
                            BinaryOp::Eqv => "==",
                            BinaryOp::Power => unreachable!(),
                        };
                        format!("({} {} {})", l, op_str, r)
                    }
                }
            }
            Expr::Unary { op, operand, .. } => {
                let o = self.expr_to_c(operand);
                match op {
                    UnaryOp::Neg => format!("(-{})", o),
                    UnaryOp::Pos => format!("(+{})", o),
                    UnaryOp::Not => format!("(!{})", o),
                }
            }
            Expr::Call { name, args, output_args, .. } => {
                let c_name = Self::translate_name(name);
                let mut all_args: Vec<String> = args.iter().map(|a| self.expr_to_c(a)).collect();
                for arg in output_args {
                    all_args.push(format!("&{}", self.expr_to_c(arg)));
                }
                format!("{}({})", c_name, all_args.join(", "))
            }
            Expr::Index { base, indices, .. } => {
                let b = self.expr_to_c(base);
                let idxs: Vec<String> = indices
                    .iter()
                    .map(|i| format!("[({} - 1)]", self.expr_to_c(i)))
                    .collect();
                format!("{}{}", b, idxs.join(""))
            }
            Expr::Member { base, member, .. } => {
                format!("{}.{}", self.expr_to_c(base), Self::translate_name(member))
            }
            Expr::Deref { operand, .. } => {
                format!("(*{})", self.expr_to_c(operand))
            }
            Expr::AddrOf { operand, .. } => {
                format!("(&{})", self.expr_to_c(operand))
            }
        }
    }

    fn type_to_c(&self, spec: &TypeSpec) -> String {
        if let Some(ref name) = spec.referenced_type {
            return Self::translate_name(name);
        }

        match spec.base {
            BaseType::Signed => {
                match spec.size.unwrap_or(32) {
                    0..=8 => "int8_t",
                    9..=16 => "int16_t",
                    17..=32 => "int32_t",
                    _ => "int64_t",
                }
                .to_string()
            }
            BaseType::Unsigned => {
                match spec.size.unwrap_or(32) {
                    0..=8 => "uint8_t",
                    9..=16 => "uint16_t",
                    17..=32 => "uint32_t",
                    _ => "uint64_t",
                }
                .to_string()
            }
            BaseType::Float => {
                if spec.size.unwrap_or(32) <= 32 {
                    "float"
                } else {
                    "double"
                }
                .to_string()
            }
            BaseType::Bit => {
                match spec.size.unwrap_or(1) {
                    1 => "bool",
                    2..=8 => "uint8_t",
                    9..=16 => "uint16_t",
                    17..=32 => "uint32_t",
                    _ => "uint64_t",
                }
                .to_string()
            }
            BaseType::Character => "char".to_string(),
            BaseType::Hollerith => "char".to_string(),
            BaseType::Fixed => "int32_t".to_string(), // TODO: Fixed-point
            BaseType::Status => "int".to_string(),
            BaseType::Pointer => {
                if let Some(ref elem) = spec.element_type {
                    format!("{}*", self.type_to_c(elem))
                } else {
                    "void*".to_string()
                }
            }
            BaseType::Void => "void".to_string(),
        }
    }

    fn translate_name(name: &str) -> String {
        name.replace('\'', "_").to_lowercase()
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new("jovial_module")
    }
}

/// Generate C code from a program
pub fn generate_c(program: &Program) -> Result<String> {
    CodeGenerator::new(&program.name).generate_c(program)
}
