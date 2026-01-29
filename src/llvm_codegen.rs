//! LLVM IR Code Generation for JOVIAL J73
//!
//! Generates native code from the JOVIAL AST using inkwell/LLVM.

use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, FloatPredicate, OptimizationLevel};

use crate::ast::*;
use crate::error::{CompileError, Result};

/// LLVM Code Generator
pub struct LLVMCodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    /// Symbol table: name -> (pointer to alloca, type)
    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,

    /// Function table: name -> function value
    functions: HashMap<String, FunctionValue<'ctx>>,

    /// Current function being generated
    current_function: Option<FunctionValue<'ctx>>,

    /// Output parameters in current function (pointer, type)
    current_output_params: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,

    /// Define constants
    defines: HashMap<String, i64>,

    /// Type aliases
    type_aliases: HashMap<String, TypeSpec>,
}

impl<'ctx> LLVMCodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            current_function: None,
            current_output_params: HashMap::new(),
            defines: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }

    /// Declare built-in functions (SQRT, ABS, I/O, etc.)
    fn declare_builtins(&mut self) {
        let f64_type = self.context.f64_type();
        let f32_type = self.context.f32_type();
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());

        // SQRT - use llvm.sqrt intrinsic
        let sqrt_f64_type = f64_type.fn_type(&[f64_type.into()], false);
        let sqrt_f64 = self.module.add_function("llvm.sqrt.f64", sqrt_f64_type, None);
        self.functions.insert("SQRT".to_string(), sqrt_f64);

        let sqrt_f32_type = f32_type.fn_type(&[f32_type.into()], false);
        let sqrt_f32 = self.module.add_function("llvm.sqrt.f32", sqrt_f32_type, None);
        self.functions.insert("SQRTF".to_string(), sqrt_f32);

        // ABS - use llvm.fabs intrinsic
        let fabs_f64_type = f64_type.fn_type(&[f64_type.into()], false);
        let fabs_f64 = self.module.add_function("llvm.fabs.f64", fabs_f64_type, None);
        self.functions.insert("ABS".to_string(), fabs_f64);

        // POW - use llvm.pow intrinsic
        let pow_f64_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);
        let pow_f64 = self.module.add_function("llvm.pow.f64", pow_f64_type, None);
        self.functions.insert("POW".to_string(), pow_f64);

        // === I/O Functions (C library) ===

        // printf(format, ...) -> int
        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true); // variadic
        let printf_fn = self.module.add_function("printf", printf_type, None);
        self.functions.insert("PRINTF".to_string(), printf_fn);

        // puts(str) -> int
        let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
        let puts_fn = self.module.add_function("puts", puts_type, None);
        self.functions.insert("PUTS".to_string(), puts_fn);

        // scanf(format, ...) -> int (for READ)
        let scanf_type = i32_type.fn_type(&[i8_ptr_type.into()], true); // variadic
        let scanf_fn = self.module.add_function("scanf", scanf_type, None);
        self.functions.insert("SCANF".to_string(), scanf_fn);

        // Create format strings for PRINT and READ
        self.create_format_strings();
    }

    /// Create global format strings for printing
    fn create_format_strings(&mut self) {
        // "%d\n" for integers
        let int_fmt = self.context.const_string(b"%d\n", true);
        let int_fmt_global = self.module.add_global(int_fmt.get_type(), None, ".fmt_int");
        int_fmt_global.set_initializer(&int_fmt);
        int_fmt_global.set_constant(true);

        // "%lld\n" for 64-bit integers
        let i64_fmt = self.context.const_string(b"%lld\n", true);
        let i64_fmt_global = self.module.add_global(i64_fmt.get_type(), None, ".fmt_i64");
        i64_fmt_global.set_initializer(&i64_fmt);
        i64_fmt_global.set_constant(true);

        // "%f\n" for floats
        let float_fmt = self.context.const_string(b"%f\n", true);
        let float_fmt_global = self.module.add_global(float_fmt.get_type(), None, ".fmt_float");
        float_fmt_global.set_initializer(&float_fmt);
        float_fmt_global.set_constant(true);

        // "%s\n" for strings
        let str_fmt = self.context.const_string(b"%s\n", true);
        let str_fmt_global = self.module.add_global(str_fmt.get_type(), None, ".fmt_str");
        str_fmt_global.set_initializer(&str_fmt);
        str_fmt_global.set_constant(true);

        // Format strings for READ (scanf) - no newline
        let read_int_fmt = self.context.const_string(b"%d", true);
        let read_int_global = self.module.add_global(read_int_fmt.get_type(), None, ".rfmt_int");
        read_int_global.set_initializer(&read_int_fmt);
        read_int_global.set_constant(true);

        let read_float_fmt = self.context.const_string(b"%f", true);
        let read_float_global = self.module.add_global(read_float_fmt.get_type(), None, ".rfmt_float");
        read_float_global.set_initializer(&read_float_fmt);
        read_float_global.set_constant(true);
    }

    /// Generate LLVM IR for a program
    pub fn generate(&mut self, program: &Program) -> Result<()> {
        // Declare built-in functions (intrinsics)
        self.declare_builtins();

        // First pass: collect defines and type aliases
        for decl in &program.declarations {
            self.collect_declaration(decl)?;
        }

        // Second pass: declare all functions (forward declarations)
        for decl in &program.declarations {
            if let Declaration::Proc { name, params, output_params, return_type, .. } = decl {
                self.declare_function(name, params, output_params, return_type.as_ref())?;
            }
        }

        // Third pass: generate all declarations
        for decl in &program.declarations {
            self.generate_declaration(decl)?;
        }

        // Generate main function if there are statements
        if !program.statements.is_empty() {
            self.generate_main(&program.statements)?;
        }

        Ok(())
    }

    /// Get the LLVM IR as a string
    pub fn get_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Write object file
    pub fn write_object_file(&self, path: &Path) -> Result<()> {
        Target::initialize_x86(&InitializationConfig::default());

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| CompileError::codegen(format!("Failed to get target: {}", e)))?;

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| CompileError::codegen("Failed to create target machine"))?;

        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| CompileError::codegen(format!("Failed to write object file: {}", e)))?;

        Ok(())
    }

    // =========================================================================
    // Type Conversion
    // =========================================================================

    fn jovial_type_to_llvm(&self, type_spec: &TypeSpec) -> BasicTypeEnum<'ctx> {
        // Check for type alias
        if let Some(ref name) = type_spec.referenced_type {
            if let Some(aliased) = self.type_aliases.get(&name.to_uppercase()) {
                return self.jovial_type_to_llvm(aliased);
            }
        }

        match type_spec.base {
            BaseType::Signed => {
                match type_spec.size.unwrap_or(32) {
                    0..=8 => self.context.i8_type().into(),
                    9..=16 => self.context.i16_type().into(),
                    17..=32 => self.context.i32_type().into(),
                    _ => self.context.i64_type().into(),
                }
            }
            BaseType::Unsigned => {
                match type_spec.size.unwrap_or(32) {
                    0..=8 => self.context.i8_type().into(),
                    9..=16 => self.context.i16_type().into(),
                    17..=32 => self.context.i32_type().into(),
                    _ => self.context.i64_type().into(),
                }
            }
            BaseType::Float => {
                if type_spec.size.unwrap_or(32) <= 32 {
                    self.context.f32_type().into()
                } else {
                    self.context.f64_type().into()
                }
            }
            BaseType::Bit => {
                match type_spec.size.unwrap_or(1) {
                    1 => self.context.bool_type().into(),
                    2..=8 => self.context.i8_type().into(),
                    9..=16 => self.context.i16_type().into(),
                    17..=32 => self.context.i32_type().into(),
                    _ => self.context.i64_type().into(),
                }
            }
            BaseType::Character | BaseType::Hollerith => self.context.i8_type().into(),
            BaseType::Fixed => self.context.i32_type().into(),
            BaseType::Status => self.context.i32_type().into(),
            BaseType::Pointer => {
                // For now, use i8* as generic pointer
                self.context.ptr_type(AddressSpace::default()).into()
            }
            BaseType::Void => self.context.i32_type().into(), // Fallback
        }
    }

    // =========================================================================
    // Declaration Collection
    // =========================================================================

    fn collect_declaration(&mut self, decl: &Declaration) -> Result<()> {
        match decl {
            Declaration::Define { name, value, .. } => {
                if let Expr::Integer { value: v, .. } = value {
                    self.defines.insert(name.to_uppercase(), *v);
                }
            }
            Declaration::Type { name, type_spec, .. } => {
                self.type_aliases.insert(name.to_uppercase(), type_spec.clone());
            }
            Declaration::Block { declarations, .. } |
            Declaration::Compool { declarations, .. } => {
                for d in declarations {
                    self.collect_declaration(d)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    // =========================================================================
    // Function Declaration
    // =========================================================================

    fn declare_function(
        &mut self,
        name: &str,
        params: &[Declaration],
        output_params: &[Declaration],
        return_type: Option<&TypeSpec>,
    ) -> Result<FunctionValue<'ctx>> {
        let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();

        // Input parameters
        for param in params {
            if let Declaration::Item { type_spec, .. } = param {
                param_types.push(self.jovial_type_to_llvm(type_spec).into());
            }
        }

        // Output parameters (as pointers)
        for param in output_params {
            if let Declaration::Item { type_spec, .. } = param {
                let _ = self.jovial_type_to_llvm(type_spec);
                param_types.push(self.context.ptr_type(AddressSpace::default()).into());
            }
        }

        let fn_type = if let Some(ret_type) = return_type {
            let ret = self.jovial_type_to_llvm(ret_type);
            ret.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        let fn_name = Self::translate_name(name);
        let function = self.module.add_function(&fn_name, fn_type, None);
        self.functions.insert(name.to_uppercase(), function);

        Ok(function)
    }

    // =========================================================================
    // Declaration Generation
    // =========================================================================

    fn generate_declaration(&mut self, decl: &Declaration) -> Result<()> {
        match decl {
            Declaration::Item { name, type_spec, initial_value, .. } => {
                // Global variable
                let llvm_type = self.jovial_type_to_llvm(type_spec);
                let global_name = Self::translate_name(name);

                let global = match llvm_type {
                    BasicTypeEnum::IntType(t) => {
                        let global = self.module.add_global(t, None, &global_name);
                        if let Some(init) = initial_value {
                            if let Some(val) = self.const_expr_to_int(init) {
                                global.set_initializer(&t.const_int(val as u64, false));
                            } else {
                                global.set_initializer(&t.const_zero());
                            }
                        } else {
                            global.set_initializer(&t.const_zero());
                        }
                        global
                    }
                    BasicTypeEnum::FloatType(t) => {
                        let global = self.module.add_global(t, None, &global_name);
                        if let Some(init) = initial_value {
                            if let Some(val) = self.const_expr_to_float(init) {
                                global.set_initializer(&t.const_float(val));
                            } else {
                                global.set_initializer(&t.const_zero());
                            }
                        } else {
                            global.set_initializer(&t.const_zero());
                        }
                        global
                    }
                    _ => {
                        // For other types, just create uninitialised global
                        let global = self.module.add_global(llvm_type, None, &global_name);
                        global.set_initializer(&llvm_type.const_zero());
                        global
                    }
                };

                self.variables.insert(
                    name.to_uppercase(),
                    (global.as_pointer_value(), llvm_type),
                );
            }

            Declaration::Proc { name, params, output_params, locals, body, .. } => {
                let function = *self.functions.get(&name.to_uppercase())
                    .ok_or_else(|| CompileError::codegen(format!("Function {} not declared", name)))?;

                self.current_function = Some(function);
                self.current_output_params.clear();

                // Create entry block
                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                // Create local scope
                let saved_vars = self.variables.clone();

                // Allocate and store parameters
                let mut param_idx = 0;
                for param in params {
                    if let Declaration::Item { name: param_name, type_spec, .. } = param {
                        let llvm_type = self.jovial_type_to_llvm(type_spec);
                        let alloca = self.builder.build_alloca(llvm_type, &Self::translate_name(param_name))
                            .map_err(|e| CompileError::codegen(format!("Failed to build alloca: {}", e)))?;
                        let param_val = function.get_nth_param(param_idx)
                            .ok_or_else(|| CompileError::codegen("Missing parameter"))?;
                        self.builder.build_store(alloca, param_val)
                            .map_err(|e| CompileError::codegen(format!("Failed to build store: {}", e)))?;
                        self.variables.insert(param_name.to_uppercase(), (alloca, llvm_type));
                        param_idx += 1;
                    }
                }

                // Handle output parameters (they come as pointers)
                for param in output_params {
                    if let Declaration::Item { name: param_name, type_spec, .. } = param {
                        let llvm_type = self.jovial_type_to_llvm(type_spec);
                        let param_val = function.get_nth_param(param_idx)
                            .ok_or_else(|| CompileError::codegen("Missing output parameter"))?;
                        let ptr = param_val.into_pointer_value();
                        self.current_output_params.insert(param_name.to_uppercase(), (ptr, llvm_type));
                        param_idx += 1;
                    }
                }

                // Collect param names to skip in locals
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

                // Allocate local variables
                for local in locals {
                    if let Declaration::Item { name: local_name, type_spec, initial_value, .. } = local {
                        if param_names.contains(&local_name.to_uppercase()) {
                            continue;
                        }
                        let llvm_type = self.jovial_type_to_llvm(type_spec);
                        let alloca = self.builder.build_alloca(llvm_type, &Self::translate_name(local_name))
                            .map_err(|e| CompileError::codegen(format!("Failed to build alloca: {}", e)))?;

                        // Initialise if there's a value
                        if let Some(init) = initial_value {
                            if let Some(val) = self.generate_expression(init)? {
                                self.builder.build_store(alloca, val)
                                    .map_err(|e| CompileError::codegen(format!("Failed to build store: {}", e)))?;
                            }
                        }

                        self.variables.insert(local_name.to_uppercase(), (alloca, llvm_type));
                    }
                }

                // Generate body
                for stmt in body {
                    self.generate_statement(stmt)?;
                }

                // Add implicit return if needed
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_return(None)
                        .map_err(|e| CompileError::codegen(format!("Failed to build return: {}", e)))?;
                }

                // Restore scope
                self.variables = saved_vars;
                self.current_function = None;
                self.current_output_params.clear();
            }

            Declaration::Block { declarations, .. } |
            Declaration::Compool { declarations, .. } => {
                for d in declarations {
                    self.generate_declaration(d)?;
                }
            }

            Declaration::Table { name, dimensions, entries, .. } => {
                // For tables, we generate a struct type and an array
                // Simplified: just create a global array of appropriate size
                let table_name = Self::translate_name(name);

                // Calculate total size from dimensions
                let mut total_size = 1u64;
                for (_, upper) in dimensions {
                    if let Some(size) = self.const_expr_to_int(upper) {
                        total_size *= size as u64;
                    }
                }

                // Create a simple struct type for entries
                let mut field_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
                for entry in entries {
                    if let Declaration::Item { type_spec, .. } = entry {
                        field_types.push(self.jovial_type_to_llvm(type_spec));
                    }
                }

                if !field_types.is_empty() {
                    let struct_type = self.context.struct_type(&field_types, false);
                    let array_type = struct_type.array_type(total_size as u32);
                    let global = self.module.add_global(array_type, None, &table_name);
                    global.set_initializer(&array_type.const_zero());
                    self.variables.insert(
                        name.to_uppercase(),
                        (global.as_pointer_value(), struct_type.into()),
                    );
                }
            }

            _ => {}
        }

        Ok(())
    }

    // =========================================================================
    // Main Function Generation
    // =========================================================================

    fn generate_main(&mut self, statements: &[Statement]) -> Result<()> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);

        self.current_function = Some(main_fn);

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        for stmt in statements {
            self.generate_statement(stmt)?;
        }

        // Return 0
        let zero = i32_type.const_int(0, false);
        self.builder.build_return(Some(&zero))
            .map_err(|e| CompileError::codegen(format!("Failed to build return: {}", e)))?;

        self.current_function = None;

        Ok(())
    }

    // =========================================================================
    // Statement Generation
    // =========================================================================

    fn generate_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Assign { target, value, .. } => {
                let val = self.generate_expression(value)?
                    .ok_or_else(|| CompileError::codegen("Assignment value has no result"))?;

                // Check if target is an output parameter
                if let Expr::Ident { name, .. } = target {
                    if let Some((ptr, ty)) = self.current_output_params.get(&name.to_uppercase()).copied() {
                        let casted_val = self.cast_value(val, ty)?;
                        self.builder.build_store(ptr, casted_val)
                            .map_err(|e| CompileError::codegen(format!("Failed to store: {}", e)))?;
                        return Ok(());
                    }
                }

                // Get target type and cast value
                let (ptr, target_ty) = self.get_lvalue_with_type(target)?;
                let casted_val = self.cast_value(val, target_ty)?;
                self.builder.build_store(ptr, casted_val)
                    .map_err(|e| CompileError::codegen(format!("Failed to store: {}", e)))?;
            }

            Statement::Call { expr, .. } => {
                // Handle simple identifier calls (procedure with no args)
                if let Expr::Ident { name, .. } = expr {
                    if let Some(function) = self.functions.get(&name.to_uppercase()).copied() {
                        self.builder.build_call(function, &[], "call")
                            .map_err(|e| CompileError::codegen(format!("Failed to call: {}", e)))?;
                    } else {
                        return Err(CompileError::codegen(format!("Unknown function: {}", name)));
                    }
                } else {
                    self.generate_expression(expr)?;
                }
            }

            Statement::If { condition, then_body, else_body, .. } => {
                let function = self.current_function
                    .ok_or_else(|| CompileError::codegen("If outside function"))?;

                let cond_val = self.generate_expression(condition)?
                    .ok_or_else(|| CompileError::codegen("If condition has no result"))?;

                // Convert to i1 if needed
                let cond_bool = if cond_val.is_int_value() {
                    let int_val = cond_val.into_int_value();
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        int_val,
                        int_val.get_type().const_zero(),
                        "ifcond",
                    ).map_err(|e| CompileError::codegen(format!("Failed to build compare: {}", e)))?
                } else {
                    cond_val.into_int_value()
                };

                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "ifcont");

                self.builder.build_conditional_branch(cond_bool, then_bb, else_bb)
                    .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;

                // Then block
                self.builder.position_at_end(then_bb);
                for s in then_body {
                    self.generate_statement(s)?;
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb)
                        .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;
                }

                // Else block
                self.builder.position_at_end(else_bb);
                for s in else_body {
                    self.generate_statement(s)?;
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb)
                        .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;
                }

                // Continue at merge
                self.builder.position_at_end(merge_bb);
            }

            Statement::While { condition, body, .. } => {
                let function = self.current_function
                    .ok_or_else(|| CompileError::codegen("While outside function"))?;

                let cond_bb = self.context.append_basic_block(function, "while.cond");
                let body_bb = self.context.append_basic_block(function, "while.body");
                let end_bb = self.context.append_basic_block(function, "while.end");

                self.builder.build_unconditional_branch(cond_bb)
                    .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;

                // Condition block
                self.builder.position_at_end(cond_bb);
                let cond_val = self.generate_expression(condition)?
                    .ok_or_else(|| CompileError::codegen("While condition has no result"))?;

                let cond_bool = if cond_val.is_int_value() {
                    let int_val = cond_val.into_int_value();
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        int_val,
                        int_val.get_type().const_zero(),
                        "whilecond",
                    ).map_err(|e| CompileError::codegen(format!("Failed to build compare: {}", e)))?
                } else {
                    cond_val.into_int_value()
                };

                self.builder.build_conditional_branch(cond_bool, body_bb, end_bb)
                    .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;

                // Body block
                self.builder.position_at_end(body_bb);
                for s in body {
                    self.generate_statement(s)?;
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(cond_bb)
                        .map_err(|e| CompileError::codegen(format!("Failed to build branch: {}", e)))?;
                }

                // End block
                self.builder.position_at_end(end_bb);
            }

            Statement::Return { value, .. } => {
                if let Some(val_expr) = value {
                    let val = self.generate_expression(val_expr)?;
                    self.builder.build_return(val.as_ref().map(|v| v as &dyn BasicValue))
                        .map_err(|e| CompileError::codegen(format!("Failed to build return: {}", e)))?;
                } else {
                    self.builder.build_return(None)
                        .map_err(|e| CompileError::codegen(format!("Failed to build return: {}", e)))?;
                }
            }

            Statement::Block { statements, .. } => {
                for s in statements {
                    self.generate_statement(s)?;
                }
            }

            Statement::Null { .. } => {
                // No-op
            }

            _ => {
                // TODO: Handle other statement types (Case, For, Goto, etc.)
            }
        }

        Ok(())
    }

    // =========================================================================
    // Expression Generation
    // =========================================================================

    fn generate_expression(&mut self, expr: &Expr) -> Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            Expr::Integer { value, .. } => {
                let val = self.context.i64_type().const_int(*value as u64, true);
                Ok(Some(val.into()))
            }

            Expr::Float { value, .. } => {
                let val = self.context.f64_type().const_float(*value);
                Ok(Some(val.into()))
            }

            Expr::Ident { name, .. } => {
                // Check defines first
                if let Some(&val) = self.defines.get(&name.to_uppercase()) {
                    return Ok(Some(self.context.i64_type().const_int(val as u64, true).into()));
                }

                // Check output params
                if let Some(&(ptr, ty)) = self.current_output_params.get(&name.to_uppercase()) {
                    let val = self.builder.build_load(ty, ptr, name)
                        .map_err(|e| CompileError::codegen(format!("Failed to load: {}", e)))?;
                    return Ok(Some(val));
                }

                // Check variables
                if let Some((ptr, ty)) = self.variables.get(&name.to_uppercase()) {
                    let val = self.builder.build_load(*ty, *ptr, name)
                        .map_err(|e| CompileError::codegen(format!("Failed to load: {}", e)))?;
                    return Ok(Some(val));
                }

                Err(CompileError::codegen(format!("Undefined variable: {}", name)))
            }

            Expr::Status { value, .. } => {
                // Status values are just integers (enum values)
                // For now, hash the name to get a unique value
                let hash = value.to_uppercase().bytes().fold(0i64, |acc, b| acc.wrapping_mul(31).wrapping_add(b as i64));
                let val = self.context.i32_type().const_int(hash as u64, false);
                Ok(Some(val.into()))
            }

            Expr::Binary { op, left, right, .. } => {
                let lhs = self.generate_expression(left)?
                    .ok_or_else(|| CompileError::codegen("Binary left has no result"))?;
                let rhs = self.generate_expression(right)?
                    .ok_or_else(|| CompileError::codegen("Binary right has no result"))?;

                let result = if lhs.is_int_value() && rhs.is_int_value() {
                    let l = lhs.into_int_value();
                    let r = rhs.into_int_value();

                    // Cast to same size
                    let r = self.builder.build_int_cast(r, l.get_type(), "cast")
                        .map_err(|e| CompileError::codegen(format!("Failed to cast: {}", e)))?;

                    match op {
                        BinaryOp::Add => self.builder.build_int_add(l, r, "add"),
                        BinaryOp::Sub => self.builder.build_int_sub(l, r, "sub"),
                        BinaryOp::Mul => self.builder.build_int_mul(l, r, "mul"),
                        BinaryOp::Div => self.builder.build_int_signed_div(l, r, "div"),
                        BinaryOp::Mod => self.builder.build_int_signed_rem(l, r, "mod"),
                        BinaryOp::Eq => {
                            let cmp = self.builder.build_int_compare(IntPredicate::EQ, l, r, "eq")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Ne => {
                            let cmp = self.builder.build_int_compare(IntPredicate::NE, l, r, "ne")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Lt => {
                            let cmp = self.builder.build_int_compare(IntPredicate::SLT, l, r, "lt")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Le => {
                            let cmp = self.builder.build_int_compare(IntPredicate::SLE, l, r, "le")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Gt => {
                            let cmp = self.builder.build_int_compare(IntPredicate::SGT, l, r, "gt")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Ge => {
                            let cmp = self.builder.build_int_compare(IntPredicate::SGE, l, r, "ge")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::And => self.builder.build_and(l, r, "and"),
                        BinaryOp::Or => self.builder.build_or(l, r, "or"),
                        BinaryOp::Xor => self.builder.build_xor(l, r, "xor"),
                        BinaryOp::Power => {
                            // Convert to f64, call pow, convert back
                            let lf = self.builder.build_signed_int_to_float(l, self.context.f64_type(), "ltof")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            let rf = self.builder.build_signed_int_to_float(r, self.context.f64_type(), "rtof")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            let pow_fn = self.functions.get("POW")
                                .ok_or_else(|| CompileError::codegen("POW not declared"))?;
                            let result = self.builder.build_call(*pow_fn, &[lf.into(), rf.into()], "pow")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                                .try_as_basic_value().left()
                                .ok_or_else(|| CompileError::codegen("POW returned void"))?;
                            return Ok(Some(result));
                        }
                        _ => return Err(CompileError::codegen("Unsupported binary op")),
                    }.map_err(|e| CompileError::codegen(format!("Failed to build op: {}", e)))?
                     .into()
                } else if lhs.is_float_value() || rhs.is_float_value() {
                    // Float operations
                    let l = if lhs.is_float_value() {
                        lhs.into_float_value()
                    } else {
                        self.builder.build_signed_int_to_float(
                            lhs.into_int_value(),
                            self.context.f64_type(),
                            "itof",
                        ).map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                    };
                    let r = if rhs.is_float_value() {
                        rhs.into_float_value()
                    } else {
                        self.builder.build_signed_int_to_float(
                            rhs.into_int_value(),
                            self.context.f64_type(),
                            "itof",
                        ).map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                    };

                    match op {
                        BinaryOp::Add => self.builder.build_float_add(l, r, "fadd"),
                        BinaryOp::Sub => self.builder.build_float_sub(l, r, "fsub"),
                        BinaryOp::Mul => self.builder.build_float_mul(l, r, "fmul"),
                        BinaryOp::Div => self.builder.build_float_div(l, r, "fdiv"),
                        BinaryOp::Lt => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::OLT, l, r, "flt")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Le => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::OLE, l, r, "fle")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Gt => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::OGT, l, r, "fgt")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Ge => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::OGE, l, r, "fge")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            return Ok(Some(cmp.into()));
                        }
                        BinaryOp::Power => {
                            let pow_fn = self.functions.get("POW")
                                .ok_or_else(|| CompileError::codegen("POW not declared"))?;
                            let result = self.builder.build_call(*pow_fn, &[l.into(), r.into()], "fpow")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                                .try_as_basic_value().left()
                                .ok_or_else(|| CompileError::codegen("POW returned void"))?;
                            return Ok(Some(result));
                        }
                        _ => return Err(CompileError::codegen("Unsupported float binary op")),
                    }.map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                     .into()
                } else {
                    return Err(CompileError::codegen("Invalid binary operand types"));
                };

                Ok(Some(result))
            }

            Expr::Unary { op, operand, .. } => {
                let val = self.generate_expression(operand)?
                    .ok_or_else(|| CompileError::codegen("Unary operand has no result"))?;

                let result = match op {
                    UnaryOp::Neg => {
                        if val.is_int_value() {
                            self.builder.build_int_neg(val.into_int_value(), "neg")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                                .into()
                        } else {
                            self.builder.build_float_neg(val.into_float_value(), "fneg")
                                .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                                .into()
                        }
                    }
                    UnaryOp::Not => {
                        self.builder.build_not(val.into_int_value(), "not")
                            .map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?
                            .into()
                    }
                    UnaryOp::Pos => val,
                };

                Ok(Some(result))
            }

            Expr::Call { name, args, output_args, .. } => {
                // Special handling for PRINT built-in
                if name.to_uppercase() == "PRINT" {
                    return self.generate_print(args);
                }

                // Special handling for READ built-in
                if name.to_uppercase() == "READ" {
                    return self.generate_read(args);
                }

                let function = self.functions.get(&name.to_uppercase())
                    .copied()
                    .ok_or_else(|| CompileError::codegen(format!("Unknown function: {}", name)))?;

                let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();

                for arg in args {
                    if let Some(val) = self.generate_expression(arg)? {
                        call_args.push(val.into());
                    }
                }

                for arg in output_args {
                    let ptr = self.generate_lvalue(arg)?;
                    call_args.push(ptr.into());
                }

                let call = self.builder.build_call(function, &call_args, "call")
                    .map_err(|e| CompileError::codegen(format!("Failed to build call: {}", e)))?;

                Ok(call.try_as_basic_value().left())
            }

            _ => Ok(None),
        }
    }

    // =========================================================================
    // PRINT Built-in
    // =========================================================================

    fn generate_print(&mut self, args: &[Expr]) -> Result<Option<BasicValueEnum<'ctx>>> {
        let printf_fn = self.functions.get("PRINTF")
            .copied()
            .ok_or_else(|| CompileError::codegen("printf not declared"))?;

        for arg in args {
            // Get the value to print
            let val = self.generate_expression(arg)?;

            // Determine format string based on type
            let (fmt_global_name, call_args): (&str, Vec<BasicMetadataValueEnum<'ctx>>) = match arg {
                Expr::String { value, .. } => {
                    // For string literals, create a global string and print it
                    let str_val = self.context.const_string(value.as_bytes(), true);
                    let str_global = self.module.add_global(str_val.get_type(), None, ".str");
                    str_global.set_initializer(&str_val);
                    str_global.set_constant(true);
                    (".fmt_str", vec![str_global.as_pointer_value().into()])
                }
                _ => {
                    if let Some(v) = val {
                        if v.is_int_value() {
                            let int_val = v.into_int_value();
                            let bit_width = int_val.get_type().get_bit_width();
                            if bit_width <= 32 {
                                (".fmt_int", vec![int_val.into()])
                            } else {
                                (".fmt_i64", vec![int_val.into()])
                            }
                        } else if v.is_float_value() {
                            // Convert float to double for printf
                            let float_val = v.into_float_value();
                            let double_val = self.builder.build_float_ext(
                                float_val,
                                self.context.f64_type(),
                                "ftod"
                            ).map_err(|e| CompileError::codegen(format!("Failed: {}", e)))?;
                            (".fmt_float", vec![double_val.into()])
                        } else {
                            return Err(CompileError::codegen("Unsupported type for PRINT"));
                        }
                    } else {
                        return Err(CompileError::codegen("PRINT argument has no value"));
                    }
                }
            };

            // Get format string pointer
            let fmt_global = self.module.get_global(fmt_global_name)
                .ok_or_else(|| CompileError::codegen(format!("Format string {} not found", fmt_global_name)))?;
            let fmt_ptr = fmt_global.as_pointer_value();

            // Build printf call
            let mut printf_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![fmt_ptr.into()];
            printf_args.extend(call_args);

            self.builder.build_call(printf_fn, &printf_args, "printf")
                .map_err(|e| CompileError::codegen(format!("Failed to call printf: {}", e)))?;
        }

        Ok(None)
    }

    // =========================================================================
    // READ Built-in
    // =========================================================================

    fn generate_read(&mut self, args: &[Expr]) -> Result<Option<BasicValueEnum<'ctx>>> {
        let scanf_fn = self.functions.get("SCANF")
            .copied()
            .ok_or_else(|| CompileError::codegen("scanf not declared"))?;

        for arg in args {
            // READ takes variable names to read into
            if let Expr::Ident { name, .. } = arg {
                // Get the variable's pointer and type
                let (ptr, ty) = if let Some((p, t)) = self.variables.get(&name.to_uppercase()) {
                    (*p, *t)
                } else {
                    return Err(CompileError::codegen(format!("Undefined variable for READ: {}", name)));
                };

                // Determine format string based on type
                let fmt_name = match ty {
                    BasicTypeEnum::IntType(_) => ".rfmt_int",
                    BasicTypeEnum::FloatType(_) => ".rfmt_float",
                    _ => return Err(CompileError::codegen("Unsupported type for READ")),
                };

                let fmt_global = self.module.get_global(fmt_name)
                    .ok_or_else(|| CompileError::codegen(format!("Format string {} not found", fmt_name)))?;
                let fmt_ptr = fmt_global.as_pointer_value();

                // Build scanf call
                self.builder.build_call(scanf_fn, &[fmt_ptr.into(), ptr.into()], "scanf")
                    .map_err(|e| CompileError::codegen(format!("Failed to call scanf: {}", e)))?;
            } else {
                return Err(CompileError::codegen("READ argument must be a variable"));
            }
        }

        Ok(None)
    }

    // =========================================================================
    // L-value Generation (for assignment targets)
    // =========================================================================

    fn generate_lvalue(&mut self, expr: &Expr) -> Result<PointerValue<'ctx>> {
        match expr {
            Expr::Ident { name, .. } => {
                if let Some((ptr, _)) = self.variables.get(&name.to_uppercase()) {
                    Ok(*ptr)
                } else {
                    Err(CompileError::codegen(format!("Undefined variable: {}", name)))
                }
            }
            _ => Err(CompileError::codegen("Invalid l-value")),
        }
    }

    fn get_lvalue_with_type(&mut self, expr: &Expr) -> Result<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match expr {
            Expr::Ident { name, .. } => {
                if let Some((ptr, ty)) = self.variables.get(&name.to_uppercase()) {
                    Ok((*ptr, *ty))
                } else {
                    Err(CompileError::codegen(format!("Undefined variable: {}", name)))
                }
            }
            _ => Err(CompileError::codegen("Invalid l-value")),
        }
    }

    /// Cast a value to a target type
    fn cast_value(&self, val: BasicValueEnum<'ctx>, target_ty: BasicTypeEnum<'ctx>) -> Result<BasicValueEnum<'ctx>> {
        // If types match, no cast needed
        if val.get_type() == target_ty {
            return Ok(val);
        }

        match (val, target_ty) {
            // Int to int cast
            (BasicValueEnum::IntValue(v), BasicTypeEnum::IntType(t)) => {
                let casted = self.builder.build_int_cast(v, t, "cast")
                    .map_err(|e| CompileError::codegen(format!("Failed to cast: {}", e)))?;
                Ok(casted.into())
            }
            // Float to float cast
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::FloatType(t)) => {
                // Check if we need to extend or truncate
                let src_is_f32 = v.get_type() == self.context.f32_type();
                let dst_is_f64 = t == self.context.f64_type();
                let casted = if src_is_f32 && dst_is_f64 {
                    self.builder.build_float_ext(v, t, "fext")
                } else {
                    self.builder.build_float_trunc(v, t, "ftrunc")
                }.map_err(|e| CompileError::codegen(format!("Failed to cast: {}", e)))?;
                Ok(casted.into())
            }
            // Int to float
            (BasicValueEnum::IntValue(v), BasicTypeEnum::FloatType(t)) => {
                let casted = self.builder.build_signed_int_to_float(v, t, "itof")
                    .map_err(|e| CompileError::codegen(format!("Failed to cast: {}", e)))?;
                Ok(casted.into())
            }
            // Float to int
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::IntType(t)) => {
                let casted = self.builder.build_float_to_signed_int(v, t, "ftoi")
                    .map_err(|e| CompileError::codegen(format!("Failed to cast: {}", e)))?;
                Ok(casted.into())
            }
            _ => {
                // For other cases, just return the value and hope LLVM figures it out
                Ok(val)
            }
        }
    }

    // =========================================================================
    // Constant Expression Evaluation
    // =========================================================================

    fn const_expr_to_int(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Integer { value, .. } => Some(*value),
            Expr::Ident { name, .. } => self.defines.get(&name.to_uppercase()).copied(),
            _ => None,
        }
    }

    fn const_expr_to_float(&self, expr: &Expr) -> Option<f64> {
        match expr {
            Expr::Float { value, .. } => Some(*value),
            Expr::Integer { value, .. } => Some(*value as f64),
            _ => None,
        }
    }

    // =========================================================================
    // Utilities
    // =========================================================================

    fn translate_name(name: &str) -> String {
        name.replace('\'', "_").to_lowercase()
    }
}

/// Generate LLVM IR for a program
pub fn generate_llvm(program: &Program) -> Result<String> {
    let context = Context::create();
    let mut codegen = LLVMCodeGenerator::new(&context, &program.name);
    codegen.generate(program)?;
    Ok(codegen.get_ir())
}

/// Compile to object file
pub fn compile_to_object(program: &Program, output_path: &Path) -> Result<()> {
    let context = Context::create();
    let mut codegen = LLVMCodeGenerator::new(&context, &program.name);
    codegen.generate(program)?;
    codegen.write_object_file(output_path)
}
