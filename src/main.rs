//! JOVIAL J73 Compiler CLI
//!
//! Usage: jovialc <input.jov> [-o output] [-f format]

use std::fs;
use std::path::PathBuf;
use std::process::Command;

use clap::Parser as ClapParser;

use jovial::codegen::generate_c;
use jovial::llvm_codegen::{generate_llvm, compile_to_object};
use jovial::parser::parse;
use jovial::semantic::{analyse, transform};

#[derive(ClapParser)]
#[command(name = "jovialc")]
#[command(author = "Zane Hambly <zanehambly@gmail.com>")]
#[command(version = "0.1.0")]
#[command(about = "JOVIAL J73 compiler", long_about = None)]
struct Cli {
    /// Input JOVIAL source file
    input: PathBuf,

    /// Output file (default: stdout for text formats)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Output format
    #[arg(short, long, value_enum, default_value = "c")]
    format: OutputFormat,

    /// Skip semantic analysis
    #[arg(long)]
    no_check: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum OutputFormat {
    /// C source code (transpiler)
    C,
    /// LLVM IR text
    Llvm,
    /// Native object file (.o)
    Object,
    /// Executable (compiles and links automatically)
    Exe,
    /// AST dump (for debugging)
    Ast,
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // Read input
    let source = fs::read_to_string(&cli.input)?;

    // Parse
    let mut program = parse(&source).map_err(|e| format!("Parse error: {}", e))?;

    // Semantic analysis and transformation
    if !cli.no_check {
        analyse(&program).map_err(|e| format!("Semantic error: {}", e))?;
    }

    // Transform AST (Call -> Index for TABLEs)
    transform(&mut program).map_err(|e| format!("Transform error: {}", e))?;

    // Generate output
    match cli.format {
        OutputFormat::C => {
            let output = generate_c(&program)?;
            if let Some(path) = cli.output {
                fs::write(&path, &output)?;
                eprintln!("Wrote {}", path.display());
            } else {
                print!("{}", output);
            }
        }
        OutputFormat::Llvm => {
            let output = generate_llvm(&program)?;
            if let Some(path) = cli.output {
                fs::write(&path, &output)?;
                eprintln!("Wrote {}", path.display());
            } else {
                print!("{}", output);
            }
        }
        OutputFormat::Object => {
            let output_path = cli.output.unwrap_or_else(|| {
                cli.input.with_extension("o")
            });
            compile_to_object(&program, &output_path)?;
            eprintln!("Wrote {}", output_path.display());
        }
        OutputFormat::Exe => {
            // Compile to temp object file, then link
            let exe_ext = if cfg!(windows) { "exe" } else { "" };
            let output_path = cli.output.unwrap_or_else(|| {
                if exe_ext.is_empty() {
                    cli.input.with_extension("")
                } else {
                    cli.input.with_extension(exe_ext)
                }
            });

            // Create temp object file
            let obj_path = cli.input.with_extension("o");
            compile_to_object(&program, &obj_path)?;

            // Link with clang
            let mut cmd = Command::new("clang");
            cmd.arg(&obj_path)
               .arg("-o")
               .arg(&output_path);

            // Add math library on Unix
            #[cfg(not(windows))]
            cmd.arg("-lm");

            // Windows MSVC needs legacy_stdio_definitions for printf/scanf
            // MinGW doesn't need it - detect by checking clang's target triple
            #[cfg(windows)]
            {
                let target = Command::new("clang")
                    .arg("-dumpmachine")
                    .output()
                    .map(|o| String::from_utf8_lossy(&o.stdout).to_lowercase())
                    .unwrap_or_default();

                // MinGW targets contain "mingw" or "gnu", MSVC contains "msvc"
                let is_mingw = target.contains("mingw") || target.contains("gnu");

                if !is_mingw {
                    cmd.arg("-llegacy_stdio_definitions");
                }
            }

            let status = cmd.status()
                .map_err(|e| format!("Failed to run linker (clang): {}. Is clang installed?", e))?;

            if !status.success() {
                // Clean up object file on failure
                let _ = fs::remove_file(&obj_path);
                return Err(format!("Linker failed with exit code: {:?}", status.code()).into());
            }

            // Clean up temp object file
            let _ = fs::remove_file(&obj_path);

            eprintln!("Wrote {}", output_path.display());
        }
        OutputFormat::Ast => {
            let output = format!("{:#?}", program);
            if let Some(path) = cli.output {
                fs::write(&path, &output)?;
                eprintln!("Wrote {}", path.display());
            } else {
                print!("{}", output);
            }
        }
    }

    Ok(())
}
