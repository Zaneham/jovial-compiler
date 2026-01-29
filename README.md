# JOVIAL J73 Compiler

A fully functional native compiler for JOVIAL J73 (MIL-STD-1589C), a programming language from 1959 designed for bombing things more accurately. Now you too can experience the joy of military avionics software development from your gaming PC.

## What is JOVIAL?

JOVIAL (Jules' Own Version of the International Algebraic Language) was created in 1959 because apparently FORTRAN wasn't violent enough. It was used extensively in:

- F-16 Fighting Falcon flight control systems
- B-2 Spirit stealth bomber
- Various missiles (the ones that find you)
- Other things the government won't tell us about

It's basically what happens when the Pentagon asks "what if ALGOL, but angrier?"

## Features

This compiler goes harder than it has any right to:

- **Native Code Generation** - Compiles to actual x86-64 machine code via LLVM. Not interpreted. Not transpiled. Real electrons doing real work.
- **C Transpiler** - For when you need to see what cursed code you've written in a language humans can read
- **Full J73 Support** - Variables, procedures, tables, STATUS enums, the whole military-industrial complex
- **I/O** - `PRINT` and `READ` because even fighter jets need to say hello sometimes
- **LSP Integration** - Yes, there's a Language Server. Yes, you can get IntelliSense for a language older than the moon landing.

## Installation

### Option 1: Download Prebuilt Binaries (Recommended)

Grab a release from the [Releases page](https://github.com/Zaneham/jovial/releases). Unzip. Run. Pretend it's 1959.

| Platform | File |
|----------|------|
| Windows x64 | `jovialc-x86_64-pc-windows-msvc.zip` |
| Linux x64 | `jovialc-x86_64-unknown-linux-gnu.tar.gz` |
| macOS ARM | `jovialc-aarch64-apple-darwin.tar.gz` |

### Option 2: Build From Source

For those who enjoy suffering:

```bash
# Clone this bad boy
git clone https://github.com/Zaneham/jovial

# Download LLVM 18 and tell Rust where it is
export LLVM_SYS_180_PREFIX=/path/to/llvm

# Build it
cargo build --release

# Question your life choices
```

### Requirements (for building)

- Rust (latest stable)
- LLVM 18 (set `LLVM_SYS_180_PREFIX` to your LLVM installation)
- A deep appreciation for military history
- Questionable decision-making skills

## Usage

```bash
# The easy way - compile straight to executable
jovialc myprogram.jov -f exe

# Run your 1959-era code at 2025 speeds
./myprogram

# Or specify an output name
jovialc myprogram.jov -f exe -o bomber.exe
```

### Output Formats

| Flag | Output | Use Case |
|------|--------|----------|
| `-f exe` | Executable | Just works. Compiles and links automatically. |
| `-f c` | C source code | When you want to see the horror |
| `-f llvm` | LLVM IR | When you want to see *more* horror |
| `-f object` | Native .o file | When you need manual control |
| `-f ast` | AST dump | Debugging (or masochism) |

## Hello World

```jovial
START HELLO;

PRINT('Hello from 1959!');

TERM
```

Yes, `TERM` instead of `END`. Yes, strings use single quotes. Yes, semicolons everywhere. Welcome to the past.

## Fibonacci (Because We Must)

```jovial
START FIBONACCI;

ITEM N S 32 = 10;
ITEM A S 32 = 0;
ITEM B S 32 = 1;
ITEM TEMP S 32 = 0;
ITEM I S 32 = 0;

PRINT('Fibonacci sequence:');

WHILE I < N;
    PRINT(A);
    TEMP := A + B;
    A := B;
    B := TEMP;
    I := I + 1;
END

TERM
```

Output:
```
Fibonacci sequence:
0
1
1
2
3
5
8
13
21
34
```

It runs. On your modern computer. In native code. A language designed when computers were the size of rooms is now printing Fibonacci numbers faster than the original programmers could blink.

## Fun Facts

- JOVIAL was standardised as MIL-STD-1589. There have been multiple revisions. People *cared* about this.
- The language uses `'` in identifiers like `CURRENT'WAYPOINT`. This is called an "apostrophe identifier" and it's exactly as cursed as it sounds.
- Arrays use 1-based indexing AND parentheses for access. `WAYPOINTS(5)` is array access, not a function call. The parser just has to figure it out.
- There are people alive today who wrote production JOVIAL code. Some of them are probably on LinkedIn.

## Why Does This Exist?

1. Preservation of computing history
2. A deep and concerning interest in military aviation software
3. To prove it could be done
4. I wasn't going to play video games anyway (lie)
5. Someone, somewhere, might have legacy JOVIAL code they need to compile
6. The LSP was already done and needed a friend

## Technical Details (For The Brave)

```
┌─────────────────────────────────────────────────────────────┐
│                    JOVIAL J73 Compiler                      │
├─────────────────────────────────────────────────────────────┤
│  Source (.jov)                                              │
│       ↓                                                     │
│  Lexer (Logos) → Tokens                                     │
│       ↓                                                     │
│  Parser (Recursive Descent) → AST                           │
│       ↓                                                     │
│  Semantic Analysis → Symbol Table, Type Checking            │
│       ↓                                                     │
│  LLVM Codegen (inkwell) → LLVM IR                          │
│       ↓                                                     │
│  LLVM Backend → Native Object File                          │
│       ↓                                                     │
│  Linker → Executable                                        │
│       ↓                                                     │
│  Your code runs on silicon that would blow the minds        │
│  of every engineer who worked on the original compilers     │
└─────────────────────────────────────────────────────────────┘
```

## Related Projects

If you've made it this far, you're clearly interested in languages that predate common sense. Good news: there's more where this came from.

### The Vintage LSP Collection

Because syntax highlighting is a human right, even for languages the Geneva Convention forgot:

| Project | What It Is | Why It Exists |
|---------|-----------|---------------|
| [jovial-lsp](https://github.com/Zaneham/jovial-lsp) | Language Server for JOVIAL | IntelliSense for bombing things |
| [hals-lsp](https://github.com/Zaneham/hals-lsp) | HAL/S Language Server | The Space Shuttle's favourite |
| [cms2-lsp](https://github.com/Zaneham/cms2-lsp) | CMS-2 Language Server | US Navy missiles deserve autocomplete |
| [coral66-lsp](https://github.com/Zaneham/coral66-lsp) | CORAL 66 Language Server | British military computing, stiff upper lip included |
| [mumps-lsp](https://github.com/Zaneham/mumps-lsp) | MUMPS Language Server | Healthcare IT from the shadow realm |
| [chill-lsp](https://github.com/Zaneham/chill-lsp) | CHILL Language Server | Telecom switches and existential dread |
| [racf-lsp](https://github.com/Zaneham/racf-lsp) | IBM RACF Language Server | Mainframe security, now with squiggly lines |
| [IBM-system360-lsp](https://github.com/Zaneham/IBM-system360-lsp) | System/360 COBOL & PL/I | For when you need to debug your grandfather's code |

### The Emulator Menagerie

When compilers aren't enough and you need to simulate entire extinct computers:

| Project | What It Emulates | Level of Concerning |
|---------|-----------------|---------------------|
| [Conway](https://github.com/Zaneham/Conway) | RISC-V & x86 binary translator | Moderate |
| [voyager-fds-emulator](https://github.com/Zaneham/voyager-fds-emulator) | Voyager Flight Data Subsystem | High |
| [viking-marsrover-emulator](https://github.com/Zaneham/Viking-Marsrover-emulator) | Viking Mars Lander | Very High |
| [minuteman-computer-emulator](https://github.com/Zaneham/Minuteman-computer-emulator) | Minuteman missile guidance | FBI watchlist |
| [setun70-emulator](https://github.com/Zaneham/setun70-emulator) | Soviet trinary computer | Cold War nostalgia |

### Other Sins Against Modern Computing

- [Plankalkuel](https://github.com/Zaneham/Plankalkuel) - An interpreter for the world's first programming language (1948). Yes, before FORTRAN. Yes, it works.
- [chill-compiler](https://github.com/Zaneham/chill-compiler) - Because one vintage compiler wasn't enough
- [SLATEC.jl](https://github.com/Zaneham/SLATEC.jl) - Mathematical libraries from the 70s, now in Julia
- [z390Zane](https://github.com/Zaneham/z390Zane) - Mainframe assembler and emulator. For the full experience.

*If you're seeing a pattern here, congratulations. You've identified a problem.*

## Contributing

Found a bug? Want to add a feature? Know where we can get actual F-16 source code? Open an issue.

## License

BSL-1.0 (Business Source License)

Basically: do whatever you want, just don't compete with me selling JOVIAL compilers. (If you're in a market where JOVIAL compilers are a competitive business, we should talk. I have questions.)

## Acknowledgments

- The US Department of Defense, for creating this beautiful mess
- LLVM, for making this possible without hand-writing x86 assembly
- Rust, for making me feel safe while doing deeply unsafe things
- You, for reading this far. You're my kind of weird.

---

*Built with love, confusion, and far too much coffee in Aotearoa New Zealand.*

*"She'll be right, mate." - Every engineer who ever deployed JOVIAL code to a fighter jet, probably*
