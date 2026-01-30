//! JOVIAL J73 Lexer
//!
//! Tokenizes JOVIAL source code according to MIL-STD-1589C.

use logos::Logos;

use crate::error::Span;

/// JOVIAL J73 tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r]+")]  // Skip whitespace (but not newlines)
pub enum Token {
    // === Literals ===

    /// Integer literal (decimal or based)
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    #[regex(r"[0-9]+B[01]+", parse_based_int)]  // Binary: 2B1010
    #[regex(r"[0-9]+O[0-7]+", parse_based_int)]  // Octal: 8O777
    #[regex(r"[0-9]+H[0-9A-Fa-f]+", parse_based_int)]  // Hex: 16HFF
    Integer(i64),

    /// Float literal
    #[regex(r"[0-9]+\.[0-9]+([Ee][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    #[regex(r"[0-9]+[Ee][+-]?[0-9]+", |lex| lex.slice().parse().ok())]
    Float(f64),

    /// String literal 'text'
    #[regex(r"'[^']*'", |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),

    // === Identifiers and Keywords ===

    /// Identifier (may contain apostrophes like CURRENT'WAYPOINT)
    #[regex(r"[A-Za-z$][A-Za-z0-9$']*", |lex| lex.slice().to_uppercase())]
    Identifier(String),

    // === Operators ===

    #[token(":=")]
    Assign,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("**")]
    Power,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("<>")]
    Ne,

    #[token("<=")]
    Le,

    #[token(">=")]
    Ge,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("=")]
    Eq,

    #[token("@")]
    At,

    // === Delimiters ===

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(".")]
    Dot,

    // === Comments and Newlines ===

    /// Comment (text in double quotes)
    #[regex(r#""[^"]*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    Comment(String),

    #[token("\n")]
    Newline,
}

/// Parse a based integer (2B1010, 8O777, 16HFF)
fn parse_based_int(lex: &mut logos::Lexer<Token>) -> Option<i64> {
    let s = lex.slice();
    // Find the base indicator (B, O, or H)
    let base_pos = s.find(|c| c == 'B' || c == 'O' || c == 'H')?;
    let base: u32 = s[..base_pos].parse().ok()?;
    let digits = &s[base_pos + 1..];
    i64::from_str_radix(digits, base).ok()
}

impl Token {
    /// Check if this identifier is a keyword
    pub fn is_keyword(&self) -> bool {
        match self {
            Token::Identifier(name) => KEYWORDS.contains(&name.as_str()),
            _ => false,
        }
    }

    /// Get keyword if this is one
    pub fn as_keyword(&self) -> Option<Keyword> {
        match self {
            Token::Identifier(name) => Keyword::from_str(name),
            _ => None,
        }
    }
}

/// JOVIAL J73 keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    // Control flow
    Abort, Begin, By, Case, Default, Else, End, Exit, Fallthru,
    For, Goto, If, Return, Stop, Term, Then, While,

    // Declarations
    Block, Compool, Constant, Def, Define, Entry, Inline, Instance,
    Item, Label, Like, Overlay, Parallel, Pos, Proc, Program,
    Ref, Rent, Rep, Start, Static, Table, Type, Zone,

    // Types
    Bit, Byte, CharOne, Fixed, Float, HolrOne, Pointer, Round,
    Signed, Status, Truncate, Unsigned, V, WordSize,

    // Built-in functions
    Abs, BitSize, ByteSize, First, Last, Loc, LBound, UBound,
    Next, NWdsEn, ShiftL, ShiftR, Sgn, Size, Sqrt, Nent,

    // Logical operators
    And, Or, Not, Xor, Eqv, Mod,

    // I/O
    Close, Open, Read, Write, IoErr, Null,

    // Compiler directives
    Coml, Copy, Delete, NoPack, NoRef, Pack, Seq, SRef, WRef,

    // Single letter type indicators
    S, U, F, B, C, H, A, D,
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            // Control flow
            "ABORT" => Some(Self::Abort),
            "BEGIN" => Some(Self::Begin),
            "BY" => Some(Self::By),
            "CASE" => Some(Self::Case),
            "DEFAULT" => Some(Self::Default),
            "ELSE" => Some(Self::Else),
            "END" => Some(Self::End),
            "EXIT" => Some(Self::Exit),
            "FALLTHRU" => Some(Self::Fallthru),
            "FOR" => Some(Self::For),
            "GOTO" => Some(Self::Goto),
            "IF" => Some(Self::If),
            "RETURN" => Some(Self::Return),
            "STOP" => Some(Self::Stop),
            "TERM" => Some(Self::Term),
            "THEN" => Some(Self::Then),
            "WHILE" => Some(Self::While),

            // Declarations
            "BLOCK" => Some(Self::Block),
            "COMPOOL" => Some(Self::Compool),
            "CONSTANT" => Some(Self::Constant),
            "DEF" => Some(Self::Def),
            "DEFINE" => Some(Self::Define),
            "ENTRY" => Some(Self::Entry),
            "INLINE" => Some(Self::Inline),
            "INSTANCE" => Some(Self::Instance),
            "ITEM" => Some(Self::Item),
            "LABEL" => Some(Self::Label),
            "LIKE" => Some(Self::Like),
            "OVERLAY" => Some(Self::Overlay),
            "PARALLEL" => Some(Self::Parallel),
            "POS" => Some(Self::Pos),
            "PROC" => Some(Self::Proc),
            "PROGRAM" => Some(Self::Program),
            "REF" => Some(Self::Ref),
            "RENT" => Some(Self::Rent),
            "REP" => Some(Self::Rep),
            "START" => Some(Self::Start),
            "STATIC" => Some(Self::Static),
            "TABLE" => Some(Self::Table),
            "TYPE" => Some(Self::Type),
            "ZONE" => Some(Self::Zone),

            // Types
            "BIT" => Some(Self::Bit),
            "BYTE" => Some(Self::Byte),
            "CHARONE" => Some(Self::CharOne),
            "FIXED" => Some(Self::Fixed),
            "FLOAT" => Some(Self::Float),
            "HOLRONE" => Some(Self::HolrOne),
            "POINTER" => Some(Self::Pointer),
            "ROUND" => Some(Self::Round),
            "SIGNED" => Some(Self::Signed),
            "STATUS" => Some(Self::Status),
            "TRUNCATE" => Some(Self::Truncate),
            "UNSIGNED" => Some(Self::Unsigned),
            "V" => Some(Self::V),
            "WORDSIZE" => Some(Self::WordSize),

            // Built-in functions
            "ABS" => Some(Self::Abs),
            "BITSIZE" => Some(Self::BitSize),
            "BYTESIZE" => Some(Self::ByteSize),
            "FIRST" => Some(Self::First),
            "LAST" => Some(Self::Last),
            "LOC" => Some(Self::Loc),
            "LBOUND" => Some(Self::LBound),
            "UBOUND" => Some(Self::UBound),
            "NEXT" => Some(Self::Next),
            "NWDSEN" => Some(Self::NWdsEn),
            "SHIFTL" => Some(Self::ShiftL),
            "SHIFTR" => Some(Self::ShiftR),
            "SGN" => Some(Self::Sgn),
            "SIZE" => Some(Self::Size),
            "SQRT" => Some(Self::Sqrt),
            "NENT" => Some(Self::Nent),

            // Logical operators
            "AND" => Some(Self::And),
            "OR" => Some(Self::Or),
            "NOT" => Some(Self::Not),
            "XOR" => Some(Self::Xor),
            "EQV" => Some(Self::Eqv),
            "MOD" => Some(Self::Mod),

            // I/O
            "CLOSE" => Some(Self::Close),
            "OPEN" => Some(Self::Open),
            "READ" => Some(Self::Read),
            "WRITE" => Some(Self::Write),
            "IOERR" => Some(Self::IoErr),
            "NULL" => Some(Self::Null),

            // Compiler directives
            "COML" => Some(Self::Coml),
            "COPY" => Some(Self::Copy),
            "DELETE" => Some(Self::Delete),
            "NOPACK" => Some(Self::NoPack),
            "NOREF" => Some(Self::NoRef),
            "PACK" => Some(Self::Pack),
            "SEQ" => Some(Self::Seq),
            "SREF" => Some(Self::SRef),
            "WREF" => Some(Self::WRef),

            // Single letter type indicators
            "S" => Some(Self::S),
            "U" => Some(Self::U),
            "F" => Some(Self::F),
            "B" => Some(Self::B),
            "C" => Some(Self::C),
            "H" => Some(Self::H),
            "A" => Some(Self::A),
            "D" => Some(Self::D),

            _ => None,
        }
    }
}

/// Complete list of JOVIAL J73 keywords
pub const KEYWORDS: &[&str] = &[
    // Control flow
    "ABORT", "BEGIN", "BY", "CASE", "DEFAULT", "ELSE", "END",
    "EXIT", "FALLTHRU", "FOR", "GOTO", "IF", "RETURN", "STOP",
    "TERM", "THEN", "WHILE",
    // Declarations
    "BLOCK", "COMPOOL", "CONSTANT", "DEF", "DEFINE", "ENTRY",
    "INLINE", "INSTANCE", "ITEM", "LABEL", "LIKE", "OVERLAY",
    "PARALLEL", "POS", "PROC", "PROGRAM", "REF", "RENT", "REP",
    "START", "STATIC", "TABLE", "TYPE", "ZONE",
    // Types
    "BIT", "BYTE", "CHARONE", "FIXED", "FLOAT", "HOLRONE",
    "POINTER", "ROUND", "SIGNED", "STATUS", "TRUNCATE", "UNSIGNED",
    "V", "WORDSIZE",
    // Built-in functions
    "ABS", "BITSIZE", "BYTESIZE", "FIRST", "LAST", "LOC",
    "LBOUND", "UBOUND", "NEXT", "NWDSEN", "SHIFTL", "SHIFTR",
    "SGN", "SIZE", "SQRT", "NENT",
    // Logical operators
    "AND", "OR", "NOT", "XOR", "EQV", "MOD",
    // I/O
    "CLOSE", "OPEN", "READ", "WRITE", "IOERR", "NULL",
    // Compiler directives
    "COML", "COPY", "DELETE", "NOPACK", "NOREF", "PACK", "SEQ", "SREF", "WREF",
    // Single letter type indicators
    "S", "U", "F", "B", "C", "H", "A", "D",
];

/// Spanned token for error reporting
#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// Tokenize source code
pub fn tokenize(source: &str) -> Vec<SpannedToken> {
    let mut tokens = Vec::new();
    let lexer = Token::lexer(source);

    for (result, span) in lexer.spanned() {
        match result {
            Ok(token) => {
                tokens.push(SpannedToken {
                    token,
                    span: span.into(),
                });
            }
            Err(_) => {
                // Skip unknown tokens for now
                // In production, we'd report an error
            }
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let tokens = tokenize("ITEM X S 16;");
        assert_eq!(tokens.len(), 5);
        assert!(matches!(&tokens[0].token, Token::Identifier(s) if s == "ITEM"));
        assert!(matches!(&tokens[1].token, Token::Identifier(s) if s == "X"));
        assert!(matches!(&tokens[2].token, Token::Identifier(s) if s == "S"));
        assert!(matches!(&tokens[3].token, Token::Integer(16)));
        assert!(matches!(&tokens[4].token, Token::Semicolon));
    }

    #[test]
    fn test_apostrophe_identifier() {
        let tokens = tokenize("CURRENT'WAYPOINT");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].token, Token::Identifier(s) if s == "CURRENT'WAYPOINT"));
    }

    #[test]
    fn test_operators() {
        let tokens = tokenize("X := Y + Z ** 2;");
        assert!(matches!(&tokens[1].token, Token::Assign));
        assert!(matches!(&tokens[3].token, Token::Plus));
        assert!(matches!(&tokens[5].token, Token::Power));
    }
}
