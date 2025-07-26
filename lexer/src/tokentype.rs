#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Literals
    Identifier,
    IntLiteral,
    StringLiteral,

    // Keywords
    Let,
    Mut,
    Fn,
    If,
    Else,
    While,
    Break,
    Continue,
    Return,
    Static,
    Inline,
    Struct,
    True,
    False,

    // Types
    Bool,
    I32,
    Unit,

    // Operators
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Equal,    // ==
    NotEqual, // !=
    Assign,   // =
    And,      // &&
    Or,       // ||
    Not,      // !
    BitAnd,   // &
    BitOr,    // |
    Lt,       // <
    Gt,       // >
    LtEq,     // <=
    GtEq,     // >=

    // Punctuation
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }
    Semicolon,  // ;
    Comma,      // ,
    Colon,      // :
    Arrow,      // ->

    // Formatting tokens
    Comment,   // //
    EmptyLine, // \n (empty line)

    // Special
    Eof,
}
