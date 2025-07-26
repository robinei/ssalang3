use crate::TokenType;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub start: u32,
    pub length: u32,
}

impl Token {
    pub fn new(token_type: TokenType, start: u32, length: u32) -> Self {
        Self {
            token_type,
            start,
            length,
        }
    }
}
