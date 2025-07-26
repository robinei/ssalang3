use crate::{Token, TokenType};

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    current_char: char,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let current_char = input.chars().next().unwrap_or('\0');
        Self {
            input,
            position: 0,
            current_char,
            line: 0,
            column: 0,
        }
    }

    fn advance(&mut self) {
        if self.current_char == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        self.position += 1;
        if self.position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input.chars().nth(self.position).unwrap_or('\0');
        }
    }

    fn peek(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.position + 1).unwrap_or('\0')
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char != '\0'
            && self.current_char.is_whitespace()
            && self.current_char != '\n'
        {
            self.advance();
        }
    }

    fn read_comment(&mut self) -> u32 {
        let start_pos = self.position;
        // Read // comments
        if self.current_char == '/' && self.peek() == '/' {
            while self.current_char != '\0' && self.current_char != '\n' {
                self.advance();
            }
        }
        (self.position - start_pos) as u32
    }

    fn read_identifier(&mut self) -> u32 {
        let start_pos = self.position;

        while self.current_char != '\0'
            && (self.current_char.is_alphanumeric() || self.current_char == '_')
        {
            self.advance();
        }

        (self.position - start_pos) as u32
    }

    fn read_number(&mut self) -> u32 {
        let start_pos = self.position;

        while self.current_char != '\0' && self.current_char.is_ascii_digit() {
            self.advance();
        }

        (self.position - start_pos) as u32
    }

    fn read_string(&mut self) -> u32 {
        let start_pos = self.position;
        self.advance(); // Skip opening quote

        while self.current_char != '\0' && self.current_char != '"' {
            if self.current_char == '\\' {
                self.advance();
                if self.current_char != '\0' {
                    self.advance();
                }
            } else {
                self.advance();
            }
        }

        if self.current_char == '"' {
            self.advance(); // Skip closing quote
        }

        (self.position - start_pos) as u32
    }

    fn is_current_line_empty(&self) -> bool {
        // Look backwards from current position to find the start of the current line
        let mut pos = self.position;

        // Go back to the start of the current line (or start of file)
        while pos > 0 {
            let prev_char = self.input.chars().nth(pos - 1).unwrap_or('\0');
            if prev_char == '\n' {
                break; // Found the start of current line
            }
            pos -= 1;
        }

        // Now check if everything from pos to current position is whitespace
        while pos < self.position {
            let ch = self.input.chars().nth(pos).unwrap_or('\0');
            if !ch.is_whitespace() || ch == '\n' {
                return false; // Found non-whitespace content
            }
            pos += 1;
        }

        true // Current line contains only whitespace
    }

    fn keyword_or_identifier(&self, start_pos: usize, length: usize) -> TokenType {
        if start_pos + length > self.input.len() {
            return TokenType::Identifier;
        }

        let ident = &self.input[start_pos..start_pos + length];
        let first_char = ident.chars().next().unwrap_or('\0');

        match first_char {
            'b' => match ident {
                "bool" => TokenType::Bool,
                "break" => TokenType::Break,
                _ => TokenType::Identifier,
            },
            'c' => match ident {
                "continue" => TokenType::Continue,
                _ => TokenType::Identifier,
            },
            'e' => match ident {
                "else" => TokenType::Else,
                _ => TokenType::Identifier,
            },
            'f' => match ident {
                "fn" => TokenType::Fn,
                "false" => TokenType::False,
                _ => TokenType::Identifier,
            },
            'i' => match ident {
                "if" => TokenType::If,
                "inline" => TokenType::Inline,
                "i32" => TokenType::I32,
                _ => TokenType::Identifier,
            },
            'l' => match ident {
                "let" => TokenType::Let,
                _ => TokenType::Identifier,
            },
            'm' => match ident {
                "mut" => TokenType::Mut,
                _ => TokenType::Identifier,
            },
            'r' => match ident {
                "return" => TokenType::Return,
                _ => TokenType::Identifier,
            },
            's' => match ident {
                "static" => TokenType::Static,
                "struct" => TokenType::Struct,
                _ => TokenType::Identifier,
            },
            't' => match ident {
                "true" => TokenType::True,
                _ => TokenType::Identifier,
            },
            'u' => match ident {
                "unit" => TokenType::Unit,
                _ => TokenType::Identifier,
            },
            'w' => match ident {
                "while" => TokenType::While,
                _ => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let start_pos = self.position as u32;

        match self.current_char {
            '\0' => Token::new(TokenType::Eof, start_pos, 0),

            '\n' => {
                // Look back to see if the current line was actually empty
                if self.is_current_line_empty() {
                    self.advance(); // Skip the newline
                    Token::new(TokenType::EmptyLine, start_pos, 1)
                } else {
                    // Just a structural newline, skip it and get next token
                    self.advance();
                    self.next_token()
                }
            }

            '/' if self.peek() == '/' => {
                let length = self.read_comment();
                Token::new(TokenType::Comment, start_pos, length)
            }

            ch if ch.is_whitespace() => {
                self.skip_whitespace();
                self.next_token() // Skip whitespace and get next token
            }

            // Fast keyword/identifier lookup by first character
            'a'..='z' | 'A'..='Z' | '_' => {
                let length = self.read_identifier();
                let token_type = self.keyword_or_identifier(start_pos as usize, length as usize);
                Token::new(token_type, start_pos, length)
            }

            '0'..='9' => {
                let length = self.read_number();
                Token::new(TokenType::IntLiteral, start_pos, length)
            }

            '"' => {
                let length = self.read_string();
                Token::new(TokenType::StringLiteral, start_pos, length)
            }

            '+' => {
                self.advance();
                Token::new(TokenType::Plus, start_pos, 1)
            }

            '*' => {
                self.advance();
                Token::new(TokenType::Star, start_pos, 1)
            }

            '&' => {
                self.advance();
                if self.current_char == '&' {
                    self.advance();
                    Token::new(TokenType::And, start_pos, 2)
                } else {
                    Token::new(TokenType::BitAnd, start_pos, 1)
                }
            }

            '|' => {
                self.advance();
                if self.current_char == '|' {
                    self.advance();
                    Token::new(TokenType::Or, start_pos, 2)
                } else {
                    Token::new(TokenType::BitOr, start_pos, 1)
                }
            }

            '/' if self.peek() != '/' => {
                self.advance();
                Token::new(TokenType::Slash, start_pos, 1)
            }

            '=' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::Equal, start_pos, 2)
                } else {
                    Token::new(TokenType::Assign, start_pos, 1)
                }
            }

            '!' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::NotEqual, start_pos, 2)
                } else {
                    Token::new(TokenType::Not, start_pos, 1)
                }
            }

            '-' => {
                self.advance();
                if self.current_char == '>' {
                    self.advance();
                    Token::new(TokenType::Arrow, start_pos, 2)
                } else {
                    Token::new(TokenType::Minus, start_pos, 1)
                }
            }

            '(' => {
                self.advance();
                Token::new(TokenType::LeftParen, start_pos, 1)
            }

            ')' => {
                self.advance();
                Token::new(TokenType::RightParen, start_pos, 1)
            }

            '{' => {
                self.advance();
                Token::new(TokenType::LeftBrace, start_pos, 1)
            }

            '}' => {
                self.advance();
                Token::new(TokenType::RightBrace, start_pos, 1)
            }

            ';' => {
                self.advance();
                Token::new(TokenType::Semicolon, start_pos, 1)
            }

            ',' => {
                self.advance();
                Token::new(TokenType::Comma, start_pos, 1)
            }

            ':' => {
                self.advance();
                Token::new(TokenType::Colon, start_pos, 1)
            }

            '<' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::LtEq, start_pos, 2)
                } else {
                    Token::new(TokenType::Lt, start_pos, 1)
                }
            }

            '>' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::GtEq, start_pos, 2)
                } else {
                    Token::new(TokenType::Gt, start_pos, 1)
                }
            }

            ch => {
                panic!(
                    "Unexpected character '{}' at position {}",
                    ch, self.position
                );
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        tokens
    }

    pub fn get_token_text(&self, token: &Token) -> &str {
        if token.length == 0 {
            ""
        } else {
            let start = token.start as usize;
            let end = start + token.length as usize;
            &self.input[start..end]
        }
    }
}
