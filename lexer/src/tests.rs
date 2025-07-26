use super::*;

#[test]
fn test_basic_tokens() {
    let mut lexer = Lexer::new("let mut fn if else while struct");
    let tokens = lexer.tokenize();

    assert_eq!(tokens.len(), 8); // 7 tokens + EOF
    assert_eq!(tokens[0].token_type, TokenType::Let);
    assert_eq!(tokens[1].token_type, TokenType::Mut);
    assert_eq!(tokens[2].token_type, TokenType::Fn);
    assert_eq!(tokens[3].token_type, TokenType::If);
    assert_eq!(tokens[4].token_type, TokenType::Else);
    assert_eq!(tokens[5].token_type, TokenType::While);
    assert_eq!(tokens[6].token_type, TokenType::Struct);
    assert_eq!(tokens[7].token_type, TokenType::Eof);
}

#[test]
fn test_operators() {
    let mut lexer = Lexer::new("+ - * / == != = -> & | && || ! < > <= >=");
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::Plus);
    assert_eq!(tokens[1].token_type, TokenType::Minus);
    assert_eq!(tokens[2].token_type, TokenType::Star);
    assert_eq!(tokens[3].token_type, TokenType::Slash);
    assert_eq!(tokens[4].token_type, TokenType::Equal);
    assert_eq!(tokens[5].token_type, TokenType::NotEqual);
    assert_eq!(tokens[6].token_type, TokenType::Assign);
    assert_eq!(tokens[7].token_type, TokenType::Arrow);
    assert_eq!(tokens[8].token_type, TokenType::BitAnd);
    assert_eq!(tokens[9].token_type, TokenType::BitOr);
    assert_eq!(tokens[10].token_type, TokenType::And);
    assert_eq!(tokens[11].token_type, TokenType::Or);
    assert_eq!(tokens[12].token_type, TokenType::Not);
    assert_eq!(tokens[13].token_type, TokenType::Lt);
    assert_eq!(tokens[14].token_type, TokenType::Gt);
    assert_eq!(tokens[15].token_type, TokenType::LtEq);
    assert_eq!(tokens[16].token_type, TokenType::GtEq);
    assert_eq!(tokens[17].token_type, TokenType::Eof);
}

#[test]
fn test_boolean_operators() {
    let mut lexer = Lexer::new("&& || ! != ==");
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::And);
    assert_eq!(tokens[1].token_type, TokenType::Or);
    assert_eq!(tokens[2].token_type, TokenType::Not);
    assert_eq!(tokens[3].token_type, TokenType::NotEqual);
    assert_eq!(tokens[4].token_type, TokenType::Equal);
    assert_eq!(tokens[5].token_type, TokenType::Eof);
}

#[test]
fn test_bitwise_operators() {
    let mut lexer = Lexer::new("& | && ||");
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::BitAnd);
    assert_eq!(tokens[1].token_type, TokenType::BitOr);
    assert_eq!(tokens[2].token_type, TokenType::And);
    assert_eq!(tokens[3].token_type, TokenType::Or);
    assert_eq!(tokens[4].token_type, TokenType::Eof);
}

#[test]
fn test_mixed_bitwise_and_logical() {
    let mut lexer = Lexer::new("& && | || &| |&");
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::BitAnd);
    assert_eq!(tokens[1].token_type, TokenType::And);
    assert_eq!(tokens[2].token_type, TokenType::BitOr);
    assert_eq!(tokens[3].token_type, TokenType::Or);
    assert_eq!(tokens[4].token_type, TokenType::BitAnd);
    assert_eq!(tokens[5].token_type, TokenType::BitOr);
    assert_eq!(tokens[6].token_type, TokenType::BitOr);
    assert_eq!(tokens[7].token_type, TokenType::BitAnd);
    assert_eq!(tokens[8].token_type, TokenType::Eof);
}

#[test]
fn test_comparison_operators() {
    let mut lexer = Lexer::new("< > <= >= == !=");
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::Lt);
    assert_eq!(tokens[1].token_type, TokenType::Gt);
    assert_eq!(tokens[2].token_type, TokenType::LtEq);
    assert_eq!(tokens[3].token_type, TokenType::GtEq);
    assert_eq!(tokens[4].token_type, TokenType::Equal);
    assert_eq!(tokens[5].token_type, TokenType::NotEqual);
    assert_eq!(tokens[6].token_type, TokenType::Eof);
}

#[test]
fn test_literals() {
    let mut lexer = Lexer::new(r#"42 true false "hello world""#);
    let tokens = lexer.tokenize();

    assert_eq!(tokens[0].token_type, TokenType::IntLiteral);
    assert_eq!(lexer.get_token_text(&tokens[0]), "42");
    assert_eq!(tokens[1].token_type, TokenType::True);
    assert_eq!(tokens[2].token_type, TokenType::False);
    assert_eq!(tokens[3].token_type, TokenType::StringLiteral);
    assert_eq!(lexer.get_token_text(&tokens[3]), r#""hello world""#);
}

#[test]
fn test_function_syntax() {
    let mut lexer = Lexer::new("fn main() -> () { }");
    let tokens = lexer.tokenize();

    let expected = vec![
        TokenType::Fn,
        TokenType::Identifier,
        TokenType::LeftParen,
        TokenType::RightParen,
        TokenType::Arrow,
        TokenType::LeftParen,
        TokenType::RightParen,
        TokenType::LeftBrace,
        TokenType::RightBrace,
        TokenType::Eof,
    ];

    for (i, expected_type) in expected.iter().enumerate() {
        assert_eq!(&tokens[i].token_type, expected_type);
    }

    // Test identifier text extraction
    assert_eq!(lexer.get_token_text(&tokens[1]), "main");
}
