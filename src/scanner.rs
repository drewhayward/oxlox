use core::panic;
use std::iter::Peekable;
use std::str::{self, CharIndices};

#[derive(Debug, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // 1-2 chars
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
}

pub struct Token {
    pub ttype: TokenType,
    pub value: Option<String>,
    pub line: u32,
}

pub struct Scanner<'a> {
    source: &'a str,
    current: Peekable<CharIndices<'a>>,
    line: u32,
    complete: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source,
            current: str::char_indices(source).peekable(),
            line: 1,
            complete: false,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, whitespace_char)) = self.current.next_if(|(_, c)| c.is_whitespace()) {
            if whitespace_char == '\n' {
                self.line += 1;
            }
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.complete {
            return None;
        }
        self.skip_whitespace();

        if let Some((_, next_char)) = self.current.next() {
            let make_token = |ttype| {
                Some(Token {
                    ttype,
                    value: None,
                    line: self.line,
                })
            };

            match next_char {
                '(' => make_token(TokenType::LeftParen),
                ')' => make_token(TokenType::RightParen),
                '{' => make_token(TokenType::LeftBrace),
                '}' => make_token(TokenType::RightBrace),
                ';' => make_token(TokenType::Semicolon),
                ',' => make_token(TokenType::Comma),
                '.' => make_token(TokenType::Dot),
                '-' => make_token(TokenType::Minus),
                '+' => make_token(TokenType::Plus),
                '/' => make_token(TokenType::Slash),
                '*' => make_token(TokenType::Star),
                _ => panic!("Unknown char '{next_char}'"),
            }
        } else {
            self.complete = true;
            Some(Token {
                ttype: TokenType::Eof,
                value: None,
                line: self.line,
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_returns_eof() {
        let source = "";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(vec![TokenType::Eof], tokens);
    }

    #[test]
    fn it_consumes_whitespace() {
        let source = "  \t\n  ";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(vec![TokenType::Eof], tokens);
    }

    #[test]
    fn it_processes_single_character_tokens() {
        let source = "(){};,.-+/*";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(
            vec![
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Semicolon,
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Minus,
                TokenType::Plus,
                TokenType::Slash,
                TokenType::Star,
                TokenType::Eof
            ],
            tokens
        )
    }

    #[test]
    #[should_panic]
    fn it_panics_for_unknown_chars() {
        let source = "()%()";
        let scanner = Scanner::new(source);
        let _tokens: Vec<_> = scanner.map(|t| t.ttype).collect();
    }
}
