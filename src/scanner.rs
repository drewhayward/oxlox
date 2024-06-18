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
    Identifier { name: String },
    String { literal: String },
    Number(f64),
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
    pub line: u32,
}

/// Implements the Iterator interface to yield Lox tokens from a Lox source string.
pub struct Scanner<'a> {
    source: &'a str,
    current: Peekable<CharIndices<'a>>,
    line: u32,
    complete: bool,
}

impl<'a> Scanner<'a> {
    /// Construct a new scanner from a source string.
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source,
            current: str::char_indices(source).peekable(),
            line: 1,
            complete: false,
        }
    }

    /// Advances the internal character iterator until non-whitespaces chars
    /// are reached and comments are consumed
    fn skip_whitespace(&mut self) {
        while let Some((pos, peeked)) = self.current.peek() {
            match peeked {
                ' ' | '\t' | '\r' => {
                    self.current.next();
                }
                '\n' => {
                    self.line += 1;
                    self.current.next();
                }
                '/' => {
                    if &self.source[*pos..*pos + 2] == "//" {
                        self.advance_to_newline();
                    } else {
                        break;
                    }
                }
                _ => break,
            };
        }
    }

    fn advance_to_newline(&mut self) {
        while let Some(_) = self.current.next_if(|(_, c)| *c != '\n') {}
    }

    fn make_string_literal(&mut self, start: usize) -> Token {
        while let Some((_, c)) = self.current.next_if(|(_, c)| *c != '"') {
            if c == '\n' {
                self.line += 1;
            }
        }

        // Consume the ending "
        let (end_pos, _) = self.current.next().expect("Unterminated string");

        Token {
            ttype: TokenType::String {
                literal: String::from(&self.source[start + 1..end_pos]),
            },
            line: self.line,
        }
    }

    fn make_number_literal(&mut self, start: usize) -> Token {
        let mut end = start;
        while let Some((pos, _)) = self.current.next_if(|(_, c)| c.is_numeric()) {
            end = pos;
        }

        if let Some(_) = self.current.next_if(|(_, c)| *c == '.') {
            while let Some((pos, _)) = self.current.next_if(|(_, c)| c.is_numeric()) {
                end = pos;
            }
        }

        
        let value_slice = &self.source[start..=end];
        let value = value_slice.parse().expect("Value should be a valid float literal.");

        Token {
            ttype: TokenType::Number(value),
            line: self.line,
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

        if let Some((pos, next_char)) = self.current.peek() {
            if next_char.is_ascii_digit() {
                let start = pos.clone();
                return Some(self.make_number_literal(start));
            }
        }

        if let Some((pos, next_char)) = self.current.next() {
            let make_token = |ttype| {
                Some(Token {
                    ttype,
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
                '!' => {
                    if let Some(_) = self.current.next_if(|(_, c)| *c == '=') {
                        make_token(TokenType::BangEqual)
                    } else {
                        make_token(TokenType::Bang)
                    }
                }
                '=' => {
                    if let Some(_) = self.current.next_if(|(_, c)| *c == '=') {
                        make_token(TokenType::EqualEqual)
                    } else {
                        make_token(TokenType::Equal)
                    }
                }
                '<' => {
                    if let Some(_) = self.current.next_if(|(_, c)| *c == '=') {
                        make_token(TokenType::LessEqual)
                    } else {
                        make_token(TokenType::Less)
                    }
                }
                '>' => {
                    if let Some(_) = self.current.next_if(|(_, c)| *c == '=') {
                        make_token(TokenType::GreaterEqual)
                    } else {
                        make_token(TokenType::Greater)
                    }
                }
                '"' => Some(self.make_string_literal(pos)),
                _ => panic!("Unknown char '{next_char}'"),
            }
        } else {
            self.complete = true;
            Some(Token {
                ttype: TokenType::Eof,
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
    fn it_consumes_comments() {
        let source = "\
        // This is a comment
        !
    ";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(vec![TokenType::Bang, TokenType::Eof], tokens);
    }

    #[test]
    fn it_yields_single_character_tokens() {
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
    fn it_yields_double_character_tokens() {
        let source = "! != = == < <= > >=";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(
            vec![
                TokenType::Bang,
                TokenType::BangEqual,
                TokenType::Equal,
                TokenType::EqualEqual,
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Eof
            ],
            tokens
        )
    }

    #[test]
    fn it_consumes_string_literals() {
        let source = "\"hello\"";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(
            vec![
                TokenType::String {
                    literal: String::from("hello")
                },
                TokenType::Eof
            ],
            tokens
        );
    }

    #[test]
    fn it_consumes_number_literals() {
        let source = "42.0 7";
        let scanner = Scanner::new(source);
        let tokens: Vec<_> = scanner.map(|t| t.ttype).collect();

        assert_eq!(
            vec![
                TokenType::Number(42.0),
                TokenType::Number(7.0),
                TokenType::Eof
            ],
            tokens
        );
    }

    #[test]
    #[should_panic]
    fn it_panics_for_unknown_chars() {
        let source = "%";
        let scanner = Scanner::new(source);
        let _tokens: Vec<_> = scanner.map(|t| t.ttype).collect();
    }
}
