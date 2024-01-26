use std::{iter::Peekable, str::Chars};

pub struct Tokens<'chars> {
    chars: Peekable<Chars<'chars>>,
}

impl<'chars> Tokens<'chars> {
    pub fn new(source: &'chars str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    fn word_or_keyword(&mut self) -> Token {
        let mut word = String::new();
        while let Some(ch) = self
            .chars
            .next_if(|ch| !(ch.is_whitespace() || PUNCTUATION.contains(ch)))
        {
            word.push(ch);
        }
        match word.as_str() {
            "data" => Token::KeywordData,
            "def" => Token::KeywordDef,
            _ => Token::Word(word),
        }
    }
}

impl Iterator for Tokens<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(ch) = self.chars.peek() else {
            return None;
        };

        if ch.is_whitespace() {
            self.chars.next();
            self.next()
        } else if ch == &'#' {
            while !matches!(self.chars.next(), Some('\n') | None) {}
            self.next()
        } else {
            Some(match ch {
                '(' => {
                    self.chars.next();
                    Token::OpeningParenthesis
                }
                ')' => {
                    self.chars.next();
                    Token::ClosingParenthesis
                }
                '[' => {
                    self.chars.next();
                    Token::OpeningBracket
                }
                ']' => {
                    self.chars.next();
                    Token::ClosingBracket
                }
                '=' => {
                    self.chars.next();
                    Token::EqualsSign
                }
                '&' => {
                    self.chars.next();
                    Token::Ampersand
                }
                '-' => {
                    self.chars.next();
                    Token::Minus
                }
                '|' => {
                    self.chars.next();
                    Token::Bar
                }
                _ => self.word_or_keyword(),
            })
        }
    }
}

const PUNCTUATION: [char; 8] = ['[', ']', '(', ')', '=', '&', '-', '|'];
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    KeywordData,
    KeywordDef,
    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBracket,
    ClosingBracket,
    EqualsSign,
    Ampersand,
    Minus,
    Bar,
}
