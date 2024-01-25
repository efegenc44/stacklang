use std::iter::Peekable;

use crate::tokens::{Token, Tokens};

pub struct Parser<'tokens> {
    tokens: Peekable<Tokens<'tokens>>,
}

impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: Tokens<'tokens>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        let Some(_) = self.tokens.next_if_eq(&expected) else {
            return Err(match self.tokens.peek() {
                Some(_) => ParseError::UnexpectedToken,
                None    => ParseError::UnexpectedEOF,
            })
        };
        Ok(())
    }

    fn expect_word(&mut self) -> ParseResult<String> {
        let next = self.tokens.next();
        let Some(Token::Word(word)) = next else {
            return Err(match next {
                Some(_) => ParseError::UnexpectedToken,
                None    => ParseError::UnexpectedEOF,
            })
        };
        Ok(word)
    }

    fn type_expr(&mut self) -> ParseResult<TypeExpr> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEOF);
        };

        match token {
            Token::Word(word) => Ok(TypeExpr::Word(word)),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn constructor(&mut self) -> ParseResult<Constructor> {
        self.expect(Token::Bar)?;
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEOF);
        };

        match token {
            Token::Word(name) => {
                let mut argument_types = vec![];
                if let Some(Token::OpeningParenthesis) = self.tokens.peek() {
                    self.tokens.next();
                    while !matches!(self.tokens.peek(), Some(Token::ClosingParenthesis)) {
                        argument_types.push(self.type_expr()?);
                    }
                    self.expect(Token::ClosingParenthesis)?;
                }
                Ok(Constructor {
                    name,
                    argument_types,
                })
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn data(&mut self) -> ParseResult<TopLevel> {
        self.expect(Token::KeywordData)?;
        let name = self.expect_word()?;
        let mut constructors = vec![];
        while let Some(Token::Bar) = self.tokens.peek() {
            constructors.push(self.constructor()?);
        }
        Ok(TopLevel::Data { name, constructors })
    }

    fn pattern(&mut self) -> ParseResult<Pattern> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEOF);
        };

        match token {
            Token::Word(name) => Ok(if name.chars().next().unwrap().is_lowercase() {
                Pattern::All(name)
            } else {
                let mut arguments = vec![];
                if let Some(Token::OpeningParenthesis) = self.tokens.peek() {
                    self.tokens.next();
                    while !matches!(self.tokens.peek(), Some(Token::ClosingParenthesis)) {
                        arguments.push(self.pattern()?);
                    }
                    self.expect(Token::ClosingParenthesis)?;
                }
                Pattern::Constructor { name, arguments }
            }),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEOF);
        };

        match token {
            Token::Word(word) => Ok(Expr::Word(word)),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn branch(&mut self) -> ParseResult<Branch> {
        self.expect(Token::Bar)?;
        let mut patterns = vec![];
        while !matches!(self.tokens.peek(), Some(Token::EqualsSign)) {
            patterns.push(self.pattern()?);
        }
        self.expect(Token::EqualsSign)?;
        let mut body = vec![];
        while let Some(Token::Word(_)) = self.tokens.peek() {
            body.push(self.expr()?);
        }
        Ok(Branch { patterns, body })
    }

    fn def(&mut self) -> ParseResult<TopLevel> {
        self.expect(Token::KeywordDef)?;
        let name = self.expect_word()?;
        let mut inputs = vec![];
        self.expect(Token::OpeningParenthesis)?;
        while !matches!(self.tokens.peek(), Some(Token::ClosingParenthesis)) {
            inputs.push(self.type_expr()?);
        }
        self.expect(Token::ClosingParenthesis)?;

        let outputs = if let Some(Token::Minus) = self.tokens.peek() {
            self.tokens.next();
            let mut outputs = vec![];
            self.expect(Token::OpeningParenthesis)?;
            while !matches!(self.tokens.peek(), Some(Token::ClosingParenthesis)) {
                outputs.push(self.type_expr()?);
            }
            self.expect(Token::ClosingParenthesis)?;
            outputs
        } else {
            vec![]
        };

        let mut branches = vec![];
        while let Some(Token::Bar) = self.tokens.peek() {
            branches.push(self.branch()?);
        }

        Ok(TopLevel::Def {
            name,
            inputs,
            outputs,
            branches,
        })
    }

    pub fn top_levels(&mut self) -> ParseResult<Vec<TopLevel>> {
        let mut top_levels = vec![];
        while let Some(token) = self.tokens.peek() {
            top_levels.push(match token {
                Token::KeywordData => self.data()?,
                Token::KeywordDef => self.def()?,
                _ => return Err(ParseError::UnexpectedToken),
            })
        }
        Ok(top_levels)
    }
}

type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken,
    UnexpectedEOF,
}

#[derive(Debug)]
pub enum TopLevel {
    Data {
        name: String,
        constructors: Vec<Constructor>,
    },
    Def {
        name: String,
        inputs: Vec<TypeExpr>,
        outputs: Vec<TypeExpr>,
        branches: Vec<Branch>,
    },
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub patterns: Vec<Pattern>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Word(String),
}

#[derive(Clone, Debug)]
pub enum Pattern {
    All(String),
    Constructor {
        name: String,
        arguments: Vec<Pattern>,
    },
}

#[derive(Debug)]
pub struct Constructor {
    pub name: String,
    pub argument_types: Vec<TypeExpr>,
}

#[derive(Debug)]
pub enum TypeExpr {
    Word(String),
}
