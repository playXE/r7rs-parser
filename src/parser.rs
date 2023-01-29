use std::fmt;

use crate::{
    expr::{Expr, Interner, NoIntern},
    lexer::{
        lexical_error::LexicalError,
        scanner::{Position, Scanner, TokenKind},
    },
};

use self::syntax_error::SyntaxError;

pub mod syntax_error;

pub struct Parser<'a, I: Interner = NoIntern> {
    symbols: &'a mut I,
    scanner: Scanner<'a>,
}

impl<'a, I: Interner> Parser<'a, I> {
    pub fn new(symbols: &'a mut I, source: &'a str, fold_case: bool) -> Self {
        Self {
            symbols,
            scanner: Scanner::new(source, fold_case, true),
        }
    }

    pub fn finished(&self) -> bool {
        !self.scanner.has_next()
    }

    pub fn parse(&mut self, prescan: bool) -> Result<Box<Expr<I>>, ParseError> {
        let token = self.scanner.token();
        let pos = token.pos;
        let res;
        match token.kind {
            TokenKind::Error => {
                let err = token.error_val.unwrap();
                self.scanner.next();

                return Err(ParseError::Lexical(pos, err));
            }

            TokenKind::Eof => {
                return Err(ParseError::Syntax(pos, SyntaxError::Empty));
            }

            TokenKind::HashSemi => {
                self.scanner.next();
                self.parse(true)?;

                return self.parse(true);
            }

            TokenKind::Ident => {
                let s = if self.scanner.fold_case {
                    token.str_val.to_lowercase()
                } else {
                    token.str_val.clone()
                };
                res = Box::new(Expr::Symbol(self.symbols.intern(&s)));
            }

            TokenKind::TrueLit => res = Box::new(Expr::Bool(true)),

            TokenKind::FalseLit => res = Box::new(Expr::Bool(false)),

            TokenKind::Int => {
                res = Box::new(Expr::Fixnum(token.int_val));
            }

            TokenKind::BigInt => {
                res = Box::new(Expr::BigInt(token.big_int_val.clone()));
            }

            TokenKind::Rat => res = Box::new(Expr::Rational(token.rat_val)),
            TokenKind::Float => {
                res = Box::new(Expr::Float(token.float_val));
            }

            TokenKind::BigRat => res = Box::new(Expr::BigRational(token.big_rat_val.clone())),

            TokenKind::Complex => res = Box::new(Expr::Complex(token.complex_val)),

            TokenKind::Char => {
                res = Box::new(Expr::Char(char::from_u32(token.int_val as u32).unwrap()))
            }

            TokenKind::String => res = Box::new(Expr::Str(token.str_val.clone())),

            TokenKind::LParent => {
                self.scanner.next();
                let mut exprs = vec![];
                while !self
                    .scanner
                    .has_token(&[TokenKind::Eof, TokenKind::RParent, TokenKind::Dot])
                {
                    exprs.push(self.parse(true)?);
                }

                if self.scanner.has_token(&[TokenKind::Dot]) {
                    self.scanner.next();

                    res = Expr::from_slice(exprs, self.parse(true)?);
                } else {
                    res = Expr::from_slice(exprs, Box::new(Expr::Null));
                }

                if !self.scanner.has_token(&[TokenKind::RParent]) {
                    return Err(ParseError::Syntax(
                        self.position(),
                        SyntaxError::ClosingParenthesisMissing,
                    ));
                }
            }

            TokenKind::RParent => {
                self.scanner.next();
                return Err(ParseError::Syntax(
                    pos,
                    SyntaxError::UnexpectedClosingParenthesis,
                ));
            }

            TokenKind::HashLParen => {
                self.scanner.next();

                let mut exprs = vec![];

                while !self
                    .scanner
                    .has_token(&[TokenKind::Eof, TokenKind::RParent])
                {
                    exprs.push(self.parse(true)?.datum());
                }

                if !self.scanner.has_token(&[TokenKind::RParent]) {
                    return Err(ParseError::Syntax(
                        self.position(),
                        SyntaxError::ClosingParenthesisMissing,
                    ));
                }
                res = Box::new(Expr::ImmutableVector(exprs.into_boxed_slice()))
            }

            TokenKind::HashGLParen => {
                self.scanner.next();

                let mut exprs = vec![];

                while !self
                    .scanner
                    .has_token(&[TokenKind::Eof, TokenKind::RParent])
                {
                    exprs.push(self.parse(true)?.datum());
                }

                if !self.scanner.has_token(&[TokenKind::RParent]) {
                    return Err(ParseError::Syntax(
                        self.position(),
                        SyntaxError::ClosingParenthesisMissing,
                    ));
                }
                res = Box::new(Expr::GrowableVector(exprs.into_boxed_slice()))
            }

            TokenKind::U8LParen => {
                self.scanner.next();

                let mut bytes = vec![];

                while !self
                    .scanner
                    .has_token(&[TokenKind::Eof, TokenKind::RParent])
                {
                    let number = self.scanner.token().int_val;
                    if number > 0 && number <= 255 {
                        bytes.push(number as u8);
                    } else {
                        return Err(ParseError::Syntax(
                            self.position(),
                            SyntaxError::NotAByteValue,
                        ));
                    }
                }

                if !self.scanner.has_token(&[TokenKind::RParent]) {
                    return Err(ParseError::Syntax(
                        self.position(),
                        SyntaxError::ClosingParenthesisMissing,
                    ));
                }
                res = Box::new(Expr::ByteVector(bytes.into_boxed_slice()))
            }

            TokenKind::Quote => {
                self.scanner.next();

                return Ok(Expr::from_slice(
                    vec![
                        Box::new(Expr::Symbol(self.symbols.intern("quote"))),
                        self.parse(true)?,
                    ],
                    Box::new(Expr::Null),
                ));
            }

            TokenKind::BackQuote => {
                self.scanner.next();

                return Ok(Expr::from_slice(
                    vec![
                        Box::new(Expr::Symbol(self.symbols.intern("quasiquote"))),
                        self.parse(true)?,
                    ],
                    Box::new(Expr::Null),
                ));
            }

            TokenKind::Comma => {
                self.scanner.next();

                return Ok(Expr::from_slice(
                    vec![
                        Box::new(Expr::Symbol(self.symbols.intern("unquote"))),
                        self.parse(true)?,
                    ],
                    Box::new(Expr::Null),
                ));
            }

            TokenKind::CommaAt => {
                self.scanner.next();

                return Ok(Expr::from_slice(
                    vec![
                        Box::new(Expr::Symbol(self.symbols.intern("unquote-splicing"))),
                        self.parse(true)?,
                    ],
                    Box::new(Expr::Null),
                ));
            }
            TokenKind::Dot => {
                self.scanner.next();
                return Err(ParseError::Syntax(pos, SyntaxError::UnexpectedDot));
            }
        }

        if prescan {
            self.scanner.next();
        }

        Ok(Box::new(Expr::Syntax(pos, res)))
    }

    pub fn position(&self) -> Position {
        self.scanner.token().pos
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseError {
    Lexical(Position, LexicalError),
    Syntax(Position, SyntaxError),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lexical(pos, err) => {
                write!(f, "error {} at {}", pos, err)
            }

            Self::Syntax(pos, err) => {
                write!(f, "error {} at {}", pos, err)
            }
        }
    }
}
