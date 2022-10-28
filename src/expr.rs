use std::fmt::Debug;

use num::{complex::Complex64, BigRational};

use crate::{lexer::scanner::Position, Fixnum};

/*
pub struct Expr<I: Interner = NoIntern> {
    pub pos: Position,
    pub kind: ExprKind<I>,
}

impl<I: Interner> Expr<I> {
    pub fn null(pos: Position) -> Box<Self> {
        Box::new(Self {
            pos,
            kind: ExprKind::Null
        })
    }
    pub fn from_slice(mut exprs: Vec<Box<Expr<I>>>, append: Box<Expr<I>>) -> Box<Self> {
        exprs.reverse();
        Self::make_list_unchecked(exprs, append)
    }

    pub fn make_list_unchecked(from_stack: Vec<Box<Expr<I>>>, append: Box<Expr<I>>) -> Box<Self> {
        let mut res = append;
        for expr in from_stack {
            res = Box::new(Expr {
                pos: expr.pos,
                kind: ExprKind::Pair(expr, res)
            });
        }

        res
    }
}

pub enum ExprKind<I: Interner> {
    Symbol(I::Sym),
    Bool(bool),
    Fixnum(Fixnum),
    BigInt(num::BigInt),
    Rational(crate::Rational),
    BigRational(BigRational),
    Float(f64),
    Complex(Complex64),
    Char(char),
    Str(String),
    Pair(Box<Expr<I>>, Box<Expr<I>>),
    ImmutableVector(Box<[Box<Expr<I>>]>),
    GrowableVector(Box<[Box<Expr<I>>]>),
    ByteVector(Box<[u8]>),
    Null,
}
*/

pub enum Expr<I: Interner> {
    Symbol(I::Sym),
    Syntax(Position, Box<Self>),
    Bool(bool),
    Fixnum(Fixnum),
    BigInt(num::BigInt),
    Rational(crate::Rational),
    BigRational(BigRational),
    Float(f64),
    Complex(Complex64),
    Char(char),
    Str(String),
    Pair(Box<Expr<I>>, Box<Expr<I>>),
    ImmutableVector(Box<[Box<Expr<I>>]>),
    GrowableVector(Box<[Box<Expr<I>>]>),
    ByteVector(Box<[u8]>),
    Null,
}

impl<I: Interner> Expr<I> {
    pub fn datum(self: Box<Self>) -> Box<Self> {
        match *self {
            Self::Pair(car, cdr) => Box::new(Self::Pair(car.datum(), cdr.datum())),
            Self::Syntax(_, expr) => expr,
            _ => self,
        }
    }

    pub fn from_slice(mut exprs: Vec<Box<Expr<I>>>, append: Box<Expr<I>>) -> Box<Self> {
        exprs.reverse();
        Self::make_list_unchecked(exprs, append)
    }

    pub fn make_list_unchecked(from_stack: Vec<Box<Expr<I>>>, append: Box<Expr<I>>) -> Box<Self> {
        let mut res = append;
        for expr in from_stack {
            res = Box::new(Expr::Pair(expr, res));
        }

        res
    }

    pub fn to_string(&self, interner: &I, escape: bool) -> String {
        fn double_string(f: f64) -> String {
            if f.is_infinite() {
                if f.is_sign_negative() {
                    "-inf.0".to_string()
                } else {
                    "+inf.0".to_string()
                }
            } else if f.is_nan() {
                if f.is_sign_negative() {
                    "-nan.0".to_string()
                } else {
                    "+nan.0".to_string()
                }
            } else {
                f.to_string()
            }
        }

        match self {
            Self::Null => "null".to_string(),
            Self::Symbol(sym) => interner.description(sym).to_string(),
            Self::Bool(x) => {
                if *x {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            Self::Syntax(_, expr) => expr.to_string(interner, escape),
            Self::Float(x) => double_string(*x),
            Self::Pair(head, tail) => {
                let mut s = String::new();
                s.push('(');
                s.push_str(&head.to_string(interner, escape));
                let mut expr = tail;

                while let Expr::Pair(car, cdr) = &**expr {
                    
                    s.push(' ');
                    s.push_str(&car.to_string(interner, escape));
                    
                    expr = cdr;
                }

                if let Expr::Null = &**expr {
                    s.push(')');
                    s
                } else {
                    s.push_str(" . ");
                    s.push_str(&tail.to_string(interner, escape));
                    s.push(')');
                    s
                }
            }
            Self::BigInt(x) => x.to_string(),
            Self::Fixnum(x) => x.to_string(),
            Self::BigRational(x) => x.to_string(),
            Self::Rational(x) => format!("{}/{}", x.numer(), x.denom()),
            Self::Complex(x) => {
                let mut res = double_string(x.re);

                if x.im.is_nan() || x.im.is_infinite() || x.im < 0.0 {
                    res = format!("{}{}", res, double_string(x.im));
                } else {
                    res = format!("{}+{}", res, double_string(x.im));
                }

                res.push('i');
                res
            }

            Self::Char(c) => {
                if !escape {
                    return c.to_string();
                } else {
                    todo!()
                }
            }
            Self::Str(s) => {
                if !escape {
                    return s.to_string();
                } else {
                    todo!()
                }
            }

            Self::ByteVector(bytes) => {
                let mut s = String::new();

                s.push_str("#u8(");

                for (i, b) in bytes.iter().enumerate() {
                    if i != bytes.len() - 1 {
                        s.push(' ');
                    }

                    s.push_str(&b.to_string());
                }

                s.push(')');

                s
            }

            Self::ImmutableVector(vector) => {
                let mut s = String::new();

                s.push_str("#(");

                for (i, b) in vector.iter().enumerate() {
                    if i != vector.len() - 1 {
                        s.push(' ');
                    }

                    s.push_str(&b.to_string(interner, escape));
                }

                s.push(')');

                s
            }

            Self::GrowableVector(vector) => {
                let mut s = String::new();

                s.push_str("#g(");

                for (i, b) in vector.iter().enumerate() {
                    if i != vector.len() - 1 {
                        s.push(' ');
                    }

                    s.push_str(&b.to_string(interner, escape));
                }

                s.push(')');

                s
            }
        }
    }
}
pub trait Sym: PartialEq + Eq {}

pub trait Interner {
    type Sym: PartialEq + Eq + Debug;
    fn intern(&mut self, s: &str) -> Self::Sym;
    fn description(&self, s: &Self::Sym) -> String;
}

impl Sym for &'static str {}
impl Sym for String {}

pub struct NoIntern;

impl Interner for NoIntern {
    type Sym = String;
    fn intern(&mut self, s: &str) -> Self::Sym {
        s.to_string()
    }

    fn description(&self, s: &Self::Sym) -> String {
        s.to_string()
    }
}
