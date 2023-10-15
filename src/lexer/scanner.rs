use std::{fmt, iter::Peekable, str::{Chars, FromStr}};

use crate::Fixnum;

use super::{lexical_error::LexicalError, scan_buffer::ScanBuffer};
use num::{bigint::Sign, complex::Complex64, BigInt, BigRational, ToPrimitive, Zero};

pub const EOF_CH: char = '\0';
pub const EOL_CH: char = '\n';
pub const RET_CH: char = '\r';
pub const TAB_CH: char = '\t';
pub const SPACE_CH: char = ' ';
pub const ZERO_CH: char = '0';
pub const B_CH: char = 'b';
pub const D_CH: char = 'd';
pub const O_CH: char = 'o';
pub const LA_CH: char = 'a';
pub const UA_CH: char = 'A';
pub const LZ_CH: char = 'z';
pub const UZ_CH: char = 'Z';
pub const DQ_CH: char = '"';
pub const BQ_CH: char = '`';
pub const Q_CH: char = '\'';
pub const OPENQ_CH: char = '‘';
pub const CLOSEQ_CH: char = '’';
pub const DOT_CH: char = '.';
pub const COMMA_CH: char = ',';
pub const AT_CH: char = '@';
pub const HASH_CH: char = '#';
pub const SEMI_CH: char = ';';
pub const BANG_CH: char = '!';
pub const MINUS_CH: char = '-';
pub const PLUS_CH: char = '+';
pub const LE_CH: char = 'e';
pub const UE_CH: char = 'E';
pub const LI_CH: char = 'i';
pub const UI_CH: char = 'I';
pub const BS_CH: char = '\\';
pub const SLASH_CH: char = '/';
pub const BAR_CH: char = '|';
pub const X_CH: char = 'x';
pub const T_CH: char = 't';
pub const R_CH: char = 'r';
pub const N_CH: char = 'n';
pub const UN_CH: char = 'N';
pub const V_CH: char = 'v';
pub const U_CH: char = 'u';
pub const G_CH: char = 'g';
pub const F_CH: char = 'f';
pub const EIGHT_CH: char = '8';
pub const LPAREN_CH: char = '(';
pub const RPAREN_CH: char = ')';
pub const DIGITS: &'static str = "0123456789";

pub const LHEX_DIGITS: &'static str = "abcdef";

pub const UHEX_DIGITS: &'static str = "ABCDEF";

pub const INITIALS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";

pub fn is_space(c: char) -> bool {
    c.is_whitespace()
}

pub fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}

pub fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

pub fn is_digit_for_radix(c: char, radix: u32) -> bool {
    c.is_digit(radix)
}

/// `is_initial` returns true if `ch` is an initial identifier character.
#[inline]
pub fn is_initial(ch: char) -> bool {
    ch.is_alphabetic() || is_special_initial(ch)
}

/// `is_special_initial` returns true if `ch` is a special subsequent for identifiers.
#[inline]
pub fn is_special_initial(ch: char) -> bool {
    "!$%&*/:<=>?^_~".contains(ch)
}

/// `is_subsequent` returns true if `ch` is a subsequent identifier character.
#[inline]
pub fn is_subsequent(ch: char) -> bool {
    if ch == '\0' {
        return false;
    }
    is_initial(ch) || ch.is_digit(10) || is_special_subsequent(ch)
}

/// `is_explicit_sign` returns true if ch is a plus (+) or minus (-) sign.
#[inline]
pub fn is_explicit_sign(ch: char) -> bool {
    ch == '+' || ch == '-'
}

/// `is_special_subsequent` returns true if `ch` is a special subsequent identifier character.
#[inline]
pub fn is_special_subsequent(ch: char) -> bool {
    is_explicit_sign(ch) || ch == '.' || ch == '@'
}

/// `is_dot_subsequent` returns true if `ch` is a dot subsequent for identifiers.
#[inline]
pub fn is_dot_subsequent(ch: char) -> bool {
    is_sign_subsequent(ch) || ch == '.'
}

/// `is_sign_subsequent` returns true if `ch` is a sign subsequent for identifiers.
#[inline]
pub fn is_sign_subsequent(ch: char) -> bool {
    is_initial(ch) || is_explicit_sign(ch) || ch == '@'
}

pub fn digit_val(c: char) -> isize {
    if DIGITS.contains(c) {
        return c as isize - ZERO_CH as isize;
    } else if LHEX_DIGITS.contains(c) {
        c as isize - LA_CH as isize + 10
    } else if UHEX_DIGITS.contains(c) {
        c as isize - UA_CH as isize + 10
    } else {
        16
    }
}

pub struct Token {
    pub pos: Position,
    pub kind: TokenKind,
    pub str_val: String,
    pub int_val: Fixnum,
    pub big_int_val: num::bigint::BigInt,
    pub rat_val: crate::Rational,
    pub big_rat_val: num::rational::Ratio<num::bigint::BigInt>,
    pub float_val: f64,
    pub complex_val: num::complex::Complex64,
    pub error_val: Option<LexicalError>,
}

impl Token {
    pub fn reset(&mut self, pos: Position) {
        self.pos = pos;
        self.kind = TokenKind::Error;

        self.str_val = String::new();
        self.int_val = 0;
        self.big_int_val = num::bigint::BigInt::from(0i32);
        self.rat_val = crate::Rational::new(0, 1);
        self.big_rat_val = num::rational::Ratio::new(
            num::bigint::BigInt::from(0i32),
            num::bigint::BigInt::from(1i32),
        );
        self.float_val = 0.0;
        self.complex_val = num::complex::Complex64::new(0.0, 0.0);
        self.error_val = None;
    }

    pub fn new(kind: TokenKind, pos: Position) -> Token {
        Token {
            pos,
            kind,
            str_val: String::new(),
            int_val: 0,
            big_int_val: num::bigint::BigInt::from(0i32),
            rat_val: crate::Rational::new(0, 1),
            big_rat_val: num::rational::Ratio::new(
                num::bigint::BigInt::from(0i32),
                num::bigint::BigInt::from(1i32),
            ),
            float_val: 0.0,
            complex_val: num::complex::Complex64::new(0.0, 0.0),
            error_val: None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Error => {
                if let Some(err) = self.error_val {
                    write!(f, "<error: {}>", err)
                } else {
                    write!(f, "<error>")
                }
            }
            TokenKind::Eof => write!(f, "<eof>"),
            TokenKind::Ident => write!(f, "{}", self.str_val),
            TokenKind::TrueLit => write!(f, "#t"),
            TokenKind::FalseLit => write!(f, "#f"),
            TokenKind::Int => write!(f, "{}", self.int_val),
            TokenKind::BigInt => write!(f, "{}", self.big_int_val),
            TokenKind::Rat => write!(f, "{}", self.rat_val),
            TokenKind::BigRat => write!(f, "{}", self.big_rat_val),
            TokenKind::Float => write!(f, "{}", self.float_val),
            TokenKind::Complex => write!(f, "{}", self.complex_val),
            TokenKind::Char => match self.int_val {
                7 => write!(f, "#\\alarm"),
                8 => write!(f, "#\\backspace"),
                127 => write!(f, "#\\delete"),
                27 => write!(f, "#\\escape"),
                0 => write!(f, "#\\nul"),
                13 => write!(f, "#\\return"),
                32 => write!(f, "#\\space"),
                9 => write!(f, "#\\tab"),
                _ => write!(f, "#\\{}", self.str_val),
            },
            TokenKind::String => write!(f, "\"{}\"", self.str_val),
            TokenKind::LParent => write!(f, "("),
            TokenKind::RParent => write!(f, ")"),
            TokenKind::HashLParen => write!(f, "#("),
            TokenKind::HashGLParen => write!(f, "#g("),
            TokenKind::U8LParen => write!(f, "#u8("),
            TokenKind::Quote => write!(f, "'"),
            TokenKind::BackQuote => write!(f, "`"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::CommaAt => write!(f, ",@"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::HashSemi => write!(f, "#;"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum TokenKind {
    Error,
    Eof,
    Ident,
    TrueLit,
    FalseLit,
    Int,
    BigInt,
    Rat,
    BigRat,
    Float,
    Complex,
    Char,
    String,
    LParent,
    RParent,
    HashLParen,
    HashGLParen,
    U8LParen,
    Quote,
    BackQuote,
    Comma,
    CommaAt,
    Dot,
    HashSemi,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{:?}", self).to_uppercase())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub const fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line == 0 {
            write!(f, "")
        } else {
            if self.col == 0 {
                write!(f, "{}", self.line)
            } else {
                write!(f, "{}:{}", self.line, self.col)
            }
        }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Position {{ line: {}, col: {} }}", self.line, self.col)
    }
}

pub struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    buffer: ScanBuffer,
    pub(crate) fold_case: bool,
    ch: char,
    lpos: Position,
    pos: Position,
    token: Token,
}

impl<'a> Scanner<'a> {
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn new(input: &'a str, fold_case: bool, prescan: bool) -> Self {
        let chars = input.chars().peekable();
        let mut this = Self {
            chars,
            buffer: ScanBuffer::new(256),
            fold_case,
            ch: SPACE_CH,
            lpos: Position::new(0, 0),
            pos: Position::new(1, 1),
            token: Token::new(TokenKind::Error, Position::new(0, 0)),
        };

        this.token.error_val = Some(LexicalError::Empty);

        if prescan {
            this.next();
        }

        this
    }

    pub fn has_token(&self, kind: &[TokenKind]) -> bool {
        kind.contains(&self.token.kind)
    }

    pub fn has_next(&self) -> bool {
        self.token.kind != TokenKind::Eof
    }

    pub fn next(&mut self) {
        while self.ch != EOF_CH {
            
            self.skip_space();
            self.token.reset(self.lpos);

            if self.ch == EOF_CH {
                break;
            }

            self.buffer.reset();

            if is_initial(self.ch) {
                self.scan_ident();
                return;
            }

            
            if self.ch == PLUS_CH || self.ch == MINUS_CH {
                
                let neg = self.ch == MINUS_CH;
                self.next_ch();
                if is_digit(self.ch) {
                    self.scan_number(10, neg, false);
                } else if self.ch == DOT_CH {
                    self.next_ch();

                    if is_dot_subsequent(self.ch) {
                        self.scan_ident();
                    } else if is_digit(self.ch) {
                        self.scan_number(10, neg, true);
                    } else {
                        self.token.kind = TokenKind::Ident;
                        self.token.str_val = self.buffer.string_value();
                    }
                } else if is_subsequent(self.ch) || self.ch == PLUS_CH || self.ch == MINUS_CH {
                    self.next_ch();
                    while is_subsequent(self.ch) && self.ch != PLUS_CH && self.ch != MINUS_CH {
                        self.next_ch();
                    }

                    let mut real_part: Option<f64> = None;

                    match self.buffer.string_value().to_lowercase().as_str() {
                        "-inf.0" => {
                            real_part = Some(-f64::INFINITY);
                        }

                        "+inf.0" => {
                            real_part = Some(f64::INFINITY);
                        }

                        "-nan.0" => {
                            real_part = Some((-1.0f64).sqrt());
                        }

                        "+nan.0" => {
                            real_part = Some(f64::NAN);
                        }

                        "-inf.0i" => {
                            self.token.kind = TokenKind::Complex;
                            self.token.complex_val = Complex64::new(0.0, -f64::INFINITY);
                        }

                        "+inf.0i" => {
                            self.token.kind = TokenKind::Complex;
                            self.token.complex_val = Complex64::new(0.0, f64::INFINITY);
                        }

                        "+i" => {
                            self.token.kind = TokenKind::Complex;
                            self.token.complex_val = Complex64::new(0.0, 1.0);
                        }

                        "-i" => {
                            self.token.kind = TokenKind::Complex;
                            self.token.complex_val = Complex64::new(0.0, -1.0);
                        }

                        _ => {
                            real_part = None;
                            while is_subsequent(self.ch) {
                                self.next_ch();
                            }

                            self.token.kind = TokenKind::Ident;
                            self.token.str_val = self.buffer.string_value();
                        }
                    }

                    if let Some(real_part) = real_part {
                        match self.ch {
                            PLUS_CH => {
                                self.next_ch();
                                self.scan_imaginary_part(real_part, false);
                            }

                            MINUS_CH => {
                                self.next_ch();
                                self.scan_imaginary_part(real_part, true);
                            }

                            _ => {
                                self.token.kind = TokenKind::Float;
                                self.token.float_val = real_part;
                            }
                        }
                    } 
                } else {
                    self.token.kind = TokenKind::Ident;
                    self.token.str_val = self.buffer.string_value();
                }

                return;
            }
            if is_digit(self.ch) {
                self.scan_number(10, false, false);
                return;
            }
            match self.ch {
                BAR_CH => {
                    self.scan_delimited_ident();
                    return;
                }

                DOT_CH => {
                    self.next_ch();
                    if is_dot_subsequent(self.ch) {
                        self.scan_ident();
                    } else if is_digit(self.ch) {
                        self.scan_number(10, false, true);
                    } else {
                        self.token.kind = TokenKind::Dot;
                    }

                    return;
                }

                LPAREN_CH | '[' => {
                    self.token.kind = TokenKind::LParent;
                    self.next_ch();
                    return;
                }

                RPAREN_CH | ']' => {
                    self.token.kind = TokenKind::RParent;
                    self.next_ch();
                    return;
                }

                Q_CH | OPENQ_CH | CLOSEQ_CH => {
                    self.token.kind = TokenKind::Quote;
                    self.next_ch();
                    return;
                }
                '`' => {
                    self.token.kind = TokenKind::BackQuote;
                    self.next_ch();
                    return;
                }

                DQ_CH => {
                    self.scan_string();
                    return;
                }

                COMMA_CH => {
                    self.next_ch();
                    if self.ch == AT_CH {
                        self.next_ch();
                        self.token.kind = TokenKind::CommaAt;
                    } else {
                        self.token.kind = TokenKind::Comma;
                    }

                    return;
                }

                HASH_CH => {
                    self.next_ch();

                    match self.ch {
                        LE_CH => {
                            self.next_ch();
                            self.scan_general_number(Some(true));
                        }
                        LI_CH => {
                            self.next_ch();
                            self.scan_general_number(Some(false));
                        }

                        B_CH => {
                            self.scan_signed_number(2);
                        }

                        O_CH => {
                            self.scan_signed_number(8);
                        }

                        D_CH => {
                            self.scan_signed_number(10);
                        }

                        X_CH => {
                            self.scan_signed_number(16);
                        }

                        U_CH => {
                            self.next_ch();
                            if self.ch != EIGHT_CH {
                                self.signal(LexicalError::IncompleteCharacterLiteral);
                                return;
                            }

                            self.next_ch();

                            if self.ch != LPAREN_CH {
                                self.signal(LexicalError::IncompleteCharacterLiteral);
                                return;
                            }

                            self.next_ch();
                            self.token.kind = TokenKind::U8LParen;
                        }

                        BAR_CH => {
                            self.next_ch();
                            let mut bar = false;

                            while (!bar || self.ch != HASH_CH) && self.ch != EOF_CH {
                                bar = self.ch == BAR_CH;
                                self.next_ch();
                            }

                            if self.ch != EOF_CH {
                                self.next_ch();
                            }

                            continue;
                        }

                        BANG_CH => {
                            self.next_ch();

                            while is_subsequent(self.ch) {
                                self.next_ch();
                            }

                            let s = self.buffer.string_starting_at(2).to_lowercase();
                            match s.as_str() {
                                "fold-case" => {
                                    self.fold_case = true;
                                }

                                "no-fold-case" => {
                                    self.fold_case = false;
                                }
                                _ => {
                                    self.signal(LexicalError::UnknownDirective);
                                    return;
                                }
                            }

                            if self.ch != EOF_CH {
                                self.next_ch();
                            }
                            continue;
                        }

                        LPAREN_CH => {
                            self.next_ch();
                            self.token.kind = TokenKind::HashLParen;
                        }

                        G_CH => {
                            self.next_ch();

                            if self.ch != LPAREN_CH {
                                return self.signal(LexicalError::IncompleteCharacterLiteral);
                            }

                            self.next_ch();
                            self.token.kind = TokenKind::HashGLParen;
                        }

                        SEMI_CH => {
                            self.next_ch();
                            self.token.kind = TokenKind::HashSemi;
                        }

                        BS_CH => {
                            self.next_ch();
                            self.scan_character_literal();
                        }

                        EOF_CH => {
                            self.signal(LexicalError::IncompleteCharacterLiteral);
                            return;
                        }

                        _ => {
                            while self.ch >= LA_CH && self.ch <= LZ_CH
                                || self.ch >= UA_CH && self.ch <= UZ_CH
                            {
                                self.next_ch();
                            }
                            let s = self.buffer.string_starting_at(1).to_lowercase();

                                match s.as_str() {
                                    "t" | "true" => self.token.kind = TokenKind::TrueLit,
                                    "f" | "false" => self.token.kind = TokenKind::FalseLit,
                                    _ => {
                                        self.signal(LexicalError::UnknownCharacterLiteral);
                                        return;
                                    } 
                                }
                        }
                    }

                    return;
                }
                _ => {
                    self.next_ch();
                    self.signal(LexicalError::IllegalCharacter);
                }
            }
        }
        self.token.kind = TokenKind::Eof;
    }

    pub fn scan_general_number(&mut self, exact: Option<bool>) {
        if self.ch == HASH_CH {
            self.next_ch();

            match self.ch {
                B_CH => {
                    self.next_ch();
                    self.scan_signed_number(2);
                }
                O_CH => {
                    self.next_ch();
                    self.scan_signed_number(8);
                }

                D_CH => {
                    self.next_ch();
                    self.scan_signed_number(10);
                }

                X_CH => {
                    self.next_ch();
                    self.scan_signed_number(16);
                }

                _ => {
                    self.signal(LexicalError::NumberExpected);
                }
            }
        } else {
            self.scan_signed_number(10);
        }

        if let Some(exact) = exact {
            if exact {
                match self.token.kind {
                    TokenKind::Float => {
                        let max = Fixnum::MAX as f64;

                        if self.token.float_val > -max && self.token.float_val < max {
                            if let Some(rat) = crate::Rational::approximate_float(self.token.float_val) {
                                self.token.kind = TokenKind::Rat;
                                self.token.rat_val = rat;
                                self.token.float_val = 0.0;
                            }
                            /*else if let Some(rat) = BigRational::approximate_float(self.token.float_val) {
                                self.token.kind = TokenKind::BigRat;
                                self.token.big_rat_val = rat;
                                self.token.float_val = 0.0;
                            }*/
                            else {
                                self.token.float_val = f64::INFINITY;
                            }
                        }
                        /*else if let Some(rat) = BigRational::approximate_float(self.token.float_val) {
                            self.token.kind = TokenKind::BigRat;
                            self.token.big_rat_val = rat;
                            self.token.float_val = 0.0;
                        }*/
                        else {
                            self.token.float_val = f64::INFINITY;
                        }
                    }
                    TokenKind::Complex => {
                        self.signal(LexicalError::ExactComplexNumberUnsupported);
                    }
                    _ => (),
                }
            }
        } else {
            match self.token.kind {
                TokenKind::Int => {
                    self.token.kind = TokenKind::Float;
                    self.token.float_val = self.token.int_val as f64;
                    self.token.int_val = 0;
                }

                TokenKind::BigInt => {
                    self.token.kind = TokenKind::Float;
                    self.token.float_val = self.token.big_int_val.to_f64().unwrap();
                    self.token.big_int_val = BigInt::zero();
                }

                TokenKind::Rat => {
                    self.token.kind = TokenKind::Float;
                    self.token.float_val =
                        (*self.token.rat_val.numer()) as f64 / (*self.token.rat_val.denom()) as f64;
                    self.token.rat_val = crate::Rational::zero();
                }

                TokenKind::BigRat => {
                    self.token.kind = TokenKind::Float;
                    self.token.float_val = self.token.big_rat_val.numer().to_f64().unwrap() / 
                        self.token.big_rat_val.denom().to_f64().unwrap();
                    self.token.big_rat_val = BigRational::zero();
                }
                _ => ()
            }
        }
    }

    pub fn scan_character_literal(&mut self) {
        match self.ch {
            EOF_CH => {
                self.signal(LexicalError::MalformedCharacterLiteral);
            }
            X_CH => {
                self.next_ch();

                if let Some(ch) = self.scan_hex_number(4) {
                    self.token.kind = TokenKind::Char;
                    self.token.int_val = ch as u32 as _;
                } else if self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
                    self.signal(LexicalError::UnknownCharacterLiteral);
                } else {
                    self.token.kind = TokenKind::Char;
                    self.token.int_val = X_CH as _;
                }
            }

            U_CH => {
                self.next_ch();

                if let Some(ch) = self.scan_hex_number(4) {
                    self.token.kind = TokenKind::Char;
                    self.token.int_val = ch as u32 as _;
                } else if self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
                    self.signal(LexicalError::UnknownCharacterLiteral);
                } else {
                    self.token.kind = TokenKind::Char;
                    self.token.int_val = U_CH as _;
                }
            }

            x if (LA_CH..LZ_CH).contains(&x) || (UA_CH..UZ_CH).contains(&x) => {
                self.next_ch();
                while self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
                    self.next_ch();
                }

                let s = self.buffer.string_starting_at(2);
                self.token.kind = TokenKind::Char;

                if s.chars().count() == 1 {
                    self.token.int_val = s.chars().next().unwrap() as _;
                } else {
                    match s.as_str() {
                        "alarm" => {
                            self.token.int_val = 7;
                        }

                        "backspace" => {
                            self.token.int_val = 8;
                        }

                        "delete" => {
                            self.token.int_val = 127;
                        }

                        "escape" => {
                            self.token.int_val = 27;
                        }

                        "newline" => {
                            self.token.int_val = 10;
                        }

                        "null" | "nul" => {
                            self.token.int_val = 0;
                        }

                        "page" => {
                            self.token.int_val = 12;
                        }

                        "return" => {
                            self.token.int_val = 13;
                        }

                        "space" => {
                            self.token.int_val = 32;
                        }

                        "tab" => {
                            self.token.int_val = 9;
                        }

                        "vtab" => {
                            self.token.int_val = 11;
                        }

                        "linefeed" => {
                            self.token.int_val = 10;
                        }
                        "esc" => {
                            self.token.int_val = 27;
                        }
                        _ => {
                            self.signal(LexicalError::UnknownCharacterLiteral);
                        }

                    }
                }
            }

            _ => {
                if self.ch as u32 > 0xd7ff {
                    self.signal(LexicalError::UnknownCharacterLiteral);
                } else {
                    self.token.kind = TokenKind::Char;
                    self.token.int_val = self.ch as u32 as _;
                    self.next_ch();
                }
            }
        }
    }

    pub fn scan_signed_number(&mut self, radix: u32) {
        match self.ch {
            MINUS_CH => {
                self.next_ch();
                self.scan_number(radix, true, false);
            }
            _ => self.scan_number(radix, false, false),
        }
    }

    pub fn scan_ident(&mut self) {
        self.next_ch();
        while is_subsequent(self.ch) {
            self.next_ch();
        }

        self.token.kind = TokenKind::Ident;
        self.token.str_val = self.buffer.string_value();
    }

    pub fn scan_hex_number(&mut self, max_digits: usize) -> Option<Fixnum> {
        if !is_digit_for_radix(self.ch, 16) {
            return None;
        }

        let mut i = max_digits;
        let mut res = 0 as Fixnum;

        while i > 0 && is_digit_for_radix(self.ch, 16) {
            let x = match res.overflowing_mul(16) {
                (_, true) => return None,
                (res, false) => res,
            };

            let y = match x.overflowing_add(digit_val(self.ch) as _) {
                (_, true) => return None,
                (x, false) => x,
            };

            res = y;
            self.next_ch();
            i -= 1;
        }

        Some(res)
    }

    /// Scans the next characters as an unsigned integer or floating point number.
    pub fn scan_number(&mut self, radix: u32, neg: bool, dot: bool) {
        let mut digits = vec![];

        let mut is_float = dot;
        let start = self.buffer.index() - 1;

        if !is_float {
            if !(radix == 10 && (self.ch == DOT_CH || (self.ch == LE_CH) || (self.ch == UE_CH))) {
                while is_digit_for_radix(self.ch, radix) {
                    digits.push(digit_val(self.ch) as u32);
                    self.next_ch();
                }
            }

            if radix == 10 {
                if self.ch == DOT_CH {
                    is_float = true;
                    self.next_ch();
                } else if self.ch == LE_CH || self.ch == UE_CH {
                    is_float = true;
                }
            }
        }

        if is_float {
            while is_digit(self.ch) {
                self.next_ch();
            }

            if self.ch == LE_CH || self.ch == UE_CH {
                self.next_ch();

                if self.ch == PLUS_CH || self.ch == MINUS_CH {
                    self.next_ch();
                }

                while is_digit(self.ch) {
                    self.next_ch();
                }
            }

            let s = self.buffer.string_starting_at(start);

            if let Ok(f) = if dot {
                format!(".{}", s).parse::<f64>()
            } else {
                s.parse::<f64>()
            } {
                match self.ch {
                    PLUS_CH => {
                        self.next_ch();
                        self.scan_imaginary_part(if neg { -f } else { f }, false);
                    }

                    MINUS_CH => {
                        self.next_ch();
                        self.scan_imaginary_part(if neg { -f } else { f }, true);
                    }
                    LI_CH | UI_CH => {
                        self.next_ch();
                        self.token.kind = TokenKind::Complex;
                        self.token.complex_val = Complex64::new(0.0, if neg { -f } else { f });
                    }
                    _ => {
                        self.token.kind = TokenKind::Float;
                        self.token.float_val = f;
                    }
                }
            } else {
                self.signal(LexicalError::MalformedFloatLiteral);
            }
        } else {
            let str: String = digits.iter().map(|x| char::from_digit(*x, radix).unwrap()).collect();
            let str = if neg {
                format!("-{}", str)
            } else {
                str
            };
            let number = BigInt::from_str(&str).unwrap();

            match self.ch {
                SLASH_CH => {
                    self.next_ch();
                    digits.clear();

                    while is_digit_for_radix(self.ch, radix) {
                        digits.push(digit_val(self.ch) as _);
                        self.next_ch();
                    }

                    if digits.is_empty() {
                        return self.scan_ident();
                    }

                    let denom = BigInt::from_slice(Sign::Plus, &digits);

                    if denom == BigInt::from(0i32) {
                        self.signal(LexicalError::DivisionByZero);
                    } else {
                        self.token.kind = TokenKind::BigRat;
                        self.token.big_rat_val = BigRational::new(number, denom);
                    }
                }

                PLUS_CH => {
                    self.next_ch();
                    self.scan_imaginary_part(number.to_f64().unwrap(), false);
                }

                MINUS_CH => {
                    self.next_ch();
                    self.scan_imaginary_part(number.to_f64().unwrap(), true);
                }

                LI_CH | UI_CH => {
                    self.next_ch();
                    self.token.kind = TokenKind::Complex;
                    self.token.complex_val = Complex64::new(0.0, number.to_f64().unwrap());
                }
                _ => {
                    #[cfg(feature="fixnum64")]
                    if let Some(i) = number.to_i64() {
                        self.token.kind = TokenKind::Int;
                        self.token.int_val = i as Fixnum;
                    } else {
                        self.token.kind = TokenKind::BigInt;
                        self.token.big_int_val = number;
                    }
                    #[cfg(feature="fixnum32")]
                    if let Some(i) = number.to_i32() {
                        self.token.kind = TokenKind::Int;
                        self.token.int_val = i as Fixnum;
                    } else {
                        self.token.kind = TokenKind::BigInt;
                        self.token.big_int_val = number;
                    }
                    
                }
            }
        }
    }

    pub fn scan_imaginary_part(&mut self, real_part: f64, neg: bool) {
        let start = self.buffer.index() - 1;
        
        if self.ch != LI_CH && self.ch != UI_CH && self.ch != N_CH && self.ch != UN_CH {
            while is_digit_for_radix(self.ch, 10) {
                self.next_ch();
            }

            if self.ch == DOT_CH {
                self.next_ch();

                while is_digit_for_radix(self.ch, 10) {
                    self.next_ch();
                }
            }

            if self.ch == LE_CH || self.ch == UE_CH {
                self.next_ch();
                if self.ch == PLUS_CH || self.ch == MINUS_CH {
                    self.next_ch();
                }
                while is_digit(self.ch) {
                    
                    self.next_ch();
                }
            }
            
            if self.ch == LI_CH || self.ch == UI_CH {
                let s = self.buffer.string_starting_at(start);
                self.next_ch();

                if let Ok(f) = s.parse::<f64>() {
                    self.token.kind = TokenKind::Complex;
                    self.token.complex_val = num::complex::Complex64::new(real_part, f);
                } else {
                    self.signal(LexicalError::MalformedFloatLiteral);
                }
            } else {
                self.signal(LexicalError::MalformedComplexLiteral);
            }
        } else {
            self.scan_ident();

            let s = self.buffer.string_starting_at(start);

            match s.to_lowercase().as_str() {
                "inf.0i" => {
                    self.token.kind = TokenKind::Complex;
                    self.token.complex_val = num::complex::Complex64::new(
                        real_part,
                        if neg { -f64::INFINITY } else { f64::INFINITY },
                    );
                    self.token.str_val = "".to_string();
                }
                "nan.0i" => {
                    self.token.kind = TokenKind::Complex;
                    self.token.complex_val = num::complex::Complex64::new(real_part, f64::NAN);
                    self.token.str_val = "".to_string();
                }

                "i" => {
                    self.token.kind = TokenKind::Complex;
                    self.token.complex_val =
                        num::complex::Complex64::new(real_part, if neg { -1.0 } else { 1.0 });
                    self.token.str_val = "".to_string();
                }

                _ => {
                    self.signal(LexicalError::MalformedComplexLiteral);
                }
            }
        }
    }

    pub fn scan_string(&mut self) {
        match self.scan_char_subsequence_until(DQ_CH) {
            CharSubsequentResult::Succed(s) => {
                self.token.kind = TokenKind::String;
                self.token.str_val = s;
            }

            CharSubsequentResult::Malformed => {
                self.signal(LexicalError::MalformedStringLiteral);
            }

            CharSubsequentResult::IllegalEscapeSequence => {
                self.signal(LexicalError::IllegalEscapeSequence);
            }

            CharSubsequentResult::IllegalEndOfLine => {
                self.signal(LexicalError::IllegalEOL);
            }

            CharSubsequentResult::IllegalHexChar => {
                self.signal(LexicalError::IllegalHexCharacter);
            }

            CharSubsequentResult::Unsupported => {
                self.signal(LexicalError::TokenNotYetSupported);
            }
        }
    }

    pub fn scan_delimited_ident(&mut self) {
        match self.scan_char_subsequence_until(BAR_CH) {
            CharSubsequentResult::Succed(s) => {
                self.token.kind = TokenKind::Ident;
                self.token.str_val = s;
            }

            CharSubsequentResult::Malformed => {
                self.signal(LexicalError::MalformedIdentifier);
            }

            CharSubsequentResult::IllegalEscapeSequence => {
                self.signal(LexicalError::IllegalEscapeSequence);
            }

            CharSubsequentResult::IllegalEndOfLine => {
                self.signal(LexicalError::IllegalEOL);
            }

            CharSubsequentResult::IllegalHexChar => {
                self.signal(LexicalError::IllegalHexCharacter);
            }

            CharSubsequentResult::Unsupported => {
                self.signal(LexicalError::TokenNotYetSupported);
            }
        }
    }

    pub fn scan_char_subsequence_until(&mut self, terminator: char) -> CharSubsequentResult {
        let mut chars = vec![];

        self.next_ch();

        while self.ch != terminator {
            if self.ch == EOF_CH {
                return CharSubsequentResult::Malformed;
            } else if self.ch == BS_CH {
                self.next_ch();

                match self.ch {
                    BS_CH => {
                        self.next_ch();
                        chars.push(BS_CH);
                    }

                    DQ_CH => {
                        self.next_ch();
                        chars.push(DQ_CH);
                    }

                    X_CH => {
                        self.next_ch();
                        if let Some(ch) = self.scan_hex_char() {
                            chars.push(ch);
                        } else {
                            return CharSubsequentResult::IllegalHexChar;
                        }
                    }
                    LA_CH => {
                        self.next_ch();
                        chars.push(7 as char);
                    }

                    B_CH => {
                        self.next_ch();
                        chars.push(8 as char);
                    }

                    T_CH => {
                        self.next_ch();
                        chars.push(9 as char);
                    }

                    N_CH => {
                        self.next_ch();
                        chars.push(10 as char);
                    }

                    V_CH => {
                        self.next_ch();
                        chars.push(11 as char);
                    }

                    F_CH => {
                        self.next_ch();
                        chars.push(12 as char);
                    }

                    R_CH => {
                        self.next_ch();
                        chars.push(13 as char);
                    }

                    LE_CH => {
                        self.next_ch();
                        chars.push(27 as char);
                    }

                    EOL_CH => {
                        self.next_ch();
                    }

                    RET_CH => {
                        self.next_ch();

                        if self.ch == EOL_CH {
                            self.next_ch();
                        }
                    }

                    _ => return CharSubsequentResult::IllegalEscapeSequence,
                }
            } else if self.ch == EOL_CH || self.ch == RET_CH {
                self.next_ch();
                return CharSubsequentResult::IllegalEndOfLine;
            } else {
                chars.push(self.ch);
                self.next_ch();
            }
        }
        self.next_ch();

        CharSubsequentResult::Succed(chars.iter().collect())
    }

    pub fn scan_hex_char(&mut self) -> Option<char> {
        if !is_digit_for_radix(self.ch, 16) {
            return None;
        }

        let mut res = 0u16;

        while is_digit_for_radix(self.ch, 16) {
            let x = match res.overflowing_mul(16) {
                (x, false) => x,
                _ => return None,
            };

            let y = match x.overflowing_add(digit_val(self.ch) as u16) {
                (y, false) => y,
                _ => return None,
            };

            res = y;
            self.next_ch();
        }

        if self.ch != SEMI_CH {
            return None;
        }

        self.next_ch();

        Some(char::from_u32(res as _).unwrap())
    }

    pub fn signal(&mut self, error: LexicalError) {
        self.token.kind = TokenKind::Error;
        self.token.error_val = Some(error);
    }

    pub fn next_ch(&mut self) {
        if self.ch == EOF_CH {
            return;
        }

        self.lpos = self.pos;

        if let Some(c) = self.chars.next() {
            if c == RET_CH {
                self.ch = EOL_CH;
                self.pos.col = 1;
                self.pos.line += 1;
                if let Some(next) = self.chars.peek() {
                    if *next == EOL_CH {
                        self.chars.next().unwrap();
                    }
                }
            } else if c == EOL_CH {
                self.ch = EOL_CH;
                self.pos.col = 1;
                self.pos.line += 1;
            } else {
                self.ch = c;
                self.pos.col += 1;
            }
        } else {
            self.ch = EOF_CH;
        }
        self.buffer.append(self.ch);
    }

    pub fn skip_space(&mut self) {
        self.skip_comment();
        while is_space(self.ch) {
            self.next_ch();
            while is_space(self.ch) {
                self.next_ch();
            }

            self.skip_comment();
        }
    }

    pub fn skip_comment(&mut self) {
        while self.ch == SEMI_CH {
            self.next_ch();

            while self.ch != EOL_CH && self.ch != EOF_CH {
                self.next_ch();
            }
        }
    }
}

pub enum CharSubsequentResult {
    Succed(String),
    Malformed,
    IllegalEscapeSequence,
    IllegalEndOfLine,
    IllegalHexChar,
    Unsupported,
}
