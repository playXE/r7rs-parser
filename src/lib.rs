
pub mod lexer;
pub mod parser;
pub mod expr;

#[cfg(feature="fixnum32")]
pub type Fixnum = i32;
#[cfg(feature="fixnum64")]
pub type Fixnum = i64;

#[cfg(feature="fixnum32")]
pub type Rational = num::Rational32;
#[cfg(feature="fixnum64")]
pub type Rational = num::Rational64;

