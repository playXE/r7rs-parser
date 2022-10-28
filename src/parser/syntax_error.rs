use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SyntaxError {
    Empty,
    ClosingParenthesisMissing,
    UnexpectedClosingParenthesis,
    UnexpectedDot,
    NotAByteValue,
    SyntaxNotYetSupported
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SyntaxError::*;
        f.write_str(match self {
            Empty => "empty input",
            ClosingParenthesisMissing => "closing parenthesis missing",
            UnexpectedClosingParenthesis => "unexpected closing parenthesis",
            UnexpectedDot => "unexpected dot",
            NotAByteValue => "not a byte value",
            SyntaxNotYetSupported => "syntax not yet supported"
        })
    }
}

impl std::error::Error for SyntaxError {}