use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LexicalError {
    Empty,
    MalformedIdentifier,
    BrokenIdentifierEncoding,
    BrokenNumberEncoding,
    MalformedFloatLiteral,
    MalformedComplexLiteral,
    MalformedStringLiteral,
    MalformedCharacterLiteral,
    UnknownCharacterLiteral,
    IncompleteCharacterLiteral,
    IllegalCharacter,
    IllegalHexCharacter,
    IllegalEscapeSequence,
    IllegalEOL,
    TokenNotYetSupported,
    DivisionByZero,
    ExactComplexNumberUnsupported,
    UnknownDirective,
    NumberExpected,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexicalError::*;
        f.write_str(match self {
            NumberExpected => "number expected",
            Empty => "no input available",
            MalformedIdentifier => "malformed identifier",
            BrokenIdentifierEncoding => "broken identifier encoding",
            BrokenNumberEncoding => "broken number encoding",
            MalformedFloatLiteral => "malformed float literal",
            MalformedComplexLiteral => "malformed complex literal",
            MalformedStringLiteral => "malformed string literal",
            MalformedCharacterLiteral => "malformed character literal",
            UnknownCharacterLiteral => "unknown character literal",
            IncompleteCharacterLiteral => "incomplete character literal",
            IllegalCharacter => "illegal character",
            IllegalHexCharacter => "illegal hex character",
            IllegalEscapeSequence => "illegal escape sequence",
            IllegalEOL => "illegal end of line",
            TokenNotYetSupported => "token not yet supported",
            DivisionByZero => "division by zero",
            ExactComplexNumberUnsupported => "exact complex number unsupported",
            UnknownDirective => "unknown directive"
        })
    }
}

impl std::error::Error for LexicalError {
    
}