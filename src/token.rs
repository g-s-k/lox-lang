#[derive(Clone, Debug)]
pub(crate) struct Token<'a> {
    pub(crate) r#type: TokenType,
    pub(crate) text: &'a str,
    pub(crate) line: usize,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semi,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    r#String,
    Number,

    And,
    Break,
    Case,
    Class,
    Continue,
    r#Default,
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
    Switch,
    This,
    True,
    Var,
    While,
}
