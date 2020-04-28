use std::fmt;

use super::{Chunk, Error, Op, ScanError, Scanner, Token, TokenType, Value};

type MaybeToken<'a> = Option<Result<Token<'a>, ScanError>>;

pub(crate) struct CompileError<'a> {
    pub err: Error,
    token: MaybeToken<'a>,
}

impl<'a> fmt::Display for CompileError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.token {
            Some(Ok(token)) => write!(
                f,
                "[line {}] Error at {}: {}",
                token.line, token.text, self.err
            ),
            Some(Err((line, e))) => write!(f, "[line {}] {}", line, e),
            None => write!(f, "Error at end: {}", self.err),
        }
    }
}

macro_rules! emit {
    ( $c:ident, const, $( $op:expr ),* ) => {{
        $( $c.chunk.write_constant($op, $c.parser.line); )*
    }};

    ( $c:ident, $( $op:expr ),* ) => {{
        $( $c.chunk.write($op, $c.parser.line); )*
    }};
}

pub(crate) struct Compiler<'a> {
    scanner: Scanner<'a>,
    parser: Parser<'a>,
    errors: Vec<CompileError<'a>>,
    panic_mode: bool,
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            scanner: Scanner::new(source),
            parser: Parser {
                current: None,
                previous: None,
                line: 1,
            },
            errors: Vec::new(),
            panic_mode: false,
            chunk: Chunk::new(""),
        }
    }

    pub fn compile(mut self) -> Result<Chunk, Vec<CompileError<'a>>> {
        self.advance();
        self.expression();

        if self.scanner.next().is_some() {
            self.error_at_current(Error::ExpectedEOF);
        }

        emit!(self, Op::Return);

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.chunk)
        }
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current.take();
        self.parser.current = loop {
            match self.scanner.next() {
                // continue churning through the errors until you find `None` (EOF) or
                // `Some(Ok(...))` (a non-error token)
                Some(Err((_, e))) => self.error_at_current(e),
                Some(Ok(token)) => {
                    self.parser.line = token.line;
                    break Some(Ok(token));
                }
                None => break None,
            }
        };
    }

    fn consume(&mut self, r#type: TokenType, if_not: Error) {
        if let Some(Ok(Token { r#type: t, .. })) = self.parser.current {
            if t == r#type {
                self.advance();
                return;
            }
        }

        self.error_at_current(if_not);
    }

    fn error(&mut self, err: Error) {
        self.error_at(self.parser.previous.clone(), err);
    }

    fn error_at_current(&mut self, err: Error) {
        self.error_at(self.parser.current.clone(), err);
    }

    fn error_at(&mut self, token: MaybeToken<'a>, err: Error) {
        if !self.panic_mode {
            self.panic_mode = true;
            self.errors.push(CompileError { err, token });
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        if let Some(operator) = self.parser.previous_type() {
            if let (Some(prefix_func), _, _) = get_rule(operator) {
                prefix_func(self);
            } else {
                self.error(Error::MissingPrefixExpr);
                return;
            }
        }

        while let Some(operator) = self.parser.current_type() {
            let (_, maybe_infix, rule_precedence) = get_rule(operator);
            if precedence > rule_precedence {
                break;
            }

            self.advance();

            if let Some(infix_func) = maybe_infix {
                infix_func(self);
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, Error::MissingRightParen);
    }

    fn number(&mut self) {
        if let Some(Ok(token)) = &self.parser.previous {
            if let Ok(v) = token.text.parse() {
                self.chunk.write_constant(Value(v), self.parser.line);
            }
        }
    }

    fn unary(&mut self) {
        if let Some(operator) = self.parser.previous_type() {
            self.parse_precedence(Precedence::Unary);

            match operator {
                TokenType::Minus => emit!(self, Op::Negate),
                _ => unreachable!(),
            }
        }
    }

    fn binary(&mut self) {
        if let Some(operator) = self.parser.previous_type() {
            // right operand
            let rule = get_rule(operator);
            self.parse_precedence(rule.2.next());
            emit!(
                self,
                match operator {
                    TokenType::Plus => Op::Add,
                    TokenType::Minus => Op::Subtract,
                    TokenType::Star => Op::Multiply,
                    TokenType::Slash => Op::Divide,
                    _ => unreachable!(),
                }
            );
        }
    }
}

struct Parser<'a> {
    current: MaybeToken<'a>,
    previous: MaybeToken<'a>,
    line: usize,
}

impl<'a> Parser<'a> {
    fn previous_type(&self) -> Option<TokenType> {
        if let Some(Ok(token)) = &self.previous {
            Some(token.r#type)
        } else {
            None
        }
    }

    fn current_type(&self) -> Option<TokenType> {
        if let Some(Ok(token)) = &self.current {
            Some(token.r#type)
        } else {
            None
        }
    }
}

type ParseFn<'a, 'b> = fn(&'b mut Compiler<'a>);
type Rule<'a, 'b> = (Option<ParseFn<'a, 'b>>, Option<ParseFn<'a, 'b>>, Precedence);

fn get_rule<'a, 'b>(sigil: TokenType) -> Rule<'a, 'b> {
    use TokenType::*;

    match sigil {
        LeftParen => (Some(Compiler::grouping), None, Precedence::None),
        RightParen => (None, None, Precedence::None),
        LeftBrace => (None, None, Precedence::None),
        RightBrace => (None, None, Precedence::None),
        Comma => (None, None, Precedence::None),
        Dot => (None, None, Precedence::None),
        Minus => (
            Some(Compiler::unary),
            Some(Compiler::binary),
            Precedence::Term,
        ),
        Plus => (None, Some(Compiler::binary), Precedence::Term),
        Semi => (None, None, Precedence::None),
        Slash => (None, Some(Compiler::binary), Precedence::Factor),
        Star => (None, Some(Compiler::binary), Precedence::Factor),
        Bang => (None, None, Precedence::None),
        BangEqual => (None, None, Precedence::None),
        Equal => (None, None, Precedence::None),
        DoubleEqual => (None, None, Precedence::None),
        Greater => (None, None, Precedence::None),
        GreaterEqual => (None, None, Precedence::None),
        Less => (None, None, Precedence::None),
        LessEqual => (None, None, Precedence::None),
        Identifier => (None, None, Precedence::None),
        r#String => (None, None, Precedence::None),
        Number => (Some(Compiler::number), None, Precedence::None),
        And => (None, None, Precedence::None),
        Class => (None, None, Precedence::None),
        Else => (None, None, Precedence::None),
        False => (None, None, Precedence::None),
        For => (None, None, Precedence::None),
        Fun => (None, None, Precedence::None),
        If => (None, None, Precedence::None),
        Nil => (None, None, Precedence::None),
        Or => (None, None, Precedence::None),
        Print => (None, None, Precedence::None),
        Return => (None, None, Precedence::None),
        Super => (None, None, Precedence::None),
        This => (None, None, Precedence::None),
        True => (None, None, Precedence::None),
        Var => (None, None, Precedence::None),
        While => (None, None, Precedence::None),
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl<'a, 'b> Precedence {
    fn next(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            _ => Self::Primary,
        }
    }
}
