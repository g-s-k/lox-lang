use std::fmt;

use super::{Chunk, Error, ObjectData, Op, ScanError, Scanner, Token, TokenType, Value, VM};

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
    vm: &'a mut VM,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, vm: &'a mut VM) -> Self {
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
            vm,
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
                self.chunk
                    .write_constant(Value::Number(v), self.parser.line);
            }
        }
    }

    fn unary(&mut self) {
        if let Some(operator) = self.parser.previous_type() {
            self.parse_precedence(Precedence::Unary);

            match operator {
                TokenType::Bang => emit!(self, Op::Not),
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
            match operator {
                TokenType::BangEqual => emit!(self, Op::Equal, Op::Not),
                TokenType::DoubleEqual => emit!(self, Op::Equal),
                TokenType::Greater => emit!(self, Op::Greater),
                TokenType::GreaterEqual => emit!(self, Op::Less, Op::Not),
                TokenType::Less => emit!(self, Op::Less),
                TokenType::LessEqual => emit!(self, Op::Greater, Op::Not),
                TokenType::Plus => emit!(self, Op::Add),
                TokenType::Minus => emit!(self, Op::Subtract),
                TokenType::Star => emit!(self, Op::Multiply),
                TokenType::Slash => emit!(self, Op::Divide),
                _ => unreachable!(),
            }
        }
    }

    fn literal(&mut self) {
        match self.parser.previous_type() {
            Some(TokenType::False) => emit!(self, Op::False),
            Some(TokenType::Nil) => emit!(self, Op::Nil),
            Some(TokenType::True) => emit!(self, Op::True),
            _ => unreachable!(),
        }
    }

    fn string(&mut self) {
        if let Some(Ok(t)) = &self.parser.previous {
            let text_without_quotes = t.text[1..t.text.len() - 1].to_string();
            let ptr = self.vm.alloc(ObjectData::r#String(text_without_quotes));

            emit!(self, const, Value::Object(ptr));
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
        Bang => (Some(Compiler::unary), None, Precedence::None),
        BangEqual => (None, Some(Compiler::binary), Precedence::Equality),
        Equal => (None, None, Precedence::None),
        DoubleEqual => (None, Some(Compiler::binary), Precedence::Equality),
        Greater => (None, Some(Compiler::binary), Precedence::Comparison),
        GreaterEqual => (None, Some(Compiler::binary), Precedence::Comparison),
        Less => (None, Some(Compiler::binary), Precedence::Comparison),
        LessEqual => (None, Some(Compiler::binary), Precedence::Comparison),
        Identifier => (None, None, Precedence::None),
        r#String => (Some(Compiler::string), None, Precedence::None),
        Number => (Some(Compiler::number), None, Precedence::None),
        And => (None, None, Precedence::None),
        Class => (None, None, Precedence::None),
        Else => (None, None, Precedence::None),
        False => (Some(Compiler::literal), None, Precedence::None),
        For => (None, None, Precedence::None),
        Fun => (None, None, Precedence::None),
        If => (None, None, Precedence::None),
        Nil => (Some(Compiler::literal), None, Precedence::None),
        Or => (None, None, Precedence::None),
        Print => (None, None, Precedence::None),
        Return => (None, None, Precedence::None),
        Super => (None, None, Precedence::None),
        This => (None, None, Precedence::None),
        True => (Some(Compiler::literal), None, Precedence::None),
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
