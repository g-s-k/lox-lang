use std::fmt;

use super::{Chunk, Error, ErrorCategory, Op, ScanError, Scanner, Token, TokenType, Value};

type MaybeToken<'a> = Option<Result<Token<'a>, ScanError>>;

#[derive(Debug)]
pub(crate) struct CompileError {
    pub err: CompileErrorType,
    line: usize,
    text: Option<String>,
}

impl Error for CompileError {
    fn category(&self) -> ErrorCategory {
        ErrorCategory::Compilation
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error", self.line)?;
        if let Some(t) = &self.text {
            write!(f, " at \"{}\"", t)?;
        }
        write!(f, ": {}", self.err)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CompileErrorType {
    // compilation
    UnexpectedChar,
    UnterminatedString,
    MissingRightParen,
    MissingPrefixExpr,
    MissingSemi,
    MissingVarName,
    ExpectedEOF,
}

impl fmt::Display for CompileErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CompileErrorType::*;

        match self {
            UnexpectedChar => write!(f, "unexpected character"),
            UnterminatedString => write!(f, "unterminated string"),
            MissingRightParen => write!(f, "expected ')'"),
            MissingPrefixExpr => write!(f, "expected expression"),
            MissingSemi => write!(f, "expected ';' after statement"),
            MissingVarName => write!(f, "expect variable name"),
            ExpectedEOF => write!(f, "expected end of input"),
        }
    }
}

macro_rules! emit {
    (const $c: ident : $short: ident / $long: ident, $val: expr ) => {{
        let index = $c.chunk.add_constant($val);
        $c.chunk.write(
            if index < 256 {
                Op::$short(index as u8)
            } else {
                Op::$long(index as u16)
            },
            $c.parser.line
        );
    }};
    ( $c:ident, const, $( $val:expr ),* ) => {{
        $( $c.chunk.write_constant($val, $c.parser.line); )*
    }};

    ( $c:ident, $( $op:expr ),* ) => {{
        $( $c.chunk.write($op, $c.parser.line); )*
    }};
}

pub(crate) struct Compiler<'compile> {
    scanner: Scanner<'compile>,
    parser: Parser<'compile>,
    errors: Vec<CompileError>,
    chunk: Chunk,
}

type ParseFn<'compile> = fn(&mut Compiler<'compile>, bool);
type Rule<'a> = (Option<ParseFn<'a>>, Option<ParseFn<'a>>, Precedence);

impl<'compile> Compiler<'compile> {
    pub fn compile(source: &'compile str) -> Result<Chunk, Vec<CompileError>> {
        let mut compiler = Self {
            scanner: Scanner::new(source),
            parser: Parser {
                current: None,
                previous: None,
                line: 1,
                panic_mode: false,
            },
            errors: Vec::new(),
            chunk: Chunk::new(""),
        };

        compiler.advance();

        while let Some(Ok(_)) = &compiler.parser.current {
            compiler.declaration();
        }

        if compiler.scanner.next().is_some() {
            compiler.error_at_current(CompileErrorType::ExpectedEOF);
        }

        emit!(compiler, Op::Return);

        if !compiler.errors.is_empty() {
            Err(compiler.errors)
        } else {
            Ok(compiler.chunk)
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

    fn consume(&mut self, r#type: TokenType, if_not: CompileErrorType) {
        if let Some(Ok(Token { r#type: t, .. })) = self.parser.current {
            if t == r#type {
                self.advance();
                return;
            }
        }

        self.error_at_current(if_not);
    }

    fn error(&mut self, err: CompileErrorType) {
        self.error_at(self.parser.previous.clone(), err);
    }

    fn error_at_current(&mut self, err: CompileErrorType) {
        self.error_at(self.parser.current.clone(), err);
    }

    fn error_at(&mut self, token: MaybeToken<'compile>, err: CompileErrorType) {
        if !self.parser.panic_mode {
            self.parser.panic_mode = true;
            self.errors.push(CompileError {
                err,
                line: self.parser.line,
                text: token
                    .map(Result::unwrap)
                    .map(|Token { text, .. }| text.to_string()),
            });
            log::error!("{}", err);
        }
    }

    fn synchronize(&mut self) {
        self.parser.panic_mode = false;

        while let Some(maybe_token) = &self.parser.current {
            if let Some(TokenType::Semi) = self.parser.previous_type() {
                return;
            }

            if let Ok(token) = maybe_token {
                match token.r#type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return,
                    _ => (),
                }
            }

            self.advance();
        }
    }

    fn declaration(&mut self) {
        match self.parser.current_type() {
            Some(TokenType::Var) => {
                self.advance();
                self.var_declaration();
            }
            _ => self.statement(),
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    fn parse_variable(&mut self, err: CompileErrorType) -> usize {
        self.consume(TokenType::Identifier, err);
        if let Some(Ok(Token { text, .. })) = self.parser.previous {
            self.chunk.add_constant(Value::r#String(text.to_string()))
        } else {
            unreachable!();
        }
    }

    fn var_declaration(&mut self) {
        let var_const = self.parse_variable(CompileErrorType::MissingVarName);

        if let Some(TokenType::Equal) = self.parser.current_type() {
            self.advance();
            self.expression();
        } else {
            emit!(self, Op::Nil);
        }

        self.consume(TokenType::Semi, CompileErrorType::MissingSemi);

        emit!(
            self,
            if var_const < 256 {
                Op::DefineGlobal(var_const as u8)
            } else {
                Op::DefineGlobalLong(var_const as u16)
            }
        );
    }

    fn statement(&mut self) {
        match self.parser.current_type() {
            Some(TokenType::Print) => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) {
        self.advance();
        self.expression();
        self.consume(TokenType::Semi, CompileErrorType::MissingSemi);
        emit!(self, Op::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semi, CompileErrorType::MissingSemi);
        emit!(self, Op::Pop);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        let can_assign = (precedence as isize) <= (Precedence::Assignment as isize);
        self.advance();
        if let Some(operator) = self.parser.previous_type() {
            if let (Some(prefix_func), _, _) = Self::get_rule(operator) {
                prefix_func(self, can_assign);
            } else {
                self.error(CompileErrorType::MissingPrefixExpr);
                return;
            }
        }

        while let Some(operator) = self.parser.current_type() {
            let (_, maybe_infix, rule_precedence) = Self::get_rule(operator);
            if precedence > rule_precedence {
                break;
            }

            self.advance();

            if let Some(infix_func) = maybe_infix {
                infix_func(self, can_assign);
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume(TokenType::RightParen, CompileErrorType::MissingRightParen);
    }

    fn number(&mut self, _: bool) {
        if let Some(Ok(token)) = &self.parser.previous {
            if let Ok(v) = token.text.parse() {
                self.chunk
                    .write_constant(Value::Number(v), self.parser.line);
            }
        }
    }

    fn unary(&mut self, _: bool) {
        if let Some(operator) = self.parser.previous_type() {
            self.parse_precedence(Precedence::Unary);

            match operator {
                TokenType::Bang => emit!(self, Op::Not),
                TokenType::Minus => emit!(self, Op::Negate),
                _ => unreachable!(),
            }
        }
    }

    fn binary(&mut self, _: bool) {
        if let Some(operator) = self.parser.previous_type() {
            // right operand
            let rule = Self::get_rule(operator);
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

    fn literal(&mut self, _: bool) {
        match self.parser.previous_type() {
            Some(TokenType::False) => emit!(self, Op::False),
            Some(TokenType::Nil) => emit!(self, Op::Nil),
            Some(TokenType::True) => emit!(self, Op::True),
            _ => unreachable!(),
        }
    }

    fn string(&mut self, _: bool) {
        let text_without_quotes = if let Some(Ok(t)) = &self.parser.previous {
            t.text[1..t.text.len() - 1].to_string()
        } else {
            return;
        };

        emit!(const self: Constant / ConstantLong, Value::r#String(text_without_quotes))
    }

    fn variable(&mut self, _: bool) {
        let token = if let Some(Ok(token)) = &self.parser.previous {
            token.clone()
        } else {
            unreachable!();
        };

        self.named_variable(&token);
    }

    fn named_variable(&mut self, Token { text, .. }: &Token) {
        let value = Value::r#String(text.to_string());
        if let Some(TokenType::Equal) = self.parser.current_type() {
            self.advance();
            self.expression();
            emit!(const self: SetGlobal / SetGlobalLong, value);
        } else {
            emit!(const self: GetGlobal / GetGlobalLong, value);
        }
    }

    fn get_rule(sigil: TokenType) -> Rule<'compile> {
        use TokenType::*;

        match sigil {
            LeftParen => (Some(Self::grouping), None, Precedence::None),
            RightParen => (None, None, Precedence::None),
            LeftBrace => (None, None, Precedence::None),
            RightBrace => (None, None, Precedence::None),
            Comma => (None, None, Precedence::None),
            Dot => (None, None, Precedence::None),
            Minus => (Some(Self::unary), Some(Self::binary), Precedence::Term),
            Plus => (None, Some(Self::binary), Precedence::Term),
            Semi => (None, None, Precedence::None),
            Slash => (None, Some(Self::binary), Precedence::Factor),
            Star => (None, Some(Self::binary), Precedence::Factor),
            Bang => (Some(Self::unary), None, Precedence::None),
            BangEqual => (None, Some(Self::binary), Precedence::Equality),
            Equal => (None, None, Precedence::None),
            DoubleEqual => (None, Some(Self::binary), Precedence::Equality),
            Greater => (None, Some(Self::binary), Precedence::Comparison),
            GreaterEqual => (None, Some(Self::binary), Precedence::Comparison),
            Less => (None, Some(Self::binary), Precedence::Comparison),
            LessEqual => (None, Some(Self::binary), Precedence::Comparison),
            Identifier => (Some(Self::variable), None, Precedence::None),
            r#String => (Some(Self::string), None, Precedence::None),
            Number => (Some(Self::number), None, Precedence::None),
            And => (None, None, Precedence::None),
            Class => (None, None, Precedence::None),
            Else => (None, None, Precedence::None),
            False => (Some(Self::literal), None, Precedence::None),
            For => (None, None, Precedence::None),
            Fun => (None, None, Precedence::None),
            If => (None, None, Precedence::None),
            Nil => (Some(Self::literal), None, Precedence::None),
            Or => (None, None, Precedence::None),
            Print => (None, None, Precedence::None),
            Return => (None, None, Precedence::None),
            Super => (None, None, Precedence::None),
            This => (None, None, Precedence::None),
            True => (Some(Self::literal), None, Precedence::None),
            Var => (None, None, Precedence::None),
            While => (None, None, Precedence::None),
        }
    }
}

struct Parser<'a> {
    current: MaybeToken<'a>,
    previous: MaybeToken<'a>,
    line: usize,
    panic_mode: bool,
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

#[derive(Clone, Copy, PartialEq, PartialOrd)]
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
