use std::{convert::TryInto, error, fmt, mem};

mod scanner;
mod token;

use {
    super::{Fun, Op, Value},
    scanner::{ScanError, Scanner},
    token::{Token, TokenType},
};

type MaybeToken<'a> = Option<Result<Token<'a>, ScanError>>;

#[derive(Debug)]
pub(crate) struct CompileError {
    pub err: CompileErrorType,
    pub line: usize,
    text: Option<String>,
}

impl error::Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error", self.line)?;
        if let Some(t) = &self.text {
            write!(f, " at \"{}\"", t)?;
        }
        write!(f, ": {}", self.err)
    }
}

#[derive(Clone, Debug)]
pub enum CompileErrorType {
    // compilation
    UnexpectedChar,
    UnterminatedString,
    MissingLeftParen(&'static str),
    MissingRightParen(&'static str),
    MissingLeftBrace(&'static str),
    MissingRightBrace(&'static str),
    MissingPrefixExpr,
    MissingSemi,
    MissingVarName,
    MissingClassName,
    MissingPropertyName,
    MissingFunName,
    MissingParamName,
    TooManyParams,
    DuplicateLocal(String),
    ReadBeforeDefined(String),
    JumpTooLarge(usize),
    TopLevelReturn,
    ExpectedEOF,
}

impl fmt::Display for CompileErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CompileErrorType::*;

        match self {
            UnexpectedChar => write!(f, "unexpected character"),
            UnterminatedString => write!(f, "unterminated string"),
            MissingLeftParen(location) => write!(f, "expected '(' {}", location),
            MissingRightParen(location) => write!(f, "expected ')' {}", location),
            MissingLeftBrace(location) => write!(f, "expected '{{' {}", location),
            MissingRightBrace(location) => write!(f, "expected '}}' {}", location),
            MissingPrefixExpr => write!(f, "expected expression"),
            MissingSemi => write!(f, "expected ';' after statement"),
            MissingVarName => write!(f, "expect variable name"),
            MissingClassName => write!(f, "expect class name"),
            MissingPropertyName => write!(f, "expect property name after '.'"),
            MissingFunName => write!(f, "expect function name"),
            MissingParamName => write!(f, "expect parameter name"),
            TooManyParams => write!(f, "cannot exceed 255 parameters"),
            DuplicateLocal(who) => write!(f, "variable `{}` already declared in this scope", who),
            ReadBeforeDefined(who) => {
                write!(f, "tried to use variable `{}` in its own initializer", who)
            }
            JumpTooLarge(distance) => write!(
                f,
                "jump distance {:x} is too large to fit in a `u16` (max value {:x})",
                distance,
                u16::MAX
            ),
            TopLevelReturn => write!(f, "return statement outside a function body"),
            ExpectedEOF => write!(f, "expected end of input"),
        }
    }
}

macro_rules! short_or_long {
    ( $value: expr; $short: ident <> $long: ident $(, $extra: expr)? ) => {{
        match $value.try_into() {
            Ok(v) => Op::$short(v $(, $extra)?),
            Err(_) => Op::$long($value as u16 $(, $extra)?),
        }
    }};
}

macro_rules! emit {
    (const $c: ident : $short: ident / $long: ident, $val: expr $(, $extra: expr)? ) => {{
        let index = $c.fun.inner.chunk.add_constant($val);
        $c.fun.inner.chunk.write(
            short_or_long!(index; $short <> $long $(, $extra)?),
            $c.parser.line
        );
    }};

    (jump $c: ident : $variant: ident) => {{
        $c.fun.inner.chunk.write(Op::$variant(u16::MAX), $c.parser.line)
    }};

    (loop $c: ident : $loop_start: expr) => {{
        let loop_end = $c.fun.inner.chunk.code.len();
        let difference = loop_end - $loop_start + 1;

        let difference = match difference.try_into() {
            Err(_) => {
                $c.error(CompileErrorType::JumpTooLarge(difference));
                u16::MAX
            }
            Ok(d) => d,
        };

        $c.fun.inner.chunk.write(Op::Loop(difference), $c.parser.line);
    }};

    ( $c:ident, $( $op:expr ),* ) => {{
        $( $c.fun.inner.chunk.write($op, $c.parser.line); )*
    }};
}

enum FunType {
    Script,
    Function,
}

struct Local<'compile> {
    name: &'compile str,
    depth: usize,
    is_defined: bool,
    is_captured: bool,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Upvalue {
    pub index: usize,
    pub is_local: bool,
}

struct FunWrapper<'compile> {
    enclosing: Option<Box<FunWrapper<'compile>>>,
    inner: Fun,
    r#type: FunType,
    locals: Vec<Local<'compile>>,
    upvalues: Vec<Upvalue>,
    scope_depth: usize,
}

impl<'compile> FunWrapper<'compile> {
    fn new<T: ToString>(name: T, r#type: FunType) -> Self {
        Self {
            enclosing: None,
            inner: Fun::new(name, 0),
            r#type,
            locals: vec![Local {
                name: "",
                depth: 0,
                is_defined: true,
                is_captured: false,
            }],
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }

    fn resolve_local(&self, query: &str) -> Option<(usize, bool)> {
        for (
            index,
            Local {
                name, is_defined, ..
            },
        ) in self.locals.iter().enumerate().rev()
        {
            if *name == query {
                return Some((index, *is_defined));
            }
        }

        None
    }

    fn resolve_upvalue(&mut self, query: &str) -> Option<usize> {
        if let Some(parent) = &mut self.enclosing {
            if let Some((index, _)) = parent.resolve_local(query) {
                parent.locals[index].is_captured = true;
                return Some(self.add_upvalue(index, true));
            } else if let Some(index) = parent.resolve_upvalue(query) {
                return Some(self.add_upvalue(index, false));
            }
        }

        None
    }

    fn add_upvalue(&mut self, local_index: usize, is_local: bool) -> usize {
        for (idx, u_val) in self.upvalues.iter().enumerate() {
            if u_val.index == local_index && u_val.is_local == is_local {
                return idx;
            }
        }

        self.upvalues.push(Upvalue {
            index: local_index,
            is_local,
        });
        self.upvalues.len() - 1
    }
}

pub(crate) struct Compiler<'compile> {
    scanner: Scanner<'compile>,
    parser: Parser<'compile>,
    errors: Vec<CompileError>,
    fun: Box<FunWrapper<'compile>>,
    alloc: Alloc<'compile>,
}

type Alloc<'compile> = &'compile mut dyn FnMut(Fun) -> Value;
type ParseFn<'compile> = fn(&mut Compiler<'compile>, bool);
type Rule<'compile> = (
    Option<ParseFn<'compile>>,
    Option<ParseFn<'compile>>,
    Precedence,
);

impl<'compile> Compiler<'compile> {
    pub fn compile(
        source: &'compile str,
        alloc: Alloc<'compile>,
    ) -> Result<Fun, Vec<CompileError>> {
        let mut compiler = Self {
            scanner: Scanner::new(source),
            parser: Parser {
                current: None,
                previous: None,
                line: 1,
                panic_mode: false,
            },
            errors: Vec::new(),
            fun: Box::new(FunWrapper::new("script", FunType::Script)),
            alloc,
        };

        compiler.advance();

        while let Some(Ok(_)) = &compiler.parser.current {
            compiler.declaration();
        }

        if compiler.scanner.next().is_some() {
            compiler.error_at_current(CompileErrorType::ExpectedEOF);
        }

        compiler.end_function();

        if compiler.errors.is_empty() {
            Ok(compiler.fun.inner)
        } else {
            Err(compiler.errors)
        }
    }

    fn end_function(&mut self) {
        emit!(self, Op::Nil, Op::Return);

        #[cfg(feature = "trace-compilation")]
        log::debug!("\n{}", self.fun.inner.chunk);
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

            let new_err = CompileError {
                err,
                line: self.parser.line,
                text: token
                    .map(Result::unwrap)
                    .map(|Token { text, .. }| text.to_string()),
            };

            log::error!("{}", new_err);
            self.errors.push(new_err);
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
            Some(TokenType::Class) => {
                self.advance();
                self.class_declaration();
            }
            Some(TokenType::Fun) => {
                self.advance();
                self.fun_declaration();
            }
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

    fn class_declaration(&mut self) {
        let name_idx = self.parse_variable(CompileErrorType::MissingClassName);
        emit!(self, short_or_long!(name_idx; Class <> ClassLong));
        self.define_variable(name_idx);

        // {
        self.consume(
            TokenType::LeftBrace,
            CompileErrorType::MissingLeftBrace("before class body"),
        );

        // }
        self.consume(
            TokenType::RightBrace,
            CompileErrorType::MissingRightBrace("after class body"),
        );
    }

    fn fun_declaration(&mut self) {
        let name_idx = self.parse_variable(CompileErrorType::MissingFunName);
        self.mark_last_local_initialized();
        self.function(FunType::Function);
        self.define_variable(name_idx);
    }

    fn function(&mut self, r#type: FunType) {
        // insert at head of list
        let fun_name = match self.parser.previous {
            Some(Ok(Token {
                r#type: TokenType::Identifier,
                text,
                ..
            })) => text.to_string().into_boxed_str(),
            _ => unreachable!("cannot compile a function without a name"),
        };
        let old_fun_wrapper =
            mem::replace(&mut self.fun, Box::new(FunWrapper::new(fun_name, r#type)));
        self.fun.enclosing = Some(old_fun_wrapper);

        self.begin_scope();

        // (
        self.consume(
            TokenType::LeftParen,
            CompileErrorType::MissingLeftParen("after function name"),
        );

        // <params>
        match self.parser.current_type() {
            Some(TokenType::RightParen) => (), // empty list
            _ => loop {
                if self.fun.inner.arity == u8::MAX {
                    self.error_at_current(CompileErrorType::TooManyParams);
                } else {
                    self.fun.inner.arity += 1;
                }

                let param_constant = self.parse_variable(CompileErrorType::MissingParamName);
                self.define_variable(param_constant);

                if let Some(TokenType::Comma) = self.parser.current_type() {
                    self.advance();
                } else {
                    break;
                }
            },
        }

        // )
        self.consume(
            TokenType::RightParen,
            CompileErrorType::MissingRightParen("after parameters"),
        );
        // {
        self.consume(
            TokenType::LeftBrace,
            CompileErrorType::MissingLeftBrace("before function body"),
        );

        // <body>
        self.block();

        self.end_function();

        // pop from head of list and insert into constant table
        if let Some(enclosing_fun) = self.fun.enclosing.take() {
            let this_fun = mem::replace(&mut self.fun, enclosing_fun);

            let fun_value = (self.alloc)(this_fun.inner);

            if this_fun.upvalues.is_empty() {
                emit!(const self: Constant / ConstantLong, fun_value)
            } else {
                let upvalues = this_fun.upvalues.into_boxed_slice();
                emit!(const self: Closure / ClosureLong, fun_value, upvalues);
            }
        } else {
            // should be infallible, enforce with runtime assertion
            unreachable!();
        }
    }

    fn parse_variable(&mut self, err: CompileErrorType) -> usize {
        self.consume(TokenType::Identifier, err);
        if let Some(Ok(Token { text, .. })) = self.parser.previous {
            self.declare_variable(text);

            // globals only
            if self.fun.scope_depth == 0 {
                self.fun
                    .inner
                    .chunk
                    .add_constant(Value::r#String(text.to_string().into_boxed_str()))
            } else {
                0
            }
        } else {
            unreachable!();
        }
    }

    fn declare_variable(&mut self, name: &'compile str) {
        if self.fun.scope_depth == 0 {
            return;
        }

        // check for shadowing in current scope only
        if self
            .fun
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth >= self.fun.scope_depth)
            .any(|l| l.name == name)
        {
            self.error(CompileErrorType::DuplicateLocal(name.to_string()));
        }

        self.add_local(name);
    }

    fn add_local(&mut self, name: &'compile str) {
        self.fun.locals.push(Local {
            name,
            depth: self.fun.scope_depth,
            is_defined: false,
            is_captured: false,
        });
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

        self.define_variable(var_const);
    }

    fn define_variable(&mut self, var_const_index: usize) {
        // globals only
        if self.fun.scope_depth == 0 {
            emit!(
                self,
                short_or_long!(var_const_index; DefineGlobal <> DefineGlobalLong)
            );
        } else {
            self.mark_last_local_initialized();
        }
    }

    fn mark_last_local_initialized(&mut self) {
        if self.fun.scope_depth > 0 {
            if let Some(last) = self.fun.locals.last_mut() {
                last.is_defined = true;
            }
        }
    }

    fn statement(&mut self) {
        match self.parser.current_type() {
            Some(TokenType::Print) => self.print_statement(),
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::If) => self.if_statement(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::LeftBrace) => {
                self.advance();
                self.begin_scope();
                self.block();
                self.end_scope();
            }
            _ => self.expression_statement(),
        }
    }

    fn begin_scope(&mut self) {
        self.fun.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.fun.scope_depth -= 1;

        for local in self.fun.locals.iter().rev() {
            if local.depth < self.fun.scope_depth {
                break;
            }

            if local.is_captured {
                emit!(self, Op::CloseUpvalue);
            } else {
                emit!(self, Op::Pop);
            }
        }
    }

    fn block(&mut self) {
        while let Some(Ok(token)) = &self.parser.current {
            if let TokenType::RightBrace = token.r#type {
                break;
            }

            self.declaration();
        }

        self.consume(
            TokenType::RightBrace,
            CompileErrorType::MissingRightBrace("after block"),
        );
    }

    fn print_statement(&mut self) {
        self.advance();
        self.expression();
        self.consume(TokenType::Semi, CompileErrorType::MissingSemi);
        emit!(self, Op::Print);
    }

    fn if_statement(&mut self) {
        // IF
        self.advance();
        // (
        self.consume(
            TokenType::LeftParen,
            CompileErrorType::MissingLeftParen("after 'if'"),
        );
        // <condition>
        self.expression();
        // )
        self.consume(
            TokenType::RightParen,
            CompileErrorType::MissingRightParen("after condition"),
        );

        let then_jump = emit!(jump self: JumpIfFalse);
        emit!(self, Op::Pop);
        // <then branch>
        self.statement();

        let else_jump = emit!(jump self: Jump);
        if let Err(e) = self.fun.inner.chunk.patch_jump(then_jump) {
            self.error(e);
        }
        emit!(self, Op::Pop);

        // ELSE
        if let Some(TokenType::Else) = self.parser.current_type() {
            self.advance();
            // <else branch>
            self.statement();
        }

        if let Err(e) = self.fun.inner.chunk.patch_jump(else_jump) {
            self.error(e);
        }
    }

    fn return_statement(&mut self) {
        if let FunType::Script = self.fun.r#type {
            self.error(CompileErrorType::TopLevelReturn);
        }

        // RETURN
        self.advance();

        if let Some(TokenType::Semi) = self.parser.current_type() {
            // no return value
            self.advance();
            emit!(self, Op::Nil, Op::Return);
        } else {
            self.expression();
            self.consume(TokenType::Semi, CompileErrorType::MissingSemi);
            emit!(self, Op::Return);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.fun.inner.chunk.code.len();

        // WHILE
        self.advance();
        // (
        self.consume(
            TokenType::LeftParen,
            CompileErrorType::MissingLeftParen("after 'while'"),
        );
        // <condition>
        self.expression();
        // )
        self.consume(
            TokenType::RightParen,
            CompileErrorType::MissingRightParen("after condition"),
        );

        let exit_jmp = emit!(jump self: JumpIfFalse);

        emit!(self, Op::Pop);
        self.statement();

        emit!(loop self: loop_start);

        if let Err(e) = self.fun.inner.chunk.patch_jump(exit_jmp) {
            self.error(e);
        }

        emit!(self, Op::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        // FOR
        self.advance();
        // (
        self.consume(
            TokenType::LeftParen,
            CompileErrorType::MissingLeftParen("after 'for'"),
        );
        // <initializer> ;
        match self.parser.current_type() {
            Some(TokenType::Semi) => (), // no initializer
            Some(TokenType::Var) => {
                self.advance();
                self.var_declaration();
            }
            _ => self.expression_statement(),
        }

        let mut loop_start = self.fun.inner.chunk.code.len();
        let exit_jmp;

        // <exit condition> ;
        if let Some(TokenType::Semi) = self.parser.current_type() {
            self.advance();
            exit_jmp = None;
        } else {
            self.expression();
            self.consume(TokenType::Semi, CompileErrorType::MissingSemi);

            exit_jmp = Some(emit!(jump self: JumpIfFalse));
            emit!(self, Op::Pop);
        }

        // <post update> )
        if let Some(TokenType::RightParen) = self.parser.current_type() {
            self.advance();
        } else {
            let body_jmp = emit!(jump self: Jump);

            let increment_start = self.fun.inner.chunk.code.len();
            self.expression();
            emit!(self, Op::Pop);
            self.consume(
                TokenType::RightParen,
                CompileErrorType::MissingRightParen("after 'for' clauses"),
            );

            emit!(loop self: loop_start);
            loop_start = increment_start;
            if let Err(e) = self.fun.inner.chunk.patch_jump(body_jmp) {
                self.error(e);
            }
        }

        // <body>
        self.statement();

        emit!(loop self: loop_start);

        if let Some(jmp) = exit_jmp {
            if let Err(e) = self.fun.inner.chunk.patch_jump(jmp) {
                self.error(e);
            }
        }

        self.end_scope();
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
        self.consume(
            TokenType::RightParen,
            CompileErrorType::MissingRightParen("after expression"),
        );
    }

    fn number(&mut self, _: bool) {
        if let Some(Ok(token)) = &self.parser.previous {
            if let Ok(v) = token.text.parse() {
                emit!(const self: Constant / ConstantLong, Value::Number(v))
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
            t.text[1..t.text.len() - 1].to_string().into_boxed_str()
        } else {
            return;
        };

        emit!(const self: Constant / ConstantLong, Value::r#String(text_without_quotes))
    }

    fn variable(&mut self, _: bool) {
        let token_text = if let Some(Ok(Token { text, .. })) = &self.parser.previous {
            (&**text).clone()
        } else {
            unreachable!();
        };

        self.named_variable(token_text);
    }

    fn named_variable(&mut self, name: &str) {
        let resolved_index = self.resolve_local(name);
        let value_maker = || Value::r#String(name.to_string().into_boxed_str());

        // "Set" expression
        if let Some(TokenType::Equal) = self.parser.current_type() {
            self.advance();
            self.expression();

            if let Some(idx) = resolved_index {
                emit!(self, short_or_long!(idx; SetLocal <> SetLocalLong));
            } else if let Some(idx) = self.fun.resolve_upvalue(name) {
                emit!(self, short_or_long!(idx; SetUpvalue <> SetUpvalueLong));
            } else {
                emit!(const self: SetGlobal / SetGlobalLong, value_maker());
            }
        // "Get" expression
        } else if let Some(idx) = resolved_index {
            emit!(self, short_or_long!(idx; GetLocal <> GetLocalLong));
        } else if let Some(idx) = self.fun.resolve_upvalue(name) {
            emit!(self, short_or_long!(idx; GetUpvalue <> GetUpvalueLong));
        } else {
            emit!(const self: GetGlobal / GetGlobalLong, value_maker());
        }
    }

    fn resolve_local(&mut self, query: &str) -> Option<usize> {
        if let Some((index, is_defined)) = self.fun.resolve_local(query) {
            if !is_defined {
                self.error(CompileErrorType::ReadBeforeDefined(query.to_string()));
            }

            Some(index)
        } else {
            None
        }
    }

    fn and(&mut self, _: bool) {
        let end_jmp = emit!(jump self: JumpIfFalse);

        emit!(self, Op::Pop);
        self.parse_precedence(Precedence::And);

        if let Err(e) = self.fun.inner.chunk.patch_jump(end_jmp) {
            self.error(e);
        }
    }

    fn or(&mut self, _: bool) {
        let else_jmp = emit!(jump self: JumpIfFalse);
        let end_jmp = emit!(jump self: Jump);

        if let Err(e) = self.fun.inner.chunk.patch_jump(else_jmp) {
            self.error(e);
        }
        emit!(self, Op::Pop);

        self.parse_precedence(Precedence::Or);
        if let Err(e) = self.fun.inner.chunk.patch_jump(end_jmp) {
            self.error(e);
        }
    }

    fn call(&mut self, _: bool) {
        let arg_count = self.argument_list();
        emit!(self, Op::Call(arg_count));
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;

        match self.parser.current_type() {
            Some(TokenType::RightParen) => (), // empty list
            _ => loop {
                self.expression();

                if arg_count == u8::MAX {
                    self.error(CompileErrorType::TooManyParams);
                } else {
                    arg_count += 1;
                }

                if let Some(TokenType::Comma) = self.parser.current_type() {
                    self.advance();
                } else {
                    break;
                }
            },
        }

        // )
        self.consume(
            TokenType::RightParen,
            CompileErrorType::MissingRightParen("after arguments"),
        );
        arg_count
    }

    fn dot(&mut self, can_assign: bool) {
        let property_idx = self.parse_variable(CompileErrorType::MissingPropertyName);

        if can_assign && self.parser.current_type() == Some(TokenType::Equal) {
            self.advance();
            self.expression();
            emit!(
                self,
                short_or_long!(property_idx; SetProperty <> SetPropertyLong)
            );
        } else {
            emit!(
                self,
                short_or_long!(property_idx; GetProperty <> GetPropertyLong)
            );
        }
    }

    fn get_rule(sigil: TokenType) -> Rule<'compile> {
        match sigil {
            TokenType::LeftParen => (Some(Self::grouping), Some(Self::call), Precedence::Call),
            TokenType::Minus => (Some(Self::unary), Some(Self::binary), Precedence::Term),
            TokenType::Plus => (None, Some(Self::binary), Precedence::Term),
            TokenType::Slash | TokenType::Star => (None, Some(Self::binary), Precedence::Factor),
            TokenType::Bang => (Some(Self::unary), None, Precedence::None),
            TokenType::BangEqual | TokenType::DoubleEqual => {
                (None, Some(Self::binary), Precedence::Equality)
            }
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => (None, Some(Self::binary), Precedence::Comparison),
            TokenType::Identifier => (Some(Self::variable), None, Precedence::None),
            TokenType::r#String => (Some(Self::string), None, Precedence::None),
            TokenType::Number => (Some(Self::number), None, Precedence::None),
            TokenType::And => (None, Some(Self::and), Precedence::And),
            TokenType::Or => (None, Some(Self::or), Precedence::Or),
            TokenType::False | TokenType::Nil | TokenType::True => {
                (Some(Self::literal), None, Precedence::None)
            }
            TokenType::Dot => (None, Some(Self::dot), Precedence::Call),
            _ => (None, None, Precedence::None),
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
    fn next(self) -> Self {
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
