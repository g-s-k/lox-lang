use std::str::CharIndices;

use super::{CompileErrorType, Token, TokenType};

type Iter<'a> = CharIndices<'a>;
type Entry = Option<(usize, char)>;
pub(crate) type ScanError = (usize, CompileErrorType);

pub(crate) struct Scanner<'a> {
    source: &'a str,
    iter: Iter<'a>,
    from: usize,
    line: usize,
    previous: Entry,
    current: Entry,
    next: Entry,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_whitespace();
        self.next();

        let c = if let Some((_, c)) = self.previous {
            c
        } else {
            return None;
        };

        let token = match c {
            'A'..='Z' | 'a'..='z' | '_' => self.identifier(c),
            '0'..='9' => {
                self.number();
                TokenType::Number
            }
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ';' => TokenType::Semi,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            '/' => TokenType::Slash,
            '*' => TokenType::Star,
            '!' if self.peek('=') => {
                self.next();
                TokenType::BangEqual
            }
            '=' if self.peek('=') => {
                self.next();
                TokenType::DoubleEqual
            }
            '<' if self.peek('=') => {
                self.next();
                TokenType::LessEqual
            }
            '>' if self.peek('=') => {
                self.next();
                TokenType::GreaterEqual
            }
            '!' => TokenType::Bang,
            '=' => TokenType::Equal,
            '<' => TokenType::Less,
            '>' => TokenType::Greater,
            '"' => {
                if let Err(e) = self.string() {
                    return Some(Err(e));
                }
                TokenType::r#String
            }
            _ => return Some(Err((self.line, CompileErrorType::UnexpectedChar(c)))),
        };

        let text = self.advance();

        #[cfg(feature = "trace-scanning")]
        log::debug!("[line {}] {:?} \"{}\"", self.line, token, text);

        Some(Ok(Token {
            r#type: token,
            line: self.line,
            text,
        }))
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut new = Self {
            source,
            iter: source.char_indices(),
            from: 0,
            line: 1,
            previous: None,
            current: None,
            next: None,
        };

        new.next();
        new.next();

        new
    }

    fn advance(&mut self) -> &'a str {
        let head;
        if let Some((idx, _)) = self.current {
            head = &self.source[self.from..idx];
            self.from = idx;
        } else {
            head = &self.source[self.from..];
        }

        head
    }

    fn next(&mut self) {
        self.previous = self.current.take();
        self.current = self.next.take();
        self.next = self.iter.next();
    }

    fn peek(&self, to_match: char) -> bool {
        if let Some((_, c)) = self.current {
            c == to_match
        } else {
            false
        }
    }

    fn peek_next(&self, to_match: char) -> bool {
        if let Some((_, c)) = self.next {
            c == to_match
        } else {
            false
        }
    }

    fn trim_whitespace(&mut self) {
        let mut in_comment = false;

        while let Some((_, c)) = self.current {
            match c {
                '\n' => {
                    self.line += 1;

                    if in_comment {
                        in_comment = false;
                    }
                }
                '/' if self.peek_next('/') => in_comment = true,
                _ if in_comment => (),
                _ if c.is_whitespace() => (),
                _ => break,
            }

            self.next();
        }

        self.advance();
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while let Some((_, c)) = self.current {
            match c {
                '\n' => self.line += 1,
                '"' => {
                    self.next();
                    break;
                }
                _ => (),
            }

            if self.next.is_none() {
                return Err((self.line, CompileErrorType::UnterminatedString));
            }

            self.next();
        }

        Ok(())
    }

    fn number(&mut self) {
        while let Some((_, '0'..='9')) = self.current {
            self.next();
        }

        if let (Some((_, '.')), Some((_, '0'..='9'))) = (self.current, self.next) {
            self.next();
        }

        while let Some((_, '0'..='9')) = self.current {
            self.next();
        }
    }

    fn current_is_identifier_token(&self) -> bool {
        if let Some((_, 'A'..='Z')) | Some((_, 'a'..='z')) | Some((_, '_')) | Some((_, '0'..='9')) =
            self.current
        {
            true
        } else {
            false
        }
    }

    fn identifier(&mut self, first_char: char) -> TokenType {
        if let Some(t) = match first_char {
            'a' => self.maybe_keyword("nd", TokenType::And),
            'b' => self.maybe_keyword("reak", TokenType::Break),
            'd' => self.maybe_keyword("efault", TokenType::r#Default),
            'e' => self.maybe_keyword("lse", TokenType::Else),
            'i' => self.maybe_keyword("f", TokenType::If),
            'n' => self.maybe_keyword("il", TokenType::Nil),
            'o' => self.maybe_keyword("r", TokenType::Or),
            'p' => self.maybe_keyword("rint", TokenType::Print),
            'r' => self.maybe_keyword("eturn", TokenType::Return),
            'v' => self.maybe_keyword("ar", TokenType::Var),
            'w' => self.maybe_keyword("hile", TokenType::While),
            'c' if self.peek('a') => self.maybe_keyword("ase", TokenType::Case),
            'c' if self.peek('l') => self.maybe_keyword("lass", TokenType::Class),
            'c' if self.peek('o') => self.maybe_keyword("ontinue", TokenType::Continue),
            'f' if self.peek('a') => self.maybe_keyword("alse", TokenType::False),
            'f' if self.peek('o') => self.maybe_keyword("or", TokenType::For),
            'f' if self.peek('u') => self.maybe_keyword("un", TokenType::Fun),
            's' if self.peek('u') => self.maybe_keyword("uper", TokenType::Super),
            's' if self.peek('w') => self.maybe_keyword("witch", TokenType::Switch),
            't' if self.peek('h') => self.maybe_keyword("his", TokenType::This),
            't' if self.peek('r') => self.maybe_keyword("rue", TokenType::True),
            _ => None,
        } {
            return t;
        }

        while self.current_is_identifier_token() {
            self.next();
        }

        TokenType::Identifier
    }

    fn maybe_keyword(&mut self, rest: &'static str, success_type: TokenType) -> Option<TokenType> {
        for r_c in rest.chars() {
            match self.current {
                Some((_, c)) if c == r_c => {
                    self.next();
                }
                _ => return None,
            }
        }

        if self.current_is_identifier_token() {
            None
        } else {
            Some(success_type)
        }
    }
}
