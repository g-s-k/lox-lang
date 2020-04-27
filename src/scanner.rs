use std::{iter::Peekable, str::CharIndices};

use super::{Token, TokenType};

type Iter<'a> = Peekable<CharIndices<'a>>;
pub(crate) type ScanError = (usize, &'static str);

pub(crate) struct Scanner<'a> {
    source: &'a str,
    iter: Iter<'a>,
    from: usize,
    line: usize,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_whitespace();

        if let Some(c) = self.next_char() {
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
                '!' | '=' | '<' | '>' => {
                    if let Some('=') = self.peek_char() {
                        self.iter.next();
                        match c {
                            '!' => TokenType::BangEqual,
                            '=' => TokenType::DoubleEqual,
                            '<' => TokenType::LessEqual,
                            '>' => TokenType::GreaterEqual,
                            _ => unreachable!(),
                        }
                    } else {
                        match c {
                            '!' => TokenType::Bang,
                            '=' => TokenType::Equal,
                            '<' => TokenType::Less,
                            '>' => TokenType::Greater,
                            _ => unreachable!(),
                        }
                    }
                }
                '"' => {
                    if let Err(e) = self.string() {
                        return Some(Err(e));
                    }
                    TokenType::r#String
                }
                _ => return Some(Err((self.line, "Unexpected character."))),
            };

            let text = self.advance();

            let token = Token {
                r#type: token,
                line: self.line,
                text,
            };

            Some(Ok(token))
        } else {
            None
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.char_indices().peekable(),
            from: 0,
            line: 1,
        }
    }

    fn advance(&mut self) -> &'a str {
        let head;
        if let Some((idx, _)) = self.iter.peek() {
            head = &self.source[self.from..*idx];
            self.from = *idx;
        } else {
            head = self.source;
        }

        head
    }

    fn next_char(&mut self) -> Option<char> {
        self.iter.next().map(|(_, c)| c)
    }

    fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, c)| *c)
    }

    fn trim_whitespace(&mut self) {
        let mut in_comment = false;

        loop {
            match self.peek_char() {
                Some('\n') => {
                    self.line += 1;
                    if in_comment {
                        self.iter.next();
                        break;
                    }
                }
                Some('/') => {
                    if let Some((_, '/')) = self.iter.peek() {
                        in_comment = true;
                    } else if !in_comment {
                        break;
                    }
                }
                Some(_) if in_comment => (),
                Some(c) if c.is_whitespace() => (),
                Some(_) | None => break,
            }

            self.iter.next();
        }

        self.advance();
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while let Some(c) = self.next_char() {
            match c {
                '\n' => self.line += 1,
                '"' => {
                    break;
                }
                _ => (),
            }

            if self.iter.peek().is_none() {
                return Err((self.line, "Unterminated string."));
            }
        }

        Ok(())
    }

    fn number(&mut self) {
        while let Some('0'..='9') = self.peek_char() {
            self.iter.next();
        }

        if let Some('.') = self.peek_char() {
            self.iter.next();
        }

        while let Some('0'..='9') = self.peek_char() {
            self.iter.next();
        }
    }

    fn identifier(&mut self, first_char: char) -> TokenType {
        if let Some(t) = match first_char {
            'a' => self.maybe_keyword("nd", TokenType::And),
            'c' => self.maybe_keyword("lass", TokenType::Class),
            'e' => self.maybe_keyword("lse", TokenType::Else),
            'i' => self.maybe_keyword("f", TokenType::If),
            'n' => self.maybe_keyword("il", TokenType::Nil),
            'o' => self.maybe_keyword("r", TokenType::Or),
            'p' => self.maybe_keyword("rint", TokenType::Print),
            'r' => self.maybe_keyword("eturn", TokenType::Return),
            's' => self.maybe_keyword("uper", TokenType::Super),
            'v' => self.maybe_keyword("ar", TokenType::Var),
            'w' => self.maybe_keyword("hile", TokenType::While),
            'f' => match self.peek_char() {
                Some('a') => self.maybe_keyword("alse", TokenType::False),
                Some('o') => self.maybe_keyword("or", TokenType::For),
                Some('u') => self.maybe_keyword("un", TokenType::Fun),
                _ => None,
            },
            't' => match self.peek_char() {
                Some('h') => self.maybe_keyword("his", TokenType::This),
                Some('r') => self.maybe_keyword("rue", TokenType::True),
                _ => None,
            },
            _ => None,
        } {
            return t;
        }

        while let Some('A'..='Z') | Some('a'..='z') | Some('_') | Some('0'..='9') = self.peek_char()
        {
            self.iter.next();
        }

        TokenType::Identifier
    }

    fn maybe_keyword(&mut self, rest: &'static str, success_type: TokenType) -> Option<TokenType> {
        for r_c in rest.chars() {
            match self.peek_char() {
                Some(c) if c == r_c => {
                    self.iter.next();
                }
                _ => return None,
            }
        }

        Some(success_type)
    }
}
