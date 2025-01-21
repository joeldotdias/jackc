pub struct Lexer<'src> {
    source: &'src str,
    pos: usize,
    peeked: Option<Result<Token<'src>, String>>,
}

#[derive(Debug, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    IntLiteral(u16),
    CharLiteral(char),
    StrLiteral(&'src str),

    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    True,
    False,
    Void,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,

    LSquare,
    RSquare,
    LParen,
    RParen,
    LCurly,
    RCurly,
    Dot,
    Comma,
    SemiColon,
    Plus,
    Minus,
    Asterisk,
    FSlash,
    Amper,
    Bar,
    Lt,
    Gt,
    Eq,
    Not,

    Eof,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            pos: 0,
            peeked: None,
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'src>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'src>, String> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(format!("Received {:?} | {}", token, unexpected)),
            Some(Err(e)) => Err(e),
            None => Err("Reached EOF".into()),
        }
    }

    pub fn expect(&mut self, expected: Token, unexpected: &str) -> Result<Token<'src>, String> {
        self.expect_where(|next| *next == expected, unexpected)
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'src>, String>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }
        enum Started {
            Slash,
            String,
            Number,
            Ident,
            // IfEqualElse(Token<'s>, Token<'s>),
        }

        loop {
            let mut chars = self.source.chars();
            let c = chars.next()?;
            let _c_at = self.pos; // keep this here in case i need to do spanned tokens
            let c_onwards = self.source;
            self.source = chars.as_str();
            self.pos += c.len_utf8();

            let just = |tok: Token<'src>| Some(Ok(tok));

            let started = match c {
                '(' => return just(Token::LParen),
                ')' => return just(Token::RParen),
                '[' => return just(Token::LSquare),
                ']' => return just(Token::RSquare),
                '{' => return just(Token::LCurly),
                '}' => return just(Token::RCurly),
                '.' => return just(Token::Dot),
                ',' => return just(Token::Comma),
                ';' => return just(Token::SemiColon),
                '+' => return just(Token::Plus),
                '-' => return just(Token::Minus),
                '*' => return just(Token::Asterisk),
                '&' => return just(Token::Amper),
                '|' => return just(Token::Bar),
                '<' => return just(Token::Lt),
                '>' => return just(Token::Gt),
                '=' => return just(Token::Eq),
                '~' => return just(Token::Not),
                '\0' => return just(Token::Eof),
                '"' => Started::String,
                '/' => Started::Slash,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' => Started::Ident,
                c if c.is_whitespace() => continue,
                c => return Some(Err(format!("Illegal {}", c))),
            };

            break match started {
                Started::String => {
                    if let Some(end) = self.source.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.pos += end + 1;
                        self.source = &self.source[end + 1..];
                        Some(Ok(Token::StrLiteral(literal)))
                    } else {
                        let err = "String termination error";
                        self.pos += self.source.len();
                        self.source = &self.source[self.source.len()..];
                        return Some(Err(err.into()));
                    }
                }

                Started::Slash => {
                    if self.source.starts_with('/') {
                        let line_end = self.source.find('\n').unwrap_or_else(|| self.source.len());
                        self.pos += line_end;
                        self.source = &self.source[line_end..];
                        continue;
                    } else {
                        Some(Ok(Token::FSlash))
                    }
                }

                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.pos += extra_bytes;
                    self.source = &self.source[extra_bytes..];

                    let keyword_or_ident = match literal {
                        "class" => Token::Class,
                        "constructor" => Token::Constructor,
                        "function" => Token::Function,
                        "method" => Token::Method,
                        "field" => Token::Field,
                        "static" => Token::Static,
                        "var" => Token::Var,
                        "int" => Token::Int,
                        "char" => Token::Char,
                        "boolean" => Token::Boolean,
                        "true" => Token::True,
                        "false" => Token::False,
                        "void" => Token::Void,
                        "null" => Token::Null,
                        "this" => Token::This,
                        "let" => Token::Let,
                        "do" => Token::Do,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "return" => Token::Return,
                        _ => Token::Ident(literal),
                    };

                    Some(Ok(keyword_or_ident))
                }

                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_digit];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.pos += extra_bytes;
                    self.source = &self.source[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => return Some(Err(format!("Couldn't parse number {}", e))),
                    };

                    Some(Ok(Token::IntLiteral(n)))
                }
            };
        }
    }
}
