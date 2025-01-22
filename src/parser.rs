use std::borrow::Cow;

use crate::lexer::{Lexer, Token};

type ParseResult<'a> = Result<TokenTree<'a>, String>;

pub struct Parser<'src> {
    // source: &'src str,
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            // source,
            lexer: Lexer::new(source),
        }
    }

    pub fn parse_class(&mut self) -> ParseResult<'src> {
        self.lexer
            .expect(Token::Class, "Expected class at start of file")?;

        let name = match self.lexer.next().unwrap() {
            Ok(t) => {
                if let Token::Ident(label) = t {
                    label
                } else {
                    return Err(format!("Expected ident | Received {:?}", t));
                }
            }
            Err(e) => return Err(e),
        };
        let (mut vars, mut subroutines) = (vec![], vec![]);

        self.lexer.expect(Token::LCurly, "missing {")?;

        while let Some(Ok(tok)) = self.lexer.next() {
            match tok {
                Token::Static | Token::Field => {
                    let class_var = self.parse_class_var(&tok)?;
                    vars.push(class_var);
                }
                Token::Method | Token::Function | Token::Constructor => {
                    let subroutine = self.parse_subroutine(&tok)?;
                    subroutines.push(subroutine);
                }
                Token::Eof => break,
                _ => todo!(),
            };

            if self.lexer.peek() == Some(&Ok(Token::RCurly)) {
                break;
            }
        }

        self.lexer.expect(Token::RCurly, "missing }")?;

        Ok(TokenTree::Class {
            name,
            vars,
            subroutines,
        })
    }

    fn parse_class_var(
        &mut self,
        kind: &Token,
    ) -> Result<(ClassVarKind, Type<'src>, Vec<&'src str>), String> {
        let kind = match kind {
            Token::Field => ClassVarKind::Field,
            Token::Static => ClassVarKind::Static,
            _ => panic!("Expected field or static | Recieved {:?}", kind),
        };

        let ty = match self.lexer.next() {
            Some(tok) => Type::try_from(&tok?)?,
            None => return Err("Expected Type".into()),
        };

        let mut vars = vec![];
        while let Some(Ok(tok)) = self.lexer.next() {
            match tok {
                Token::Ident(label) => {
                    vars.push(label);
                }
                Token::Comma => continue,
                Token::SemiColon => break,
                _ => return Err(format!("Unexpect {:?}", tok)),
            }
        }

        Ok((kind, ty, vars))
    }

    fn parse_subroutine(&mut self, kind: &Token) -> Result<Subroutine<'src>, String> {
        let kind = match kind {
            Token::Method => SubroutineKind::Method,
            Token::Function => SubroutineKind::Function,
            Token::Constructor => SubroutineKind::Constructor,
            _ => panic!(
                "Expected method or function or constructor | Recieved {:?}",
                kind
            ),
        };

        let return_type = match self.lexer.next() {
            Some(tok) => Type::try_from(&tok?)?,
            None => return Err("Expected Type".into()),
        };

        let name = self.parse_ident()?;

        self.lexer.expect(Token::LParen, "Missing (")?;
        let mut params = vec![];
        while let Some(Ok(tok)) = self.lexer.next() {
            match tok {
                Token::Int | Token::Char | Token::Boolean | Token::Ident(_) => {
                    let ty = Type::try_from(&tok)?;
                    let param_name = match self.lexer.next().unwrap() {
                        Ok(t) => {
                            if let Token::Ident(label) = t {
                                label
                            } else {
                                return Err(format!("Expected ident | Received {:?}", t));
                            }
                        }
                        Err(e) => return Err(e),
                    };

                    params.push((ty, param_name));
                }
                Token::Comma => continue,
                Token::RParen => break,
                _ => return Err(format!("Unexpect {:?}", tok)),
            }
        }

        let body = self.parse_block()?;

        Ok(Subroutine {
            name,
            kind,
            params,
            body,
            return_type,
        })
    }

    fn parse_block(&mut self) -> Result<Vec<TokenTree<'src>>, String> {
        self.lexer.expect(Token::LCurly, "Missing {")?;
        let mut block = vec![];

        while let Some(Ok(tok)) = self.lexer.next() {
            let stmt = match tok {
                Token::Let => self.parse_let_stmt()?,
                Token::RCurly => break,
                _ => todo!(),
            };

            block.push(stmt);
        }

        Ok(block)
    }

    fn parse_let_stmt(&mut self) -> ParseResult<'src> {
        let var_name = self.parse_ident()?;
        self.lexer.expect(Token::Eq, "Expected =")?;
        let value = Box::new(self.parse_expr()?);
        self.lexer.expect(Token::SemiColon, "Expected ;")?;

        Ok(TokenTree::LetStmt { var_name, value })
    }

    fn parse_expr(&mut self) -> ParseResult<'src> {
        let mut lhs = if let Some(Ok(l_tok)) = self.lexer.next() {
            match l_tok {
                Token::Ident(label) => TokenTree::Atom(Atom::Ident(label)),
                Token::IntLiteral(n) => TokenTree::Atom(Atom::Int(n)),
                Token::CharLiteral(c) => TokenTree::Atom(Atom::Char(c)),
                Token::StrLiteral(s) => TokenTree::Atom(Atom::String(Cow::Borrowed(s))),
                Token::True => TokenTree::Atom(Atom::Bool(true)),
                Token::False => TokenTree::Atom(Atom::Bool(false)),
                Token::This => TokenTree::Atom(Atom::This),

                Token::Not | Token::Minus => {
                    let op = match l_tok {
                        Token::Not => Op::Not,
                        Token::Minus => Op::Minus,
                        _ => return Err(format!("Supports only - or ! | Received {:?}", l_tok)),
                    };
                    let expr = self.parse_expr()?;

                    TokenTree::UnaryExpr(op, Box::new(expr))
                }

                Token::LParen => {
                    let expr = self.parse_expr()?;
                    self.lexer.expect(Token::RParen, "Missing )")?;
                    TokenTree::ParenExpr(Box::new(expr))
                }

                token => return Err(format!("Unexpected {:?}", token)),
            }
        } else {
            return Err("Expected token".into());
        };

        while self.lexer.peek() != Some(&Ok(Token::SemiColon)) {
            match &self.lexer.peeked {
                Some(r_tok) => {
                    let r_tok = r_tok.as_ref()?;
                    match r_tok {
                        Token::Plus
                        | Token::Minus
                        | Token::Asterisk
                        | Token::FSlash
                        | Token::Eq
                        | Token::Gt
                        | Token::Lt
                        | Token::Amper
                        | Token::Bar => {
                            let op = Op::try_from(r_tok)?;
                            self.lexer.next();
                            let rhs = self.parse_expr()?;
                            lhs = TokenTree::BinaryExpr(Box::new(lhs), op, Box::new(rhs));
                        }

                        Token::LParen => {
                            self.lexer.next();
                            let mut args = vec![];
                            /* while self.lexer.peek() != Some(&Ok(Token::RParen)) {
                                let arg = self.parse_expr()?;
                                args.push(arg);
                            } */

                            while let Some(Ok(tok)) = self.lexer.next() {
                                match tok {
                                    Token::Comma => continue,
                                    Token::RParen => break,
                                    _ => {
                                        // fix this
                                        println!("In args : {:?}", tok);
                                        let arg = self.parse_expr()?;
                                        args.push(arg);
                                    }
                                }
                            }

                            lhs = TokenTree::Call {
                                callee: Box::new(lhs),
                                args,
                            }
                        }

                        Token::LSquare => {
                            self.lexer.next();
                            let index = self.parse_expr()?;
                            self.lexer.expect(Token::RSquare, "Missing ]")?;
                            lhs = TokenTree::ArrayIndex {
                                arr: Box::new(lhs),
                                index: Box::new(index),
                            }
                        }
                        _ => return Ok(lhs),
                    }
                }
                None => todo!(),
            }
        }

        Ok(lhs)
    }

    fn parse_ident(&mut self) -> Result<&'src str, String> {
        match self.lexer.next().unwrap() {
            Ok(t) => {
                if let Token::Ident(label) = t {
                    Ok(label)
                } else {
                    return Err(format!("Expected ident | Received {:?}", t));
                }
            }
            Err(e) => return Err(e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'src> {
    Int(u16),
    Char(char),
    String(Cow<'src, str>),
    Bool(bool),
    Ident(&'src str),
    This,
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine<'src> {
    name: &'src str,
    kind: SubroutineKind,
    params: Vec<(Type<'src>, &'src str)>,
    body: Vec<TokenTree<'src>>,
    return_type: Type<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'src> {
    Atom(Atom<'src>),

    Class {
        name: &'src str,
        vars: Vec<(ClassVarKind, Type<'src>, Vec<&'src str>)>,
        subroutines: Vec<Subroutine<'src>>,
    },

    VarDecl(Type<'src>, Vec<&'src str>),
    LetStmt {
        var_name: &'src str,
        value: Box<TokenTree<'src>>,
    },
    DoStmt(Box<TokenTree<'src>>),
    IfStmt {
        cond: Box<TokenTree<'src>>,
        true_branch: Vec<TokenTree<'src>>,
        false_branch: Option<Vec<TokenTree<'src>>>,
    },
    WhileStmt {
        cond: Box<TokenTree<'src>>,
        body: Vec<TokenTree<'src>>,
    },
    ReturnStmt(Option<Box<TokenTree<'src>>>),
    ArrayIndex {
        arr: Box<TokenTree<'src>>,
        index: Box<TokenTree<'src>>,
    },
    Call {
        callee: Box<TokenTree<'src>>,
        args: Vec<TokenTree<'src>>,
    },

    ParenExpr(Box<TokenTree<'src>>),
    UnaryExpr(Op, Box<TokenTree<'src>>),
    BinaryExpr(Box<TokenTree<'src>>, Op, Box<TokenTree<'src>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SubroutineKind {
    Constructor,
    Function,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassVarKind {
    Static,
    Field,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'src> {
    Int,
    Boolean,
    Char,
    Void,
    Class(&'src str),
}

impl<'a> TryFrom<&Token<'a>> for Type<'a> {
    type Error = String;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Char => Ok(Type::Char),
            Token::Int => Ok(Type::Int),
            Token::Boolean => Ok(Type::Boolean),
            Token::Void => Ok(Type::Void),
            Token::Ident(label) => Ok(Type::Class(label)),
            _ => Err(format!("{:?} is not a valid type", value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Not,
    LessThan,
    GreaterThan,
    Equals,
}

impl TryFrom<&Token<'_>> for Op {
    type Error = String;

    fn try_from(value: &Token<'_>) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(Op::Plus),
            Token::Minus => Ok(Op::Minus),
            Token::Asterisk => Ok(Op::Multiply),
            Token::FSlash => Ok(Op::Divide),
            Token::Eq => Ok(Op::Equals),
            Token::Gt => Ok(Op::GreaterThan),
            Token::Lt => Ok(Op::LessThan),
            Token::Amper => Ok(Op::And),
            Token::Bar => Ok(Op::Or),
            _ => Err(format!("{:?} is not a valid operator", value)),
        }
    }
}
