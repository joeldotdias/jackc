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

            println!("{:?}", tok);
        }

        self.lexer.expect(Token::RCurly, "missing }")?;

        Ok(TokenTree::Class {
            name,
            vars,
            subroutines,
        })
    }

    pub fn parse_class_var(
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
            println!("Parsing class, vars {:?}", tok);
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

    pub fn parse_subroutine(&mut self, kind: &Token) -> Result<Subroutine<'src>, String> {
        let kind = match kind {
            Token::Method => SubroutineKind::Method,
            Token::Function => SubroutineKind::Function,
            Token::Constructor => SubroutineKind::Constructor,
            _ => panic!(
                "Expected method or function or constructor | Recieved {:?}",
                kind
            ),
        };

        println!("Kind {:?}", kind);

        let return_type = match self.lexer.next() {
            Some(tok) => Type::try_from(&tok?)?,
            None => return Err("Expected Type".into()),
        };
        println!("Type {:?}", return_type);

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
        println!("Name {}", name);

        self.lexer.expect(Token::LParen, "Missing (")?;
        let mut params = vec![];
        while let Some(Ok(tok)) = self.lexer.next() {
            println!("Parsing params {:?}", tok);
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

    pub fn parse_block(&mut self) -> Result<Vec<TokenTree<'src>>, String> {
        self.lexer.expect(Token::LCurly, "Missing {")?;
        let mut block = vec![];

        while let Some(Ok(lhs)) = self.lexer.next() {
            let stmt = match lhs {
                Token::Let => self.parse_let_stmt()?,
                Token::RCurly => break,
                _ => todo!(),
            };

            block.push(stmt);
        }

        Ok(block)
    }

    fn parse_let_stmt(&mut self) -> ParseResult<'src> {
        todo!()
    }

    /*
    pub fn parse_expr_within(&mut self, min_binding_pow: u8) -> Result<TokenTree<'src>, String> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Null)),
            Some(Err(e)) => return Err(e),
        };

        let mut lhs = match lhs {
            Token::StrLiteral(s) => TokenTree::Atom(Atom::String(Cow::Borrowed(s))),
            Token::IntLiteral(n) => TokenTree::Atom(Atom::Int(n)),
            Token::True => TokenTree::Atom(Atom::Bool(true)),
            Token::False => TokenTree::Atom(Atom::Bool(false)),
            Token::Null => TokenTree::Atom(Atom::Null),
            Token::Ident(label) => TokenTree::Atom(Atom::Ident(label)),
            Token::This => TokenTree::Atom(Atom::This),

            Token::LParen => {
                let lhs = self.parse_expr_within(0)?;
                self.lexer
                    .expect(Token::RParen, "Unexpected end to parentheses enclosed expr")?;
                // TokenTree::C

                todo!()
            }

            _ => todo!(),
        };

        todo!()
    } */
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'src> {
    String(Cow<'src, str>),
    Int(u16),
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

    // Subroutine(Subroutine<'src>),
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
