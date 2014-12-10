//! Simple reader/writer for Monty
//!
//! Doesn't record location or do any error recovery, and doesn't yet implement any sort of macro
//! system.

use std;
use std::fmt;

/// A TokenTree is roughly a value in the "syntax world", they are the things that macros
/// manipulate, that get fed into eval, and that can be printed.
#[deriving(Show, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenTree {
    Leaf(Token),
    Delimited(Vec<TokenTree>),
}

impl TokenTree {
    fn is_leaf(&self) -> bool {
        match self {
            &TokenTree::Leaf(_) => true,
            _ => false
        }
    }

    fn format(&self, f: &mut fmt::Formatter, depth: u32) -> fmt::Result {
        fn indent(f: &mut fmt::Formatter, depth: u32) -> fmt::Result {
            for _ in range(0, depth) {
                try!(write!(f, " "))
            }
            Ok(())
        }

        match self {
            &TokenTree::Leaf(ref tok) => {
                try!(indent(f, depth));
                writeln!(f, "| {:p}", *tok)
            }
            &TokenTree::Delimited(ref inner) => {
                for tt in inner.iter() {
                    try!(tt.format(f, depth + if tt.is_leaf() { 0 } else { 4 }));
                }
                Ok(())
            }
        }
    }
}

impl fmt::Pointer for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, 0)
    }
}

#[deriving(Show, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    Identifier(String),
    True,
    False,
    Number(u64),
    LParen,
    RParen,
    Quote,
    Dot,
}

impl fmt::Pointer for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Token::Identifier(ref name) => write!(f, "{}", name),
            &Token::True => write!(f, "#t"),
            &Token::False => write!(f, "#f"),
            &Token::Number(num) => write!(f, "{}", num),
            &Token::LParen => write!(f, "("),
            &Token::RParen => write!(f, ")"),
            &Token::Quote => write!(f, "'"),
            &Token::Dot => write!(f, "."),
        }
    }
}

pub fn read<R>(input: std::iter::Peekable<char, R>) -> Result<Option<TokenTree>, &'static str>
where R: Iterator<char> {
    let mut lexer = Lexer::new(input).peekable();
    read_inner(&mut lexer)
}

fn read_inner<R>(lexer: &mut std::iter::Peekable<Token, R>) -> Result<Option<TokenTree>, &'static str>
where R: Iterator<Token> {
    let mut inner = Vec::new();
    match lexer.next() {
        Some(Token::LParen) => {
            while lexer.peek().map(|tok| tok != &Token::RParen).unwrap_or(false) {
                match read_inner(lexer) {
                    Ok(Some(tt)) => inner.push(tt),
                    Ok(None) => return Err("unmatched ("),
                    Err(e) => return Err(e),
                }
            }
            if lexer.next() != Some(Token::RParen) {
                return Err("unmatched (")
            }
            Ok(Some(TokenTree::Delimited(inner)))
        },
        Some(Token::RParen) => Err("extra )"),
        Some(tok) => Ok(Some(TokenTree::Leaf(tok))),
        None => Ok(None),
    }
}

pub fn write(input: &TokenTree) -> String {
    let mut s = String::new();
    write_inner(input, &mut s);
    s
}

fn write_inner(input: &TokenTree, output: &mut String) {
    match input {
        &TokenTree::Leaf(ref tok) => output.extend(format!("{:p}", *tok).chars()),
        &TokenTree::Delimited(ref inner) => {
            output.push('(');
            for tt in inner.iter() { output.push(' '); write_inner(tt, output); output.push(' '); }
            output.push(')');
        }
    }
}

struct Lexer<R> {
    pub reader: std::iter::Peekable<char, R>,
    pub done: bool
}

impl<R: Iterator<char>> Lexer<R> {
    fn new(iter: std::iter::Peekable<char, R>) -> Lexer<R> {
        Lexer {
            reader: iter,
            done: false
        }
    }

    fn lex_ident_into(&mut self, dest: &mut String) {
        loop {
            let c = *self.reader.peek().unwrap();
            if is_ident_continue(c) {
                let c = self.reader.next().unwrap();
                dest.push(c);
            } else {
                break
            }
        }
    }
}

fn is_ident_start(c: char) -> bool {
    match c {
        'a' ... 'z' | 'A' ... 'Z' |
        '_' | '!' | '$' | '%' | '&' |
        '*' | '/' | ':' | '<' | '=' |
        '>' | '?' | '^' | '~' => true,
        _ => false
    }
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || match c {
        '-' | '+' | '.' | '@' => true,
        _ => false
    }
}

impl<R: Iterator<char>> Iterator<Token> for Lexer<R> {
    fn next(&mut self) -> Option<Token> {
        loop {
            let c = match self.reader.next() {
                Some(c) => c,
                None => break
            };
            match c {
                '(' => return Some(Token::LParen),
                ')' => return Some(Token::RParen),
                '\'' => return Some(Token::Quote),
                ';' => {
                    let c = self.reader.by_ref().take_while(|&c| c != '\n' && c != '\r').last();
                    if let Some('\r') = c {
                        if self.reader.next().unwrap() != '\n' {
                            println!("CR not found followed by LF!");
                        }
                    }
                    continue
                },
                '#' => match self.reader.next() {
                    Some('t') => return Some(Token::True),
                    Some('f') => return Some(Token::False),
                    wat => println!("Invalid character `{}` following `#`,
                        expected `t` or `f`", wat)
                },
                c @ '0'...'9' => {
                    let mut lit = String::new();
                    lit.push(c);
                    loop {
                        if self.reader.peek().unwrap().is_digit(10) {
                            let c = self.reader.next().unwrap();
                            lit.push(c);
                        } else {
                            break;
                        }
                    }
                    return Some(Token::Number(from_str(lit.as_slice())
                                       .expect("Lexer accepted invalid numeric literal!")));
                },
                c if is_ident_start(c) => {
                    let mut ident = String::with_capacity(16);
                    ident.push(c);
                    self.lex_ident_into(&mut ident);
                    return Some(Token::Identifier(ident));
                },
                // "peculiar identifier"s
                c @ '+' | c @ '-' => return Some(Token::Identifier(format!("{}", c))),
                '.' => {
                    match self.reader.peek() {
                        Some(&'.') => {
                            self.reader.next();
                            match self.reader.peek() {
                                Some(&'.') => {
                                    self.reader.next();
                                    if self.reader.peek().map(|&c| is_ident_continue(c)).unwrap_or(false) {
                                        let mut ident = "...".into_string();
                                        self.lex_ident_into(&mut ident);
                                        return Some(Token::Identifier(ident));
                                    } else {
                                        return Some(Token::Identifier("...".into_string()))
                                    }
                                },
                                Some(&c) if is_ident_continue(c) => {
                                    let mut ident = "..".into_string();
                                    ident.push(c);
                                    self.lex_ident_into(&mut ident);
                                    return Some(Token::Identifier(ident));
                                },
                                Some(_) | None => {
                                    return Some(Token::Identifier("..".into_string()));
                                },
                            }
                        },
                        Some(&c) if is_ident_continue(c) => {
                            let mut ident = ".".into_string();
                            ident.push(c);
                            self.lex_ident_into(&mut ident);
                            return Some(Token::Identifier(ident));
                        },
                        Some(_) | None => {
                            return Some(Token::Dot)
                        }
                    }
                },
                '\0' | ' ' | '\t' | '\n' => continue,
                '\r' => match self.reader.next() {
                    Some('\n') => { }
                    _ => println!("CR not found followed by LF!")
                },
                wat => println!("Found unexpected character when lexing: `{}`", wat),
            }
        }
        None
    }
}
