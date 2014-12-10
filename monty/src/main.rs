extern crate linenoise;

use std::cell::RefCell;
use std::rc::Rc;
use syntax::{TokenTree, Token};
use eval::{Builtin, Environment, Formals};

mod syntax;
mod eval;

#[deriving(Clone)]
pub enum Value {
    Bool(bool),
    String(String),
    Symbol(String),
    Char(char),
    Number(i64),
    Pair(Blob, Blob),
    Vector(Vec<Blob>),
    /// A built-in function
    Base(Builtin),
    /// A user-defined function
    UserDefined(Formals, Vec<TokenTree>, Environment),
    /// The result of evaluating, for example, define, or set!
    Void,
    EmptyList,
}

fn pair_up<I>(mut vals: I) -> Value
where I : Iterator<Blob> {
    match vals.next() {
        Some(v) => Value::Pair(v, Rc::new(RefCell::new(pair_up(vals)))),
        None => Value::EmptyList
    }
}

impl Value {
    /// Convert a TokenTree to a Blob, not doing any evaluation, merely ouputing lists of atoms.
    pub fn from_tt(tt: &TokenTree) -> Value {
        match *tt {
            TokenTree::Leaf(ref tok) => match *tok {
                Token::Number(n) => Value::Number(n),
                Token::True => Value::Bool(true),
                Token::False => Value::Bool(false),
                Token::Quote => panic!("single quote token should never appear"),
                Token::Dot => panic!("dot unimplemented"),
                Token::Identifier(ref id) => Value::Symbol(id.clone()),
                Token::String(ref n) => {
                    Value::String(n.clone())
                }
                ref t => panic!("unexpected token {} when evaluating", t)
            },
            TokenTree::Delimited(ref tts) =>
                    pair_up(tts.iter().map(|tt| Rc::new(RefCell::new(Value::from_tt(tt))))),
            TokenTree::Quoted(ref tt) => Value::from_tt(&**tt)
        }
    }
}

pub type Blob = Rc<RefCell<Value>>;

fn main() {
    let mut repl_env = eval::Environment::initial();

    loop {
        match linenoise::prompt("Monty! -> ") {
            Some(input) => {
                let mut iter = std::str::from_utf8(input.as_bytes()).unwrap().chars();
                loop {
                    match syntax::read(iter.by_ref().peekable()) {
                        Ok(Some(tt)) => {
                            match repl_env.eval(&tt) {
                                Ok(val) => println!("{}", eval::print(&*val.borrow())),
                                Err(e) => println!("eval error: {}", e),
                            }
                        }
                        Ok(None) => break,
                        Err(e) => {
                            println!("read error: {}", e);
                            break
                        }
                    }
                }
            }
            None => { break }
        }
    }
}
