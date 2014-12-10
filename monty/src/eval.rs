use std;
use syntax;
use pair_up;
use Value;
use Blob;
use linenoise::prompt;
use syntax::TokenTree;
use syntax::Token;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub fn print(val: &Value) -> String {
    match *val {
        Value::String(ref s) => format!("\"{}\"", s.escape_default()),
        Value::Char(c) => format!("#\\{}", c),
        Value::Bool(true) => "#t".into_string(),
        Value::Bool(false) => "#f".into_string(),
        Value::Number(n) => format!("{}", n),
        Value::Pair(ref a, ref b) => format!("({} . {})", print(&*a.borrow()), print(&*b.borrow())),
        Value::Base(_) => "#[base-procedure]".into_string(),
        Value::UserDefined(..) => "#[user-procedure]".into_string(),
        Value::Void => "#<void>".into_string(),
        Value::EmptyList => "()".into_string(),
        Value::Symbol(ref n) => format!("'{}", n),
        Value::Vector(ref v) => format!("#({})", v.iter().map(|v| print(&*v.borrow())).collect::<Vec<String>>().connect(" ")),
    }
}

#[deriving(Show, Clone)]
pub enum Formals {
    /// Corresponds to `(lambda foo ...)`
    List(String),
    /// Corresponds to `(lambda (a b c) ...)`
    AllGiven(Vec<String>),
    /// Corresponds to `(lambda (a b c . ds) ...)`
    SomeGiven(Vec<String>, String),
}

#[deriving(Clone)]
pub struct Environment {
    bindings: HashMap<String, Blob>,
    parent: Option<Box<Environment>>,
}

#[deriving(Show, Clone, Copy, PartialEq)]
pub enum Builtin {
    Eqv,
    Eq,
    Equal,
    Number,
    EqSign,
    Lt,
    Gt,
    Le,
    Ge,
    Plus,
    Star,
    Minus,
    Div,
    NumberString,
    StringNumber,
    Pair,
    Cons,
    Car,
    Cdr,
    Eval,
    Read,
    Write,
}

fn builtin_bindings() -> HashMap<String, Blob> {
    let mut hm = HashMap::with_capacity(22);
    {
        let help: |&str, Builtin| = |s, b| {
            hm.insert(s.into_string(), Rc::new(RefCell::new(Value::Base(b))));
        };

        help("eqv?", Builtin::Eqv);
        help("eq?", Builtin::Eq);
        help("equal?", Builtin::Equal);
        help("number?", Builtin::Number);
        help("=", Builtin::EqSign);
        help("<", Builtin::Lt);
        help(">", Builtin::Gt);
        help("<=", Builtin::Le);
        help(">=", Builtin::Ge);
        help("+", Builtin::Plus);
        help("*", Builtin::Star);
        help("-", Builtin::Minus);
        help("/", Builtin::Div);
        help("number->string", Builtin::NumberString);
        help("string->number", Builtin::StringNumber);
        help("pair?", Builtin::Pair);
        help("cons", Builtin::Cons);
        help("car", Builtin::Car);
        help("cdr", Builtin::Cdr);
        help("eval", Builtin::Eval);
        help("read", Builtin::Read);
        help("write", Builtin::Write);
    }

    hm
}

fn parse_formals(tt: &TokenTree) -> Result<Formals, String> {
    Ok(match *tt {
        TokenTree::Leaf(Token::Identifier(ref n)) => Formals::List(n.clone()),
        TokenTree::Delimited(ref tts) => {
            let mut givens = Vec::new();
            let mut want_rest = false;
            let mut iter = tts.iter();
            for tt in iter {
                match *tt {
                    TokenTree::Leaf(Token::Identifier(ref n)) => givens.push(n.clone()),
                    TokenTree::Leaf(Token::Dot) => { want_rest = true; break }
                    _ => return Err("invalid syntax in formal parameters".into_string()),
                }
            }
            if want_rest {
                match iter.next() {
                    Some(&TokenTree::Leaf(Token::Identifier(ref n))) => Formals::SomeGiven(givens, n.clone()),
                    _ => return Err("invalid syntax in formal parameters".into_string()),
                }
            } else {
                Formals::AllGiven(givens)
            }
        },
        _ => return Err("invalid syntax in formal parameters".into_string()),
    })
}

fn eqv(blob1: &Blob, blob2: &Blob) -> bool {
    match (&*blob1.borrow(), &*blob2.borrow()) {
        (&Value::Bool(true), &Value::Bool(true)) => true,
        (&Value::Bool(false), &Value::Bool(true)) => true,
        (&Value::Symbol(ref s1), &Value::Symbol(ref s2)) => s1 == s2,
        (&Value::Number(n1), &Value::Number(n2)) => n1 == n2,
        (&Value::Char(c1), &Value::Char(c2)) => c1 == c2,
        (&Value::EmptyList, &Value::EmptyList) => true,
        (&Value::Pair(..), &Value::Pair(..))
            | (&Value::String(..), &Value::String(..))
            | (&Value::Vector(..), &Value::Vector(..))
            | (&Value::UserDefined(..), &Value::UserDefined(..)) => &*blob1 as *const _ == &*blob2 as *const _,
            (&Value::Base(b1), &Value::Base(b2)) => b1 == b2 && &*blob1 as *const _ == &*blob2 as *const _,
        _ => false
    }
}

fn equal(blob1: &Blob, blob2: &Blob) -> bool {
    match (&*blob1.borrow(), &*blob2.borrow()) {
        (&Value::Pair(ref a1, ref a2), &Value::Pair(ref b1, ref b2)) =>
            eqv(a1, b1) && equal(a2, b2),
        (&Value::Vector(ref as_), &Value::Vector(ref bs)) =>
            as_.iter().zip(bs.iter()).all(|(a, b)| equal(a, b)),
        (&Value::String(ref a), &Value::String(ref b)) => a == b,
        _ => eqv(blob1, blob2)
    }
}

impl Environment {
    pub fn initial() -> Environment {
        Environment {
            bindings: builtin_bindings(),
            parent: None
        }
    }

    fn lookup(&self, val: &str) -> Option<Blob> {
        match self.bindings.get(val) {
            Some(val) => Some(val.clone()),
            None => match self.parent {
                Some(ref env) => env.lookup(val),
                None => None,
            }
        }
    }

    fn list_from_tts<'a, I>(&mut self, mut tts: I) -> Result<Blob, String>
    where I : Iterator<&'a TokenTree> {
        let mut vals = Vec::new();
        for tt in tts {
            vals.push(try!(self.eval(tt)));
        }
        Ok(Rc::new(RefCell::new(pair_up(vals.into_iter()))))
    }

    fn apply_builtin<'a, I>(&mut self, bin: Builtin, mut tts: I) -> Result<Blob, String>
    where I : Iterator<&'a TokenTree> {
        let mut vals = Vec::new();
        for tt in tts {
            vals.push(try!(self.eval(tt)))
        }
        Ok(Rc::new(RefCell::new(match bin {
            Builtin::Eqv => {
                if vals.len() != 2 {
                    return Err("eqv? not given 2 arguments".into_string())
                }
                Value::Bool(eqv(&vals[0], &vals[1]))
            },
            Builtin::Eq => {
                if vals.len() != 2 {
                    return Err("eq? not given two arguments".into_string())
                }
                Value::Bool(&*vals[0].borrow() as *const _ == &*vals[1].borrow() as *const _)
            },
            Builtin::Equal => {
                if vals.len() != 2 {
                    return Err("equal? not given 2 arguments".into_string())
                }
                Value::Bool(equal(&vals[0], &vals[1]))
            },
            Builtin::Number => {
                if vals.len() != 1 {
                    return Err("number? not given 1 argument".into_string())
                }
                Value::Bool(match &*vals[0].borrow() {
                    &Value::Number(_) => true,
                    _ => false
                })
            },
            Builtin::EqSign => {
                if vals.len() != 2 {
                    return Err("= not given 2 arguments".into_string())
                }
                Value::Bool(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a == b,
                    _ => false
                })
            },
            Builtin::Lt => {
                if vals.len() != 2 {
                    return Err("< not given 2 arguments".into_string())
                }
                Value::Bool(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a < b,
                    _ => false
                })
            },
            Builtin::Gt => {
                if vals.len() != 2 {
                    return Err("> not given 2 arguments".into_string())
                }
                Value::Bool(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a > b,
                    _ => false
                })
            },
            Builtin::Le => {
                if vals.len() != 2 {
                    return Err("<= not given 2 arguments".into_string())
                }
                Value::Bool(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a <= b,
                    _ => false
                })
            },
            Builtin::Ge => {
                if vals.len() != 2 {
                    return Err(">= not given 2 arguments".into_string())
                }
                Value::Bool(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a >= b,
                    _ => false
                })
            },
            Builtin::Plus => {
                if vals.len() != 2 {
                    return Err("+ not given 2 arguments".into_string())
                }
                Value::Number(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a + b,
                    _ => return Err("+ not given 2 numbers".into_string()),
                })
            },
            Builtin::Star => {
                if vals.len() != 2 {
                    return Err("* not given 2 arguments".into_string())
                }
                Value::Number(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a * b,
                    _ => return Err("* not given 2 numbers".into_string()),
                })
            },
            Builtin::Minus => {
                if vals.len() != 2 {
                    return Err("- not given 2 arguments".into_string())
                }
                Value::Number(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => a - b,
                    _ => return Err("- not given 2 numbers".into_string()),
                })
            },
            Builtin::Div => {
                if vals.len() != 2 {
                    return Err("/ not given 2 arguments".into_string())
                }
                Value::Number(match (&*vals[0].borrow(), &*vals[1].borrow()) {
                    (&Value::Number(a), &Value::Number(b)) => if b == 0 {
                        return Err("division by zero".into_string())
                    } else {
                        a / b
                    },
                    _ => return Err("/ not given 2 numbers".into_string()),
                })
            },
            Builtin::NumberString => {
                if vals.len() != 1 {
                    return Err("number->string not given 1 argument".into_string())
                }
                Value::String(match &*vals[0].borrow() {
                    &Value::Number(a) => a.to_string(),
                    _ => return Err("number->string not given a number".into_string())
                })
            },
            Builtin::StringNumber => {
                if vals.len() != 1 {
                    return Err("string->number not given 1 argument".into_string())
                }
                match &*vals[0].borrow() {
                    &Value::String(ref n) => match from_str(n.as_slice()) {
                        Some(n) => Value::Number(n),
                        None => Value::Bool(false),
                    },
                    _ => return Err("string->number not given a string".into_string())
                }
            },
            Builtin::Pair => {
                if vals.len() != 1 {
                    return Err("pair? not given 1 argument".into_string())
                }
                Value::Bool(match &*vals[0].borrow() {
                    &Value::Pair(..) => true,
                    _ => false
                })
            },
            Builtin::Cons => {
                if vals.len() != 2 {
                    return Err("cons not given 2 arguments".into_string())
                }
                Value::Pair(vals[0].clone(), vals[1].clone())
            },
            Builtin::Car => {
                if vals.len() != 1 {
                    return Err("car not given 1 argument".into_string())
                }
                match &*vals[0].borrow() {
                    &Value::Pair(ref a, _) => return Ok(a.clone()),
                    _ => return Err("car not given a pair".into_string())
                }
            },
            Builtin::Cdr => {
                if vals.len() != 1 {
                    return Err("cdr not given 1 argument".into_string())
                }
                match &*vals[0].borrow() {
                    &Value::Pair(_, ref b) => return Ok(b.clone()),
                    _ => return Err("cdr not given a pair".into_string())
                }
            },
            Builtin::Eval => {
                if vals.len() != 1 {
                    return Err("eval not given 1 argument".into_string())
                }
                match &*vals[0].borrow() {
                    &Value::String(ref n) => {
                        return self.eval(&match syntax::read(n.chars().peekable()) {
                            Ok(Some(tt)) => tt,
                            Ok(None) => return Ok(Rc::new(RefCell::new(Value::Void))),
                            Err(e) => return Err(e.into_string())
                        });
                    },
                    _p @ &Value::Pair(..) => {
                        return Err("evaluating a list is not yet implemented".into_string())
                    },
                    _ => return Err("eval not passed a list or string".into_string())
                }
            },
            Builtin::Read => {
                if vals.len() != 0 {
                    return Err("read given arguments".into_string())
                }
                match prompt("") {
                    None => Value::Void,
                    Some(cs) =>
                        match std::str::from_utf8(cs.as_bytes()) {
                            Some(s) => {
                                let mut iter = s.chars();
                                Value::from_tt(&match syntax::read(iter.by_ref().peekable()) {
                                    Ok(Some(tt)) => tt,
                                    Ok(_) => return Ok(Rc::new(RefCell::new(Value::Void))),
                                    Err(e) => return Err(e.into_string())
                                })
                            },
                            None => return Err("read: non-utf8 input".into_string())
                        }
                }
            },
            Builtin::Write => {
                if vals.len() != 1 {
                    return Err("write not given 1 argument".into_string())
                }
                print(&*vals[0].borrow());
                Value::Void
            }
        })))
    }

    fn apply(&mut self, tts: &[TokenTree]) -> Result<Blob, String> {
        let lam = match tts.get(0) {
            Some(&TokenTree::Leaf(Token::Identifier(ref n))) => match self.lookup(n.as_slice()) {
                Some(l) => l,
                None => return Err(format!("unbound identifier {}", n))
            },
            Some(&TokenTree::Leaf(ref t)) => return Err(format!("unexpected token in procedure position: {}", t)),
            Some(tts) => try!(self.eval(tts)),
            None => return Err("tried to apply with nothing?".into_string()),
        };
        Ok(match *lam.borrow() {
            Value::Base(bin) => try!(self.apply_builtin(bin, tts.iter().skip(1))),
            Value::UserDefined(ref formals, ref body, ref env) => {
                let mut new_env = Environment {
                    bindings: HashMap::new(),
                    parent: Some(box env.clone()),
                };
                let mut tts = tts.iter().skip(1);
                match *formals {
                    Formals::List(ref name) => {
                        new_env.bindings.insert(name.clone(), try!(self.list_from_tts(tts)));
                    },
                    Formals::AllGiven(ref names) => {
                        for (name, tt) in names.iter().zip(tts) {
                            new_env.bindings.insert(name.clone(), try!(self.eval(tt)));
                        }
                    },
                    Formals::SomeGiven(ref names, ref rest) =>  {
                        for (name, tt) in names.iter().zip(tts.by_ref()) {
                            new_env.bindings.insert(name.clone(), try!(self.eval(tt)));
                        }
                        new_env.bindings.insert(rest.clone(), try!(self.list_from_tts(tts)));
                    }
                }
                let mut val = None;
                for tt in body.iter() {
                    val = Some(try!(new_env.eval(tt)));
                }
                match val {
                    Some(v) => v,
                    None => return Err("no body in lambda".into_string())
                }
            },
            ref v => return Err(format!("tried to apply non-procedure {}", print(v))),
        })
    }

    pub fn eval(&mut self, tt: &TokenTree) -> Result<Blob, String> {
        Ok(match *tt {
            TokenTree::Leaf(ref tok) => Rc::new(RefCell::new(match *tok {
                Token::Number(n) => Value::Number(n),
                Token::True => Value::Bool(true),
                Token::False => Value::Bool(false),
                Token::Quote => panic!("quote unimplemented"),
                Token::Dot => panic!("dot unimplemented"),
                Token::Identifier(ref id) => {
                    match self.lookup(&**id) {
                        Some(val) => return Ok(val),
                        None => return Err(format!("unbound identifier {}", id)),
                    }
                },
                Token::String(ref n) => {
                    Value::String(n.clone())
                }
                ref t => panic!("unexpected token {} when evaluating", t)
            })),
            TokenTree::Quoted(ref tt) => {
                Rc::new(RefCell::new(Value::from_tt(&**tt)))
            },
            TokenTree::Delimited(ref tts) => {
                let name = match tts.get(0) {
                    Some(&TokenTree::Leaf(Token::Identifier(ref n))) => n,
                    Some(_) => return self.apply(tts.as_slice()),
                    None => return Err("tried to evaluate ()".into_string())
                };
                // is it one of the few syntactic forms we know about?
                match name.as_slice() {
                    "lambda" => {
                        let formals = try!(parse_formals(match tts.get(1) {
                            Some(tt) => tt,
                            None => return Err("no formal arguments in lambda".into_string()),
                        }));
                        let body = tts.iter().skip(2).map(|x| x.clone()).collect();
                        Rc::new(RefCell::new(Value::UserDefined(formals, body, self.clone())))
                    },
                    "if" => {
                        match *try!(self.eval(match tts.get(1) {
                            Some(tt) => tt,
                            None => return Err("no predicate in if".into_string()),
                        })).borrow() {
                            Value::Bool(false) => try!(self.eval(match tts.get(3) {
                                Some(tt) => tt,
                                None => return Err("no else in if".into_string())
                            })),
                            _ => try!(self.eval(match tts.get(2) {
                                Some(tt) => tt,
                                None => return Err("no consequent in if".into_string())
                            })),
                        }
                    },
                    "set!" => {
                        let sym = match tts.get(1) {
                            Some(&TokenTree::Leaf(Token::Identifier(ref n))) => n.clone(),
                            Some(_) => return Err("non-identifier used in set!".into_string()),
                            None => return Err("no identifier passed to set!".into_string()),
                        };
                        let val = try!(self.eval(match tts.get(2) {
                            Some(tt) => tt,
                            None => return Err("no expression passed to set!".into_string())
                        }));
                        match self.lookup(sym.as_slice()) {
                            Some(bl) => *bl.borrow_mut() = val.borrow().clone(),
                            None => return Err(format!("unbound identifier {}", sym))
                        }
                        Rc::new(RefCell::new(Value::Void))
                    },
                    "let" => {
                        let mut bindings = HashMap::new();
                        let vars = match tts.get(1) {
                            Some(&TokenTree::Delimited(ref vars)) => vars.clone(),
                            Some(_) => return Err("non-list passed as binding list".into_string()),
                            None => return Err("no bindings given to let".into_string()),
                        };
                        for tt in vars.iter() {
                            match (tt.get(0), tt.get(1)) {
                                (Some(&TokenTree::Leaf(Token::Identifier(ref name))), Some(val)) => {
                                    bindings.insert(name.clone(), try!(self.eval(val)));
                                },
                                _ => return Err("invalid let syntax".into_string())
                            }
                        }

                        let mut new_env = Environment {
                            bindings: bindings,
                            parent: Some(box self.clone())
                        };
                        let mut val = None;
                        for tt in tts.iter().skip(2) {
                            val = Some(new_env.eval(tt))
                        }
                        match val {
                            None => return Err("no expressions in let body".into_string()),
                            Some(v) => try!(v)
                        }
                    },
                    "letrec" => {
                        let mut bindings = HashMap::new();
                        let vars = match tts.get(1) {
                            Some(&TokenTree::Delimited(ref vars)) => vars.clone(),
                            Some(_) => return Err("non-list passed as binding list".into_string()),
                            None => return Err("no bindings given to let".into_string()),
                        };
                        for tt in vars.iter() {
                            match tt.get(0) {
                                Some(&TokenTree::Leaf(Token::Identifier(ref name))) => {
                                    bindings.insert(name.clone(), Rc::new(RefCell::new(Value::Void)));
                                },
                                _ => return Err("invalid let syntax".into_string())
                            }
                        }

                        let mut new_env = Environment {
                            bindings: bindings,
                            parent: Some(box self.clone())
                        };

                        for tt in vars.iter() {
                            match (tt.get(0), tt.get(1)) {
                                (Some(&TokenTree::Leaf(Token::Identifier(ref name))), Some(val)) => {
                                    let val = try!(new_env.eval(val));
                                    *new_env.bindings.get(name).unwrap().borrow_mut() = val.borrow().clone();
                                },
                                _ => return Err("invalid letrec syntax".into_string())
                            }
                        }


                        let mut val = None;
                        for tt in tts.iter().skip(2) {
                            val = Some(new_env.eval(tt))
                        }
                        match val {
                            None => return Err("no expressions in letrec body".into_string()),
                            Some(v) => { try!(v); },
                        }
                        Rc::new(RefCell::new(Value::Void))
                    },
                    "begin" => {
                        let mut val = None;
                        for tt in tts.iter().skip(1) {
                            val = Some(try!(self.eval(tt)));
                        }
                        match val {
                            None => return Err("no begin body".into_string()),
                            Some(v) => return Ok(v)
                        }
                    },
                    "define" => {
                        let sym = match tts.get(1) {
                            Some(&TokenTree::Leaf(Token::Identifier(ref n))) => n.clone(),
                            Some(_) => return Err("non-identifier used in define".into_string()),
                            None => return Err("no identifier passed to define".into_string()),
                        };
                        let val = try!(self.eval(match tts.get(2) {
                            Some(tt) => tt,
                            None => return Err("no expression passed to define".into_string())
                        }));
                        self.bindings.insert(sym.clone(), Rc::new(RefCell::new(Value::Void)));

                        match self.lookup(sym.as_slice()) {
                            Some(bl) => *bl.borrow_mut() = val.borrow().clone(),
                            None => return Err(format!("unbound identifier {}", sym))
                        }
                        Rc::new(RefCell::new(Value::Void))
                    },
                    _ => {
                        try!(self.apply(tts.as_slice()))
                    }
                }
            }
        })
    }
}
