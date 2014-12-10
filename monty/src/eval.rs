use syntax::TokenTree;
use syntax::Token;
use std::collections::HashMap;

/// A Value is an actual value at runtime.
// roughly corresponds to a blob..

#[deriving(Show, Clone)]
pub enum Value {
    Bool(bool),
    Number(u64),
    Pair(Box<Value>, Box<Value>),
    /// A built-in function
    Base(Builtin),
    /// A user-defined function
    UserDefined(Formals, Vec<TokenTree>),
    /// The result of evaluating, for example, define, or set!
    Void,
}

pub fn print(val: &Value) -> String {
    use self::Value::*;
    match *val {
        Bool(true) => "#t".into_string(),
        Bool(false) => "#f".into_string(),
        Number(n) => format!("{}", n),
        Pair(ref a, ref b) => format!("({} . {})", print(&**a), print(&**b)),
        Base(_) => "#[base-procedure]".into_string(),
        UserDefined(..) => "#[user-procedure]".into_string(),
        Void => "#<void>".into_string(),
    }
}

#[deriving(Show, Clone)]
pub enum Formals {
    /// Corresponds to `(lambda foo ...)`
    List,
    /// Corresponds to `(lambda (a b c) ...)`
    AllGiven(Vec<String>),
    /// Corresponds to `(lambda (a b c . ds) ...)`
    SomeGiven(Vec<String>, String),
}

#[deriving(Show, Clone)]
pub struct Environment<'a> {
    bindings: HashMap<String, Value>,
    parent: Option<&'a mut &'a mut Environment>,
}

#[deriving(Show, Clone)]
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
}


fn builtin_bindings() -> HashMap<String, Value> {
    use self::Builtin::*;
    use self::Value::Base;

    let mut hm = HashMap::with_capacity(22);
    hm.insert("eqv?".into_string(), Base(Eqv));
    hm.insert("eq?".into_string(), Base(Eq));
    hm.insert("equal?".into_string(), Base(Equal));
    hm.insert("number?".into_string(), Base(Number));
    hm.insert("=".into_string(), Base(EqSign));
    hm.insert("<".into_string(), Base(Lt));
    hm.insert(">".into_string(), Base(Gt));
    hm.insert("<=".into_string(), Base(Le));
    hm.insert(">=".into_string(), Base(Ge));
    hm.insert("+".into_string(), Base(Plus));
    hm.insert("*".into_string(), Base(Star));
    hm.insert("-".into_string(), Base(Minus));
    hm.insert("/".into_string(), Base(Div));
    hm.insert("number->string".into_string(), Base(NumberString));
    hm.insert("string->number".into_string(), Base(StringNumber));
    hm.insert("pair?".into_string(), Base(Pair));
    hm.insert("cons".into_string(), Base(Cons));
    hm.insert("car".into_string(), Base(Car));
    hm.insert("cdr".into_string(), Base(Cdr));
    hm
}

impl Environment {
    pub fn initial() -> Environment<'static> {
        Environment {
            bindings: builtin_bindings(),
            parent: None
        }
    }

    fn lookup(&self, val: &str) -> Option<Value> {
        match self.bindings.get(val) {
            Some(val) => Some(val.clone()),
            None => match self.parent {
                Some(env) => env.lookup(val),
                None => None,
            }
        }
    }

    pub fn eval(&mut self, tt: &TokenTree) -> Result<Value, String> {
        Ok(match *tt {
            TokenTree::Leaf(ref tok) => match *tok {
                Token::Number(n) => Value::Number(n),
                Token::True => Value::Bool(true),
                Token::False => Value::Bool(false),
                Token::Quote => panic!("quote unimplemented"),
                Token::Dot => panic!("dot unimplemented"),
                Token::Identifier(ref id) => {
                    match self.lookup(&**id) {
                        Some(val) => val,
                        None => return Err(format!("unbound identifier {}", id)),
                    }
                },
                ref t => panic!("unexpected token {} when evaluating", t)
            },
            TokenTree::Delimited(ref tts) => {
                let name = match tts.get(0) {
                    Some(Token::Identifier(ref n)) => n,
                    Some(t) => return Err(format!("tried to apply `{}` which is not a procedure", t)),
                    None => return Err("tried to evaluate ()")
                };
                // is it one of the few syntactic forms we know about?
                match name {
                    "lambda" => {
                        let formals = parse_formals(match tts.get(1) {
                            Some(tt) => tt,
                            None => return Err("no formal arguments in lambda".into_string()),
                        });
                        let body = tts.iter().skip(2).map(|x| x.clone()).collect();
                        Value::UserDefined(formals, body)
                    },
                    "if" => {
                        match self.eval(match tts.get(1) {
                            Some(tt) => tt,
                            None => return Err("no predicate in if".into_string()),
                        }) {
                            Value::Bool(false) => self.eval(match tts.get(3) {
                                Some(tt) => tt,
                                None => return Err("no else in if")
                            }),
                            _ => self.eval(match tts.get(2) {
                                Some(tt) => tt,
                                None => return Err("no consequent in if")
                            }),
                        }
                    },
                    "set!" => {
                        let sym = match tts.get(1) {
                            Some(TokenTree::Leaf(Token::Identifier(n))) => n,
                            Some(_) => return Err("non-identifier used in set!".into_string()),
                            None => return Err("no identifier passed to set!".into_string()),
                        };
                        let val = self.eval(match tts.get(2) {
                            Some(tt) => tt,
                            None => return Err("no expression passed to set!".into_string())
                        });
                        self.bindings.insert(n, val);
                        Value::Void
                    },
                    "let" => {
                        let mut bindings = HashMap::new(),
                        let vars = match tts.get(1) {
                            Some(TokenTree::Delimited(vars)) => vars,
                            Some(_) => return Err("non-list passed as let vars".into_string()),
                            None => return Err("no bindings given to let".into_string()),
                        };
                        for tt in vars.iter() {
                            match (tt.get(0), tt.get(1)) {
                                (Some(&TokenTree::Leaf(Token::Identifier(ref name))), Some(val)) => {
                                    bindings.insert(name, self.eval(val))
                                },
                                _ => return Err("invalid let syntax".into_string())
                            }
                        }

                        let val = {
                            let mut new_env = Environment {
                                bindings: bindings,
                                parent: Some(&mut self)
                            };
                            let mut val = None;
                            for tt in tts.iter().skip(2) {
                                val = Some(new_env.eval(tt))
                            }
                            val
                        };
                    },
                    "letrec" => {

                    },
                    "begin" => {

                    },
                    name => {

                    }
                }
            }
        })
    }
}
