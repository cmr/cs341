#![feature(globs)]

extern crate linenoise;

mod syntax;
mod eval;

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
                                Ok(val) => println!("{}", eval::print(&val)),
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
