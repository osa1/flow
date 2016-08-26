#![feature(test)]

extern crate bit_set;
extern crate test;

mod ast;
mod cfg;
mod lexer;
mod parser;
mod uniq;
mod var;

#[cfg(test)]
mod test_utils;

use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    if let Some(file) = env::args().nth(1) {
        println!("{:?}", file);

        let contents = {
            let mut file = File::open(file).unwrap();
            let mut contents : String = String::new();
            file.read_to_string(&mut contents);
            contents
        };

        let tokens : Result<Vec<lexer::Tok>, lexer::LexerError> = lexer::tokenize(&contents);
        // println!("tokens: {:?}", tokens);
        // for tok in tokens.as_ref().unwrap().iter() {
        //     println!("{:?}\n", tok);
        // }
        println!("total {} tokens", tokens.as_ref().unwrap().len());
        // let mut parser = parser::Parser::new(tokens.as_ref().unwrap());
        // let ast = parser.block();
        // println!("ast: {:?}", ast);
    }
}
