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

        let mut tokens : Vec<lexer::Tok> = lexer::tokenize(&contents).unwrap();
        println!("tokens: {:?}", tokens);
        // for tok in tokens.as_ref().unwrap().iter() {
        //     println!("{:?}\n", tok);
        // }
        println!("total {} tokens", tokens.len());
        tokens.push(lexer::Tok::EOS);
        let mut parser = parser::Parser::new(&tokens);
        let ast = parser.block();
        println!("ast: {:?}", ast);
    }
}
