#![feature(plugin)]
#![plugin(rustlex)]

#[allow(plugin_as_library)] extern crate rustlex;

extern crate bit_set;

mod ast;
mod cfg;
mod lexer;
mod parser;
mod uniq;
mod var;

use std::env;
use std::fs::File;
use std::io::BufReader;
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

        let inp = BufReader::new(contents.as_bytes());
        let lexer = lexer::Lexer::new(inp);
        let mut tokens : Vec<lexer::Token> = lexer.collect();
        tokens.push(lexer::Token::EOS); // ugh

        println!("tokens: {:?}", tokens);
        let mut parser = parser::Parser::new(&tokens);
        let ast = parser.block();
        println!("ast: {:?}", ast);
    }
}
