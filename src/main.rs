#![feature(test)]

extern crate bit_set;
extern crate test;

mod ast;
mod cfg;
mod lexer;
mod parser;
mod uniq;
mod utils;
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
            file.read_to_string(&mut contents).unwrap();
            contents
        };

        let mut tokens : Vec<lexer::Tok> = lexer::tokenize(&contents).unwrap();
        tokens.push(lexer::Tok::EOS);
        let parser = parser::Parser::new(&tokens);
        let defs = parser.parse();
        for (var, cfg) in defs.iter() {
            println!("var: {:?}", var);
            let mut str = String::new().into_bytes();
            cfg.print(&mut str);
            println!("{}", unsafe { String::from_utf8_unchecked(str) });
        }
    }
}
