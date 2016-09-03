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
use std::time::Instant;

fn main() {
    if let Some(file) = env::args().nth(1) {
        println!("{:?}", file);

        let contents = {
            let mut file = File::open(file).unwrap();
            let mut contents : String = String::new();
            file.read_to_string(&mut contents).unwrap();
            contents
        };

        let before_lexer = Instant::now();
        let mut tokens : Vec<lexer::Tok> = lexer::tokenize(&contents).unwrap();
        tokens.push(lexer::Tok::EOS);
        let after_lexer = Instant::now();
        let lexer_time = after_lexer.duration_since(before_lexer);

        println!("tokens: {:?}", tokens);

        let before_parser = Instant::now();
        let parser = parser::Parser::new(&tokens);
        let defs = parser.parse();
        let after_parser = Instant::now();
        let parser_time = after_parser.duration_since(before_parser);

        let before_printer = Instant::now();
        for (var, cfg) in defs.iter() {
            println!("var: {:?}", var);
            let mut str = String::new().into_bytes();
            cfg.print(&mut str);
            println!("{}", unsafe { String::from_utf8_unchecked(str) });
        }
        let after_printer = Instant::now();
        let printer_time = after_printer.duration_since(before_printer);

        println!("Lexing took {} ms.", lexer_time.subsec_nanos() as f64 / 1000000f64);
        println!("Parsing took {} ms.", parser_time.subsec_nanos() as f64 / 1000000f64);
        println!("Printing took {} ms.", printer_time.subsec_nanos() as f64 / 1000000f64);
    }
}
