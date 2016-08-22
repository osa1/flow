#![feature(plugin)]
#![plugin(rustlex)]

#[allow(plugin_as_library)] extern crate rustlex;

extern crate bit_set;
extern crate lalrpop_util;

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
        let tokens : Vec<lexer::Token> = lexer.collect();
        println!("tokens: {:?}", tokens);
        let mb_ast : Result<Vec<Box<ast::Stmt>>, lalrpop_util::ParseError<(),lexer::Token,()>> =
            parser::parse_StmtList(tokens);
        println!("ast: {:?}", mb_ast);
    }
}
