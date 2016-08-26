use ast::*;
use lexer::Tok;


pub struct Parser<'a> {
    ts: &'a [Tok],
    pos: usize,
}

/// Can the given token follow a statement? Useful when terminating statement
/// lists etc.
fn stat_follow(tok : &Tok) -> bool {
    match tok {
        &Tok::Else | &Tok::ElseIf | &Tok::End | &Tok::Until | &Tok::EOS => true,
        _ => false,
    }
}

impl<'a> Parser<'a> {
    pub fn new(ts: &'a [Tok]) -> Parser<'a> {
        Parser {
            ts: ts,
            pos: 0,
        }
    }

    /// Tok under cursor.
    fn cur_tok(&self) -> Tok {
        self.ts[self.pos].clone()
    }

    /// Reference to token under cursor.
    fn cur_tok_(&self) -> &Tok {
        &self.ts[self.pos]
    }

    /// Skip the given token or fail if it's not found.
    fn expect_tok(&mut self, tok : Tok) {
        if self.ts[self.pos] != tok {
            panic!("Unexpected token. Expected {:?}, found {:?}", tok, self.cur_tok_());
        }
        self.skip();
    }

    fn skip(&mut self) {
        self.pos += 1;
    }
}


////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    fn stat(&mut self) -> Box<Stmt> {
        match self.cur_tok_() {
            &Tok::Semic => { self.skip(); self.stat() },
            &Tok::If => { self.skip(); self.ifstat() },
            &Tok::While => { self.skip(); self.whilestat() },
            &Tok::Do => { self.skip(); self.dostat() },
            &Tok::For => { self.skip(); self.forstat() },
            &Tok::Repeat => { self.skip(); self.repeatstat() },
            &Tok::Function => { self.skip(); self.funcstat() },
            &Tok::Local => {
                self.skip(); // skip "local"
                match self.cur_tok_() {
                    &Tok::Function => { self.skip(); self.localfuncstat() },
                    _ => self.localstat(),
                }
            },
            &Tok::DColon => { self.skip(); self.labelstat() },
            &Tok::Return => { self.skip(); self.returnstat() },
            &Tok::Break => { self.skip(); self.breakstat() },
            &Tok::Goto => { self.skip(); self.gotostat() },
            _ => self.exprstat(),
        }
    }

    fn ifstat(&mut self) -> Box<Stmt> {
        let mut conds = vec![];
        // parse initial condition
        conds.push(self.cond_then());
        // parse elseif blocks
        while self.cur_tok_() == &Tok::ElseIf {
            self.skip(); // skip elseif
            conds.push(self.cond_then());
        }
        // parse optional else block
        let else_ = if self.cur_tok_() == &Tok::Else {
            self.skip(); // skip else
            Some(self.block())
        } else {
            None
        };

        self.expect_tok(Tok::End);

        Box::new(Stmt::If {
            conds: conds,
            else_: else_,
        })
    }

    // <condition> then <block>
    fn cond_then(&mut self) -> (Box<Exp>, Block) {
        let cond = self.exp();
        self.expect_tok(Tok::Then);
        let block = self.block();
        (cond, block)
    }

    // <condition> do <block> end
    fn whilestat(&mut self) -> Box<Stmt> {
        let cond = self.exp();
        self.expect_tok(Tok::Do);
        let block = self.block();
        self.expect_tok(Tok::End);
        Box::new(Stmt::While(cond, block))
    }

    // <block> end
    fn dostat(&mut self) -> Box<Stmt> {
        let block = self.block();
        self.expect_tok(Tok::End);
        Box::new(Stmt::Do(block))
    }

    // <fornum | forlist> end
    fn forstat(&mut self) -> Box<Stmt> {
        let var1 = self.name();
        match self.cur_tok_() {
            &Tok::Assign => { self.skip(); self.fornum(var1) },
            &Tok::Comma | &Tok::In => self.forlist(var1),
            _ => panic!("Syntax error in for statement"),
        }
    }

    // A single variable
    fn name(&mut self) -> Id {
        match self.cur_tok() {
            Tok::Ident(s) => { self.skip(); s },
            tok => panic!("name: unexpected token {:?}", tok),
        }
    }

    // exp1, exp2 [,exp3] <forbody>
    fn fornum(&mut self, var : Id) -> Box<Stmt> {
        let start = self.exp();
        self.expect_tok(Tok::Comma);
        let end = self.exp();
        let step = {
            if self.cur_tok_() == &Tok::Comma {
                self.skip();
                Some(self.exp())
            } else {
                None
            }
        };

        self.expect_tok(Tok::Do);
        let body = self.block();
        self.expect_tok(Tok::End);

        Box::new(Stmt::ForRange {
            var: var,
            start: start,
            end: end,
            step: step,
            body: body,
        })
    }

    // <name> {,<name>} in <explist> <forbody>
    fn forlist(&mut self, var1 : Id) -> Box<Stmt> {
        let mut vars = vec![var1];
        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // skip ,
            vars.push(self.name());
        }

        println!("parsed names: {:?}", vars);

        self.expect_tok(Tok::In);

        let mut exps = vec![self.exp()];
        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // skip ,
            exps.push(self.exp());
        }

        let body = self.forbody();

        Box::new(Stmt::ForIn {
            vars: vars,
            exps: exps,
            body: body,
        })
    }

    fn forbody(&mut self) -> Block {
        self.expect_tok(Tok::Do);
        let block = self.block();
        self.expect_tok(Tok::End);
        block
    }

    fn repeatstat(&mut self) -> Box<Stmt> {
        let block = self.block();
        self.expect_tok(Tok::Until);
        let cond = self.exp();
        Box::new(Stmt::Repeat(block, cond))
    }

    fn funcstat(&mut self) -> Box<Stmt> {
        let (n, mut sels, mname) = self.funcname();
        let (mut args, vararg, body) = self.fundef();
        match mname {
            Some(mname) => {
                args.insert(0, "self".to_owned());
                sels.push(mname);
            },
            None => {}
        }

        let lhs =
            sels.into_iter().fold(
                Var::Var(n),
                |lhs, sel| Var::Select(Box::new(Exp::Var(lhs)),
                                       Box::new(Exp::Var(Var::Var(sel)))));

        let rhs =
            Box::new(Exp::FunDef {
                args: args,
                vararg: vararg,
                body: body,
            });

        Box::new(Stmt::Assign {
            lhss: vec![lhs],
            rhss: vec![rhs],
            is_local: false,
        })
    }

    fn funcname(&mut self) -> (Id, Vec<Id>, Option<Id>) {
        let n1 = self.name();
        let mut fs = vec![];
        while self.cur_tok_() == &Tok::Dot {
            self.skip(); // consume .
            fs.push(self.name());
        }
        let mname = {
            if self.cur_tok_() == &Tok::Colon {
                self.skip(); // consume :
                Some(self.name())
            } else {
                None
            }
        };
        (n1, fs, mname)
    }

    // <name> <fundef>
    fn localfuncstat(&mut self) -> Box<Stmt> {
        let fname = self.name();
        let (args, vararg, body) = self.fundef();

        Box::new(Stmt::Assign {
            lhss: vec![Var::Var(fname)],
            rhss: vec![Box::new(Exp::FunDef {
                args:args,
                vararg: vararg,
                body: body,
            })],
            is_local: true,
        })
    }

    // <namelist> [= <explist>]
    fn localstat(&mut self) -> Box<Stmt> {
        // there should be at least one name
        let mut lhss = vec![Var::Var(self.name())];
        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // consume ,
            lhss.push(Var::Var(self.name()));
        }

        let mut rhss = vec![];
        if self.cur_tok_() == &Tok::Assign {
            self.skip(); // consume =
            rhss.push(self.exp());
            while self.cur_tok_() == &Tok::Comma {
                self.skip(); // cosume ,
                rhss.push(self.exp());
            }
        }

        Box::new(Stmt::Assign {
            lhss: lhss,
            rhss: rhss,
            is_local: true,
        })
    }

    fn labelstat(&mut self) -> Box<Stmt> {
        let label = self.name();
        self.expect_tok(Tok::DColon);
        Box::new(Stmt::Label(label))
    }

    fn returnstat(&mut self) -> Box<Stmt> {
        let mut explist = vec![];
        if self.pos < self.ts.len() && !stat_follow(&self.ts[self.pos]) {
            explist.push(self.exp());
            while &self.ts[self.pos] == &Tok::Comma {
                self.skip(); // skip ,
                explist.push(self.exp());
            }
        }
        Box::new(Stmt::Return(explist))
    }

    fn breakstat(&mut self) -> Box<Stmt> {
        Box::new(Stmt::Break)
    }

    fn gotostat(&mut self) -> Box<Stmt> {
        let lbl = self.name();
        Box::new(Stmt::Goto(lbl))
    }

    // function call or assignment. Both start with a <prefixexp>.
    fn exprstat(&mut self) -> Box<Stmt> {
        match *self.suffixedexp() {
            Exp::Var(var) => {
                // assignment
                let mut varlist = vec![var];
                while &self.ts[self.pos] == &Tok::Comma {
                    self.pos += 1;
                    match *self.suffixedexp() {
                        Exp::Var(var) => varlist.push(var),
                        _ => panic!("exprstat"),
                    }
                }

                // not a "local" assignment, so a '= <exp>' has to follow
                self.expect_tok(Tok::Assign);
                let mut explist = vec![self.exp()];
                while &self.ts[self.pos] == &Tok::Comma {
                    explist.push(self.exp());
                }

                Box::new(Stmt::Assign {
                    lhss: varlist,
                    rhss: explist,
                    is_local: false,
                })
            },
            Exp::FunCall(fc) => {
                // function call
                Box::new(Stmt::FunCall(fc))
            },
            _ => panic!("exprstat"),
        }
    }

    // <funcall> or <var>
    fn suffixedexp(&mut self) -> Box<Exp> {
        let mut e0 = self.exp0();

        loop {
            println!("suffixedexp e0: {:?}", e0);
            match self.cur_tok() {
                Tok::LParen | Tok::SLit(_) | Tok::LBrace | Tok::Colon => {
                    // function or method call
                    // TODO: we redundantly copy string literals here
                    println!("suffixedexp parsing a funcall {:?}", e0);
                    e0 = Box::new(Exp::FunCall(self.funcall(e0)))
                },
                Tok::Dot => {
                    // field selection
                    self.skip(); // consume .
                    let name = self.name();
                    e0 = Box::new(Exp::Var(Var::Select(e0, Box::new(Exp::String(name.into_bytes())))))
                },
                Tok::LBracket => {
                    // field selection
                    self.skip(); // consume [
                    let field = self.exp();
                    self.expect_tok(Tok::RBracket);
                    e0 = Box::new(Exp::Var(Var::Select(e0, field)))
                },
                _ => { break; }
            }
        }

        e0
    }
}

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    fn binop(&mut self) -> Option<Binop> {
        match &self.ts[self.pos] {
            &Tok::Plus => Some(Binop::Add),
            &Tok::Minus => Some(Binop::Sub),
            &Tok::Star => Some(Binop::Mul),
            &Tok::Slash => Some(Binop::Div),
            &Tok::Exp => Some(Binop::Exp),
            &Tok::Percent => Some(Binop::Mod),
            &Tok::DDot => Some(Binop::Concat),
            &Tok::LT => Some(Binop::LT),
            &Tok::LEq => Some(Binop::LTE),
            &Tok::GT => Some(Binop::GT),
            &Tok::GEq => Some(Binop::GTE),
            &Tok::Equal => Some(Binop::EQ),
            &Tok::NotEqual => Some(Binop::NEQ),
            &Tok::And => Some(Binop::And),
            &Tok::Or => Some(Binop::Or),
            &Tok::DSlash => Some(Binop::IDiv),
            &Tok::DLT => Some(Binop::ShiftL),
            &Tok::DGT => Some(Binop::ShiftR),
            &Tok::Ampersand => Some(Binop::BAnd),
            &Tok::Pipe => Some(Binop::BOr),
            &Tok::Tilde => Some(Binop::BXor),
            _ => None
        }
    }

    fn unop(&mut self) -> Option<Unop> {
        match &self.ts[self.pos] {
            &Tok::Minus => Some(Unop::Neg),
            &Tok::Not => Some(Unop::Not),
            &Tok::Sh => Some(Unop::Len),
            &Tok::Tilde => Some(Unop::Complement),
            _ => None
        }
    }

    fn exp(&mut self) -> Box<Exp> {
        self.exp_(0)
    }

    fn exp_(&mut self, current_prec : Prec) -> Box<Exp> {
        let mut e1 = match self.unop() {
            None =>
                self.simpleexp(),
            Some(unop) => {
                self.skip();
                let e = self.exp_(unop_prec(unop));
                Box::new(Exp::Unop(unop, e))
            },
        };

        while let Some(op) = self.binop() {
            let op_prec = binop_prec(op);

            if op_prec.left < current_prec { break; }

            self.skip();
            let e2 = self.exp_(op_prec.right);
            e1 = Box::new(Exp::Binop(e1, op, e2));
        }

        e1
    }

    fn simpleexp(&mut self) -> Box<Exp> {
        match self.cur_tok() {
            Tok::Num(s) => { self.skip(); Box::new(Exp::Number(s)) },
            Tok::SLit(s) => { self.skip(); Box::new(Exp::String(s.into_bytes())) },
            Tok::Nil => { self.skip(); Box::new(Exp::Nil) },
            Tok::True => { self.skip(); Box::new(Exp::Bool(true)) }
            Tok::False => { self.skip(); Box::new(Exp::Bool(false)) }
            Tok::Ellipsis => { self.skip(); Box::new(Exp::Vararg) },
            Tok::LBrace => { self.skip(); self.constructor() },
            Tok::Function => {
                self.skip();
                let (args, vararg, body) = self.fundef();
                Box::new(Exp::FunDef { args: args, vararg: vararg, body: body })
            },
            _ =>
                self.suffixedexp(),
        }
    }

    // [fieldlist] '}'
    fn constructor(&mut self) -> Box<Exp> {
        println!("constructor");
        let mut fields = vec![];

        if &self.ts[self.pos] != &Tok::RBrace {
            fields.push(self.field());
            while self.cur_tok_() == &Tok::Comma || self.cur_tok_() == &Tok::Semic {
                self.skip(); // skip , or ;
                // we may have encountered trailing , or ;
                if self.cur_tok_() == &Tok::RBrace { break; }
                // otherwise parse a field
                fields.push(self.field());
            }
        }

        self.expect_tok(Tok::RBrace);
        Box::new(Exp::Tbl(fields))
    }

    fn field(&mut self) -> TblField {
        match &self.ts[self.pos] {
            &Tok::LBracket => {
                self.skip(); // skip [
                let e1 = self.exp();
                self.expect_tok(Tok::RBracket);
                self.expect_tok(Tok::Assign);
                let e2 = self.exp();
                TblField::ExpField { lhs: e1, rhs: e2 }
            },
            &Tok::Ident(ref n) if &self.ts[self.pos + 1] == &Tok::Assign => {
                self.skip(); // skip identifier
                self.expect_tok(Tok::Assign);
                let e2 = self.exp();
                TblField::NamedField { lhs: n.to_owned(), rhs: e2 }
            },
            _ => {
                println!("field");
                TblField::Field(self.exp())
            }
        }

    }

    // name | '(' exp ')'
    fn exp0(&mut self) -> Box<Exp> {
        match &self.ts[self.pos] {
            &Tok::Ident(ref s) => {
                self.skip(); // skip ident
                Box::new(Exp::Var(Var::Var(s.to_owned())))
            },
            &Tok::LParen => {
                self.skip(); // skip (
                let exp = self.exp();
                self.expect_tok(Tok::RParen);
                exp
            },
            tok =>
                panic!("exp0: Unexpected token: {:?}", tok),
        }
    }

    // function or method call.
    // <funargs> | : <name> <funargs>
    fn funcall(&mut self, f : Box<Exp>) -> FunCall {
        match self.cur_tok_() {
            &Tok::Colon => {
                self.skip();
                let mname = self.name();
                let args = self.funargs();
                FunCall::MethodCall(f, mname, args)
            },
            _ => {
                let args = self.funargs();
                FunCall::FunCall(f, args)
            },
        }
    }

    fn funargs(&mut self) -> Vec<Box<Exp>> {
        println!("funargs");

        let mut args = vec![];

        match self.cur_tok() {
            Tok::LParen => {
                self.skip(); // skip (
                // first arg, if exists
                if self.cur_tok_() != &Tok::RParen {
                    args.push(self.exp());
                }
                // other args
                while self.cur_tok_() == &Tok::Comma {
                    self.skip(); // skip ,
                    args.push(self.exp());
                }
                self.expect_tok(Tok::RParen);
            },
            Tok::LBrace => {
                // single table argument
                self.skip();
                args.push(self.constructor());
            },
            Tok::SLit(str) => {
                // single string argument
                self.skip();
                args.push(Box::new(Exp::String(str.into_bytes())));
            },
            _ => panic!("funargs"),
        }

        args
    }

    pub fn block(&mut self) -> Block {
        let mut stats = vec![];
        while !stat_follow(self.cur_tok_()) {
            if self.cur_tok_() == &Tok::Semic {
                self.skip();
            } else {
                stats.push(self.stat());
            }
        }
        stats
    }
}

////////////////////////////////////////////////////////////////////////////////
// Shared parts
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    // ( <idlist> [, ...] ) <block> end
    fn fundef(&mut self) -> (Vec<Id>, bool, Block) {
        self.expect_tok(Tok::LParen);

        let mut args = vec![];
        let mut vararg = false;
        if &self.ts[self.pos] != &Tok::RParen {
            if &self.ts[self.pos] == &Tok::Ellipsis {
                self.skip();
                vararg = true;
            } else {
                args.push(self.name());
                while &self.ts[self.pos] == &Tok::Comma {
                    self.skip(); // skip ,
                    if &self.ts[self.pos] == &Tok::Ellipsis {
                        self.skip();
                        vararg = true;
                    } else {
                        args.push(self.name());
                    }
                }
            }

        }

        self.expect_tok(Tok::RParen);
        let block = self.block();
        self.expect_tok(Tok::End);

        (args, vararg, block)
    }
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test_parser {
    use ast::*;
    use lexer::{Tok, tokenize};
    use parser::Parser;

    use std::io::BufReader;

    #[test]
    fn parser_exp_1() {
        let s = "1 + 2";
        let mut tokens = {
            let ret = tokenize(s);
            assert!(ret.is_ok());
            ret.unwrap()
        };
        tokens.push(Tok::EOS); // ugh
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.exp(),
                   Box::new(Exp::Binop(Box::new(Exp::Number("1".to_owned())),
                                       Binop::Add,
                                       Box::new(Exp::Number("2".to_owned())))));
    }
}
