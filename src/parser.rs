use ast::*;
use lexer::Token;

pub struct Parser<'a> {
    ts: &'a [Token],
    pos: usize,
}

enum SuffixedExp {
    Var(Var),
    FunCall(FunCall),
}

impl SuffixedExp {
    pub fn to_exp(self) -> Exp {
        match self {
            SuffixedExp::Var(v) => Exp::Var(v),
            SuffixedExp::FunCall(fc) => Exp::FunCall(fc),
        }
    }
}

/// Can the given token follow a statement? Useful when terminating statement
/// lists etc.
fn stat_follow(tok : &Token) -> bool {
    match tok {
        &Token::Else | &Token::ElseIf | &Token::End | &Token::Until | &Token::EOS => true,
        _ => false,
    }
}

impl<'a> Parser<'a> {
    pub fn new(ts: &'a [Token]) -> Parser<'a> {
        Parser {
            ts: ts,
            pos: 0,
        }
    }

    /// Token under cursor.
    fn cur_tok(&self) -> Token {
        self.ts[self.pos].clone()
    }

    /// Reference to token under cursor.
    fn cur_tok_(&self) -> &Token {
        &self.ts[self.pos]
    }

    /// Skip the given token or fail if it's not found.
    fn expect_tok(&mut self, tok : Token) {
        if self.ts[self.pos] != tok {
            panic!("Unexpected token");
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
        match self.cur_tok() {
            Token::Semic => { self.skip(); self.stat() },
            Token::If => { self.skip(); self.ifstat() },
            Token::While => { self.skip(); self.whilestat() },
            Token::Do => { self.skip(); self.dostat() },
            Token::For => { self.skip(); self.forstat() },
            Token::Repeat => { self.skip(); self.repeatstat() },
            Token::Function => { self.skip(); self.funcstat() },
            Token::Local => {
                self.skip();
                match self.cur_tok() {
                    Token::Function => self.localfuncstat(),
                    _ => self.localstat(),
                }
            },
            Token::DColon => { self.skip(); self.labelstat() },
            Token::Return => { self.skip(); self.returnstat() },
            Token::Break => { self.skip(); self.breakstat() },
            Token::Goto => { self.skip(); self.gotostat() },
            _ => self.exprstat(),
        }
    }

    fn ifstat(&mut self) -> Box<Stmt> {
        let mut conds = vec![];
        // parse initial condition
        conds.push(self.cond_then());
        // parse elseif blocks
        while self.cur_tok() == Token::ElseIf {
            self.skip(); // skip elseif
            conds.push(self.cond_then());
        }
        // parse optional else block
        let else_ = if self.cur_tok() == Token::Else {
            Some(self.block())
        } else {
            None
        };

        self.expect_tok(Token::End);

        Box::new(Stmt::If {
            conds: conds,
            else_: else_,
        })
    }

    // <condition> then <block>
    fn cond_then(&mut self) -> (Box<Exp>, Block) {
        let cond = self.exp();
        self.expect_tok(Token::Then);
        let block = self.block();
        (cond, block)
    }

    // <condition> do <block> end
    fn whilestat(&mut self) -> Box<Stmt> {
        let cond = self.exp();
        self.expect_tok(Token::Do);
        let block = self.block();
        self.expect_tok(Token::End);
        Box::new(Stmt::While(cond, block))
    }

    // <block> end
    fn dostat(&mut self) -> Box<Stmt> {
        let block = self.block();
        self.expect_tok(Token::End);
        Box::new(Stmt::Do(block))
    }

    // <fornum | forlist> end
    fn forstat(&mut self) -> Box<Stmt> {
        let var1 = self.name();
        match self.cur_tok() {
            Token::Equal => self.fornum(var1),
            Token::Comma | Token::In => self.forlist(var1),
            _ => panic!("Syntax error in for statement"),
        }
    }

    // A single variable
    fn name(&mut self) -> Id {
        match self.cur_tok() {
            Token::Ident(s) => { self.skip(); s },
            tok => panic!("name: unexpected token {:?}", tok),
        }
    }

    // exp1, exp2 [,exp3] <forbody>
    fn fornum(&mut self, var : Id) -> Box<Stmt> {
        let start = self.exp();
        self.expect_tok(Token::Comma);
        let end = self.exp();
        let step = {
            if self.cur_tok() == Token::Comma {
                self.skip();
                Some(self.exp())
            } else {
                None
            }
        };

        let body = self.block();
        self.expect_tok(Token::End);

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
        while self.cur_tok_() == &Token::Comma {
            vars.push(self.name());
            self.pos += 1;
        }

        self.expect_tok(Token::In);

        let mut exps = vec![self.exp()];
        while self.cur_tok_() == &Token::Comma {
            exps.push(self.exp());
            self.pos += 1;
        }

        let body = self.forbody();

        Box::new(Stmt::ForIn {
            vars: vars,
            exps: exps,
            body: body,
        })
    }

    fn forbody(&mut self) -> Block {
        self.expect_tok(Token::Do);
        let block = self.block();
        self.expect_tok(Token::End);
        block
    }

    fn repeatstat(&mut self) -> Box<Stmt> {
        let block = self.block();
        self.expect_tok(Token::Until);
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
        while self.cur_tok() == Token::Dot {
            self.skip(); // consume .
            fs.push(self.name());
        }
        let mname = {
            if self.cur_tok() == Token::Colon {
                self.skip(); // consume :
                Some(self.name())
            } else {
                None
            }
        };
        (n1, fs, mname)
    }

    // function <name> <fundef>
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
        while self.cur_tok_() == &Token::Comma {
            self.skip(); // consume ,
            lhss.push(Var::Var(self.name()));
        }

        let mut rhss = vec![];
        if self.cur_tok_() == &Token::Assign {
            self.skip(); // consume =
            rhss.push(self.exp());
            while self.cur_tok_() == &Token::Comma {
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
        self.expect_tok(Token::DColon);
        Box::new(Stmt::Label(label))
    }

    fn returnstat(&mut self) -> Box<Stmt> {
        let mut explist = vec![];
        if self.pos < self.ts.len() && !stat_follow(&self.ts[self.pos]) {
            explist.push(self.exp());
            while &self.ts[self.pos] == &Token::Comma {
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
        match self.suffixedexp() {
            SuffixedExp::Var(var) => {
                // assignment
                let mut varlist = vec![var];
                while &self.ts[self.pos] == &Token::Comma {
                    self.pos += 1;
                    match self.suffixedexp() {
                        SuffixedExp::Var(var) => varlist.push(var),
                        SuffixedExp::FunCall(_) => panic!("exprstat"),
                    }
                }

                // not a "local" assignment, so a '= <exp>' has to follow
                self.expect_tok(Token::Equal);
                let mut explist = vec![self.exp()];
                while &self.ts[self.pos] == &Token::Comma {
                    explist.push(self.exp());
                }

                Box::new(Stmt::Assign {
                    lhss: varlist,
                    rhss: explist,
                    is_local: false,
                })
            },
            SuffixedExp::FunCall(fc) => {
                // function call
                Box::new(Stmt::FunCall(fc))
            },
        }
    }

    // <funcall> or <var>
    fn suffixedexp(&mut self) -> SuffixedExp {
        let e0 = self.exp0();
        println!("suffixedexp e0: {:?}", e0);
        match self.cur_tok() {
            Token::LParen | Token::SLit(_) | Token::LBrace | Token::Colon => {
                // function or method call
                // TODO: we redundantly copy string literals here
                println!("suffixedexp parsing a funcall {:?}", e0);
                SuffixedExp::FunCall(self.funcall(e0))
            },
            Token::Dot => {
                // field selection
                self.skip(); // consume .
                let name = self.name();
                SuffixedExp::Var(Var::Select(e0, Box::new(Exp::String(name.into_bytes()))))
            },
            Token::LBracket => {
                // field selection
                self.skip(); // consume [
                let field = self.exp();
                self.expect_tok(Token::RBracket);
                SuffixedExp::Var(Var::Select(e0, field))
            },
            Token::EOS => {
                match *e0 {
                    Exp::Var(v) => SuffixedExp::Var(v),
                    Exp::FunCall(fc) => SuffixedExp::FunCall(fc),
                    _ => panic!("suffixedexp: unexpected end of stream"),
                }
            },
            tok =>
                panic!("suffixedexp: unexpected token: {:?}", tok),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    fn binop(&mut self) -> Option<Binop> {
        match &self.ts[self.pos] {
            &Token::Plus => Some(Binop::Add),
            &Token::Minus => Some(Binop::Sub),
            &Token::Star => Some(Binop::Mul),
            &Token::Slash => Some(Binop::Div),
            &Token::Exp => Some(Binop::Exp),
            &Token::Percent => Some(Binop::Mod),
            &Token::DDot => Some(Binop::Concat),
            &Token::LT => Some(Binop::LT),
            &Token::LEq => Some(Binop::LTE),
            &Token::GT => Some(Binop::GT),
            &Token::GEq => Some(Binop::GTE),
            &Token::Equal => Some(Binop::EQ),
            &Token::NotEqual => Some(Binop::NEQ),
            &Token::And => Some(Binop::And),
            &Token::Or => Some(Binop::Or),
            &Token::DSlash => Some(Binop::IDiv),
            &Token::DLT => Some(Binop::ShiftL),
            &Token::DGT => Some(Binop::ShiftR),
            &Token::Ampersand => Some(Binop::BAnd),
            &Token::Pipe => Some(Binop::BOr),
            &Token::Tilde => Some(Binop::BXor),
            _ => None
        }
    }

    fn unop(&mut self) -> Option<Unop> {
        match &self.ts[self.pos] {
            &Token::Minus => Some(Unop::Neg),
            &Token::Not => Some(Unop::Not),
            &Token::Sh => Some(Unop::Len),
            &Token::Tilde => Some(Unop::Complement),
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
            Token::Num(s) => { self.skip(); Box::new(Exp::Number(s)) },
            Token::SLit(s) => { self.skip(); Box::new(Exp::String(s)) },
            Token::Nil => { self.skip(); Box::new(Exp::Nil) },
            Token::True => { self.skip(); Box::new(Exp::Bool(true)) }
            Token::False => { self.skip(); Box::new(Exp::Bool(false)) }
            Token::Ellipsis => { self.skip(); Box::new(Exp::Vararg) },
            Token::LBrace => { self.skip(); self.constructor() },
            Token::Function => {
                self.skip();
                let (args, vararg, body) = self.fundef();
                Box::new(Exp::FunDef { args: args, vararg: vararg, body: body })
            },
            _ =>
                Box::new(self.suffixedexp().to_exp()),
        }
    }

    // [fieldlist] '}'
    fn constructor(&mut self) -> Box<Exp> {
        let mut fields = vec![];

        if &self.ts[self.pos] != &Token::RBrace {
            fields.push(self.field());
            while &self.ts[self.pos] == &Token::Comma
                || &self.ts[self.pos] == &Token::Semic {
                fields.push(self.field());
            }
            // skip optional trailing comma
            if &self.ts[self.pos] == &Token::Comma
                || &self.ts[self.pos] == &Token::Semic { self.skip(); }
        }

        self.expect_tok(Token::RBrace);
        Box::new(Exp::Tbl(fields))
    }

    fn field(&mut self) -> TblField {
        match &self.ts[self.pos] {
            &Token::LBracket => {
                self.skip(); // skip [
                let e1 = self.exp();
                self.expect_tok(Token::RBracket);
                self.expect_tok(Token::Assign);
                let e2 = self.exp();
                TblField::ExpField { lhs: e1, rhs: e2 }
            },
            &Token::Ident(ref n) => {
                self.skip(); // skip identifier
                self.expect_tok(Token::Assign);
                let e2 = self.exp();
                TblField::NamedField { lhs: n.to_owned(), rhs: e2 }
            },
            _ =>
                TblField::Field(self.exp())
        }

    }

    // name | '(' exp ')'
    fn exp0(&mut self) -> Box<Exp> {
        match &self.ts[self.pos] {
            &Token::Ident(ref s) => {
                self.skip(); // skip ident
                Box::new(Exp::Var(Var::Var(s.to_owned())))
            },
            &Token::LParen => {
                self.skip(); // skip (
                let exp = self.exp();
                self.expect_tok(Token::RParen);
                exp
            },
            tok =>
                panic!("exp0: Unexpected token: {:?}", tok),
        }
    }

    // function or method call.
    // <funargs> | : <name> <funargs>
    fn funcall(&mut self, f : Box<Exp>) -> FunCall {
        match &self.ts[self.pos] {
            &Token::Colon => {
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
            Token::LParen => {
                self.skip(); // skip (
                args.push(self.exp());
                while self.cur_tok() == Token::Comma {
                    self.skip(); // skip ,
                    args.push(self.exp());
                }
                self.expect_tok(Token::RParen);
            },
            Token::LBrace => {
                // single table argument
                self.skip();
                args.push(self.constructor());
            },
            Token::SLit(str) => {
                // single string argument
                self.skip();
                args.push(Box::new(Exp::String(str)));
            },
            _ => panic!("funargs"),
        }

        args
    }

    pub fn block(&mut self) -> Block {
        let mut stats = vec![];
        while !stat_follow(&self.cur_tok()) {
            stats.push(self.stat());
        }
        stats
    }
}

////////////////////////////////////////////////////////////////////////////////
// Shared parts
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    // ( <idlist> [, ...] ) <block>
    fn fundef(&mut self) -> (Vec<Id>, bool, Block) {
        self.expect_tok(Token::LParen);

        let mut args = vec![];
        let mut vararg = false;
        if &self.ts[self.pos] != &Token::RParen {
            if &self.ts[self.pos] == &Token::Ellipsis {
                self.skip();
                vararg = true;
            } else {
                args.push(self.name());
                while &self.ts[self.pos] == &Token::Comma {
                    self.skip(); // skip ,
                    if &self.ts[self.pos] == &Token::Ellipsis {
                        self.skip();
                        vararg = true;
                    } else {
                        args.push(self.name());
                    }
                }
            }

        }

        self.expect_tok(Token::RParen);
        let block = self.block();

        (args, vararg, block)
    }
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test_parser {
    use ast::*;
    use lexer::{Lexer, Token};
    use parser::Parser;

    use std::io::BufReader;

    #[test]
    fn parser_exp_1() {
        let s = "1 + 2";
        let inp = BufReader::new(s.as_bytes());
        let mut tokens : Vec<Token> = Lexer::new(inp).collect();
        tokens.push(Token::EOS); // ugh
        let mut parser = Parser::new(&tokens);
        assert_eq!(parser.exp(),
                   Box::new(Exp::Binop(Box::new(Exp::Number("1".to_owned())),
                                       Binop::Add,
                                       Box::new(Exp::Number("2".to_owned())))));
    }
}
