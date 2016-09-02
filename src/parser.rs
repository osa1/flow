use ast::*;
use cfg::{Atom, BasicBlock, CFGBuilder, LHS, RHS, Stat};
use cfg;
use lexer::Tok;
use uniq::UniqCounter;
use utils::Either;

use std::collections::HashMap;
use std::collections::HashSet;
use std;

pub struct Parser<'a> {
    ts: &'a [Tok],
    pos: usize,

    /// Closure definitions.
    defs: HashMap<cfg::Var, cfg::CFG>,

    /// Current CFG. New statements and basic blocks are added here.
    cfg: cfg::CFGBuilder,

    global_vars: HashMap<String, cfg::Var>,
    local_vars: Vec<HashMap<String, cfg::Var>>,

    /// Indices of closures scope in `local_vars` vector.
    /// TODO: Give some examples.
    clo_scope: Vec<CloScope>,

    /// Variable counter used to generate fresh variables.
    var_gen: UniqCounter,

    // TODO: Need a symbol table to map uniqs to original names and positions.
}

struct CloScope {
    /// This is the index of where the closure scope starts in `local_vars`.
    scope_idx: usize,

    /// Captured variables. These are variables used in the closure that are
    /// bound in scopes before `scope_idx` in `local_vars`.
    captured: HashSet<cfg::Var>,
}

impl CloScope {
    fn new(scope_idx : usize) -> CloScope {
        CloScope {
            scope_idx: scope_idx,
            captured: HashSet::new(),
        }
    }
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
        debug_assert!(ts.len() > 0 && &ts[ts.len() - 1] == &Tok::EOS);

        Parser {
            ts: ts,
            pos: 0,
            defs: HashMap::new(),
            cfg: cfg::CFGBuilder::new(vec![]),
            global_vars: HashMap::new(),
            local_vars: vec![],
            clo_scope: vec![],
            var_gen: UniqCounter::new(b'p'),
        }
    }

    /// Tok under cursor.
    #[inline(always)]
    fn cur_tok(&self) -> Tok {
        unsafe { self.ts.get_unchecked(self.pos).clone() }
    }

    /// Reference to the token under cursor.
    #[inline(always)]
    fn cur_tok_(&self) -> &Tok {
        unsafe { &self.ts.get_unchecked(self.pos) }
    }

    /// Skip the given token or fail if it's not found.
    #[inline(always)]
    fn expect_tok(&mut self, tok : Tok) {
        if self.cur_tok_() != &tok {
            panic!("Unexpected token. Expected {:?}, found {:?}", tok, self.cur_tok_());
        }
        self.skip();
    }

    #[inline(always)]
    fn skip(&mut self) {
        self.pos += 1;
    }

    /// Generate a new variable or return an existing variable, for variable
    /// declarations/assignments.
    fn var_asgn(&mut self, s : &str, is_local : bool) -> cfg::Var {
        if is_local {
            let v = self.fresh_var();
            let env_idx = self.local_vars.len() - 1;
            (&mut self.local_vars[env_idx]).insert(s.to_owned(), v);
            v
        } else {
            // It could be a locally-bound variable. Search local envs first.
            for local_env in self.local_vars.iter() {
                match local_env.get(s) {
                    Some(v) => { return *v; },
                    None => {}
                }
            }
            // Global variable
            // TODO: Duplicated from 'var_use'
            let v1 = match self.global_vars.get(s) {
                None => None,
                Some(v) => Some(*v),
            };
            match v1 {
                None => {
                    let v = self.fresh_var();
                    self.global_vars.insert(s.to_owned(), v);
                    v
                },
                Some(v) => v
            }
        }
    }

    /// Generate a new variable or return an existing variable, for uses of
    /// variables.
    fn var_use(&mut self, s : &str) -> cfg::Var {
        // TODO: Searching through many environments may be too slow?
        // look for local variables first
        for local_env in self.local_vars.iter() {
            match local_env.get(s) {
                Some(v) => { return *v; },
                None => {},
            }
        }

        // it has to be a global variable
        // Stupid borrow checker issues again.
        let v1 = match self.global_vars.get(s) {
            None => None,
            Some(v) => Some(*v),
        };
        match v1 {
            None => {
                let v = self.fresh_var();
                self.global_vars.insert(s.to_owned(), v);
                v
            },
            Some(v) => v
        }
    }

    #[inline(always)]
    fn fresh_var(&mut self) -> cfg::Var {
        self.var_gen.fresh()
    }

    /// Enter a new lexical scope.
    #[inline(always)]
    fn enter_scope(&mut self) {
        self.local_vars.push(HashMap::new());
    }

    #[inline(always)]
    fn enter_closure(&mut self) {
        self.clo_scope.push(CloScope::new(self.local_vars.len() - 1));
        self.enter_scope();
    }

    /// Exit the current scope.
    #[inline(always)]
    fn exit_scope(&mut self) {
        self.local_vars.pop().unwrap();
    }

    #[inline(always)]
    fn exit_closure(&mut self) -> HashSet<cfg::Var> {
        self.exit_closure();
        self.clo_scope.pop().unwrap().captured
    }

    #[inline(always)]
    fn register_def(&mut self, var : cfg::Var, cfg : cfg::CFG) {
        self.defs.insert(var, cfg);
    }

    #[inline(always)]
    fn add_stat(&mut self, stat : cfg::Stat) {
        self.cfg.add_stat(stat);
    }

    #[inline(always)]
    fn terminate(&mut self, bb : cfg::BasicBlock) {
        self.cfg.terminate(cfg::Terminator::Jmp(bb));
    }

    #[inline(always)]
    fn cond_terminate(&mut self, cond : Atom, then_bb : cfg::BasicBlock, else_bb : cfg::BasicBlock) {
        self.cfg.terminate(cfg::Terminator::CondJmp(cond, then_bb, else_bb));
    }

    #[inline(always)]
    fn ret(&mut self, exps : Vec<Atom>) {
        self.cfg.terminate(cfg::Terminator::Ret(exps));
    }

    #[inline(always)]
    fn new_bb(&mut self) -> cfg::BasicBlock {
        self.cfg.new_bb()
    }

    #[inline(always)]
    fn set_bb(&mut self, bb : cfg::BasicBlock) {
        self.cfg.set_bb(bb)
    }
}

////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    /// Parse a a list of statements in a new scope.
    pub fn block(&mut self) {
        self.enter_scope();
        self.block_();
        self.exit_scope();
    }

    /// Parse a list of statements in the existing scope.
    fn block_(&mut self) {
        while !stat_follow(self.cur_tok_()) {
            if self.cur_tok_() == &Tok::Semic {
                self.skip(); // skip ;
            } else {
                self.stat();
            }
        }
    }

    fn stat(&mut self) {
        match self.cur_tok_() {
            &Tok::Semic => { self.skip(); self.stat(); },
            &Tok::If => { self.skip(); self.ifstat(); },
            &Tok::While => { self.skip(); self.whilestat(); },
            &Tok::Do => { self.skip(); self.dostat(); },
            &Tok::For => { self.skip(); self.forstat(); },
            &Tok::Repeat => { self.skip(); self.repeatstat(); },
            &Tok::Function => { self.skip(); self.funcstat(); },
            &Tok::Local => {
                self.skip(); // skip "local"
                match self.cur_tok_() {
                    &Tok::Function => { self.skip(); self.localfuncstat(); },
                    _ => { self.localstat(); },
                }
            },
            &Tok::DColon => { self.skip(); self.labelstat(); },
            &Tok::Return => { self.skip(); self.returnstat(); },
            &Tok::Break => { self.skip(); self.breakstat(); },
            &Tok::Goto => { self.skip(); self.gotostat(); },
            _ => { self.exprstat(); },
        }
    }

    fn ifstat(&mut self) {
        // continuation
        let cont_bb = self.new_bb();

        // parse first condition and block
        let then_bb = self.new_bb();
        let else_bb = self.new_bb();
        self.cond_then(then_bb, else_bb, cont_bb);
        self.set_bb(else_bb);

        // if there are no elseif or else blocks then cont_bb is just else_bb
        match self.cur_tok_() {
            &Tok::ElseIf | &Tok::Else => {},
            _ => {
                // then_bb was previously jumping to cont_bb, so fix that
                self.set_bb(then_bb);
                self.terminate(else_bb);
                // continuation is not cont_bb because there are not elseif or
                // else blocks
                self.set_bb(else_bb);
                return;
            },
        }

        // otherwise parse rest of the blocks, using cont_bb as the continuation
        // of the whole if expression.

        // parse elseifs
        while self.cur_tok_() == &Tok::ElseIf {
            self.skip(); // skip elseif
            let then_bb = self.new_bb();
            let else_bb = self.new_bb();
            self.cond_then(then_bb, else_bb, cont_bb);
            self.set_bb(else_bb);
        }

        // parse optional else block
        if self.cur_tok_() == &Tok::Else {
            self.skip(); // skip else
            self.block();
            self.terminate(cont_bb);
        }

        self.set_bb(cont_bb);
    }

    /// <condition> then <block>
    fn cond_then(&mut self, then_bb : cfg::BasicBlock, else_bb : cfg::BasicBlock, cont_bb : cfg::BasicBlock) {
        let cond = self.exp();
        self.expect_tok(Tok::Then);
        self.cond_terminate(cond, then_bb, else_bb);

        // then block jumps to the continuation
        self.set_bb(then_bb);
        self.block();
        self.terminate(cont_bb);
    }

    // <condition> do <block> end
    fn whilestat(&mut self) {
        // bb that checks the condition
        let cond_bb = self.new_bb();
        // bb for the body
        let body_bb = self.new_bb();
        // bb for the continuation
        let cont_bb = self.new_bb();

        // start with the condition
        self.terminate(cond_bb);
        self.set_bb(cond_bb);
        let cond = self.exp();
        self.cond_terminate(cond, body_bb, cont_bb);

        self.expect_tok(Tok::Do);
        self.set_bb(body_bb);
        self.block();
        self.terminate(cond_bb); // loop
        self.expect_tok(Tok::End);

        self.cfg.set_bb(cont_bb);
    }

    // <block> end
    fn dostat(&mut self) {
        self.block();
        self.expect_tok(Tok::End);
    }

    // <fornum | forlist> end
    fn forstat(&mut self) {
        unimplemented!();
        // let var1 = self.name();
        // match self.cur_tok_() {
        //     &Tok::Assign => { self.skip(); self.fornum(var1) },
        //     &Tok::Comma | &Tok::In => self.forlist(var1),
        //     _ => panic!("Syntax error in for statement"),
        // }
    }

    // exp1, exp2 [,exp3] <forbody>
    fn fornum(&mut self, var : Id) {
        unimplemented!();
        // let start = self.exp();
        // self.expect_tok(Tok::Comma);
        // let end = self.exp();
        // let step = {
        //     if self.cur_tok_() == &Tok::Comma {
        //         self.skip();
        //         Some(self.exp())
        //     } else {
        //         None
        //     }
        // };

        // self.expect_tok(Tok::Do);
        // let body = self.block();
        // self.expect_tok(Tok::End);

        // Stmt::ForRange {
        //     var: var,
        //     start: start,
        //     end: end,
        //     step: step,
        //     body: body,
        // }
    }

    // <name> {,<name>} in <explist> <forbody>
    fn forlist(&mut self, var1 : Id) {
        unimplemented!();
        // let mut vars = vec![var1];
        // while self.cur_tok_() == &Tok::Comma {
        //     self.skip(); // skip ,
        //     vars.push(self.name());
        // }

        // self.expect_tok(Tok::In);

        // let mut exps = vec![self.exp()];
        // while self.cur_tok_() == &Tok::Comma {
        //     self.skip(); // skip ,
        //     exps.push(self.exp());
        // }

        // let body = self.forbody();

        // Stmt::ForIn {
        //     vars: vars,
        //     exps: exps,
        //     body: body,
        // }
    }

    fn forbody(&mut self) {
        unimplemented!();
        // self.expect_tok(Tok::Do);
        // let block = self.block();
        // self.expect_tok(Tok::End);
        // block
    }

    fn repeatstat(&mut self) {
        // beginning of the loop
        let begin_bb = self.new_bb();
        // continuation
        let cont_bb = self.new_bb();

        // jump to the loop
        self.terminate(begin_bb);
        self.block(); // TODO: Scoping here is different
        self.expect_tok(Tok::Until);

        let cond = self.exp();
        self.cond_terminate(cond, begin_bb, cont_bb);

        self.set_bb(cont_bb);
    }

    fn funcstat(&mut self) {
        let (n, mut sels, mname) = self.funcname();

        let (fun_var, captures) = {
            // parse function body. register the CFG as a new definition.
            let (fun_cfg, captures) = {
                match mname {
                    Some(mname) => {
                        sels.push(mname);
                        self.fundef(true) // is_method = true
                    },
                    None => {
                        self.fundef(false) // is_method = false
                    }
                }
            };

            let fun_var = self.fresh_var();
            self.register_def(fun_var, fun_cfg);
            (fun_var, captures)
        };


        // initialize the table
        let table_var = self.fresh_var();
        self.add_stat(Stat::Assign(LHS::Var(table_var, ), RHS::NewTbl));
        // write the function to the table
        self.add_stat(Stat::Assign(LHS::Tbl(table_var, Atom::Number(Number::Int(0))),
                                   RHS::Atom(Atom::Var(fun_var))));
        // write captured values to the table
        for (capture_idx, captured) in captures.into_iter().enumerate() {
            self.add_stat(Stat::Assign(LHS::Tbl(table_var, Atom::Number(Number::Int((capture_idx + 1) as i64))),
                                       RHS::Atom(Atom::Var(captured))));
        }
        // write the closure to its final location
        // TODO: move strings from 'self' to atoms somehow
        let mut lhs : cfg::Var = self.var_use(&n);
        if sels.len() > 0 {
            for i in 0 .. sels.len() - 2 {
                let new_lhs = self.fresh_var();
                let sel = Atom::String((&sels[i]).to_owned());
                self.add_stat(Stat::Assign(LHS::Var(new_lhs), RHS::ReadTbl(Atom::Var(lhs), sel)));
                lhs = new_lhs;
            }
        }
        self.add_stat(Stat::Assign(LHS::Tbl(lhs, Atom::String((&sels[sels.len() - 1]).to_owned())),
                                   RHS::Atom(Atom::Var(table_var))));
    }

    /// (tbl, fields, optional method name)
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
    fn localfuncstat(&mut self) {
        unimplemented!()
        // let fname = self.name();
        // let (args, vararg, body) = self.fundef();

        // Stmt::Assign {
        //     lhss: vec![Var::Var(fname)],
        //     rhss: vec![Box::new(Exp::FunDef {
        //         args:args,
        //         vararg: vararg,
        //         body: body,
        //     })],
        //     is_local: true,
        // }
    }

    // <namelist> [= <explist>]
    fn localstat(&mut self) {
        unimplemented!();
        /*
        // there should be at least one name
        let mut lhss = vec![self.name()];
        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // consume ,
            lhss.push(Var::Var(self.name()));
        }

        let mut rhss = vec![];
        if self.cur_tok_() == &Tok::Assign {
            self.skip(); // consume =
            rhss.push(self.exp());
            while self.cur_tok_() == &Tok::Comma {
                self.skip(); // consume ,
                rhss.push(self.exp());
            }
        }

        Stmt::Assign {
            lhss: lhss,
            rhss: rhss,
            is_local: true,
        }
        */
    }

    fn labelstat(&mut self) -> Stmt {
        let label = self.name();
        self.expect_tok(Tok::DColon);
        Stmt::Label(label)
    }

    fn returnstat(&mut self) {
        let mut explist = vec![];
        if self.pos < self.ts.len() && self.cur_tok_() != &Tok::Semic && !stat_follow(&self.ts[self.pos]) {
            explist.push(self.exp());
            while self.cur_tok_() == &Tok::Comma {
                self.skip(); // skip ,
                explist.push(self.exp());
            }
        }
        self.ret(explist);
    }

    fn breakstat(&mut self) -> Stmt {
        Stmt::Break
    }

    fn gotostat(&mut self) -> Stmt {
        let lbl = self.name();
        Stmt::Goto(lbl)
    }

    // function call or assignment. Both start with a <suffixedexp>.
    fn exprstat(&mut self) {
        unimplemented!();
        //match self.suffixedexp_stat() {
        //    Exp::Var(var) => {
        //        // assignment
        //        let mut varlist = vec![var];
        //        while self.cur_tok_() == &Tok::Comma {
        //            self.skip(); // skip ,
        //            match *self.suffixedexp() {
        //                Exp::Var(var) => varlist.push(var),
        //                _ => panic!("exprstat"),
        //            }
        //        }

        //        // not a "local" assignment, so a '= <exp>' has to follow
        //        self.expect_tok(Tok::Assign);
        //        let mut explist = vec![self.exp()];
        //        while self.cur_tok_() == &Tok::Comma {
        //            self.skip(); // skip ,
        //            explist.push(self.exp());
        //        }

        //        Stmt::Assign {
        //            lhss: varlist,
        //            rhss: explist,
        //            is_local: false,
        //        }
        //    },
        //    Exp::FunCall(fc) => {
        //        // function call
        //        Stmt::FunCall(fc)
        //    },
        //    _ => panic!("exprstat"),
        //}
    }
}

////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    fn binop(&mut self) -> Option<Binop> {
        match self.cur_tok_() {
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
        match self.cur_tok_() {
            &Tok::Minus => Some(Unop::Neg),
            &Tok::Not => Some(Unop::Not),
            &Tok::Sh => Some(Unop::Len),
            &Tok::Tilde => Some(Unop::Complement),
            _ => None
        }
    }

    fn exp(&mut self) -> Atom {
        self.exp_(0)
    }

    fn exp_(&mut self, current_prec : Prec) -> Atom {
        let mut e1 = match self.unop() {
            None =>
                self.simpleexp(),
            Some(unop) => {
                self.skip(); // skip op
                let e = self.exp_(UNOP_PREC);
                let ret = self.fresh_var();
                self.add_stat(Stat::Assign(LHS::Var(ret), RHS::Unop(unop, e)));
                Atom::Var(ret)
            },
        };

        // mmm not sure if the precedence helps with the evaluation order

        while let Some(op) = self.binop() {
            let op_prec = binop_prec(op);

            if current_prec > op_prec.left { break; }

            self.skip(); // skip op
            let e2 = self.exp_(op_prec.right);
            let ret = self.fresh_var();
            self.add_stat(Stat::Assign(LHS::Var(ret), RHS::Binop(e1, op, e2)));
            e1 = Atom::Var(ret);
        }

        e1
    }

    fn simpleexp(&mut self) -> cfg::Atom {
        match self.cur_tok() {
            Tok::Num(n) => unimplemented!(), // { self.skip(); Atom::Number(n) },
            Tok::SLit(s) => { self.skip(); Atom::String(s) },
            Tok::Nil => { self.skip(); Atom::Nil },
            Tok::True => { self.skip(); Atom::Bool(true) }
            Tok::False => { self.skip(); Atom::Bool(false) }
            Tok::Ellipsis => { unimplemented!() }
            Tok::LBrace => { self.skip(); self.constructor() },
            Tok::Function => {
                self.skip();
                unimplemented!()
                // let (args, vararg, body) = self.fundef();
                // Box::new(Exp::FunDef { args: args, vararg: vararg, body: body })
            },
            _ =>
                self.suffixedexp_exp(),
        }
    }

    // [fieldlist] '}'
    fn constructor(&mut self) -> Atom {
        let ret = self.fresh_var();
        self.add_stat(Stat::Assign(LHS::Var(ret), RHS::NewTbl));

        // current index to use for expressions without keys
        // eg. in { a, b, c } we use this as keys for a, b and c.
        let mut cur_pos = 1;

        if self.cur_tok_() != &Tok::RBrace {
            if self.field(ret, cur_pos) { cur_pos += 1; }
            while self.cur_tok_() == &Tok::Comma || self.cur_tok_() == &Tok::Semic {
                self.skip(); // skip , or ;
                // we may have encountered trailing , or ;
                if self.cur_tok_() == &Tok::RBrace { break; }
                // otherwise parse a field
                if self.field(ret, cur_pos) { cur_pos += 1; }
            }
        }

        self.expect_tok(Tok::RBrace);
        Atom::Var(ret)
    }

    /// true -> field with implicit key
    fn field(&mut self, tbl : cfg::Var, pos : i64) -> bool {
        match &self.ts[self.pos] {
            &Tok::LBracket => {
                self.skip(); // skip [
                let e1 = self.exp();
                self.expect_tok(Tok::RBracket);
                self.expect_tok(Tok::Assign);
                let e2 = self.exp();
                self.add_stat(Stat::Assign(LHS::Tbl(tbl, e1), RHS::Atom(e2)));
                false
            },
            &Tok::Ident(ref n) if &self.ts[self.pos + 1] == &Tok::Assign => {
                self.skip(); // skip identifier
                self.skip(); // skip =
                let e2 = self.exp();
                self.add_stat(Stat::Assign(LHS::Tbl(tbl, Atom::String(n.to_owned())), RHS::Atom(e2)));
                false
            },
            _ => {
                let e = self.exp();
                self.add_stat(Stat::Assign(LHS::Tbl(tbl, Atom::Number(Number::Int(pos))), RHS::Atom(e)));
                true
            }
        }

    }

    // A suffixedexp is either a function call, or an expression that can be
    // used as a LHS of an assignment (e.g. a table field selection). Since they
    // can be used statement context (when parsing a LHS or a function call
    // statement) and in expression context, we have two different functions
    // that parse the same thing but does different things.
    //
    // - Expression variant returns an Atom for the result.
    // - Statement variant emits a function call statement or an assignment.

    // <funcall> or <var>
    fn suffixedexp_exp(&mut self) -> Atom {
        let mut e0 = self.exp0();

        loop {
            match unsafe { self.ts.get_unchecked(self.pos) } {
                &Tok::LParen | &Tok::SLit(_) | &Tok::LBrace | &Tok::Colon => {
                    // function or method call
                    e0 = self.funcall(e0);
                },
                &Tok::Dot => {
                    // field selection
                    self.skip(); // consume .
                    let name = self.name();
                    let e1 = self.fresh_var();
                    self.add_stat(Stat::Assign(LHS::Var(e1), RHS::ReadTbl(e0, Atom::String(name))));
                    e0 = Atom::Var(e1);
                },
                &Tok::LBracket => {
                    // field selection
                    self.skip(); // consume [
                    let field = self.exp();
                    self.expect_tok(Tok::RBracket);
                    let e1 = self.fresh_var();
                    self.add_stat(Stat::Assign(LHS::Var(e1), RHS::ReadTbl(e0, field)));
                    e0 = Atom::Var(e1);
                },
                _ => { break; }
            }
        }

        e0
    }

    fn lhs_to_atom(&mut self, lhs : LHS) -> Atom {
        match lhs {
            LHS::Var(var) => Atom::Var(var),
            LHS::Tbl(var, sel) => {
                let ret = self.fresh_var();
                self.add_stat(Stat::Assign(LHS::Var(ret), RHS::ReadTbl(Atom::Var(var), sel)));
                Atom::Var(ret)
            },
        }
    }

    fn atom_lhs_to_atom(&mut self, lhs_atom : Either<Atom, LHS>) -> Atom {
        match lhs_atom {
            Either::Left(atom) => atom,
            Either::Right(lhs) => self.lhs_to_atom(lhs),
        }
    }

    fn atom_to_var(&mut self, atom : Atom) -> cfg::Var {
        match atom {
            Atom::Var(var) => var,
            _ => {
                let ret = self.fresh_var();
                self.add_stat(Stat::Assign(LHS::Var(ret), RHS::Atom(atom)));
                ret
            }
        }
    }

    /// Parse a single assignment LHS or function call.
    fn suffixedexp_stat(&mut self) -> Either<Atom, LHS> {
        let mut last : Either<Atom, LHS> = Either::Left(self.exp0());

        loop {
            match unsafe { self.ts.get_unchecked(self.pos) } {
                &Tok::LParen | &Tok::SLit(_) | &Tok::LBrace | &Tok::Colon => {
                    last = {
                        let atom = self.atom_lhs_to_atom(last);
                        Either::Left(self.funcall(atom))
                    };
                },
                &Tok::Dot => {
                    last = {
                        // field selection
                        let atom = self.atom_lhs_to_atom(last);
                        self.skip(); // consume .
                        let name = self.name();
                        Either::Right(LHS::Tbl(self.atom_to_var(atom), Atom::String(name)))
                    };
                },
                &Tok::LBracket => {
                    // field selection
                    last = {
                        self.skip(); // consume [
                        let atom = self.atom_lhs_to_atom(last);
                        let field = self.exp();
                        self.expect_tok(Tok::RBracket);
                        Either::Right(LHS::Tbl(self.atom_to_var(atom), field))
                    };
                },
                _ => { break; },
            }
        }

        last
    }

    // name | '(' exp ')'
    fn exp0(&mut self) -> Atom {
        match &self.ts[self.pos] {
            &Tok::Ident(ref s) => {
                self.skip(); // skip ident
                Atom::Var(self.var_use(s))
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
    fn funcall(&mut self, f : Atom) -> Atom {
        match self.cur_tok_() {
            &Tok::Colon => {
                self.skip(); // skip :
                let mname = self.name();
                let mut args = self.funargs();
                // binder for the function to call
                let fun_var = self.fresh_var();
                // binder for the result
                let ret_var = self.fresh_var();
                self.add_stat(Stat::Assign(LHS::Var(fun_var), RHS::ReadTbl(f, Atom::String(mname))));
                args.insert(0, Atom::Var(fun_var));
                self.add_stat(Stat::Assign(LHS::Var(ret_var), RHS::FunCall(Atom::Var(fun_var), args)));
                Atom::Var(ret_var)
            },
            _ => {
                let mut args = self.funargs();
                args.insert(0, f.clone());
                // binder for the result
                let ret_var = self.fresh_var();
                self.add_stat(Stat::Assign(LHS::Var(ret_var), RHS::FunCall(f, args)));
                Atom::Var(ret_var)
            },
        }
    }

    fn funargs(&mut self) -> Vec<Atom> {
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
                self.skip(); // skip {
                args.push(self.constructor());
            },
            Tok::SLit(str) => {
                // single string argument
                self.skip(); // skip string
                args.push(Atom::String(str));
            },
            _ => panic!("funargs"),
        }

        args
    }
}

////////////////////////////////////////////////////////////////////////////////
// Shared parts
////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {

    // A single variable
    fn name(&mut self) -> Id {
        match self.cur_tok() {
            Tok::Ident(s) => { self.skip(); s },
            tok => panic!("name: unexpected token {:?}", tok),
        }
    }

    /// Compile a function definition to CFG. Also returns captured variables.
    /// Syntax: ( <idlist> [, ...] ) <block> end
    fn fundef(&mut self, is_method : bool) -> (cfg::CFG, HashSet<cfg::Var>) {
        self.expect_tok(Tok::LParen);

        self.enter_closure();

        // collect args
        let mut args = vec![];
        let mut vararg = false;
        if self.cur_tok_() != &Tok::RParen {
            if self.cur_tok_() == &Tok::Ellipsis {
                self.skip(); // skip ...
                vararg = true;
            } else {
                let clo_name = self.name();
                let clo_arg = self.var_asgn(&clo_name, true);
                args.push(clo_arg);
                while self.cur_tok_() == &Tok::Comma {
                    self.skip(); // skip ,
                    if self.cur_tok_() == &Tok::Ellipsis {
                        self.skip(); // skip ...
                        vararg = true;
                        // we should break here as vararg has to be the last arg
                        break;
                    } else {
                        let arg_name = self.name();
                        args.push(self.var_asgn(&arg_name, true));
                    }
                }
            }
        }

        self.expect_tok(Tok::RParen);
        let mut fun_cfg = std::mem::replace(&mut self.cfg, CFGBuilder::new(args));
        let block = self.block();
        std::mem::swap(&mut self.cfg, &mut fun_cfg);
        self.expect_tok(Tok::End);

        let captured = self.exit_closure();

        (fun_cfg.build(), captured)
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
    use test_utils::*;

    use test::Bencher;

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

    #[bench]
    fn parser_bench(b : &mut Bencher) {

        // Read all Lua files, concatenate contents and parse as one big
        // program. 10878 lines in total.

        let lua = concat_lua_tests();
        let mut tokens = tokenize(&lua).unwrap();
        tokens.push(Tok::EOS);
        b.iter(|| {
            let mut parser = Parser::new(&tokens);
            parser.block()
        });
    }
}
