use ast::*;
use cfg::{BasicBlock, CFGBuilder, CFG, LHS, RHS, Stat, Var, Terminator};
use labels::Labels;
use lexer::Tok;
use scoping::{Scopes, VarOcc};
use uniq::{ENTRY_UNIQ};
use utils::Either;

use std::collections::HashMap;
use std::collections::HashSet;
use std;

pub struct Parser<'a> {
    /// The token stream.
    ts: &'a [Tok],

    /// Current position in the token stream.
    /// TODO: Maybe remove this and update the slice instead.
    pos: usize,

    /// Scope handler.
    scopes: Scopes,

    /// Label handler.
    labels: Labels,

    /// Closure definitions.
    defs: HashMap<Var, CFG>,

    /// Current CFG. New statements and basic blocks are added here.
    cur_cfg: CFGBuilder,

    /// `Var` for `cur_cfg`. e.g. what definition we are parsing right now.
    cur_var: Var,

    /// Where to jump on `break`. Push continuation when entering a loop. Pop it
    /// afterwards.
    loop_conts: Vec<BasicBlock>,
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
        // instead of doing bounds checking we rely on this terminator
        debug_assert!(ts.len() > 0 && &ts[ts.len() - 1] == &Tok::EOS);

        Parser {
            ts: ts,
            pos: 0,
            scopes: Scopes::new(),
            labels: Labels::new(),
            defs: HashMap::new(),
            cur_cfg: CFGBuilder::new(vec![]),
            cur_var: ENTRY_UNIQ, // special
            loop_conts: vec![],
        }
    }

    /// Entry point. Returns list of definitions.
    pub fn parse(mut self) -> HashMap<Var, CFG> {
        self.block();
        let mut defs = self.defs;
        let captures = vec![]; // no captures at the top level
        defs.insert(self.cur_var, self.cur_cfg.build(captures));
        defs
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

    /// Generate a LHS of a variable (in the source program). Handles captured
    /// variables in closures.
    fn var_lhs_use(&mut self, s : &str) -> LHS {
        match self.scopes.var_occ(s) {
            VarOcc::Captured(var) => LHS::Captured(var),
            VarOcc::Local(var) => LHS::Var(var),
            VarOcc::Global(var) => LHS::Var(var),
        }
    }

    fn lhs_to_var(&mut self, lhs : LHS) -> Var {
        match lhs {
            LHS::Tbl(tbl, sel) => {
                let ret = self.fresh_var();
                self.assign(LHS::Var(ret), RHS::ReadTbl(tbl, sel));
                ret
            },
            LHS::Var(v) => v,
            LHS::Captured(v) => {
                let ret = self.fresh_var();
                self.assign(LHS::Var(ret), RHS::Captured(v));
                ret
            }
        }
    }

    #[inline(always)]
    fn fresh_var(&mut self) -> Var {
        self.scopes.fresh_var()
    }

    #[inline(always)]
    fn register_def(&mut self, var : Var, cfg : CFG) {
        self.defs.insert(var, cfg);
    }

    #[inline(always)]
    fn add_stat(&mut self, stat : Stat) {
        self.cur_cfg.add_stat(stat);
    }

    #[inline(always)]
    fn assign(&mut self, lhs : LHS, rhs : RHS) {
        self.add_stat(Stat::Assign(lhs, rhs));
    }

    #[inline(always)]
    fn multiassign(&mut self, mut lhss : Vec<LHS>, rhss : Vec<RHS>) {
        let n_lhss = lhss.len();
        let n_rhss = rhss.len();

        if n_lhss > n_rhss {
            // last assignment will be a multi assign
            // TODO: remove clone()s
            if n_rhss > 0 {
                for i in 0 .. n_rhss - 1 {
                    self.assign(lhss[i].clone(), rhss[i].clone());
                }
                // last one is a multi assign
                for _ in lhss.drain(0 .. n_rhss - 1) {} // ugh
                self.add_stat(Stat::MultiAssign(lhss, rhss[rhss.len() - 1].clone()));
            }
        } else {
            for (lhs, rhs) in lhss.into_iter().zip(rhss.into_iter()) {
                self.assign(lhs, rhs);
            }
        }
    }

    #[inline(always)]
    fn enter_loop(&mut self, cont : BasicBlock) {
        self.loop_conts.push(cont);
    }

    #[inline(always)]
    fn exit_loop(&mut self) {
        self.loop_conts.pop().unwrap();
    }

    #[inline(always)]
    fn terminate(&mut self, bb : BasicBlock) {
        self.cur_cfg.terminate(Terminator::Jmp(bb));
    }

    #[inline(always)]
    fn cond_terminate(&mut self, cond : Var, then_bb : BasicBlock, else_bb : BasicBlock) {
        self.cur_cfg.terminate(Terminator::CondJmp(cond, then_bb, else_bb));
    }

    #[inline(always)]
    fn ret(&mut self, exps : Vec<Var>) {
        self.cur_cfg.terminate(Terminator::Ret(exps));
    }

    #[inline(always)]
    fn new_bb(&mut self) -> BasicBlock {
        self.cur_cfg.new_bb()
    }

    #[inline(always)]
    fn set_bb(&mut self, bb : BasicBlock) {
        self.cur_cfg.set_bb(bb)
    }

    fn emit_string(&mut self, s : String) -> Var {
        let ret = self.fresh_var();
        self.assign(LHS::Var(ret), RHS::String(s));
        ret
    }

    fn emit_number(&mut self, num : Number) -> Var {
        let ret = self.fresh_var();
        self.assign(LHS::Var(ret), RHS::Number(num));
        ret
    }

    fn emit_nil(&mut self) -> Var {
        let ret = self.fresh_var();
        self.assign(LHS::Var(ret), RHS::Nil);
        ret
    }

    fn emit_bool(&mut self, b : bool) -> Var {
        let ret = self.fresh_var();
        self.assign(LHS::Var(ret), RHS::Bool(b));
        ret
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Scoping
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {
    fn enter_scope(&mut self) {
        self.scopes.enter();
        self.labels.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.labels.exit_scope();
        self.scopes.exit();
    }

    fn enter_closure(&mut self, args: Vec<String>) -> Vec<Var> {
        self.labels.enter_closure();
        self.scopes.enter_closure(args)
    }

    fn exit_closure(&mut self) -> HashSet<Var> {
        self.labels.exit_closure();
        self.scopes.exit_closure()
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////////////////////////

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
    fn cond_then(&mut self, then_bb : BasicBlock, else_bb : BasicBlock, cont_bb : BasicBlock) {
        let cond = self.exp();
        self.expect_tok(Tok::Then);
        self.cond_terminate(cond, then_bb, else_bb);

        // then block jumps to the continuation
        self.set_bb(then_bb);
        self.block();
        self.terminate(cont_bb);
    }

    /// <condition> do <block> end
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
        self.enter_loop(cont_bb);
        let cond = self.exp();
        self.cond_terminate(cond, body_bb, cont_bb);

        self.expect_tok(Tok::Do);
        self.set_bb(body_bb);
        self.block();
        self.terminate(cond_bb); // loop
        self.expect_tok(Tok::End);

        self.exit_loop();
        self.set_bb(cont_bb);
    }

    // <block> end
    fn dostat(&mut self) {
        self.block();
        self.expect_tok(Tok::End);
    }

    // <fornum | forlist> end
    fn forstat(&mut self) {
        let var1 = self.name();
        match self.cur_tok_() {
            &Tok::Assign => { self.skip(); self.fornum(var1) },
            &Tok::Comma | &Tok::In => self.forlist(var1),
            _ => panic!("Syntax error in for statement"),
        }
    }

    // exp1, exp2 [,exp3] <forbody>
    fn fornum(&mut self, cond_var : Id) {
        self.enter_scope();
        let cond_var = self.scopes.var_decl(cond_var);

        // evaluate start, end, and step expressions
        let start = self.exp();
        self.expect_tok(Tok::Comma);
        let end = self.exp();
        let step = {
            if self.cur_tok_() == &Tok::Comma {
                self.skip();
                self.exp()
            } else {
                self.emit_number(Number::Int(1))
            }
        };

        // bb that checks the condition
        let cond_bb = self.new_bb();
        // bb for the body
        let body_bb = self.new_bb();
        // bb for the continuation
        let cont_bb = self.new_bb();

        // start with the condition
        self.terminate(cond_bb);
        self.set_bb(cond_bb);
        self.enter_loop(cont_bb);
        self.assign(LHS::Var(cond_var), RHS::Binop(start, Binop::LT, end));
        self.cond_terminate(cond_var, body_bb, cont_bb);

        // body
        self.set_bb(body_bb);
        self.expect_tok(Tok::Do);
        self.block_();
        self.expect_tok(Tok::End);
        // update the counter
        self.assign(LHS::Var(start), RHS::Binop(start, Binop::Add, step));
        // loop
        self.terminate(cond_bb);

        self.exit_scope();
        self.exit_loop();

        self.set_bb(cont_bb);
    }

    // <name> {,<name>} in <explist> <forbody>
    fn forlist(&mut self, var1 : Id) {

        // From the user manual:
        //
        //   A for statement like
        //
        //        for var_1, ···, var_n in explist do block end
        //
        //   is equivalent to the code:
        //
        //        do
        //          local f, s, var = explist
        //          while true do
        //            local var_1, ···, var_n = f(s, var)
        //            if var_1 == nil then break end
        //            var = var_1
        //            block
        //          end
        //        end

        self.enter_scope();

        let mut vars = vec![LHS::Var(self.scopes.var_decl(var1))];
        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // skip ,
            let var_name = self.name();
            vars.push(LHS::Var(self.scopes.var_decl(var_name)));
        }

        self.expect_tok(Tok::In);

        // this evaluates exps in the explist which is what we want
        let mut exps = vec![self.exp()];
        while self.cur_tok_() == &Tok::Comma {
            exps.push(self.exp());
        }

        // iterator function
        let f_var = self.fresh_var();
        // TODO: initial state?
        let s_var = self.fresh_var();
        // TODO: not sure what this stands for..
        let var = self.fresh_var();

        let mut exps : &mut [Var] = &mut exps;

        //
        // stupid borrow checker strikes again below
        //

        // try to bind f_var
        let exps = {
            if !exps.is_empty() {
                self.assign(LHS::Var(f_var), RHS::Var(exps[0]));
                &mut exps[1..]
            } else {
                exps
            }
        };

        // try to bind s_var
        let exps = {
            if !exps.is_empty() {
                self.assign(LHS::Var(s_var), RHS::Var(exps[0]));
                &mut exps[1..]
            } else {
                exps
            }
        };

        // try to bind var
        if !exps.is_empty() {
            self.assign(LHS::Var(var), RHS::Var(exps[0]));
        }
        // rest of the exps are ignored as they're already evaluated at this point

        let body_bb = self.new_bb();
        let cont_bb = self.new_bb();

        // jump to the loop body
        self.terminate(body_bb);
        self.set_bb(body_bb);
        self.enter_loop(cont_bb);
        self.multiassign(vars, vec![RHS::FunCall(f_var, vec![s_var, var])]);


        self.expect_tok(Tok::Do);
        self.block_();
        self.expect_tok(Tok::End);

        self.exit_scope();
        self.exit_loop();

        self.set_bb(cont_bb);
    }

    fn repeatstat(&mut self) {
        // beginning of the loop
        let begin_bb = self.new_bb();
        // continuation
        let cont_bb = self.new_bb();

        // jump to the loop
        self.terminate(begin_bb);
        self.enter_scope();

        self.enter_loop(cont_bb);
        self.block_();
        self.expect_tok(Tok::Until);

        let cond = self.exp();
        self.cond_terminate(cond, begin_bb, cont_bb);

        self.exit_scope();
        self.exit_loop();

        self.set_bb(cont_bb);
    }

    fn init_closure(&mut self, fun : Var, captures : Vec<Var>) -> Var {
        // initialize the table (closure)
        let table_var = self.fresh_var();
        self.assign(LHS::Var(table_var), RHS::NewTbl);

        // write the function to the table
        let fun_idx_var = self.fresh_var();
        self.assign(LHS::Var(fun_idx_var), RHS::Number(Number::Int(0)));
        self.assign(LHS::Tbl(table_var, fun_idx_var), RHS::Var(fun));

        // write captured values to the table
        for (capture_idx, captured) in captures.into_iter().enumerate() {
            let capture_idx_var = self.fresh_var();
            self.assign(LHS::Var(capture_idx_var), RHS::Number(Number::Int((capture_idx + 1) as i64)));
            self.assign(LHS::Tbl(table_var, capture_idx_var), RHS::Var(captured));
        }

        table_var
    }

    fn funcstat(&mut self) {
        let (n, mut sels, mname) = self.funcname();

        let (fun_var, captures) = {
            // parse function body. register the CFG as a new definition.
            let fun_cfg = {
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
            let fun_captures = fun_cfg.get_captures();
            self.register_def(fun_var, fun_cfg);
            (fun_var, fun_captures)
        };

        let clo = self.init_closure(fun_var, captures);

        // write the closure to its final location
        // TODO: move strings from 'self' to atoms somehow
        let mut lhs : LHS = self.var_lhs_use(&n);
        for sel in sels {
            lhs = {
                let sel_var = self.emit_string(sel);
                let lhs_var = self.lhs_to_var(lhs);
                LHS::Tbl(lhs_var, sel_var)
            };
        }

        self.assign(lhs, RHS::Var(clo));
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
        let fname = self.name();
        let fun_cfg = self.fundef(false); // is_method = false
        let fun_var = self.fresh_var();
        let fun_captures = fun_cfg.get_captures();
        self.register_def(fun_var, fun_cfg);

        let clo = self.init_closure(fun_var, fun_captures);
        let lhs = self.var_lhs_use(&fname);
        self.assign(lhs, RHS::Var(clo));
    }

    // <namelist> [= <explist>]
    fn localstat(&mut self) {
        let mut lhss = vec![];
        // there should be at least one name
        {
            let name = self.name();
            let var = self.scopes.var_decl(name);
            lhss.push(LHS::Var(var));
        }

        while self.cur_tok_() == &Tok::Comma {
            self.skip(); // consume ,
            let name = self.name();
            let var = self.scopes.var_decl(name);
            lhss.push(LHS::Var(var));
        }

        let mut rhss = vec![];
        if self.cur_tok_() == &Tok::Assign {
            self.skip(); // consume =
            rhss.push(RHS::Var(self.exp()));
            while self.cur_tok_() == &Tok::Comma {
                self.skip(); // consume ,
                rhss.push(RHS::Var(self.exp()));
            }
        }

        self.multiassign(lhss, rhss);
    }

    fn labelstat(&mut self) {
        let label = self.name();
        self.expect_tok(Tok::DColon);
        let label_bb = self.new_bb();
        self.terminate(label_bb);
        self.set_bb(label_bb);
        self.labels.label(label, label_bb, &mut self.cur_cfg);
    }

    fn gotostat(&mut self) {
        let lbl = self.name();
        self.labels.goto(lbl, self.cur_cfg.cur_bb(), &mut self.cur_cfg);
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

    fn breakstat(&mut self) {
        if self.loop_conts.len() == 0 {
            panic!("break: not in loop");
        }
        let jmp_target = self.loop_conts[self.loop_conts.len() - 1];
        // statements after break should not be added to the current bb
        let temp_bb = self.new_bb();
        self.terminate(jmp_target);
        self.set_bb(temp_bb);
    }

    // function call or assignment. Both start with a <suffixedexp>.
    fn exprstat(&mut self) {
        match self.suffixedexp_stat() {
            Either::Left(lhs) => {
                // assignment
                let mut lhss = vec![lhs];
                while self.cur_tok_() == &Tok::Comma {
                    self.skip(); // skip ,
                    match self.suffixedexp_stat() {
                        Either::Left(lhs) => lhss.push(lhs),
                        Either::Right(_) => panic!("exprstat"),
                    }
                }
                // not a "local" assignment, so a '= <exp>' has to follow
                self.expect_tok(Tok::Assign);
                let mut rhss = vec![RHS::Var(self.exp())];
                while self.cur_tok_() == &Tok::Comma {
                    self.skip(); // skip ,
                    rhss.push(RHS::Var(self.exp()));
                }

                self.multiassign(lhss, rhss);
            },
            Either::Right(_) => {
                // function call, nothing to do
            },
        }
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

    fn exp(&mut self) -> Var {
        self.exp_(0)
    }

    fn exp_(&mut self, current_prec : Prec) -> Var {
        let mut e1 = match self.unop() {
            None =>
                self.simpleexp(),
            Some(unop) => {
                self.skip(); // skip op
                let e = self.exp_(UNOP_PREC);
                let ret = self.fresh_var();
                self.assign(LHS::Var(ret), RHS::Unop(unop, e));
                ret
            },
        };

        // mmm not sure if the precedence helps with the evaluation order

        while let Some(op) = self.binop() {
            let op_prec = binop_prec(op);

            if current_prec > op_prec.left { break; }

            self.skip(); // skip op
            let e2 = self.exp_(op_prec.right);
            let ret = self.fresh_var();
            self.assign(LHS::Var(ret), RHS::Binop(e1, op, e2));
            e1 = ret;
        }

        e1
    }

    fn simpleexp(&mut self) -> Var {
        match self.cur_tok() {
            Tok::Num(n) => { self.skip(); self.emit_number(n) },
            Tok::SLit(s) => { self.skip(); self.emit_string(s) },
            Tok::Nil => { self.skip(); self.emit_nil() },
            Tok::True => { self.skip(); self.emit_bool(true) }
            Tok::False => { self.skip(); self.emit_bool(false) }
            // Tok::Ellipsis => { panic!("... is not yet supported ") }
            Tok::Ellipsis => { self.skip(); self.fresh_var() } // TODO
            Tok::LBrace => { self.skip(); self.constructor() },
            Tok::Function => {
                self.skip(); // skip function
                let fun_cfg = self.fundef(false); // is_method = false
                let fun_var = self.fresh_var();
                let fun_captures = fun_cfg.get_captures();
                self.register_def(fun_var, fun_cfg);
                self.init_closure(fun_var, fun_captures)
            },
            _ =>
                self.suffixedexp_exp(),
        }
    }

    // [fieldlist] '}'
    fn constructor(&mut self) -> Var {
        let ret = self.fresh_var();
        self.assign(LHS::Var(ret), RHS::NewTbl);

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
        ret
    }

    /// true -> field with implicit key
    fn field(&mut self, tbl : Var, pos : i64) -> bool {
        match &self.ts[self.pos] {
            &Tok::LBracket => {
                self.skip(); // skip [
                let e1 = self.exp();
                self.expect_tok(Tok::RBracket);
                self.expect_tok(Tok::Assign);
                let e2 = self.exp();
                self.assign(LHS::Tbl(tbl, e1), RHS::Var(e2));
                false
            },
            &Tok::Ident(ref n) if &self.ts[self.pos + 1] == &Tok::Assign => {
                self.skip(); // skip identifier
                self.skip(); // skip =
                let e2 = self.exp();
                let sel = self.emit_string(n.to_owned());
                self.assign(LHS::Tbl(tbl, sel), RHS::Var(e2));
                false
            },
            _ => {
                let e = self.exp();
                let sel = self.emit_number(Number::Int(pos));
                self.assign(LHS::Tbl(tbl, sel), RHS::Var(e));
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
    fn suffixedexp_exp(&mut self) -> Var {
        let mut e0 = {
            let lhs_or_rhs = self.exp0();
            self.lhs_var_to_var(lhs_or_rhs)
        };

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
                    let sel = self.emit_string(name);
                    self.assign(LHS::Var(e1), RHS::ReadTbl(e0, sel));
                    e0 = e1;
                },
                &Tok::LBracket => {
                    // field selection
                    self.skip(); // consume [
                    let field = self.exp();
                    self.expect_tok(Tok::RBracket);
                    let e1 = self.fresh_var();
                    self.assign(LHS::Var(e1), RHS::ReadTbl(e0, field));
                    e0 = e1;
                },
                _ => { break; }
            }
        }

        e0
    }

    fn lhs_var_to_var(&mut self, lhs_rhs : Either<LHS, Var>) -> Var {
        match lhs_rhs {
            Either::Left(lhs) => self.lhs_to_var(lhs),
            Either::Right(var) => var,
        }
    }

    /// Parse a single assignment LHS or function call.
    fn suffixedexp_stat(&mut self) -> Either<LHS, Var> {
        let mut last : Either<LHS, Var> = self.exp0();

        loop {
            match unsafe { self.ts.get_unchecked(self.pos) } {
                &Tok::LParen | &Tok::SLit(_) | &Tok::LBrace | &Tok::Colon => {
                    last = {
                        let fun = self.lhs_var_to_var(last);
                        Either::Right(self.funcall(fun))
                    };
                },
                &Tok::Dot => {
                    last = {
                        // field selection
                        let tbl = self.lhs_var_to_var(last);
                        self.skip(); // consume .
                        let name = self.name();
                        let sel = self.emit_string(name);
                        Either::Left(LHS::Tbl(tbl, sel))
                    };
                },
                &Tok::LBracket => {
                    // field selection
                    last = {
                        self.skip(); // consume [
                        let tbl = self.lhs_var_to_var(last);
                        let sel = self.exp();
                        self.expect_tok(Tok::RBracket);
                        Either::Left(LHS::Tbl(tbl, sel))
                    };
                },
                _ => { break; },
            }
        }

        last
    }

    /// name | '(' exp ')'
    fn exp0(&mut self) -> Either<LHS, Var> {
        match &self.ts[self.pos] {
            &Tok::Ident(ref s) => {
                self.skip(); // skip ident
                Either::Left(self.var_lhs_use(s))
            },
            &Tok::LParen => {
                self.skip(); // skip (
                let exp = self.exp();
                self.expect_tok(Tok::RParen);
                Either::Right(exp)
            },
            tok =>
                panic!("exp0: Unexpected token: {:?}", tok),
        }
    }

    // function or method call.
    // <funargs> | : <name> <funargs>
    fn funcall(&mut self, f : Var) -> Var {
        match self.cur_tok_() {
            &Tok::Colon => {
                self.skip(); // skip :
                let mname = self.name();
                let mut args = self.funargs();
                // binder for the function to call
                let fun_var = self.fresh_var();
                // binder for the result
                let ret_var = self.fresh_var();
                let meth_sel = self.emit_string(mname);
                self.assign(LHS::Var(fun_var), RHS::ReadTbl(f, meth_sel));
                args.insert(0, fun_var);
                self.assign(LHS::Var(ret_var), RHS::FunCall(fun_var, args));
                ret_var
            },
            _ => {
                let mut args = self.funargs();
                args.insert(0, f.clone());
                // binder for the result
                let ret_var = self.fresh_var();
                self.assign(LHS::Var(ret_var), RHS::FunCall(f, args));
                ret_var
            },
        }
    }

    fn funargs(&mut self) -> Vec<Var> {
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
                args.push(self.emit_string(str));
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
    fn fundef(&mut self, is_method : bool) -> CFG {
        // TODO: Bind "self"

        // collect args
        self.expect_tok(Tok::LParen);
        let mut args : Vec<String> = Vec::new();
        if self.cur_tok_() != &Tok::RParen {
            if self.cur_tok_() == &Tok::Ellipsis {
                self.skip(); // skip ...
            } else {
                // first arg
                args.push(self.name());
                // rest of the args
                while self.cur_tok_() == &Tok::Comma {
                    self.skip(); // skip ,
                    if self.cur_tok_() == &Tok::Ellipsis {
                        self.skip(); // skip ...
                        // we should break here as vararg has to be the last arg
                        break;
                    } else {
                        args.push(self.name());
                    }
                }
            }
        }
        self.expect_tok(Tok::RParen);

        let cfg_args = self.enter_closure(args);
        let mut fun_cfg = std::mem::replace(&mut self.cur_cfg, CFGBuilder::new(cfg_args));

        self.block_();
        std::mem::swap(&mut self.cur_cfg, &mut fun_cfg);

        self.expect_tok(Tok::End);

        let captures = self.exit_closure();
        fun_cfg.build(captures.into_iter().collect())
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

/*
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
*/

    #[bench]
    fn parser_bench(b : &mut Bencher) {

        // Read all Lua files, concatenate contents and parse as one big
        // program. 10878 lines in total.

        let lua = concat_lua_tests();
        let mut tokens = tokenize(&lua).unwrap();
        tokens.push(Tok::EOS);
        b.iter(|| {
            let mut parser = Parser::new(&tokens);
            parser.parse()
        });
    }
}
