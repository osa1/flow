use bit_set::{BitSet};
use bit_set;

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;
use std::iter::Iterator;
use std::mem;

use ast;
use stat::{Stat, LHS, RHS};
use uniq::{Uniq, VARARG_UNIQ};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Variables in CFGs are just Uniqs. Keep a symbol table if you need to attach more information to
/// the variables.
pub type Var = Uniq;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Phantom types for open/closed control-flow graphs.
////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Open {
    /// Current basic block. New statemets are added here. Initially this is the entry block.
    cur_bb: BasicBlock,
}

pub struct Closed {}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CFG<A> {
    /// INVARIANT: There's at least one basic block: ENTRY_BLOCK.
    blocks: Vec<BasicBlock_>,

    /// Arguments of the function.
    args: Vec<Var>,

    /// Captured variables.
    captures: HashSet<Var>,

    /// All variables mentioned in the CFG.
    vars: HashSet<Var>,

    /// Definition sites of variables.
    defsites: HashMap<Var, HashSet<BasicBlock>>,

    /// This is either `Open` or `Closed`. `Open` means new statements and basic blocks can be
    /// added. `Closed` means no additions from the outside.
    open_closed: A,
}

/// New basic blocks and statements can be added by the user.
pub type OpenCFG = CFG<Open>;

/// No additions from the outside.
pub type ClosedCFG = CFG<Closed>;

#[derive(Debug)]
struct BasicBlock_ {
    /// Statements of this basic block.
    stats: Vec<Stat>,

    /// What to do after the last statement in a basic block.
    term: Terminator,

    /// Predecessors of this basic block in the CFG.
    preds: BitSet,

    /// Successors of this basic block in the CFG.
    succs: BitSet,

    /// Every path from S0 (entry) to this block needs to visit these.
    dominators: BitSet,

    /// Immediate dominator of the block. `ENTRY_BLOCK` doesn't have one. This gives the parent in
    /// the dominator tree. Since `ENTRY_BLOCK` is the root, it doesn't have an `immediate_dom`.
    immediate_dom: Option<usize>,

    /// Blocks that have this block as immediate dominator. This gives children in the dominator
    /// tree.
    dom_tree_children: BitSet,

    /// Dominance frontier of this block.
    dom_frontier: BitSet,

    /// Variables defined in this block.
    vars: HashSet<Var>,

    /// Phi nodes in this block.
    phis: HashSet<Var>,
}

// Basic blocks are kept abstract and only manipulated via the CFG they belong.
#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct BasicBlock(usize);

pub static ENTRY_BLOCK : BasicBlock = BasicBlock(0);

/// What to do after the last statement in a basic block.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Terminator {
    Jmp(BasicBlock),
    CondJmp(Var, BasicBlock, BasicBlock),
    Ret(Vec<Var>),
    // TODO: switch etc. for optimizations?

    /// This is used when we see a goto statement before seeing a label for it. All `Unknown`s
    /// should be removed before returning a `CFG` from `CFGBuilder`.
    Unknown,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl OpenCFG {
    pub fn new(args: Vec<Var>) -> CFG<Open> {
        let defsites = {
            let mut defsites = HashMap::new();
            for arg in args.iter() {
                let mut arg_set = HashSet::new();
                arg_set.insert(ENTRY_BLOCK);
                defsites.insert(*arg, arg_set);
            }
            defsites
        };
        CFG {
            blocks: vec![BasicBlock_::mk_entry_block()],
            args: args,
            captures: HashSet::new(),
            vars: HashSet::new(),
            defsites: defsites,
            open_closed: Open { cur_bb: ENTRY_BLOCK },
        }
    }

    /// Add a statement to the current basic block.
    pub fn add_stat(&mut self, stat : Stat) {
        // Take a peek at the statement to update defsites etc.
        match &stat {
            &Stat::Assign(ref lhs, ref rhs) => {
                self.process_stat_lhs(lhs);
                self.process_stat_rhs(rhs);
            },
            &Stat::MultiAssign(ref lhss, ref rhs) => {
                for lhs in lhss {
                    self.process_stat_lhs(lhs);
                }
                self.process_stat_rhs(rhs);
            },
        }

        unsafe { self.blocks.get_unchecked_mut(self.open_closed.cur_bb.0) }.stats.push(stat);
    }

    /// Terminate current basic block with the given terminator.
    pub fn terminate(&mut self, term : Terminator) {
        let cur_bb = self.open_closed.cur_bb;
        self.terminate_bb(cur_bb, term);
    }

    /// Terminate a basic block with the given terminator.
    pub fn terminate_bb(&mut self, bb: BasicBlock, term: Terminator) {
        match &term {
            &Terminator::Jmp(dst) => {
                self.mk_pred(bb, dst);
            },
            &Terminator::CondJmp(_, dst1, dst2) => {
                self.mk_pred(bb, dst1);
                self.mk_pred(bb, dst2);
            },
            &Terminator::Ret(_) => {},
            &Terminator::Unknown => {},
        }
        unsafe { self.blocks.get_unchecked_mut(bb.0) }.term = term;
    }

    /// Create a fresh basic block.
    pub fn new_bb(&mut self) -> BasicBlock {
        let ret = BasicBlock(self.blocks.len());
        self.blocks.push(BasicBlock_::new());
        ret
    }

    /// Set current basic block.
    pub fn set_cur_bb(&mut self, bb : BasicBlock) {
        debug_assert!(bb.0 < self.blocks.len());
        self.open_closed.cur_bb = bb;
    }

    /// Get the current basic block.
    pub fn cur_bb(&self) -> BasicBlock {
        return self.open_closed.cur_bb;
    }

    /// Finalize the builder and generate a CFG.
    pub fn build(mut self, captures : HashSet<Var>) -> CFG<Closed> {
        self.build_();
        CFG {
            blocks: self.blocks,
            args: self.args,
            captures: captures,
            vars: self.vars,
            defsites: self.defsites,
            open_closed: Closed {},
        }
    }

    #[cfg(test)]
    pub fn bb_terminator(&self, bb: BasicBlock) -> Terminator {
        self.blocks[bb.0].term.clone()
    }

    /// Update variables and defsites.
    fn process_stat_lhs(&mut self, lhs: &LHS) {
        match lhs {
            &LHS::Tbl(v1, v2) => {
                self.vars.insert(v1);
                self.vars.insert(v2);
            },
            &LHS::Var(v) => {
                self.vars.insert(v);
                self.insert_defsite(v);
            },
            &LHS::Captured(_) => {},
            &LHS::Dynamic(_) => {}
        }
    }

    /// Add current basic block as a defsite for the given variable.
    fn insert_defsite(&mut self, var: Var) {
        match self.defsites.entry(var) {
            Entry::Vacant(entry) => {
                entry.insert(set!(self.open_closed.cur_bb));
            },
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(self.open_closed.cur_bb);
            },
        }
    }

    /// Update variables and defsites.
    fn process_stat_rhs(&mut self, rhs: &RHS) {
        match rhs {
            &RHS::Nil => {},
            &RHS::Var(v) => {
                self.vars.insert(v);
            },
            &RHS::Bool(_) => {},
            &RHS::Number(_) => {},
            &RHS::String(_) => {},
            &RHS::FunCall(fun, ref args) => {
                self.vars.insert(fun);
                for arg in args {
                    self.vars.insert(*arg);
                }
            },
            &RHS::NewTbl => {},
            &RHS::ReadTbl(v1, v2) => {
                self.vars.insert(v1);
                self.vars.insert(v2);
            },
            &RHS::Binop(v1, _, v2) => {
                self.vars.insert(v1);
                self.vars.insert(v2);
            },
            &RHS::Unop(_, v1) => {
                self.vars.insert(v1);
            },
            &RHS::Captured(_) => {},
            &RHS::Dynamic(_) => {},
            &RHS::Function(_) => {},
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl BasicBlock_ {
    pub fn new() -> BasicBlock_ {
        BasicBlock_ {
            stats: Vec::new(),
            term: Terminator::Ret(vec![]),
            preds: BitSet::new(),
            succs: BitSet::new(),
            dominators: BitSet::new(),
            immediate_dom: None,
            dom_tree_children: BitSet::new(),
            dom_frontier: BitSet::new(),
            vars: HashSet::new(),
            phis: HashSet::new(),
        }
    }

    pub fn mk_entry_block() -> BasicBlock_ {
        let mut bb = BasicBlock_::new();
        bb.dominators.insert(ENTRY_BLOCK.0);
        bb
    }
}

#[derive(Clone)]
pub struct BasicBlockIter<'b> {
    bitset_iter: bit_set::Iter<'b, u32>,
}

impl<'b> Iterator for BasicBlockIter<'b> {
    type Item = BasicBlock;

    fn next(&mut self) -> Option<BasicBlock> {
        self.bitset_iter.next().map(|b| BasicBlock(b))
    }
}

impl<A> CFG<A> {
    /// Get variables captured by the closure.
    pub fn get_captures(&self) -> Vec<Var> {
        self.captures.iter().cloned().collect()
    }

    /// Make `node1` a predecessor of `node2`. Also makes `node2` a successor of
    /// `node1`.
    fn mk_pred(&mut self, node1 : BasicBlock, node2 : BasicBlock) {
        self.blocks[node2.0].preds.insert(node1.0);
        self.blocks[node1.0].succs.insert(node2.0);
    }

    /// Does `block1` dominate `block2` ?
    fn dominates(&self, block1 : BasicBlock, block2 : BasicBlock) -> bool {
        self.blocks[block2.0].dominators.contains(block1.0)
    }

    /// Predecessors of a given node.
    fn preds(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].preds.iter() }
    }

    /// Successors of a given node.
    fn succs(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].succs.iter() }
    }

    /// Dominators of a given node.
    ///
    /// If `d` is a dominator of `n`, every path from entry node to `n` needs to
    /// visit `d`.
    fn dominators(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dominators.iter() }
    }

    /// Immediate dominator of a block. It doesn't exist for `EntryNode` and
    /// thus it's an error to ask for immediate dominator of `EntryNode`.
    ///
    /// `d` is immediate dominator of `n` if it's a dominator of `n` and it
    /// doesn't dominate any other dominators of `n`.
    fn immediate_dom(&self, block : BasicBlock) -> BasicBlock {
        BasicBlock(self.blocks[block.0].immediate_dom.unwrap())
    }

    /// Children of the node in the dominator tree. TODO: Say more about what's
    /// a dominator tree.
    fn dom_tree_children(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dom_tree_children.iter() }
    }

    #[cfg(test)]
    fn dom_frontier(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dom_frontier.iter() }
    }

    fn has_phi(&self, block : BasicBlock, var : Var) -> bool {
        self.blocks[block.0].  phis.contains(&var)
    }

    fn insert_phi(&mut self, block : BasicBlock, var : Var) {
        self.blocks[block.0].phis.insert(var);
    }

    #[cfg(test)]
    fn add_defined_var(&mut self, bb: BasicBlock, var: Var) {
        self.blocks[bb.0].vars.insert(var);
        self.vars.insert(var);
        match self.defsites.entry(var) {
            Entry::Vacant(entry) => {
                entry.insert(set!(bb));
            },
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(bb);
            },
        }
    }

    /// Compute dominators, dominator tree, dominance frontier etc. DO NOT
    /// query those without calling this first.
    // TODO: We probably need a `CFGBuilder` type that returns an immutable
    // `CFG` type when built.
    fn build_(&mut self) {
        self.compute_doms();
        self.compute_imm_doms();
        self.compute_df();
        self.insert_phis();
    }

    fn n_blocks(&self) -> usize {
        self.blocks.len()
    }
}

// Actual computation

impl<A> CFG<A> {
    /// Compute dominators.
    fn compute_doms(&mut self) {
        let n_blocks = self.n_blocks();

        // Initialization: All blocks other than the entry are initialized as dominated by all
        // blocks.
        {
            let initial_set : BitSet = (0 .. n_blocks).collect();
            for i in 1 .. n_blocks {
                self.blocks[i].dominators = initial_set.clone();
            }
        }

        // Iterate until fixpoint:
        //
        // * dom(S0) = {S0}
        // * dom(N)  = {N} \union (forall pred . intersect(dom(pred)))
        //
        // In words, every node dominates itself, and no other node domintes the entry.
        //
        // For non-entry node `n`, if `m` dominates all of its predecessors then it dominates `m`.
        // (remember that every node dominates itself, so it works when `m` is a predecessor)
        while self.compute_doms_step() {}
    }

    fn compute_doms_step(&mut self) -> bool {
        let mut changed = false;

        for block_idx in 1 .. self.n_blocks() {
            let mut new_doms = BitSet::new();

            // uhhh... Need a better way to intersect a collection of things
            let mut first = true;
            for pred in self.preds(BasicBlock(block_idx)) {
                if first {
                    first = false;
                    new_doms = self.blocks[pred.0].dominators.clone();
                } else {
                    new_doms = new_doms.intersection(&self.blocks[pred.0].dominators).collect();
                }
            }

            // every node dominates itself
            new_doms.insert(block_idx);

            if &new_doms != &self.blocks[block_idx].dominators {
                self.blocks[block_idx].dominators = new_doms;
                changed = true;
            }
        }

        changed
    }

    /// Compute immediate dominators.
    fn compute_imm_doms(&mut self) {

        // Idea: Say we have `n` dominators. One of them should be the last one to visit when
        // moving from entry to this node. That one is the immediate dominator.
        //
        // How to find it? Since it's the last dominator to visit, it shouldn't dominate any other
        // dominators (Otherwise it wouldn't be last one to visit). So just look at all the
        // dominators, and find the one that doesn't dominate any others.
        //
        // (is this the most efficient way though?)

        let n_blocks = self.n_blocks();

        for i in 0 .. n_blocks {
            let mut immediate_dom : Option<usize> = None;

            'outer: // ugh
            for dom_idx in self.dominators(BasicBlock(i)) {
                // Skip self
                if dom_idx.0 == i { continue; }

                // For all other dominators...
                for dom_idx_1 in self.dominators(BasicBlock(i)) {
                    // Skip self
                    if dom_idx_1.0 == i { continue; }
                    // For all _other_ dominators, so skip dom_idx
                    if dom_idx == dom_idx_1 { continue; }

                    if self.dominates(dom_idx, dom_idx_1) {
                        // it dominates some of the other dominators... skip
                        continue 'outer;
                    }
                }

                // it seems like we found a dominator that doesn't dominate any of the other
                // dominators.
                immediate_dom = Some(dom_idx.0);
                break;
            }

            self.blocks[i].immediate_dom = immediate_dom;
            if let Some(imm_dom) = immediate_dom {
                self.blocks[imm_dom].dom_tree_children.insert(i);
            }
        }
    }

    /// Compute dominance frontiers.
    fn compute_df(&mut self) {
        // TODO: I'm using a separate data structure to make borrow checker happy. There should be
        // a better way -- need to do post-order traversal while mutating `self`.

        let mut dfs : Vec<BitSet> = vec![BitSet::new(); self.n_blocks()];
        self.compute_df_(ENTRY_BLOCK, &mut dfs);

        for (b_idx, df) in dfs.into_iter().enumerate() {
            self.blocks[b_idx].dom_frontier = df;
        }
    }

    fn compute_df_(&self, block : BasicBlock, dfs : &mut Vec<BitSet>) {
        let mut s = BitSet::new();

        for succ in self.succs(block) {
            if self.immediate_dom(succ) != block {
                s.insert(succ.0);
            }
        }

        for dom_child in self.dom_tree_children(block) {
            self.compute_df_(dom_child, dfs);
            for w in dfs[dom_child.0].iter() {
                if !self.dominates(block, BasicBlock(w)) || block.0 == w {
                    s.insert(w);
                }
            }
        }

        dfs[block.0] = s;
    }

    /// Insert phi functions.
    fn insert_phis(&mut self) {
        // borrow checker problems...
        let vars = mem::replace(&mut self.vars, HashSet::new());
        let defsites = mem::replace(&mut self.defsites, HashMap::new());

        for var in vars.iter().cloned() {
            // whenever node x contains a definition of some variable a, any node in the dominance
            // frontier of x needs a phi for a

            // do not try to insert phi nodes for varargs: varargs are function arguments and
            // they're immutable
            if var == VARARG_UNIQ { continue; }

            // let mut next_workset = defsites.get(&var).unwrap().clone();
            let mut next_workset =
                match defsites.get(&var) {
                    None => {
                        for bb in self.blocks.iter() {
                            println!("{:?}", bb);
                        }

                        panic!("Var doesn't have a defsite: {:?}\n defsites: {:?}", var, defsites);
                    },
                    Some(defsites) => defsites.clone()
                };

            let mut workset : HashSet<BasicBlock> = HashSet::new();

            while !next_workset.is_empty() {
                mem::swap(&mut next_workset, &mut workset);

                for work in workset.drain() {
                    next_workset.remove(&work); // don't repeat work

                    // redundant clone() here -- to make borrow checker happy
                    for dom_ in self.blocks[work.0].dom_frontier.clone().iter() {
                        let dom = BasicBlock(dom_);
                        if !self.has_phi(dom, var) {
                            self.insert_phi(dom, var);
                            // now that `dom` has a new definition, we need to work it, to be able
                            // to update its dominance frontier
                            if !defsites.get(&var).unwrap().contains(&dom) {
                                next_workset.insert(dom);
                            }
                        }
                    }
                }
            }
        }

        // put borrowed stuff back
        self.vars = vars;
        self.defsites = defsites;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Printers
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<A> CFG<A> {
    pub fn print(&self, buf : &mut Vec<u8>) {
        write!(buf, "args: ").unwrap();
        let mut first = true;
        for arg in self.args.iter() {
            if !first { write!(buf, ", ").unwrap(); }
            first = false;
            write!(buf, "{:?}", arg).unwrap();
        }
        write!(buf, "\n").unwrap();

        // TODO: generalize comma-separated printing
        write!(buf, "captures: ").unwrap();
        let mut first = true;
        for arg in self.captures.iter() {
            if !first { write!(buf, ", ").unwrap(); }
            first = false;
            write!(buf, "{:?}", arg).unwrap();
        }
        write!(buf, "\n").unwrap();

        for (block_idx, block) in self.blocks.iter().enumerate() {
            write!(buf, "basic block {}:\n", block_idx).unwrap();
            block.print(buf);
            write!(buf, "\n\n").unwrap();
        }
    }
}

impl BasicBlock_ {
    pub fn print(&self, buf : &mut Vec<u8>) {
        writeln!(buf, "  phis: {:?}", self.phis).unwrap();
        for stat in self.stats.iter() {
            write!(buf, "  ").unwrap();
            stat.print(buf);
            write!(buf, "\n").unwrap();
        }
        write!(buf, "  ").unwrap();
        self.term.print(buf);
    }
}

impl BasicBlock {
    pub fn print(&self, buf : &mut Vec<u8>) {
        write!(buf, "{:?}", self.0).unwrap();
    }
}

impl Terminator {
    pub fn print(&self, buf : &mut Vec<u8>) {
        match self {
            &Terminator::Jmp(bb) => {
                write!(buf, "jump ").unwrap();
                bb.print(buf);
            },
            &Terminator::CondJmp(cond, then_bb, else_bb) => {
                write!(buf, "if {:?} then ", cond).unwrap();
                then_bb.print(buf);
                write!(buf, " else ").unwrap();
                else_bb.print(buf);
            },
            &Terminator::Ret(ref rets) => {
                write!(buf, "return ").unwrap();
                let mut first = true;
                for ret in rets {
                    if !first { write!(buf, ", ").unwrap(); }
                    first = false;
                    write!(buf, "{:?}", ret).unwrap();
                }
            },
            &Terminator::Unknown => {
                write!(buf, "<unknown>").unwrap();
            }
        }
    }
}

impl Stat {
    pub fn print(&self, buf : &mut Vec<u8>) {
        match self {
            &Stat::Assign(ref lhs, ref rhs) => {
                lhs.print(buf);
                write!(buf, " = ").unwrap();
                rhs.print(buf);
            },
            &Stat::MultiAssign(ref lhss, ref rhs) => {
                let mut first = true;
                for lhs in lhss {
                    if !first { write!(buf, ", ").unwrap(); }
                    first = false;
                    lhs.print(buf);
                }
                write!(buf, " = ").unwrap();
                rhs.print(buf);
            },
        }
    }
}

impl LHS {
    pub fn print(&self, buf : &mut Vec<u8>) {
        match self {
            &LHS::Tbl(var, ref sel) => {
                write!(buf, "{:?}[{:?}]", var, sel).unwrap();
            },
            &LHS::Var(var) => {
                write!(buf, "{:?}", var).unwrap();
            },
            &LHS::Captured(var) => {
                write!(buf, "env[{:?}]", var).unwrap();
            }
            &LHS::Dynamic(ref var) => {
                write!(buf, "dynenv[{:?}]", var).unwrap();
            }
        }
    }
}

impl RHS {
    pub fn print(&self, buf : &mut Vec<u8>) {
        match self {
            &RHS::Nil => { write!(buf, "nil").unwrap(); },
            &RHS::Var(var) => { write!(buf, "{:?}", var).unwrap(); },
            &RHS::Bool(b) => { write!(buf, "{}", if b { "true" } else { "false" }).unwrap(); },
            &RHS::Number(ref n) => { n.print(buf); },
            &RHS::String(ref s) => { write!(buf, "\"{}\"", s).unwrap(); },
            &RHS::FunCall(fun, ref args) => {
                write!(buf, "{:?}(", fun).unwrap();
                let mut first = true;
                for arg in args {
                    if !first { write!(buf, ", ").unwrap(); }
                    first = false;
                    write!(buf, "{:?}", arg).unwrap();
                }
                write!(buf, ")").unwrap();
            },
            &RHS::NewTbl => {
                write!(buf, "{{}}").unwrap();
            },
            &RHS::ReadTbl(tbl, sel) => {
                write!(buf, "{:?}[{:?}]", tbl, sel).unwrap();

            },
            &RHS::Binop(a1, op, a2) => {
                write!(buf, "{:?} ", a1).unwrap();
                op.print(buf);
                write!(buf, " {:?}", a2).unwrap();
            },
            &RHS::Unop(op, a) => {
                op.print(buf);
                write!(buf, "{:?}", a).unwrap();
            },
            &RHS::Captured(var) => {
                write!(buf, "env[{:?}]", var).unwrap();
            },
            &RHS::Dynamic(ref var) => {
                write!(buf, "dynenv[{:?}]", var).unwrap();
            },
            &RHS::Function(var) => {
                write!(buf, "function[{:?}]", var).unwrap();
            }
        }
    }
}

impl ast::Unop {
    pub fn print(&self, buf : &mut Vec<u8>) {
        let str = match *self {
            ast::Unop::Neg => "-",
            ast::Unop::Not => "not ",
            ast::Unop::Len => "#",
            ast::Unop::Complement => "~",
        };
        write!(buf, "{}", str).unwrap();
    }
}

impl ast::Binop {
    fn print(&self, buf : &mut Vec<u8>) {
        let str = match *self {
            ast::Binop::Add => "+",
            ast::Binop::Sub => "-",
            ast::Binop::Mul => "*",
            ast::Binop::Div => "/",
            ast::Binop::Exp => "^",
            ast::Binop::Mod => "%",
            ast::Binop::Concat => "..",
            ast::Binop::LT => "<",
            ast::Binop::LTE => "<=",
            ast::Binop::GT => ">",
            ast::Binop::GTE => ">=",
            ast::Binop::EQ => "==",
            ast::Binop::NEQ => "~=",
            ast::Binop::And => "and",
            ast::Binop::Or => "or",
            ast::Binop::IDiv => "//",
            ast::Binop::ShiftL => "<<",
            ast::Binop::ShiftR => ">>",
            ast::Binop::BAnd => "&&",
            ast::Binop::BOr => "||",
            ast::Binop::BXor => "~",
        };
        write!(buf, "{}", str).unwrap();
    }
}

impl ast::Number {
    pub fn print(&self, buf : &mut Vec<u8>) {
        match self {
            &ast::Number::Int(i) => { write!(buf, "{}", i).unwrap(); },
            &ast::Number::Float(f) => { write!(buf, "{}", f).unwrap(); },
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;
    use uniq::UniqCounter;

    use std::collections::HashSet;

    #[test]
    fn dom_test_1() {
        let mut cfg_builder = OpenCFG::new(vec![]);
        let block1 = cfg_builder.new_bb();
        let block2 = cfg_builder.new_bb();

        cfg_builder.terminate_bb(ENTRY_BLOCK, Terminator::Jmp(block1));
        cfg_builder.terminate_bb(block1, Terminator::Jmp(block2));

        let cfg = cfg_builder.build(HashSet::new());

        // dominators
        assert_eq!(cfg.dominators(ENTRY_BLOCK).collect::<HashSet<BasicBlock>>(),
                   set!(ENTRY_BLOCK));
        assert_eq!(cfg.dominators(block1).collect::<HashSet<BasicBlock>>(),
                   set!(ENTRY_BLOCK, block1));
        assert_eq!(cfg.dominators(block2).collect::<HashSet<BasicBlock>>(),
                   set!(ENTRY_BLOCK, block1, block2));

        // immediate dominators
        assert_eq!(cfg.immediate_dom(block1), ENTRY_BLOCK);
        assert_eq!(cfg.immediate_dom(block2), block1);
    }

    #[test]
    fn dom_test_2() {
        // Figure 18.3 (a)
        let mut cfg = OpenCFG::new(vec![]);
        let b1  = cfg.new_bb();
        let b2  = cfg.new_bb();
        let b3  = cfg.new_bb();
        let b4  = cfg.new_bb();
        let b5  = cfg.new_bb();
        let b6  = cfg.new_bb();
        let b7  = cfg.new_bb();
        let b8  = cfg.new_bb();
        let b9  = cfg.new_bb();
        let b10 = cfg.new_bb();
        let b11 = cfg.new_bb();
        let b12 = cfg.new_bb();

        cfg.mk_pred(ENTRY_BLOCK, b1);
        cfg.mk_pred(b1, b2);
        cfg.mk_pred(b2, b3);
        cfg.mk_pred(b3, b2);
        cfg.mk_pred(b2, b4);
        cfg.mk_pred(b4, b2);
        cfg.mk_pred(b4, b5);
        cfg.mk_pred(b4, b6);
        cfg.mk_pred(b5, b8);
        cfg.mk_pred(b5, b7);
        cfg.mk_pred(b6, b7);
        cfg.mk_pred(b8, b9);
        cfg.mk_pred(b9, b8);
        cfg.mk_pred(b9, b10);
        cfg.mk_pred(b10, b5);
        cfg.mk_pred(b7, b11);
        cfg.mk_pred(b11, b12);
        cfg.mk_pred(b10, b12);

        let cfg = cfg.build(HashSet::new());

        // Figure 18.3 (b)
        assert_eq!(cfg.immediate_dom(b1), ENTRY_BLOCK);
        assert_eq!(cfg.immediate_dom(b2), b1);
        assert_eq!(cfg.immediate_dom(b3), b2);
        assert_eq!(cfg.immediate_dom(b4), b2);
        assert_eq!(cfg.immediate_dom(b5), b4);
        assert_eq!(cfg.immediate_dom(b6), b4);
        assert_eq!(cfg.immediate_dom(b7), b4);
        assert_eq!(cfg.immediate_dom(b8), b5);
        assert_eq!(cfg.immediate_dom(b9), b8);
        assert_eq!(cfg.immediate_dom(b10), b9);
        assert_eq!(cfg.immediate_dom(b11), b7);
        assert_eq!(cfg.immediate_dom(b12), b4);
    }

    #[test]
    fn dom_test_3() {
        // Figure 19.4
        let mut cfg = OpenCFG::new(vec![]);
        let b1  = cfg.new_bb();
        let b2  = cfg.new_bb();
        let b3  = cfg.new_bb();
        let b4  = cfg.new_bb();
        let b5  = cfg.new_bb();
        let b6  = cfg.new_bb();
        let b7  = cfg.new_bb();

        cfg.mk_pred(ENTRY_BLOCK, b1);
        cfg.mk_pred(b1, b2);
        cfg.mk_pred(b2, b3);
        cfg.mk_pred(b2, b4);
        cfg.mk_pred(b3, b5);
        cfg.mk_pred(b3, b6);
        cfg.mk_pred(b5, b7);
        cfg.mk_pred(b6, b7);
        cfg.mk_pred(b7, b2);

        let cfg = cfg.build(HashSet::new());

        assert_eq!(cfg.immediate_dom(b1), ENTRY_BLOCK);
        assert_eq!(cfg.immediate_dom(b2), b1);
        assert_eq!(cfg.immediate_dom(b3), b2);
        assert_eq!(cfg.immediate_dom(b4), b2);
        assert_eq!(cfg.immediate_dom(b5), b3);
        assert_eq!(cfg.immediate_dom(b6), b3);
        assert_eq!(cfg.immediate_dom(b7), b3);

        assert_eq!(cfg.dom_frontier(b1).collect::<HashSet<BasicBlock>>(),
                   set!());
        assert_eq!(cfg.dom_frontier(b2).collect::<HashSet<BasicBlock>>(),
                   set!(b2));
        assert_eq!(cfg.dom_frontier(b3).collect::<HashSet<BasicBlock>>(),
                   set!(b2));
        assert_eq!(cfg.dom_frontier(b4).collect::<HashSet<BasicBlock>>(),
                   set!());
        assert_eq!(cfg.dom_frontier(b5).collect::<HashSet<BasicBlock>>(),
                   set!(b7));
        assert_eq!(cfg.dom_frontier(b6).collect::<HashSet<BasicBlock>>(),
                   set!(b7));
        assert_eq!(cfg.dom_frontier(b7).collect::<HashSet<BasicBlock>>(),
                   set!(b2));
    }

    #[test]
    fn phi_test_1() {
        // Figure 19.4
        let mut cfg = OpenCFG::new(vec![]);
        let b1  = cfg.new_bb();
        let b2  = cfg.new_bb();
        let b3  = cfg.new_bb();
        let b4  = cfg.new_bb();
        let b5  = cfg.new_bb();
        let b6  = cfg.new_bb();
        let b7  = cfg.new_bb();

        cfg.mk_pred(ENTRY_BLOCK, b1);
        cfg.mk_pred(b1, b2);
        cfg.mk_pred(b2, b3);
        cfg.mk_pred(b2, b4);
        cfg.mk_pred(b3, b5);
        cfg.mk_pred(b3, b6);
        cfg.mk_pred(b5, b7);
        cfg.mk_pred(b6, b7);
        cfg.mk_pred(b7, b2);

        let mut var_gen = UniqCounter::new(b't');
        let var_i = var_gen.fresh();
        let var_j = var_gen.fresh();
        let var_k = var_gen.fresh();

        cfg.add_defined_var(b1, var_i);
        cfg.add_defined_var(b1, var_j);
        cfg.add_defined_var(b1, var_k);
        cfg.add_defined_var(b5, var_j);
        cfg.add_defined_var(b5, var_k);
        cfg.add_defined_var(b6, var_j);
        cfg.add_defined_var(b6, var_k);

        let cfg = cfg.build(HashSet::new());

        assert!(cfg.has_phi(b2, var_j));
        assert!(cfg.has_phi(b2, var_k));
        assert!(cfg.has_phi(b7, var_j));
        assert!(cfg.has_phi(b7, var_k));
    }
}
