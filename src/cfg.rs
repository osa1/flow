use bit_set::{BitSet};
use bit_set;

use std::collections::hash_map::Entry;
use std::collections::hash_set;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;

macro_rules! set {
    ( $( $x:expr ),* ) => {
        {
            let mut ret = HashSet::new();
            $(
                ret.insert($x);
            )*
            ret
        }
    };
}

// Basic blocks are kept abstract and only manipulated via the CFG they belong.
// Internally we don't use this type because we don't get required instances of
// u32 automatically (no GND).
#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct BasicBlock(usize);

pub static ENTRY_BLOCK : BasicBlock = BasicBlock(0);

pub struct CFG {
    /// INVARIANT: There's at least one basic block: First block is the entry.
    blocks: Vec<BasicBlock_>,
}

/// Variables.
#[derive(Copy, Clone)]
pub struct Var(u32); // a variable goes by its unique integer

#[derive(Debug)]
struct BasicBlock_ {
    /// Predecessors of this basic block in the CFG.
    preds: BitSet,

    /// Successors of this basic block in the CFG.
    succs: BitSet,

    /// Every path from S0 (entry) to this block needs to visit these.
    dominators: BitSet,

    /// Immediate dominator of the block. `ENTRY_BLOCK` doesn't have one. This
    /// gives the parent in the dominator tree. Since `ENTRY_BLOCK` is the root,
    /// it doesn't have an `immediate_dom`.
    immediate_dom: Option<usize>,

    /// Blocks that have this block as immediate dominator. This gives children
    /// in the dominator tree.
    dom_tree_children: BitSet,

    /// Dominance frontier of the block.
    dom_frontier: BitSet,

    /// Variables defined in this block.
    vars: HashSet<u32>,

    /// Phi nodes in this block.
    phis: HashSet<u32>,
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

impl CFG {
    pub fn new() -> CFG {
        let mut doms = BitSet::new();
        doms.insert(0);

        CFG {
            blocks: vec![BasicBlock_ {
                preds: BitSet::new(),
                succs: BitSet::new(),
                dominators: doms,
                immediate_dom: None,
                dom_tree_children: BitSet::new(),
                dom_frontier: BitSet::new(),
                vars: HashSet::new(),
                phis: HashSet::new(),
            }]
        }
    }

    pub fn new_block(&mut self) -> BasicBlock {
        let mut doms = BitSet::new();
        let ret = self.n_blocks();
        doms.insert(ret);

        self.blocks.push(BasicBlock_ {
            preds: BitSet::new(),
            succs: BitSet::new(),
            dominators: doms,
            immediate_dom: None,
            dom_tree_children: BitSet::new(),
            dom_frontier: BitSet::new(),
            vars: HashSet::new(),
            phis: HashSet::new(),
        });

        BasicBlock(ret)
    }

    /// Make `node1` a predecessor of `node2`. Also makes `node2` a successor of
    /// `node1`.
    pub fn mk_pred(&mut self, node1 : BasicBlock, node2 : BasicBlock) {
        self.blocks[node2.0].preds.insert(node1.0);
        self.blocks[node1.0].succs.insert(node2.0);
    }

    /// Make `node1` a successor of `node2`. Also makes `node2` a predecessor of
    /// `node1`.
    pub fn mk_succ(&mut self, node1 : BasicBlock, node2 : BasicBlock) {
        self.blocks[node2.0].succs.insert(node1.0);
        self.blocks[node1.0].preds.insert(node2.0);
    }

    /// Does `block1` dominate `block2` ?
    pub fn dominates(&self, block1 : BasicBlock, block2 : BasicBlock) -> bool {
        self.blocks[block2.0].dominators.contains(block1.0)
    }

    /// Predecessors of a given node.
    pub fn preds(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].preds.iter() }
    }

    /// Successors of a given node.
    pub fn succs(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].succs.iter() }
    }

    /// Dominators of a given node.
    ///
    /// If `d` is a dominator of `n`, every path from entry node to `n` needs to
    /// visit `d`.
    pub fn dominators(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dominators.iter() }
    }

    /// Immediate dominator of a block. It doesn't exist for `EntryNode` and
    /// thus it's an error to ask for immediate dominator of `EntryNode`.
    ///
    /// `d` is immediate dominator of `n` if it's a dominator of `n` and it
    /// doesn't dominate any other dominators of `n`.
    pub fn immediate_dom(&self, block : BasicBlock) -> BasicBlock {
        BasicBlock(self.blocks[block.0].immediate_dom.unwrap())
    }

    /// Children of the node in the dominator tree. TODO: Say more about what's
    /// a dominator tree.
    pub fn dom_tree_children(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dom_tree_children.iter() }
    }

    pub fn dom_frontier(&self, block : BasicBlock) -> BasicBlockIter {
        BasicBlockIter { bitset_iter: self.blocks[block.0].dom_frontier.iter() }
    }

    pub fn has_phi(&self, block : BasicBlock, var : Var) -> bool {
        self.blocks[block.0].phis.contains(&var.0)
    }

    pub fn insert_phi(&mut self, block : BasicBlock, var : Var) {
        self.blocks[block.0].phis.insert(var.0);
    }

    /// Compute dominators, dominator tree, dominance frontier etc. DO NOT
    /// query those without calling this first.
    // TODO: We probably need a `CFGBuilder` type that returns an immutable
    // `CFG` type when built.
    pub fn build(&mut self) {
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

impl CFG {
    /// Compute dominators.
    fn compute_doms(&mut self) {
        let n_blocks = self.n_blocks();

        // Initialization: All blocks other than the entry are initialized as
        // dominated by all blocks.
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
        // In words, every node dominates itself, and no other node domintes the
        // entry.
        //
        // For non-entry node `n`, if `m` dominates all of its predecessors then
        // it dominates `m`. (remember that every node dominates itself, so it
        // works when `m` is a predecessor)
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

        // Idea: Say we have `n` dominators. One of them should be the last one
        // to visit when moving from entry to this node. That one is the
        // immediate dominator.
        //
        // How to find it? Since it's last dominator to visit, it shouldn't
        // dominate any other dominators (Otherwise it wouldn't be last one to
        // visit). So just look at all the dominators, and find the one that
        // doesn't dominate any others.
        //
        // (is this the most efficient way though?)

        let n_blocks = self.n_blocks();

        for i in 1 .. n_blocks {
            let mut immediate_dom : Option<usize> = None;

            'outer: // ugh
            for dom_idx in self.dominators(BasicBlock(i)) {
                // Skip self
                if dom_idx.0 == i { continue; }

                // For all other dominators...
                for dom_idx_1 in self.dominators(BasicBlock(i)) {
                    if dom_idx == dom_idx_1 { continue; }
                    if dom_idx_1.0 == i { continue; }

                    if self.dominates(dom_idx, dom_idx_1) {
                        // it dominates some of the other dominators... skip
                        continue 'outer;
                    }
                }

                // it seems like we found a dominator that doesn't dominate any
                // of the other dominators.
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
        // TODO: I'm using a separate data structure to make borrow checker
        // happy. There should be a better way -- need to do post-order
        // traversal while mutating `self`.

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
        // definition sites of variables
        let mut defsites : HashMap<u32, HashSet<BasicBlock>> = HashMap::new();

        // all variables defined in the cfg
        let mut vars : HashSet<u32> = HashSet::new();

        for block in 0 .. self.n_blocks() {
            for var in self.blocks[block].vars.iter().cloned() {
                vars.insert(var);

                match defsites.entry(var) {
                    Entry::Vacant(entry) => {
                        entry.insert(set!(BasicBlock(block)));
                    },
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(BasicBlock(block));
                    }
                }
            }
        }

        for var_ in vars.iter().cloned() {
            let var = Var(var_);

            // whenever node x contains a definition of some variable a, any
            // node in the dominance frontier of x needs a phi for a
            let mut workset = defsites.get(&var_).unwrap().clone();

            while let Some(work) = { let v = workset.drain().next(); v } {
                // redundant clone() here -- to make borrow checker happy
                for dom_ in self.blocks[work.0].dom_frontier.clone().iter() {
                    let dom = BasicBlock(dom_);
                    if !self.has_phi(dom, var) {
                        self.insert_phi(dom, var);
                        // now that `dom` has a new definition, we need to work
                        // it, to be able to update its dominance frontier
                        if !defsites.get(&var_).unwrap().contains(&dom) {
                            workset.insert(dom);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashSet;

    #[test]
    fn dom_test_1() {
        let mut cfg = CFG::new();
        let block1 = cfg.new_block();
        let block2 = cfg.new_block();

        cfg.mk_pred(ENTRY_BLOCK, block1);
        cfg.mk_pred(block1, block2);

        cfg.build();

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
        let mut cfg = CFG::new();
        let b1  = cfg.new_block();
        let b2  = cfg.new_block();
        let b3  = cfg.new_block();
        let b4  = cfg.new_block();
        let b5  = cfg.new_block();
        let b6  = cfg.new_block();
        let b7  = cfg.new_block();
        let b8  = cfg.new_block();
        let b9  = cfg.new_block();
        let b10 = cfg.new_block();
        let b11 = cfg.new_block();
        let b12 = cfg.new_block();

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

        cfg.build();

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
        let mut cfg = CFG::new();
        let b1  = cfg.new_block();
        let b2  = cfg.new_block();
        let b3  = cfg.new_block();
        let b4  = cfg.new_block();
        let b5  = cfg.new_block();
        let b6  = cfg.new_block();
        let b7  = cfg.new_block();

        cfg.mk_pred(ENTRY_BLOCK, b1);
        cfg.mk_pred(b1, b2);
        cfg.mk_pred(b2, b3);
        cfg.mk_pred(b2, b4);
        cfg.mk_pred(b3, b5);
        cfg.mk_pred(b3, b6);
        cfg.mk_pred(b5, b7);
        cfg.mk_pred(b6, b7);
        cfg.mk_pred(b7, b2);

        cfg.build();

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
}
