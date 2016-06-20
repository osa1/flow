#![feature(alloc_system)]
extern crate alloc_system;

////////////////////////////////////////////////////////////////////////////////

use std::collections::HashSet;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

pub struct CFG {
    /// First block is the entry.
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug)]
pub struct BasicBlock {
    /// Predecessors of this basic block in the CFG.
    preds: Vec<usize>,

    /// Successors of this basic block in the CFG.
    succs: Vec<usize>,

    /// Every path from S0 to this block needs to visit these. Initially
    /// contains the block itself. Call `generate_dominators()` of the CFG to
    /// update this.
    dominators: HashSet<usize>,

    /// Immediate dominator of the block. Call `generate_dominators()` to
    /// generate.
    immediate_dom: Option<usize>,
}

impl CFG {
    pub fn new() -> CFG {
        CFG {
            blocks: vec![BasicBlock {
                preds: vec![],
                succs: vec![],
                dominators: set!(0),
                immediate_dom: None,
            }]
        }
    }

    pub fn new_block(&mut self) -> usize {
        let ret = self.n_blocks();

        self.blocks.push(BasicBlock {
            preds: vec![],
            succs: vec![],
            dominators: set!(ret),
            immediate_dom: None,
        });

        ret
    }

    /// Make `node1` a predecessor of `node2`.
    pub fn mk_pred(&mut self, node1 : usize, node2 : usize) {
        self.blocks[node2].preds.push(node1);
        self.blocks[node1].succs.push(node2);
    }

    /// Make `node1` a successor of `node2`.
    pub fn mk_succ(&mut self, node1 : usize, node2 : usize) {
        self.blocks[node2].succs.push(node1);
        self.blocks[node1].preds.push(node2);
    }

    fn n_blocks(&self) -> usize {
        self.blocks.len()
    }

    /// Does `block1` dominate `block2` ?
    pub fn dominates(&self, block1 : usize, block2 : usize) -> bool {
        self.blocks[block2].dominators.contains(&block1)
    }

    pub fn immediate_dom(&self, block : usize) -> Option<usize> {
        self.blocks[block].immediate_dom
    }

    pub fn dominators(&self, block : usize) -> &HashSet<usize> {
        &self.blocks[block].dominators
    }

    pub fn print(&self) {
        for (block_idx, block) in self.blocks.iter().enumerate() {
            println!("[{}], preds: {:?}, succs: {:?}, doms: {:?} imm_dom: {:?}",
                     block_idx, block.preds, block.succs, block.dominators,
                     block.immediate_dom);
        }
    }
}

impl CFG {

    /// True -> there has been changes
    // TODO: Efficient way of getting intersection of a collection of HashSets?
    fn iterate_dominators(&mut self) -> bool {
        let mut changed = false;

        for block_idx in 1 .. self.n_blocks() {
            let mut new_doms = HashSet::new();

            {
                let preds = &self.blocks[block_idx].preds;

                let mut first = true;

                for pred in preds {
                    if first {
                        first = false;
                        new_doms = self.blocks[*pred].dominators.clone();
                    } else {
                        new_doms = new_doms.intersection(
                            &self.blocks[*pred].dominators).cloned().collect();
                    }
                }
            }

            new_doms.insert(block_idx); // insert self

            if &new_doms != &self.blocks[block_idx].dominators {
                self.blocks[block_idx].dominators = new_doms;
                changed = true;
            }
        }

        changed
    }

    pub fn generate_dominators(&mut self) {
        let n_blocks = self.n_blocks();

        // Initialization: All blocks other than the first one are initialized
        // as dominated by all blocks.
        {
            let initial_set : HashSet<usize> = (0 .. n_blocks).collect();
            for i in 1 .. n_blocks {
                self.blocks[i].dominators = initial_set.clone();
            }
        }

        // Do the iteration
        // self.print();
        while self.iterate_dominators() {
            // self.print();
        }

        // Generate immediate dominators
        for i in 1 .. n_blocks {
            let mut immediate_dom = None;

            'outer: // ugh
            for dom_idx in self.blocks[i].dominators.iter() {
                // Skip self
                if *dom_idx == i { continue; }

                // For all other dominators...
                for dom_idx_1 in self.blocks[i].dominators.iter() {
                    if dom_idx == dom_idx_1 { continue; }
                    if *dom_idx_1 == i { continue; }

                    if self.dominates(*dom_idx, *dom_idx_1) {
                        continue 'outer;
                    }
                }

                immediate_dom = Some(*dom_idx);
                break;
            }

            self.blocks[i].immediate_dom = immediate_dom;
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

        cfg.mk_pred(0, block1);
        cfg.mk_pred(block1, block2);

        cfg.generate_dominators();

        // dominators
        assert_eq!(cfg.dominators(0), &set!(0));
        assert_eq!(cfg.dominators(block1), &set!(0, block1));
        assert_eq!(cfg.dominators(block2), &set!(0, block1, block2));

        // immediate dominators
        assert_eq!(cfg.immediate_dom(0), None);
        assert_eq!(cfg.immediate_dom(block1), Some(0));
        assert_eq!(cfg.immediate_dom(block2), Some(1));
    }

    #[test]
    fn dom_test_2() {
        // Figure 18.3 (a)
        let mut cfg = CFG::new();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();
        let _ = cfg.new_block();

        cfg.mk_pred(0, 1);
        cfg.mk_pred(1, 2);
        cfg.mk_pred(2, 3);
        cfg.mk_pred(3, 2);
        cfg.mk_pred(2, 4);
        cfg.mk_pred(4, 2);
        cfg.mk_pred(4, 5);
        cfg.mk_pred(4, 6);
        cfg.mk_pred(5, 8);
        cfg.mk_pred(5, 7);
        cfg.mk_pred(6, 7);
        cfg.mk_pred(8, 9);
        cfg.mk_pred(9, 8);
        cfg.mk_pred(9, 10);
        cfg.mk_pred(10, 5);
        cfg.mk_pred(7, 11);
        cfg.mk_pred(11, 12);
        cfg.mk_pred(10, 12);

        cfg.generate_dominators();

        // Figure 18.3 (b)
        assert_eq!(cfg.immediate_dom(0), None);
        assert_eq!(cfg.immediate_dom(1), Some(0));
        assert_eq!(cfg.immediate_dom(2), Some(1));
        assert_eq!(cfg.immediate_dom(3), Some(2));
        assert_eq!(cfg.immediate_dom(4), Some(2));
        assert_eq!(cfg.immediate_dom(5), Some(4));
        assert_eq!(cfg.immediate_dom(6), Some(4));
        assert_eq!(cfg.immediate_dom(7), Some(4));
        assert_eq!(cfg.immediate_dom(8), Some(5));
        assert_eq!(cfg.immediate_dom(9), Some(8));
        assert_eq!(cfg.immediate_dom(10), Some(9));
        assert_eq!(cfg.immediate_dom(11), Some(7));
        assert_eq!(cfg.immediate_dom(12), Some(4));
    }
}

fn main() {}
