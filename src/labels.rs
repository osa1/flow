//! This module implements the algorithm described in `goto_notes.txt`.

use cfg::{BasicBlock, OpenCFG, Terminator};

use std::collections::HashMap;

pub type Label = String;

pub struct Labels {
    current_scope_depth: i32,

    labels: Vec<HashMap<Label, BasicBlock>>,

    // INVARIANT: These two vectors have the same length.

    // TODO: Why not one vector of enum?
    jumps_to_parent: Vec<Vec<JumpToParent>>,
    unknown_dests: Vec<Vec<UnknownJump>>,
}

/// A jump to a label in a parent scope.
#[derive(Debug)]
struct JumpToParent {
    /// The label.
    label: Label,

    /// The basic block that makes the jump.
    bb: BasicBlock,

    /// Scope depth at the jump site.
    scope_depth: i32,

    /// Scope depth at the destination.
    jump_target_depth: i32,
}

/// A jump to an unknown place.
#[derive(Debug)]
struct UnknownJump {
    /// The label.
    label: Label,

    /// The basic block that makes the jump.
    bb: BasicBlock,

    /// Scope depth at the jump site.
    scope_depth: i32,
}

impl Labels {
    pub fn new() -> Labels {
        Labels {
            current_scope_depth: 0,
            labels: vec![HashMap::new()],
            jumps_to_parent: vec![vec![]],
            unknown_dests: vec![vec![]],
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_scope_depth += 1;
        self.labels.push(HashMap::new());
        // pending gotos can't jump to a new scope, so we push empty sets
        self.jumps_to_parent.push(vec![]);
        self.unknown_dests.push(vec![]);
    }

    pub fn exit_scope(&mut self) {
        self.current_scope_depth -= 1;
        self.labels.pop().unwrap();
        // any unknown destination in this scope will need to be assigned a destination
        let unknown_dests = self.unknown_dests.pop().unwrap();
        self.unknown_dests.last_mut().unwrap().extend(unknown_dests.into_iter());
        // any parent jumps may need to be updated if a forward declaration in a closer scope is
        // found later
        let jumps_to_parents = self.jumps_to_parent.pop().unwrap();
        self.jumps_to_parent.last_mut().unwrap().extend(jumps_to_parents.into_iter());
    }

    pub fn enter_closure(&mut self) {
        self.enter_scope();
    }

    pub fn exit_closure(&mut self) {
        self.current_scope_depth -= 1;
        self.labels.pop().unwrap();
        let unknown_dests = self.unknown_dests.pop().unwrap();
        if !unknown_dests.is_empty() {
            // TODO: This shouldn't be a panic
            panic!("gotos without destinations on function exit: {:?}",
                   unknown_dests.into_iter().map(|jmp| jmp.label).collect::<Vec<Label>>());
        }
        self.jumps_to_parent.pop().unwrap();
    }

    pub fn goto(&mut self, lbl: Label, cur_bb: BasicBlock, cfg: &mut OpenCFG) -> Option<BasicBlock> {
        // First, check if we have this label defined in the current scope.
        match self.labels.last_mut().unwrap().get(&lbl) {
            Some(bb) => {
                // This is the final destination, we don't add the goto to any of the sets.
                cfg.terminate_bb(cur_bb, Terminator::Jmp(*bb));
                return Some(*bb);
            },
            None => { /* keep searching */ }
        }

        // Search parent scopes
        let mut parent_depth : i32 = self.current_scope_depth - 1;
        while parent_depth >= 0 {
            match self.labels[parent_depth as usize].get(&lbl) {
                Some(bb) => {
                    // Found a target, but we may need to update it if we see another one in a
                    // closer scope in the future (i.e. a forward jump).
                    self.jumps_to_parent.last_mut().unwrap().push(JumpToParent {
                        label: lbl,
                        bb: cur_bb,
                        scope_depth: self.current_scope_depth,
                        jump_target_depth: parent_depth,
                    });
                    cfg.terminate_bb(cur_bb, Terminator::Jmp(*bb));
                    return Some(*bb);
                },
                None => {
                    // look at parent of the parent
                    parent_depth -= 1;
                }
            }
        }

        // We don't know where to jump yet.
        cfg.terminate_bb(cur_bb, Terminator::Unknown);
        self.unknown_dests.last_mut().unwrap().push(UnknownJump {
            label: lbl,
            bb: cur_bb,
            scope_depth: self.current_scope_depth,
        });

        None
    }

    pub fn label(&mut self, lbl: Label, label_bb: BasicBlock, cfg: &mut OpenCFG) {
        // Was the label defined in the current scope before? Error if so.
        if self.labels.last().unwrap().get(&lbl).is_some() {
            panic!("Label was defined in the current scope before: {}", lbl);
        }

        // println!("jumps_to_parent: {:?}", self.jumps_to_parent);
        // println!("unknown_dests: {:?}", self.unknown_dests);

        // First, remove any jumps to unknown targets
        {
            let mut remove_unknown_dests = vec![];
            for (idx, unknown_dest) in self.unknown_dests.last_mut().unwrap().iter().enumerate() {
                if &unknown_dest.label == &lbl {
                    remove_unknown_dests.push(idx);
                    cfg.terminate_bb(unknown_dest.bb, Terminator::Jmp(label_bb));
                    if unknown_dest.scope_depth != self.current_scope_depth {
                        debug_assert!(self.current_scope_depth < unknown_dest.scope_depth);
                        // move this to `jumps_to_parent` set
                        self.jumps_to_parent.last_mut().unwrap().push(JumpToParent {
                            label: lbl.clone(),
                            bb: unknown_dest.bb,
                            scope_depth: unknown_dest.scope_depth,
                            jump_target_depth: self.current_scope_depth,
                        });
                    }
                }
            }

            let mut removed = 0;
            for rm in remove_unknown_dests.into_iter() {
                self.unknown_dests.last_mut().unwrap().remove(rm - removed);
                removed += 1;
            }
        }

        // Second, update jumps to parents if the current label is closer
        {
            for mut parent_jmp in self.jumps_to_parent.last_mut().unwrap().iter_mut() {
                debug_assert!(self.current_scope_depth <= parent_jmp.scope_depth);
                if &parent_jmp.label == &lbl && parent_jmp.jump_target_depth < self.current_scope_depth {
                    cfg.terminate_bb(parent_jmp.bb, Terminator::Jmp(label_bb));
                    parent_jmp.jump_target_depth = self.current_scope_depth;
                }
            }
        }

        // Add this label to the current scope for backwards jumps
        self.labels.last_mut().unwrap().insert(lbl, label_bb);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test_labels {
    use super::*;

    use cfg::{OpenCFG, Terminator};

    #[test]
    fn pgm1() {
        // ::x::
        // goto x
        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        labels.label("x".to_string(), x_bb, &mut cfg_builder);
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), Some(x_bb));
    }

    #[test]
    fn pgm2() {
        // goto x
        // ::x::

        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), None);
        labels.label("x".to_string(), x_bb, &mut cfg_builder);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Jmp(x_bb));
    }

    #[test]
    fn pgm3() {
        // ::x::
        // do
        //     goto x
        // end
        // goto x

        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        labels.label("x".to_string(), x_bb, &mut cfg_builder);
        labels.enter_scope();
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), Some(x_bb));
        labels.exit_scope();
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), Some(x_bb));
    }

    #[test]
    fn pgm4() {
        // do
        //     goto x
        // end
        // ::x::

        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        labels.enter_scope();
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), None);
        labels.exit_scope();

        labels.label("x".to_string(), x_bb, &mut cfg_builder);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Jmp(x_bb));
    }

    #[test]
    fn pgm5() {
        // ::x::
        // do
        //     do
        //         goto x
        //     end
        //     do
        //         ::x::
        //     end
        // end

        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x1_bb = cfg_builder.new_bb();
        let x2_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        labels.label("x".to_string(), x1_bb, &mut cfg_builder);
        labels.enter_scope();
        labels.enter_scope();
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), Some(x1_bb));
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Jmp(x1_bb));
        cfg_builder.set_cur_bb(cur_bb);
        labels.exit_scope();
        labels.enter_scope();
        labels.label("x".to_string(), x2_bb, &mut cfg_builder);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Jmp(x1_bb));
        labels.exit_scope();
        labels.exit_scope();
    }

    #[test]
    fn pgm6() {
        // do
        //     do
        //         goto x
        //     end
        //     do
        //         ::x::
        //     end
        // end
        // ::x::

        let mut labels = Labels::new();
        let mut cfg_builder = OpenCFG::new(vec![]);

        let cur_bb = cfg_builder.new_bb();
        let x1_bb = cfg_builder.new_bb();
        let x2_bb = cfg_builder.new_bb();
        cfg_builder.set_cur_bb(cur_bb);

        labels.enter_scope();
        labels.enter_scope();
        assert_eq!(labels.goto("x".to_string(), cur_bb, &mut cfg_builder), None);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Unknown);
        labels.exit_scope();
        labels.enter_scope();
        labels.label("x".to_string(), x1_bb, &mut cfg_builder);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Unknown);
        labels.exit_scope();
        labels.exit_scope();
        labels.label("x".to_string(), x2_bb, &mut cfg_builder);
        assert_eq!(cfg_builder.bb_terminator(cur_bb), Terminator::Jmp(x2_bb));
    }
}
