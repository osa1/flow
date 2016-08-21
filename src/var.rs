use uniq::Uniq;

use std::cmp::Ordering;

/// Position in a file.
#[derive(Debug)]
pub enum Pos {
    /// Usually as a place holder.
    Unknown,
    KnownPos(KnownPos),
}

#[derive(Debug)]
pub struct KnownPos {
    file: String,
    line: usize,
    col:  usize,
}

#[derive(Debug)]
pub struct Var {
    /// Position of the variable.
    pos:  Pos,

    /// Name of the variable as written in the source file.
    name: String,

    /// Unique number of the variable. Bounds of a variable is determined using
    /// this. E.g. occurences of `x` in same scope will have same uniq, and the
    /// uniq will be different than other `x`s in different scopes in the
    /// program.
    uniq: Uniq,
}

impl PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        self.uniq == other.uniq
    }
}

impl Eq for Var {}

impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Var) -> Option<Ordering> {
        self.uniq.partial_cmp(&other.uniq)
    }
}

impl Ord for Var {
    fn cmp(&self, other: &Var) -> Ordering {
        self.uniq.cmp(&other.uniq)
    }
}
