use ast::{Binop, Unop, Number};
use cfg::Var;

#[derive(Debug)]
pub enum Stat {
    Assign(LHS, RHS),

    /// (lhss, rhs)
    MultiAssign(Vec<LHS>, RHS),

    // Phi(Var, Vec<Var>),
}

#[derive(Debug, Clone)]
pub enum LHS {
    /// Table index
    Tbl(Var, Var),

    /// Variable
    Var(Var),

    /// Write to a captured varible
    Captured(Var),
}

#[derive(Debug, Clone)]
pub enum RHS {
    Nil,
    Var(Var),
    Bool(bool),
    Number(Number),
    String(String),
    FunCall(Var, Vec<Var>),
    NewTbl,
    ReadTbl(Var, Var),
    Binop(Var, Binop, Var),
    Unop(Unop, Var),

    /// Value of a captured variable.
    Captured(Var),
}
