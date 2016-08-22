pub type Id = String;

#[derive(Debug)]
pub struct Block {
    pub stmts:  Vec<Box<Stmt>>,
    pub mb_ret: Option<Vec<Box<Exp>>>,
}

#[derive(Debug)]
pub enum Stmt {
    Assign {
        lhss: Vec<Var>,
        rhss: Vec<Box<Exp>>,
        is_local: bool,
    },

    // Conditional
    If {
        conds: Vec<(Box<Exp>, Block)>,
        else_: Option<Block>,
    },

    // Function call
    FunCall(FunCall),

    // Goto
    Label(String),
    Goto(String),

    // Looping
    Do(Block),
    While(Box<Exp>, Block),
    Repeat(Block, Box<Exp>),
    Break,

    ForRange {
        var:   Id,
        start: Box<Exp>,
        end:   Box<Exp>,
        step:  Option<Box<Exp>>,
        body:  Block,
    },

    ForIn {
        vars: Vec<Id>,
        exps: Vec<Box<Exp>>,
        body: Block,
    },
}

#[derive(Debug)]
pub enum Var {
    Var(Id),
    Select(Box<Exp>, Box<Exp>),
}

#[derive(Debug)]
pub enum Exp {
    Nil,
    Bool(bool),
    // Number(Number),
    Number(String),
    String(Vec<u8>),
    Vararg,

    FunDef {
        args:   Vec<Id>,
        vararg: bool,
        body:   Block,
    },

    Var(Var),

    FunCall(FunCall),

    Tbl(Vec<TblField>),

    Binop(Box<Exp>, Binop, Box<Exp>),

    Unop(Unop, Box<Exp>),
}

// pub enum Number {
//     Int(i64),
//     Float(f64),
// }

#[derive(Debug)]
pub enum TblField {
    /// [exp] = exp
    ExpField {
        rhs: Box<Exp>,
        lhs: Box<Exp>,
    },

    /// name = exp
    NamedField {
        rhs: String,
        lhs: Box<Exp>,
    },

    Field(Box<Exp>),
}

#[derive(Debug)]
pub enum FunCall {
    FunCall(Box<Exp>, Vec<Box<Exp>>),
    MethodCall(Box<Exp>, String, Vec<Box<Exp>>),
}

#[derive(Debug)]
pub enum Binop {
    Add, Sub, Mul, Div, Exp, Mod, Concat,
    LT, LTE, GT, GTE, EQ, NEQ, And, Or,
    IDiv, ShiftL, ShiftR, BAnd, BOr, BXor
}

#[derive(Debug)]
pub enum Unop {
    Neg, Not, Len, Complement
}
