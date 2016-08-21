// use var::Var;

pub type Var = String;

pub struct Block {
    pub stmts:  Vec<Box<Stmt>>,
    pub mb_ret: Option<Vec<Box<Exp>>>,
}

pub enum Stmt {
    Assign {
        lhs: LHS,
        rhs: Vec<Box<Exp>>,
    },

    FunCall(FunCall),

    Label(String),

    Break,

    Goto(String),

    Do(Block),

    While(Box<Exp>, Block),

    Repeat(Block, Box<Exp>),

    If {
        conds: Vec<(Box<Exp>, Block)>,
        else_: Option<(Box<Exp>, Block)>,
    },

    ForRange {
        var:   Var,
        start: Box<Exp>,
        end:   Box<Exp>,
        step:  Option<Box<Exp>>,
        body:  Block,
    },

    ForIn {
        vars: Vec<Var>,
        exps: Vec<Box<Exp>>,
        body: Block,
    },
}

pub enum LHS {
    FieldLHS(Vec<String>),
    VarLHS(Var),
}

pub enum Exp {
    Nil,
    Bool(bool),
    // Number(Number),
    Number(String),
    String(Vec<u8>),
    Vararg,

    FunDef {
        args:   Vec<Var>,
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

pub enum FunCall {
    MethodCall {
        obj:   Box<Exp>,
        mname: String,
        args:  Vec<Box<Exp>>,
    },

    FunCall {
        fun:  Box<Exp>,
        args: Vec<Box<Exp>>,
    },
}

pub enum Binop {
    Add, Sub, Mul, Div, Exp, Mod, Concat,
    LT, LTE, GT, GTE, EQ, NEQ, And, Or,
    IDiv, ShiftL, ShiftR, BAnd, BOr, BXor
}

pub enum Unop {
    Neg, Not, Len, Complement
}
