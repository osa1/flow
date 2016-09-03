pub type Id = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Binop {
    Add, Sub, Mul, Div, Exp, Mod, Concat,
    LT, LTE, GT, GTE, EQ, NEQ, And, Or,
    IDiv, ShiftL, ShiftR, BAnd, BOr, BXor
}

/// Operator precedence.
pub type Prec = u8;

/// Precedence and associativity of a binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BinopPrec {
    pub left: Prec,
    pub right: Prec,
}

pub fn binop_prec(op : Binop) -> BinopPrec {
    match op {
        Binop::Add | Binop::Sub =>
            BinopPrec { left: 10, right: 10 },
        Binop::Mul | Binop::Mod =>
            BinopPrec { left: 11, right: 11 },
        Binop::Exp =>
            BinopPrec { left: 14, right: 13 },
        Binop::Div | Binop::IDiv =>
            BinopPrec { left: 11, right: 11 },
        Binop::BAnd =>
            BinopPrec { left: 6, right: 6 },
        Binop::BOr =>
            BinopPrec { left: 4, right: 4 },
        Binop::BXor =>
            BinopPrec { left: 5, right: 5 },
        Binop::ShiftL | Binop::ShiftR =>
            BinopPrec { left: 7, right: 7 },
        Binop::Concat =>
            BinopPrec { left: 9, right: 8 },
        Binop::EQ | Binop::LT | Binop::LTE | Binop::NEQ | Binop::GT | Binop::GTE =>
            BinopPrec { left: 3, right: 3 },
        Binop::And =>
            BinopPrec { left: 2, right: 2 },
        Binop::Or =>
            BinopPrec { left: 1, right: 1 },
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Unop {
    Neg, Not, Len, Complement
}

pub const UNOP_PREC : Prec = 12;
