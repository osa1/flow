pub enum Either<T1, T2> {
    Left(T1),
    Right(T2),
}

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
