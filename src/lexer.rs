#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Plus,              // +
    Minus,             // -
    Star,              // *
    Slash,             // /
    Percent,           // %
    Exp,               // ^
    Sh,                // #
    Equal,             // ==
    NotEqual,          // ~=
    LEq,               // <=
    GEq,               // \>=
    LT,                // <
    GT,                // \>
    Assign,            // =
    LParen,            // (
    RParen,            // )
    LBrace,            // {
    RBrace,            // }
    LBracket,          // [
    RBracket,          // ]
    DColon,            // ::
    Semic,             // ;
    Colon,             // :
    Comma,             // ,
    Dot,               // .
    DDot,              // ..
    Ellipsis,          // ...
    DLT,               // <<
    DGT,               // >>
    Ampersand,         // &
    Pipe,              // |
    DSlash,            // //
    Tilde,             // ~

    And,               // and
    Break,             // break
    Do,                // do
    Else,              // else
    ElseIf,            // elseif
    End,               // end
    False,             // false
    For,               // for
    Function,          // function
    Goto,              // goto
    If,                // if
    In,                // in
    Local,             // local
    Nil,               // nil
    Not,               // not
    Or,                // or
    Repeat,            // repeat
    Return,            // return
    Then,              // then
    True,              // true
    Until,             // until
    While,             // while

    Num(String),       // number constant
    SLit(Vec<u8>),     // string constant
    Ident(String),     // identifier

    /// End-of-stream. Used in the parser. Lexer doesn't generate this.
    EOS,
}

macro_rules! tok {
    ( $x:expr ) => {
        {
            |_ : &mut Lexer<R>| { Some($x) }
        }
    };
}

macro_rules! skip {
    () => {
        {
            |_ : &mut Lexer<R>| { None }
        }
    };
}

rustlex! Lexer {

    // Size of string or comment delimiters. E.g. 4 in `[==[`.
    property delim_size : usize = 0;

    // variables are "letters, digits, and underscores, not beginning with a
    // digit and not being a reserved word"
    let LETTER   = ['a'-'z''A'-'Z'];
    let DIGIT    = ['0'-'9'];
    let VAR_INIT = (LETTER | '_');
    let VAR_CHAR = (LETTER | DIGIT | '_');
    let VAR      = VAR_INIT VAR_CHAR*;

    let STR      = '"' ([^'\\''"']|'\\'.)* '"' ;

    COMMENT {
        . => skip!()

        ']' '='+ ']' => |l : &mut Lexer<R>| {
            if l.yystr().len() == l.delim_size {
                l.INITIAL();
                None
            } else {
                None
            }
        }
    }

    INITIAL {
        . => skip!()

        // Comments
        '[' '='+ '[' => |l : &mut Lexer<R>| {
            l.delim_size = l.yystr().len() - 2;
            l.COMMENT();
            None
        }

        "--" [^'\n']+ => skip!()

        // Strings. TODO: incomplete
        STR         => |l : &mut Lexer<R>| { Some(Token::SLit(l.yystr().into_bytes())) }

        // Variables. TODO: rustlex doesn't support getting lexeme positions.
        VAR         => |l : &mut Lexer<R>| { Some(Token::Ident(l.yystr())) }

        // Numbers. TODO incomplete
        DIGIT+      => |l : &mut Lexer<R>| { Some(Token::Num(l.yystr())) }

        // Keywords
        "and"       => tok!(Token::And)
        "break"     => tok!(Token::Break)
        "do"        => tok!(Token::Do)
        "else"      => tok!(Token::Else)
        "elseif"    => tok!(Token::ElseIf)
        "end"       => tok!(Token::End)
        "false"     => tok!(Token::False)
        "for"       => tok!(Token::For)
        "function"  => tok!(Token::Function)
        "goto"      => tok!(Token::Goto)
        "if"        => tok!(Token::If)
        "in"        => tok!(Token::In)
        "local"     => tok!(Token::Local)
        "nil"       => tok!(Token::Nil)
        "not"       => tok!(Token::Not)
        "or"        => tok!(Token::Or)
        "repeat"    => tok!(Token::Repeat)
        "return"    => tok!(Token::Return)
        "then"      => tok!(Token::Then)
        "true"      => tok!(Token::True)
        "until"     => tok!(Token::Until)
        "while"     => tok!(Token::While)

        // Operators, brackets etc.
        '+'         => tok!(Token::Plus)
        '-'         => tok!(Token::Minus)
        '*'         => tok!(Token::Star)
        '/'         => tok!(Token::Slash)
        '%'         => tok!(Token::Percent)
        '^'         => tok!(Token::Exp)
        '#'         => tok!(Token::Sh)
        "=="        => tok!(Token::Equal)
        "~="        => tok!(Token::NotEqual)
        "<="        => tok!(Token::LEq)
        ">="        => tok!(Token::GEq)
        '<'         => tok!(Token::LT)
        '>'         => tok!(Token::GT)
        '='         => tok!(Token::Assign)
        '('         => tok!(Token::LParen)
        ')'         => tok!(Token::RParen)
        '{'         => tok!(Token::LBrace)
        '}'         => tok!(Token::RBrace)
        '['         => tok!(Token::LBracket)
        ']'         => tok!(Token::RBracket)
        "::"        => tok!(Token::DColon)
        ';'         => tok!(Token::Semic)
        ':'         => tok!(Token::Colon)
        ','         => tok!(Token::Comma)
        '.'         => tok!(Token::Dot)
        ".."        => tok!(Token::DDot)
        "..."       => tok!(Token::Ellipsis)
        "<<"        => tok!(Token::DLT)
        ">>"        => tok!(Token::DGT)
        '&'         => tok!(Token::Ampersand)
        '|'         => tok!(Token::Pipe)
        "//"        => tok!(Token::DSlash)
        '~'         => tok!(Token::Tilde)
    }
}

#[test]
fn test_lexer_1() {
    use std::io::BufReader;

    let str = "var1 + var2 -- comments\nvar3";
    let inp = BufReader::new(str.as_bytes());
    let lexer = Lexer::new(inp);
    assert_eq!(lexer.collect::<Vec<Token>>(),
               vec![
                 Token::Ident("var1".to_string()),
                 Token::Plus,
                 Token::Ident("var2".to_string()),
                 Token::Ident("var3".to_string()),
               ]);
}
