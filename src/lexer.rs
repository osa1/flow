use std::char;
use std;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Tok {
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

    /// String literals. Interpreted.
    /// (e.g. "\t" appears here as a tab character, not as '\' 't')
    // SLit(Vec<u8>),
    // Switching to String for easier debugging
    SLit(String),

    Ident(String),     // identifier

    /// End-of-stream. Used in the parser. Lexer doesn't generate this.
    EOS,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokPos {
    tok: Tok,
    line: i32,
    col: i32,
}

/// Could the char be a first character for an identifier or keyword?
fn is_first_id_char(ch : char) -> bool {
    let ch = ch as u8;
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
}

/// Could the char be a first character for a number?
fn is_first_num_char(ch : char) -> bool {
    is_dec_num_char(ch)
}

/// Is this a decimal numeral character?
fn is_dec_num_char(ch : char) -> bool {
    let ch = ch as u8;
    ch >= b'0' && ch <= b'9'
}

fn is_hex_num_char(ch : char) -> bool {
    let ch = ch as u8;
    (ch >= b'0' && ch <= b'9') || (ch >= b'a' && ch <= b'f') || (ch >= b'A' && ch <= b'F')
}

fn hex_digit(ch : char) -> Option<u8> {
    let ch = ch as u8;
    if ch >= b'0' && ch <= b'9' {
        Some(ch - b'0')
    } else if ch >= b'a' && ch <= b'f' {
        Some(ch - b'a' + 10)
    } else if ch >= b'A' && ch <= b'F' {
        Some(ch - b'A' + 10)
    } else {
        None
    }
}

fn dec_digit(ch : char) -> Option<u8> {
    let ch = ch as u8;
    if ch >= b'0' && ch <= b'9' {
        Some(ch - b'0')
    } else {
        None
    }
}

/// Letters, digits, and underscores are valid identifier characters.
fn is_id_char(ch : char) -> bool {
    let ch = ch as u8;
    (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || (ch >= b'0' && ch <= b'9') || ch == b'_'
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum StringDelim {
    Single, // '
    Double, // "
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Mode {
    Top,
    String(StringDelim),
    LongComment(i32),
    LongString(i32),
    Ident,
    Number,
    HexNumber,
}

macro_rules! fail {
    ($fmt:expr, $( $arg:tt ),*) => { return Err(format!($fmt, $( $arg ),*)); };
}

pub type LexerError = String;

pub fn tokenize(str : &str) -> Result<Vec<Tok>, LexerError> {
    let mut ts = vec![];

    let mut col  : i32 = 1;
    let mut line : i32 = 1;

    // buffer used for collecting identifier or string characters
    let mut buf = String::new();
    // what are we tokenizing?
    let mut mode = Mode::Top;

    // when parsing numbers, did we see a '.'?
    let mut seen_dot = false;
    // when parsing numbers, did we see a 'p' or 'e'?
    let mut seen_exp = false;

    let mut chars = str.chars();
    let mut c = chars.next();

    loop {
        match mode {

            Mode::Top => {
                match c {
                    Some('[') => {
                        // may be beginning of a long string
                        let mut delim_size = 0;
                        c = chars.next();
                        col += 1;
                        while c == Some('=') {
                            delim_size += 1;
                            c = chars.next();
                            col += 1;
                        }
                        if c == Some('[') {
                            // beginning of a long string
                            mode = Mode::LongString(delim_size);
                            c = chars.next();
                            col += 1;
                        } else {
                            // not a string, revert.
                            // we've read delim_size times '='s and a '['
                            // before that.
                            ts.push(Tok::LBracket);
                            for _ in 0 .. delim_size / 2 {
                                ts.push(Tok::Equal);
                            }
                            for _ in 0 .. delim_size % 2 {
                                ts.push(Tok::Assign);
                            }
                        }
                    },
                    Some('-') => {
                        // may be beginning of a single-line comment
                        c = chars.next();
                        col += 1;
                        if c == Some('-') {
                            c = chars.next();
                            col += 1;
                            // may be a long comment
                            if c == Some('[') {
                                let mut delim_size = 0;
                                c = chars.next();
                                col += 1;
                                while c == Some('=') {
                                    delim_size += 1;
                                    c = chars.next();
                                    col += 1;
                                }
                                if c == Some('[') {
                                    mode = Mode::LongComment(delim_size);
                                    c = chars.next();
                                    col += 1;
                                } else {
                                    // just a short comment. it's fine that we
                                    // skipped some characters so far.
                                    loop {
                                        if c == Some('\n') {
                                            break;
                                        } else {
                                            c = chars.next();
                                            col += 1;
                                        }
                                    }
                                }
                            } else {
                                // short comment
                                loop {
                                    if c == Some('\n') {
                                        break;
                                    } else {
                                        c = chars.next();
                                        col += 1;
                                    }
                                }
                            }
                        } else {
                            // it wasn't a comment
                            ts.push(Tok::Minus);
                        }
                    },
                    Some('+') => { ts.push(Tok::Plus); c = chars.next(); col += 1; },
                    Some('*') => { ts.push(Tok::Star); c = chars.next(); col += 1; },
                    Some('/') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('/') {
                            ts.push(Tok::DSlash);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::Slash);
                        }
                    },
                    Some('%') => { ts.push(Tok::Percent); c = chars.next(); col += 1; },
                    Some('^') => { ts.push(Tok::Exp);     c = chars.next(); col += 1; },
                    Some('#') => { ts.push(Tok::Sh);      c = chars.next(); col += 1; },
                    Some('=') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('=') {
                            ts.push(Tok::Equal);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::Assign);
                        }
                    },
                    Some('~') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('=') {
                            ts.push(Tok::NotEqual);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::Tilde);
                        }
                    },
                    Some('<') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('=') {
                            ts.push(Tok::LEq);
                            c = chars.next();
                            col += 1;
                        } else if c == Some('<') {
                            ts.push(Tok::DLT);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::LT);
                        }
                    },
                    Some('>') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('=') {
                            ts.push(Tok::GEq);
                            c = chars.next();
                            col += 1;
                        } else if c == Some('>') {
                            ts.push(Tok::DGT);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::GT);
                        }
                    },
                    Some('(') => { ts.push(Tok::LParen);   c = chars.next(); col += 1; },
                    Some(')') => { ts.push(Tok::RParen);   c = chars.next(); col += 1; },
                    Some('{') => { ts.push(Tok::LBrace);   c = chars.next(); col += 1; },
                    Some('}') => { ts.push(Tok::RBrace);   c = chars.next(); col += 1; },
                    Some(']') => { ts.push(Tok::RBracket); c = chars.next(); col += 1; },
                    Some(':') => {
                        c = chars.next();
                        col += 1;
                        if c == Some(':') {
                            ts.push(Tok::DColon);
                            c = chars.next();
                            col += 1;
                        } else {
                            ts.push(Tok::Colon);
                        }
                    },
                    Some(';') => { ts.push(Tok::Semic); c = chars.next(); col += 1; },
                    Some(',') => { ts.push(Tok::Comma); c = chars.next(); col += 1; },
                    Some('.') => {
                        c = chars.next();
                        col += 1;
                        if c == Some('.') {
                            c = chars.next();
                            col += 1;
                            if c == Some('.') {
                                ts.push(Tok::Ellipsis);
                                c = chars.next();
                                col += 1;
                            } else {
                                ts.push(Tok::DDot);
                            }
                        } else if c.is_some() && is_dec_num_char(c.unwrap()) {
                            buf.clear();
                            buf.push('.');
                            buf.push(c.unwrap());
                            c = chars.next();
                            col += 1;
                            mode = Mode::Number;
                        } else {
                            ts.push(Tok::Dot);
                        }
                    },
                    Some('&') => { ts.push(Tok::Ampersand); c = chars.next(); col += 1; },
                    Some('|') => { ts.push(Tok::Pipe);      c = chars.next(); col += 1; },

                    Some('"')  => { c = chars.next(); col += 1; mode = Mode::String(StringDelim::Double); },
                    Some('\'') => { c = chars.next(); col += 1; mode = Mode::String(StringDelim::Single); },

                    Some('\n') => {
                        line += 1;
                        col = 1;
                        c = chars.next();
                    },

                    Some(' ') => {
                        c = chars.next();
                        col += 1;
                    },

                    Some('0') => {
                        buf.clear();
                        buf.push('0');
                        c = chars.next();
                        col += 1;
                        seen_dot = false;
                        seen_exp = false;
                        // hex number?
                        if c == Some('x') || c == Some('X') {
                            buf.push(c.unwrap());
                            mode = Mode::HexNumber;
                            c = chars.next();
                            col += 1;
                        } else {
                            mode = Mode::Number;
                        }
                    },

                    Some(c_) if is_first_id_char(c_) => {
                            buf.clear();
                            mode = Mode::Ident;
                    },

                    Some(c_) if is_first_num_char(c_) => {
                            seen_dot = false;
                            seen_exp = false;
                            buf.clear();
                            mode = Mode::Number;
                    },

                    Some(c_) => {
                        fail!("lexer: unsupported char: '{}' at {}, {}", c_, line, col);
                    },

                    None => { break; },
                }
            },

            Mode::Ident => {
                match c {
                    Some(c_) if is_id_char(c_) => {
                        buf.push(c_);
                        c = chars.next();
                        col += 1;
                    },
                    _ => {
                        if &buf == "and" { ts.push(Tok::And); }
                        else if &buf == "break" { ts.push(Tok::Break); }
                        else if &buf == "do" { ts.push(Tok::Do); }
                        else if &buf == "else" { ts.push(Tok::Else); }
                        else if &buf == "elseif" { ts.push(Tok::ElseIf); }
                        else if &buf == "end" { ts.push(Tok::End); }
                        else if &buf == "false" { ts.push(Tok::False); }
                        else if &buf == "for" { ts.push(Tok::For); }
                        else if &buf == "function" { ts.push(Tok::Function); }
                        else if &buf == "goto" { ts.push(Tok::Goto); }
                        else if &buf == "if" { ts.push(Tok::If); }
                        else if &buf == "in" { ts.push(Tok::In); }
                        else if &buf == "local" { ts.push(Tok::Local); }
                        else if &buf == "nil" { ts.push(Tok::Nil); }
                        else if &buf == "not" { ts.push(Tok::Not); }
                        else if &buf == "or" { ts.push(Tok::Or); }
                        else if &buf == "repeat" { ts.push(Tok::Repeat); }
                        else if &buf == "return" { ts.push(Tok::Return); }
                        else if &buf == "then" { ts.push(Tok::Then); }
                        else if &buf == "true" { ts.push(Tok::True); }
                        else if &buf == "until" { ts.push(Tok::Until); }
                        else if &buf == "while" { ts.push(Tok::While); }
                        else { ts.push(Tok::Ident(std::mem::replace(&mut buf, String::new()))); }
                        mode = Mode::Top;
                    },
                }
            },

            Mode::Number | Mode::HexNumber => {
                match c {
                    Some('.') => {
                        if seen_dot {
                            fail!("invalid number constant at {}, {}", line, col);
                        }
                        seen_dot = true;
                        buf.push('.');
                        c = chars.next();
                        col += 1;
                    },
                    Some(c_) if (mode == Mode::Number && is_dec_num_char(c_)) ||
                                (mode == Mode::HexNumber && is_hex_num_char(c_)) => {
                        buf.push(c_);
                        c = chars.next();
                        col += 1;
                    },
                    Some('p') | Some('P') | Some('e') | Some('E') => {
                        if seen_exp {
                            fail!("invalid number constant at {}, {}", line, col);
                        }
                        seen_exp = true;
                        buf.push(c.unwrap());
                        c = chars.next();
                        col += 1;
                    },
                    Some('-') => {
                        // either the first character, or right after an
                        // exponent prefix
                        if buf.is_empty() {
                            buf.push(c.unwrap());
                            c = chars.next();
                            col += 1;
                        } else {
                            // Rust String API sucks
                            let c1 = buf.pop().unwrap();
                            match c1 {
                                'p' | 'P' | 'e' | 'E' => {
                                    buf.push(c1);
                                    buf.push('-');
                                    c = chars.next();
                                    col += 1;
                                },
                                _ => {
                                    // end of the literal
                                    buf.push(c1);
                                    ts.push(Tok::Num(std::mem::replace(&mut buf, String::new())));
                                    mode = Mode::Top;
                                },
                            }
                        }
                    },
                    Some('+') => {
                        // right after an exponent prefix
                        let c1 = buf.pop();
                        match c1 {
                            Some('p') | Some('P') | Some('e') | Some('E') => {
                                buf.push(c1.unwrap());
                                buf.push('+');
                                c = chars.next();
                                col += 1;
                            },
                            _ => {
                                // end of the literal
                                // TODO: This is not entirely correct. What
                                // happens if we see something like `0x+1`?
                                buf.push(c1.unwrap());
                                ts.push(Tok::Num(std::mem::replace(&mut buf, String::new())));
                                mode = Mode::Top;
                            }
                        }
                    },
                    _ => {
                        ts.push(Tok::Num(std::mem::replace(&mut buf, String::new())));
                        mode = Mode::Top;
                    },
                }
            },

            Mode::String(delim) => {
                match c {
                    Some('\\') => {
                        col += 1;
                        // '\a' (bell), '\b' (backspace), '\f' (form feed), '\n'
                        // (newline), '\r' (carriage return), '\t' (horizontal
                        // tab), '\v' (vertical tab), '\\' (backslash), '\"',
                        // and '\''
                        c = chars.next();
                        col += 1;
                        match c {
                            Some('a') =>  { buf.push(7 as char);  c = chars.next(); col += 1; }
                            Some('b') =>  { buf.push(8 as char);  c = chars.next(); col += 1; }
                            Some('f') =>  { buf.push(12 as char); c = chars.next(); col += 1; }
                            Some('n') =>  { buf.push('\n');       c = chars.next(); col += 1; }
                            Some('r') =>  { buf.push('\r');       c = chars.next(); col += 1; }
                            Some('t') =>  { buf.push('\t');       c = chars.next(); col += 1; }
                            Some('v') =>  { buf.push(11 as char); c = chars.next(); col += 1; }
                            Some('\\') => { buf.push('\\');       c = chars.next(); col += 1; }
                            Some('"') =>  { buf.push('"');        c = chars.next(); col += 1; }
                            Some('\'') => { buf.push('\'');       c = chars.next(); col += 1; }
                            Some('\n') => { buf.push('\n');       c = chars.next(); col  = 1; line += 1; }
                            Some('u') => {
                                // utf-8 escape
                                // must be followed by a {
                                c = chars.next();
                                col += 1;
                                match c {
                                    Some('{') => {
                                        // one or more hex numbers
                                        let mut utf8 : u32 = 0;
                                        c = chars.next();
                                        col += 1;
                                        loop {
                                            match c {
                                                Some('}') => {
                                                    buf.push(char::from_u32(utf8).unwrap());
                                                    c = chars.next();
                                                    col += 1;
                                                    break;
                                                },
                                                Some(c_) => {
                                                    match hex_digit(c_) {
                                                        Some(hex) => {
                                                            utf8 = utf8 * 16 + (hex as u32);
                                                            c = chars.next();
                                                            col += 1;
                                                        },
                                                        None => fail!("invalid hex digit at {}, {}", line, col),
                                                    }
                                                },
                                                _ => fail!("invalid hex digit at {}, {}", line, col),
                                            }
                                        }

                                    },
                                    _ => { fail!("wrong utf-8 escape in string literal at {}, {}", line, col); },
                                }
                            },
                            Some('z') => {
                                // skip whitespace
                                loop {
                                    c = chars.next();
                                    col += 1;
                                    match c {
                                        Some(' ') | Some('\t') => {}, // just skip
                                        Some('\n') => { col = 1; line += 1; }, // fix position
                                        _ => { break; },
                                    }
                                }
                            },
                            Some('x') => {
                                col += 1;
                                // two hex digits must follow
                                let hex1 = {
                                    // mmm Option monad
                                    c = chars.next();
                                    col += 1;
                                    match c {
                                        Some(c_) => {
                                            match hex_digit(c_) {
                                                Some(d) => d,
                                                None => fail!("invalid hex char at {}, {}", line, col),
                                            }
                                        },
                                        None => fail!("invalid hex char at {}, {}", line, col),
                                    }
                                };
                                // FIXME: repetition
                                let hex2 = {
                                    c = chars.next();
                                    col += 1;
                                    match c {
                                        Some(c_) => {
                                            match hex_digit(c_) {
                                                Some(d) => d,
                                                None => fail!("invalid hex char at {}, {}", line, col),
                                            }
                                        },
                                        None => fail!("invalid hex char at {}, {}", line, col),
                                    }
                                };
                                buf.push((hex1 * 16 + hex2) as char);
                                c = chars.next();
                                col += 1;
                            },
                            Some(c_) if is_dec_num_char(c_) => {
                                // up to three decimal digits (at least one)
                                let mut n : i32 = ((c_ as u8) - b'0') as i32;
                                for _ in 0 .. 2 {
                                    c = chars.next();
                                    col += 1;
                                    if c.is_some() && is_dec_num_char(c.unwrap()) {
                                        n = n * 10 + (((c.unwrap() as u8) - b'0') as i32);
                                    } else {
                                        break;
                                    }
                                }
                                // TODO: make sure n <= 255?
                                buf.push((n as u8) as char);
                            },
                            _ => fail!("invalid string escape at {}, {}", line, col),
                        }
                    },

                    Some('\'') if delim == StringDelim::Single => {
                        ts.push(Tok::SLit(std::mem::replace(&mut buf, String::new())));
                        c = chars.next();
                        col += 1;
                        mode = Mode::Top;
                    },

                    Some('"') if delim == StringDelim::Double => {
                        ts.push(Tok::SLit(std::mem::replace(&mut buf, String::new())));
                        c = chars.next();
                        col += 1;
                        mode = Mode::Top;
                    },

                    Some(c_) => {
                        buf.push(c_);
                        col += 1;
                        c = chars.next();
                    },

                    None => {
                        fail!("string not terminated {}, {}", line, col);
                    }
                }
            },

            Mode::LongComment(comment_delim_size) => {
                match c {
                    Some(']') => {
                        let mut delim = 0;
                        c = chars.next();
                        col += 1;
                        while c == Some('=') {
                            delim += 1;
                            c = chars.next();
                            col += 1;
                        }
                        if c == Some(']') && delim == comment_delim_size {
                            mode = Mode::Top;
                            c = chars.next();
                            col += 1;
                        }
                    },
                    Some('\n') => { c = chars.next(); col = 1; line += 1; },
                    Some(_) => { c = chars.next(); col += 1; },
                    None => { fail!("comment not terminated {}, {}", line, col); }
                }
            },

            Mode::LongString(string_delim_size) => {
                match c {
                    Some(']') => {
                        let mut delim = 0;
                        c = chars.next();
                        col += 1;
                        while c == Some('=') {
                            delim += 1;
                            c = chars.next();
                            col += 1;
                        }
                        if c == Some(']') && delim == string_delim_size {
                            mode = Mode::Top;
                            col += 1;
                            c = chars.next();
                            ts.push(Tok::SLit(std::mem::replace(&mut buf, String::new())));
                        } else {
                            buf.push(']');
                            for _ in 0 .. delim {
                                buf.push('=');
                            }
                        }
                    },
                    Some('\n') => { buf.push('\n'); c = chars.next(); col = 1; line += 1; },
                    Some(c_) => { buf.push(c_); c = chars.next(); col += 1; },
                    None => { fail!("string not terminated {}, {}", line, col); }
                }
            },
        }
    }

    Ok(ts)
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    use test::Bencher;

    use test_utils::*;

    #[test]
    fn test_lexer_1() {
        let str = "var1 + var2 -- comments\nvar3";
        assert_eq!(tokenize(str),
                   Ok(vec![
                        Tok::Ident("var1".to_string()),
                        Tok::Plus,
                        Tok::Ident("var2".to_string()),
                        Tok::Ident("var3".to_string()),
                      ]));
    }

    #[test]
    fn lexer_nums() {
        let nums = "3.0 3.1416 314.16e-2 0.31416E1 34e1 0x0.1E 0xA23p-4  0X1.921FB54442D18P+1";
        assert_eq!(tokenize(nums).unwrap().len(), 8);
    }

    #[bench]
    fn lexer_bench(b : &mut Bencher) {

        // Read all Lua files, concatenate contents.
        // 10878 lines in total.

        // ed9887a: 4,891,755 ns/iter (+/- 205,915)

        let lua = concat_lua_tests();
        b.iter(|| {
            tokenize(&lua).unwrap()
        });
    }
}
