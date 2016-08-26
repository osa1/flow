use std::fs;
use std::io::Read;

pub fn is_lua_file(s : &str) -> bool {
    let len = s.len();
    len > 4 && &s[ len - 4 .. ] == ".lua"
}

/// Concatenate contents of all Lua files under tests.
pub fn concat_lua_tests() -> String {
    let mut lua = String::new();
    let bench_files_dir = "lua-5.3.1-tests/".to_string();

    for file_ in fs::read_dir(&bench_files_dir).unwrap() {
        let file = file_.unwrap();
        let fname_os = file.file_name();
        let fname = fname_os.to_str().unwrap();
        if is_lua_file(fname) {
            let mut path = bench_files_dir.clone();
            path.push_str(fname);
            let mut f = fs::File::open(path).unwrap();
            f.read_to_string(&mut lua).unwrap();
            lua.push('\n');
        }
        // println!("{}", lua.len());
    }

    lua
}
