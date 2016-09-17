//! We abstract things related with scoping, variable captures etc. here.

use cfg::Var;
use uniq::UniqCounter;

use std::collections::HashMap;
use std::collections::HashSet;

pub struct Scopes {
    local_scopes: Vec<Scope>,
    global_scope: Scope,
    var_gen: UniqCounter,
}

struct Scope {
    vars: HashMap<String, Var>,

    /// Only available in closure scopes.
    captures: Option<HashSet<Var>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VarOcc {
    Captured(Var),
    Local(Var),
    Global(Var),
}

impl VarOcc {
    #[cfg(test)]
    pub fn is_captured(&self) -> bool {
        match self {
            &VarOcc::Captured(_) => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn is_local(&self) -> bool {
        match self {
            &VarOcc::Local(_) => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn is_global(&self) -> bool {
        match self {
            &VarOcc::Global(_) => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn get_var(&self) -> Var {
        match self {
            &VarOcc::Captured(var) => var,
            &VarOcc::Local(var) => var,
            &VarOcc::Global(var) => var,
        }
    }
}

impl Scopes {
    pub fn new() -> Scopes {
        Scopes {
            local_scopes: vec![],
            global_scope: Scope { vars: HashMap::new(), captures: None },
            var_gen: UniqCounter::new(b's'),
        }
    }

    pub fn fresh_var(&mut self) -> Var {
        self.var_gen.fresh()
    }

    /// Enter a new scope.
    pub fn enter(&mut self) {
        self.local_scopes.push(Scope { vars: HashMap::new(), captures: None });
    }

    /// Enter a closure scope. Closures have an initial set of local variables: arguments.
    pub fn enter_closure(&mut self, args : Vec<String>) -> Vec<Var> {
        let mut env = HashMap::new();

        // bind arguments
        let mut arg_vars = vec![];
        for arg in args {
            let arg_var = self.fresh_var();
            arg_vars.push(arg_var);
            env.insert(arg, arg_var);
        }

        self.local_scopes.push(Scope { vars: env, captures: Some(HashSet::new()) });
        arg_vars
    }

    /// It's an error to exit() when top-most scope is a closure scope.
    pub fn exit(&mut self) {
        let top = self.local_scopes.pop().unwrap();
        if top.captures.is_some() { panic!("Scopes.exit(): In a closure scope."); }
    }

    /// It's an error to exit_closure() when top-most scope is not a closure scope.
    /// Returns captured variables.
    pub fn exit_closure(&mut self) -> HashSet<Var> {
        let top = self.local_scopes.pop().unwrap();
        match top.captures {
            Some(captures) => captures,
            None => panic!("Scopes.exit_closure(): In a non-closure scope."),
        }
    }

    /// Declare a local variable.
    pub fn var_decl(&mut self, s : String) -> Var {
        let var = self.fresh_var();
        self.local_scopes.last_mut().unwrap_or(&mut self.global_scope).vars.insert(s, var);
        var
    }

    /// Find an occurence of a variable.
    pub fn var_occ(&mut self, s : &str) -> VarOcc {
        // search local variables first
        {
            // closures that capture this variable
            let mut closures : Vec<&mut HashSet<Var>> = vec![];

            for scope in self.local_scopes.iter_mut().rev() {
                match scope.vars.get(s).cloned() {
                    Some(var) => {
                        for closure in closures.iter_mut() {
                            closure.insert(var);
                        }
                        if closures.is_empty() {
                            return VarOcc::Local(var);
                        } else {
                            return VarOcc::Captured(var);
                        }
                    },
                    None => {
                        match &mut scope.captures {
                            &mut Some(ref mut captures) => {
                                closures.push(captures);
                            },
                            &mut None => { /* nothing to do */ },
                        }
                    },
                }
            }
        }

        // global variable
        let var = match self.global_scope.vars.get(s).cloned() {
            // weird code as a borrow checker workaround
            Some(var) => { return VarOcc::Global(var); },
            None => { self.fresh_var() },
        };
        self.global_scope.vars.insert(s.to_owned(), var);
        VarOcc::Global(var)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test_scoping {
    use super::*;

    use std::collections::HashSet;

    #[test]
    fn scoping_1() {
        let mut scopes = Scopes::new();
        let var1 = scopes.var_occ("var1");
        scopes.enter();
        let var2 = scopes.var_decl("var2".to_string());
        scopes.enter_closure(vec![]);

        let occ1 = scopes.var_occ("var2");
        assert_eq!(occ1, VarOcc::Captured(var2));

        let occ2 = scopes.var_occ("var1");
        assert_eq!(occ2, var1);

        let captured = scopes.exit_closure();
        assert_eq!(captured, set![var2]);
    }

    #[test]
    fn scoping_2() {
        // test nested closures
        let mut scopes = Scopes::new();

        let var1 = "var1";
        let var2 = "var2";
        let var3 = "var3";

        // a global variable. make sure this is not captured.
        let global = scopes.var_occ("global");
        assert!(global.is_global());

        scopes.enter_closure(vec![var1.to_string()]);
        scopes.enter_closure(vec![var2.to_string()]);
        scopes.enter_closure(vec![var3.to_string()]);

        let occ1 = scopes.var_occ(var1);
        assert!(occ1.is_captured());

        let occ2 = scopes.var_occ(var2);
        assert!(occ2.is_captured());

        let occ3 = scopes.var_occ(var3);
        assert!(occ3.is_local());

        let occ4 = scopes.var_occ("global");
        assert!(occ4.is_global());

        let captures = scopes.exit_closure();
        assert_eq!(captures, set![occ1.get_var(), occ2.get_var()]);

        let occ5 = scopes.var_occ(var2);
        assert!(occ5.is_local());

        let captures = scopes.exit_closure();
        assert_eq!(captures, set![occ1.get_var()]);

        let occ6 = scopes.var_occ(var1);
        assert!(occ6.is_local());

        let captures = scopes.exit_closure();
        assert_eq!(captures, set![]);
    }
}
