enum AST {
    Atom(String),
    Var(i64),
    Cons(Box<AST>, Box<AST>),
    Nil
}

fn new_variable(counter: &mut i64) -> AST {
    *counter = *counter + 1;
    Var(*counter)
}

fn is_variable(tree: AST) -> bool {
    match tree {
        Var(_) => true,
        _ => false
    }
}

fn variable_before(x: AST, y: AST) -> bool {
    match x {
        Var(x_val) => {
            match y {
                Var(y_val) => {
                    x_val < y_val
                },
                _ => false
            }
        },
        _ => false
    }
}

fn print_ast(tree: AST) -> String{
    match tree {
        Atom(str) => str,
        Var(n) => format!("?{}", n),
        Cons(first, rest) => String::from_str("()"),
        Nil => String::from_str("()")
    }
}

fn main() {
    ;
}
