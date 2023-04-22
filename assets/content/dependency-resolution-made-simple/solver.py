"""
A simple SAT solver in Python.
"""

class Expr:
    pass


class TrueExpr(Expr):
    pass


class FalseExpr(Expr):
    pass


class Var(Expr):
    def __init__(self, name: str):
        self.name = name


class Not(Expr):
    def __init__(self, expr: Expr):
        self.expr = expr


class And(Expr):
    def __init__(self, expr1: Expr, expr2: Expr):
        self.expr1 = expr1
        self.expr2 = expr2


class Or(Expr):
    def __init__(self, expr1: Expr, expr2: Expr):
        self.expr1 = expr1
        self.expr2 = expr2


def impl(p: Expr, q: Expr) -> Expr:
    return Or(Not(p), q)


def iff(p: Expr, q: Expr) -> Expr:
    return And(impl(p, q), impl(q, p))


def replace(e: Expr, name: str, value: bool) -> Expr:
    if isinstance(e, TrueExpr):
        return TrueExpr()
    elif isinstance(e, FalseExpr):
        return FalseExpr()
    elif isinstance(e, Var):
        if e.name == name:
            return TrueExpr() if value else FalseExpr()
        else:
            return Var(e.name)
    elif isinstance(e, Not):
        return Not(replace(e.expr, name, value))
    elif isinstance(e, And):
        return And(replace(e.expr1, name, value), replace(e.expr2, name, value))
    elif isinstance(e, Or):
        return Or(replace(e.expr1, name, value), replace(e.expr2, name, value))
    else:
        raise TypeError("Invalid expression type")


def eval_expr(e: Expr) -> bool:
    if isinstance(e, TrueExpr):
        return True
    elif isinstance(e, FalseExpr):
        return False
    elif isinstance(e, Var):
        raise ValueError(f"eval: the variable {e.name} has not been replaced.")
    elif isinstance(e, Not):
        return not eval_expr(e.expr)
    elif isinstance(e, And):
        return eval_expr(e.expr1) and eval_expr(e.expr2)
    elif isinstance(e, Or):
        return eval_expr(e.expr1) or eval_expr(e.expr2)
    else:
        raise TypeError("Invalid expression type")


def free(e: Expr) -> set[str]:
    if isinstance(e, TrueExpr) or isinstance(e, FalseExpr):
        return set()
    elif isinstance(e, Var):
        return {e.name}
    elif isinstance(e, Not):
        return free(e.expr)
    elif isinstance(e, And):
        return free(e.expr1).union(free(e.expr2))
    elif isinstance(e, Or):
        return free(e.expr1).union(free(e.expr2))
    else:
        raise TypeError("Invalid expression type")


def any_var(e: Expr) -> str | None:
    variables: list[str] = list(free(e))
    if len(variables) == 0:
        return None
    else:
        return variables[0]


Bindings = dict[str, bool]


def join(a: Bindings | None, b: Bindings | None) -> Bindings | None:
    if a is not None:
        return a
    else:
        return b


def solve(e: Expr) -> Bindings | None:
    return solver(e, {})


def solver(e: Expr, bs: Bindings) -> Bindings | None:
    free_var = any_var(e)
    if free_var is None:
        if eval_expr(replace_all(e, bs)):
            return bs
        else:
            return None
    else:
        # Replace with True.
        t: Expr = replace(e, free_var, True)
        t_bs: Bindings = dict(bs)
        t_bs[free_var] = True
        # Replace with False.
        f: Expr = replace(e, free_var, False)
        f_bs: Bindings = dict(bs)
        f_bs[free_var] = False
        # Solve both branches, and join them.
        return join(solver(t, t_bs), solver(f, f_bs))


def replace_all(e: Expr, bindings: Bindings) -> Expr:
    for name, value in bindings.items():
        e = replace(e, name, value)
    return e

#
# Frontend
#

def ors(l: list[str]) -> Expr:
    if len(l) > 1:
        first, rest, *rest_ = l
        return Or(Var(first), ors([rest] + rest_))
    elif len(l) == 1:
        return Var(l[0])
    else:
        raise ValueError("ors")

def ands(l: list[Expr]) -> Expr:
    if len(l) > 1:
        first, rest, *rest_ = l
        return And(first, ands([rest] + rest_))
    elif len(l) == 1:
        return l[0]
    else:
        raise ValueError("ands")

def dep(p: str, deps: list[str]) -> Expr:
    return impl(Var(p), ors(deps))

def notboth(a: str, b: str) -> Expr:
    return Not(And(Var(a), Var(b)))

formulas: list[Expr] = [
    Var("A1"),
    dep("A1", ["B2", "B3"]),
    dep("A1", ["G3"]),
    dep("B1", ["D1"]),
    dep("B2", ["D2"]),
    dep("B3", ["D2", "D3"]),
    dep("G1", ["D1"]),
    dep("G2", ["D2"]),
    dep("G3", ["D3"]),
    notboth("B1", "B2"),
    notboth("B2", "B3"),
    notboth("B3", "B1"),
    notboth("D1", "D2"),
    notboth("D2", "D3"),
    notboth("D3", "D1"),
    notboth("G1", "G2"),
    notboth("G2", "G3"),
    notboth("G3", "G1"),
]

formula: Expr = ands(formulas)

def string_of_expr(e: Expr) -> str:
    if isinstance(e, TrueExpr):
        return "T"
    elif isinstance(e, FalseExpr):
        return "F"
    elif isinstance(e, Var):
        return e.name
    elif isinstance(e, Not):
        return "¬" + string_of_expr(e.expr)
    elif isinstance(e, And):
        return "(" + string_of_expr(e.expr1) + " ∧ " + string_of_expr(e.expr2) + ")"
    elif isinstance(e, Or):
        return "(" + string_of_expr(e.expr1) + " ∨ " + string_of_expr(e.expr2) + ")"
    else:
        raise TypeError("Invalid expression type")

print(string_of_expr(formula))

bs: Bindings | None = solve(formula)
if bs is not None:
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        print(k, v)