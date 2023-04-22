"""
A simple SAT solver in Python.
"""

# loom:start(classes)
class Expr:
    pass


class FalseExpr(Expr):
    pass


class TrueExpr(Expr):
    pass


class Var(Expr):
    def __init__(self, name: str):
        self.name = name


class Not(Expr):
    def __init__(self, expr: Expr):
        self.expr = expr


class And(Expr):
    def __init__(self, exprs: list[Expr]):
        self.exprs = exprs


class Or(Expr):
    def __init__(self, exprs: list[Expr]):
        self.exprs = exprs


class Impl(Expr):
    def __init__(self, p: Expr, q: Expr):
        self.p = p
        self.q = q
# loom:end(classes)

# loom:start(replace)
def replace(e: Expr, name: str, value: bool) -> Expr:
    if isinstance(e, FalseExpr):
        return FalseExpr()
    elif isinstance(e, TrueExpr):
        return TrueExpr()
    elif isinstance(e, Var):
        if e.name == name:
            return TrueExpr() if value else FalseExpr()
        else:
            return Var(e.name)
    elif isinstance(e, Not):
        return Not(replace(e.expr, name, value))
    elif isinstance(e, And):
        return And([replace(expr, name, value) for expr in e.exprs])
    elif isinstance(e, Or):
        return Or([replace(expr, name, value) for expr in e.exprs])
    elif isinstance(e, Impl):
        return Impl(replace(e.p, name, value), replace(e.q, name, value))
    else:
        raise TypeError("Invalid expression type")
# loom:end(replace)

# loom:start(eval)
def eval_expr(e: Expr) -> bool:
    if isinstance(e, FalseExpr):
        return False
    elif isinstance(e, TrueExpr):
        return True
    elif isinstance(e, Var):
        raise ValueError(f"eval: the variable {e.name} has not been replaced.")
    elif isinstance(e, Not):
        return not eval_expr(e.expr)
    elif isinstance(e, And):
        return all(eval_expr(expr) for expr in e.exprs)
    elif isinstance(e, Or):
        return any(eval_expr(expr) for expr in e.exprs)
    elif isinstance(e, Impl):
        return (not eval_expr(e.p)) or eval_expr(e.q)
    else:
        raise TypeError("Invalid expression type")
# loom:end(eval)

# loom:start(free)
def free(e: Expr) -> set[str]:
    if isinstance(e, FalseExpr) or isinstance(e, TrueExpr):
        return set()
    elif isinstance(e, Var):
        return {e.name}
    elif isinstance(e, Not):
        return free(e.expr)
    elif isinstance(e, And):
        return set.union(*(free(expr) for expr in e.exprs))
    elif isinstance(e, Or):
        return set.union(*(free(expr) for expr in e.exprs))
    elif isinstance(e, Impl):
        return free(e.p).union(free(e.q))
    else:
        raise TypeError("Invalid expression type")

def any_var(e: Expr) -> str | None:
    variables: list[str] = list(free(e))
    if len(variables) == 0:
        return None
    else:
        return variables[0]
# loom:end(free)

# loom:start(solver)
Bindings = dict[str, bool]


def solve(e: Expr) -> Bindings | None:
    return solver(e, {})


def solver(e: Expr, bs: Bindings) -> Bindings | None:
    free_var = any_var(e)
    if free_var is None:
        if eval_expr(e):
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


def join(a: Bindings | None, b: Bindings | None) -> Bindings | None:
    if a is not None:
        return a
    else:
        return b
# loom:end(solver)

#
# Frontend
#

# loom:start(example)
def dep(p: str, deps: list[str]) -> Expr:
    return Impl(Var(p), Or([Var(d) for d in deps]))


def notboth(a: str, b: str) -> Expr:
    return Not(And([Var(a), Var(b)]))


formula: And = And(
    [
        Var("Alpha-v1"),
        dep("Alpha-v1", ["Beta-v2", "Beta-v3"]),
        dep("Alpha-v1", ["Gamma-v3"]),
        dep("Beta-v1", ["Delta-v1"]),
        dep("Beta-v2", ["Delta-v2"]),
        dep("Beta-v3", ["Delta-v2", "Delta-v3"]),
        dep("Gamma-v1", ["Delta-v1"]),
        dep("Gamma-v2", ["Delta-v2"]),
        dep("Gamma-v3", ["Delta-v3"]),
        notboth("Beta-v1", "Beta-v2"),
        notboth("Beta-v2", "Beta-v3"),
        notboth("Beta-v3", "Beta-v1"),
        notboth("Delta-v1", "Delta-v2"),
        notboth("Delta-v2", "Delta-v3"),
        notboth("Delta-v3", "Delta-v1"),
        notboth("Gamma-v1", "Gamma-v2"),
        notboth("Gamma-v2", "Gamma-v3"),
        notboth("Gamma-v3", "Gamma-v1"),
    ]
)
# loom:end(example)


def string_of_expr(e: Expr) -> str:
    if isinstance(e, FalseExpr):
        return r"\mathbf{F}"
    elif isinstance(e, TrueExpr):
        return r"\mathbf{T}"
    elif isinstance(e, Var):
        return r"\text{" + e.name + "}"
    elif isinstance(e, Not):
        return r"\neg" + string_of_expr(e.expr)
    elif isinstance(e, And):
        return "(" + r" \land ".join(string_of_expr(expr) for expr in e.exprs) + ")"
    elif isinstance(e, Or):
        return "(" + r" \lor ".join(string_of_expr(expr) for expr in e.exprs) + ")"
    elif isinstance(e, Impl):
        return "(" + string_of_expr(e.p) + r"\implies" + string_of_expr(e.q) + ")"
    else:
        raise TypeError("Invalid expression type")


for e in formula.exprs:
    print(r"\land \, &" + string_of_expr(e) + r" \\")

bs: Bindings | None = solve(formula)
if bs is not None:
    print("| Variable | Value |")
    print("| -------- | ----- |")
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        print(f"| {k} | {v} |")

# loom:start(run)
bs: Bindings | None = solve(formula)
if bs is not None:
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        print(k, v)
# loom:end(run)