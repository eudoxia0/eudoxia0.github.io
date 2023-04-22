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
        return set().union(*[free(expr) for expr in e.exprs])
    elif isinstance(e, Or):
        return set().union(*[free(expr) for expr in e.exprs])
    elif isinstance(e, Impl):
        return free(e.p).union(free(e.q))
    else:
        raise TypeError("Invalid expression type")


def any_var(e: Expr) -> str | None:
    variables: list[str] = sorted(list(free(e)))
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
        # Solve both branches, and return the first one that works.
        return solver(t, t_bs) or solver(f, f_bs)
# loom:end(solver)

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

#
# Example
#

# loom:start(deps)
from dataclasses import dataclass


@dataclass(frozen=True)
class Dependency:
    name: str
    minimum: int
    maximum: int

@dataclass(frozen=True)
class Package:
    name: str
    version: int
    depends_on: list[Dependency]

packages: list[Package] = [
    Package(
        "app",
        0,
        [
            Dependency("sql", 2, 2),
            Dependency("threads", 2, 2),
            Dependency("http", 3, 4),
            Dependency("stdlib", 4, 4),
        ],
    ),
    Package("sql", 0, []),
    Package("sql", 1, [Dependency("stdlib", 1, 4), Dependency("threads", 1, 1)]),
    Package("sql", 2, [Dependency("stdlib", 2, 4), Dependency("threads", 1, 2)]),
    Package("threads", 0, [Dependency("stdlib", 2, 4)]),
    Package("threads", 1, [Dependency("stdlib", 2, 4)]),
    Package("threads", 2, [Dependency("stdlib", 3, 4)]),
    Package("http", 0, [Dependency("stdlib", 0, 3)]),
    Package("http", 1, [Dependency("stdlib", 0, 3)]),
    Package("http", 2, [Dependency("stdlib", 1, 4)]),
    Package("http", 3, [Dependency("stdlib", 2, 4)]),
    Package("http", 4, [Dependency("stdlib", 3, 4)]),
]
# loom:end(deps)

# loom:start(convert)
from itertools import combinations

def convert(root: str, packages: list[Package]) -> Expr:
    """
    Given a package-version to use as the root of the build DAG, and a list of
    package dependency constraints, convert them into a logical expression.
    """
    # First things first: we need the root package to be part of the assignment.
    terms: list[Expr] = [Var(root)]
    # Add implications.
    for p in packages:
        # Package versions imply their dependencies.
        for dep in p.depends_on:
            versions: list[int] = list(range(dep.minimum, dep.maximum + 1))
            deps: list[Expr] = [package_var(dep.name, v) for v in versions]
            impl: Impl = Impl(package_var(p.name, p.version), Or(deps))
            terms.append(impl)
    # Exclude every pair of versions. We do this by taking the set.of free
    # variables in the expression built up so far, and for each package depended
    # upon, we find all the versions mentioned for it and pairwise exclude them.
    variables: set[str] = free(And(terms))
    varnames: set[str] = set([var_name(v) for v in variables])
    for name in varnames:
        vers: set[int] = {var_version(v) for v in variables if var_name(v) == name}
        for a, b in all_combinations(vers):
            terms.append(Not(And([package_var(name, a), package_var(name, b)])))
    # Finally, return the built up expression as a conjunction.
    return And(terms)

def package_var(name: str, version: int) -> Var:
    return Var(f"{name}-v{version}")

def var_name(var: str) -> str:
    return var.split("-v")[0]

def var_version(var: str) -> int:
    return int(var.split("-v")[1])

def all_combinations(lst: set[int]) -> list[tuple[int, int]]:
    return list(combinations(lst, 2))
# loom:end(convert)

# loom:start(conversion)
formula: Expr = convert("app-v0", packages)
# loom:end(conversion)

def pretty_print(expr: And):
    print("$$")
    print(r"\begin{align*}")
    for e in expr.exprs:
        print(f"\\land ~~ &{string_of_expr(e)} \\\\")
    print(r"\end{align*}")
    print("$$")

pretty_print(formula)

# loom:start(solution)
bs: Bindings | None = solve(formula)
if bs is not None:
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        print(k, v)
# loom:end(solution)

if bs is not None:
    print("| Variable | Value |")
    print("| -------- | ----- |")
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        b: str = r"\true" if v else r"\false"
        print(f"| `{k}` | ${b}$ |")
