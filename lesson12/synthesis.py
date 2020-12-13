#!/usr/bin/env python3

import z3
import lark

GRAMMAR = """
?start: sum

?sum: term
  | sum "+" term        -> add
  | sum "-" term        -> sub

?term: item
  | term "*"  item      -> mul
  | term "/"  item      -> div
  | term ">>" item      -> shr
  | term "<<" item      -> shl

?item: NUMBER           -> num
  | "-" item            -> neg
  | CNAME               -> var
  | "(" start ")"

%import common.NUMBER
%import common.WS
%import common.CNAME
%ignore WS
"""

HOLE_PREFIX = '_hole'


def make_if(cond, true, false):
    return (cond != 0) * true + (cond == 0) * false


def interpret(expr, lookup):
    op = expr.data
    if op in ['add', 'sub', 'mul', 'div', 'shl', 'shr']:
        lhs = interpret(expr.children[0], lookup)
        rhs = interpret(expr.children[1], lookup)
        if op == 'add':
            return lhs + rhs
        elif op == 'sub':
            return lhs - rhs
        elif op == 'mul':
            return lhs * rhs
        elif op == 'div':
            return lhs / rhs
        elif op == 'shl':
            return lhs << rhs
        elif op == 'shr':
            return lhs >> rhs
    elif op == 'neg':
        arg = interpret(expr.children[0], lookup)
        return -arg
    elif op == 'num':
        return int(expr.children[0])
    elif op == 'var':
        return lookup(expr.children[0])
    elif op == 'if':
        cond = interpret(expr.children[0], lookup)
        true = interpret(expr.children[1], lookup)
        false = interpret(expr.children[2], lookup)
        return make_if(cond, true, false)

    raise ValueError('unrecognized expression')


def run(expr, env=None):
    if env is None:
        env = {}
    return interpret(expr, lambda n: env[n])


def z3_expr(expr, plain_vars=[], _vars=None):
    _vars = dict(_vars) if _vars else {}

    counter = 0

    def get_var(name):
        nonlocal counter

        if name in _vars:
            return _vars[name]
        else:
            if name.startswith(HOLE_PREFIX):
                # possibly substitute any variable for a hole
                e = z3.BitVec(HOLE_PREFIX + '__' + str(counter), 8)
                counter += 1
                for plain_var in plain_vars:
                    e = make_if(z3.BitVec(HOLE_PREFIX + '__' +
                                          str(counter), 8), get_var(plain_var), e)
                    counter += 1
                return e
            else:
                var = z3.BitVec(name, 8)
                _vars[name] = var
                return var

    return interpret(expr, get_var), _vars


def solve_z3(phi):
    solver = z3.Solver()
    solver.add(phi)
    solver.check()
    model = solver.model()
    # print(model)
    return {decl.name(): model[decl] for decl in model.decls()}


def synthesize(expr1, expr2):
    # jank, but I want to get all the variables first
    plain_vars = {key: var for key, var in z3_expr(expr1)[1].items()
                  if not key.startswith(HOLE_PREFIX)}

    z3_expr1, _vars = z3_expr(expr1, list(plain_vars.keys()))
    z3_expr2, _ = z3_expr(expr2, list(plain_vars.keys()), _vars)

    # print(z3_expr1)
    # print(z3_expr2)

    goal = z3.ForAll(
        list(plain_vars.values()),
        z3_expr1 == z3_expr2,
    )

    return solve_z3(goal), z3_expr2


def fill_holes(string):
    try:
        counter = 0
        while True:
            string.index('??')
            string = string.replace('??', HOLE_PREFIX + str(counter), 1)
            counter += 1
    except ValueError:
        return string


def main(sketch, target):
    parser = lark.Lark(GRAMMAR)

    expr1 = parser.parse(sketch)
    expr2 = parser.parse(fill_holes(target))

    print()

    # seems to turn bitshifts into extract/concat, don't know how to disable
    simplify_args = {
        'mul2concat': False,
    }

    try:
        model, result = synthesize(expr1, expr2)
        result = z3.substitute(
            result, [(z3.BitVec(k, 8), v) for k, v in model.items()])
        print(str(z3.simplify(result, **simplify_args)))
    except z3.z3types.Z3Exception as e:
        print('Unsatisfiable: {}'.format(e))

    print()


if __name__ == '__main__':
    try:
        print()
        main(
            input('Enter sketch expression: '),
            input('Enter target expression, with ?? holes: '))
    except (KeyboardInterrupt, EOFError):
        print()
