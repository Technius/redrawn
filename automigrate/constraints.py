import functools
import tyrell.spec.expr as E

__slots__ = ["ExprDsl"]

BOp = E.BinaryOperator
UOp = E.UnaryOperator

class ExprDsl(object):
    ExprType = E.ExprType

    tree: E.Expr

    def __init__(self, tree: E.Expr):
        self.tree = tree

    @staticmethod
    def _convert_arg(v):
        if isinstance(v, ExprDsl):
            return v.tree
        if isinstance(v, E.Expr):
            return v
        if isinstance(v, (int, bool)):
            return E.ConstExpr(v)

    def _binop(self, ty, other):
        return ExprDsl(E.BinaryExpr(ty, self.tree, self._convert_arg(other)))

    def __neg__(self):
        return ExprDsl(E.UnaryExpr(UOp.NEG, self.tree))

    def __add__(self, other):
        return self._binop(BOp.ADD, other)

    def __radd__(self, other):
        return ExprDsl(other).__add__(self)

    def __eq__(self, other):
        return self._binop(BOp.EQ, other)

    def __req__(self, other):
        return ExprDsl(other).__eq__(self)

    def __gt__(self, other):
        return self._binop(BOp.GT, other)

    def __rgt__(self, other):
        return ExprDsl(other).__gt__(self)

    def __ge__(self, other):
        return self._binop(BOp.GE, other)

    def __rge__(self, other):
        return ExprDsl(other).__gte__(self)

    def __or__(self, other):
        return self._binop(BOp.OR, other)

    def __ror__(self, other):
        return ExprDsl(other).__ror__(self)

    def implies(self, other) -> 'ExprDsl':
        return self._binop(BOp.IMPLY, other)

    def not_(self):
        return ExprDsl(E.UnaryExpr(UOp.NOT, self.tree))

    @staticmethod
    def and_(*cs):
        args = cs
        if not cs:
            raise ValueError("and_ requires at least one argument")
        if len(cs) == 1:
            args = cs[0]
        return functools.reduce(lambda c1, c2: c1._binop(BOp.AND, c2), cs[1:], cs[0])

    @staticmethod
    def property(name: str, type: E.ExprType, *operands: E.Expr) -> 'ExprDsl':
        return ExprDsl(E.PropertyExpr(name, type, [ExprDsl._convert_arg(x) for x in operands]))

    @staticmethod
    def param(index: int) -> 'ExprDsl':
        return ExprDsl(E.ParamExpr(index))
