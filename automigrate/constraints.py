import tyrell.spec.expr as E

__slots__ = ["ExprDsl"]

BOp = E.BinaryOperator

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

    def __or__(self, other):
        return self._binop(BOp.OR, other)

    def __ror__(self, other):
        return ExprDsl(other).__ror__(self)

    @staticmethod
    def property(name: str, type: E.ExprType, operand: E.Expr) -> 'ExprDsl':
        return ExprDsl(E.PropertyExpr(name, type, ExprDsl._convert_arg(operand)))

    @staticmethod
    def param(index: int) -> 'ExprDsl':
        return ExprDsl(E.ParamExpr(index))
