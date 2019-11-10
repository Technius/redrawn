from tyrell.interpreter import PostOrderInterpreter
import tyrell.spec as spec
import enum
from typing import List
import itertools
import types
from collections import namedtuple


State = namedtuple("State", "status store")


class StoreStatus(enum.IntEnum):
    """Status code for storage. Anything other than SUCCESS is a failure."""
    SUCCESS = 0
    EXISTS = 1
    NOT_EXISTS = 2
    TOO_BIG = 3


def build_datastore_v1_spec(input_types: List[str], output_type: str):
    keys = ["file1", "file2", "file3"]
    data = ["data1", "data2", "data3"]
    states = itertools.product(keys, data)
    tys = spec.spec.TypeSpec()
    state_props = [
        ("status", [], spec.expr.ExprType.INT),
        ("size", [], spec.expr.ExprType.INT),
        ("contains_key", [spec.expr.ExprType.VALUE], spec.expr.ExprType.BOOL),
    ]
    type_ns = types.SimpleNamespace(**{
        "KeyConst": spec.type.EnumType("KeyConst", keys),
        "ValueConst": spec.type.EnumType("ValueConst", data),
        "InitState": spec.type.EnumType("InitState", ["init"]),
        "Key": spec.type.ValueType("Key"),
        "Value": spec.type.ValueType("Value"),
        "Status": spec.type.ValueType("Status"),
        "State": spec.type.ValueType("State", state_props),
        # See source file for tyrell.enumerator.smt.SmtEnumerator
        # Note that NO type can contain "Empty" or else SMT enumerator fails
        "Empty": spec.type.ValueType("Empty"),
    })
    for t in type_ns.__dict__.values():
        tys.define_type(t)

    from ..constraints import ExprDsl as E

    def size_prop(expr): return E.property("size", E.ExprType.INT, expr)

    prods = spec.spec.ProductionSpec()
    prods.add_func_production(name="const_init", lhs=type_ns.State, rhs=[type_ns.InitState],
                              constraints=[(size_prop(E.param(0)) == 0).tree])
    prods.add_func_production(name="const_key", lhs=type_ns.Key, rhs=[type_ns.KeyConst])
    prods.add_func_production(name="const_val", lhs=type_ns.Value, rhs=[type_ns.ValueConst])
    prods.add_func_production(
        name="store_file",
        lhs=type_ns.State, rhs=[type_ns.Key, type_ns.Value, type_ns.State],
        constraints=[
            E.property("contains_key", E.ExprType.BOOL, E.param(3), E.param(1)).implies(
                E.param(0) == E.param(3)).tree,
            (E.property("contains_key", E.ExprType.BOOL, E.param(3), E.param(1)).not_()).implies(
                E.and_(
                    size_prop(E.param(0)) == size_prop(E.param(3)) + 1,
                    E.property("contains_key", E.ExprType.BOOL, E.param(0), E.param(1)),
                )
            ).tree,
        ]
    )
    prods.add_func_production(name="delete_file",
        lhs=type_ns.State, rhs=[type_ns.Key, type_ns.State],
        constraints=[
            (E.property("contains_key", E.ExprType.BOOL, E.param(2), E.param(1)).not_()).implies(
                E.param(0) == E.param(2)).tree,
            E.property("contains_key", E.ExprType.BOOL, E.param(2), E.param(1)).implies(
                E.and_(
                    size_prop(E.param(2)) == size_prop(E.param(0)) + 1,
                    E.property("contains_key", E.ExprType.BOOL, E.param(0), E.param(1)).not_(),
                )
            ).tree,
            (size_prop(E.param(0)) >= 0).tree,
        ]
    )
    prods.add_func_production(name="noop", lhs=type_ns.State, rhs=[type_ns.State],
                              constraints=[(E.param(0) == E.param(1)).tree])
    prods.add_func_production(name="empty", lhs=type_ns.Empty, rhs=[type_ns.Empty])

    prog_spec = spec.spec.ProgramSpec("program", [getattr(type_ns, n) for n in input_types],
                                      getattr(type_ns, output_type))

    preds = spec.spec.PredicateSpec()
    preds.add_predicate("occurs", ["const_init", 100])
    return spec.spec.TyrellSpec(tys, prog_spec, prod_spec=prods, pred_spec=preds)


class DatastoreV1Interpreter(PostOrderInterpreter):
    def eval_KeyConst(self, val: str):
        return val

    def eval_ValueConst(self, val: str):
        return val

    def eval_InitState(self, _: str):
        return State(status=StoreStatus.SUCCESS, store={})

    def eval_StatusCode(self, val: str):
        return StoreStatus(val)

    def eval_const_key(self, node, args):
        return args[0]

    def eval_const_val(self, node, args):
        return args[0]

    def eval_const_status(self, node, args):
        return args[0]

    def eval_const_init(self, node, args):
        return args[0]

    def eval_store_file(self, node, args):
        return State(status=StoreStatus.SUCCESS,
                     store={**args[2].store, args[0]: args[1]})

    def eval_delete_file(self, node, args):
        nstore = args[1].store.copy()
        if args[0] in nstore:
            status = StoreStatus.SUCCESS
            del nstore[args[0]]
        else:
            status = StoreStatus.NOT_EXISTS
        return State(status=status, store=nstore)

    def eval_noop(self, node, args):
        return args[0]

    def apply_status(self, state: State) -> int:
        return int(state.status)

    def apply_size(self, state: State) -> int:
        return len(state.store)

    def apply_contains_key(self, state: State, key: str) -> bool:
        return key in state.store

    def infer_contains_key(self, state: State):
        return [[k] for k in state.store.keys()]
