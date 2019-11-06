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
    TOO_BIG = 2


def build_datastore_v1_spec(input_types: List[str], output_type: str):
    keys = ["file1", "file2", "file3"]
    data = ["data1", "data2", "data3"]
    states = itertools.product(keys, data)
    tys = spec.spec.TypeSpec()
    state_props = [
        ("status", spec.expr.ExprType.INT),
        ("size", spec.expr.ExprType.INT),
    ]
    type_ns = types.SimpleNamespace(**{
        "Key": spec.type.EnumType("Key", keys),
        "Value": spec.type.EnumType("Value", data),
        "InitState": spec.type.EnumType("InitState", ["init"]),
        "String": spec.type.ValueType("String"),
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
    prods.add_func_production(name="const_init", lhs=type_ns.State, rhs=[type_ns.InitState])
    prods.add_func_production(name="const_key", lhs=type_ns.String, rhs=[type_ns.Key])
    prods.add_func_production(name="const_val", lhs=type_ns.String, rhs=[type_ns.Value])
    prods.add_func_production(
        name="store_file",
        lhs=type_ns.State, rhs=[type_ns.String, type_ns.String, type_ns.State],
        constraints=[
            (
                (size_prop(E.param(0)) == size_prop(E.param(3))) |
                (size_prop(E.param(0)) == size_prop(E.param(3)) + 1)
            ).tree,
            (size_prop(E.param(0)) > 0).tree
        ]
    )
    prods.add_func_production(name="noop", lhs=type_ns.State, rhs=[type_ns.State])
    prods.add_func_production(name="empty", lhs=type_ns.Empty, rhs=[type_ns.Empty])

    prog_spec = spec.spec.ProgramSpec("program", [getattr(type_ns, n) for n in input_types],
                                      getattr(type_ns, output_type))

    preds = spec.spec.PredicateSpec()
    preds.add_predicate("occurs", ["const_init", 100])
    return spec.spec.TyrellSpec(tys, prog_spec, prod_spec=prods, pred_spec=preds)


class DatastoreV1Interpreter(PostOrderInterpreter):
    def eval_Key(self, val: str):
        return val

    def eval_Value(self, val: str):
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

    def eval_noop(self, node, args):
        return args[0]

    def apply_status(self, state: State):
        return int(state.status)

    def apply_size(self, state: State):
        return len(state.store)
