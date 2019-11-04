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
    type_ns = types.SimpleNamespace(**{
        "Key": spec.type.EnumType("Key", keys),
        "Value": spec.type.EnumType("Value", data),
        "EmptyState": spec.type.EnumType("EmptyState", ["empty"]),
        "String": spec.type.ValueType("String"),
        "Status": spec.type.ValueType("Status"),
        "State": spec.type.ValueType("State", [("status", spec.expr.ExprType.INT)]),
    })
    for t in type_ns.__dict__.values():
        tys.define_type(t)

    prods = spec.spec.ProductionSpec()
    prods.add_func_production(name="const_empty", lhs=type_ns.State, rhs=[type_ns.EmptyState])
    prods.add_func_production(name="const_key", lhs=type_ns.String, rhs=[type_ns.Key])
    prods.add_func_production(name="const_val", lhs=type_ns.String, rhs=[type_ns.Value])
    prods.add_func_production(name="store_file", lhs=type_ns.State, rhs=[type_ns.String, type_ns.String, type_ns.State])

    prog_spec = spec.spec.ProgramSpec("program", [getattr(type_ns, n) for n in input_types],
                                      getattr(type_ns, output_type))
    return spec.spec.TyrellSpec(tys, prog_spec, prod_spec=prods)


class DatastoreV1Interpreter(PostOrderInterpreter):
    def eval_Key(self, val):
        return val

    def eval_Value(self, val):
        return val

    def eval_EmptyState(self, val):
        return State(status=StoreStatus.SUCCESS, store={})

    def eval_StatusCode(self, val):
        return StoreStatus(val)

    def eval_const_key(self, node, args):
        return args[0]

    def eval_const_val(self, node, args):
        return args[0]

    def eval_const_status(self, node, args):
        return args[0]

    def eval_const_empty(self, node, args):
        return args[0]

    def eval_store_file(self, node, args):
        return State(status=StoreStatus.SUCCESS, store={args[0]: args[1], **args[2].store})
