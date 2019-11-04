import argparse
import sys
from typing import List

import automigrate.lang.datastore as DS
import tyrell


def parse_args(args: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("synth_file", help="Synthesis configuration file")
    ns = parser.parse_args(args)
    return ns


def main(args_: List[str]):
    args = parse_args(args_)
    print("TODO: Parse stuff {}".format(args.synth_file))
    interpreter = DS.DatastoreV1Interpreter()
    builder = tyrell.dsl.Builder(DS.build_datastore_v1_spec(["String", "String"], "State"))
    progn = """(store_file (@param 0) (const_val (Value "data1")) (store_file (const_key (Key "file3")) (const_val (Value "data1")) (const_empty (EmptyState "empty"))))"""
    result = interpreter.eval(builder.from_sexp_string(progn), ["foo", "bar"])
    print(result)


if __name__ == "__main__":
    main(sys.argv[1:])
