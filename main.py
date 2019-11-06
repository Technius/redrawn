import argparse
import sys
from typing import List
import tyrell

import automigrate.lang.datastore as DS


def parse_args(args: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("synth_file", help="Synthesis configuration file",
                        type=argparse.FileType("r", encoding="utf8"))
    parser.add_argument("-d", "--depth", help="Enumeration depth", type=int, default=5)
    parser.add_argument("-l", "--loc", help="Number of function calls in synthesized program", type=int, default=2)
    ns = parser.parse_args(args)
    return ns


def main(args_: List[str]):
    args = parse_args(args_)
    interpreter = DS.DatastoreV1Interpreter()
    spec = DS.build_datastore_v1_spec(["String", "String"], "State")
    builder = tyrell.dsl.Builder(spec)
    with args.synth_file as f:
        lines = []
        for l in f:
            i = l.rfind(";;")
            lines.append(l if i == -1 else l[:i].strip())
        progn = "\n".join(lines)

    # Sample code for now. This just evaluates the given program and then uses
    # the input/output pair to synthesize an equivalent program.
    inputs = ["foo", "bar"]
    outputs = interpreter.eval(builder.from_sexp_string(progn), inputs)
    print("Evaluation result:", outputs)

    # enumerator = tyrell.enumerator.ExhaustiveEnumerator(spec, max_depth=args.depth)
    enumerator = tyrell.enumerator.SmtEnumerator(spec, depth=args.depth, loc=args.loc)
    decider = tyrell.decider.ExampleConstraintPruningDecider(
        spec=spec,
        interpreter=interpreter,
        examples=[
            tyrell.decider.Example(input=inputs, output=outputs)
        ]
    )
    synthesizer = tyrell.synthesizer.Synthesizer(enumerator=enumerator, decider=decider)
    print("Synthesizing with max depth {depth}, {loc} functions".format(depth=args.depth, loc=args.loc))
    prog_syn = synthesizer.synthesize()
    print("Synthesized program:", prog_syn)
    if prog_syn:
        print("Synthesized result:", interpreter.eval(prog_syn, inputs))


if __name__ == "__main__":
    main(sys.argv[1:])
