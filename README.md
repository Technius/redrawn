# Redrawn

Redrawn is an experimental implementation of an automated API migration
framework, written in Racket and using
[Rosette](https://docs.racket-lang.org/rosette-guide/ch_getting-started.html).

This was a course project for CS292C: Computer-Aided Reasoning for
Software (Fall 2019) at UCSB.

## Setup

Either run `raco setup` or run the appropriate commands to install the
dependencies specified in `info.rkt`.

## Usage

To test out the automated sketching translation, run

```bash
racket main.rkt -v i:integer -v x:integer -v b:boolean examples/prog8.txt
```

The `-v` flag will generate a symbolic value of the specified type and bind it
to the specified variable. For example, `-v i:integer` creates a symbolic
integer and binds it to the variable `i`. Currently, only `integer` and
`boolean` are supported.

Run `racket main.rkt -h` for additional basic usage information.
