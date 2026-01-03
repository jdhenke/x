## x

A minimal, eXecutable, self hosted lisp.

## About

x is a programming language. This repo makes x code eXecutable in a few ways.

It is a subset of [MIT Scheme](https://www.gnu.org/software/mit-scheme/)'s syntax
and has its own minimal standard library.

- `./xs` adapts MIT scheme to match the x runtime.

Further, x itself is sufficient to:

- `xi.x` - write its own interpreter
- `xc.x` - write its own compiler
- `check.x` - pass unit tests for each language feature
- `verify.x` - verify that chaining self hosted runtimes produces a stable "fixed point"
- `aoc/` - solve all 2025 advent of code puzzles

## Setup

### Scheme

Download [MIT Scheme](https://www.gnu.org/software/mit-scheme/).

You should be able to run:

```bash
scheme
```

And get a prompt back.

### Bootstrap

Run:

```bash
./bootstrap
```

You should now have three x runtimes:

- `./xs` a wrapper around scheme
- `./xi` a native x interpreter
- `./xc` a native x compiler

All accept x source from stdin and run it.

```bash
for x in ./xs ./xi ./xc; do
    echo "(println (+ 1 2))" | $x
done
```

To have `xc` only compile and not run, using `./xc -o output.bin`.

### Verify

To verify chaining the runtimes together in different orders produces the same results, run:

```bash
./verify
```

Note: for performance reasons, this limits to chains that only run two nested interpreters.

To verify a particular chain of:

- s (scheme x interpeter)
- i (native x interpreter)
- c (native x compiler)
- t (x tests)

Run `./verify` with the chain string as an argument e.g.:

```bash
./verify sict
./verify scccc
```

Valid chains match the regex: `^s(c|i)*(t|(c(i|c|t)))$`

