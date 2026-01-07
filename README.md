## x

A minimal, eXecutable, self hosted lisp.

## About

x is a programming language. This repo makes x code eXecutable in a few ways.

It is a subset of [MIT Scheme](https://www.gnu.org/software/mit-scheme/)'s
syntax and has its own minimal standard library.

- `./xs` adapts MIT scheme to match the x runtime.

Further, x itself is sufficient to:

- write its own interpreter (`xi.x`)
- write its own compiler (`xc.x`)
- pass unit tests for each language feature (`test.x`)
- solve Advent of Code puzzles (`aoc/`)

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

To have `xc` only compile and not run, use `-o <path>` e.g.

```bash
./xc -o output.bin < code.x
```

### Verify

Because the x interpreter and compiler are written themselves in x, one can
chain them together after bootstrapping with the scheme runtime.

To verify that chaining the runtimes together in different orders produces the
same exact results, run:

```bash
./verify
```

Note: for performance reasons, this limits to chains that do not run nested
interpeters.

## Examples: Advent of Code

See `./aoc` for solutions to Advent of Code puzzles written in x.

You can run with any runtime e.g.:

```bash
./xs < ./aoc/2025/day1.x
./xi < ./aoc/2025/day1.x
./xc < ./aoc/2025/day1.x
```

