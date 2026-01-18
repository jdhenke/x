## x

A minimal, eXecutable, self hosted lisp.

## About

x is a programming language. This repo makes x code eXecutable in a few ways.

It is a subset of [MIT Scheme](https://www.gnu.org/software/mit-scheme/)'s
syntax and has its own minimal standard library.

- `./xs` adapts MIT scheme to match the x runtime.

Further, x itself is sufficient to:

- write its own interpreter ([`xi.x`](./xi.x))
- write its own compiler ([`xc.x`](./xc.x))
- pass unit tests for each language feature ([`test.x`](./test.x))
- solve Advent of Code puzzles ([`aoc/`](./aoc))

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

All accept x code from stdin and run it.

```bash
for x in ./xs ./xi ./xc; do
    echo "(println (+ 1 2))" | $x
done
```

To have `xc` only compile and not run, use `-o <path>` e.g.

```bash
./xc -o /tmp/output.bin < code.x
```

Note: all three runtimes support [tail
recursion](https://en.wikipedia.org/wiki/Tail_call). See
[below](#verify-callcc) for details on `call/cc` support.

### Verify

Because the x interpreter and compiler are written in x themselves, one can
chain them together after bootstrapping with the scheme runtime.

To verify that chaining the runtimes together in different orders reproduces
the same results, including the "fixed point" that the compiler can reproduce
itself byte-for-byte, run:

```bash
./verify
```

Note: for performance reasons, this limits to chains that do not run nested
interpeters.

### Verify `null`

Although the above verification is quite rigorous in the sense that it verifies
the internal consistency of the different runtimes as black boxes from all
feasible angles, it should be noted that this style of verification fails to
ensure such outputs are sensible in an externally verifiable way.

For proof, see that a `null` interpreter and compiler pass verification:

```bash
truncate -s 0 xi.x
truncate -s 0 xc.x

./verify

git checkout HEAD -- xi.x xc.x
```

Deciding whether this is a feature or a bug of `./verify` is an exercise left
to the reader.

### Verify `call/cc`

Support for
[continuations](https://en.wikipedia.org/wiki/Call-with-current-continuation)
varies by runtime:

- `./xs` inherits support from MIT scheme
- `./xi` supports by internally representing computations in [Continuation-Passing
  Style](https://en.wikipedia.org/wiki/Continuation-passing_style) (CPS)
- `./xc` does not support itself, but accepts externally CPS-converted code using `./cps`

These can each be tested with:

```bash
cat test-k.x | ./xs
cat test-k.x | ./xi
cat test-k.x | ./cps | ./xc
```

Or by running: [`./verify-k`](./verify-k)

Note: [`test-k.x`](./test-k.x) purposefully produces random pairings (that
still satisfy constraints!) each time.

## Examples: Advent of Code

See [`./aoc`](./aoc) for solutions to Advent of Code puzzles written in x.

You can run with any runtime e.g.:

```bash
./xs < ./aoc/2025/day1.x
./xi < ./aoc/2025/day1.x
./xc < ./aoc/2025/day1.x
```

