# Verify `null` Experiment

## Motivation 

A driver for x is: what is the smallest self hosted language I can create?

As a stress test and thought experiment, I wondered, similar to [empty
quines](https://en.wikipedia.org/wiki/Quine_(computing)#Empty_quines), if an
empty solution is technically valid here as well?

If so, could that be made real in practice, such that the same verification
script that checks "real" solutions also passes for the empty solution?

Turns out, yes.

This directory replicates the root of the directory except it replaces both the
x interpreter (`xi.x`) and compiler (`xc.x`) with empty files, and shows that
running the same exact `./verify` script passes.

## Usage

Although the `./verify` script is quite rigorous in the sense that it ensures
the different runtimes can reproduce and behave consistently relative to each
other, it should be noted that this style of verification does not necessarily
ensure anything specific to the x language in particular.

In other words, it verifies from the outside, never the inside, of the results.

For proof, see that a `null` interpreter and compiler pass verification:

```bash
wc -l xi.x xc.x

./verify
```

Deciding whether this is a feature or a bug of `./verify` is an exercise left
to the reader.

## Next

The [next experiment](../02-min-eval/) attempts to find the next smallest
"solution" after `null` toward a minimally self hosted language.

