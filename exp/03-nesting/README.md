# Nesting Experiment

## Motivation

Just like how the [minimal `eval`](../02-min-eval/) solution could evaluate
itself, does that still work with x?

The answer is yes, but sometimes slowly.

## Usage

Each runtime populates `runtime` with the string of the chain of runtimes
that's currently being run.

To see a few versions of nesting print their own runtime, run:

Run:

```bash
./demo
```

Output (can be slow):

```
Running: S ...
Hello from: s

Running: S | i ...
Hello from: si

Running: S | i | i ...
Hello from: sii

Running: C ...
Hello from: c

Running: I ...
Hello from: ci

Running: I | i ...
Hello from: cii
```

Note: `I` is running `./xi` which is built by compiling (`c`) the interpreter
(`i`), hence `I` self reporting `ci`.

