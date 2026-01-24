# `call/cc` Experiment

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

