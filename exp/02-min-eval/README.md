# Minimal `eval` Experiment

## Motivation

After [`null`](../01-null/), what is the most minimal solution to a self hosted
language?

While not a fully self hosted compiler, the next exploratory step I made took
inspiration from Paul Graham's [Roots of
Lisp](https://paulgraham.com/rootsoflisp.html), his exploration of John
McCarthy's lisp evaluator, which was itself written in lisp.

Paul Graham converted [McCarthy's
paper](https://www-formal.stanford.edu/jmc/recursive.pdf) into a [common lisp
version](https://sep.turbifycdn.com/ty/cdn/paulgraham/jmc.lisp), which was
executable, but not self evaluating. You could not feed the common lisp
definition of John McCarthy's eval back into itself.

This experiment shows a (perhaps even more) minimal `eval` that _is_ self
evaluating, written in MIT scheme: [`eval.scm`](./eval.scm)

```scheme
; Minimal self hosting eval.
; Assumes only quote, lambda, cond, apply special forms.
; Assumes only #t, #f, list?, equal?, cons, car, cdr values.

((lambda (y.)
   ((lambda (append. assoc. bind.)
      (y.
       (lambda (eval.)
         ((lambda (evcond. evlambda. evlist.)
            (lambda (e a)
              (cond ((list? e)
                     (cond ((equal? (car e) 'quote) (car (cdr e)))
                           ((equal? (car e) 'lambda) (evlambda. e a))
                           ((equal? (car e) 'cond) (evcond. (cdr e) a))
                           ((equal? (car e) 'apply)
                            (apply (eval. (car (cdr e)) a)
                                   (eval. (car (cdr (cdr e))) a)))
                           (#t
                            (apply (eval. (car e) a) (evlist. (cdr e) a)))))
                    (#t (assoc. e a)))))
          (y.
           (lambda (evcond.)
             (lambda (c a)
               (cond ((eval. (car (car c)) a) (eval. (car (cdr (car c))) a))
                     (#t (evcond. (cdr c) a))))))
          (lambda (e a)
            (lambda args
              (eval.
               (car (cdr (cdr e)))
               (cond ((list? (car (cdr e)))
                      (append. (bind. (car (cdr e)) args) a))
                     (#t (cons (cons (car (cdr e)) (cons args '())) a))))))
          (y.
           (lambda (evlist.)
             (lambda (l a)
               (cond ((equal? l '()) '())
                     (#t (cons (eval. (car l) a) (evlist. (cdr l) a)))))))))))
    (y.
     (lambda (append.)
       (lambda (x y)
         (cond ((equal? x '()) y)
               (#t (cons (car x) (append. (cdr x) y)))))))
    (y.
     (lambda (assoc.)
       (lambda (x y)
         (cond ((equal? (car (car y)) x) (car (cdr (car y))))
               (#t (assoc. x (cdr y)))))))
    (y.
     (lambda (bind.)
       (lambda (x y)
         (cond ((equal? x '()) '())
               (#t
                (cons (cons (car x) (cons (car y) '()))
                      (bind. (cdr x) (cdr y))))))))))
 (lambda (f)
   ((lambda (x)
      (f (lambda args (apply (x x) args))))
    (lambda (x)
      (f (lambda args (apply (x x) args)))))))

```

A few comparisons and notes:

- In contrast to Graham's faithful reproduction of McCarthy's work, this
(selfishly) diverges from a simple port of the common lisp to scheme.
- There's no `define`, `set`, or `let`; only anonymous `lambda` functions.
- Reuse of definitions is used by [Immediately Invoked Function
Expressions](https://en.wikipedia.org/wiki/Immediately_invoked_function_expression)
- Recursion is made possibly through the [Y
Combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator),
ironically the name of the world's most famous startup accelerator that Paul
Graham co-founded.
- This does not even use the shorthand combinations of `car` and `cdr`
- It only uses lists, no pairs.

## Usage

As proof this actually works, run:

```bash
./demo
```

This defines a `reverse` operation in terms the minimal `eval` can understand.

```scheme
((lambda (y.)
   ((lambda (append.)
     (y.
       (lambda (reverse.)
         (lambda (l)
           (cond ((equal? l '()) l)
                  (#t (append. (reverse. (cdr l)) (cons (car l) '()))))))))
     (y.
       (lambda (append.)
         (lambda (x y)
           (cond ((equal? x '()) y)
                 (#t (cons (car x) (append. (cdr x) y)))))))))
 (lambda (f)
   ((lambda (x)
      (f (lambda args (apply (x x) args))))
    (lambda (x)
      (f (lambda args (apply (x x) args)))))))
```

Then demonstrates `(reverse '(a b c d e f))` being run in increasingly nested
layers of the self evaluated `eval`.

### Reflection

This is pretty cool! A minimal, self hosting `eval` in 60 lines of code.

And, in constrast to the [`null` solution](../01-null/), we actually inspected
some expected behavior by demonstrating `reverse` works properly.

However, this is not truly self hosted in the sense that you still need MIT
scheme to run this evaluator to start. And as minimal as the functions and
special forms are that are identified as being the sole requirements of this
evaluator, I wondered how much I was still inheriting and taking for granted
from MIT scheme without realizing it. A truly self hosted system would run
without MIT scheme at all and really prove its indepenence.

And furthemore, could it be useful beyond simple operations like `reverse`?

So, while still a cool result, these lingering questions really spurred me on
towards making x even more real, in having both a functioning interpreter and
compiler, and to ultimately enable solving real programming challenges in
practice e.g. [Advent of Code](../../aoc/).

