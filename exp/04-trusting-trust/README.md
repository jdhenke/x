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

First, see [`su.x`](./su.x) and observe that, when compiled by the unaffected
compiler, `xc0`, it only accepts the password in its source code: `r007p4$$`

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
Access denied
```

Now, we will insert a compiler backdoor that causes `su.x` to accept the backdoor password: `trustno1`

Create the first infected compiler `xc1` by applying the hack to the source
code, bootstrapping the compiler, then reverting the hack.

```bash
git -C ../.. apply < hack.diff
/tmp/xc0 -o /tmp/xc1 < xc.x
git -C ../.. apply -R < hack.diff
```

Now, if we compile `su.x` using the infected `xc1` compiler:

```bash
/tmp/xc1 -o /tmp/su1 < su.x
```

One can see that `su1` now behaves just like `su0` but now _also_ accepts the backdoor password: `trustno1`

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

Now, even though the backdoor no longer exists in the source code, if we use
the infected `xc1` compiler to create the next compiler `xc2`, we can see that
the backdoor persists! And by extension, would persist indefinitely.

```bash
/tmp/xc1 -o /tmp/xc2 < xc.x
/tmp/xc2 -o /tmp/su2 < su.x
```

We can see `su2`, compiled with `xc2`, is still affected:

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

As a final sanity check, if we use the clean `xc0` compiler, we get back to a
`su` that only accepts the proper password.

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

