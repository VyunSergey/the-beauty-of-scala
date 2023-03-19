# Collatz Conjecture

Welcome to Collatz Conjecture problem.

## Instructions

[The Collatz Conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) or `3x+1 | x/2` problem can be summarized as follows:

* Take any positive integer `n`
* If `n` is even `->` divide `n` by 2 to get `n / 2`
* If `n` is odd `->` multiply `n` by 3 and add 1 to get `3n + 1`
* Repeat the process indefinitely
* The conjecture states that no matter which number `n` you start with, you will
always reach `1` eventually

Given a number `n` return the number of steps required to reach `1`

## Examples

Starting with `n = 12` the steps would be as follows:
```text
i=0 n=12
i=1 n=6
i=2 n=3
i=3 n=10
i=4 n=5
i=5 n=16
i=6 n=8
i=7 n=4
i=8 n=2
i=9 n=1
```

Resulting in `9` steps. So for input `n = 12` the return value would be `9`

## Source
