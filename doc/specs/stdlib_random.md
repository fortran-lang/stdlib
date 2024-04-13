---
title: random
---

# Statistical Distributions -- Pseudorandom Number Generator Module

[TOC]

## `random_seed` - set or get a value of seed to the probability distribution pseudorandom number generator

### Status

Experimental

### Description

Set or get the seed value before calling the probability distribution pseudorandom number generator for variates.

### Syntax

`call ` [[stdlib_random(module):random_seed(interface)]] `(put, get)`

### Arguments

`put`: argument has `intent(in)` and may be a scalar of type `integer`.

`get`: argument has `intent(out)` and is a scalar of type `integer`.

### Return value

Return a scalar of type `integer`.

### Example

```fortran
{!example/random/example_random_seed.f90!}
```

## `dist_rand` - Get a random integer with specified kind

### Status

Experimental

### Description

Generate an integer pseudorandom number in a specific range [-2^k, 2^k - 1] according to the input integer kind n. This pseudorandom number will be operated by bit opeartors instead of normal arithmetic operators.

### Syntax

`result = ` [[stdlib_random(module):dist_rand(interface)]] `(n)`

### Arguments

`n`: argument has `intent(in)` is a scalar of type `integer`.

### Return value

Return a scalar of type `integer`.

### Example

```fortran
{!example/random/example_dist_rand.f90!}
```
