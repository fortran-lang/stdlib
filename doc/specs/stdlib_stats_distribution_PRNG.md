---
title: stats_distribution_PRNG
---

# Statistical Distributions -- Pseudorandom Number Generator Module

[TOC]

## `random_seed` - set or get a value of seed to the probability distribution pseudorandom number generator

### Status

Experimental

### Description

Set or get the seed value before calling the probability distribution pseudorandom number generator for variates.

### Syntax

`call [[stdlib_stats_distribution_PRNG(module):random_seed(interface)]](put, get)`

### Arguments

`put`: argument has `intent(in)` and may be a scalar of type `integer`.

`get`: argument has `intent(out)` and is a scalar of type `integer`.

### Return value

Return a scalar of type `integer`.

### Example

```fortran
program demo_random_seed
    use stdlib_stats_distribution_PRNG, only : random_seed
    implicit none
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)     ! set and get current value of seed
end program demo_random_seed
```

## `dist_rand` - Get a random integer with specified kind

### Status

Experimental

### Description

Generate an integer pseudorandom number in a specific range [-2^k, 2^k - 1] according to the input integer kind n. This pseudorandom number will be operated by bit opeartors instead of normal arithmetic operators.

### Syntax

`result = [[stdlib_stats_distribution_PRNG(module):dist_rand(interface)]](n)`

### Arguments

`n`: argument has `intent(in)` is a scalar of type `integer`.

### Return value

Return a scalar of type `integer`.

### Example

```fortran
program demo_dist_rand
    use stdlib_stats_distribution_PRNG, only : dist_rand, random_seed
    implicit none
    integer :: put, get

    put = 135792468
    call random_seed(put, get)     ! set and get current value of seed
    print *, dist_rand(1_int8)     ! random integer in [-2^7, 2^7 - 1]
! -90
    print *, dist_rand(1_int16)    ! random integer in [-2^15, 2^15 - 1]
! -32725
    print *, dist_rand(1_int32)    ! random integer in [-2^31, 2^31 - 1]
! -1601563881
    print *, dist_rand(1_int64)    ! random integer in [-2^63, 2^63 - 1]
! 180977695517992208
end program demo_dist_rand
```
