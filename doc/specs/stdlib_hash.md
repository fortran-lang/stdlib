---
title: Hashing algorithms
...

# The `stdlib_hash` module

[TOC]


## Introduction

Hash functions to obtain a hashed value of the provided variable.


## Available procedures


#### `waterhash`

Portation of the [waterhash algorithm](https://github.com/tommyettinger/waterhash) from C to Fortran.
Waterhash provides a 32 bit hash using a single 64 bit seed.
The original waterhash implementation is available under the terms of the Unlicense (public domain).

#### Status

Experimental

#### Description

Calculates the hash of an array of chars.

#### Syntax

`res = [[stdlib_hash(module):waterhash(interface)]] (var, seed)`

#### Class

Pure function.

#### Argument

- `var`: Shall be an rank one array of 8 bit wide integer values.
         Intrinsic character types and [[stdlib_string_type(module):string_type(type)]]
         values are implicitly converted with `transfer`.
         It is an `intent(in)` argument.

- `seed`: Shall be an intrinsic integer 64 bit wide integer. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic integer 32 bit wide integer.


#### Example

```fortran
program demo_waterhash
    use stdlib_hash, only : waterhash
    use stdlib_kinds, only : int8, int64
    implicit none
    integer(int64), parameter :: seed = 433494437_int64
    integer(int8), parameter :: array(*) = &
      [119_int8, 97_int8, 116_int8, 101_int8, 114_int8, &
      &104_int8, 97_int8, 115_int8, 104_int8]
    print '(z0.8)', waterhash(array, seed)  ! return "55EB744D"
    print '(z0.8)', waterhash("Value", seed)  ! return "B5E5E913"
 end program demo_waterhash
```
