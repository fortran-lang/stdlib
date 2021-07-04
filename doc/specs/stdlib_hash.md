---
title: Hashing algorithms
...

# The `stdlib_hash` module

[TOC]


## Introduction

Hash functions to obtain a hashed value of the provided variable.


## Available procedures


### `hash` function

#### Status

Experimental

#### Description

Calculates the hash of an input character variable.

#### Syntax

`res = [[stdlib_hash(module):hash(interface)]] (var, seed)`

#### Class

Pure function.

#### Argument

`var`: Shall be an intrinsic character type. It is an `intent(in)` argument.

`seed`: Shall be an intrinsic integer 64 bit wide integer. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic integer 32 bit wide integer.


#### Example

```fortran
program demo_hash
    use stdlib_hash, only : hash
    use stdlib_kinds, only : int64
    implicit none
    print '(z0.8)', hash("Value", 433494437_int64)  ! return "B5E5E913"
 end program demo_hash
```


#### `waterhash`

Portation of the [waterhash algorithm](https://github.com/tommyettinger/waterhash) from C to Fortran.
The original waterhash implementation is available under the terms of the Unlicense (public domain).

#### Status

Experimental

#### Description

Calculates the hash of an array of chars.

#### Syntax

`res = [[stdlib_hash(module):waterhash]] (array, seed)`

#### Class

Pure function.

#### Argument

`array`: Shall be an rank one array of 8 bit wide integer values. It is an `intent(in)` argument.

`seed`: Shall be an intrinsic integer 64 bit wide integer. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic integer 32 bit wide integer.


#### Example

```fortran
program demo_hash
    use stdlib_hash, only : waterhash
    use stdlib_kinds, only : int8, int64
    implicit none
    integer(int8), parameter :: array(*) = &
      [119_int8, 97_int8, 116_int8, 101_int8, 114_int8, &
      &104_int8, 97_int8, 115_int8, 104_int8]
    print '(z0.8)', waterhash(array, 433494437_int64)  ! return "55EB744D"
 end program demo_hash
```
