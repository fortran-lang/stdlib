---
title: math
---

# The `stdlib_math` module

[TOC]

## Introduction

`stdlib_math` module provides general purpose mathematical functions.


## Procedures and Methods provided


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `clip` function

#### Description

Returns a value which lies in the given interval [`xmin`, `xmax`] (interval is `xmin` and `xmax` inclusive) and is closest to the input value `x`.  

#### Syntax

`res = [[stdlib_math(module):clip(interface)]] (x, xmin, xmax)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument(s)

`x`: scalar of either `integer` or `real` type. This argument is `intent(in)`.  
`xmin`: scalar of either `integer` or `real` type. This argument is `intent(in)`.  
`xmax`: scalar of either `integer` or `real` type, which must be greater than or equal to `xmin`. This argument is `intent(in)`.

Note: All arguments must have same `type` and same `kind`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of the arguments.

#### Examples

##### Example 1:

Here inputs are of type `integer` and kind `int32`
```fortran
program demo_clip_integer
  use stdlib_math, only: clip
  use stdlib_kinds, only: int32
  implicit none
  integer(int32) :: x
  integer(int32) :: xmin
  integer(int32) :: xmax
  integer(int32) :: clipped_value

  xmin = -5_int32
  xmax = 5_int32
  x = 12_int32

  clipped_value = clip(x, xmin, xmax)
  ! clipped_value <- 5
end program demo_clip_integer
```

##### Example 2:

Here inputs are of type `real` and kind `sp`
```fortran
program demo_clip_real
  use stdlib_math, only: clip
  use stdlib_kinds, only: sp
  implicit none
  real(sp) :: x
  real(sp) :: xmin
  real(sp) :: xmax
  real(sp) :: clipped_value

  xmin = -5.769_sp
  xmax = 3.025_sp
  x = 3.025_sp

  clipped_value = clip(x, xmin, xmax)
  ! clipped_value <- 3.02500010
end program demo_clip_real
```
