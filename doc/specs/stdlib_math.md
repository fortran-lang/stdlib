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

Limits the input value `x` to the given interval [`xmin`, `xmax`] (interval is `xmin` and `xmax` inclusive). Returns a value which lies in the given interval and is closest to the input value `x`.  
If the input value `x` already lies in the given interval, then the output value will be equal to the input value.

#### Syntax

`res = [[stdlib_math(module):clip(interface)]] (x, xmin, xmax)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument(s)

`x`: scalar of either `integer` or `real`. This argument is `intent(in)`.  
`xmin`: scalar of either `integer` or `real`. This argument is `intent(in)`.  
`xmax`: scalar of either `integer` or `real`, which must be greater than or equal to `xmin`. This argument is `intent(in)`.

Note: All arguments must have same `type` and same `kind`.

#### Output value or Result value

Output is a scalar of either `integer` or `real` depending on the arguments. The output value will have `type` and `kind` same as to that of the arguments.

#### Examples

##### Example 1:

Here inputs are of type `integer` and kind `int32`
```fortran
program demo
  use stdlib_math
  use iso_fortran_env
  implicit none
  integer(int32) :: x
  integer(int32) :: xmin
  integer(int32) :: xmax
  integer(int32) :: clipped_value

  xmin = -5_int32
  ! xmin <- -5
  xmax = 5_int32
  ! xmax <- 5
  x = 12_int32
  ! x <- 12

  clipped_value = clip(x, xmin, xmax)
  ! clipped_value <- 5
end program demo
```

##### Example 2:

Here inputs are of type `real` and kind `real32` (or `sp`)
```fortran
program demo
  use stdlib_math
  use iso_fortran_env
  implicit none
  real(real32) :: x
  real(real32) :: xmin
  real(real32) :: xmax
  real(real32) :: clipped_value

  xmin = -5.769_real32
  ! xmin <-  -5.76900005
  xmax = 3.025_real32
  ! xmax <- 3.02500010
  x = 3.025_real32
  ! x <- 3.02500010

  clipped_value = clip(x, xmin, xmax)
  ! clipped_value <- 3.02500010
end program demo
```
