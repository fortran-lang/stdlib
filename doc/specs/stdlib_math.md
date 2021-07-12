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

### `linspace` - Create a linearly spaced rank one array

#### Description

Returns a linearly spaced rank 1 array from [`start`, `end`]. Optionally, you can specify the length of the returned array by passing `n`.

#### Syntax

`res = [[stdlib_math(module):linspace(interface)]] (start, end [, n])`

#### Status

Experimental

#### Class

Function.

#### Argument(s)

`start`: Shall be scalar of any numeric type. This argument is `intent(in)`.
`end`: Shall be the same `type` and `kind` as `start`. This argument is `intent(in)`.
`n`: Shall be an integer specifying the length of the output. This argument is `intent(in)`.

#### Output value or Result value

The output is a rank 1 array of `type` and `kind`, whose length is either 100 (default value) or `n`.

If `n` == 1, return a rank 1 array whose only element is `end`.
If `n` <= 0, return a rank 1 array with length 0

#### Examples

##### Example 1:

Here inputs are of type `complex` and kind `dp`
```fortran
program demo_linspace_complex
  use stdlib_math, only: linspace
  use stdlib_kinds, only: dp
  implicit none

  complex(dp) :: start = complex(10.0_dp, 5.0_dp)
  complex(dp) :: end = complex(-10.0_dp, 15.0_dp)

  complex(dp) :: z(11)

  z = linspace(start, end, 11)
end program demo_linspace_complex
```

##### Example 2:

Here inputs are of type `integer` and kind `int16`
```fortran
program demo_linspace_int16
  use stdlib_math, only: linspace
  use stdlib_kinds, only: int16
  implicit none

  integer(int16) :: start = 10_int16
  integer(int16) :: end = 23_int16

  integer(int16) :: r(15)

  r = linspace(start, end, 15)
end program demo_linspace_int16
```

### `logspace` - Create a logarithmically spaced rank one array

#### Description

Returns a logarithmically spaced rank 1 array from [`base`^`start`, `base`^`end`]. The default size of the array is 100. Optionally, you can specify the length of the returned array by passing `n`.

#### Syntax

`res = [[stdlib_math(module):logspace(interface)]] (start, end [, n [, base]])`

#### Status

Experimental

#### Class

Function.

#### Argument(s)

`start`: Shall be a scalar of any numeric type. This argument is `intent(in)`.
`end`: Shall be the same `type` and `kind` as `start`. This argument is `intent(in)`.
`n`: Shall be an integer specifying the length of the output. This argument is `optional` and `intent(in)`.
`base` : Shall be a scalar of any numeric type. This argument is `optional` and `intent(in)`

#### Output value or Result value

The output is a rank 1 array of `type` and `kind`, whose length is either 50 (default value) or `n`.

If `n` == 1, return a rank 1 array whose only element is `base`^`end`.
If `n` <= 0, return a rank 1 array with length 0

#### Examples

##### Example 1:

Here inputs are of type `complex` and kind `dp`
```fortran
program demo_logspace_complex
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  complex(dp) :: start = complex(10.0_dp, 5.0_dp)
  complex(dp) :: end = complex(-10.0_dp, 15.0_dp)

  complex(dp) :: z(11)

  z = logspace(start, end, 11)
end program demo_logspace_complex
```

##### Example 2:

Here inputs are of type `integer` and kind `int16`
```fortran
program demo_logspace_int16
  use stdlib_math, only: logspace
  use stdlib_kinds, only: int16
  implicit none

  integer(int16) :: start = 10_int16
  integer(int16) :: end = 23_int16

  integer(int16) :: r(15)

  r = logspace(start, end, 15)
end program demo_logspace_int16
```
