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

### `gcd` function

#### Description

Returns the greatest common divisor of two integers.

#### Syntax

`res = [[stdlib_math(module):gcd(interface)]] (a, b)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument(s)

`a`: One integer with `intent(in)` to get the divisor for.
`b`: Another integer with `intent(in)` to get the divisor for.

Note: All arguments must be integers of the same `kind`.

#### Output value or Result value

Returns an integer of the same `kind` as that of the arguments.

#### Examples

##### Example 1:

```fortran
program demo_gcd
  use stdlib_math, only: gcd
  implicit none
  integer :: a, b, c
  
  a = 48
  b = 18
  c = gcd(a, b) ! returns 6
end program demo_gcd
```

### `linspace` - Create a linearly spaced rank one array

#### Description

Returns a linearly spaced rank 1 array from [`start`, `end`]. Optionally, you can specify the length of the returned array by passing `n`.

#### Syntax

`res = [[stdlib_math(module):linspace(interface)]] (start, end [, n])`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`start`: Shall be scalar of any numeric type or kind. This argument is `intent(in)`.
`end`: Shall be the same `type` and `kind` as `start`. This argument is `intent(in)`.
`n`: Shall be an integer specifying the length of the output. This argument is `optional` and `intent(in)`.

#### Output value or Result value

The output is a rank 1 array whose length is either 100 (default value) or `n`.

If `n` == 1, return a rank 1 array whose only element is `end`.
If `n` <= 0, return a rank 1 array with length 0.

If `start`/`end` are `real` or `complex` types, the `result` will be of the same type and kind as `start`/`end`.
If `start`/`end` are `integer` types, the `result` will default to a `real(dp)` array.

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

Here inputs are of type `integer` and kind `int16`, with the result defaulting to `real(dp)`.
```fortran
program demo_linspace_int16
  use stdlib_math, only: linspace
  use stdlib_kinds, only: int16, dp
  implicit none

  integer(int16) :: start = 10_int16
  integer(int16) :: end = 23_int16

  real(dp) :: r(15)

  r = linspace(start, end, 15)
end program demo_linspace_int16
```

### `logspace` - Create a logarithmically spaced rank one array

#### Description

Returns a logarithmically spaced rank 1 array from [`base`^`start`, `base`^`end`]. The default size of the array is 50. Optionally, you can specify the length of the returned array by passing `n`. You can also specify the `base` used to compute the range (default 10).

#### Syntax

`res = [[stdlib_math(module):logspace(interface)]] (start, end [, n [, base]])`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`start`: Shall be a scalar of any numeric type. All kinds are supported for real and complex arguments. For integers, only the default kind is currently implemented. This argument is `intent(in)`.
`end`: Shall be the same `type` and `kind` as `start`. This argument is `intent(in)`.
`n`: Shall be an integer specifying the length of the output. This argument is `optional` and `intent(in)`.
`base` : Shall be a scalar of any numeric type. All kinds are supported for real and complex arguments. For integers, only the default kind is currently implemented. This argument is `optional` and `intent(in)`.

#### Output value or Result value

The output is a rank 1 array whose length is either 50 (default value) or `n`.

If `n` == 1, return a rank 1 array whose only element is `base`^`end`.
If `n` <= 0, return a rank 1 array with length 0

The `type` and `kind` of the output is dependent on the `type` and `kind` of the passed parameters.

For function calls where the `base` is not specified: `logspace(start, end)`/`logspace(start, end, n)`, the `type` and `kind` of
the output follows the same scheme as above for `linspace`.
>If `start`/`end` are `real` or `complex` types, the `result` will be the same type and kind as `start`/`end`.
>If `start`/`end` are integer types, the `result` will default to a `real(dp)` array.

For function calls where the `base` is specified, the `type` and `kind` of the result is in accordance with the following table:

| `start`/`end` | `n` | `base` | `output` |
| ------------- | --- | ------ | -------- |
| `real(KIND)` | `Integer` | `real(KIND)` | `real(KIND)` |
| "          " | "       " | `complex(KIND)` | `complex(KIND)` |
| "          " | "       " | `Integer` | `real(KIND)` |
| `complex(KIND)` | "       " | `real(KIND)` | `complex(KIND)` |
| "             " | "       " | `complex(KIND)` | `complex(KIND)` |
| "             " | "       " | `Integer` | `complex(KIND)` |
| `Integer` | "        " | `real(KIND)` | `real(KIND)` |
| "              " | "        " | `complex(KIND)` | `complex(KIND)` |
| "              " | "        " | `Integer` | `Integer` |

#### Examples

##### Example 1:

Here inputs are of type `complex` and kind `dp`. `n` and `base` is not specified and thus default to 50 and 10, respectively.
```fortran
program demo_logspace_complex
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  complex(dp) :: start = (10.0_dp, 5.0_dp)
  complex(dp) :: end = (-10.0_dp, 15.0_dp)

  complex(dp) :: z(11) ! Complex values raised to complex powers results in complex values

  z = logspace(start, end, 11)
end program demo_logspace_complex
```

##### Example 2:

Here inputs are of type `integer` and default kind. `base` is not specified and thus defaults to 10.
```fortran
program demo_logspace_int
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  integer :: start = 10
  integer :: end = 23
  integer :: n = 15

  real(dp) :: r(n) ! Integer values raised to real powers results in real values

  r = logspace(start, end, n)
end program demo_logspace_int
```

##### Example 3:

Here `start`/`end` are of type `real` and double precision. `base` is type `complex` and also double precision.
```fortran
program demo_logspace_rstart_cbase
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  real(dp) :: start = 0.0_dp
  real(dp) :: end = 3.0_dp
  integer :: n = 4
  complex(dp) :: base = (0.0_dp, 1.0_dp)

  complex(dp) :: z(n) ! complex values raised to real powers result in complex values

  z = logspace(start, end, n, base)

end program demo_logspace_rstart_cbase
```
## `arange`

### Status

Experimental

### Class

Pure function.

### Description

Creates a one-dimensional `array` of the `integer/real` type with fixed-spaced values of given spacing, within a given interval.

### Syntax

`result = [[stdlib_math(module):arange(interface)]](start [, end, step])`

### Arguments

All arguments should be the same type and kind.

`start`: Shall be an `integer/real` scalar.
This is an `intent(in)` argument.  
The default `start` value is `1`.

`end`: Shall be an `integer/real` scalar.
This is an `intent(in)` and `optional` argument.  
The default `end` value is the inputted `start` value.

`step`: Shall be an `integer/real` scalar and large than `0`. 
This is an `intent(in)` and `optional` argument.   
The default `step` value is `1`.

#### Warning
If `step = 0`, the `step` argument will be corrected to `1/1.0` by the internal process of the `arange` function.   
If `step < 0`, the `step` argument will be corrected to `abs(step)` by the internal process of the `arange` function. 

### Return value

Returns a one-dimensional `array` of fixed-spaced values.

For `integer` type arguments, the length of the result vector is `(end - start)/step + 1`.  
For `real` type arguments, the length of the result vector is `floor((end - start)/step) + 1`.

### Example

```fortran
program demo_math_arange
    use stdlib_math, only: arange

    print *, arange(3)                 !! [1,2,3]
    print *, arange(-1)                !! [1,0,-1]
    print *, arange(0,2)               !! [0,1,2]
    print *, arange(1,-1)              !! [1,0,-1]
    print *, arange(0, 2, 2)           !! [0,2]

    print *, arange(3.0)               !! [1.0,2.0,3.0]
    print *, arange(0.0,5.0)           !! [0.0,1.0,2.0,3.0,4.0,5.0]
    print *, arange(0.0,6.0,2.5)       !! [0.0,2.5,5.0]

    print *, (1.0,1.0)*arange(3)       !! [(1.0,1.0),(2.0,2.0),[3.0,3.0]]

    print *, arange(0.0,2.0,-2.0)      !! [0.0,2.0].     Not recommended: `step` argument is negative!
    print *, arange(0.0,2.0,0.0)       !! [0.0,1.0,2.0]. Not recommended: `step` argument is zero!

end program demo_math_arange
```
