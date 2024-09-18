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

`res = ` [[stdlib_math(module):clip(interface)]] ` (x, xmin, xmax)`

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
{!example/math/example_clip_integer.f90!}
```

##### Example 2:

Here inputs are of type `real` and kind `sp`
```fortran
{!example/math/example_clip_real.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `swap` subroutine

#### Description

Swaps the values in `lhs` and `rhs`.

#### Syntax

`call` [[stdlib_math(module):swap(interface)]] ` (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental subroutine.

#### Argument(s)

`lhs`: scalar or array of any of the intrinsic types `integer`, `real`, `complex`, `logical`, `character`, `string_type`, `bitset` type. This argument is `intent(inout)`.
`rhs`: scalar or array of any of the intrinsic types `integer`, `real`, `complex`, `logical`, `character`, `string_type`, `bitset` type. This argument is `intent(inout)`.

##### Note
All arguments must have same `type` and same `kind`.

**WARNING**: For fix size characters with different length, the `swap` subroutine will truncate the longest amongst `lhs` and `rhs`. To avoid truncation it is possible to pass a subsection of the string.

#### Examples

```fortran
{!example/math/example_math_swap.f90!}
```

### `gcd` function

#### Description

Returns the greatest common divisor of two integers.

#### Syntax

`res = ` [[stdlib_math(module):gcd(interface)]] ` (a, b)`

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
{!example/math/example_gcd.f90!}
```

### `linspace` - Create a linearly spaced rank one array

#### Description

Returns a linearly spaced rank 1 array from [`start`, `end`]. Optionally, you can specify the length of the returned array by passing `n`.

#### Syntax

`res = ` [[stdlib_math(module):linspace(interface)]] ` (start, end [, n])`

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
{!example/math/example_linspace_complex.f90!}
```

##### Example 2:

Here inputs are of type `integer` and kind `int16`, with the result defaulting to `real(dp)`.
```fortran
{!example/math/example_linspace_int16.f90!}
```

### `logspace` - Create a logarithmically spaced rank one array

#### Description

Returns a logarithmically spaced rank 1 array from [`base`^`start`, `base`^`end`]. The default size of the array is 50. Optionally, you can specify the length of the returned array by passing `n`. You can also specify the `base` used to compute the range (default 10).

#### Syntax

`res = ` [[stdlib_math(module):logspace(interface)]] ` (start, end [, n [, base]])`

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
{!example/math/example_logspace_complex.f90!}
```

##### Example 2:

Here inputs are of type `integer` and default kind. `base` is not specified and thus defaults to 10.
```fortran
{!example/math/example_logspace_int.f90!}
```

##### Example 3:

Here `start`/`end` are of type `real` and double precision. `base` is type `complex` and also double precision.
```fortran
{!example/math/example_logspace_rstart_cbase.f90!}
```
### `arange` function

#### Status

Experimental

#### Class

Pure function.

#### Description

Creates a rank-1 `array` of the `integer/real` type with fixed-spaced values of given spacing, within a given interval.

#### Syntax

`result = ` [[stdlib_math(module):arange(interface)]] `(start [, end, step])`

#### Arguments

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

##### Warning
If `step = 0`, the `step` argument will be corrected to `1/1.0` by the internal process of the `arange` function.   
If `step < 0`, the `step` argument will be corrected to `abs(step)` by the internal process of the `arange` function. 

#### Return value

Returns a rank-1 `array` of fixed-spaced values.

For `integer` type arguments, the length of the result vector is `(end - start)/step + 1`.  
For `real` type arguments, the length of the result vector is `floor((end - start)/step) + 1`.

#### Example

```fortran
{!example/math/example_math_arange.f90!}
```

### `arg` function

#### Status

Experimental

#### Class

Elemental function.

#### Description

`arg` computes the phase angle (radian version) of `complex` scalar in the interval (-π,π]. 
The angles in `θ` are such that `z = abs(z)*exp((0.0, θ))`.

#### Syntax

`result = ` [[stdlib_math(module):arg(interface)]] `(z)`

#### Arguments

`z`: Shall be a `complex` scalar/array.
This is an `intent(in)` argument.

#### Return value

Returns the `real` type phase angle (radian version) of the `complex` argument `z`.

Notes: Although the angle of the complex number `0` is undefined, `arg((0,0))` returns the value `0`.

#### Example

```fortran
{!example/math/example_math_arg.f90!}
```

### `argd` function

#### Status

Experimental

#### Class

Elemental function.

#### Description

`argd` computes the phase angle (degree version) of `complex` scalar in the interval (-180.0,180.0]. 
The angles in `θ` are such that `z = abs(z)*exp((0.0, θ*π/180.0))`.

#### Syntax

`result = ` [[stdlib_math(module):argd(interface)]] `(z)`

#### Arguments

`z`: Shall be a `complex` scalar/array.
This is an `intent(in)` argument.

#### Return value

Returns the `real` type phase angle (degree version) of the `complex` argument `z`.

Notes: Although the angle of the complex number `0` is undefined, `argd((0,0))` returns the value `0`.

#### Example

```fortran
{!example/math/example_math_argd.f90!}
```

### `argpi` function

#### Status

Experimental

#### Class

Elemental function.

#### Description

`argpi` computes the phase angle (IEEE circular version) of `complex` scalar in the interval (-1.0,1.0]. 
The angles in `θ` are such that `z = abs(z)*exp((0.0, θ*π))`.

#### Syntax

`result = ` [[stdlib_math(module):argpi(interface)]] `(z)`

#### Arguments

`z`: Shall be a `complex` scalar/array.
This is an `intent(in)` argument.

#### Return value

Returns the `real` type phase angle (circular version) of the `complex` argument `z`.

Notes: Although the angle of the complex number `0` is undefined, `argpi((0,0))` returns the value `0`.

#### Example

```fortran
{!example/math/example_math_argpi.f90!}
```

### `deg2rad`

#### Status

Experimental

#### Class

Elemenal function.

### Description

`deg2rad` converts phase angles from degrees to radians.

#### Syntax

`result = ` [[stdlib_math(module):deg2rad(interface)]] `(theta)`

#### Arguments

`theta`: Shall be a `real` scalar/array.

#### Return value

Returns the `real` phase angle in radians.

#### Example

```fortran
{!example/math/example_math_deg2rad.f90!}
```

### `rad2deg`

#### Status

Experimental

#### Class

Elemenal function.

### Description

`rad2deg` converts phase angles from radians to degrees.

#### Syntax

`result = ` [[stdlib_math(module):rad2deg(interface)]] `(theta)`

#### Arguments

`theta`: Shall be a `real` scalar/array.

#### Return value

Returns the `real` phase angle in degrees.

#### Example

```fortran
{!example/math/example_math_rad2deg.f90!}
```

### `is_close` function

#### Description

Returns a boolean scalar/array where two scalars/arrays are element-wise equal within a tolerance.

```fortran
!> For `real` type
is_close(a, b, rel_tol, abs_tol) = abs(a - b) <= max(rel_tol*(abs(a), abs(b)), abs_tol)

!> and for `complex` type
is_close(a, b, rel_tol, abs_tol) = is_close(a%re, b%re, rel_tol, abs_tol) .and. &
                                   is_close(a%im, b%im, rel_tol, abs_tol)
```

#### Syntax

`bool = ` [[stdlib_math(module):is_close(interface)]] ` (a, b [, rel_tol, abs_tol, equal_nan])`

#### Status

Experimental.

#### Class

Elemental function.

#### Arguments

Note: All `real/complex` arguments must have same `kind`.  
If the value of `rel_tol/abs_tol` is negative (not recommended), 
it will be corrected to `abs(rel_tol/abs_tol)` by the internal process of `is_close`.

`a`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`b`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`rel_tol`: Shall be a `real` scalar/array.
This argument is `intent(in)` and `optional`, which is `sqrt(epsilon(..))` by default.

`abs_tol`: Shall be a `real` scalar/array.
This argument is `intent(in)` and `optional`, which is `0.0` by default.

`equal_nan`: Shall be a `logical` scalar/array.
This argument is `intent(in)` and `optional`, which is `.false.` by default.
Whether to compare `NaN` values as equal. If `.true.`, 
`NaN` values in `a` will be considered equal to `NaN` values in `b`.

#### Result value

Returns a `logical` scalar/array.

#### Example

```fortran
{!example/math/example_math_is_close.f90!}
```

### `all_close` function

#### Description

Returns a boolean scalar where two arrays are element-wise equal within a tolerance.

#### Syntax

`bool = ` [[stdlib_math(module):all_close(interface)]] ` (a, b [, rel_tol, abs_tol, equal_nan])`

#### Status

Experimental.

#### Class

Pure function.

#### Arguments

Note: All `real/complex` arguments must have same `kind`.  
If the value of `rel_tol/abs_tol` is negative (not recommended), 
it will be corrected to `abs(rel_tol/abs_tol)` by the internal process of `all_close`.

`a`: Shall be a `real/complex` array.
This argument is `intent(in)`.

`b`: Shall be a `real/complex` array.
This argument is `intent(in)`.

`rel_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `sqrt(epsilon(..))` by default.

`abs_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `0.0` by default.

`equal_nan`: Shall be a `logical` scalar.
This argument is `intent(in)` and `optional`, which is `.false.` by default.
Whether to compare `NaN` values as equal. If `.true.`, 
`NaN` values in `a` will be considered equal to `NaN` values in `b`.

#### Result value

Returns a `logical` scalar.

#### Example

```fortran
{!example/math/example_math_all_close.f90!}
```

### `diff` function

#### Description

Computes differences between adjacent elements of an array.

#### Syntax

For a rank-1 array:  
`y = ` [[stdlib_math(module):diff(interface)]] `(x [, n, prepend, append])`

and for a rank-2 array:  
`y = ` [[stdlib_math(module):diff(interface)]] `(x [, n, dim, prepend, append])`

#### Status

Experimental.

#### Class

Pure function.

#### Arguments

`x`: The array to take a difference of.
Shall be a `real/integer` and `rank-1/rank-2` array.
This argument is `intent(in)`.

`n`: How many times to iteratively calculate the difference.
Shall be an `integer` scalar.
This argument is `intent(in)` and `optional`, and has value of `1` by default.

`dim`: The dimension of the input array along which to calculate the difference.
Its value must be between `1` and `rank(x)`.
Shall be an `integer` scalar.
This argument is `intent(in)` and `optional` and has a value of `1` by default.

`prepend`, `append`: Arrays to prepend or append to a along axis prior to performing the difference.
The dimension and shape must match a except along axis.
Shall be a `real/integer` and `rank-1/rank-2` array.
This argument is `intent(in)` and `optional`, which is no value by default.

Note: 

- The `x`, `prepend` and `append` arguments must have the same `type`, `kind` and `rank`.  
- If the value of `n` is less than or equal to `0` (which is not recommended), the return value of `diff` is `x`.  
- If the value of `dim` is not equal to `1` or `2` (which is not recommended),
`1` will be used by the internal process of `diff`.


#### Result value

Returns the finite difference of the input array.
Shall be a `real/integer` and `rank-1/rank-2` array.
When both `prepend` and `append` are not present, the result `y` has one fewer element than `x` alongside the dimension `dim`.

#### Example

```fortran
{!example/math/example_diff.f90!}
```

### `meshgrid` subroutine

#### Description

Computes a list of coordinate matrices from coordinate vectors.

For $n \geq 1$ coordinate vectors $(x_1, x_2, ..., x_n)$ of sizes $(s_1, s_2, ..., s_n)$, `meshgrid` computes $n$ coordinate matrices $(X_1, X_2, ..., X_n)$ with identical shape corresponding to the selected indexing:
- Cartesian indexing (default behavior): the shape of the coordinate matrices is $(s_2, s_1, s_3, s_4, ... s_n)$.
- matrix indexing: the shape of the coordinate matrices is $(s_1, s_2, s_3, s_4, ... s_n)$.

#### Syntax

For a 2D problem in Cartesian indexing:
`call ` [[stdlib_math(module):meshgrid(interface)]] `(x, y, xm, ym)`

For a 3D problem in Cartesian indexing:
`call ` [[stdlib_math(module):meshgrid(interface)]] `(x, y, z, xm, ym, zm)`

For a 3D problem in matrix indexing:
`call ` [[stdlib_math(module):meshgrid(interface)]] `(x, y, z, xm, ym, zm, indexing="ij")`

The subroutine can be called in `n`-dimensional situations, as long as `n` is inferior to the maximum allowed array rank.

#### Status

Experimental.

#### Class

Subroutine.

#### Arguments

For a `n`-dimensional problem, with `n >= 1`:

`x1, x2, ..., xn`: The coordinate vectors.
Shall be `real/integer` and `rank-1` arrays.
These arguments are `intent(in)`.

`xm1, xm2, ..., xmn`: The coordinate matrices.
Shall be arrays of type `real` or `integer` of adequate shape:
- for Cartesian indexing, the shape of the coordinate matrices must be `[size(x2), size(x1), size(x3), ..., size(xn)]`.
- for matrix indexing, the shape of the coordinate matrices must be `[size(x1), size(x2), size(x3), ..., size(xn)]`.

These argument are `intent(out)`.

`indexing`: the selected indexing.
Shall be an `integer` equal to `stdlib_meshgrid_xy` for Cartesian indexing (default), or `stdlib_meshgrid_ij` for matrix indexing. `stdlib_meshgrid_xy` and `stdlib_meshgrid_ij` are public constants defined in the module.
This argument is `intent(in)` and `optional`, and is equal to `stdlib_meshgrid_xy` by default.

#### Example

```fortran
{!example/math/example_meshgrid.f90!}
```
