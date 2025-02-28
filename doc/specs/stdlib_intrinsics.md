---
title: intrinsics
---

# The `stdlib_intrinsics` module

[TOC]

## Introduction

The `stdlib_intrinsics` module provides replacements for some of the well known intrinsic functions found in Fortran compilers for which either a faster and/or more accurate implementation is found which has also proven of interest to the Fortran community.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_sum` function

#### Description

The `stdlib_sum` function can replace the intrinsic `sum` for `real`, `complex` or `integer` arrays. It follows a chunked implementation which maximizes vectorization potential as well as reducing the round-off error. This procedure is recommended when summing large (e..g, >2**10 elements) arrays, for repetitive summation of smaller arrays consider the classical `sum`.

#### Syntax

`res = ` [[stdlib_intrinsics(module):stdlib_sum(interface)]] ` (x [,mask] )`

`res = ` [[stdlib_intrinsics(module):stdlib_sum(interface)]] ` (x, dim [,mask] )`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: N-D array of either `real`, `complex` or `integer` type. This argument is `intent(in)`.

`dim` (optional): scalar of type `integer` with a value in the range from 1 to n, where n equals the rank of `x`.

`mask` (optional): N-D array of `logical` values, with the same shape as `x`. This argument is `intent(in)`.

#### Output value or Result value

If `dim` is absent, the output is a scalar of the same `type` and `kind` as to that of `x`. Otherwise, an array of rank n-1, where n equals the rank of `x`, and a shape similar to that of `x` with dimension `dim` dropped is returned.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_sum_kahan` function

#### Description

The `stdlib_sum_kahan` function can replace the intrinsic `sum` for `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential complemented by an `elemental` kernel based on the [kahan summation](https://doi.org/10.1145%2F363707.363723) strategy to reduce the round-off error:

```fortran
elemental subroutine kahan_kernel_<kind>(a,s,c)
    type(<kind>), intent(in) :: a
    type(<kind>), intent(inout) :: s
    type(<kind>), intent(inout) :: c
    type(<kind>) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = t
end subroutine
```

#### Syntax

`res = ` [[stdlib_intrinsics(module):stdlib_sum_kahan(interface)]] ` (x [,mask] )`

`res = ` [[stdlib_intrinsics(module):stdlib_sum_kahan(interface)]] ` (x, dim [,mask] )`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.

`dim` (optional): scalar of type `integer` with a value in the range from 1 to n, where n equals the rank of `x`.

`mask` (optional): N-D array of `logical` values, with the same shape as `x`. This argument is `intent(in)`.

#### Output value or Result value

If `dim` is absent, the output is a scalar of the same type and kind as to that of `x`. Otherwise, an array of rank n-1, where n equals the rank of `x`, and a shape similar to that of `x` with dimension `dim` dropped is returned.

#### Example

```fortran
{!example/intrinsics/example_sum.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_dot_product` function

#### Description

The `stdlib_dot_product` function can replace the intrinsic `dot_product` for 1D `real`, `complex` or `integer` arrays. It follows a chunked implementation which maximizes vectorization potential as well as reducing the round-off error. This procedure is recommended when crunching large arrays, for repetitive products of smaller arrays consider the classical `dot_product`.

#### Syntax

`res = ` [[stdlib_intrinsics(module):stdlib_dot_product(interface)]] ` (x, y)`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real`, `complex` or `integer` type. This argument is `intent(in)`.

`y`: 1D array of the same type and kind as `x`. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x` and `y`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_dot_product_kahan` function

#### Description

The `stdlib_dot_product_kahan` function can replace the intrinsic `dot_product` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential, complemented by the same `elemental` kernel based on the [kahan summation](https://doi.org/10.1145%2F363707.363723) used for `stdlib_sum` to reduce the round-off error.

#### Syntax

`res = ` [[stdlib_intrinsics(module):stdlib_dot_product_kahan(interface)]] ` (x, y)`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.

`y`: 1D array of the same type and kind as `x`. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of the same type and kind as to that of `x` and `y`.

```fortran
{!example/intrinsics/example_dot_product.f90!}
```