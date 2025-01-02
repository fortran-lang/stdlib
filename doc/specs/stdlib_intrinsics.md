---
title: intrinsics
---

# The `stdlib_intrinsics` module

[TOC]

## Introduction

The `stdlib_intrinsics` module provides replacements for some of the well known intrinsic functions found in Fortran compilers for which either a faster and/or more accurate implementation is found which has also proven of interest to the Fortran community.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `fsum` function

#### Description

The `fsum` function can replace the intrinsic `sum` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential as well as reducing the round-off error. This procedure is recommended when summing large arrays, for repetitive summation of smaller arrays consider the classical `sum`.

#### Syntax

`res = ` [[stdlib_intrinsics(module):fsum(interface)]] ` (x [,mask] )`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.
`mask` (optional): 1D array of `logical` values. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x`.

#### Example

```fortran
{!example/math/example_intrinsics_sum.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `fsum_kahan` function

#### Description

The `fsum_kahan` function can replace the intrinsic `sum` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential, complemented by an `elemental` kernel based on the [kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) strategy to reduce the round-off error:

```fortran
elemental subroutine vkahan_<kind>(a,s,c)
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

`res = ` [[stdlib_intrinsics(module):fsum_kahan(interface)]] ` (x [,mask] )`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.
`mask` (optional): 1D array of `logical` values. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x`.

#### Example

```fortran
{!example/math/example_intrinsics_sum.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `fprod` function

#### Description

The `fprod` function can replace the intrinsic `dot_product` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential as well as reducing the round-off error. This procedure is recommended when crunching large arrays, for repetitive products of smaller arrays consider the classical `dot_product`.

#### Syntax

`res = ` [[stdlib_intrinsics(module):fprod(interface)]] ` (x, y)`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.
`y`: 1D array of the same type and kind as `x`. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x` and `y`.

#### Example

```fortran
{!example/math/example_intrinsics_dot_duct.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `fprod_kahan` function

#### Description

The `fprod_kahan` function can replace the intrinsic `dot_product` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximizes vectorization potential , complemented by the same `elemental` kernel based on the [kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) used for `fsum` to reduce the round-off error.

#### Syntax

`res = ` [[stdlib_intrinsics(module):fprod_kahan(interface)]] ` (x, y)`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.
`y`: 1D array of the same type and kind as `x`. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x` and `y`.

```fortran
{!example/math/example_intrinsics_dot_duct.f90!}
```