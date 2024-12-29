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

The `fsum` function can replace the intrinsic `sum` for 1D `real` or `complex` arrays. It follows a chunked implementation which maximaxes vectorization potential as well as reducing the round-off error.

#### Syntax

`res = ` [[stdlib_intrinsics(module):fsum(interface)]] ` (x [,mask] )`

#### Status

Experimental

#### Class

Pure function.

#### Argument(s)

`x`: 1D array of either `real` or `complex` type. This argument is `intent(in)`.
`mask`: 1D array of `logical` values. This argument is `intent(in)`.

#### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x`.

#### Example

```fortran
{!example/math/example_intrinsics_sum.f90!}
```
