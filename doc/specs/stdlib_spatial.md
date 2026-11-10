---
title: spatial
---

# The `stdlib_spatial` module

This module provides implementations of algorithms for spatial data processing.

[TOC]

## `kabsch_umeyama` - Finding optimal rotation matrix

### Status

Experimental

### Description

Compute the optimal similarity transform (Kabsch–Umeyama):
\[
    P \approx c \, R \, Q + t
\]
where:

- R is an orthogonal rotation matrix,
- c is an optional scaling factor,
- t is a translation vector.

The transformation minimizes the root-mean-square deviation (RMSD) between corresponding columns
of P and Q, optionally using weights and with optional scaling.
The implementation is based on the algorithm described here: [Aligning point patterns with Kabsch–Umeyama algorithm](https://zpl.fi/aligning-point-patterns-with-kabsch-umeyama-algorithm)

### Syntax

`call ` [[stdlib_spatial(module):kabsch_umeyama(interface)]] `(P, Q, R, t, c, rmsd [, W, scale])`

### Arguments

`P`: Shall be a `real` or `complex` rank-2 array. It is an `intent(in)` argument.

`Q`: Shall be a rank-2 array with same kind as `P`. It is an `intent(in)` argument.

`R`: Shall be a rank-2 array with same kind as `P`. For `real` kinds, the algorithm returns a proper rotation matrix, meaning `det(R) = 1`. It is an `intent(out)` argument.

`t`: Shall be a rank-1 array with same kind as `P`. It is an `intent(out)` argument.

`c`: Scalar value of the same type as `P`. It is an `intent(out)` argument. If `scale` is disabled `c` will be returned with a value of `1`.

`rmsd`: Scalar value of `real` kind. It is an `intent(out)` argument.

`W` (optional): Shall be a rank-1 array of `real` kind. It is an `intent(in)` argument. By default, `W` is an array of `1`s.

`scale` (optional): Shall be a logical type. It is an `intent(in)` argument. By default, `scale = .true.`.

### Example

```fortran
{!example/spatial/example_kabsch_umeyama.f90!}
```