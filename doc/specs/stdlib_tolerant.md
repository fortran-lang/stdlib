---
title: tolerant
---

# Tolerant comparison of reals

[TOC]

## Introduction

The age-old adagium is that you should never compare real variables directly, as rounding/truncation errors can
give false results. This module provides the means to allow a certain tolerance with such comparisons. In addition
it provides tolerant versions of the `floor`, `ceil` and `round` functions. The operation is in most respects
simple, in that a margin is used which is based on the "epsilon" value for the given kind of reals.


## Procedures and operators provided

The module's procedures and operators work on ordinary single-precision and double-precision reals.

### `teq`, `tne`, `tgt`, `tge`, `tlt`, `tle` - Tolerant relational operators

#### Description

Compare two reals with a tolerance based on a small margin (set to three times the epsilon value).
The implementation is such that if `x .teq. y` is true, then `x .tne. y` is never true and vice versa.
Similarly for the pairs `.tlt.` and `.tge.`, `.tle.` and `.tgt.`.

#### Syntax

`if ( x .teq. y ) write(*,*) 'x and y are equal'`

#### Status

Experimental

#### Class

Operator.

#### Arguments

`x`, `y`: Two reals (of the same kind) to be compared

#### Result value

A logical indicating whether the two operands are different enough or not.


### `tfloor`, `tceil`, `tround` - Tolerant versions of the `floor`, `ceil` and `round` functions

#### Description

Provide tolerant versions of the `floor`, `ceil` and `round` functions that take a small interval into account.
While the actual interval can be set, the advised default is three times epsilon(). Note that the implementation
is actually much more involved  than would seem necessary. It is the result of extensive research.


#### Syntax

```fortran
    fl = [[stdlib_tolerant(module):tfloor(interface)]](x)  ! Or: tfloor( x, ct ) if you want control over the interval
    cl = [[stdlib_tolerant(module):tfceil(interface)]](x)  ! Or: tceil( x, ct )
    rnd = [[stdlib_tolerant(module):tround(interface)]](x) ! Or: tround( x, ct )
```

#### Status

Experimental

#### Class

Elemental function

#### Arguments

`x`: The real number that is to be truncated or rounded

`ct`: Tolearance for comparison (optional, defaults to 3*epsilon)

#### Return value

A real value of the same kind as the argument but with proper truncation or rounding in accordance
with the function.

