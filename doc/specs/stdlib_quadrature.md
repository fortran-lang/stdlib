---
title: quadrature
---

# Numerical integration

[TOC]

## `trapz` - integrate sampled values using trapezoidal rule

### Status

Experimental

### Description

Returns the trapezoidal rule integral of an array `y` representing discrete samples of a function. The integral is computed assuming either equidistant abscissas with spacing `dx` or arbitrary abscissas `x`.

### Syntax

`result = ` [[stdlib_quadrature(module):trapz(interface)]] `(y, x)`

`result = ` [[stdlib_quadrature(module):trapz(interface)]] `(y, dx)`

### Arguments

`y`: Shall be a rank-one array of type `real`.

`x`: Shall be a rank-one array of type `real` having the same kind and size as `y`. 

`dx`: Shall be a scalar of type `real` having the same kind as `y`.

### Return value

The result is a scalar of type `real` having the same kind as `y`.

If the size of `y` is zero or one, the result is zero.

### Example

```fortran
{!example/quadrature/example_trapz.f90!}
```

## `trapz_weights` - trapezoidal rule weights for given abscissas

### Status

Experimental

### Description

Given an array of abscissas `x`, computes the array of weights `w` such that if `y` represented function values tabulated at `x`, then `sum(w*y)` produces a trapezoidal rule approximation to the integral.

### Syntax

`result = ` [[stdlib_quadrature(module):trapz_weights(interface)]] `(x)`

### Arguments

`x`: Shall be a rank-one array of type `real`.

### Return value

The result is a `real` array with the same size and kind as `x`.

If the size of `x` is one, then the sole element of the result is zero.

### Example

```fortran
{!example/quadrature/example_trapz_weights.f90!}
```

## `simps` - integrate sampled values using Simpson's rule

### Status

Experimental

### Description

Returns the Simpson's rule integral of an array `y` representing discrete samples of a function. The integral is computed assuming either equidistant abscissas with spacing `dx` or arbitrary abscissas `x`. 

Simpson's ordinary ("1/3") rule is used for odd-length arrays. For even-length arrays, Simpson's 3/8 rule is also utilized in a way that depends on the value of `even`. If `even` is negative (positive), the 3/8 rule is used at the beginning (end) of the array. If `even` is zero or not present, the result is as if the 3/8 rule were first used at the beginning of the array, then at the end of the array, and these two results were averaged.

### Syntax

`result = ` [[stdlib_quadrature(module):simps(interface)]] `(y, x [, even])`

`result = ` [[stdlib_quadrature(module):simps(interface)]] `(y, dx [, even])`

### Arguments

`y`: Shall be a rank-one array of type `real`.

`x`: Shall be a rank-one array of type `real` having the same kind and size as `y`. 

`dx`: Shall be a scalar of type `real` having the same kind as `y`.

`even`: (Optional) Shall be a default-kind `integer`.

### Return value

The result is a scalar of type `real` having the same kind as `y`.

If the size of `y` is zero or one, the result is zero.

If the size of `y` is two, the result is the same as if `trapz` had been called instead.

### Example

```fortran
{!example/quadrature/example_simps.f90!}
```

## `simps_weights` - Simpson's rule weights for given abscissas

### Status

Experimental

### Description

Given an array of abscissas `x`, computes the array of weights `w` such that if `y` represented function values tabulated at `x`, then `sum(w*y)` produces a Simpson's rule approximation to the integral.

Simpson's ordinary ("1/3") rule is used for odd-length arrays. For even-length arrays, Simpson's 3/8 rule is also utilized in a way that depends on the value of `even`. If `even` is negative (positive), the 3/8 rule is used at the beginning (end) of the array and the 1/3 rule used elsewhere. If `even` is zero or not present, the result is as if the 3/8 rule were first used at the beginning of the array, then at the end of the array, and then these two results were averaged.

### Syntax

`result = ` [[stdlib_quadrature(module):simps_weights(interface)]] `(x [, even])`

### Arguments

`x`: Shall be a rank-one array of type `real`.

`even`: (Optional) Shall be a default-kind `integer`.

### Return value

The result is a `real` array with the same size and kind as `x`.

If the size of `x` is one, then the sole element of the result is zero.

If the size of `x` is two, then the result is the same as if `trapz_weights` had been called instead.

### Example

```fortran
{!example/quadrature/example_simps_weights.f90!}
```

## `gauss_legendre` - Gauss-Legendre quadrature (a.k.a. Gaussian quadrature) nodes and weights

### Status

Experimental

### Description

Computes Gauss-Legendre quadrature (also known as simply Gaussian quadrature) nodes and weights,
 for any `N` (number of nodes).
Using the nodes `x` and weights `w`, you can compute the integral of some function `f` as follows:
`integral = sum(f(x) * w)`.

Only double precision is supported - if lower precision is required, you must do the appropriate conversion yourself.
Accuracy has been validated up to N=64 by comparing computed results to tablulated values known to be accurate to machine precision
(maximum difference from those values is 2 epsilon).

### Syntax

`subroutine ` [[stdlib_quadrature(module):gauss_legendre(interface)]] ` (x, w[, interval])`

### Arguments

`x`: Shall be a rank-one array of type `real(real64)`. It is an *output* argument, representing the quadrature nodes.

`w`: Shall be a rank-one array of type `real(real64)`, with the same dimension as `x`. 
It is an *output* argument, representing the quadrature weights.

`interval`: (Optional) Shall be a two-element array of type `real(real64)`. 
If present, the nodes and weigts are calculated for integration from `interval(1)` to `interval(2)`.
If not specified, the default integral is -1 to 1.

### Example

```fortran
{!example/quadrature/example_gauss_legendre.f90!}
```

## `gauss_legendre_lobatto` - Gauss-Legendre-Lobatto quadrature nodes and weights

### Status

Experimental

### Description

Computes Gauss-Legendre-Lobatto quadrature nodes and weights,
 for any `N` (number of nodes).
Using the nodes `x` and weights `w`, you can compute the integral of some function `f` as follows:
`integral = sum(f(x) * w)`.

Only double precision is supported - if lower precision is required, you must do the appropriate conversion yourself.
Accuracy has been validated up to N=64 by comparing computed results to tablulated values known to be accurate to machine precision
(maximum difference from those values is 2 epsilon).

### Syntax

`subroutine ` [[stdlib_quadrature(module):gauss_legendre_lobatto(interface)]] ` (x, w[, interval])`

### Arguments

`x`: Shall be a rank-one array of type `real(real64)`. It is an *output* argument, representing the quadrature nodes.

`w`: Shall be a rank-one array of type `real(real64)`, with the same dimension as `x`. 
It is an *output* argument, representing the quadrature weights.

`interval`: (Optional) Shall be a two-element array of type `real(real64)`. 
If present, the nodes and weigts are calculated for integration from `interval(1)` to `interval(2)`.
If not specified, the default integral is -1 to 1.

### Example

```fortran
{!example/quadrature/example_gauss_legendre_lobatto.f90!}
```
