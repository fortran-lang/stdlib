---
title: stats_distribution_uniform
---

# Statistical Distributions -- Uniform Distribution Module

[TOC]

## `shuffle` - Using Fisher-Yates algorithm to generate a random permutation of a list

### Status

Experimental

### Description

Applying Fisher-Yates algorithm to generate an unbiased permutation for any list of intrinsic numerical data types.

### Syntax

`result = ` [[stdlib_stats_distribution_uniform(module):shuffle(interface)]] `( list )`

### Class

Function.

### Arguments

`list`: argument has `intent(in)` and is a rank one array of `integer`, `real`, or `complex` type.

### Return value

Return a randomized rank one array of the input type.

### Example

```fortran
{!example/stats_distribution_uniform/example_shuffle.f90!}
```

## `rvs_uniform` - uniform distribution random variates

### Status

Experimental

### Description

Without argument the function returns a scalar standard uniformly distributed variate U(0,1) of `real` type with single precision on [0,1].

With single argument `scale` of `integer` type the function returns a scalar uniformly distributed variate of `integer` type on [0,scale]. This is the standard Rectangular distribution.

With single argument `scale` of `real` or `complex` type the function returns a scalar uniformly distributed variate of `real` type on [0, scale] or `complex` type on [(0, 0i), (scale, i(scale))].

With double arguments `loc` and `scale` the function returns a scalar uniformly distributed random variates of `integer` or `real` type on [loc, loc + scale], or `complex` type on [(loc, i(loc)), ((loc + scale), i(loc + scale))], dependent of input type.

With triple arguments `loc`, `scale` and `array_size` the function returns a rank one array of uniformly distributed variates of `integer`, `real` or `complex` type with an array size of `array_size`.

For `complex` type, the real part and imaginary part are independent of each other.

Note: the algorithm used for generating uniform random variates is fundamentally limited to double precision.

### Syntax

`result = ` [[stdlib_stats_distribution_uniform(module):rvs_uniform(interface)]] `([[loc,] scale] [[[,array_size]]])`

### Class

Elemental function (without the third argument).

### Arguments

`loc`: optional argument has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: optional argument has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

`loc` and `scale` must have the same type and kind when both are present.

### Return value

The result is a scalar or a rank one array with size of `array_size`, of type `integer`, `real` or `complex` depending on the input type.

### Example

```fortran
{!example/stats_distribution_uniform/example_uniform_rvs.f90!}
```

## `pdf_uniform` - Uniform distribution probability density function

### Status

Experimental

### Description

The probability density function of the uniform distribution:

f(x) = 0       x < loc or x > loc + scale  for all types uniform distributions

For random variable x in [loc, loc + scale]:

f(x) = 1 / (scale + 1);            for discrete uniform distribution.

f(x) = 1 / scale;                  for continuous uniform distribution.

f(x) = 1 / (scale%re * scale%im);  for complex uniform distribution.

### Syntax

`result = ` [[stdlib_stats_distribution_uniform(module):pdf_uniform(interface)]] `(x, loc, scale)`

### Class

Elemental function.

### Arguments

`x`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

All three arguments must have the same type and kind.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, of type `real`.

### Example

```fortran
{!example/stats_distribution_uniform/example_uniform_pdf.f90!}
```

## `cdf_uniform` - Uniform distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the uniform distribution:

F(x) = 0             x < loc             for all types uniform distributions

F(x) = 1             x > loc + scale     for all types uniform distributions

For random variable x in [loc, loc + scale]:

F(x) = (x - loc + 1) / (scale + 1);      for discrete uniform distribution.

F(x) = (x - loc) / scale;                for continuous uniform distribution.

F(x) = (x%re - loc%re)(x%im - loc%im) / (scale%re * scale%im); for complex uniform distribution.

### Syntax

`result = ` [[stdlib_stats_distribution_uniform(module):cdf_uniform(interface)]] `(x, loc, scale)`

### Class

Elemental function.

### Arguments

`x`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

All three arguments must have the same type and kind.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, of type `real`.

### Example

```fortran
{!example/stats_distribution_uniform/example_uniform_cdf.f90!}
```
