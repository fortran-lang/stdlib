---
title: stats_distribution_normal
---

# Statistical Distributions -- Normal Distribution Module

[TOC]

## `rvs_normal` - normal distribution random variates

### Status

Experimental

### Description

A normal continuous random variate distribution, also known as Gaussian, or Gauss or Laplace-Gauss distribution. The location `loc` specifies the mean or expectation. The `scale` specifies the standard deviation. 

Without argument the function returns a standard normal distributed random variate N(0,1).

With two arguments, the function returns a normal distributed random variate N(loc, scale^2). For complex arguments, the real and imaginary parts are independent of each other.

With three arguments, the function returns a rank one array of normal distributed random variates.

Note: the algorithm used for generating normal random variates is fundamentally limited to double precision.

### Syntax

`result = [[stdlib_stats_distribution_normal(module):rvs_normal(interface)]]([loc, scale] [[, array_size]])`

### Class

Elemental function (passing both `loc` and `scale`).

### Arguments

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer`.

`loc` and `scale` arguments must be of the same type.

### Return value

The result is a scalar or rank one array, with a size of `array_size`, and as the same type of `scale` and `loc`.

### Example

```fortran
{!example/stats_distribution_normal/example_normal_rvs.f90!}
```

## `pdf_normal` - normal distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable normal distribution:

$$f(x) = \frac{1}{\sigma \sqrt{2}} e^{-\frac{1}{2}(\frac{x-\mu}{\sigma})^{2}}$$

For complex varible (x + y i) with independent real x and imaginary y parts, the joint probability density function is the product of corresponding marginal pdf of real and imaginary pdf (ref. "Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

$$f(x + y \mathit{i}) = f(x) f(y) = \frac{1}{2\sigma_{x}\sigma_{y}} e^{-\frac{1}{2}[(\frac{x-\mu}{\sigma_{x}})^{2}+(\frac{y-\nu}{\sigma_{y}})^{2}]}$$

### Syntax

`result = [[stdlib_stats_distribution_normal(module):pdf_normal(interface)]](x, loc, scale)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `real` or `complex`.

All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, and as the same type of input arguments.

### Example

```fortran
{!example/stats_distribution_normal/example_normal_pdf.f90!}
```

## `cdf_normal` - normal distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the single real variable normal distribution:

$$F(x)=\frac{1}{2}\left [ 1+erf(\frac{x-\mu}{\sigma \sqrt{2}}) \right ]$$

For the complex variable (x + y i) with independent real x and imaginary y parts, the joint cumulative distribution function is the product of corresponding marginal cdf of real and imaginary cdf (ref. "Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

$$F(x+y\mathit{i})=F(x)F(y)=\frac{1}{4} [1+erf(\frac{x-\mu}{\sigma_{x} \sqrt{2}})] [1+erf(\frac{y-\nu}{\sigma_{y} \sqrt{2}})]$$

### Syntax

`result = [[stdlib_stats_distribution_normal(module):cdf_normal(interface)]](x, loc, scale)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `real` or `complex`.

All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, as the same type of input arguments.

### Example

```fortran
{!example/stats_distribution_normal/example_norm_cdf.f90!}
```
