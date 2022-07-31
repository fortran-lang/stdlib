---
title: stats_distribution_exponential
---

# Statistical Distributions -- Exponential Distribution Module

[TOC]

## `rvs_exp` - exponential distribution random variates

### Status

Experimental

### Description

An exponential distribution is the distribution of time between events in a Poisson point process. The inverse scale parameter `lambda` specifies the average time between events, also called the rate of events.

Without argument the function returns a random sample from the standard exponential distribution `E(1)` with `lambda = 1`.

With a single argument, the function returns a random sample from the exponential distribution `E(lambda)`.
For complex arguments, the real and imaginary parts are sampled independently of each other.

With two arguments the function returns a rank one array of exponentially distributed random variates.

Note: the algorithm used for generating exponetial random variates is fundamentally limited to double precision. Ref.: Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating random variables', J. Statist. Software, v5(8).

### Syntax

`result = [[stdlib_stats_distribution_exponential(module):rvs_exp(interface)]]([lambda] [[, array_size]])`

### Class

Function

### Arguments

`lambda`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. The value of `lambda` has to be non-negative.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

### Return value

The result is a scalar or rank one array with a size of `array_size`, and has the same type of `lambda`.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_rvs.f90!}
```

## `pdf_exp` - exponential distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable exponential distribution:

$$f(x)=\begin{cases} \lambda e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{}$$

For a complex variable (x + y i) with independent real x and imaginary y parts, the joint probability density function
is the product of the corresponding marginal pdf of real and imaginary pdf (for more details, see
"Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

$$f(x+\mathit{i}y)=f(x)f(y)=\begin{cases} \lambda_{x} \lambda_{y} e^{-(\lambda_{x} x + \lambda_{y} y)} &x\geqslant 0, y\geqslant 0 \\\\ 0 &otherwise\end{}$$

### Syntax

`result = [[stdlib_stats_distribution_exponential(module):pdf_exp(interface)]](x, lambda)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`lambda`: has `intent(in)` and is a scalar of type `real` or `complex`.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, and has the same type of input arguments.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_pdf.f90!}
```

## `cdf_exp` - exponential cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable exponential distribution:

$$F(x)=\begin{cases}1 - e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{}$$

For a complex variable (x + y i) with independent real x and imaginary y parts, the joint cumulative distribution
function is the product of corresponding marginal cdf of real and imaginary cdf (for more details, see
"Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

$$F(x+\mathit{i}y)=F(x)F(y)=\begin{cases} (1 - e^{-\lambda_{x} x})(1 - e^{-\lambda_{y} y}) &x\geqslant 0, \;\; y\geqslant 0 \\\\ 0 &otherwise \end{}$$

### Syntax

`result = [[stdlib_stats_distribution_exponential(module):cdf_exp(interface)]](x, lambda)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`lambda`: has `intent(in)` and is a scalar of type `real` or `complex`.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, and has the same type of input arguments.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_cdf.f90!}
```
