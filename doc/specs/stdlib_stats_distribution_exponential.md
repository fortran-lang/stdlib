---
title: stats_distribution_exponential
---

# Statistical Distributions -- Exponential Distribution Module

[TOC]

## `rvs_exp` - exponential distribution random variates

### Status

Experimental

### Description

An exponential distribution is the distribution of time between events in a Poisson point process.
The inverse scale parameter `lambda` specifies the average time between events (\(\lambda\)), also called the rate of events.

Without argument, the function returns a random sample from the standard exponential distribution \(E(\lambda=1)\).

With a single argument, the function returns a random sample from the exponential distribution \(E(\lambda=\text{lambda})\).
For complex arguments, the real and imaginary parts are sampled independently of each other.

With two arguments, the function returns a rank-1 array of exponentially distributed random variates.

@note
The algorithm used for generating exponential random variates is fundamentally limited to double precision.[^1]

### Syntax

`result = ` [[stdlib_stats_distribution_exponential(module):rvs_exp(interface)]] `([lambda] [[, array_size]])`

### Class

Elemental function

### Arguments

`lambda`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.
If `lambda` is `real`, its value must be positive. If `lambda` is `complex`, both the real and imaginary components must be positive.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

### Return value

The result is a scalar or rank-1 array with a size of `array_size`, and the same type as `lambda`.
If `lambda` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_rvs.f90!}
```

## `pdf_exp` - exponential distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable exponential distribution is:

$$f(x)=\begin{cases} \lambda e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{cases}$$

For a complex variable \(z=(x + y i)\) with independent real \(x\) and imaginary \(y\) parts, the joint probability density function is the product of the corresponding real and imaginary marginal pdfs:[^2]

$$f(x+\mathit{i}y)=f(x)f(y)=\begin{cases} \lambda_{x} \lambda_{y} e^{-(\lambda_{x} x + \lambda_{y} y)} &x\geqslant 0, y\geqslant 0 \\\\ 0 &\text{otherwise}\end{cases}$$

### Syntax

`result = ` [[stdlib_stats_distribution_exponential(module):pdf_exp(interface)]] `(x, lambda)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`lambda`: has `intent(in)` and is a scalar of type `real` or `complex`.
If `lambda` is `real`, its value must be positive. If `lambda` is `complex`, both the real and imaginary components must be positive.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `lambda` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_pdf.f90!}
```

## `cdf_exp` - exponential cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable exponential distribution:

$$F(x)=\begin{cases}1 - e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{cases}$$

For a complex variable  \(z=(x + y i)\) with independent real \(x\) and imaginary \(y\) parts, the joint cumulative distribution function is the product of corresponding real and imaginary marginal cdfs:[^2]

$$F(x+\mathit{i}y)=F(x)F(y)=\begin{cases} (1 - e^{-\lambda_{x} x})(1 - e^{-\lambda_{y} y}) &x\geqslant 0, \;\; y\geqslant 0 \\\\ 0 & \text{otherwise} \end{cases}$$

### Syntax

`result = ` [[stdlib_stats_distribution_exponential(module):cdf_exp(interface)]] `(x, lambda)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`lambda`: has `intent(in)` and is a scalar of type `real` or `complex`.
If `lambda` is `real`, its value must be positive. If `lambda` is `complex`, both the real and imaginary components must be positive.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `lambda` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_exponential/example_exponential_cdf.f90!}
```

[^1]: Marsaglia, George, and Wai Wan Tsang. "The ziggurat method for generating random variables." _Journal of statistical software_ 5 (2000): 1-7.

[^2]: Miller, Scott, and Donald Childers. _Probability and random processes: With applications to signal processing and communications_. Academic Press, 2012 (p. 197).
