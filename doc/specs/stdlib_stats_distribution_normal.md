---
title: stats_distribution_normal
---

# Statistical Distributions -- Normal Distribution Module

[TOC]

## `rvs_normal` - normal distribution random variates

### Status

Experimental

### Description

A normal continuous random variate distribution, also known as Gaussian, or Gauss or Laplace-Gauss distribution.
The location `loc` specifies the mean or expectation (\(\mu\)). The `scale` specifies the standard deviation (\(\sigma\)).

Without argument, the function returns a standard normal distributed random variate \(N(0,1)\).

With two arguments, the function returns a normal distributed random variate \(N(\mu=\text{loc}, \sigma^2=\text{scale}^2)\). For complex arguments, the real and imaginary parts are independent of each other.

With three arguments, the function returns a rank-1 array of normal distributed random variates.

@note
The algorithm used for generating exponential random variates is fundamentally limited to double precision.[^1]

### Syntax

`result = ` [[stdlib_stats_distribution_normal(module):rvs_normal(interface)]] `([loc, scale] [[, array_size]])`

### Class

Elemental function (passing both `loc` and `scale`).

### Arguments

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: optional argument has `intent(in)` and is a positive scalar of type `real` or `complex`.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer`.

`loc` and `scale` arguments must be of the same type.

### Return value

The result is a scalar or rank-1 array, with a size of `array_size`, and the same type as `scale` and `loc`. If `scale` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_normal/example_normal_rvs.f90!}
```

## `pdf_normal` - normal distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable normal distribution:

$$f(x) = \frac{1}{\sigma \sqrt{2}} \exp{\left[-\frac{1}{2}\left(\frac{x-\mu}{\sigma}\right)^{2}\right]}$$

For a complex varible \( z=(x + y i) \) with independent real \( x \) and imaginary \( y \) parts, the joint probability density function is the product of the the corresponding real and imaginary marginal pdfs:[^2]

$$f(x + y \mathit{i}) = f(x) f(y) = \frac{1}{2\sigma_{x}\sigma_{y}} \exp{\left[-\frac{1}{2}\left(\left(\frac{x-\mu_x}{\sigma_{x}}\right)^{2}+\left(\frac{y-\mu_y}{\sigma_{y}}\right)^{2}\right)\right]}$$

### Syntax

`result = ` [[stdlib_stats_distribution_normal(module):pdf_normal(interface)]] `(x, loc, scale)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a positive scalar of type `real` or `complex`.

All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `scale` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_normal/example_normal_pdf.f90!}
```

## `cdf_normal` - normal distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the single real variable normal distribution:

$$F(x) = \frac{1}{2}\left [ 1+\text{erf}\left(\frac{x-\mu}{\sigma \sqrt{2}}\right) \right ]$$

For the complex variable \( z=(x + y i) \) with independent real \( x \) and imaginary \( y \) parts, the joint cumulative distribution function is the product of the corresponding real and imaginary marginal cdfs:[^2]

$$ F(x+y\mathit{i})=F(x)F(y)=\frac{1}{4} \
\left[ 1+\text{erf}\left(\frac{x-\mu_x}{\sigma_x \sqrt{2}}\right) \right] \
\left[ 1+\text{erf}\left(\frac{y-\mu_y}{\sigma_y \sqrt{2}}\right) \right] $$

### Syntax

`result = ` [[stdlib_stats_distribution_normal(module):cdf_normal(interface)]] `(x, loc, scale)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a positive scalar of type `real` or `complex`.

All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `scale` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_normal/example_normal_cdf.f90!}
```

[^1]: Marsaglia, George, and Wai Wan Tsang. "The ziggurat method for generating random variables." _Journal of statistical software_ 5 (2000): 1-7.

[^2]: Miller, Scott, and Donald Childers. _Probability and random processes: With applications to signal processing and communications_. Academic Press, 2012 (p. 197).
