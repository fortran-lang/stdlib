---

title: stats_distribution_gamma
---

# Statistical Distributions -- Gamma Distribution Module

[TOC]

## `rvs_gamma` - gamma distribution random variates

### Status

Experimental

### Description

The gamma distribution is a continuous probability distribution widely used in modeling waiting times, queue lengths, and other non-negative quantities. It is defined by a shape parameter (\\(k\\) or \\(\\alpha\\)) that controls the distribution's form and a rate parameter (\\(\\lambda\\)) that controls its scale.

Without arguments, the function returns a random sample from the standard gamma distribution \\(\\Gamma(k=1, \\lambda=1)\\).

With a single argument for shape parameter, the function returns a random sample from the gamma distribution \\(\\Gamma(k=\\text{shape}, \\lambda=1)\\).

With two arguments (shape, rate), the function returns a random sample from the gamma distribution \\(\\Gamma(k=\\text{shape}, \\lambda=\\text{rate})\\).

The optional `loc` parameter specifies the location (shift) of the distribution.

With three or more arguments including `array_size`, the function returns a rank-1 array of gamma distributed random variates.

For complex shape and rate parameters, the real and imaginary parts are sampled independently of each other.

@note
The algorithm used for generating gamma random variates is fundamentally limited to double precision.[^1]


### Syntax

`result = [[stdlib_stats_distribution_gamma(module):rvs_gamma(interface)]]([[shape] [[, rate]] [[, loc]] [[, array_size]]])`

### Class

Impure elemental function

### Arguments

`shape`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive. This is the shape (or "form") parameter \\(k\\) of the distribution. Default value is 1.0.

`rate`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive. This is the rate parameter \(\\lambda\) (inverse of scale). Default value is 1.0.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. 
Specifies the location (shift) of the distribution with default value 0.0. The distribution support is x > loc.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

### Return value

The result is a scalar or rank-1 array with a size of `array_size`, and the same type as `shape`. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_gamma/example_gamma_rvs.f90!}
```

## `pdf_gamma` - gamma distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable gamma distribution is:

$$ f(x)= \frac{\lambda^{k}}{\Gamma (k)}x^{k-1}e^{-\lambda x} ,\quad x>0,\ k>0,\ \lambda>0 $$

where \\(k\\) is the shape parameter, \\(\\lambda\\) is the rate parameter, and \\(\\Gamma(k)\\) is the gamma function.

An optional `loc` parameter specifies the location (shift) of the distribution.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint probability density function is the product of the corresponding real and imaginary marginal pdfs:[^2]

$$f(x+\mathit{i}y)=f(x)f(y)$$

### Syntax

`result = [[stdlib_stats_distribution_gamma(module):pdf_gamma(interface)]](x, shape, rate [[, loc]])`

### Class

Impure elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the pdf.

`shape`: has `intent(in)` and is a scalar of type `real` or `complex`. The shape parameter \\(k\\). 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive.

`rate`: has `intent(in)` and is a scalar of type `real` or `complex`. The rate parameter \\(\\lambda\\). 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter with default value 0.0.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_gamma/example_gamma_pdf.f90!}
```

## `cdf_gamma` - gamma distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable gamma distribution:

$$ F(x)= \frac{\gamma (k, \lambda x)}{\Gamma (k)},\quad x>0,\ k>0,\ \lambda>0 $$

where \\(\\gamma(k, z)\\) is the lower incomplete gamma function, and \\(\\Gamma(k)\\) is the gamma function. This is often referred to as the regularized gamma P function.

An optional `loc` parameter specifies the location (shift) of the distribution.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint cumulative distribution function is the product of the corresponding real and imaginary marginal cdfs:[^2]

$$F(x+\mathit{i}y)=F(x)F(y)$$

### Syntax

`result = [[stdlib_stats_distribution_gamma(module):cdf_gamma(interface)]](x, shape, rate [[, loc]])`

### Class

Impure elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the cdf.

`shape`: has `intent(in)` and is a scalar of type `real` or `complex`. The shape parameter \(k\). 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive.

`rate`: has `intent(in)` and is a scalar of type `real` or `complex`. The rate parameter \(\lambda\). 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter with default value 0.0.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_gamma/example_gamma_cdf.f90!}
```
[^1]: Marsaglia, George, and Wai Wan Tsang. "The ziggurat method for generating random variables." _Journal of statistical software_ 5 (2000): 1-7.

[^2]: Miller, Scott, and Donald Childers. _Probability and random processes: With applications to signal processing and communications_. Academic Press, 2012 (p. 197).
````
