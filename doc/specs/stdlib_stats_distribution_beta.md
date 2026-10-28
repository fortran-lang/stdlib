---
title: stats_distribution_beta
---

# Statistical Distributions -- Beta Distribution Module

[TOC]

## `rvs_beta` - beta distribution random variates

### Status

Experimental

### Description

The beta distribution is a continuous probability distribution defined on the interval [0, 1], widely used for modeling random variables that represent proportions, probabilities, and other bounded quantities. It is defined by two shape parameters (\\(a\\) and \\(b\\)) that control the distribution's form.

With two arguments (a, b), the function returns a random sample from the beta distribution \\(\\text{Beta}(a, b)\\).

The optional `loc` parameter specifies the location (shift) of the distribution.

With three or more arguments including `array_size`, the function returns a rank-1 array of beta distributed random variates.

For complex shape parameters, the real and imaginary parts are sampled independently of each other.

@note
For shape parameters less than 1, the function uses a uniform method. For parameters greater than or equal to 1, it uses the gamma ratio method[^1], where \\(X \\sim \\text{Beta}(a,b)\\) is generated as \\(X = \\frac{Y_1}{Y_1 + Y_2}\\) where \\(Y_1 \\sim \\Gamma(a,1)\\) and \\(Y_2 \\sim \\Gamma(b,1)\\).

### Syntax

`result = [[stdlib_stats_distribution_beta(module):rvs_beta(interface)]](a, b [[, loc]] [[, array_size]])`

### Class

Impure elemental function

### Arguments

`a`: has `intent(in)` and is a scalar of type `real` or `complex`. 
If `a` is `real`, its value must be positive. If `a` is `complex`, both the real and imaginary components must be positive. This is the first shape parameter of the distribution.

`b`: has `intent(in)` and is a scalar of type `real` or `complex`. 
If `b` is `real`, its value must be positive. If `b` is `complex`, both the real and imaginary components must be positive. This is the second shape parameter of the distribution.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. 
Specifies the location (shift) of the distribution with default value 0.0. The distribution support is loc < x < loc + 1.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

### Return value

The result is a scalar or rank-1 array with a size of `array_size`, and the same type as `a`. If `a` or `b` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_beta/example_beta_rvs.f90!}
```

## `pdf_beta` - beta distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable beta distribution is:

$$ f(x)= \\frac{x^{a-1}(1-x)^{b-1}}{B(a,b)} ,\\quad 0<x<1,\\ a>0,\\ b>0 $$

where \\(a\\) and \\(b\\) are the shape parameters, and \\(B(a,b)\\) is the beta function.

An optional `loc` parameter specifies the location (shift) of the distribution.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint probability density function is the product of the corresponding real and imaginary marginal pdfs:[^2]

$$f(x+\\mathit{i}y)=f(x)f(y)$$

### Syntax

`result = [[stdlib_stats_distribution_beta(module):pdf_beta(interface)]](x, a, b [[, loc]])`

### Class

Impure elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the pdf.

`a`: has `intent(in)` and is a scalar of type `real` or `complex`. The first shape parameter. 
If `a` is `real`, its value must be positive. If `a` is `complex`, both the real and imaginary components must be positive.

`b`: has `intent(in)` and is a scalar of type `real` or `complex`. The second shape parameter. 
If `b` is `real`, its value must be positive. If `b` is `complex`, both the real and imaginary components must be positive.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter with default value 0.0.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `a` or `b` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_beta/example_beta_pdf.f90!}
```

## `cdf_beta` - beta distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable beta distribution is:

$$ F(x)= I_x(a, b),\\quad 0<x<1,\\ a>0,\\ b>0 $$

where \\(I_x(a,b)\\) is the regularized incomplete beta function.

An optional `loc` parameter specifies the location (shift) of the distribution.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint cumulative distribution function is the product of the corresponding real and imaginary marginal cdfs:[^2]

$$F(x+\\mathit{i}y)=F(x)F(y)$$

### Syntax

`result = [[stdlib_stats_distribution_beta(module):cdf_beta(interface)]](x, a, b [[, loc]])`

### Class

Impure elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the cdf.

`a`: has `intent(in)` and is a scalar of type `real` or `complex`. The first shape parameter. 
If `a` is `real`, its value must be positive. If `a` is `complex`, both the real and imaginary components must be positive.

`b`: has `intent(in)` and is a scalar of type `real` or `complex`. The second shape parameter. 
If `b` is `real`, its value must be positive. If `b` is `complex`, both the real and imaginary components must be positive.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter with default value 0.0.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `a` or `b` is non-positive, the result is `NaN`.

### Example

```fortran
{!example/stats_distribution_beta/example_beta_cdf.f90!}
```

[^1]: Devroye, Luc. _Non-Uniform Random Variate Generation_. Springer-Verlag, 1986 (Chapter IX, Section 3).

[^2]: Miller, Scott, and Donald Childers. _Probability and random processes: With applications to signal processing and communications_. Academic Press, 2012 (p. 197).
