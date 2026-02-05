---

title: stats_distribution_gamma
---

# Statistical Distributions -- Gamma Distribution Module

[TOC]

## `rvs_gamma` - gamma distribution random variates

### Status

Experimental

### Description

The gamma distribution is a continuous probability distribution widely used in modeling waiting times, queue lengths, and other non-negative quantities. It is parameterized by a shape parameter (\\(k\\) or \\(\\alpha\\)) that controls the distribution's form and a rate parameter (\\(\\lambda\\) or \\(\\beta\\)) that controls its scale. The location parameter (\\(\\text{loc}\\)) shifts the distribution.

With a single argument for shape parameter, the function returns a random sample from the standard gamma distribution with rate = 1.0 and loc = 0.0.

With three arguments (loc, shape, rate), the function returns a random sample from the shifted gamma distribution.

With four arguments (loc, shape, rate, array_size), the function returns a rank-1 array of gamma distributed random variates.

For complex shape and rate parameters, the real and imaginary parts are sampled independently of each other.

@note
The algorithm used for generating gamma random variates is fundamentally limited to double precision.[^1]


### Syntax

`result = [[stdlib_stats_distribution_gamma(module):rvs_gamma(interface)]](shape)`

`result = [[stdlib_stats_distribution_gamma(module):rvs_gamma(interface)]](loc, shape, rate [, array_size])`

### Class

Impure elemental function

### Arguments

`shape`: has `intent(in)` and is a scalar of type `real` or `complex`. 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive. This is the shape (or "form") parameter \\(k\\) of the distribution.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`. 
Specifies the location (shift) of the distribution. The distribution support is x > loc.

`rate`: has `intent(in)` and is a scalar of type `real` or `complex`. 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive. This is the rate parameter \\(\\lambda\\) (inverse of scale).

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer` with default kind.

### Return value

The result is a scalar or rank-1 array with a size of `array_size`, and the same type as `shape`. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
program demo_gamma_rvs
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma
    implicit none
    real ::  g(2,3,4)
    complex :: shape, rate
    integer :: put, get
    put = 1234567
    call random_seed(put, get)
    print *, rgamma(2.0)
    !single standard gamma random variate with shape of 2.0, rate=1.0
! 2.50538206
    print *, rgamma(0.0, 3.0, 2.0)  !gamma random variate with loc=0.0, shape=3.0, rate=2.0
! 1.30591583
    g(:,:,:) = 0.5
    print *, rgamma(g)
    !a rank 3 array of 60 standard gamma random variates with shape=0.5
!  1.03841162  1.33044529  0.912742674  0.131288037  0.638593793 
!  1.03565669E-02  0.624804378  1.12179172  4.91380468E-02  6.69969944E-03 
!  6.67014271E-02  0.132111162  0.101102419  0.648416579  1.14922595 
!  2.29003578E-02  1.85964716E-04  1.21213868E-02  1.69112933 
!  7.30440915E-02  0.395139128  0.182758048  0.427981257  0.985665262
    print *, rgamma(0.0, 0.5, 1.0, 10)
    ! an array of 10 random variates with loc=0.0, shape=0.5, rate=1.0
!  1.39297554E-04  0.296419382  0.352113068  2.80515051  3.65264394E-04 
!  0.197743446  5.54569438E-02  9.30598825E-02  1.02596343  1.85311246
    shape = (3.0, 4.0)
    rate = (2.0, 0.7)
    print *, rgamma((0.0, 0.0), shape, rate)
    !single complex gamma random variate with real part of shape = 3.0,
    !rate=2.0; imaginary part of shape=4.0, rate=0.7
! (0.826188326,3.54749799)
end program demo_gamma_rvs
```

## `pdf_gamma` - gamma distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable gamma distribution is:

$$ f(x)= \frac{\lambda^{k}}{\Gamma (k)}(x-\text{loc})^{k-1}e^{-\lambda (x-\text{loc})} ,\quad x>\text{loc},\ k>0,\ \lambda>0 $$

where \\(k\\) is the shape parameter, \\(\\lambda\\) is the rate parameter, and \\(\\Gamma(k)\\) is the gamma function.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint probability density function is the product of the corresponding real and imaginary marginal pdfs:[^2]

$$f(x+\mathit{i}y)=f(x)f(y)$$

### Syntax

`result = [[stdlib_stats_distribution_gamma(module):pdf_gamma(interface)]](x, loc, shape, rate)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the pdf.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter.

`shape`: has `intent(in)` and is a scalar of type `real` or `complex`. The shape parameter \\(k\\). 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive.

`rate`: has `intent(in)` and is a scalar of type `real` or `complex`. The rate parameter \\(\\lambda\\). 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
program demo_gamma_pdf
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma,&
                                             gamma_pdf => pdf_gamma
    implicit none
    real :: x(2,3,4),g(2,3,4),s(2,3,4)
    complex :: shape, rate
    integer :: put, get
    put = 1234567
    call random_seed(put, get)
    print *, gamma_pdf(1.0, 0.0, 1.0, 1.0)
    !a probability density at 1.0 with loc=0.0, shape=1.0, rate=1.0
! 0.367879450
    g(:,:,:) = 2.0
    s(:,:,:) = 1.0
    x = reshape(rgamma(0.0, 2.0, 1.0, 24),[2,3,4]) ! gamma random variates array
    print *, gamma_pdf(x,0.0,g,s)     ! a rank 3 gamma probability density array
!  0.204550430  0.320178866  0.274986655  0.348611295  0.101865448 
!  0.102199331  0.358981341  0.223676488  0.254329354  0.356714427 
!  0.267390072  0.305148095  0.367848188  7.26194456E-02  1.49471285E-02 
!  0.246272027  0.360770017  0.339665830  0.101558588  0.358678699 
!  0.224196941  0.359253854  7.56355673E-02  0.251869917
    shape = (1.0, 1.5)
    rate  = (1.0, 2.)
    print *, gamma_pdf((1.5,1.0), (0.0,0.0), shape, rate)
    ! a complex expon probability density function at (1.5,1.0) with real part
    !of shape=1.0, rate=1.0 and imaginary part of shape=1.5, rate=2.0
! 9.63761061E-02
end program demo_gamma_pdf
```

## `cdf_gamma` - gamma distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable gamma distribution:

$$ F(x)= \frac{\gamma (k, \lambda (x-\text{loc}))}{\Gamma (k)},\quad x>\text{loc},\ k>0,\ \lambda>0 $$

where \\(\\gamma(k, z)\\) is the lower incomplete gamma function and \\(\\Gamma(k)\\) is the gamma function. This is often referred to as the regularized gamma P function.

For a complex variable \\(z=(x + y i)\\) with independent real \\(x\\) and imaginary \\(y\\) parts, the joint cumulative distribution function is the product of the corresponding real and imaginary marginal cdfs:[^2]

$$F(x+\mathit{i}y)=F(x)F(y)$$

### Syntax

`result = [[stdlib_stats_distribution_gamma(module):cdf_gamma(interface)]](x, loc, shape, rate)`

### Class

Elemental function

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`. The point at which to evaluate the cdf.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`. The location (shift) parameter.

`shape`: has `intent(in)` and is a scalar of type `real` or `complex`. The shape parameter \\(k\\). 
If `shape` is `real`, its value must be positive. If `shape` is `complex`, both the real and imaginary components must be positive.

`rate`: has `intent(in)` and is a scalar of type `real` or `complex`. The rate parameter \\(\\lambda\\). 
If `rate` is `real`, its value must be positive. If `rate` is `complex`, both the real and imaginary components must be positive.

All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to the arguments, and the same type as the input arguments. If `shape` or `rate` is non-positive, the result is `NaN`.

### Example

```fortran
program demo_gamma_cdf
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma,&
                                             gamma_cdf => cdf_gamma
    implicit none
    real :: x(2,3,4),g(2,3,4),s(2,3,4)
    complex :: shape, rate
    integer :: seed_put, seed_get
    seed_put = 1234567
    call random_seed(seed_put, seed_get)
    print *, gamma_cdf(1.0, 0.0, 0.5, 1.0)
    ! a standard gamma cumulative at 1.0 with loc=0.0, shape=0.5, rate=1.0
! 0.842700839
    print *, gamma_cdf(2.0, 0.0, 1.5, 2.0)
    ! a cumulative at 2.0 with loc=0.0, shape=1.5, rate=2.0
! 0.953988254
    g(:,:,:) = 1.0
    s(:,:,:) = 1.0
    x = reshape(rgamma(0.0, 1.0, 1.0, 24),[2,3,4])
    !gamma random variates array with a shape=1.0, rate=1.0
    print *, gamma_cdf(x,0.0,g,s)        ! a rank 3 standard gamma cumulative array
!  0.710880339  0.472411335  0.578345954  0.383050948  0.870905757 
!  0.870430350  0.170215249  0.677347481  0.620089889  0.161825046 
!  4.17549349E-02  0.510665894  0.252201647  0.911497891  0.984424412 
!  0.635621786  0.177783430  0.414842933  0.871342421  0.338317066 
!  2.06879266E-02  0.335232288  0.907408893  0.624871135
    shape = (.7, 2.1)
    rate  = (0.5,1.0)
    print *, gamma_cdf((0.5,0.5),(0.0,0.0),shape,rate)
    !complex gamma cumulative distribution at (0.5,0.5) with real part of
    !shape=0.7,rate=0.5 and imaginary part of shape=2.1,rate=1.0
! 2.87349485E-02
end program demo_gamma_cdf
```
[^1]: Marsaglia, George, and Wai Wan Tsang. "The ziggurat method for generating random variables." _Journal of statistical software_ 5 (2000): 1-7.

[^2]: Miller, Scott, and Donald Childers. _Probability and random processes: With applications to signal processing and communications_. Academic Press, 2012 (p. 197).
````
