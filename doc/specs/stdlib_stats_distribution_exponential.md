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

With single argument, the function returns a random sample from the exponential distribution `E(lambda)`. For complex arguments, the real and imaginary parts are independent of each other.

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

The result is a scalar or rank one array with a size of `array_size`, and as the same type of `lambda`.

### Example

```fortran
program demo_exponential_rvs
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_exponential, only: rexp => rvs_exp

    implicit none
    real ::  a(2,3,4)
    complex :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, rexp( )         !single standard exponential random variate

! 0.358690143

    print *, rexp(2.0)       !exponential random variate with lambda=2.0

! 0.816459715

    print *, rexp(0.3, 10)   !an array of 10 variates with lambda=0.3

!  1.84008647E-02  3.59742008E-02  0.136567295  0.262772143  3.62352766E-02 
!  0.547133625  0.213591918  4.10784185E-02  0.583882213  0.671128035

    scale = (2.0, 0.7)
    print *, rexp(scale)
    !single complex exponential random variate with real part of lambda=2.0;
    !imagainary part of lambda=0.7

! (1.41435969,4.081114382E-02)

end program demo_exponential_rvs
```

## `pdf_exp` - exponential distribution probability density function

### Status

Experimental

### Description

The probability density function (pdf) of the single real variable exponential distribution:

$$f(x)=\begin{cases} \lambda e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{}$$

For a complex varible (x + y i) with independent real x and imaginary y parts, the joint probability density function is the product of corresponding marginal pdf of real and imaginary pdf (ref. "Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

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

The result is a scalar or an array, with a shape conformable to arguments, and as the same type of input arguments.

### Example

```fortran
program demo_exponential_pdf
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_exponential, only: exp_pdf => pdf_exp,     &
                                                     rexp => rvs_exp

    implicit none
    real :: x(2,3,4),a(2,3,4)
    complex :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, exp_pdf(1.0,1.0) !a probability density at 1.0 in standard expon

! 0.367879450

    print *, exp_pdf(2.0,2.0) !a probability density at 2.0 with lambda=2.0

! 3.66312787E-02

    x = reshape(rexp(0.5, 24),[2,3,4]) ! standard expon random variates array
    a(:,:,:) = 0.5
    print *, exp_pdf(x, a)     ! a rank 3 standard expon probability density

!  0.457115263  0.451488823  0.492391467  0.485233188  0.446215510 
!  0.401670188  0.485127628  0.316924453  0.418474048  0.483173639 
!  0.307366133  0.285812140  0.448017836  0.426440030  0.403896868 
!  0.334653258  0.410376132  0.485370994  0.333617479  0.263791025 
!  0.249779820  0.457159877  0.495636940  0.482243657

    scale = (1.0, 2.)
    print *, exp_pdf((1.5,1.0), scale)
    ! a complex expon probability density function at (1.5,1.0) with real part
    !of lambda=1.0 and imaginary part of lambda=2.0

! 6.03947677E-02

end program demo_exponential_pdf
```

## `cdf_exp` - exponential distribution cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function (cdf) of the single real variable exponential distribution:

$$F(x)=\begin{cases}1 - e^{-\lambda x} &x\geqslant 0 \\\\ 0 &x< 0\end{}$$

For a complex variable (x + y i) with independent real x and imaginary y parts, the joint cumulative distribution function is the product of corresponding marginal cdf of real and imaginary cdf (ref. "Probability and Random Processes with Applications to Signal Processing and Communications", 2nd ed., Scott L. Miller and Donald Childers, 2012, p.197):

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

The result is a scalar or an array, with a shape conformable to arguments, and as the same type of input arguments.

### Example

```fortran
program demo_exponential_cdf
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_exponential, only : exp_cdf => cdf_exp,    &
                                                      rexp => rvs_exp

    implicit none
    real :: x(2,3,4),a(2,3,4)
    complex :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, exp_cdf(1.0, 1.0)  ! a standard exponential cumulative at 1.0

! 0.632120550

    print *, exp_cdf(2.0, 2.0) ! a cumulative at 2.0 with lambda=2

! 0.981684387

    x = reshape(rexp(0.5, 24),[2,3,4])
    ! standard exponential random variates array
    a(:,:,:) = 0.5
    print *, exp_cdf(x, a)  ! a rank 3 array of standard exponential cumulative

!  8.57694745E-02  9.70223546E-02  1.52170658E-02  2.95336246E-02 
!  0.107568979  0.196659625  2.97447443E-02  0.366151094  0.163051903 
!  3.36527228E-02  0.385267735  0.428375721  0.103964329  0.147119939 
!  0.192206264  0.330693483  0.179247737  2.92580128E-02  0.332765043 
!  0.472417951  0.500440359  8.56802464E-02  8.72612000E-03  3.55126858E-02

    scale = (0.5,1.0)
    print *, exp_cdf((0.5,0.5),scale)
    !complex exponential cumulative distribution at (0.5,0.5) with real part of
    !lambda=0.5 and imaginary part of lambda=1.0

! 8.70351046E-02

end program demo_exponential_cdf

```
