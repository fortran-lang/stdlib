---
title: stats_distribution
---

# Statistical Distributions -- Exponential Distribution Module

[TOC]

## `exponential_distribution_rvs` - exponential distribution random variates

### Status

Experimental

### Description

An exponentially distributed random variate distribution is the distribution of time between events in a Poisson point process. The inverse scale parameter `lamda` specifies the rate of change.

Without augument the function returns a standard exponential distributed random variate with `lamda = 1.0`. The function is elemental.

With single argument, the function returns an exponential distributed random variate E(lamda). The function is elemental. For complex auguments, the real and imaginary parts are independent of each other.

With two auguments the function returns a rank one array of random variates.

The rate parameter `lamda` must be greater than 0.

### Syntax

`result = [[stdlib_stats_distribution_expon(module):exponential_distribution_rvs(interface)]]([lamda] [[, array_size]])`

### Arguments

`lamda`: optional argument has `intent(in)` and is a scalar of type `real` or `complx`.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer`.

### Return value

The result is a scalar or rank one array, with a size of `array_size`, of type `real` or `complx`.

### Example

```fortran
program demo_exponential_rvs
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_expon, only:                                  &
        rexp => exponential_distribution_rvs

    implicit none
    real ::  a(2,3,4)
    complx :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, rexp( )         !single standard exponential random variate

! 0.358690143

    print *, rexp(2.0)       !exponential random variate with lamda=2.0

! 0.816459715

    print *, rexp(0.3, 10)   !an array of 10 variates with lamda=0.3

! [1.84008647E-02, 3.59742008E-02, 0.136567295, 0.262772143, 3.62352766E-02,
!  0.547133625, 0.213591918, 4.10784185E-02, 0.583882213, 0.671128035]

    a(:,:,:) = 0.5
    print *, rexp(a)         !a rank 3 array of 24 exponential random variates

! [0.219550118, 0.318272740, 0.426896989, 0.803026378, 0.395067871,
!  5.93891777E-02, 0.809226036, 1.27890170, 1.38805652, 0.179149821,
!  1.75288841E-02, 7.23171830E-02, 0.157068044, 0.153069839, 0.421180248,
!  0.517792642, 2.09411430, 0.785641313, 0.116311245, 0.295113146,
!  0.824005902, 0.123385273, 5.50238751E-02, 3.52851897E-02]

    scale = (2.0, 0.7)
    print *, rexp(scale)     !single complex exponential random variate with real part of lamda=2.0; imagainary part of lamda=0.7

! (1.41435969,4.081114382E-02)

end program demo_exponential_rvs
```

## `exponential_distribution_pdf` - exponential probability density function

### Status

Experimental

### Description

The probability density function of the continuous exponential distribution.

$$ f(x)=\begin{cases}lamda \times e^{-lamda \times x} &x\geqslant 0 \\\\ 0 &x< 0\end{} $$

x is supported on [0, \infty)

### Syntax

`result = [[stdlib_stats_distribution_expon(module):exponential_distribution_pdf(interface)]](x, lamda)`

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complx`.

`lamda`: has `intent(in)` and is a scalar of type `real` or `complx`.

The function is elemental, i.e., all auguments could be arrays conformable to each other. All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to auguments, of type `real`.

### Example

```fortran
program demo_exponential_pdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_expon, only :                                 &
        exp_pdf => exponential_distribution_pdf,                                &
        rexp => exponential_distribution_rvs

    implicit none
    real :: x(2,3,4),a(2,3,4)
    complx :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, exp_pdf(1.0,1.0) !a probability density at 1.0 in standard expon

! 0.367879450

    print *, exp_pdf(2.0,2.0) !a probability density at 2.0 with lamda=2.0

! 3.66312787E-02

    x = reshape(rexp(0.5, 24),[2,3,4]) ! standard expon random variates array
    a(:,:,:) = 0.5
    print *, exp_pdf(x, a)     ! a rank 3 standard expon probability density

! [0.457115263, 0.451488823, 0.492391467, 0.485233188, 0.446215510,
!  0.401670188, 0.485127628, 0.316924453, 0.418474048, 0.483173639,
!  0.307366133, 0.285812140, 0.448017836, 0.426440030, 0.403896868,
!  0.334653258, 0.410376132, 0.485370994, 0.333617479, 0.263791025,
!  0.249779820, 0.457159877, 0.495636940, 0.482243657]

    scale = (1.0, 2.)
    print *, exp_pdf((1.5,1.0), scale)
    ! a complex expon probability density function at (1.5,1.0) with real part of lamda=1.0 and imaginary part of lamda=2.0

! 6.03947677E-02

end program demo_exponential_pdf
```

## `exponential_distribution_cdf` - exponential cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the exponential continuous distribution

$$ F(x)=\begin{cases}1 - e^{-lamda \times x} &x\geqslant 0 \\\\ 0 &x< 0\end{} $$

x is supported on [0, \infty)

### Syntax

`result = [[stdlib_stats_distribution_expon(module):exponential_distribution_cdf(interface)]](x, lamda)`

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complx`.

`lamda`: has `intent(in)` and is a scalar of type `real` or `complx`.

The function is elemental, i.e., all auguments could be arrays conformable to each other. All arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to auguments, of type `real`.

### Example

```fortran
program demo_exponential_cdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_expon, only :                                 &
        exp_cdf => exponential_distribution_cdf,                                &
        rexp => exponential_distribution_rvs

    implicit none
    real :: x(2,3,4),a(2,3,4)
    complx :: scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, exp_cdf(1.0, 1.0)  ! a standard exponential cumulative at 1.0

! 0.632120550

    print *, exp_cdf(2.0, 2.0) ! a cumulative at 2.0 with lamda=2

! 0.981684387

    x = reshape(rexp(0.5, 24),[2,3,4])
    ! standard exponential random variates array
    a(:,:,:) = 0.5
    print *, exp_cdf(x, a)  ! a rank 3 array of standard exponential cumulative

! [8.57694745E-02, 9.70223546E-02, 1.52170658E-02, 2.95336246E-02,
!  0.107568979, 0.196659625, 2.97447443E-02, 0.366151094, 0.163051903,
!  3.36527228E-02, 0.385267735, 0.428375721, 0.103964329, 0.147119939,
!  0.192206264, 0.330693483, 0.179247737, 2.92580128E-02, 0.332765043,
!  0.472417951, 0.500440359, 8.56802464E-02, 8.72612000E-03, 3.55126858E-02]

    scale = (0.5,1.0)
    print *, exp_cdf((0.5,0.5),scale)
    !complex exponential cumulative distribution at (0.5,0.5) with real part of lamda=0.5 and imaginary part of lamda=1.0

! 8.70351046E-02

end program demo_exponential_cdf

```
