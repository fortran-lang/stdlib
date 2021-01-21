---
title: stats_distribution
---

# Statistical Distributions Normal Module

[TOC]

## `normal_distribution_rvs` - normal distribution random variates

### Status

Experimental

### Description

A normal continuous random variate distribution, also known as Gaussian, or Gauss or Laplace-Gauss distribution. The location `loc` specifies the mean or expectation. The `scale` specifies the standard deviation. 

Without augument the function returns a standard normal distributed random variate N(0,1). The function is elemental.

With two arguments, the function returns a normal distributed random variate N(loc, scale^2). For complex auguments, the real and imaginary parts are independent of each other. The function is elemental.

With three auguments, the function returns a rank one array of normal distributed random variates.

### Syntax

`result = [[stdlib_stats_distribution_normal(module):normal_distribution_rvs(interface)]]([loc, scale] [[, array_size]])`

### Arguments

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer`.

`loc`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: optional argument has `intent(in)` and is a scalar of type `real` or `complex`.

`loc` and `scale` arguments must have the same type.

### Return value

The result is a scalar or rank one array, with a size of `array_size`, of type `real` or `complex`.

### Example

```fortran
program demo_normal_rvs
    use stdlib_stats_distribution_PRNG, only: random_seed
    use stdlib_stats_distribution_normal, only: norm => normal_distribution_rvs

    implicit none
    real ::  a(2,3,4), b(2,3,4)
    complx :: loc, scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, norm( )           !single standard normal random variate

! 0.563655198

    print *, norm(1.0, 2.0)
    !normal random variate miu=1.0, sigma=2.0

! -0.633261681

    print *, norm(0.,1.0,10)       !an array of 10 standard norml random variates

! -3.38123664E-02  -0.190365672  0.220678389  -0.424612164  -0.249541596
!  0.865260184  1.11086845  -0.328349441  1.10873628  1.27049923

    a(:,:,:) = 1.0
    b(:,:,:) = 1.0
    print *, norm(a,b)         ! a rank 3 random variates array

!0.152776539  -7.51764774E-02  1.47208166  0.180561781  1.32407105
! 1.20383692  0.123445868  -0.455737948  -0.469808221  1.60750175
! 1.05748117  0.720934749  0.407810807  1.48165631  2.31749439
! 0.414566994  3.06084275  1.86505437  1.36338580  7.26878643E-02
! 0.178585172  1.39557445  0.828021586  0.872084975

    loc = (-1.0, 2.0)
    scale = (2.0, 1.0)
    print *, norm(loc, scale)
    !single complex normal random variate with real part of miu=-1, sigma=2;
	!imagainary part of miu=2.0 and sigma=1.0

! (1.22566295,2.12518454)

end program demo_normal_rvs
```

## `normal_distribution_pdf` - normal probability density function

### Status

Experimental

### Description

The probability density function of the continuous normal distribution.

$$f(x)=\frac{1}{\sigma&space;\sqrt{2&space;\pi}}&space;e^{-\frac{1}{2}(\frac{x-\mu}{\sigma})^{2}})$$

### Syntax

`result = [[stdlib_stats_distribution_normal(module):normal_distribution_pdf(interface)]](x, loc, scale)`

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `real` or `complex`.

The function is elemental, i.e., all three auguments could be arrays conformable to each other. All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to auguments, of type `real`.

### Example

```fortran
program demo_normal_pdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_normal, only :                                &
                                             norm_pdf=>normal_distribution_pdf, &
                                             norm => normal_distribution_rvs

    implicit none
    real :: x(3,4,5),a(3,4,5),b(3,4,5)
    complx :: loc, scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, norm_pdf(1.0,0.,1.) !a probability density at 1.0 in standard normal

! 0.241970733

    print *, norm_pdf(2.0,-1.0, 2.0)
    !a probability density at 2.0 with miu=-1.0 sigma=2.0

!6.47588000E-02

    x = reshape(norm(0.0, 1.0, 60),[3,4,5])
    ! standard normal random variates array

    a(:,:,:) = 0.0
    b(:,:,:) = 1.0
    print *, norm_pdf(x, a, b)  ! standard normal probability density array

!  0.340346158  0.285823315  0.398714304  0.391778737  0.389345556
!  0.364551932  0.386712372  0.274370432  0.215250477  0.378006011
!  0.215760440  0.177990928  0.278640658  0.223813817  0.356875211
!  0.285167664  0.378533930  0.390739858  0.271684974  0.138273031
!  0.135456234  0.331718773  0.398283750  0.383706540

    loc = (1.0, -0.5)
    scale = (1.0, 2.)
    print *, norm_pdf((1.5,1.0), loc, scale)
    ! a complex normal probability density function at (1.5,1.0) with real part
	! of miu=1.0, sigma=1.0 and imaginary part of miu=-0.5, sigma=2.0

! 5.30100204E-02

end program demo_normal_pdf
```

## `normal_distribution_cdf` - normal cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the normal continuous distribution

$$F(X)=\frac{1}{2}\left&space;[&space;1&space;&plus;&space;erf(\frac{x-\mu}{\sqrt{2}&space;\sigma})&space;\right&space;])$$

### Syntax

`result = [[stdlib_stats_distribution_normal(module):normal_distribution_cdf(interface)]](x, loc, scale)`

### Arguments

`x`: has `intent(in)` and is a scalar of type `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `real` or `complex`.

The function is elemental, i.e., all three auguments could be arrays conformable to each other. All three arguments must have the same type.

### Return value

The result is a scalar or an array, with a shape conformable to auguments, of type `real`.

### Example

```fortran
program demo_norm_cdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_normal, only :                                &
                                          norm_cdf => normal_distribution_cdf,  &
                                          norm => normal_distribution_rvs

    implicit none
    real :: x(2,3,4),a(2,3,4),b(2,3,4)
    complx :: loc, scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, norm_cdf(1.0, 0.0, 1.0)  ! a standard normal cumulative at 1.0

! 0.841344714

    print *, norm_cdf(2.0, -1.0, 2.0)
    ! a cumulative at 2.0 with miu=-1 sigma=2

! 0.933192849

    x = reshape(norm(0.0, 1.0, 24),[2,3,4])
    ! standard normal random variates array

    a(:,:,:) = 0.0
    b(:,:,:) = 1.0
    print *, norm_cdf(x, a, b)        ! standard normal cumulative array

!  0.713505626  0.207069695  0.486513376  0.424511284  0.587328553
!  0.335559726  0.401470929  0.806552052  0.866687536  0.371323735
!  0.866228044  0.898046613  0.198435277  0.141147852  0.681565762
!  0.206268221  0.627057910  0.580759525  0.190364420  7.27325380E-02
!  7.08068311E-02  0.728241026  0.522919059  0.390097380

    loc = (1.0,0.0)
    scale = (0.5,1.0)
    print *, norm_cdf((0.5,-0.5),loc,scale)
    !complex normal cumulative distribution at (0.5,-0.5) with real part of
	!miu=1.0, sigma=0.5 and imaginary part of miu=0.0, sigma=1.0

!4.89511043E-02

end program demo_norm_cdf

```
