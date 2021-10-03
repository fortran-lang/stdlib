---
title: stats_distribution_uniform
---

# Statistical Distributions -- Uniform Distribution Module

[TOC]

## `shuffle` - Using Fisher-Yates algorithm to generate a random permutation of a list

### Status

Experimental

### Description

Applying Fisher-Yates algorithm to generate an unbiased permutation for any list of intrinsic numerical data types.

### Syntax

`result = [[stdlib_stats_distribution_uniform(module):shuffle(interface)]]( list )`

### Class

Function.

### Arguments

`list`: argument has `intent(in)` and is a rank one array of `integer`, `real`, or `complex` type.

### Return value

Return a randomized rank one array of the input type.

### Example

```fortran
program demo_shuffle
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_uniform, only : shuffle
    implicit none
    integer :: seed_put, seed_get, i
    real :: x(10)
    integer :: n(10)
    complex :: z(10)

    do i=1, 10
        n(i) = i
        x(i) = real(i)
        z(i) = cmplx(real(i), real(i))
    end do
    seed_put = 32165498
    call random_seed(seed_put, seed_get)    ! set and get current value of seed
    print *, shuffle(n)                          ! get randomized n

!10   6   9   2   8   1   3   5   7   4

    print *, shuffle(x)                          ! get randomized x

!5.0   10.0   9.0   4.0   3.0   8.0   2.0   1.0   7.0   6.0

    print *, shuffle(z)                          ! get randomized z

!(8.0, 8.0)    (7.0, 7.0)    (4.0, 4.0)    (1.0, 1.0)    (5.0, 5.0)
!(9.0, 9.0)    (6.0, 6.0)    (3.0, 3.0)    (2.0, 2.0)    (10.0, 10.0)

end program demo_shuffle
```

## `rvs_uniform` - uniform distribution random variates

### Status

Experimental

### Description

Without argument the function returns a scalar standard uniformly distributed variate U(0,1) of `real` type with single precision on [0,1].

With single argument `scale` of `integer` type the function returns a scalar uniformly distributed variate of `integer` type on [0,scale]. This is the standard Rectangular distribution.

With single argument `scale` of `real` or `complex` type the function returns a scalar uniformly distributed variate of `real` or `complex` type on [0, scale].

With double arguments `loc` and `scale` the function returns a scalar uniformly distributed random variates of `integer`, `real` or `complex` type on [loc, loc + scale] dependent of input type.

With triple arguments `loc`, `scale` and `array_size` the function returns a rank one array of uniformly distributed variates of `integer`, `real` or `complex` type with an array size of `array_size`.

For `complex` type, the real part and imaginary part are independent of each other.

### Syntax

`result = [[stdlib_stats_distribution_uniform(module):rvs_uniform(interface)]]([[loc,] scale] [[[,array_size]]])`

### Class

Elemental function (without the third argument).

### Arguments

`loc`: optional argument has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: optional argument has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`array_size`: optional argument has `intent(in)` and is a scalar of type `integer`.

`loc` and `scale` must have the same type and kind when both are present.

### Return value

The result is a scalar or a rank one array, with size of `array_size`, of type `integer`, `real` or `complex` depending on the input type.

### Example

```fortran
program demo_uniform_rvs
    use stdlib_stats_distribution_PRNG, only:random_seed
    use stdlib_stats_distribution_uniform, only:uni=> rvs_uniform

    implicit none
    complex :: loc, scale
    real :: a(3,4,5), b(3,4,5)
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, uni( )           !real standard uniform random variate in [0., 1.]
! 0.161520019

    print *, uni(3.0)         !an uniform random variate in [0., 3.]
! 1.65974522

    print *, uni(-0.5, 1.0)   !an uniform random variate in [-0.5, 0.5]
! 0.486900032

    print *, uni(-1.0,2.0,10) 
    !an array of 10 uniform random variates in [-1., 1.]

!0.884182811  -0.771520197  0.560377002  0.709313750  -7.12267756E-02
!-0.431066573  0.497536063  -0.396331906  -0.325983286  0.137686729

    print *, uni(20)          !a random integer variate in [0, 20]
! 17

    print *, uni(5,13)        !a random integer variate in [5, 18]
! 15

    print *, uni(3,19,10)     !an array of 10 integer variates in [3,22]

!7  16  16  12  9  21  19  4  3  19

    loc = (-0.5, -0.5)
    scale = (1.0, 1.0)

    print *, uni(scale)       !a complex uniform random variate in unit square

!(0.139202669, 0.361759573)

    print *, uni(loc,scale)
    !a complex uniform random variate in [(-0.5, -0.5), (0.5, 0.5)]

!(0.296536088,-0.143987954)

    print *, uni(loc, scale, 10)
    !an array of 10 complex uniform random variate in [(-0.5, -0.5), (0.5, 0.5)]

!(-0.302334785,-0.401923567)    (0.281620383,9.534919262E-02)
! (-0.374348879,0.457528770)     (0.442990601,-0.240510434)
! (-0.421572685,0.279313922)     (-0.182090610,5.901372433E-02)
! (-7.864198089E-02,0.378484428)    (-0.423258364,-0.201292425)
! (0.193327367,-0.353985727)    (-0.397661150,0.355926156)

    a(:,:,:) = -0.5
    b(:,:,:) = 1.0

    print *, uni(a,b)
    !a rank 3 array of random variates in [-0.5,0.5]

! -0.249188632   -0.199248433   -0.389813602   2.88307667E-03   0.238479793,
!  0.264856219   -0.205177426   -0.480921626   0.131218433   0.252170086,
! -0.303151041   -8.89462233E-02   -0.377370685   0.341802299   0.323204756,
! 0.358679056   -0.138909757   0.384329498   -0.109372199   0.132353067,
! 0.494320452   0.419343710   -0.103044361   0.461389005   0.403132677
! 0.121850729   0.403839290   -0.349389791   0.490482628   0.156600773
! 8.46788883E-02   -0.483680278   0.388107836   0.119698405   0.154214382
! 0.153113484   0.236523747   0.155937552   -0.135760903   0.219589531
! 0.394639254   6.30156994E-02   -0.342692465   -0.444846451   -0.215700030
! 0.204189956   -0.208748132   0.355063021   8.98272395E-02   -0.237928331
! 2.98077464E-02   -0.485149682   -8.06870461E-02   -0.372713923
! -0.178335011   0.283877611   -2.13934183E-02   -9.21690464E-03
! 4.56320047E-02   0.220112979

end program demo_uniform_rvs
```

## `pdf_uniform` - Uniform probability density function

### Status

Experimental

### Description

The probability density function of the uniform distribution.

f(x) = 1 / (scale + 1);            for discrete uniform distribution.

f(x) = 1 / scale;                  for continuous uniform distribution.

f(x) = 1 / (scale%re * scale%im);  for complex uniform distribution.

### Syntax

`result = [[stdlib_stats_distribution_uniform(module):pdf_uniform(interface)]](x, loc, scale)`

### Class

Elemental function.

### Arguments

`x`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

All three arguments must have the same type and kind.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, of type `real`.

### Example

```fortran
program demo_uniform_pdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_uniform,  only : uni_pdf => pdf_uniform,     &
                                                   uni => rvs_uniform

    implicit none
    complex :: loc, scale
    real :: a(3,4,5), b(3,4,5), x(3,4,5)
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, uni_pdf(3, 2, 10)       !probability density at 3 in range [2, 10]

! 9.09090936E-02

    print *, uni_pdf(0.5,0.0,1.0)    !a probability density at 0.5 in [0., 1.]

! 1.00000000


    print *, uni_pdf(0.7,-1.0,2.0)   !a probability density at 0.7 in [-1., 1.]

! 0.500000000

    a(:,:,:) = 0.0
    b(:,:,:) = 2.0
    x = reshape(uni(0.,2.,60),[3,4,5])! uniform random variates array in [0., 2.]
    print *, uni_pdf(x, a, b)         ! probability density array in [0., 2.]

! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000

    loc = (-0.5,-0.5)
    scale = (1.0,1.0)
    print *, uni_pdf((-0.1,0.2), loc, scale)
    ! joint probability density at (-0.1,0.2) in [(-0.5, -0.5), (0.5, 0.5)]

! 1.00000000
end program demo_uniform_pdf

```

## `cdf_uniform` - Uniform cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the uniform distribution

F(x) = (x - loc + 1) / (scale + 1);      for discrete uniform distribution.

F(x) = (x - loc) / scale;                for continuous uniform distribution.

F(x) = (x%re - loc%re)(x%im - loc%im) / (scale%re * scale%im); for complex uniform distribution.

### Syntax

`result = [[stdlib_stats_distribution_uniform(module):cdf_uniform(interface)]](x, loc, scale)`

### Class

Elemental function.

### Arguments

`x`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`loc`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

`scale`: has `intent(in)` and is a scalar of type `integer`, `real` or `complex`.

All three arguments must have the same type and kind.

### Return value

The result is a scalar or an array, with a shape conformable to arguments, of type `real`.

### Example

```fortran
program demo_uniform_cdf
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_uniform, only : uni_cdf => cdf_uniform,      &
                                                  uni => rvs_uniform

    implicit none
    real :: x(3,4,5), a(3,4,5), b(3,4,5)
    complex :: loc, scale
    integer :: seed_put, seed_get

    seed_put = 1234567
    call random_seed(seed_put, seed_get)

    print *, uni_cdf(0.5,0.,1.)      ! a cumulative at 0.5 in [0., 1.]

!0.500000000

    print *, uni_cdf(0.7,-1.0,2.0)   ! a cumulative at 0.7 in [-1.0, 1.0]

! 0.850000024

    print *, uni_cdf(6, 2, 10)       ! a cumulative at 6 in [2, 10]

! 0.454545468

    a(:,:,:) = -1.0
    b(:,:,:) = 2.0
    x = reshape(uni(-1.0,2.0,60),[3,4,5]) ! uniform random variates array
    print *, uni_cdf(x,a,b)        ! cumulative array in [-1.0, 1.0]

!0.161520004  0.553248405  0.986900032  0.942091405  0.114239901  0.780188501  
! 0.854656875  0.464386612  0.284466714  0.748768032  0.301834047  0.337008357  
!0.568843365  0.596165061  0.180993259  0.614166319  0.214835495 7.98164606E-02 
!0.641274095  0.607101977  0.701139212  0.230517209  1.97925568E-02 0.857982159 
!0.712761045  0.139202654  0.361759573  0.796536088  0.356012046  0.197665215   
!9.80764329E-02 0.781620383  0.595349193  0.125651121  0.957528770  0.942990601 
!0.259489566  7.84273148E-02  0.779313922  0.317909390  0.559013724 0.421358019 
!0.878484428  7.67416358E-02  0.298707575  0.693327367  0.146014273 0.102338850 
!0.855926156  0.250811368  0.300751567  0.110186398  0.502883077  0.738479793   
!0.764856219  0.294822574  1.90783739E-02 0.631218433 0.752170086  0.196848959

    loc = (0., 0.)
    scale=(2., 1.)
    print *, uni_cdf((1.2,0.5), loc, scale)
    ! joint cumulative distribution at (1.2,0.5) in [(0.,0.), (2.,1.)]

! 0.300000012
end program demo_uniform_cdf

```
