---
title: stats_distribution
---

# Statistical Distributions

[TOC]

## `random_seed` - set or get a value of seed to the random distribution pseudorandom number generator

### Status

Experimental

### Description

Sets the seed value before calling random distribution for variates. 

### Syntax

`call random_seed(put [, get])`

### Arguments

`put`: argument has intent `in` and may be a scalar of type `integer` with kind of int32.

`get`: optional argument has intent `out` and is a scalar of type `integer` with kind of int32.

### Return value


### Example

```fortran
program demo_norm_seed
    use stdlib_stats_distribution, only : random_seed
    use iso_fortran_env, only : int32
    implicit none
    integer(int32) :: seed, seed_value

    seed = 1234567
    call random_seed(seed)                   ! reset the value of seed
    call random_seed(seed, seed_value)       ! get current value of seed
end program demo_norm_seed
```

## `uniform_distribution_rvs` - uniform distribution random variates

### Status

Experimental

### Description

Using the parameters `loc` and `scale`, one obtains the uniformly distributed random variates on [loc, loc + scale]. Return a scalar of type `real` pseudorandom number or a rank one array of such numbers with a size of `array_size`.

### Syntax

`result = [[stdlib_stats_distribution(module):uniform_distribution_rvs(interface)]](loc, scale, [array_size])`

### Arguments

`array_size`: optional argument has intent `in` and is a scalar of type `integer`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real`, or is a rank one array of type `real` with size of `array_size`.

### Example

```fortran
program demo_uniform_rvs
    use stdlib_stats_distribution, only: uniform => uniform_distribution_rvs
    implicit none
    real :: loc, scale, rv(10)

    loc = 0.0
    scale = 1.0
    print *, uniform(loc, scale)            ! single standard uniform random variate in [0, 1)
    rv = uniform(loc, scale, 10)            ! an array of 10 uniform random variates in [0, 1)
    loc = -1.0
    scale = 2.0
    print *, uniform(loc, scale)            ! single uniform random variate in [-1,1)
    loc = -0.5
    scale = 1.0
    rv = uniform(loc, scale, 10)            ! an array of 10 uniform variates in [-0.5, 0.5)
end program demo_uniform_rvs
```

## `uniform_distribution_pdf` - uniform probability density function

### Status

Experimental

### Description

The probability density function of the continuous uniform distribution.

![equation](https://latex.codecogs.com/gif.latex?f(x)=\begin{cases}\frac{1}{scale}&loc\leqslant&space;x<loc&plus;scale\\\\0&x<loc,or,x>loc&plus;scale\end{cases})

### Syntax

`result = [[stdlib_stats_distribution(module):uniform_distribution_pdf(interface)]](x, loc, scale)`

### Arguments

`x`: has intent `in` and is a scalar or an array of type `real`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `x`.

### Example

```fortran
program demo_uniform_pdf
    use stdlib_stats_distribution,  uniform_pdf => uniform_distribution_pdf, &
                                    uniform => uniform_distribution_rvs
    implicit none
    real :: loc, scale, x(3,4)

    loc = 0.0
    scale = 1.0
    print *, uniform_pdf(0.5, loc, scale)             ! a probability density at 0.5 in [0,1)
    print *, uniform_pdf(0.7, -1.0, 2.0)              ! a probability density at 0.7 in [-1,1)
    x = reshape(uniform(loc, scale, 12),[3,4])        ! uniform random variates array in [0,1)
    print *, uniform_pdf(x, loc, scale)               ! probability density array in [0,1)
end program demo_uniform_pdf

```

## `uniform_distribution_cdf` - uniform cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the uniform continuous distribution

![equation](https://latex.codecogs.com/gif.latex?F(x)=\begin{cases}\0&x<loc\\\\\frac{x-loc}{scale}&loc\leq&space;x<loc&plus;scale\\\\1&x\geq&space;loc&plus;scale\end{cases})


### Syntax

`result = [[stdlib_stats_distribution(module):uniform_distribution_cdf(interface)]](x, loc, scale)`

### Arguments

`x`: has intent `in` and is a scalar or an array of type `real`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `x`.

### Example

```fortran
program demo_uniform_cdf
    use stdlib_stats_distribution, uniform_cdf => uniform_distribution_cdf, &
                                   uniform => uniform_distribution_rvs
    implicit none
    real :: loc, scale, x(3,4)

loc = 0.0
    scale = 1.0
    print *, uniform_cdf(0.5, loc, scale)             ! a cumulative at 0.5 in [0,1)
    print *, uniform_cdf(0.7, -1.0, 2.0)              ! a cumulative at 0.7 in [-1,1)
    x = reshape(uniform(loc, scale, 12),[3,4])        ! uniform random variates array in [0,1)
    print *, uniform_cdf(x, loc, scale)               ! cumulative array in [0,1)
end program demo_uniform_cdf

```

## `normal_distribution_rvs` - normal distribution random variates

### Status

Experimental

### Description

A normal continuous random variate distribution, also known as Gaussian, or Gauss or Laplace-Gauss distribution. The location `loc` specifies the mean or expectation. The `scale` specifies the standard deviation. 

The function return a scalar or a rank one array of type `real` pseudorandom number.

### Syntax

`result = [[stdlib_stats_distribution(module):normal_distribution_rvs(interface)]](loc, scale [, array_size])`

### Arguments

`array_size`: optional argument has intent `in` and is a scalar of type `integer`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real`, or is a rank one array of type `real` with size of `array_size`.

### Example

```fortran
program demo_normal_rvs
    use stdlib_stats_distribution, only: norm => normal_distribution_rvs
    implicit none
    real :: loc, scale, rv(10)

    loc = 0.0
    scale = 1.0
    print *, norm(loc, scale)             ! single standard normal random variate
    rv = norm(loc, scale, 10)             ! an array of 10 standard norml random variates 
    print *, norm(-1.0, 2.0)              ! single norma random variate with \mu=-1, \sigma=2
    rv = norm(1.0, 1.0, 10)               ! an array of 10 normal variates with \mu=1, \sigma=1
end program demo_normal_rvs
```

## `normal_distribution_pdf` - normal probability density function

### Status

Experimental

### Description

The probability density function of the continuous normal distribution.

![equation](https://latex.codecogs.com/gif.latex?f(x)=\frac{1}{\sigma&space;\sqrt{2&space;\pi}}&space;e^{-\frac{1}{2}(\frac{x-\mu}{\sigma})^{2}})

### Syntax

`result = [[stdlib_stats_distribution(module):normal_distribution_pdf(interface)]](x, loc, scale)`

### Arguments

`x`: has intent `in` and is a scalar or an array of type `real`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `x`.

### Example

```fortran
program demo_normal_pdf
    use stdlib_stats_distribution, norm_pdf => normal_distribution_pdf, &
                                   norm => normal_distribution_rvs
    implicit none
    real :: loc, scale, x(3,4)

    loc = 0.0
    scale = 1.0
    print *, norm_pdf(1.0,loc, scale)         ! a probability density at 1.0 in standard normal
    print *, norm_pdf(2.0,-1.0, 2.0)          ! a probability density at 2.0 with \mu=-1.0 \sigma=2.0
    x = reshape(norm(0.0, 1.0, 12),[3,4])     ! standard normal random variates array
    print *, norm_pdf(x, 0.0, 1.0)            ! standard normal probability density array
end program demo_norm_pdf
```

## `normal_distribution_cdf` - normal cumulative distribution function

### Status

Experimental

### Description

Cumulative distribution function of the normal continuous distribution

![equation](https://latex.codecogs.com/gif.latex?F(X)=\frac{1}{2}\left&space;[&space;1&space;&plus;&space;erf(\frac{x-\mu}{\sqrt{2}&space;\sigma})&space;\right&space;])

### Syntax

`result = [[stdlib_stats_distribution(module):normal_distribution_cdf(interface)]](x, loc, scale)`

### Arguments

`x`: has intent `in` and is a scalar or an array of type `real`.

`loc`: has intent `in` and is a scalar of type `real`.

`scale`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `x`.

### Example

```fortran
program demo_norm_cdf
    use stdlib_stats_distribution, norm_cdf => normal_distribution_cdf, &
                                   norm => normal_distribution_rvs
    implicit none
    real :: loc, scale, x(3,4)

    print *, norm_cdf(1.0, 0.0, 1.0)                    ! a standard normal cumulative at 1.0
    print *, norm_cdf(2.0, -1.0, 2.0)                   ! a cumulative at 2.0 with \mu=-1 \sigma=2
    x = reshape(norm(0.0, 1.0, 12),[3,4])               ! standard normal random variates array 
    print *, norm_cdf(x, 0.0, 1.0)                      ! standard normal cumulative array
end program demo_norm_cdf

```
## `binomial_distribution_rvs` - binomial distribution random variates

### Status

Experimental

### Description

A binomial discrete random variate distribution. It is used to characterize the number of successes in a sequence of n Bernoulli trials, each with a same probablity of p.

For a single trial, binomial distribution is Bernoulli distribution.

### Syntax

`result = [[stdlib_stats_distribution(module):binomial_distribution_rvs(interface)]](n, p, [array_size])`

### Arguments

`n`: has intent `in` and is a scalar of type `integer`.

`p`: has intent `in` and is a scalar of type `real` with single precision.

`array_size`: optional argument has intent `in` and is a scalar of type `integer`.

### Return value

The result is a scalar of type `integer`, or is a rank one array of type `integer` with a size of `array_size`.

### Example

```fortran
program demo_binomial_rvs
    use stdlib_stats_distribution, only: binom => binomial_distribution_rvs
    implicit none
    integer :: n
    real :: p, rv(10)

    n = 20
    p = 0.3
    print *, binom(n, p)                      ! single binomial random variate
    rv = binom(n, p, 10)                      ! an array of 10 standard binomial random variates 
end program demo_normal_rvs
```

## `binomial_distribution_pmf` - Binomial probability mass function

### Status

Experimental

### Description

The probability mass function of the discrete binomial distribution.

![equation](https://latex.codecogs.com/gif.latex?\begin{align*}p(k)=&\binom{n}{k}p^{k}q^{n-k},k=0,1,2,\cdots,n\\&0\leqslant&space;p\leqslant&space;1,q=1-p\end{align})

### Syntax

`result = [[stdlib_stats_distribution(module):binomial_distribution_pmf(interface)]](k, n, p)`

### Arguments

`k`: has intent `in` and is a scalar or an array of type `integer`.

`n`: has intent `in` and is a scalar of type `integer`.

`p`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `k`.

### Example

```fortran
program demo_binom_pmf
    use stdlib_stats_distribution, only: binom_pmf => binomial_distribution_pmf
    implicit none
    real :: p
    integer :: n, m(2,3)

    n = 20
    p = 0.4
    m = reshape(source=[1,2,3,4,5,6], shape=[2,3])
    print *, binom_pmf(5, n, p)               ! a probability density for 5 in binomial
    print *, binom_pmf(m, n, p)               ! binomial probability density array
end program demo_binom_pmf
```

## `binomial_distribution_cdf` - Binomial cumulative distribution function

### Status

Experimental

### Description

The cumuative distribution function of the discrete binomial distribution.

![equation](https://latex.codecogs.com/gif.latex?\begin{align*}F(k)=&\sum_{i=0}^{k}\binom{n}{i}p^{i}q^{n-i},k=0,1,2,\cdots,n\\&0\leqslant&space;p\leqslant&space;1,q=1-p\end{align})

### Syntax

`result = [[stdlib_stats_distribution(module):binomial_distribution_cdf(interface)]](k, n, p)`

### Arguments

`k`: has intent `in` and is a scalar or an array of type `integer`.

`n`: has intent `in` and is a scalar of type `integer`.

`p`: has intent `in` and is a scalar of type `real`.

### Return value

The result is a scalar of type `real` or an array of type `real` with a shape conformable to input `k`.

### Example

```fortran
program demo_binom_cdf
    use stdlib_stats_distribution, only: binom_cdf => binomial_distribution_cdf
    implicit none
    real :: p
    integer :: n, m(3,2)

    n = 20
    p = 0.4
    m = reshape(source=[1,2,3,4,5,6], shape=[3,2])
    print *, binom_cdf(5, n, p)               ! total probability for k not greater than 5
    print *, binom_cdf(m, n, p)               ! binomial cumulative probability array
end program demo_binom_cdf
```
