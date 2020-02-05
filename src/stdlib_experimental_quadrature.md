# Numerical integration

## Implemented

* `trapz`
* `trapz_weights`

## `trapz` - integrate sampled values using trapezoidal rule

Returns the trapezoidal rule integral of an array `y` representing discrete samples of a function. The integral is computed assuming either equidistant abscissas with spacing `dx` or arbitary abscissas `x`.

### Syntax

`result = trapz(y, x)`

`result = trapz(y, dx)`

### Arguments

`y`: Shall be a rank-one array of type `real`.

`x`: Shall be a rank-one array of type `real` having the same kind and size as `y`. 

`dx`: Shall be a scalar of type `real` having the same kind as `y`.

### Return value

The result is a scalar of type `real` having the same kind as `y`.

If the size of `y` is zero or one, the result is zero.

### Example

```fortran
program demo_trapz
    use stdlib_experimental_quadrature, only: trapz
    implicit none
    real :: x(5) = [0., 1., 2., 3., 4.]
    real :: y(5) = x**2
    print *, trapz(y, x) 
! 22.0
    print *, trapz(y, 0.5) 
! 11.0
end program
```

## `trapz_weights` - compute trapezoidal rule weights for given abscissas

Given an array of abscissas `x`, computes the array of weights `w` such that if `y` represented function values tabulated at `x`, then `sum(w*y)` produces a trapezoidal rule approximation to the integral.

### Syntax

`result = trapz_weights(x)`

### Arguments

`x`: Shall be an array of type `real`.

### Return value

The result is a `real` array with the same size and kind as `x`.

If the size of `x` is one, then the only element of the result is zero.

### Example

```fortran
program demo_trapz_weights
    use stdlib_experimental_quadrature, only: trapz_weights
    implicit none
    real :: x(5) = [0., 1., 2., 3., 4.]
    real :: y(5) = x**2
    real :: w(5) 
    w = trapz_weight(x)
    print *, dot_product(w, y)
! 22.0
end program

```
