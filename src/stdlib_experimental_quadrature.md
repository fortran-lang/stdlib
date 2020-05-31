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

## `trapz_weights` - trapezoidal rule weights for given abscissas

Given an array of abscissas `x`, computes the array of weights `w` such that if `y` represented function values tabulated at `x`, then `sum(w*y)` produces a trapezoidal rule approximation to the integral.

### Syntax

`result = trapz_weights(x)`

### Arguments

`x`: Shall be a rank-one array of type `real`.

### Return value

The result is a `real` array with the same size and kind as `x`.

If the size of `x` is one, then the sole element of the result is zero.

### Example

```fortran
program demo_trapz_weights
    use stdlib_experimental_quadrature, only: trapz_weights
    implicit none
    real :: x(5) = [0., 1., 2., 3., 4.]
    real :: y(5) = x**2
    real :: w(5) 
    w = trapz_weights(x)
    print *, sum(w*y)
! 22.0
end program

```

# `simps` - integrate sampled values using Simpson's rule

Returns the Simpson's rule integral of an array `y` representing discrete samples of a function. The integral is computed assuming either equidistant abscissas with spacing `dx` or arbitary abscissas `x`. 

Simpson's ordinary ("1/3") rule is used for odd-length arrays. For even-length arrays, Simpson's 3/8 rule is also utilized in a way that depends on the value of `even`. If `even` is negative (positive), the 3/8 rule is used at the beginning (end) of the array. If `even` is zero or not present, the result is as if the 3/8 rule were first used at the beginning of the array, then at the end of the array, and these two results were averaged.

### Syntax

`result = simps(y, x [, even])`

`result = simps(y, dx [, even])`

### Arguments

`y`: Shall be a rank-one array of type `real`.

`x`: Shall be a rank-one array of type `real` having the same kind and size as `y`. 

`dx`: Shall be a scalar of type `real` having the same kind as `y`.

`even`: (Optional) Shall be a default-kind `integer`.

### Return value

The result is a scalar of type `real` having the same kind as `y`.

If the size of `y` is zero or one, the result is zero.

If the size of `y` is two, the result is the same as if `trapz` had been called instead.

### Example

TBD

# `simps_weights` - Simpson's rule weights for given abscissas

Given an array of abscissas `x`, computes the array of weights `w` such that if `y` represented function values tabulated at `x`, then `sum(w*y)` produces a Simpson's rule approximation to the integral.

Simpson's ordinary ("1/3") rule is used for odd-length arrays. For even-length arrays, Simpson's 3/8 rule is also utilized in a way that depends on the value of `even`. If `even` is negative (positive), the 3/8 rule is used at the beginning (end) of the array and the 1/3 rule used elsewhere. If `even` is zero or not present, the result is as if the 3/8 rule were first used at the beginning of the array, then at the end of the array, and then these two results were averaged.

### Syntax

`result = simps_weights(x [, even])`

### Arguments

`x`: Shall be a rank-one array of type `real`.

`even`: (Optional) Shall be a default-kind `integer`.

### Return value

The result is a `real` array with the same size and kind as `x`.

If the size of `x` is one, then the sole element of the result is zero.

If the size of `x` is two, then the result is the same as if `trapz_weights` had been called instead.

### Example

TBD
