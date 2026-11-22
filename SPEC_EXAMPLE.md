# `stdlib_math`

`stdlib_math` module provides general purpose mathematical functions.

* [`arange`](#arange)
* [`clip`](#clip)
* [`gcd`](#gcd)
* [`linspace`](#linspace)
* [`logspace`](#logspace)

## `arange`

Creates a one-dimensional array of the `integer`/`real` type with fixed-spaced
values of given spacing, within a given interval.

### Interface

```fortran
pure function arange(start, end, step) result(res)
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, intent(in) :: start
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, intent(in), optional :: end, step
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, allocatable :: res(:)
```

All arguments must have the same type and kind.

### Arguments

* `start`: Start of the interval if at least `end` is present.
  Otherwise, its value is the end of the interval, and the start of the interval is 1.
* `end`: End of the interval.
* `step`: Increment of the fixed-space array. Default value is 1 if omitted.

### Result

The result is an array of fixed-spaced values from `start` to `end` with an increment of `step`.

### Example

```fortran
program example_arange
  use stdlib_math, only: arange

  print *, arange(3)              ! [1,2,3]
  print *, arange(-1)             ! [1,0,-1]
  print *, arange(0, 2)           ! [0,1,2]
  print *, arange(1, -1)          ! [1,0,-1]
  print *, arange(0, 2, 2)        ! [0,2]

  print *, arange(3.0)            ! [1.0,2.0,3.0]
  print *, arange(0.0, 5.0)       ! [0.0,1.0,2.0,3.0,4.0,5.0]
  print *, arange(0.0, 6.0, 2.5)  ! [0.0,2.5,5.0]

  print *, (1.0, 1.0) * arange(3) ! [(1.0,1.0), (2.0,2.0), (3.0,3.0)]

  print *, arange(0.0, 2.0, -2.0) ! [0.0,2.0].     Not recommended: `step` argument is negative!
  print *, arange(0.0, 2.0, 0.0)  ! [0.0,1.0,2.0]. Not recommended: `step` argument is zero!

end program example_arange
```

## `clip`

Returns a value in interval [xmin, xmax] (inclusive) that is closest to the input value x.

### Interface

```fortran
elemental function clip(x, xmin, xmax) result(res)
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, intent(in) :: x
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, intent(in) :: xmin
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}}, intent(in) :: xmax
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}} :: res
```

All arguments must have the same type and kind.

### Arguments

* `x`: Input value to clip
* `xmin`: Lower bound of the clipping interval
* `xmax`: Upper bound of the clipping interval

### Result

Result is `xmin` if `x < xmin`, `xmax` if `x > xmax`, and `x` otherwise.

## Example

```fortran
program example_clip
  use stdlib_math, only: clip
  implicit none

  print *, clip(12, -5, 5)     !  5
  print *, clip(-7, -5, 5)     ! -5
  print *, clip(3, -5, 5)      !  3
  print *, clip(0.1, 1.2, 5.6) ! 1.2

end program example_clip
```

## `gcd`

Returns the greatest common denominator of two numbers.

### Interface

```fortran
elemental function gcd(a, b) result(res)
  integer{int8, int16, int32, int64}, intent(in) :: a
  integer{int8, int16, int32, int64}, intent(in) :: b
  integer{int8, int16, int32, int64}, intent(in) :: res
```

All arguments must have the same type and kind.

### Arguments

* `a`: First input argument
* `b`: Second input argument

### Result

Greatest common denominator of `a` and `b`.

### Example

```
program example_gcd
  use stdlib_math, only: gcd
  implicit none

  print *, gcd(48, 18) ! 6

end program example_gcd
```

## `linspace`

Create a linearly spaced one-dimensional array.

### Interface

```fortran
function linspace(start, end) result(res)
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: start
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: end
  real{sp, dp, qp} :: res(DEFAULT_LINSPACE_LENGTH)
```

```fortran
function linspace(start, end, n) result(res)
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: start
  {integer{int8, int16, int32, int64}, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: end
  integer, intent(in) :: n
  {real{sp, dp, qp}, complex{sp, dp, qp}} :: res(n)
```

`start` and `end` must have the same type and kind.
`res` must have the same type and kind as `start` and `end` if their type is `real` or `complex`.
Otherwise, the type and kind of `res` are `real(dp)`.
The value of `DEFAULT_LINSPACE_LENGTH` is 100.

### Arguments

* `start`: Starting value of the linearly spaced array
* `end`: Ending value of the linearly spaced array
* `n`: Number of elements to return. Default value is 100 if omitted.

### Result

A rank-1 array of linearly spaced values between `start` and `end`.
The number of elements in the array is `n` if present, and 100 otherwise.

### Example

```fortran
program example_linspace
  use stdlib_math, only: clip
  implicit none

  print *, linspace(1, 100)              ! [1., 2., ... , 99. , 100.]
  print *, linspace(1., 100.)            ! [1., 2., ... , 99. , 100.] (same as above)
  print *, linspace(10, 100, 10)         ! [10., 20., ... , 90. , 100.]
  print *, linspace((1.,1.), (3.,5.), 3) ! [(1.,1.), (2.,3.), (3.,5.)]

end program example_linspace
```

## `logspace`

Create a logarithmically spaced one-dimensional array.

### Interface

```fortran
function logspace(start, end) result(res)
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: start
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: end
  {real{sp, dp, qp}, complex{sp, dp, qp}} :: res(DEFAULT_LOGSPACE_LENGTH)
```

```fortran
function logspace(start, end, n) result(res)
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: start
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: end
  integer, intent(in) :: n
  {real{sp, dp, qp}, complex{sp, dp, qp}} :: res(n)
```

```fortran
function logspace(start, end, n, base) result(res)
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: start
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: end
  integer, intent(in) :: n
  {integer, real{sp, dp, qp}, complex{sp, dp, qp}}, intent(in) :: base
  {real{sp, dp, qp}, complex{sp, dp, qp}} :: res(n)
```

`start` and `end` must have the same type and kind.
`res` must have the same type and kind as `start` and `end` if their type is `real` or `complex`.
Otherwise, the type and kind of `res` are `real(dp)`.
The value of `DEFAULT_LOGSPACE_LENGTH` is 50.

### Arguments

* `start`: Exponent of the starting value in the result array such that `base**start` is the starting value.
* `end`: Exponent of the starting value in the result array such that `base**end` is the ending value.
* `n`: Number of elements to return as the result. Default value is 50 if omitted.
* `base`: Base of the logarithm to use. Default value is 10 if omitted.

### Result

A rank-1 array of logarithmically spaced values between `base**start` and `base**end`.
The number of elements in the array is `n` if present, and 50 otherwise.
If `base` is provided and the type of `start` and `end` arguments is `real` or `integer`,
then the type of the result is the same as the type of `start` and `end` arguments.
If `base` is provided and the type of `start` and `end` arguments is `complex`,
then the type of the result is `complex` as well.

### Example

```fortran
program example_logspace
  use stdlib_math, only: logspace
  implicit none

  print *, logspace(-2, 2)      ! [1d-2, ... , 1d2]
  print *, logspace(0, 3, 4)    ! [1.d0, 1.d1, 1.d2, 1.d3]
  print *, logspace(0, 3, 4, 2) ! [1., 2., 4., 8.]

end program example_logspace
```
