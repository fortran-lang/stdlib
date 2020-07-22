---
title: stats
---

# Descriptive statistics

[TOC]

## `corr` - Pearson correlation of array elements

### Version

Experimental

### Description

Returns the Pearson correlation of the elements of `array` along dimension `dim` if the corresponding element in `mask` is `true`.

The Pearson correlation between two rows (or columns), say `x` and `y`, of `array` is defined as:

```
 corr(x, y) = cov(x, y) / sqrt( var(x) * var(y))
```

### Syntax

`result = [[stdlib_stats(module):corr(interface)]](array, dim [, mask])`

### Arguments

`array`: Shall be a rank-1 or a rank-2 array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to `n`, where `n` is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either a scalar or an array of the same shape as `array`.

### Return value

If `array` is of rank 1 and of type `real` or `complex`, the result is of type `real` and has the same kind as `array`.
If `array` is of rank 2 and of type `real` or `complex`, the result is of the same type and kind as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `array` is of rank 1 and of size larger than 1, a scalar equal to 1 is returned. Otherwise, IEEE `NaN` is returned.
If `array` is of rank 2, a rank-2 array  with the corresponding correlations is returned.

If `mask` is specified, the result is the Pearson correlation of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_corr
    use stdlib_stats, only: corr
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    real :: y(1:2, 1:3) = reshape([ -1., 40., -3., 4., 10., 6. ], [ 2, 3])
    print *, corr(x, 1)           !returns 1.
    print *, corr(y, 2)           !returns reshape([ 1., -.32480, -.32480, 1. ], [ 2, 3])
end program demo_corr
```

## `cov` - covariance of array elements

### Version

Experimental

### Description

Returns the covariance of the elements of `array` along dimension `dim` if the corresponding element in `mask` is `true`.

Per default, the covariance is defined as:

```
 cov(array) = 1/(n-1) sum_i (array(i) - mean(array) * (array(i) - mean(array)))
```

where `n` is the number of elements.

The scaling can be changed with the logical argument `corrected`. If `corrected` is `.false.`, then the sum is scaled with `n`, otherwise with `n-1`.


### Syntax

`result = [[stdlib_stats(module):cov(interface)]](array, dim [, mask [, corrected]])`

### Arguments

`array`: Shall be a rank-1 or a rank-2 array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to `n`, where `n` is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either a scalar or an array of the same shape as `array`.

`corrected` (optional): Shall be a scalar of type `logical`. If `corrected` is `.true.` (default value), the sum is scaled with `n-1`. If `corrected` is `.false.`, then the sum is scaled with `n`.

### Return value

If `array` is of rank 1 and of type `real` or `complex`, the result is of type `real` corresponding to the type of `array`.
If `array` is of rank 2 and of type `real` or `complex`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `array` is of rank 1, a scalar with the covariance (that is the variance) of all elements in `array` is returned.
If `array` is of rank 2, a rank-2 array is returned.

If `mask` is specified, the result is the covariance of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_cov
    use stdlib_stats, only: cov
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    real :: y(1:2, 1:3) = reshape([ 1., 2., 3., 4., 5., 6. ], [ 2, 3])
    print *, cov(x, 1)                          !returns 3.5
    print *, cov(x, 1, corrected = .false.)     !returns 2.9167
    print *, cov(y, 1)                          !returns a square matrix of size 3 with all elements equal to 0.5
end program demo_cov
```

## `mean` - mean of array elements

### Version

Experimental

### Description

Returns the mean of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

### Syntax

`result = [[stdlib_stats(module):mean(interface)]](array [, mask])`

`result = [[stdlib_stats(module):mean(interface)]](array, dim [, mask])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to `n`, where `n` is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either a scalar or an array of the same shape as `array`.

### Return value

If `array` is of type `real` or `complex`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the mean of all elements in `array` is returned. Otherwise, an array of rank `n-1`, where `n` equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the mean of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_mean
    use stdlib_stats, only: mean
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    real :: y(1:2, 1:3) = reshape([ 1., 2., 3., 4., 5., 6. ], [ 2, 3])
    print *, mean(x)                                  !returns 3.5
    print *, mean(y)                                  !returns 3.5
    print *, mean(y, 1)                               !returns [ 1.5, 3.5, 5.5 ]
    print *, mean(y, 1,y > 3.)                        !returns [ NaN, 4.0, 5.5 ]
end program demo_mean
```

## `moment` - central moments of array elements

### Version

Experimental

### Description

Returns the _k_-th order central moment of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

If a scalar or an array `center` is provided, the function returns the _k_-th order moment about 'center', of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.


The _k_-th order central moment is defined as :

```
 moment(array) = 1/n sum_i (array(i) - mean(array))^k
```

where `n` is the number of elements.

The _k_-th order moment about `center` is defined as :

```
 moment(array) = 1/n sum_i (array(i) - center)^k
```

### Syntax

`result = [[stdlib_stats(module):moment(interface)]](array, order [, center [, mask]])`

`result = [[stdlib_stats(module):moment(interface)]](array, order, dim [, center [, mask]])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`order`: Shall be an scalar of type `integer`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to `n`, where `n` is the rank of `array`.

`center` (optional): Shall be a scalar of the same type of `result` if `dim` is not provided. If `dim` is provided, `center` shall be a scalar or an array (with a shape similar to that of `array` with dimension `dim` dropped) of the same type of `result`.

`mask` (optional): Shall be of type `logical` and either a scalar or an array of the same shape as `array`.

### Return value

If `array` is of type `real` or `complex`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the _k_-th (central) moment of all elements in `array` is returned. Otherwise, an array of rank `n-1`, where `n` equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the _k_-th  (central) moment of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_moment
    use stdlib_stats, only: moment
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    real :: y(1:2, 1:3) = reshape([ 1., 2., 3., 4., 5., 6. ], [ 2, 3])
    print *, moment(x, 2)                            !returns 2.9167
    print *, moment(y, 2)                            !returns 2.9167
    print *, moment(y, 2, 1)                         !returns [0.25, 0.25, 0.25]
    print *, moment(y, 2, 1, mask = (y > 3.))        !returns [NaN, 0., 0.25]
    print *, moment(x, 2, center = 0.)               !returns 15.1667
    print *, moment(y, 1, 1, center = 0.)            !returns [1.5, 3.5, 5.5]
end program demo_moment
```

## `var` - variance of array elements

### Version

Experimental

### Description

Returns the variance of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

Per default, the variance is defined as the best unbiased estimator and is computed as:

```
 var(array) = 1/(n-1) sum_i (array(i) - mean(array))^2
```

where `n` is the number of elements.

The use of the term `n-1` for scaling is called Bessel 's correction. The scaling can be changed with the logical argument `corrected`. If `corrected` is `.false.`, then the sum is scaled with `n`, otherwise with `n-1`.


### Syntax

`result = [[stdlib_stats(module):var(interface)]](array [, mask [, corrected]])`

`result = [[stdlib_stats(module):var(interface)]](array, dim [, mask [, corrected]])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to `n`, where `n` is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either a scalar or an array of the same shape as `array`.

`corrected` (optional): Shall be a scalar of type `logical`. If `corrected` is `.true.` (default value), the sum is scaled with `n-1`. If `corrected` is `.false.`, then the sum is scaled with `n`.

### Return value

If `array` is of type `real` or `complex`, the result is of type `real` corresponding to the type of `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the variance of all elements in `array` is returned. Otherwise, an array of rank `n-1`, where `n` equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the variance of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

If the variance is computed with only one single element, then the result is IEEE `NaN` if `corrected` is `.true.` and is `0.` if `corrected` is `.false.`.

### Example

```fortran
program demo_var
    use stdlib_stats, only: var
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    real :: y(1:2, 1:3) = reshape([ 1., 2., 3., 4., 5., 6. ], [ 2, 3])
    print *, var(x)                                  !returns 3.5
    print *, var(x, corrected = .false.)             !returns 2.9167
    print *, var(y)                                  !returns 3.5
    print *, var(y, 1)                               !returns [0.5, 0.5, 0.5]
    print *, var(y, 1, y > 3.)                       !returns [NaN, NaN, 0.5]
    print *, var(y, 1, y > 3., corrected=.false.)    !returns [NaN, 0., 0.25]
end program demo_var
```
