# Descriptive statistics

## Implemented

 * `mean`

## `mean` - mean of array elements

### Description

Returns the mean of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

### Syntax

`result = mean(array [, mask])`

`result = mean(array, dim [, mask])`

### Arguments

`array`: Shall be an array of type `integer`, or `real`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to n, where n is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either by a scalar or an array of the same shape as `array`.

### Return value

If `array` is of type `real`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `double precision`.

If `dim` is absent, a scalar with the mean of all elements in `array` is returned. Otherwise, an array of rank n-1, where n equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the mean of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_mean
    use stdlib_experimental_stats, only: mean
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    print *, mean(x)                            !returns 3.5
    print *, mean( reshape(x, [ 2, 3 ] ))       !returns 3.5
    print *, mean( reshape(x, [ 2, 3 ] ), 1)    !returns [ 1.5, 3.5, 5.5 ]
    print *, mean( reshape(x, [ 2, 3 ] ), 1,&
                   reshape(x, [ 2, 3 ] ) > 3.)  !returns [ NaN, 4.0, 5.5 ]
end program demo_mean
```
