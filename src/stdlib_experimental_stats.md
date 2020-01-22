# Descriptive statistics

## Implemented

 * `mean`

## `mean` - mean of array elements

### Description

Returns the mean of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided.

### Syntax

`result = mean(array)`

`result = mean(array, dim)`

### Arguments

`array`: Shall be an array of type `integer`, or `real`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to n, where n is the rank of `array`.

### Return value

If `array` is of type `real`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `double precision`.

If `dim` is absent, a scalar with the mean of all elements in `array` is returned. Otherwise, an array of rank n-1, where n equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

### Example

```fortran
program test
    use stdlib_experimental_stats, only: mean
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    print *, mean(x)                            !returns 21.
    print *, mean( reshape(x, [ 2, 3 ] ))       !returns 21.
    print *, mean( reshape(x, [ 2, 3 ] ), 1)    !returns [ 3., 7., 11. ]
end program
```
