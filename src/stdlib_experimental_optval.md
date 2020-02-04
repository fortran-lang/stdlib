# Default values for optional arguments

## Implemented

* `optval`

## `optval` - fallback value for optional arguments

### Description

Returns `x` if it is PRESENT, otherwise `default`. 

This function is intended to be called in a procedure with one or more OPTIONAL arguments, in order to conveniently fall back to a default value if an OPTIONAL argument is not PRESENT.

### Syntax

`result = optval(x, default)`

### Arguments

`x`: Shall be of type `integer`, `real`, `complex`, or `logical`, or a scalar of type `character`.

`default`: Shall have the same type, kind, and rank as `x`.

### Return value

If `x` is PRESENT, the result is `x`, otherwise the result is `default`.

### Example

```fortran
program demo_optval
    use stdlib_experimental_optval, only: optval
    implicit none
    print *, root(64.0)
! 8.0
    print *, root(64.0, 3)
! 4.0
contains
    real function root(x, n)
	real, intent(in) :: x
        integer, intent(in), optional :: n
        root = x**(1.0/optval(n, 2))
    end function root
end program demo_optval
```
