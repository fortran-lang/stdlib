---
title: str2num
---

# The `stdlib_str2num` module

This module proposes a function-style interface for string-to-number conversion. It also profits from Fortran's interfaces to implement precision-dependant algorithms to maximize runtime efficiency.

[TOC]

## `to_num` - conversion of strings to numbers

### Status

Experimental

### Description

Convert a string or an array of strings to numerical types.

### Syntax

`number = [[stdlib_str2num(module):to_num(interface)]](string, mold)`

### Arguments

`string`: argument has `intent(in)` and is of type `character(*)`.

`mold`: argument has `intent(in)` and is of numerical type (that is of `integer` or of `real`). **Note**: the mold argument is included to help compilers chose the correct implementation at compile-time. Currently, compilers are not able to disambiguate functions with respect to the left-hand-side of an assignment.

### Return value

Return a scalar of numerical type (i.e., `integer`, or `real`).

### Example

```fortran
program example_string_to_number
  use stdlib_kinds, only: dp
  use stdlib_str2num, only: to_num
  implicit none
  character(:), allocatable :: txt
  real(dp) :: x

  txt = ' 8.8541878128e−12 '
  x = to_num( txt , x )
end program example_string_to_number
```

## `to_num_p` - conversion of a stream of values in a string to numbers

### Status

Experimental

### Description

Convert a stream of values in a string to an array of values.

### Syntax

`number = [[stdlib_str2num(module):to_num_p(interface)]](string, mold)`

### Arguments

`string`: argument has `intent(in)` and is of type `character(*), pointer`.

`mold`: argument has `intent(in)` and is of numerical type (currently of `integer` or `real`). **Note**: the mold argument is included to help compilers chose the correct implementation at compile-time. Currently, compilers are not able to disambiguate functions with respect to the left-hand-side of an assignment.

### Return value

Return a scalar of numerical type (i.e., `integer` or `real`).

### Example

```fortran
program example_str2num
    use stdlib_kinds, only: dp
    use stdlib_str2num, only: to_num_p
    character(:), allocatable, target :: chain
    character(len=:), pointer :: cptr
    real(dp), allocatable :: r(:)
    integer :: i 

    chain = " 1.234   1.E1 1e0     0.1234E0  12.21e+001 -34.5E1"
    allocate( r(6) )
    cptr => chain
    do i =1, 6
        r(i) = to_num_p( cptr , r(i) ) !> the cptr pointer is shifted within the function
    end do
end program
```

## Note
The accuracy of the conversion is implementation dependent; it is recommended that implementers guarantee precision down to the last 3 bits.

**The current implementation has been tested to provide for** :

`sp`  : exact match

`dp`  : precision up-to epsilon(0.0_dp)

`qp` : precision around 200*epsilon(0.0_qp)

Where precision refers to the relative difference between `to_num` and `read`. On the other hand, `to_num` provides speed-ups ranging from 4x to >10x compared to the intrinsic `read`.