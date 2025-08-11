---
title: unique function
---

# The `unique` function

[TOC]

## Introduction

This function returns an array containing only the unique values extracted from an input array. This is useful for removing duplicates from datasets and finding the distinct elements in a collection.

## Status

The `unique` function is currently in **experimental** status.

## Version History

|Version|Change|
|---|---|
|v0.1.0|Initial functionality in experimental status|

## Requirements

This function has been designed to handle arrays of different types, including intrinsic numeric types, character arrays, and `string_type` arrays. The function should be efficient while maintaining an easy-to-use interface.

## Usage

```fortran
! Get unique values from an integer array
integer :: x(5) = [1, 2, 3, 3, 4]
integer, allocatable :: y(:)
y = unique(x)    ! y will be [1, 2, 3, 4]

! Get sorted unique values from a real array
real :: a(8) = [3.1, 2.5, 7.2, 3.1, 2.5, 8.0, 7.2, 9.5]
real, allocatable :: b(:)
b = unique(a, sorted=.true.)  ! b will be [2.5, 3.1, 7.2, 8.0, 9.5]
```

## API

### `unique` - Returns unique values from an array

#### Interface

```fortran
pure function unique(array, sorted) result(unique_values)
    <type>, intent(in) :: array(:)
    logical, intent(in), optional :: sorted
    <type>, allocatable :: unique_values(:)
end function unique
```

where `<type>` can be any of:
* `integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`
* `real(sp)`, `real(dp)`, `real(xdp)`, `real(qp)`
* `complex(sp)`, `complex(dp)`, `complex(xdp)`, `complex(qp)`
* `character(len=*)`
* `type(string_type)`

#### Arguments

`array`: Array whose unique values need to be extracted.

`sorted` (optional): Whether the output vector needs to be sorted or not. Default is `.false.`.

#### Result

The function returns an allocatable array containing only the unique values from the input array. 

If `sorted` is `.true.`, the returned array will be sorted in order of non-decreasing values. 

If `sorted` is `.false.` (the default), the order of elements is unspecified but generally reflects the order of first appearance of each unique value in the input array.

## Examples

### Example 1: Basic usage with integers

```fortran
program example_unique_integers
    use stdlib_sorting, only: unique
    implicit none
    
    integer :: data(10) = [1, 2, 3, 3, 4, 5, 5, 6, 6, 6]
    integer, allocatable :: unique_values(:)
    
    ! Get unique values
    unique_values = unique(data)
    
    ! Print the results
    print *, "Original array: ", data
    print *, "Unique values:  ", unique_values
    
end program example_unique_integers
```

Expected output:
```
Original array:  1 2 3 3 4 5 5 6 6 6
Unique values:   1 2 3 4 5 6
```

### Example 2: Using the sorted option with real values

```fortran
program example_unique_reals
    use stdlib_kinds, only: sp
    use stdlib_sorting, only: unique
    implicit none
    
    real(sp) :: data(8) = [3.1, 2.5, 7.2, 3.1, 2.5, 8.0, 7.2, 9.5]
    real(sp), allocatable :: unique_values(:)
    
    ! Get unique values in sorted order
    unique_values = unique(data, sorted=.true.)
    
    ! Print the results
    print *, "Original array: ", data
    print *, "Sorted unique values: ", unique_values
    
end program example_unique_reals
```

Expected output:
```
Original array:  3.1 2.5 7.2 3.1 2.5 8.0 7.2 9.5
Sorted unique values:  2.5 3.1 7.2 8.0 9.5
```

### Example 3: Working with character arrays

```fortran
program example_unique_strings
    use stdlib_sorting, only: unique
    implicit none
    
    character(len=6) :: data(7) = ["apple ", "banana", "cherry", "apple ", "date  ", "banana", "cherry"]
    character(len=6), allocatable :: unique_values(:)
    integer :: i
    
    ! Get unique values
    unique_values = unique(data)
    
    ! Print the results
    print *, "Original array:"
    do i = 1, size(data)
        print *, data(i)
    end do
    
    print *, "Unique values:"
    do i = 1, size(unique_values)
        print *, unique_values(i)
    end do
    
end program example_unique_strings
```

## Implementation Notes

The implementation uses a sorting-based approach to identify unique elements efficiently. When `sorted=.true.`, the algorithm sorts the input array and then identifies adjacent duplicate elements. When `sorted=.false.`, the function still uses sorting internally but ensures that the order of first appearance is preserved.

## Future Enhancements

Future versions might include additional features:

1. Return the indices of the first occurrence of each unique element
2. Return indices that can reconstruct the original array from the unique elements
3. Support for multi-dimensional arrays
4. Tolerance parameter for floating-point comparisons

## Related Functions

* `sort` - Sorts an array in ascending or descending order
* `sort_index` - Creates index array that would sort an array
* `ord_sort` - Performs a stable sort on an array 