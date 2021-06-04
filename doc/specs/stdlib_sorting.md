---
title: Sorting Procedures
---

# The `stdlib_sorting` module

[TOC]

## Overview of sorting

The sorting of collections of data is useful in the analysis of those
collections.
With its absence of generics and limited polymorphism, it is
impractical, in current Fortran, to provide sorting routines for
arbitrary collections of arbitrary types of data.
However Fortran's arrays are by far its most widely used collection,
and arrays of arbitrary types of data can often be sorted in terms of
a single component of intrinsic type.
The Fortran Standard Library therefore provides a module,
`stdlib_sorting`, with procedures to sort arrays of simple intrinsic
numeric types, i.e. the different kinds of integers and reals, the
default assumed length character, and the `stdlib_string_type`
module's `string_type` type.

## Overview of the module

The module `stdlib_sorting` defines several public entities, one
default integer parameter, `int_size`, and three overloaded
subroutines: `ORD_SORT`, `SORT`, and `SORT_INDEX`. The
overloaded subroutines also each have seven specific names for
versions corresponding to different types of array arguments.

### The `int_size` parameter

The `int_size` parameter is used to specify the kind of integer used
in indexing the various arrays. Currently the module sets `int_size`
to the value of `int64` from the `stdlib_kinds` module.

### The module subroutines

The `stdlib_sorting` module provides three different overloaded
subroutines intended to sort three different kinds of arrays of
data:
* `ORD_SORT` is intended to sort simple arrays of intrinsic data
  that have significant sections that were partially ordered before
  the sort;
* `SORT_INDEX` is based on `ORD_SORT`, but in addition to sorting the
  input array, it returns indices that map the original array to its
  sorted version. This enables related arrays to be re-ordered in the
  same way; and
* `SORT` is intended to sort simple arrays of intrinsic data
  that are effectively unordered before the sort.

#### Licensing

The Fortran Standard Library is distributed under the MIT
License. However components of the library may be based on code with
additional licensing restrictions. In particular `ORD_SORT`,
`SORT_INDEX`, and `SORT` are translations of codes with their
own distribution restrictions.

The `ORD_SORT` and `SORT_INDEX` subroutines are essentially
translations to Fortran 2008 of the `"Rust" sort` of the Rust Language
distributed as part of
[`slice.rs`](https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs).
The header of the `slice.rs` file has as its licensing requirements:

    Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
    file at the top-level directory of this distribution and at
    http://rust-lang.org/COPYRIGHT.

    Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
    http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
    <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
    option. This file may not be copied, modified, or distributed
    except according to those terms.

So the license for the `slice.rs` code is compatible with the use of
modified versions of the code in the Fortran Standard Library under
the MIT license.

The `SORT` subroutine is essentially a translation to Fortran
2008 of the
[`introsort`]((http://www.cs.rpi.edu/~musser/gp/introsort.ps) of David
Musser.  David Musser has given permission to include a variant of
`introsort` in the Fortran Standard Library under the MIT license
provided we cite:

    Musser, D.R., “Introspective Sorting and Selection Algorithms,”
    Software—Practice and Experience, Vol. 27(8), 983–993 (August 1997).

as the official source of the algorithm.


#### The `ORD_SORT` subroutine

`ORD_SORT` is a translation of the `"Rust" sort` sorting algorithm
contained in [`slice.rs`]
(https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs).
`"Rust" sort`, in turn, is inspired by the [`timsort` algorithm]
(http://svn.python.org/projects/python/trunk/Objects/listsort.txt)
that Tim Peters created for the Python Language.
`ORD_SORT` is a hybrid stable comparison algorithm combining `merge sort`,
and `insertion sort`. It has always at worst O(N Ln(N)) runtime
performance in sorting random data, having a performance about 15-25%
slower than `SORT` on such data. However it has much better
performance than `SORT` on partially sorted data, having O(N)
performance on uniformly increasing or decreasing data.


When sorting in an increasing order, `ORD_SORT` begins by traversing the array
starting in its tail attempting to identify `runs` in the array, where a run is
either a uniformly decreasing sequence, `ARRAY(i-1) > ARRAY(i)`, or a
non-decreasing, `ARRAY(i-1) <= ARRAY(i)`, sequence. First delimited decreasing
sequences are reversed in their order. Then, if the sequence has less than
`MIN_RUN` elements, previous elements in the array are added to the run using
`insertion sort` until the run contains `MIN_RUN` elements or the array is
completely processed. As each run is identified the start and length of the run
are then pushed onto a stack and the stack is then processed using `merge` until
it obeys the stack invariants:

1. len(i-2) > len(i-1) + len(i)
2. len(i-1) > len(i)

ensuring that processing the stack is, at worst, of order `O(N
Ln(N))`. However, because of the identification of decreasing and
non-decreasing runs, processing of structured data can be much faster,
with processing of uniformly decreasing or non-decreasing arrays being
of order O(N). The result in our tests is that `ORD_SORT` is about
25% slower than `SORT` on purely random data, depending on
the compiler, but can be `Ln(N)` faster than `SORT` on highly
structured data. As a modified `merge sort`, `ORD_SORT` requires the
use of a "scratch" array, that may be provided as an optional `work`
argument or allocated internally on the stack.

Arrays can be also sorted in a decreasing order by providing the argument `reverse
= .true.`.

#### The `SORT_INDEX` subroutine

The `SORT` and `ORD_SORT` subroutines can sort rank 1 isolated
arrays of intrinsic types, but do nothing for the coordinated sorting
of related data, e.g., multiple related rank 1 arrays, higher rank
arrays, or arrays of derived types. For such related data, what is
useful is an array of indices that maps a rank 1 array to its sorted
form. For such a sort, a stable sort is useful, therefore the module
provides a subroutine, `SORT_INDEX`, that generates such an array of
indices based on the `ORD_SORT` algorithm, in addition to sorting
the input array.

The logic of `SORT_INDEX` parallels that of `ORD_SORT`, with
additional housekeeping to keep the array of indices consistent with
the sorted positions of the input array. Because of this additional
housekeeping it has slower runtime performance than `ORD_SORT`.
`SORT_INDEX` requires the use of two "scratch" arrays, that may be
provided as optional `work` and `iwork` arguments or allocated
internally on the stack.

#### The `SORT` subroutine

`SORT` uses the `introsort` sorting algorithm of David Musser.
`introsort` is a hybrid unstable comparison algorithm combining
`quicksort`, `insertion sort`, and `heap sort`. While this algorithm's
runtime performance is always O(N Ln(N)), it is relatively fast on
randomly ordered data, but does not show the improvement in
performance on partly sorted data found for `ORD_SORT`.

First it examines the array and estimates the depth of recursion a
quick sort would require for ideal (random) data, `D =
Ceiling(Ln(N)/Ln(2))`. It then defines a limit to the number of
`quicksort` recursions to be allowed in processing,
`D_limit = factor * D`, where factor is currently 2, and
calls `introsort` proper. `introsort` proper then:

1. Examines the number of elements remaining to be sorted, and, if
   they are less than 16, sorts them using insertion sort and returns;
2. If they are not less than 16, checks whether the current depth of
   recursion exceeds `D_limit` and, if it does, processes the remaining
   elements with heap sort and returns;
3. If the current depth of recursion does not exceed `D_limit`, then
   in effect does a `quicksort` step:

    * Partitions the remaining array using a median of three,
    * Calls `introsort` proper on the leftmost partition,
    * Calls `introsort` proper on the rightmost partition, and then
      returns.

The resulting algorithm is of order O(N Ln(N)) run time performance
for all inputs. Because it relies on `quicksort`, the coefficient of
the O(N Ln(N)) behavior is typically small compared to other sorting
algorithms on random data. On partially sorted data it can show either
slower `heap sort` performance, or enhanced performance by up to a
factor of six. Still, even when it shows enhanced performance, its
performance on partially sorted data is typically an order of
magnitude slower than `ORD_SORT`. Its memory requirements are also
low, being of order O(Ln(N)), while the memory requirements of
`ORD_SORT` and `SORT_INDEX` are of order O(N).

### Specifications of the `stdlib_sorting` procedures

#### `ord_sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input `array` with the elements sorted in order of
increasing, or decreasing, value.

##### Syntax

`call [[stdlib_sorting(module):ord_sort(interface)]]( array[, work, reverse ] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`, `character(*)`, or
`type(string_type)`. It is an `intent(inout)` argument. On input it is
the array to be sorted. If both the type of `array` is real and at
least one of the elements is a `NaN`, then the ordering of the result
is undefined. Otherwise on return its elements will be sorted in order
of non-decreasing value.

`work` (optional): shall be a rank one array of the same type as
array, and shall have at least `size(array)/2` elements. It is an
`intent(out)` argument. It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

`reverse` (optional): shall be a scalar of type default logical. It
is an `intent(in)` argument. If present with a value of `.true.` then
`array` will be sorted in order of non-increasing values in stable
order. Otherwise index will sort `array` in order of non-decreasing
values in stable order.

##### Notes

`ORD_SORT` implements a hybrid sorting algorithm combining
`merge sort`, and `insertion sort`. For most purposes it behaves like
a `merge sort`, providing worst case `O(N Ln(N))` run time performance
for most random arrays, that is typically slower than `SORT`.
However, if the array has significant runs of decreasing or
non-decreasing values, performance can be much better than
`SORT`, with `O(N)` behavior on uniformly decreasing, or
non-decreasing arrays. The optional `work` array replaces "scratch"
memory that would otherwise be allocated on the stack. If `array` is of
any type `REAL` the order of its elements on return undefined if any
element of `array` is a `NaN`.  Sorting of `CHARACTER(*)` and
`STRING_TYPE` arrays are based on the operators `>` and `<`, and not on the
function `LGT`.


##### Example

```fortran
    program demo_ord_sort
      use stdlib_sorting, only: ord_sort
      implicit none
      integer, allocatable :: array1(:), work(:)
    
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      allocate(work, mold = array1)
      call ord_sort(array1, work)
      print*, array1   !print [1, 3, 4, 4, 5, 9, 10]
    end program demo_ord_sort
```

#### `sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input array with the elements sorted in order of increasing, or
decreasing, value.

##### Syntax

`call [[stdlib_sorting(module):sort(interface)]]( array[, reverse] )`

##### Class

Pure generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. `character(*)`, or
`type(string_type)`. It is an `intent(inout)` argument. On return its
input elements will be sorted in order of non-decreasing value.


`reverse` (optional): shall be a scalar of type default logical. It
is an `intent(in)` argument. If present with a value of `.true.` then
`array` will be sorted in order of non-increasing values in unstable
order. Otherwise index will sort `array` in order of non-decreasing
values in unstable order.

##### Notes

`SORT` implements a hybrid sorting algorithm combining
`quicksort`, `merge sort`, and `insertion sort`. For most purposes it
behaves like a `quicksort` with a median of three partition, providing
good, `O(N Ln(N))`, run time performance for most random arrays, but
defaulting to `merge sort` if the structure of the array results in
the `quicksort` not converging as rapidly as expected. If `array` is of
any type `REAL`, the behavior of the sorting is undefined if any
element of `array` is a `NaN`.  Sorting of `CHARACTER(*)` and
`STRING_TYPE` arrays are based on the operators `<`, `<=`, `>`, and
`>=`, and not on the functions `LLT`, `LLE`, `LGT`, or `LGE`.

##### Example


```fortran
    program demo_sort
      use stdlib_sorting, only: sort
      implicit none
      integer, allocatable :: array(:)
    
      array = [ 5, 4, 3, 1, 10, 4, 9]
      call sort(array)
      print*, array   !print [1, 3, 4, 4, 5, 9, 10]
    end program demo_sort
```

#### `sort_index` - creates an array of sorting indices for an input array, while also sorting the array.

##### Status

Experimental

##### Description

Returns the input `array` sorted in the direction requested while
retaining order stability, and an integer array whose elements would
sort the input `array` to produce the output `array`.

##### Syntax

`call [[stdlib_sorting(module):sort_index(interface)]]( array, index[, work, iwork, reverse ] )`

##### Class

Generic subroutine.

##### Arguments

`array`: shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`, `character(*)`, or
`type(string_type)`. It is an `intent(inout)` argument. On input it
will be an array whose sorting indices are to be determined. On return
it will be the sorted array.

`index`: shall be a rank one integer array of kind `int_size` and of
the size of `array`. It is an `intent(out)` argument. On return it
shall have values that are the indices needed to sort the original
array in the desired direction.

`work` (optional): shall be a rank one array of any of the same type as
`array`, and shall have at least `size(array)/2` elements. It is an
`intent(out)` argument. It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

`iwork` (optional): shall be a rank one integer array of kind
`int_size`, and shall have at least `size(array)/2` elements. It
is an `intent(out)` argument.  It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

`reverse` (optional): shall be a scalar of type default logical. It
is an `intent(in)` argument. If present with a value of `.true.` then
`index` will sort `array` in order of non-increasing values in stable
order. Otherwise index will sort `array` in order of non-decreasing
values in stable order.

##### Notes

`SORT_INDEX` implements the hybrid sorting algorithm of `ORD_SORT`,
keeping the values of `index` consistent with the elements of `array`
as it is sorted. As a `merge sort` based algorithm, it is a stable
sorting comparison algorithm. The optional `work` and `iwork` arrays
replace "scratch" memory that would otherwise be allocated on the
stack. If `array` is of any kind of `REAL` the order of the elements in
`index` and `array` on return are undefined if any element of `array`
is a `NaN`. Sorting of `CHARACTER(*)` and `STRING_TYPE` arrays are
based on the operator `>`, and not on the function `LGT`.

It should be emphasized that the order of `array` will typically be
different on return


##### Examples

Sorting a related rank one array:

```Fortran
    subroutine sort_related_data( a, b, work, index, iwork )
        ! Sort `a`, and  also  sort `b` to be reorderd the same way as `a`
        integer, intent(inout)         :: a(:)
        integer(int32), intent(inout)  :: b(:) ! The same size as a
		integer(int32), intent(out)    :: work(:)
		integer(int_size), intent(out) :: index(:)
		integer(int_size), intent(out) :: iwork(:)
		! Find the indices to sort a
        call sort_index(a, index(1:size(a)),&
            work(1:size(a)/2), iwork(1:size(a)/2))
		! Sort b based on the sorting of a
		b(:) = b( index(1:size(a)) )
	end subroutine sort_related_data
```

Sorting a rank 2 array based on the data in a column

```Fortran
	subroutine sort_related_data( array, column, work, index, iwork )
	    ! Reorder rows of `array` such that `array(:, column)` is  sorted
	    integer, intent(inout)         :: array(:,:)
		integer(int32), intent(in)     :: column
		integer(int32), intent(out)    :: work(:)
		integer(int_size), intent(out) :: index(:)
		integer(int_size), intent(out) :: iwork(:)
		integer, allocatable           :: dummy(:)
		integer :: i
		allocate(dummy(size(array, dim=1)))
		! Extract a column of `array`
		dummy(:) = array(:, column)
		! Find the indices to sort the column
		call sort_index(dummy, index(1:size(dummy)),&
		    work(1:size(dummy)/2), iwork(1:size(dummy)/2))
		! Sort a based on the sorting of its column
		do i=1, size(array, dim=2)
		    array(:, i) = array(index(1:size(array, dim=1)), i)
		end do
	end subroutine sort_related_data
```

Sorting an array of a derived type based on the data in one component

```fortran
	subroutine sort_a_data( a_data, a, work, index, iwork )
	    ! Sort `a_data` in terms or its component `a`
	    type(a_type), intent(inout)      :: a_data(:)
		integer(int32), intent(inout)    :: a(:)
		integer(int32), intent(out)    :: work(:)
		integer(int_size), intent(out) :: index(:)
		integer(int_size), intent(out) :: iwork(:)
		! Extract a component of `a_data`
		a(1:size(a_data)) = a_data(:) % a
		! Find the indices to sort the component
		call sort_index(a(1:size(a_data)), index(1:size(a_data)),&
		    work(1:size(a_data)/2), iwork(1:size(a_data)/2))
		! Sort a_data based on the sorting of that component
		a_data(:) = a_data( index(1:size(a_data)) )
	end subroutine sort_a_data
```


### Performance benchmarks

We have performed benchmarks of the procedures on nine different
integer arrays each of size `2**20`:

* Blocks - the array is divided into six blocks, each of distinct
  uniformly increasing integers.
* Decreasing - values decrease uniformly from `2**20-1` to `0`.
* Identical - all integers have the same value of 10.
* Increasing - values increase uniformly from `0` to `2**20-1`.
* Random dense - the integers are generated randomly from a set of
  values from `0` to `2**18-1` so duplicates are dense.
* Random order - a set of integers from `0` to `2**20 - 1` in random
  order.
* Random sparse - the integers are generated randomly from a set of
  values from `0` to `2**22-1` so duplicates are sparse.
* Random-3 - the increasing array has 3 random exchanges of individual
  elements.
* Random-10 - the final ten elements of the increasing array are
  replaced by random values.

On three different default character arrays, each of length 4 and of
size `26**4`:

* Char. Decreasing - values decrease uniformly from `"zzzz"` to
  `"aaaa"`.
* Char. Increasing - values decrease uniformly from `"aaaa"` to
  `"zzzz"`.
* Char. Random - the set of strings from `"aaaa"` to `"zzzz"` in
  random order.

On three different `string_type` arrays, each of length 4 elements and
of size `26**3`:

* String Decreasing - values decrease uniformly from `"zzz"` to
  `"aaa"`.
* String Increasing - values decrease uniformly from `"aaa"` to
  `"zzz"`.
* String Random - the set of strings from `"aaa"` to `"zzz"` in
  random order.

These benchmarks have been performed on two different compilers, both
on a MacBook Pro, featuring a 2.3 GHz Quad-Core Intel Core i5, with 8
GB 2133 MHz LPDDR3 memory. The first compiler was GNU Fortran
(GCC) 10.2.0, with the following results:

|    Type     | Elements |    Array Name   |    Method   |  Time (s) |
|-------------|----------|-----------------|-------------|-----------|
|     Integer | 1048576  |          Blocks |    Ord_Sort |   0.00738 |
|     Integer | 1048576  |      Decreasing |    Ord_Sort |   0.00380 |
|     Integer | 1048576  |       Identical |    Ord_Sort |   0.00220 |
|     Integer | 1048576  |      Increasing |    Ord_Sort |   0.00209 |
|     Integer | 1048576  |    Random dense |    Ord_Sort |   0.17972 |
|     Integer | 1048576  |    Random order |    Ord_Sort |   0.17503 |
|     Integer | 1048576  |   Random sparse |    Ord_Sort |   0.17340 |
|     Integer | 1048576  |        Random 3 |    Ord_Sort |   0.00847 |
|     Integer | 1048576  |       Random 10 |    Ord_Sort |   0.00484 |
|   Character |  456976  |  Char. Decrease |    Ord_Sort |   0.00763 |
|   Character |  456976  |  Char. Increase |    Ord_Sort |   0.00414 |
|   Character |  456976  |    Char. Random |    Ord_Sort |   0.23746 |
| String_type |   17576  | String Decrease |    Ord_Sort |   0.00543 |
| String_type |   17576  | String Increase |    Ord_Sort |   0.00347 |
| String_type |   17576  |   String Random |    Ord_Sort |   0.09461 |
|     Integer | 1048576  |          Blocks |        Sort |   0.10556 |
|     Integer | 1048576  |      Decreasing |        Sort |   0.13348 |
|     Integer | 1048576  |       Identical |        Sort |   0.15719 |
|     Integer | 1048576  |      Increasing |        Sort |   0.05316 |
|     Integer | 1048576  |    Random dense |        Sort |   0.15047 |
|     Integer | 1048576  |    Random order |        Sort |   0.15176 |
|     Integer | 1048576  |   Random sparse |        Sort |   0.15767 |
|     Integer | 1048576  |        Random 3 |        Sort |   0.19907 |
|     Integer | 1048576  |       Random 10 |        Sort |   0.34244 |
|   Character |  456976  |  Char. Decrease |        Sort |   0.30723 |
|   Character |  456976  |  Char. Increase |        Sort |   0.10984 |
|   Character |  456976  |    Char. Random |        Sort |   0.20642 |
| String_type |   17576  | String Decrease |        Sort |   0.15101 |
| String_type |   17576  | String Increase |        Sort |   0.05569 |
| String_type |   17576  |   String Random |        Sort |   0.08499 |
|     Integer | 1048576  |          Blocks |  Sort_Index |   0.01163 |
|     Integer | 1048576  |      Decreasing |  Sort_Index |   0.00720 |
|     Integer | 1048576  |       Identical |  Sort_Index |   0.00451 |
|     Integer | 1048576  |      Increasing |  Sort_Index |   0.00452 |
|     Integer | 1048576  |    Random dense |  Sort_Index |   0.20295 |
|     Integer | 1048576  |    Random order |  Sort_Index |   0.20190 |
|     Integer | 1048576  |   Random sparse |  Sort_Index |   0.20221 |
|     Integer | 1048576  |        Random 3 |  Sort_Index |   0.01406 |
|     Integer | 1048576  |       Random 10 |  Sort_Index |   0.00765 |
|   Character |  456976  |  Char. Decrease |  Sort_Index |   0.00912 |
|   Character |  456976  |  Char. Increase |  Sort_Index |   0.00515 |
|   Character |  456976  |    Char. Random |  Sort_Index |   0.24693 |
| String_type |   17576  | String Decrease |  Sort_Index |   0.00528 |
| String_type |   17576  | String Increase |  Sort_Index |   0.00341 |
| String_type |   17576  |   String Random |  Sort_Index |   0.09554 |

The second compiler was Intel(R) Fortran Intel(R) 64 Compiler Classic
for applications running on Intel(R) 64, Version 2021.2.0 Build
20210228_000000, with the following results:

|    Type     | Elements |   Array Name    |    Method   |  Time (s) |
|-------------|----------|-----------------|-------------|-----------|
|     Integer | 1048576  |          Blocks |    Ord_Sort |   0.00320 |
|     Integer | 1048576  |      Decreasing |    Ord_Sort |   0.00142 |
|     Integer | 1048576  |       Identical |    Ord_Sort |   0.00102 |
|     Integer | 1048576  |      Increasing |    Ord_Sort |   0.00158 |
|     Integer | 1048576  |    Random dense |    Ord_Sort |   0.09859 |
|     Integer | 1048576  |    Random order |    Ord_Sort |   0.09704 |
|     Integer | 1048576  |   Random sparse |    Ord_Sort |   0.09599 |
|     Integer | 1048576  |        Random 3 |    Ord_Sort |   0.00396 |
|     Integer | 1048576  |       Random 10 |    Ord_Sort |   0.00183 |
|   Character |  456976  |  Char. Decrease |    Ord_Sort |   0.00763 |
|   Character |  456976  |  Char. Increase |    Ord_Sort |   0.00341 |
|   Character |  456976  |    Char. Random |    Ord_Sort |   0.21991 |
| String_type |   17576  | String Decrease |    Ord_Sort |   0.01957 |
| String_type |   17576  | String Increase |    Ord_Sort |   0.00573 |
| String_type |   17576  |   String Random |    Ord_Sort |   0.37850 |
|     Integer | 1048576  |          Blocks |        Sort |   0.03668 |
|     Integer | 1048576  |      Decreasing |        Sort |   0.04073 |
|     Integer | 1048576  |       Identical |        Sort |   0.03884 |
|     Integer | 1048576  |      Increasing |        Sort |   0.01279 |
|     Integer | 1048576  |    Random dense |        Sort |   0.06945 |
|     Integer | 1048576  |    Random order |        Sort |   0.07151 |
|     Integer | 1048576  |   Random sparse |        Sort |   0.07224 |
|     Integer | 1048576  |        Random 3 |        Sort |   0.07954 |
|     Integer | 1048576  |       Random 10 |        Sort |   0.14395 |
|   Character |  456976  |  Char. Decrease |        Sort |   0.30367 |
|   Character |  456976  |  Char. Increase |        Sort |   0.11316 |
|   Character |  456976  |    Char. Random |        Sort |   0.20233 |
| String_type |   17576  | String Decrease |        Sort |   0.64479 |
| String_type |   17576  | String Increase |        Sort |   0.23737 |
| String_type |   17576  |   String Random |        Sort |   0.31361 |
|     Integer | 1048576  |          Blocks |  Sort_Index |   0.00643 |
|     Integer | 1048576  |      Decreasing |  Sort_Index |   0.00219 |
|     Integer | 1048576  |       Identical |  Sort_Index |   0.00126 |
|     Integer | 1048576  |      Increasing |  Sort_Index |   0.00130 |
|     Integer | 1048576  |    Random dense |  Sort_Index |   0.12911 |
|     Integer | 1048576  |    Random order |  Sort_Index |   0.13024 |
|     Integer | 1048576  |   Random sparse |  Sort_Index |   0.12956 |
|     Integer | 1048576  |        Random 3 |  Sort_Index |   0.00781 |
|     Integer | 1048576  |       Random 10 |  Sort_Index |   0.00281 |
|   Character |  456976  |  Char. Decrease |  Sort_Index |   0.00779 |
|   Character |  456976  |  Char. Increase |  Sort_Index |   0.00393 |
|   Character |  456976  |    Char. Random |  Sort_Index |   0.22561 |
| String_type |   17576  | String Decrease |  Sort_Index |   0.01878 |
| String_type |   17576  | String Increase |  Sort_Index |   0.00543 |
| String_type |   17576  |   String Random |  Sort_Index |   0.37748 |

