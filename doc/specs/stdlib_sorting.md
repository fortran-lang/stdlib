---
title: Sorting Procedures
---

# The `stdlib_sorting` module

(TOC)

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
`stdlib_sorting`, with procedures to sort arrays of single intrinsic
numeric types, i.e. the different kinds of integers and reals.

## Overview of the module

The module `stdlib_sorting` defines several public entities one
default integer parameter, `int_size`, and three overloaded
subroutines: `ORD_SORT`, `UNORD_SORT`, and `ORD_SORTING`. The
overloaded subroutines also each have seven specific names for
versions corresponding to differend types of array arguments.

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
* `ORD_SORTING` is intended to provide indices for sorting arrays of
  derived type data, based on the ordering of an intrinsic component
  of the derived type; and
* `UNORD_SORT` is intended to sort simple arrays of intrinsic data
  that are effectively unordered before the sort.

#### Licensing

The Fortran Standard Library is distributed under the MIT
License. However components of the library may be based on code with
additional licensing restriction. In particular `ORD_SORT`,
`ORD_SORTING`, and `UNORD_SORT` are translations of codes with their
own distribution restrictions.

The `ORD_SORT` and `ORD_SORTING` subroutines are essentially
translations to Fortran 2008 of the `"rust" sort` of the Rust Lsnguage
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

so the license for the `slice.rs` code is compatible with the use of
modified versions of the code in the Fortran Standard Library under
the MIT license.

The `UNORD_SORT` subroutine is essentially a translation to Fortran
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
slower than `UNORD_SORT` on such data. However it has much better
performance than `UNORD_SORT` on partially sorted data, having O(N)
performance on uniformly increasing or decreasing data.


`ORD_SORt` begins by traversing the array starting in its tail
attempting to identify `runs` in the array, where a run is either a
uniformly decreasing sequence, `ARRAY(i-1) > ARRAY(i)`, or a
non-decreasing, `ARRAY(i-1) <= ARRAY(i)`, sequence. Once deliminated
decreasing sequences are reversed in their order. Then, if the
sequence has less than `MIN_RUN` elements, previous elements in the
array are added to the run using `insertion sort` until the run
contains `MIN_RUN` elements or the array is completely processed. As
each run is identified the start and length of the run 
is then pushed onto a stack and the stack is then processed using
`merge` until it obeys the stack invariants:

1. len(i-2) > len(i-1) + len(i)
2. len(i-1) > len(i)

ensuring that processing the stack is, at worst, of order `O(N
Ln(N))`. However, because of the identification of decreasing and
non-decreasing runs, processing of structured data can be much faster,
with processing of uniformly decreasing or non-decreasing arrays being
of order O(N). The result in our tests is that `ORD_SORT` is about
25% slower than `UNORD_SORT` on purely random data, depending on
the compiler, but can be `Ln(N)` faster than `UNORD_SORT` on highly
structured data. As a modified `merge sort`, `ORD_SORT` requires the
use of a "scratch" array, that may be provided as an optional `work`
argument or allocated internally on the stack.

#### The `ORD_SORTING` subroutine

The `UNORD_SORT` and `ORD_SORT` subroutines can sort rank 1 isolated
arrays of intrinsic types, but do nothing for the coordinated sorting
of related data, e.g., multiple related rank 1 arrays, higher rank
arrays, or arrays of derived types. For such related data, what is
useful is an array of indices that maps a rank 1 array to its sorted
form. For such a sort, a stable sort is useful, therefore the module
provides a subroutine, `ORD_SORTING`, that generates such an array of
indices based on the `ORD_SORT` algorithm.

The logic of `ORD_SORTING` parallels that of `ORD_SORT`, with
additional housekeeping to keep the array of indices consistent with
the sorted positions of the input array. Because of this additional
housekeeping it has slower runtime performance than `ORD_SORT`.
`ORD_SORTING` requires the use of two "scratch" arrays, that may be
provided as optional `work` and `iwork` arguments or allocated
internally on the stack.

#### The `UNORD_SORT` subroutines

`UNORD_SORT` uses the `introsort` sorting algorithm of David Musser.
`introsort` is a hybrid unstable comparison algorithm combining
`quicksort`, `insertion sort`, and `heap sort`. While this algorithm's
runtime performance is always O(N Ln(N)), it is relatively fast on
randomly ordered data, but inconsistent in performance on partly
sorted data.as the official source of the algorithm.

As with `introsort`, `UNORD_SORT` is an unstable hybrid algorithm.
First it examines the array and estimates the depth of recursion a
quick sort would require for ideal (random) data, `D =
Ceiling(Ln(N)/Ln(2))`. It then defines a limit to the number of
quicksort recursions to be allowed in processing,
`D_limit = factor * D`, where factor is currently 2, and
calls `introsort` proper. `introsort` proper then:

1. Examines the number of elements remaining to be sorted, and, if
   they are less than 16, sorts them using insertion sort and returns;
2. If they are not less than 16, checks whether the current depth of
   recursion exceeds `D_limit` and, if it does, processes the remaining
   elements with heap sort and returns;
3. If the current depth of recursion does not exceed `D_limit`, then
   in effect does a quicksort step:

    * Partitions the remaining array using a median of three,
    * Calls introsort proper on the leftmost partition,
    * Calls introsort proper on the rightmost partition, and then
      returns.

The resulting algorithm is of order O(N Ln(N)) run time
performance for all inputs. Because it relies on `quicksort`, the
coefficient of the O(N Ln(N)) behavior is typically small compared to
other sorting algorithms on random data. On partially sorted data it
can show either slower `heap sort` performance, or enhanced
performance by up to a factor of six. Still, even when it shows
enhanced performance, its performance on partially sorted data is
typically an order of magnitude slower then `ORD_SORT`.

### Tentative specifications of the `stdlib_sorting` procedures

#### `ord_sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input `array` with the elements sorted in order of
increasing value.

##### Syntax

`call [[stdlib_sorting(module):ord_sort(subroutine)]]ord_sort ( array[, work ] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(real32)`, `real(real64)`, or `real(real128)`. It is an
`intent(inout)` argument. On input it is the array to be sorted. If
both the type of `array` is real and at least one of the elements is a
`NaN`, then the ordering of the result is undefined. Otherwise on
return its elements will be sorted in order of non-decreasing value.

`work` (optional): shall be a rank one array of any of the same type as
array, and shall have at least `size(array)/2` elements. It is an
`intent(inout)` argument. It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

##### Notes

`ORD_SORT` implements a hybrid sorting algorithm combining
`merge sort`, and `insertion sort`. For most purposes it behaves like
a `merge sort`, providing worst case `O(N Ln(N))` run time performance
for most random arrays, that is typically slower than `UNORD_SORT`.
However, if the array has significant runs of decreasing or
non-decreasing values, performance can be much better than
`UNORD_SORT`, with `O(N)` behavior on uniformly decreasing, or
non-decreasing arrays. The optional `work` array replaces "scratch"
memory that would otherwise be allocated on the stack. If `array` is of
any type `REAL` the order of its elements on return undefined if any
element of `array` is a `NaN`.


##### Example

```Fortran
	...
	! Read arrays from sorted files
	call read_sorted_file( 'dummy_file1', array1 )
	call read_sorted_file( 'dummy_file2', array2 )
	! Concatenate the arrays
	allocate( array( size(array1) + size(array2) ) )
	array( 1:size(array1) ) = array1(:)
	array( size(array1)+1:size(array1)+size(array2) ) = array2(:)
	! Sort the resulting array
	call ord_sort( array, work )
	! Process the sorted array
	call array_search( array, values )
	...
```

#### `ord_sorting` - creates an array of sorting indices for an input array.

##### Status

Experimental

##### Description

Returns an integer array whose elements would sort the input array in
the specified direction retaining order stability.

##### Syntax

`call [[stdlib_sorting(module):ord_sorting(subroutine)]]ord_sorting ( array, index[, work, iwork, reverse, zero_based ] )`

##### Class

Generic subroutine.

##### Arguments

`array`: shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(real32)`, `real(real64)`, or `real(real128)`. It is an
`intent(inout)` argument. On input it will be an array whose sorting
indices are to be determined. On return it will be the sorted
array.

`index`: shall be a rank one integer array of kind `int_size` and of
the size of `array`. It is an `intent(out)` argument. On return it
shall have values that are the indices needed to sort the original
array in the desired direction.

`work` (optional): shall be a rank one array of any of the same type as
array, and shall have at least `size(array)/2` elements. It is an
`intent(inout)` argument. It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

`iwork` (optional): shall be a rank one integer array of kind
`int_size`, and shall have at least `size(array)/2` elements. It
is an `intent(inout)` argument. Its contents on return are undefined.

`reverse` (optional): shall be a scalar of type default logical. It
is an `intent(in)` argument. If present with a value of `.true.` then
`index` will sort `array` in order of non-increasing values in stable
order. Otherwise index will sort `array` in order of non-decreasing
values in stable order.

`zero_based` (optional): shall be a scalar of type default logical.
It is an `intent(in)` argument. If present with the value `.true.`
the values of `index` will be for zero based array indexing,
otherwise the values of index will be for one's based array
indexing.

##### Notes

`ORD_SORTING` implements the hybrid sorting algorithm of `ORD_SORT`,
keeping the values of `index` consistent with the elements of `array`
as it is sorted. As a `merge sort` based algorithm, it is a stable
sorting comparison algorithm. The optional `work` and `iwork` arrays
replace "scratch" memory that would otherwise be allocated on the
stack. If `array` is of any kind of `REAL` the order of the elements in
`index` and `array` on return are undefined if any element of `array`
is a `NaN`. It should be emphasized that the order of `array` will
typically be different on return.


##### Examples

Sorting a related rank one array:

```Fortran
	subroutine sort_related_data( a, b, work, index, iwork )
	    ! Sort `b` in terms or its related array `a`
	    integer, intent(inout)           :: a(:)
		integer(int32), intent(inout)    :: b(:) ! The same size as a
		integer(int32), intent(inout)    :: work(:)
		integer(int_size), intent(inout) :: index(:)
		integer(int_size), intent(inout) :: iwork(:)
		! Find the indices to sort a
		call ord_sorting(a, index(1:size(a)),&
		    work(1:size(a)/2), iwork(1:size(a)/2))
		! Sort b based on the sorting of a
		b(:) = b( index(1:size(a)) )
	end subroutine sort_related_data
```

Sorting a rank 2 array based on the data in a column

```Fortran
	subroutine sort_related_data( array, column, work, index, iwork )
	    ! Sort `a_data` in terms or its component `a`
	    integer, intent(inout)           :: a(:,:)
		integer(int32), intent(in)       :: column
		integer(int32), intent(inout)    :: work(:)
		integer(int_size), intent(inout) :: index(:)
		integer(int_size), intent(inout) :: iwork(:)
		integer, allocatable             :: dummy(:)
		integer :: i
		allocate(dummy(size(a, dim=1)))
		! Extract a component of `a_data`
		dummy(:) = a(:, column)
		! Find the indices to sort the column
		call ord_sorting(dummy, index(1:size(dummy)),&
		    work(1:size(dummy)/2), iwork(1:size(dummy)/2))
		! Sort a based on the sorting of its column
		do i=1, size(a, dim=2)
		    a(:, i) = a(index(1:size(a, dim=1)), i)
		end do
	end subroutine sort_related_data
```

Sorting an array of a derived type based on the dsta in one component
```Fortran
	subroutine sort_a_data( a_data, a, work, index, iwork )
	    ! Sort `a_data` in terms or its component `a`
	    type(a_type), intent(inout)      :: a_data(:)
		integer(int32), intent(inout)    :: a(:)
		integer(int32), intent(inout)    :: work(:)
		integer(int_size), intent(inout) :: index(:)
		integer(int_size), intent(inout) :: iwork(:)
		! Extract a component of `a_data`
		a(1:size(a_data)) = a_data(:) % a
		! Find the indices to sort the component
		call ord_sorting(a(1:size(a_data)), index(1:size(a_data)),&
		    work(1:size(a_data)/2), iwork(1:size(a_data)/2))
		! Sort a_data based on the sorting of that component
		a_data(:) = a_data( index(1:size(a_data)) )
	end subroutine sort_a_data
```

#### `unord_sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input array with the elements sorted in order of increasing
value.

##### Syntax

`call [[stdlib_sorting(module):unord_sort(subroutine)]]unord_sort ( array )`

##### Class

Pure generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(real32)`, `real(real64)`, or `real(real128)`. It
is an `intent(inout)` argument. On return its input elements will be
sorted in order of non-decreasing value.

##### Notes

`UNORD_SORT` implements a hybrid sorting algorithm combining
`quicksort`, `merge sort`, and `insertion sort`. For most purposes it
behaves like a `quicksort` with a median of three partition, providing
good, `O(N Ln(N))`, run time performance for most random arrays, but
defaulting to `merge sort` if the structure of the array results in
the `quicksort` not converging as rapidly as expected. If `array` is of
any type `REAL`, the behavior of the sorting is undefined if any
element of `array` is a `NaN`.

##### Example

```Fortran
	...
	! Read random data from a file
	call read_file( 'dummy_file', array )
	! Sort the random data
	call unord_sort( array )
	! Process the sorted data
	call array_search( array, values )
	...
```

#### Specific procedures

Usually the name of a generic procedure is the most convenient way of
invoking it. However sometimes it is necessary to pass a procedure as
an argument to another procedure. In that case it is usually necessary
to know the name of the specific procedure desired. The following
table lists the specific subroutines and the corresponding types of
their `array` arguments.

| Generic Subroutine | Specific Subroutine | Array type      |
|--------------------|---------------------|-----------------|
| `ORD_SORT`         | `INT8_ORD_SORT`     | `INTEGER(INT8)` |
|                    | `INT16_ORD_SORT`    | `INTEGER(INT16)` |
|                    | `INT32_ORD_SORT`    | `INTEGER(INT32)` |
|                    | `INT64_ORD_SORT`    | `INTEGER(INT64)` |
|                    | `SP_ORD_SORT`    | `REAL(REAL32)` |
|                    | `DP_ORD_SORT`    | `REAL(REAL64)` |
|                    | `QP_ORD_SORT`    | `REAL(REAL128)` |
| `ORD_SORTING`      | `INT8_ORD_SORTING`     | `INTEGER(INT8)` |
|                    | `INT16_ORD_SORTING`    | `INTEGER(INT16)` |
|                    | `INT32_ORD_SORTING`    | `INTEGER(INT32)` |
|                    | `INT64_ORD_SORTING`    | `INTEGER(INT64)` |
|                    | `SP_ORD_SORTING`    | `REAL(REAL32)` |
|                    | `DP_ORD_SORTING`    | `REAL(REAL64)` |
|                    | `QP_ORD_SORTING`    | `REAL(REAL128)` |
| `UNORD_SORT`         | `INT8_UNORD_SORT`     | `INTEGER(INT8)` |
|                    | `INT16_UNORD_SORT`    | `INTEGER(INT16)` |
|                    | `INT32_UNORD_SORT`    | `INTEGER(INT32)` |
|                    | `INT64_UNORD_SORT`    | `INTEGER(INT64)` |
|                    | `SP_UNORD_SORT`    | `REAL(REAL32)` |
|                    | `DP_UNORD_SORT`    | `REAL(REAL64)` |
|                    | `QP_UNORD_SORT`    | `REAL(REAL128)` |


### Performance benchmarks

We have performed benchmarks of the procedures on nine different
arrays each of size `2**20`:

* Blocks - the array is divided into siz blocks, each of distinct
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

These benchmarks have been performed on two different compilers, both
on a MacBook Pro, featuring a 2.3 GHz Quad-Core Intel Core i5, with 8
GB 2133 MHz LPDDR3 memory. The first compiler was GNU Fortran
(GCC) 10.2.0, with the following results:

|   Type  | Elements |   Order       |    Method     | Time (s)  |
|---------|----------|---------------|---------------|-----------|
| Integer | 1048576  |        Blocks |      Ord_Sort |   0.00692 |
| Integer | 1048576  |    Decreasing |      Ord_Sort |   0.00377 |
| Integer | 1048576  |     Identical |      Ord_Sort |   0.00236 |
| Integer | 1048576  |    Increasing |      Ord_Sort |   0.00205 |
| Integer | 1048576  |  Random dense |      Ord_Sort |   0.16675 |
| Integer | 1048576  |  Random order |      Ord_Sort |   0.16780 |
| Integer | 1048576  | Random sparse |      Ord_Sort |   0.16715 |
| Integer | 1048576  |      Random 3 |      Ord_Sort |   0.00907 |
| Integer | 1048576  |     Random 10 |      Ord_Sort |   0.00435 |
| Integer | 1048576  |        Blocks |   Ord_Sorting |   0.01177 |
| Integer | 1048576  |    Decreasing |   Ord_Sorting |   0.00695 |
| Integer | 1048576  |     Identical |   Ord_Sorting |   0.00438 |
| Integer | 1048576  |    Increasing |   Ord_Sorting |   0.00429 |
| Integer | 1048576  |  Random dense |   Ord_Sorting |   0.19079 |
| Integer | 1048576  |  Random order |   Ord_Sorting |   0.19242 |
| Integer | 1048576  | Random sparse |   Ord_Sorting |   0.19212 |
| Integer | 1048576  |      Random 3 |   Ord_Sorting |   0.01504 |
| Integer | 1048576  |     Random 10 |   Ord_Sorting |   0.00794 |
| Integer | 1048576  |        Blocks |    Unord_Sort |   0.20889 |
| Integer | 1048576  |    Decreasing |    Unord_Sort |   0.12826 |
| Integer | 1048576  |     Identical |    Unord_Sort |   0.15646 |
| Integer | 1048576  |    Increasing |    Unord_Sort |   0.05012 |
| Integer | 1048576  |  Random dense |    Unord_Sort |   0.14359 |
| Integer | 1048576  |  Random order |    Unord_Sort |   0.14613 |
| Integer | 1048576  | Random sparse |    Unord_Sort |   0.15159 |
| Integer | 1048576  |      Random 3 |    Unord_Sort |   0.13826 |
| Integer | 1048576  |     Random 10 |    Unord_Sort |   0.35356 |

The second compiler was ifort (IFORT) 18.0.3 20180410, with the
following results:

|   Type  | Elements |   Order       |    Method     | Time (s)  |
|---------|----------|---------------|---------------|-----------|
| Integer | 1048576  |        Blocks |      Ord_Sort |   0.00231 |
| Integer | 1048576  |    Decreasing |      Ord_Sort |   0.00123 |
| Integer | 1048576  |     Identical |      Ord_Sort |   0.00088 |
| Integer | 1048576  |    Increasing |      Ord_Sort |   0.00088 |
| Integer | 1048576  |  Random dense |      Ord_Sort |   0.08914 |
| Integer | 1048576  |  Random order |      Ord_Sort |   0.08779 |
| Integer | 1048576  | Random sparse |      Ord_Sort |   0.08357 |
| Integer | 1048576  |      Random 3 |      Ord_Sort |   0.00335 |
| Integer | 1048576  |     Random 10 |      Ord_Sort |   0.00196 |
| Integer | 1048576  |        Blocks |   Ord_Sorting |   0.00519 |
| Integer | 1048576  |    Decreasing |   Ord_Sorting |   0.00240 |
| Integer | 1048576  |     Identical |   Ord_Sorting |   0.00110 |
| Integer | 1048576  |    Increasing |   Ord_Sorting |   0.00113 |
| Integer | 1048576  |  Random dense |   Ord_Sorting |   0.10185 |
| Integer | 1048576  |  Random order |   Ord_Sorting |   0.10222 |
| Integer | 1048576  | Random sparse |   Ord_Sorting |   0.10376 |
| Integer | 1048576  |      Random 3 |   Ord_Sorting |   0.00628 |
| Integer | 1048576  |     Random 10 |   Ord_Sorting |   0.00264 |
| Integer | 1048576  |        Blocks |    Unord_Sort |   0.07272 |
| Integer | 1048576  |    Decreasing |    Unord_Sort |   0.03873 |
| Integer | 1048576  |     Identical |    Unord_Sort |   0.03677 |
| Integer | 1048576  |    Increasing |    Unord_Sort |   0.01153 |
| Integer | 1048576  |  Random dense |    Unord_Sort |   0.06706 |
| Integer | 1048576  |  Random order |    Unord_Sort |   0.06808 |
| Integer | 1048576  | Random sparse |    Unord_Sort |   0.06798 |
| Integer | 1048576  |      Random 3 |    Unord_Sort |   0.07230 |
| Integer | 1048576  |     Random 10 |    Unord_Sort |   0.13640 |


