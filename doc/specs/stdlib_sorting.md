---
title: sorting
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

The module `stdlib_sorting` defines several public entities, two
default integer parameters, `int_index` and `int_index_low`, and four overloaded
subroutines: `ORD_SORT`, `SORT`, `RADIX_SORT` and `SORT_INDEX`. The
overloaded subroutines also each have several specific names for
versions corresponding to different types of array arguments.

### The parameters `int_index` and `int_index_low`

The parameters `int_index` and `int_index_low` are used to specify the kind of integer used
in indexing the various arrays. Currently the module sets `int_index` and
`int_index_low`
to the value of `int64` and `int32` from the `stdlib_kinds` module, respectively.

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
  same way;
* `SORT` is intended to sort simple arrays of intrinsic data
  that are effectively unordered before the sort;
* `RADIX_SORT` is intended to sort fixed width intrinsic data 
  types (integers and reals).

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

#### The `RADIX_SORT` subroutine

`RADIX_SORT` is a implementation of LSD [radix sort](https://www.growingwiththeweb.com/sorting/radix-sort-lsd/),
using `256` as the radix. It only works for fixed width data,
thus integers and reals. `RADIX_SORT` is always of O(N) runtime performance
for any input data. For large and random data, it is about five (or more)
times faster than other sort subroutines.

The `RADIX_SORT` needs a buffer that have same size of the input data.
Your can provide it using `work` argument, if not the subroutine will
allocate the buffer and deallocate before return.

### Specifications of the `stdlib_sorting` procedures

#### `ord_sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input `array` with the elements sorted in order of
increasing, or decreasing, value.

##### Syntax

`call ` [[stdlib_sorting(module):ord_sort(interface)]] `( array[, work, reverse ] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`, `character(*)`, `type(string_type)`,
`type(bitset_64)`, or `type(bitset_large)`.
It is an `intent(inout)` argument. On input it is
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
{!example/sorting/example_ord_sort.f90!}
```

#### `sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input array with the elements sorted in order of increasing, or
decreasing, value.

##### Syntax

`call ` [[stdlib_sorting(module):sort(interface)]] `( array[, reverse] )`

##### Class

Pure generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. `character(*)`, `type(string_type)`,
`type(bitset_64)`, or `type(bitset_large)`.
It is an `intent(inout)` argument. On return its
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
{!example/sorting/example_sort.f90!}
```

#### `radix_sort` - sorts an input array

##### Status

Experimental

##### Description

Returns an input array with the elements sorted in order of increasing, or
decreasing, value.

##### Syntax

`call ` [[stdlib_sorting(module):radix_sort(interface)]] `( array[, work, reverse] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`. It is an `intent(inout)` argument. On return its
input elements will be sorted in order of non-decreasing value.

`work` (optional): shall be a rank one array of the same type as
array, and shall have at least `size(array)` elements. It is an
`intent(inout)` argument, and its contents on return are undefined.

`reverse` (optional): shall be a scalar of type default `logical`. It
is an `intent(in)` argument. If present with a value of `.true.` then
`array` will be sorted in order of non-increasing values in unstable
order. Otherwise index will sort `array` in order of non-decreasing
values in unstable order.

##### Notes

`radix_sort` implements a LSD radix sort algorithm with a `256` radix. For any
input data it provides `O(N)` run time performance. If `array` is of
any type `real` the order of its elements on return undefined if any
element of `array` is a `NaN`.

##### Example

```fortran
{!example/sorting/example_radix_sort.f90!}
```

#### `sort_index` - creates an array of sorting indices for an input array, while also sorting the array.

##### Status

Experimental

##### Description

Returns the input `array` sorted in the direction requested while
retaining order stability, and an integer array whose elements would
sort the input `array` to produce the output `array`.

##### Syntax

`call ` [[stdlib_sorting(module):sort_index(interface)]] `( array, index[, work, iwork, reverse ] )`

##### Class

Generic subroutine.

##### Arguments

`array`: shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`, `character(*)`, `type(string_type)`,
`type(bitset_64)`, or `type(bitset_large)`.
It is an `intent(inout)` argument. On input it
will be an array whose sorting indices are to be determined. On return
it will be the sorted array.

`index`: shall be a rank one integer array of kind `int_index` or `int_index_low` and of
the size of `array`. It is an `intent(out)` argument. On return it
shall have values that are the indices needed to sort the original
array in the desired direction.

`work` (optional): shall be a rank one array of any of the same type as
`array`, and shall have at least `size(array)/2` elements. It is an
`intent(out)` argument. It is intended to be used as "scratch"
memory for internal record keeping. If associated with an array in
static storage, its use can significantly reduce the stack memory
requirements for the code. Its contents on return are undefined.

`iwork` (optional): shall be a rank one integer array of the same kind
of the array `index`, and shall have at least `size(array)/2` elements. It
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

Sorting a rank one array with `sort_index`:

```Fortran
{!example/sorting/example_sort_index.f90!}
```

Sorting a related rank one array:

```Fortran
    subroutine sort_related_data( a, b, work, index, iwork )
        ! Sort `a`, and  also  sort `b` to be reorderd the same way as `a`
        integer, intent(inout)         :: a(:)
        integer(int32), intent(inout)  :: b(:) ! The same size as a
        integer(int32), intent(out)    :: work(:)
        integer(int_index), intent(out) :: index(:)
        integer(int_index), intent(out) :: iwork(:)
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
        integer(int_index), intent(out) :: index(:)
        integer(int_index), intent(out) :: iwork(:)
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
        ! Sort `a_data` in terms of its component `a`
        type(a_type), intent(inout)      :: a_data(:)
        integer(int32), intent(inout)    :: a(:)
        integer(int32), intent(out)    :: work(:)
        integer(int_index), intent(out) :: index(:)
        integer(int_index), intent(out) :: iwork(:)
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
integer arrays each of size `2**16`:

* Blocks - the array is divided into six blocks, each of distinct
  uniformly increasing integers.
* Decreasing - values decrease uniformly from `2**16-1` to `0`.
* Identical - all integers have the same value of 10.
* Increasing - values increase uniformly from `0` to `2**16-1`.
* Random dense - the integers are generated randomly from a set of
  values from `0` to `2**14-1` so duplicates are dense.
* Random order - a set of integers from `0` to `2**16 - 1` in random
  order.
* Random sparse - the integers are generated randomly from a set of
  values from `0` to `2**18-1` so duplicates are sparse.
* Random-3 - the increasing array has 3 random exchanges of individual
  elements.
* Random-10 - the final ten elements of the increasing array are
  replaced by random values.

On three different default character arrays, each of length 4 and of
size `20**4, with characters drawn from the set "a"-"p":

* Char. Decreasing - values decrease uniformly from `"pppp"` to
  `"aaaa"`.
* Char. Increasing - values decrease uniformly from `"aaaa"` to
  `"pppp"`.
* Char. Random - the set of strings from `"aaaa"` to `"pppp"` in
  random order.

On three different `string_type` arrays, each of length 4 elements and
of size `16**3`, with characters drawn from the set "a"-"p":

* String Decreasing - values decrease uniformly from `"ppp"` to
  `"aaa"`.
* String Increasing - values decrease uniformly from `"aaa"` to
  `"ppp"`.
* String Random - the set of strings from `"aaa"` to `"ppp"` in
  random order.

These benchmarks have been performed on two different compilers, both
on WSL with Ubuntu-20.04, Intel(R) Core(TM) i7-10700 CPU @ 2.9GHz, with
32 GB DDR4 memory. The first compiler is GNU Fortran (GCC) 9.4.0, with
the following results.

|    Type     | Elements |    Array Name   |    Method   |  Time (s) |
|-------------|----------|-----------------|-------------|-----------|
|     Integer |   65536  |          Blocks |    Ord_Sort |  0.001048 |
|     Integer |   65536  |      Decreasing |    Ord_Sort |  0.000204 |
|     Integer |   65536  |       Identical |    Ord_Sort |  0.000097 |
|     Integer |   65536  |      Increasing |    Ord_Sort |  0.000096 |
|     Integer |   65536  |    Random dense |    Ord_Sort |  0.006580 |
|     Integer |   65536  |    Random order |    Ord_Sort |  0.006886 |
|     Integer |   65536  |   Random sparse |    Ord_Sort |  0.006821 |
|     Integer |   65536  |        Random 3 |    Ord_Sort |  0.000461 |
|     Integer |   65536  |       Random 10 |    Ord_Sort |  0.000226 |
|   Character |   65536  |  Char. Decrease |    Ord_Sort |  0.000824 |
|   Character |   65536  |  Char. Increase |    Ord_Sort |  0.000370 |
|   Character |   65536  |    Char. Random |    Ord_Sort |  0.016020 |
| String_type |    4096  | String Decrease |    Ord_Sort |  0.000465 |
| String_type |    4096  | String Increase |    Ord_Sort |  0.000169 |
| String_type |    4096  |   String Random |    Ord_Sort |  0.004194 |
|     Integer |   65536  |          Blocks |  Radix_Sort |  0.001610 |
|     Integer |   65536  |      Decreasing |  Radix_Sort |  0.001076 |
|     Integer |   65536  |       Identical |  Radix_Sort |  0.001074 |
|     Integer |   65536  |      Increasing |  Radix_Sort |  0.001060 |
|     Integer |   65536  |    Random dense |  Radix_Sort |  0.001161 |
|     Integer |   65536  |    Random order |  Radix_Sort |  0.001069 |
|     Integer |   65536  |   Random sparse |  Radix_Sort |  0.001005 |
|     Integer |   65536  |        Random 3 |  Radix_Sort |  0.001057 |
|     Integer |   65536  |       Random 10 |  Radix_Sort |  0.001046 |
|     Integer |   65536  |     rand-real32 |  Radix_Sort |  0.001429 |
|     Integer |   65536  |          Blocks |        Sort |  0.004269 |
|     Integer |   65536  |      Decreasing |        Sort |  0.005108 |
|     Integer |   65536  |       Identical |        Sort |  0.006257 |
|     Integer |   65536  |      Increasing |        Sort |  0.002093 |
|     Integer |   65536  |    Random dense |        Sort |  0.006032 |
|     Integer |   65536  |    Random order |        Sort |  0.006030 |
|     Integer |   65536  |   Random sparse |        Sort |  0.006126 |
|     Integer |   65536  |        Random 3 |        Sort |  0.007930 |
|     Integer |   65536  |       Random 10 |        Sort |  0.014729 |
|   Character |   65536  |  Char. Decrease |        Sort |  0.020623 |
|   Character |   65536  |  Char. Increase |        Sort |  0.008028 |
|   Character |   65536  |    Char. Random |        Sort |  0.014258 |
| String_type |    4096  | String Decrease |        Sort |  0.005542 |
| String_type |    4096  | String Increase |        Sort |  0.001987 |
| String_type |    4096  |   String Random |        Sort |  0.003267 |
|     Integer |   65536  |          Blocks |  Sort_Index |  0.000686 |
|     Integer |   65536  |      Decreasing |  Sort_Index |  0.000529 |
|     Integer |   65536  |       Identical |  Sort_Index |  0.000218 |
|     Integer |   65536  |      Increasing |  Sort_Index |  0.000214 |
|     Integer |   65536  |    Random dense |  Sort_Index |  0.008044 |
|     Integer |   65536  |    Random order |  Sort_Index |  0.008042 |
|     Integer |   65536  |   Random sparse |  Sort_Index |  0.008148 |
|     Integer |   65536  |        Random 3 |  Sort_Index |  0.000677 |
|     Integer |   65536  |       Random 10 |  Sort_Index |  0.000387 |
|   Character |   65536  |  Char. Decrease |  Sort_Index |  0.000932 |
|   Character |   65536  |  Char. Increase |  Sort_Index |  0.000487 |
|   Character |   65536  |    Char. Random |  Sort_Index |  0.017231 |
| String_type |    4096  | String Decrease |  Sort_Index |  0.000489 |
| String_type |    4096  | String Increase |  Sort_Index |  0.000183 |
| String_type |    4096  |   String Random |  Sort_Index |  0.004102 |

The second compiler is Intel(R) Fortran Intel(R) 64 Compiler Classic
for applications running on Intel(R) 64, Version 2021.7.0 Build
20220726_000000, with the following results:

|    Type     | Elements |    Array Name   |    Method   |  Time (s) |
|-------------|----------|-----------------|-------------|-----------|
|     Integer |   65536  |          Blocks |    Ord_Sort |  0.000135 |
|     Integer |   65536  |      Decreasing |    Ord_Sort |  0.000053 |
|     Integer |   65536  |       Identical |    Ord_Sort |  0.000033 |
|     Integer |   65536  |      Increasing |    Ord_Sort |  0.000034 |
|     Integer |   65536  |    Random dense |    Ord_Sort |  0.003291 |
|     Integer |   65536  |    Random order |    Ord_Sort |  0.003546 |
|     Integer |   65536  |   Random sparse |    Ord_Sort |  0.003313 |
|     Integer |   65536  |        Random 3 |    Ord_Sort |  0.000145 |
|     Integer |   65536  |       Random 10 |    Ord_Sort |  0.000070 |
|   Character |   65536  |  Char. Decrease |    Ord_Sort |  0.000696 |
|   Character |   65536  |  Char. Increase |    Ord_Sort |  0.000338 |
|   Character |   65536  |    Char. Random |    Ord_Sort |  0.015255 |
| String_type |    4096  | String Decrease |    Ord_Sort |  0.001276 |
| String_type |    4096  | String Increase |    Ord_Sort |  0.000153 |
| String_type |    4096  |   String Random |    Ord_Sort |  0.024705 |
|     Integer |   65536  |          Blocks |  Radix_Sort |  0.001038 |
|     Integer |   65536  |      Decreasing |  Radix_Sort |  0.000910 |
|     Integer |   65536  |       Identical |  Radix_Sort |  0.000441 |
|     Integer |   65536  |      Increasing |  Radix_Sort |  0.000803 |
|     Integer |   65536  |    Random dense |  Radix_Sort |  0.000363 |
|     Integer |   65536  |    Random order |  Radix_Sort |  0.000741 |
|     Integer |   65536  |   Random sparse |  Radix_Sort |  0.000384 |
|     Integer |   65536  |        Random 3 |  Radix_Sort |  0.000877 |
|     Integer |   65536  |       Random 10 |  Radix_Sort |  0.000801 |
|     Integer |   65536  |     rand-real32 |  Radix_Sort |  0.000604 |
|     Integer |   65536  |          Blocks |        Sort |  0.001342 |
|     Integer |   65536  |      Decreasing |        Sort |  0.001391 |
|     Integer |   65536  |       Identical |        Sort |  0.001485 |
|     Integer |   65536  |      Increasing |        Sort |  0.000447 |
|     Integer |   65536  |    Random dense |        Sort |  0.002778 |
|     Integer |   65536  |    Random order |        Sort |  0.002896 |
|     Integer |   65536  |   Random sparse |        Sort |  0.003136 |
|     Integer |   65536  |        Random 3 |        Sort |  0.002996 |
|     Integer |   65536  |       Random 10 |        Sort |  0.005752 |
|   Character |   65536  |  Char. Decrease |        Sort |  0.021973 |
|   Character |   65536  |  Char. Increase |        Sort |  0.008391 |
|   Character |   65536  |    Char. Random |        Sort |  0.015155 |
| String_type |    4096  | String Decrease |        Sort |  0.034014 |
| String_type |    4096  | String Increase |        Sort |  0.010464 |
| String_type |    4096  |   String Random |        Sort |  0.015748 |
|     Integer |   65536  |          Blocks |  Sort_Index |  0.000381 |
|     Integer |   65536  |      Decreasing |  Sort_Index |  0.000085 |
|     Integer |   65536  |       Identical |  Sort_Index |  0.000046 |
|     Integer |   65536  |      Increasing |  Sort_Index |  0.000046 |
|     Integer |   65536  |    Random dense |  Sort_Index |  0.004020 |
|     Integer |   65536  |    Random order |  Sort_Index |  0.004059 |
|     Integer |   65536  |   Random sparse |  Sort_Index |  0.004073 |
|     Integer |   65536  |        Random 3 |  Sort_Index |  0.000215 |
|     Integer |   65536  |       Random 10 |  Sort_Index |  0.000101 |
|   Character |   65536  |  Char. Decrease |  Sort_Index |  0.000680 |
|   Character |   65536  |  Char. Increase |  Sort_Index |  0.000356 |
|   Character |   65536  |    Char. Random |  Sort_Index |  0.016231 |
| String_type |    4096  | String Decrease |  Sort_Index |  0.001219 |
| String_type |    4096  | String Increase |  Sort_Index |  0.000125 |
| String_type |    4096  |   String Random |  Sort_Index |  0.018631 |
