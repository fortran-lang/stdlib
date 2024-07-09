



!! Licensing:
!!
!! This file is subject both to the Fortran Standard Library license, and
!! to additional licensing requirements as it contains translations of
!! other software.
!!
!! The Fortran Standard Library, including this file, is distributed under
!! the MIT license that should be included with the library's distribution.
!!
!!   Copyright (c) 2021 Fortran stdlib developers
!!
!!   Permission is hereby granted, free of charge, to any person obtaining a
!!   copy of this software and associated documentation files (the
!!   "Software"),  to deal in the Software without restriction, including
!!   without limitation the rights to use, copy, modify, merge, publish,
!!   distribute, sublicense, and/or sellcopies of the Software, and to permit
!!   persons to whom the Software is furnished to do so, subject to the
!!   following conditions:
!!
!!   The above copyright notice and this permission notice shall be included
!!   in all copies or substantial portions of the Software.
!!
!!   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
!!   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!!   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
!!   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
!!   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
!!   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
!!   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!!
!! Two of the generic subroutines, `ORD_SORT` and `SORT_INDEX`, are
!! substantially translations to Fortran 2008 of the `"Rust" sort` sorting
!! routines in
!! [`slice.rs`](https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs)
!! The `rust sort` implementation is distributed with the header:
!!
!!   Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
!!   file at the top-level directory of this distribution and at
!!   http://rust-lang.org/COPYRIGHT.
!!
!!   Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
!!   http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
!!   <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
!!   option. This file may not be copied, modified, or distributed
!!   except according to those terms.
!!
!! so the license for the original`slice.rs` code is compatible with the use
!! of modified versions of the code in the Fortran Standard Library under
!! the MIT license.
!!
!! One of the generic subroutines, `SORT`, is substantially a
!! translation to Fortran 2008, of the `introsort` of David Musser.
!! David Musser has given permission to include a variant of `introsort`
!! in the Fortran Standard Library under the MIT license provided
!! we cite:
!!
!!   Musser, D.R., “Introspective Sorting and Selection Algorithms,”
!!   Software—Practice and Experience, Vol. 27(8), 983–993 (August 1997).
!!
!! as the official source of the algorithm.

module stdlib_sorting
!! This module implements overloaded sorting subroutines named `ORD_SORT`,
!! `SORT_INDEX`, and `SORT`, that each can be used to sort four kinds
!! of `INTEGER` arrays, three kinds of `REAL` arrays, `character(len=*)` arrays,
!! and arrays of `type(string_type)`.
!! ([Specification](../page/specs/stdlib_sorting.html))
!!
!! By default sorting is in order of 
!! increasing value, but there is an option to sort in decreasing order. 
!! All the subroutines have worst case run time performance of `O(N Ln(N))`, 
!! but on largely sorted data `ORD_SORT` and `SORT_INDEX` can have a run time 
!! performance of `O(N)`.
!!
!! `ORD_SORT` is a translation of the `"Rust" sort` sorting algorithm in
!! `slice.rs`:
!! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs
!! which in turn is inspired by the `timsort` algorithm of Tim Peters,
!! http://svn.python.org/projects/python/trunk/Objects/listsort.txt.
!! `ORD_SORT` is a hybrid stable comparison algorithm combining `merge sort`,
!! and `insertion sort`. It is always at worst O(N Ln(N)) in sorting random
!! data, having a performance about 25% slower than `SORT` on such
!! data, but has much better performance than `SORT` on partially
!! sorted data, having O(N) performance on uniformly non-increasing or
!! non-decreasing data.
!!
!! `SORT_INDEX` is a modification of `ORD_SORT` so that in addition to
!! sorting the input array, it returns the indices that map to a
!! stable sort of the original array. These indices are
!! intended to be used to sort data that is correlated with the input
!! array, e.g., different arrays in a database, different columns of a
!! rank 2 array, different elements of a derived type. It is less
!! efficient than `ORD_SORT` at sorting a simple array.
!!
!! `SORT` uses the `INTROSORT` sorting algorithm of David Musser,
!! http://www.cs.rpi.edu/~musser/gp/introsort.ps. `introsort` is a hybrid
!! unstable comparison algorithm combining `quicksort`, `insertion sort`, and
!! `heap sort`. While this algorithm is always O(N Ln(N)) it is relatively
!! fast on randomly ordered data, but inconsistent in performance on partly
!! sorted data, sometimes having `merge sort` performance, sometimes having
!! better than `quicksort` performance. `UNORD_SOORT` is about 25%
!! more efficient than `ORD_SORT` at sorting purely random data, but af an
!! order of `Ln(N)` less efficient at sorting partially sorted data.

    use stdlib_kinds, only: &
        int8,               &
        int16,              &
        int32,              &
        int64,              &
        sp,                 &
        dp,                 &
        xdp,                &
        qp

    use stdlib_optval, only: optval

    use stdlib_string_type, only: string_type, assignment(=), operator(>), &
        operator(>=), operator(<), operator(<=)

    use stdlib_bitsets, only: bitset_64, bitset_large, &
        assignment(=), operator(>), operator(>=), operator(<), operator(<=)

    implicit none
    private

    integer, parameter, public :: int_index = int64 !! Integer kind for indexing
    integer, parameter, public :: int_index_low = int32 !! Integer kind for indexing using less than `huge(1_int32)` values


! Constants for use by tim_sort
    integer, parameter :: &
! The maximum number of entries in a run stack, good for an array of
! 2**64 elements see
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
        max_merge_stack = int( ceiling( log( 2._dp**64 ) / &
                               log(1.6180339887_dp) ) )

    type run_type
!! Version: experimental
!!
!! Used to pass state around in a stack among helper functions for the
!! `ORD_SORT` and `SORT_INDEX` algorithms
        integer(int_index) :: base = 0
        integer(int_index) :: len = 0
    end type run_type

    public ord_sort
!! Version: experimental
!!
!! The generic subroutine implementing the `ORD_SORT` algorithm to return
!! an input array with its elements sorted in order of (non-)decreasing
!! value. Its use has the syntax:
!!
!!     call ord_sort( array[, work, reverse] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
!!   `real(real128)`, `character(*)`, `type(string_type)`, 
!!   `type(bitset_64)`, `type(bitset_large)`. If both the 
!!   type of `array` is real and at least one of the elements is a 
!!   `NaN`, then the ordering of the result is undefined. Otherwise it 
!!   is defined to be the original elements in non-decreasing order.
!!
!! * work (optional): shall be a rank 1 array of the same type as
!!   `array`, and shall have at least `size(array)/2` elements. It is an
!!   `intent(out)` argument to be used as "scratch" memory
!!   for internal record keeping. If associated with an array in static
!!   storage, its use can significantly reduce the stack memory requirements
!!   for the code. Its value on return is undefined.
!!
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `array` will be sorted in order of non-increasing values in stable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in stable order.
!!
!!#### Example
!!
!!```fortran
!!    ...
!!    ! Read arrays from sorted files
!!    call read_sorted_file( 'dummy_file1', array1 )
!!    call read_sorted_file( 'dummy_file2', array2 )
!!    ! Concatenate the arrays
!!    allocate( array( size(array1) + size(array2) ) )
!!    array( 1:size(array1) ) = array1(:)
!!    array( size(array1)+1:size(array1)+size(array2) ) = array2(:)
!!    ! Sort the resulting array
!!    call ord_sort( array, work )
!!    ! Process the sorted array
!!    call array_search( array, values )
!!    ...
!!```

    public sort
!! Version: experimental
!!
!! The generic subroutine implementing the `SORT` algorithm to return
!! an input array with its elements sorted in order of (non-)decreasing
!! value. Its use has the syntax:
!!
!!     call sort( array[, reverse] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
!!   `real(real128)`, `character(*)`, `type(string_type)`, 
!!   `type(bitset_64)`, `type(bitset_large)`. If both the type
!!   of `array` is real and at least one of the elements is a `NaN`, then
!!   the ordering of the result is undefined. Otherwise it is defined to be the
!!   original elements in non-decreasing order.
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `array` will be sorted in order of non-increasing values in unstable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in unstable order.
!!
!!#### Example
!!
!!```fortran
!!    ...
!!    ! Read random data from a file
!!    call read_file( 'dummy_file', array )
!!    ! Sort the random data
!!    call sort( array )
!!    ! Process the sorted data
!!    call array_search( array, values )
!!    ...
!!```

    public radix_sort
!! Version: experimental
!!
!! The generic subroutine implementing the LSD radix sort algorithm to return
!! an input array with its elements sorted in order of (non-)decreasing
!! value. Its use has the syntax:
!!
!!     call radix_sort( array[, work, reverse] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`.
!!   If both the type of `array` is real and at least one of the
!!   elements is a `NaN`, then the ordering of the result is undefined.
!!   Otherwise it is defined to be the original elements in
!!   non-decreasing order. Especially, -0.0 is lesser than 0.0.
!!
!! * work (optional): shall be a rank 1 array of the same type as
!!   `array`, and shall have at least `size(array)` elements. It is an
!!   `intent(inout)` argument to be used as buffer. Its value on return is
!!   undefined. If it is not present, `radix_sort` will allocate a
!!   buffer for use, and deallocate it before return. If you do several
!!   similar `radix_sort`s, reusing the `work` array is a good parctice.
!!   This argument is not present for `int8_radix_sort` because it use
!!   counting sort, so no buffer is needed.
!!
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `array` will be sorted in order of non-increasing values in stable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in stable order.
!!
!!#### Example
!!
!!```fortran
!!    ...
!!    ! Read random data from a file
!!    call read_file( 'dummy_file', array )
!!    ! Sort the random data
!!    call radix_sort( array )
!!    ...
!!```

    public sort_index
!! Version: experimental
!!
!! The generic subroutine implementing the `SORT_INDEX` algorithm to
!! return an index array whose elements would sort the input array in the
!! desired direction. It is primarily intended to be used to sort a
!! derived type array based on the values of a component of the array.
!! Its use has the syntax:
!!
!!     call sort_index( array, index[, work, iwork, reverse ] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
!!   `real(real128)`, `character(*)`, `type(string_type)`, 
!!   `type(bitset_64)`, `type(bitset_large)`. If both the 
!!   type of `array` is real and at least one of the elements is a `NaN`, 
!!   then the ordering of the `array` and `index` results is undefined. 
!!   Otherwise it is defined to be as specified by reverse.
!!
!! * index: a rank 1 array of sorting indices. It is an `intent(out)`
!!   argument of the type `integer(int_index)`. Its size shall be the
!!   same as `array`. On return, if defined, its elements would
!!   sort the input `array` in the direction specified by `reverse`.
!!
!! * work (optional): shall be a rank 1 array of the same type as
!!   `array`, and shall have at least `size(array)/2` elements. It is an
!!   `intent(out)` argument to be used as "scratch" memory
!!   for internal record keeping. If associated with an array in static
!!   storage, its use can significantly reduce the stack memory requirements
!!   for the code. Its value on return is undefined.
!!
!! * iwork (optional): shall be a rank 1 integer array of kind `int_index`,
!!   and shall have at least `size(array)/2` elements. It is an
!!   `intent(out)` argument to be used as "scratch" memory
!!   for internal record keeping. If associated with an array in static
!!   storage, its use can significantly reduce the stack memory requirements
!!   for the code. Its value on return is undefined.
!!
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `index` will sort `array` in order of non-increasing values in stable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in stable order.
!!
!!#### Examples
!!
!! Sorting a related rank one array:
!!
!!```Fortran
!!    subroutine sort_related_data( a, b, work, index, iwork )
!!        ! Sort `b` in terms or its related array `a`
!!        integer, intent(inout)         :: a(:)
!!        integer(int32), intent(inout)  :: b(:) ! The same size as a
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!    ! Find the indices to sort a
!!        call sort_index(a, index(1:size(a)),&
!!            work(1:size(a)/2), iwork(1:size(a)/2))
!!    ! Sort b based on the sorting of a
!!        b(:) = b( index(1:size(a)) )
!!    end subroutine sort_related_data
!!```
!!
!! Sorting a rank 2 array based on the data in a column
!!
!!```Fortran
!!    subroutine sort_related_data( array, column, work, index, iwork )
!!    ! Sort `a_data` in terms or its component `a`
!!        integer, intent(inout)         :: a(:,:)
!!        integer(int32), intent(in)     :: column
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!        integer, allocatable           :: dummy(:)
!!        integer :: i
!!        allocate(dummy(size(a, dim=1)))
!!    ! Extract a component of `a_data`
!!        dummy(:) = a(:, column)
!!    ! Find the indices to sort the column
!!        call sort_index(dummy, index(1:size(dummy)),&
!!                        work(1:size(dummy)/2), iwork(1:size(dummy)/2))
!!    ! Sort a based on the sorting of its column
!!        do i=1, size(a, dim=2)
!!            a(:, i) = a(index(1:size(a, dim=1)), i)
!!        end do
!!    end subroutine sort_related_data
!!```
!!
!! Sorting an array of a derived type based on the dsta in one component
!!```fortran
!!    subroutine sort_a_data( a_data, a, work, index, iwork )
!!    ! Sort `a_data` in terms or its component `a`
!!        type(a_type), intent(inout)    :: a_data(:)
!!        integer(int32), intent(inout)  :: a(:)
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!    ! Extract a component of `a_data`
!!        a(1:size(a_data)) = a_data(:) % a
!!    ! Find the indices to sort the component
!!        call sort_index(a(1:size(a_data)), index(1:size(a_data)),&
!!                        work(1:size(a_data)/2), iwork(1:size(a_data)/2))
!!    ! Sort a_data based on the sorting of that component
!!        a_data(:) = a_data( index(1:size(a_data)) )
!!    end subroutine sort_a_data
!!```

    interface ord_sort
!! Version: experimental
!!
!! The generic subroutine interface implementing the `ORD_SORT` algorithm,
!! a translation to Fortran 2008, of the `"Rust" sort` algorithm found in
!! `slice.rs`
!! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
!! `ORD_SORT` is a hybrid stable comparison algorithm combining `merge sort`,
!! and `insertion sort`.
!! ([Specification](../page/specs/stdlib_sorting.html#ord_sort-sorts-an-input-array))
!!
!! It is always at worst O(N Ln(N)) in sorting random
!! data, having a performance about 25% slower than `SORT` on such
!! data, but has much better performance than `SORT` on partially
!! sorted data, having O(N) performance on uniformly non-increasing or
!! non-decreasing data.

        module subroutine int8_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `int8_ord_sort( array )` sorts the input `ARRAY` of type `integer(int8)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            integer(int8), intent(inout)         :: array(0:)
            integer(int8), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine int8_ord_sort

        module subroutine int16_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `int16_ord_sort( array )` sorts the input `ARRAY` of type `integer(int16)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            integer(int16), intent(inout)         :: array(0:)
            integer(int16), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine int16_ord_sort

        module subroutine int32_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `int32_ord_sort( array )` sorts the input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            integer(int32), intent(inout)         :: array(0:)
            integer(int32), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine int32_ord_sort

        module subroutine int64_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `int64_ord_sort( array )` sorts the input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            integer(int64), intent(inout)         :: array(0:)
            integer(int64), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine int64_ord_sort

        module subroutine sp_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `sp_ord_sort( array )` sorts the input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            real(sp), intent(inout)         :: array(0:)
            real(sp), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine sp_ord_sort

        module subroutine dp_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `dp_ord_sort( array )` sorts the input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            real(dp), intent(inout)         :: array(0:)
            real(dp), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine dp_ord_sort

        module subroutine string_type_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `string_type_ord_sort( array )` sorts the input `ARRAY` of type `type(string_type)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            type(string_type), intent(inout)         :: array(0:)
            type(string_type), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine string_type_ord_sort

        module subroutine char_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `char_ord_sort( array )` sorts the input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            character(len=*), intent(inout)         :: array(0:)
            character(len=len(array)), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine char_ord_sort

        module subroutine bitset_64_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `bitset_64_ord_sort( array )` sorts the input `ARRAY` of type `type(bitset_64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            type(bitset_64), intent(inout)         :: array(0:)
            type(bitset_64), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine bitset_64_ord_sort

        module subroutine bitset_large_ord_sort( array, work, reverse )
!! Version: experimental
!!
!! `bitset_large_ord_sort( array )` sorts the input `ARRAY` of type `type(bitset_large)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
            type(bitset_large), intent(inout)         :: array(0:)
            type(bitset_large), intent(out), optional :: work(0:)
            logical, intent(in), optional :: reverse
        end subroutine bitset_large_ord_sort


    end interface ord_sort
    interface radix_sort
!! Version: experimental
!!
!! The generic subroutine interface implementing the LSD radix sort algorithm,
!! see https://en.wikipedia.org/wiki/Radix_sort for more details.
!! It is always O(N) in sorting random data, but need a O(N) buffer.
!! ([Specification](../page/specs/stdlib_sorting.html#radix_sort-sorts-an-input-array))
!!

        pure module subroutine int8_radix_sort(array, reverse)
            integer(kind=int8), dimension(:), intent(inout) :: array
            logical, intent(in), optional :: reverse
        end subroutine int8_radix_sort

        pure module subroutine int16_radix_sort(array, work, reverse)
            integer(kind=int16), dimension(:), intent(inout) :: array
            integer(kind=int16), dimension(:), intent(inout), target, optional :: work
            logical, intent(in), optional :: reverse
        end subroutine int16_radix_sort

        pure module subroutine int32_radix_sort(array, work, reverse)
            integer(kind=int32), dimension(:), intent(inout) :: array
            integer(kind=int32), dimension(:), intent(inout), target, optional :: work
            logical, intent(in), optional :: reverse
        end subroutine int32_radix_sort

        pure module subroutine int64_radix_sort(array, work, reverse)
            integer(kind=int64), dimension(:), intent(inout) :: array
            integer(kind=int64), dimension(:), intent(inout), target, optional :: work
            logical, intent(in), optional :: reverse
        end subroutine int64_radix_sort

        module subroutine sp_radix_sort(array, work, reverse)
            real(kind=sp), dimension(:), intent(inout), target :: array
            real(kind=sp), dimension(:), intent(inout), target, optional :: work
            logical, intent(in), optional :: reverse
        end subroutine sp_radix_sort

        module subroutine dp_radix_sort(array, work, reverse)
            real(kind=dp), dimension(:), intent(inout), target :: array
            real(kind=dp), dimension(:), intent(inout), target, optional :: work
            logical, intent(in), optional :: reverse
        end subroutine dp_radix_sort
    end interface radix_sort

    interface sort
!! Version: experimental
!!
!! The generic subroutine interface implementing the `SORT` algorithm, based
!! on the `introsort` of David Musser.
!! ([Specification](../page/specs/stdlib_sorting.html#sort-sorts-an-input-array))

        pure module subroutine int8_sort( array, reverse )
!! Version: experimental
!!
!! `int8_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int8)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            integer(int8), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine int8_sort

        pure module subroutine int16_sort( array, reverse )
!! Version: experimental
!!
!! `int16_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int16)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            integer(int16), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine int16_sort

        pure module subroutine int32_sort( array, reverse )
!! Version: experimental
!!
!! `int32_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            integer(int32), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine int32_sort

        pure module subroutine int64_sort( array, reverse )
!! Version: experimental
!!
!! `int64_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            integer(int64), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine int64_sort

        pure module subroutine sp_sort( array, reverse )
!! Version: experimental
!!
!! `sp_sort( array[, reverse] )` sorts the input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            real(sp), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine sp_sort

        pure module subroutine dp_sort( array, reverse )
!! Version: experimental
!!
!! `dp_sort( array[, reverse] )` sorts the input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            real(dp), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine dp_sort

        pure module subroutine string_type_sort( array, reverse )
!! Version: experimental
!!
!! `string_type_sort( array[, reverse] )` sorts the input `ARRAY` of type `type(string_type)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            type(string_type), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine string_type_sort

        pure module subroutine char_sort( array, reverse )
!! Version: experimental
!!
!! `char_sort( array[, reverse] )` sorts the input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            character(len=*), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine char_sort

        pure module subroutine bitset_64_sort( array, reverse )
!! Version: experimental
!!
!! `bitset_64_sort( array[, reverse] )` sorts the input `ARRAY` of type `type(bitset_64)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            type(bitset_64), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine bitset_64_sort

        pure module subroutine bitset_large_sort( array, reverse )
!! Version: experimental
!!
!! `bitset_large_sort( array[, reverse] )` sorts the input `ARRAY` of type `type(bitset_large)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
            type(bitset_large), intent(inout)         :: array(0:)
            logical, intent(in), optional :: reverse
        end subroutine bitset_large_sort


    end interface sort

    interface sort_index
!! Version: experimental
!!
!! The generic subroutine interface implementing the `SORT_INDEX` algorithm,
!! based on the `"Rust" sort` algorithm found in `slice.rs`
!! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
!! but modified to return an array of indices that would provide a stable
!! sort of the rank one `ARRAY` input.
!! ([Specification](../page/specs/stdlib_sorting.html#sort_index-creates-an-array-of-sorting-indices-for-an-input-array-while-also-sorting-the-array))
!!
!! The indices by default correspond to a
!! non-decreasing sort, but if the optional argument `REVERSE` is present
!! with a value of `.TRUE.` the indices correspond to a non-increasing sort.

        module subroutine int8_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int8_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int8)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int8), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            integer(int8), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int8_sort_index_default

        module subroutine int16_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int16_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int16)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int16), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            integer(int16), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int16_sort_index_default

        module subroutine int32_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int32_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int32), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            integer(int32), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int32_sort_index_default

        module subroutine int64_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int64_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int64), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            integer(int64), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int64_sort_index_default

        module subroutine sp_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `sp_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            real(sp), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            real(sp), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine sp_sort_index_default

        module subroutine dp_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `dp_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            real(dp), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            real(dp), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine dp_sort_index_default

        module subroutine string_type_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `string_type_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(string_type)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(string_type), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            type(string_type), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine string_type_sort_index_default

        module subroutine char_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `char_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            character(len=*), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            character(len=len(array)), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine char_sort_index_default

        module subroutine bitset_64_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `bitset_64_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(bitset_64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(bitset_64), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            type(bitset_64), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine bitset_64_sort_index_default

        module subroutine bitset_large_sort_index_default( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `bitset_large_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(bitset_large)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(bitset_large), intent(inout)                     :: array(0:)
            integer(int_index), intent(out)                      :: index(0:)
            type(bitset_large), intent(out), optional             :: work(0:)
            integer(int_index), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine bitset_large_sort_index_default

        module subroutine int8_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int8_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int8)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int8), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            integer(int8), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int8_sort_index_low

        module subroutine int16_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int16_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int16)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int16), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            integer(int16), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int16_sort_index_low

        module subroutine int32_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int32_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int32), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            integer(int32), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int32_sort_index_low

        module subroutine int64_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `int64_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            integer(int64), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            integer(int64), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine int64_sort_index_low

        module subroutine sp_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `sp_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            real(sp), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            real(sp), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine sp_sort_index_low

        module subroutine dp_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `dp_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            real(dp), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            real(dp), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine dp_sort_index_low

        module subroutine string_type_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `string_type_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(string_type)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(string_type), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            type(string_type), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine string_type_sort_index_low

        module subroutine char_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `char_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            character(len=*), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            character(len=len(array)), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine char_sort_index_low

        module subroutine bitset_64_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `bitset_64_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(bitset_64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(bitset_64), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            type(bitset_64), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine bitset_64_sort_index_low

        module subroutine bitset_large_sort_index_low( array, index, work, iwork, &
            reverse )
!! Version: experimental
!!
!! `bitset_large_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `type(bitset_large)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
            type(bitset_large), intent(inout)                     :: array(0:)
            integer(int_index_low), intent(out)                      :: index(0:)
            type(bitset_large), intent(out), optional             :: work(0:)
            integer(int_index_low), intent(out), optional            :: iwork(0:)
            logical, intent(in), optional             :: reverse
        end subroutine bitset_large_sort_index_low


    end interface sort_index


end module stdlib_sorting
