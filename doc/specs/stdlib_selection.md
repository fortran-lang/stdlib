---
title: Selection Procedures
---

# The `stdlib_selection` module

[TOC]

## Overview of selection

Suppose you wish to find the value of the kth-smallest entry in an array of size N, or
the index of that value. While it could be done by sorting the whole array
using `[[stdlib_sorting(module):sort(interface)]]` or 
`[[stdlib_sorting(module):sort_index(interface)]]` from 
`[[stdlib_sorting(module)]]` and then finding the k-th entry, that would
require O(N x LOG(N)) time. However selection of a single entry can be done in
O(N) time, which is much faster for large arrays.  This is useful, for example,
to quickly find the median of an array, or some other percentile.

The Fortran Standard Library therefore provides a module, `stdlib_selection`,
which implements selection algorithms.

## Overview of the module

The module `stdlib_selection` defines two generic subroutines:
* `select` is used to find the kth-smallest entry of an array. The input
array is also modified in-place, and on return will be partially sorted
such that `all(array(1:k) <= array(k)))`  and `all(array(k) <= array((k+1):size(array)))`.
The user can optionally specify `left` and `right` indices to constrain the search
for the kth-smallest value. This can be useful if you have previously called `select`
to find a smaller or larger rank (that will have led to partial sorting of
`array`, thus implying some constraints on the location).

* `arg_select` is used to find the index of the kth-smallest entry of an array.
In this case the input array is not modified, but the user must provide an
input index array with the same size as `array`, having unique indices from
`1:size(array)`, which is modified instead. On return the index array is modified
such that `all(array(index(1:k)) <= array(index(k)))` and `all(array(k) <= array(k+1:size(array)))`.
The user can optionally specify `left` and `right` indices to constrain the search
for the kth-smallest value. This can be useful if you have previously called `arg_select`
to find a smaller or larger rank (that will have led to partial sorting of
`index`, thus implying some constraints on the location).

#### Licensing

The Fortran Standard Library is distributed under the MIT
License. However components of the library may be based on code with
additional licensing restrictions. In particular `select` and `arg_select`
were derived using some code from quickSelect in the Coretran library, by Leon Foks,
https://github.com/leonfoks/coretran. Leon has given permission for the code here
to be released under stdlib's MIT licence.

### Specifications of the `stdlib_selection` procedures

#### `select` - find the kth smallest value in an input array

##### Status

Experimental

##### Description

Returns the k-th smallest value of `array(:)`, and also partially sorts `array(:)`
such that `all(array(1:k) <= array(k))`  and `all(array(k) <= array((k+1):size(array)))`

##### Syntax

`call [[stdlib_selection(module):select(interface)]]( array, k, kth_smallest [, left, right ] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. It is an `intent(inout)` argument. 

`k`: shall be a scalar with any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`. It
is an `intent(in)` argument. We search for the `k`-th smallest entry of `array(:)`.

`kth_smallest`: shall be a scalar with the same type as `array`. On return it contains
the k-th smallest entry of `array(:)`.

`left` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `array(left:size(array))`. If not present it is 1. This is typically useful if multiple calls
to `select` are made, because the partial sorting of `array` implies constraints on where
we need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `array(1:right)`. If not present it is `size(array)`. This is typically useful if multiple calls
to `select` are made, because the partial sorting of `array` implies constraints on where
we need to search.

##### Notes

Selection of a single value should have runtime of O(`size(array)`), so it is
asymptotically faster than sorting `array` entirely. The test program at the the
end of this document shows that is the case.

On return the elements of `array` will be partially sorted such that:
`all( array(1:k-1) <= array(k) )` and `all(array(k) <= array(k+1:size(array)))`.

##### Example

```fortran
    program demo_select
      use stdlib_selection, only: select
      implicit none

      real, allocatable :: array(:)
      real :: kth_smallest
      integer :: k, left, right

      array = [3., 2., 7., 4., 5., 1., 4., -1.]

      k = 2
      call select(array, k, kth_smallest)
      print*, kth_smallest ! print 1.0

      k = 7
      ! Due to the previous call to select, we know for sure this is in an
      ! index >= 2
      call select(array, k, kth_smallest, left=2)
      print*, kth_smallest ! print 5.0

      k = 6
      ! Due to the previous two calls to select, we know for sure this is in
      ! an index >= 2 and <= 7
      call select(array, k, kth_smallest, left=2, right=7)
      print*, kth_smallest ! print 4.0

    end program demo_select
```

#### `arg_select` - find the kth smallest value in an input array

##### Status

Experimental

##### Description

Returns the index of the k-th smallest value of `array(:)`, and also partially sorts
the index-array `indx(:)` such that `all(array(indx(1:k)) <= array(indx(k)))`  and
`all(array(indx(k)) <= array(indx((k+1):size(array))))`

##### Syntax

`call [[stdlib_selection(module):arg_select(interface)]]( array, indx, k, kth_smallest [, left, right ] )`

##### Class

Generic subroutine.

##### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. It is an `intent(in)` argument. On input it is
the array in which we search for the kth smallest entry.

`indx`: shall be a rank one array with the same size as `array`, containing all integers
from `1:size(array)` in any order. It is of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`. It is an
`intent(inout)` argument. On return its elements will define a partial sorting of `array(:)` such that:
 `all( array(indx(1:k-1)) <= array(indx(k)) )` and `all(array(indx(k)) <= array(indx(k+1:size(array))))`.

`k`: shall be a scalar with the same type as `indx`. It is an `intent(in)`
argument. We search for the `k`-th smallest entry of `array(:)`.

`kth_smallest`: a scalar with the same type as `indx`. It is an `intent(out)` argument,
and on return it contains the index of the k-th smallest entry of `array(:)`.

`left` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `array(indx(left:size(array)))`. If not present it is 1. This is typically useful if multiple calls
to `arg_select` are made, because the partial sorting of `indx` implies constraints on where
we need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `array(indx(1:right))`. If not present it is `size(array)`. This is typically useful if multiple calls
to `arg_select` are made, because the reordering of `indx` implies constraints on
where we need to search.

##### Notes

`arg_select` does not modify `array`, unlike `select`.

While it is essential that that `indx` contains the integers `1:size(array)` (in any
order), the code does not check for this.

Selection of a single value should have runtime of O(`size(array)`), so it is
asymptotically faster than sorting `array` entirely. The test program at the end of
these documents confirms that is the case.


##### Example


```fortran
    program demo_arg_select
      use stdlib_selection, only: arg_select
      implicit none

      real, allocatable :: array(:)
      integer, allocatable :: indx(:)
      integer :: kth_smallest
      integer :: k, left, right

      array = [3., 2., 7., 4., 5., 1., 4., -1.]
      indx = [( k, k = 1, size(array) )]

      k = 2
      call arg_select(array, indx, k, kth_smallest)
      print*, array(kth_smallest) ! print 1.0

      k = 7
      ! Due to the previous call to arg_select, we know for sure this is in an
      ! index >= 2
      call arg_select(array, indx, k, kth_smallest, left=2)
      print*, array(kth_smallest) ! print 5.0

      k = 6
      ! Due to the previous two calls to arg_select, we know for sure this is in
      ! an index >= 2 and <= 7
      call arg_select(array, indx, k, kth_smallest, left=2, right=7)
      print*, array(kth_smallest) ! print 4.0

    end program demo_arg_select
```

## Comparison with using `sort`

The following program compares the timings of `select` and `arg_select` for
computing the median of an array, vs using `sort` from stdlib.  In theory we
should see a speed improvement with the selection routines which grows like
LOG(size(`array`)).

```fortran
  program selection_vs_sort
    use stdlib_kinds, only: dp, sp, int64
    use stdlib_selection, only: select, arg_select
    use stdlib_sorting, only: sort
    implicit none

    call compare_select_sort_for_median(1)
    call compare_select_sort_for_median(11)
    call compare_select_sort_for_median(101)
    call compare_select_sort_for_median(1001)
    call compare_select_sort_for_median(10001)
    call compare_select_sort_for_median(100001)


    contains
        subroutine compare_select_sort_for_median(N)
            integer, intent(in) :: N

            integer :: i, k, result_arg_select, indx(N), indx_local(N)
            real :: random_vals(N), local_random_vals(N)
            integer, parameter :: test_reps = 100
            integer(int64) :: t0, t1
            real :: result_sort, result_select
            integer(int64) :: time_sort, time_select, time_arg_select
            logical :: select_test_passed, arg_select_test_passed

            ! Ensure N is odd
            if(mod(N, 2) /= 1) stop

            time_sort = 0
            time_select = 0
            time_arg_select = 0

            select_test_passed = .true.
            arg_select_test_passed = .true.

            indx = (/( i, i = 1, N) /)

            k = (N+1)/2 ! Deliberate integer division

            do i = 1, test_reps
                call random_number(random_vals)

                ! Compute the median with sorting
                local_random_vals = random_vals
                call system_clock(t0)
                call sort(local_random_vals)
                result_sort = local_random_vals(k)
                call system_clock(t1)
                time_sort = time_sort + (t1 - t0)

                ! Compute the median with selection, assuming N is odd
                local_random_vals = random_vals
                call system_clock(t0)
                call select(local_random_vals, k, result_select)
                call system_clock(t1)
                time_select = time_select + (t1 - t0)

                ! Compute the median with arg_select, assuming N is odd
                local_random_vals = random_vals
                indx_local = indx
                call system_clock(t0)
                call arg_select(local_random_vals, indx_local, k, result_arg_select)
                call system_clock(t1)
                time_arg_select = time_arg_select + (t1 - t0)

                if(result_select /= result_sort) select_test_passed = .FALSE.
                if(local_random_vals(result_arg_select) /= result_sort) arg_select_test_passed = .FALSE.
            end do

            print*, "select    ; N=", N, '; ', merge('PASS', 'FAIL', select_test_passed), &
                '; Relative-speedup-vs-sort:', (1.0*time_sort)/(1.0*time_select)
            print*, "arg_select; N=", N, '; ', merge('PASS', 'FAIL', arg_select_test_passed), &
                '; Relative-speedup-vs-sort:', (1.0*time_sort)/(1.0*time_arg_select)

        end subroutine

  end program
```

The results seem consistent with expectations when the `array` is large; the program prints:
```
 select    ; N=           1 ; PASS; Relative-speedup-vs-sort:   1.90928173    
 arg_select; N=           1 ; PASS; Relative-speedup-vs-sort:   1.76875830    
 select    ; N=          11 ; PASS; Relative-speedup-vs-sort:   1.14835048    
 arg_select; N=          11 ; PASS; Relative-speedup-vs-sort:   1.00794709    
 select    ; N=         101 ; PASS; Relative-speedup-vs-sort:   2.31012774    
 arg_select; N=         101 ; PASS; Relative-speedup-vs-sort:   1.92877376    
 select    ; N=        1001 ; PASS; Relative-speedup-vs-sort:   4.24190664    
 arg_select; N=        1001 ; PASS; Relative-speedup-vs-sort:   3.54580402    
 select    ; N=       10001 ; PASS; Relative-speedup-vs-sort:   5.61573362    
 arg_select; N=       10001 ; PASS; Relative-speedup-vs-sort:   4.79348087    
 select    ; N=      100001 ; PASS; Relative-speedup-vs-sort:   7.28823519    
 arg_select; N=      100001 ; PASS; Relative-speedup-vs-sort:   6.03007460    
```
