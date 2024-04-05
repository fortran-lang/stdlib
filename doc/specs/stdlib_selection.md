---
title: selection
---

# The `stdlib_selection` module

[TOC]

## Overview of selection

Suppose you wish to find the value of the k-th smallest entry in an array of size N, or
the index of that value. While it could be done by sorting the whole array
using [[stdlib_sorting(module):sort(interface)]] or 
[[stdlib_sorting(module):sort_index(interface)]] from 
[[stdlib_sorting(module)]] and then finding the k-th entry, that would
require O(N x LOG(N)) time. However selection of a single entry can be done in
O(N) time, which is much faster for large arrays.  This is useful, for example,
to quickly find the median of an array, or some other percentile.

The Fortran Standard Library therefore provides a module, `stdlib_selection`,
which implements selection algorithms.

## Overview of the module

The module `stdlib_selection` defines two generic subroutines:

* `select` is used to find the k-th smallest entry of an array. The input
array is also modified in-place, and on return will be partially sorted
such that `all(array(1:k) <= array(k)))`  and `all(array(k) <= array((k+1):size(array)))` is true.
The user can optionally specify `left` and `right` indices to constrain the search
for the k-th smallest value. This can be useful if you have previously called `select`
to find a smaller or larger rank (that will have led to partial sorting of
`array`, thus implying some constraints on the location).

* `arg_select` is used to find the index of the k-th smallest entry of an array.
In this case the input array is not modified, but the user must provide an
input index array with the same size as `array`, having indices that are a permutation of
`1:size(array)`, which is modified instead. On return the index array is modified
such that `all(array(index(1:k)) <= array(index(k)))` and `all(array(k) <= array(k+1:size(array)))`.
The user can optionally specify `left` and `right` indices to constrain the search
for the k-th smallest value. This can be useful if you have previously called `arg_select`
to find a smaller or larger rank (that will have led to partial sorting of
`index`, thus implying some constraints on the location).


## `select` - find the k-th smallest value in an input array

### Status

Experimental

### Description

Returns the k-th smallest value of `array(:)`, and also partially sorts `array(:)`
such that `all(array(1:k) <= array(k))`  and `all(array(k) <= array((k+1):size(array)))`

### Syntax

`call ` [[stdlib_selection(module):select(interface)]] `( array, k, kth_smallest [, left, right ] )`

### Class

Generic subroutine.

### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(xdp)`, `real(qp)`. It is an `intent(inout)` argument. 

`k`: shall be a scalar with any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`. It
is an `intent(in)` argument. We search for the `k`-th smallest entry of `array(:)`.

`kth_smallest`: shall be a scalar with the same type as `array`. It is an
`intent(out)` argument. On return it contains the k-th smallest entry of
`array(:)`.

`left` (optional): shall be a scalar with the same type as `k`. It is an
`intent(in)` argument. If specified then we assume the k-th smallest value is
definitely contained in `array(left:size(array))`. If `left` is not present,
the default is 1. This is typically useful if multiple calls to `select` are
made, because the partial sorting of `array` implies constraints on where we
need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an
`intent(in)` argument. If specified then we assume the k-th smallest value is
definitely contained in `array(1:right)`. If `right` is not present, the
default is `size(array)`. This is typically useful if multiple calls to
`select` are made, because the partial sorting of `array` implies constraints
on where we need to search.

### Notes

Selection of a single value should have runtime of O(`size(array)`), so it is
asymptotically faster than sorting `array` entirely. The test program at the
end of this document shows that is the case.

The code does not support `NaN` elements in `array`; it will run, but there is
no consistent interpretation given to the order of `NaN` entries of `array`
compared to other entries.

`select` was derived from code in the Coretran library by Leon Foks,
https://github.com/leonfoks/coretran. Leon Foks has given permission for the
code here to be released under stdlib's MIT license.

### Example

```fortran
{!example/selection/example_select.f90!}
```

## `arg_select` - find the index of the k-th smallest value in an input array

### Status

Experimental

### Description

Returns the index of the k-th smallest value of `array(:)`, and also partially sorts
the index-array `indx(:)` such that `all(array(indx(1:k)) <= array(indx(k)))`  and
`all(array(indx(k)) <= array(indx((k+1):size(array))))`

### Syntax

`call ` [[stdlib_selection(module):arg_select(interface)]] `( array, indx, k, kth_smallest [, left, right ] )`

### Class

Generic subroutine.

### Arguments

`array` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(xdp)`, `real(qp)`. It is an `intent(in)` argument. On input it is
the array in which we search for the k-th smallest entry.

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
in `array(indx(left:size(array)))`. If `left` is not present, the default is 1.
This is typically useful if multiple calls to `arg_select` are made, because
the partial sorting of `indx` implies constraints on where we need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `array(indx(1:right))`. If `right` is not present, the default is
`size(array)`. This is typically useful if multiple calls to `arg_select` are
made, because the reordering of `indx` implies constraints on where we need to
search.

### Notes

`arg_select` does not modify `array`, unlike `select`.

The partial sorting of `indx` is not stable, i.e., indices that map to equal
values of array may be reordered.

The code does not support `NaN` elements in `array`; it will run, but there is
no consistent interpretation given to the order of `NaN` entries of `array`
compared to other entries.

While it is essential that `indx` contains a permutation of the integers `1:size(array)`, 
the code does not check for this. For example if `size(array) == 4`, then we could have 
`indx = [4, 2, 1, 3]` or `indx = [1, 2, 3, 4]`, but not `indx = [2, 1, 2, 4]`. It is the user's
responsibility to avoid such errors.

Selection of a single value should have runtime of O(`size(array)`), so it is
asymptotically faster than sorting `array` entirely. The test program at the end of
these documents confirms that is the case.

`arg_select` was derived using code from the Coretran library by Leon Foks,
https://github.com/leonfoks/coretran. Leon Foks has given permission for the
code here to be released under stdlib's MIT license.

### Example


```fortran
{!example/selection/example_arg_select.f90!}
```

## Comparison with using `sort`

The following program compares the timings of `select` and `arg_select` for
computing the median of an array, vs using `sort` from `stdlib`.  In theory we
should see a speed improvement with the selection routines which grows like
LOG(size(`array`)).

```fortran
{!example/selection/selection_vs_sort.f90!}
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
