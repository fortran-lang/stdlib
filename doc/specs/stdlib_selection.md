---
title: Selection Procedures
---

# The `stdlib_selection` module

[TOC]

## Overview of selection

Suppose you wish to find the value of the kth-smallest entry in an array, or the index
of that value. While it could be done by sorting the whole array using `sort` or `sort_index`
from `stdlib_sorting` and then finding the k-th entry, that would require O(N x LOG(N)) 
time. However selection of a single entry can be done in O(N) time, so is much faster for large arrays. 
This is useful, for example, to quickly find the median of an array, or some other percentile.

The Fortran Standard Library therefore provides a module, `stdlib_selection`, which implements
selection algorithms.

## Overview of the module

The module `stdlib_selection` defines two generic subroutines:
* `select` is used to find the kth-smallest entry of an array. The input
array is also modified in-place, and on return will be partially sorted 
such that `all(array(1:k) <= array(k)))`  and `all(array(k) <= array((k+1):size(array)))`.
The user can optionally specify `left` and `right` indices to constrain the search
for the kth-smallest value. This can be useful if you have previously called `select`
to find a smaller or larger rank (that will have led to partial sorting of
`array`, thus implying some constraint on the location).

* `arg_select` is used to find the index of the kth-smallest entry of an array. 
In this case the input array is not modified, but the user must provide an 
input index array with the same size as `array`, having unique indices from 
`1:size(array)`, which is modified instead. On return the index array is modified 
such that `all(array(index(1:k)) <= array(index(k)))` and `all(array(k) <= array(k+1:size(array)))`.
The user can optionally specify `left` and `right` indices to constrain the search
for the kth-smallest value. This can be useful if you have previously called `select`
to find a smaller or larger rank (that will have led to partial sorting of
`index`, thus implying some constraint on the location).

#### Licensing

The Fortran Standard Library is distributed under the MIT
License. However components of the library may be based on code with
additional licensing restrictions. In particular `select` and `arg_select`
were derived by modifying a matlab implementation of "qselect" by Manolis
Lourakis, https://www.mathworks.com/matlabcentral/fileexchange/68947-qselect
Below is the licence of the matlab qselect

Copyright (c) 2018, Manolis Lourakis
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution
* Neither the name of Foundation for Research and Technology - Hellas nor the
  names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


### Specifications of the `stdlib_selection` procedures

#### `select` - find the kth smallest value in an input array

##### Status

Experimental

##### Description

Returns the k-th smallest value of array `a(:)`, and also partially sorts `a(:)`
such that `all(a(1:k) <= a(k))`  and `all(a(k) <= a((k+1):size(a)))`

##### Syntax

`call [[stdlib_selection(module):select(interface)]]( a, k, kth_smallest [, left, right ] )`

##### Class

Generic subroutine.

##### Arguments

`a` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. It is an `intent(inout)` argument. On input it is
the array in which we search for the kth smallest entry. On return its elements 
will be partially sorted such that:
 `all( a(1:k-1) <= a(k) )` and `all(a(k) <= a(k+1:size(a)))`.

`k`: shall be a scalar with any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`. It 
is an `intent(in)` argument. We search for the `k`-th smallest entry of `a(:)`.

`kth_smallest`: shall be a scalar with the same type as `a`. On return it contains
the k-th smallest entry of `a(:)`. 

`left` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `a(left:size(a))`. If not present it is 1. This is typically useful if multiple calls
to `select` are made, because the partial sorting of `a` implies constraints on where
we need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `a(1:right)`. If not present it is `size(a)`. This is typically useful if multiple calls
to `select` are made, because the partial sorting of `a` implies constraints on where
we need to search.

##### Notes

Selection of a single value should have runtime of O(`size(a)`), so it is asymptotically faster
than sorting `a` entirely.

##### Example

```fortran
    program demo_select
      use stdlib_selection, only: select
      implicit none
     
      real, allocatable :: array(:)
      real :: kth_smallest
      integer :: k, left, right 

      array = 1.0 * [3, 2, 7, 4, 5, 1, 4, -1]

      k = 2
      call select(array, k, kth_smallest)
      print*, kth_smallest ! print 1

      k = 7
      ! Due to the previous call to select, we know for sure this is in an
      ! index >= 2
      call select(array, k, kth_smallest, left=2)
      print*, kth_smallest ! print 5

      k = 6
      ! Due to the previous two calls to select, we know for sure this is in 
      ! an index >= 2 and <= 7
      call select(array, k, kth_smallest, left=2, right=7)
      print*, kth_smallest ! print 4

    end program demo_select
```

#### `arg_select` - find the kth smallest value in an input array

##### Status

Experimental

##### Description

Returns the index of the k-th smallest value of array `a(:)`, and also partially sorts 
the index-array `indx(:)` such that `all(a(indx(1:k)) <= a(indx(k)))`  and 
`all(a(indx(k)) <= a(indx((k+1):size(a))))`

##### Syntax

`call [[stdlib_selection(module):arg_select(interface)]]( a, indx, k, kth_smallest [, left, right ] )`

##### Class

Generic subroutine.

##### Arguments

`a` : shall be a rank one array of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`,
`real(sp)`, `real(dp)`, `real(qp)`. It is an `intent(in)` argument. On input it is
the array in which we search for the kth smallest entry. 

`indx`: shall be a rank one array with the same size as `a`, containing integers
from `1:size(a)` in any order. It is of any of the types:
`integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`. It is an
`intent(inout)` argument. On return its elements will define a partial sorting of `a(:)` such that:
 `all( a(indx(1:k-1)) <= a(indx(k)) )` and `all(a(indx(k)) <= a(indx(k+1:size(a))))`.

`k`: shall be a scalar with the same type as `indx`. It is an `intent(in)`
argument. We search for the `k`-th smallest entry of `a(:)`.

`kth_smallest`: a scalar with the same type as `indx`. It is an `intent(out)` argument,
and on return it contains the index of the k-th smallest entry of `a(:)`. 

`left` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `a(indx(left:size(a)))`. If not present it is 1. This is typically useful if multiple calls
to `select` are made, because the partial sorting of `a` implies constraints on where
we need to search.

`right` (optional): shall be a scalar with the same type as `k`. It is an `intent(in)`
argument. If specified then we assume the k-th smallest value is definitely contained
in `a(indx(1:right))`. If not present it is `size(a)`. This is typically useful if multiple calls
to `select` are made, because the reordering of `indx` implies constraints on
where we need to search.

##### Notes

`arg_select` does not modify `a`, unlike `select`. 

While it is essential that that `indx` contains the integers 1:size(a), the code does not check for this.

Selection of a single value should have runtime of O(`size(a)`), so it is asymptotically faster
than sorting `a` entirely. 


##### Example


```fortran
    program demo_select
      use stdlib_selection, only: arg_select
      implicit none
     
      real, allocatable :: array(:)
      integer, allocatable :: indx(:)
      integer :: kth_smallest
      integer :: k, left, right 

      array = 1.0 * [3, 2, 7, 4, 5, 1, 4, -1]
      indx = (/( k, k = 1, size(array) )/)
      
      k = 2
      call arg_select(array, indx, k, kth_smallest)
      print*, array(kth_smallest) ! print 1

      k = 7
      ! Due to the previous call to arg_select, we know for sure this is in an
      ! index >= 2
      call arg_select(array, indx, k, kth_smallest, left=2)
      print*, array(kth_smallest) ! print 5

      k = 6
      ! Due to the previous two calls to arg_select, we know for sure this is in 
      ! an index >= 2 and <= 7
      call arg_select(array, indx, k, kth_smallest, left=2, right=7)
      print*, array(kth_smallest) ! print 4

    end program demo_select
```

