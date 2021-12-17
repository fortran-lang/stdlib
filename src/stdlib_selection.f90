! Specify kinds/types for the input array in select and arg_select
! The index arrays are of all INT_KINDS_TYPES

module stdlib_selection
!! Quickly find the k-th smallest value of an array, or the index of the k-th smallest value.
!! ([Specification](../page/specs/stdlib_selection.html))
!
! This code was modified from the "Coretran" implementation "quickSelect" by
! Leon Foks, https://github.com/leonfoks/coretran/tree/HEAD/src/sorting
!
! Leon Foks gave permission to release this code under stdlib's MIT license.
! (https://github.com/fortran-lang/stdlib/pull/500#commitcomment-57418593)
!

use stdlib_kinds

implicit none

private

public :: select, arg_select

interface select
    !! version: experimental
    !! ([Specification](..//page/specs/stdlib_selection.html#select-find-the-k-th-smallest-value-in-an-input-array))

      module procedure select_1_iint8_int8
      module procedure select_1_iint8_int16
      module procedure select_1_iint8_int32
      module procedure select_1_iint8_int64
      module procedure select_1_iint16_int8
      module procedure select_1_iint16_int16
      module procedure select_1_iint16_int32
      module procedure select_1_iint16_int64
      module procedure select_1_iint32_int8
      module procedure select_1_iint32_int16
      module procedure select_1_iint32_int32
      module procedure select_1_iint32_int64
      module procedure select_1_iint64_int8
      module procedure select_1_iint64_int16
      module procedure select_1_iint64_int32
      module procedure select_1_iint64_int64
      module procedure select_1_rsp_int8
      module procedure select_1_rsp_int16
      module procedure select_1_rsp_int32
      module procedure select_1_rsp_int64
      module procedure select_1_rdp_int8
      module procedure select_1_rdp_int16
      module procedure select_1_rdp_int32
      module procedure select_1_rdp_int64
end interface

interface arg_select
    !! version: experimental
    !! ([Specification](..//page/specs/stdlib_selection.html#arg_select-find-the-index-of-the-k-th-smallest-value-in-an-input-array))
      module procedure arg_select_1_iint8_int8
      module procedure arg_select_1_iint8_int16
      module procedure arg_select_1_iint8_int32
      module procedure arg_select_1_iint8_int64
      module procedure arg_select_1_iint16_int8
      module procedure arg_select_1_iint16_int16
      module procedure arg_select_1_iint16_int32
      module procedure arg_select_1_iint16_int64
      module procedure arg_select_1_iint32_int8
      module procedure arg_select_1_iint32_int16
      module procedure arg_select_1_iint32_int32
      module procedure arg_select_1_iint32_int64
      module procedure arg_select_1_iint64_int8
      module procedure arg_select_1_iint64_int16
      module procedure arg_select_1_iint64_int32
      module procedure arg_select_1_iint64_int64
      module procedure arg_select_1_rsp_int8
      module procedure arg_select_1_rsp_int16
      module procedure arg_select_1_rsp_int32
      module procedure arg_select_1_rsp_int64
      module procedure arg_select_1_rdp_int8
      module procedure arg_select_1_rdp_int16
      module procedure arg_select_1_rdp_int32
      module procedure arg_select_1_rdp_int64
end interface

contains

      subroutine select_1_iint8_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int8), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int8), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint8_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int8), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int8), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint8_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int8), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int8), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint8_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int8), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int8), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint16_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int16), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int16), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint16_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int16), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int16), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint16_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int16), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int16), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint16_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int16), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int16), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint32_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int32), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int32), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint32_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int32), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int32), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint32_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int32), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int32), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint32_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int32), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int32), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint64_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int64), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int64), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint64_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int64), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int64), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint64_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int64), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int64), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_iint64_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  integer(int64), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  integer(int64), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rsp_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(sp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(sp), intent(inout) :: a, b
                  real(sp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(sp), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(sp), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rsp_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(sp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(sp), intent(inout) :: a, b
                  real(sp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(sp), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(sp), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rsp_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(sp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(sp), intent(inout) :: a, b
                  real(sp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(sp), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(sp), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rsp_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(sp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(sp), intent(inout) :: a, b
                  real(sp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(sp), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(sp), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rdp_int8(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int8), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(dp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(dp), intent(inout) :: a, b
                  real(dp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(dp), intent(inout) :: a(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(dp), intent(inout) :: array(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rdp_int16(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int16), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(dp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(dp), intent(inout) :: a, b
                  real(dp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(dp), intent(inout) :: a(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(dp), intent(inout) :: array(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rdp_int32(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int32), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(dp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(dp), intent(inout) :: a, b
                  real(dp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(dp), intent(inout) :: a(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(dp), intent(inout) :: array(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine select_1_rdp_int64(a, k, kth_smallest, left, right)
          !! select - select the k-th smallest entry in a(:).
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(inout) :: a(:)
              !! Array in which we seek the k-th smallest entry.
              !! On output it will be partially sorted such that
              !! `all(a(1:(k-1)) <= a(k)) .and. all(a(k) <= a((k+1):size(a)))`
              !! is true.
          integer(int64), intent(in) :: k
              !! We want the k-th smallest entry. E.G. `k=1` leads to
              !! `kth_smallest=min(a)`, and `k=size(a)` leads to
              !! `kth_smallest=max(a)`
          real(dp), intent(out) :: kth_smallest
              !! On output contains the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!    the k-th smallest entry of `a` is in `a(left:right)`
              !! and also that:
              !!    `maxval(a(1:(left-1))) <= minval(a(left:right))`
              !! and:
              !!    `maxval(a(left:right))) <= minval(a((right+1):size(a)))`
              !! then one or both bounds can be specified to narrow the search.
              !! The constraints are available if we have previously called the
              !! subroutine with different `k` (because of how `a(:)` becomes
              !! partially sorted, see documentation for `a(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "select must have  1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call medianOf3(a, l, mid, r)
              call swap(a(l), a(mid))
              call partition(a, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = a(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  real(dp), intent(inout) :: a, b
                  real(dp) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine medianOf3(a, left, mid, right)
                  real(dp), intent(inout) :: a(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(right) < a(left)) call swap(a(right), a(left))
                  if(a(mid)   < a(left)) call swap(a(mid)  , a(left))
                  if(a(right) < a(mid) ) call swap(a(mid)  , a(right))
              end subroutine

              pure subroutine partition(array,left,right,iPivot)
                  real(dp), intent(inout) :: array(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(left)
                  lo = left
                  hi=right
                  do while (lo <= hi)
                    do while (array(hi) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                       if(array(lo) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(array(lo),array(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(array(left),array(hi))
                  iPivot=hi
              end subroutine
      end subroutine


      subroutine arg_select_1_iint8_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int8), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int8), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint8_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int8), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int8), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint8_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int8), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int8), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint8_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int8), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int8), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int8), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int8) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint16_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int16), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int16), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint16_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int16), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int16), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint16_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int16), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int16), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint16_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int16), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int16), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int16), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int16) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint32_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int32), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int32), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint32_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int32), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int32), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint32_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int32), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int32), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint32_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int32), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int32), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int32), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int32) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint64_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int64), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int64), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint64_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int64), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int64), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint64_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int64), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int64), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_iint64_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          integer(int64), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  integer(int64), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  integer(int64), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  integer(int64) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rsp_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(sp), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(sp), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rsp_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(sp), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(sp), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rsp_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(sp), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(sp), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rsp_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(sp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(sp), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(sp), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  real(sp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rdp_int8(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int8), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int8), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int8), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int8), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int8) :: l, r, mid, iPivot
          integer, parameter :: ip = int8

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int8), intent(inout) :: a, b
                  integer(int8) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(dp), intent(in) :: a(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(dp), intent(in) :: array(:)
                  integer(int8), intent(inout) :: indx(:)
                  integer(int8), intent(in) :: left, right
                  integer(int8), intent(out) :: iPivot 
                  integer(int8) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rdp_int16(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int16), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int16), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int16), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int16), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int16) :: l, r, mid, iPivot
          integer, parameter :: ip = int16

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int16), intent(inout) :: a, b
                  integer(int16) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(dp), intent(in) :: a(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(dp), intent(in) :: array(:)
                  integer(int16), intent(inout) :: indx(:)
                  integer(int16), intent(in) :: left, right
                  integer(int16), intent(out) :: iPivot 
                  integer(int16) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rdp_int32(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int32), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int32), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int32), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int32), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int32) :: l, r, mid, iPivot
          integer, parameter :: ip = int32

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int32), intent(inout) :: a, b
                  integer(int32) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(dp), intent(in) :: a(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(dp), intent(in) :: array(:)
                  integer(int32), intent(inout) :: indx(:)
                  integer(int32), intent(in) :: left, right
                  integer(int32), intent(out) :: iPivot 
                  integer(int32) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine
      subroutine arg_select_1_rdp_int64(a, indx, k, kth_smallest, left, right)
          !! arg_select - find the index of the k-th smallest entry in `a(:)`
          !!
          !! Partly derived from the "Coretran" implementation of 
          !! quickSelect by Leon Foks, https://github.com/leonfoks/coretran
          !!
          real(dp), intent(in) :: a(:)
              !! Array in which we seek the k-th smallest entry.
          integer(int64), intent(inout) :: indx(:)
              !! Array of indices into `a(:)`. Must contain each integer
              !! from `1:size(a)` exactly once. On output it will be partially
              !! sorted such that
              !! `all( a(indx(1:(k-1)))) <= a(indx(k)) ) .AND.
              !!  all( a(indx(k))  <= a(indx( (k+1):size(a) )) )`.
          integer(int64), intent(in) :: k
              !! We want index of the k-th smallest entry. E.G. `k=1` leads to
              !! `a(kth_smallest) = min(a)`, and `k=size(a)` leads to
              !! `a(kth_smallest) = max(a)`
          integer(int64), intent(out) :: kth_smallest
              !! On output contains the index with the k-th smallest value of `a(:)`
          integer(int64), intent(in), optional :: left, right
              !! If we know that:
              !!  the k-th smallest entry of `a` is in `a(indx(left:right))`
              !! and also that:
              !!  `maxval(a(indx(1:(left-1)))) <= minval(a(indx(left:right)))`
              !! and:
              !!  `maxval(a(indx(left:right))) <= minval(a(indx((right+1):size(a))))`
              !! then one or both bounds can be specified to reduce the search
              !! time. These constraints are available if we have previously
              !! called the subroutine with a different `k` (due to the way that
              !! `indx(:)` becomes partially sorted, see documentation for `indx(:)`).

          integer(int64) :: l, r, mid, iPivot
          integer, parameter :: ip = int64

          l = 1_ip
          if(present(left)) l = left
          r = size(a, kind=ip)
          if(present(right)) r = right

          if(size(a) /= size(indx)) then
              error stop "arg_select must have size(a) == size(indx)"
          end if

          if(l > r .or. l < 1_ip .or. r > size(a, kind=ip) &
              .or. k < l .or. k > r                        & !i.e. if k is not in the interval [l; r]  
              ) then
              error stop "arg_select must have 1 <= left <= k <= right <= size(a)";
          end if

          searchk: do
              mid = l + ((r-l)/2_ip) ! Avoid (l+r)/2 which can cause overflow

              call arg_medianOf3(a, indx, l, mid, r)
              call swap(indx(l), indx(mid))
              call arg_partition(a, indx, l, r, iPivot)

              if (iPivot < k) then
                l = iPivot + 1_ip
              elseif (iPivot > k) then
                r = iPivot - 1_ip
              elseif (iPivot == k) then
                kth_smallest = indx(k)
                return
              end if
          end do searchk

          contains
              pure subroutine swap(a, b)
                  integer(int64), intent(inout) :: a, b
                  integer(int64) :: tmp
                  tmp = a; a = b; b = tmp
              end subroutine

              pure subroutine arg_medianOf3(a, indx, left, mid, right)
                  real(dp), intent(in) :: a(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, mid, right 
                  if(a(indx(right)) < a(indx(left))) call swap(indx(right), indx(left))
                  if(a(indx(mid))   < a(indx(left))) call swap(indx(mid)  , indx(left))
                  if(a(indx(right)) < a(indx(mid)) ) call swap(indx(mid)  , indx(right))
              end subroutine

              pure subroutine arg_partition(array, indx, left,right,iPivot)
                  real(dp), intent(in) :: array(:)
                  integer(int64), intent(inout) :: indx(:)
                  integer(int64), intent(in) :: left, right
                  integer(int64), intent(out) :: iPivot 
                  integer(int64) :: lo,hi
                  real(dp) :: pivot

                  pivot = array(indx(left))
                  lo = left
                  hi = right
                  do while (lo <= hi)
                    do while (array(indx(hi)) > pivot)
                      hi=hi-1_ip
                    end do
                    inner_lohi: do while (lo <= hi )
                      if(array(indx(lo)) > pivot) exit inner_lohi
                      lo=lo+1_ip
                    end do inner_lohi
                    if (lo <= hi) then
                      call swap(indx(lo),indx(hi))
                      lo=lo+1_ip
                      hi=hi-1_ip
                    end if
                  end do
                  call swap(indx(left),indx(hi))
                  iPivot=hi
              end subroutine
      end subroutine

end module


