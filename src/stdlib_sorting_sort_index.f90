


!! Licensing:
!!
!! This file is subjec† both to the Fortran Standard Library license, and
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
!! The generic subroutine, `SORT_INDEX`, is substantially a translation to
!! Fortran 2008 of the `"Rust" sort` sorting routines in
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

submodule(stdlib_sorting) stdlib_sorting_sort_index

    implicit none

contains


    module subroutine int8_sort_index_default( array, index, work, iwork, reverse )
! A modification of `int8_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int8), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        integer(int8), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int8), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            integer(int8) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int8) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int8), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int8), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            integer(int8) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int8_sort_index_default


    module subroutine int16_sort_index_default( array, index, work, iwork, reverse )
! A modification of `int16_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int16), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        integer(int16), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int16), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            integer(int16) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int16) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int16), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int16), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            integer(int16) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int16_sort_index_default


    module subroutine int32_sort_index_default( array, index, work, iwork, reverse )
! A modification of `int32_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int32), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        integer(int32), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int32), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            integer(int32) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int32) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int32), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int32), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            integer(int32) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int32_sort_index_default


    module subroutine int64_sort_index_default( array, index, work, iwork, reverse )
! A modification of `int64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int64), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        integer(int64), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int64), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            integer(int64) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int64) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int64), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int64), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            integer(int64) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int64_sort_index_default


    module subroutine sp_sort_index_default( array, index, work, iwork, reverse )
! A modification of `sp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        real(sp), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        real(sp), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        real(sp), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            real(sp) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            real(sp) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            real(sp), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            real(sp), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            real(sp) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine sp_sort_index_default


    module subroutine dp_sort_index_default( array, index, work, iwork, reverse )
! A modification of `dp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        real(dp), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        real(dp), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        real(dp), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            real(dp) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            real(dp) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            real(dp), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            real(dp), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            real(dp) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine dp_sort_index_default


    module subroutine string_type_sort_index_default( array, index, work, iwork, reverse )
! A modification of `string_type_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(string_type), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        type(string_type), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(string_type), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            type(string_type) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            type(string_type) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            type(string_type), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(string_type), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            type(string_type) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine string_type_sort_index_default


    module subroutine char_sort_index_default( array, index, work, iwork, reverse )
! A modification of `char_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        character(len=*), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        character(len=len(array)), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        character(len=:), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( character(len=len(array)) :: buf(0:array_size/2-1), &
                      stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            character(len=len(array)) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            character(len=len(array)) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            character(len=len(array)), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            character(len=len(array)), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            character(len=len(array)) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine char_sort_index_default


    module subroutine bitset_64_sort_index_default( array, index, work, iwork, reverse )
! A modification of `bitset_64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(bitset_64), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        type(bitset_64), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(bitset_64), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            type(bitset_64) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            type(bitset_64) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            type(bitset_64), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(bitset_64), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            type(bitset_64) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine bitset_64_sort_index_default


    module subroutine bitset_large_sort_index_default( array, index, work, iwork, reverse )
! A modification of `bitset_large_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(bitset_large), intent(inout)         :: array(0:)
        integer(int_index), intent(out)           :: index(0:)
        type(bitset_large), intent(out), optional :: work(0:)
        integer(int_index), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(bitset_large), allocatable :: buf(:)
        integer(int_index), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index) :: key_index
            type(bitset_large) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            type(bitset_large) :: tmp
            integer(int_index) :: i
            integer(int_index) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)
            type(bitset_large), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(bitset_large), intent(inout) :: buf(0:)
            integer(int_index), intent(inout) :: index(0:)
            integer(int_index), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(inout) :: index(0:)

            integer(int_index) :: itemp
            integer(int_index) :: lo, hi
            type(bitset_large) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine bitset_large_sort_index_default


    module subroutine int8_sort_index_low( array, index, work, iwork, reverse )
! A modification of `int8_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int8), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        integer(int8), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int8), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int8), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            integer(int8) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int8), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int8) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int8), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int8), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int8), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int8), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int8), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            integer(int8) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int8_sort_index_low


    module subroutine int16_sort_index_low( array, index, work, iwork, reverse )
! A modification of `int16_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int16), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        integer(int16), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int16), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int16), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            integer(int16) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int16), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int16) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int16), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int16), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int16), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int16), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int16), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            integer(int16) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int16_sort_index_low


    module subroutine int32_sort_index_low( array, index, work, iwork, reverse )
! A modification of `int32_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int32), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        integer(int32), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int32), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int32), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            integer(int32) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int32), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int32) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int32), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int32), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int32), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int32), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int32), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            integer(int32) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int32_sort_index_low


    module subroutine int64_sort_index_low( array, index, work, iwork, reverse )
! A modification of `int64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        integer(int64), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        integer(int64), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        integer(int64), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            integer(int64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            integer(int64) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            integer(int64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int64) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            integer(int64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int64), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            integer(int64), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            integer(int64), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            integer(int64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            integer(int64) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine int64_sort_index_low


    module subroutine sp_sort_index_low( array, index, work, iwork, reverse )
! A modification of `sp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        real(sp), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        real(sp), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        real(sp), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            real(sp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            real(sp) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            real(sp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            real(sp) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            real(sp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            real(sp), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            real(sp), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            real(sp), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            real(sp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            real(sp) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine sp_sort_index_low


    module subroutine dp_sort_index_low( array, index, work, iwork, reverse )
! A modification of `dp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        real(dp), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        real(dp), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        real(dp), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            real(dp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            real(dp) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            real(dp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            real(dp) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            real(dp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            real(dp), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            real(dp), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            real(dp), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            real(dp), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            real(dp) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine dp_sort_index_low


    module subroutine string_type_sort_index_low( array, index, work, iwork, reverse )
! A modification of `string_type_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(string_type), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        type(string_type), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(string_type), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(string_type), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            type(string_type) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(string_type), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            type(string_type) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(string_type), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            type(string_type), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(string_type), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(string_type), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(string_type), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            type(string_type) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine string_type_sort_index_low


    module subroutine char_sort_index_low( array, index, work, iwork, reverse )
! A modification of `char_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        character(len=*), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        character(len=len(array)), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        character(len=:), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( character(len=len(array)) :: buf(0:array_size/2-1), &
                      stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            character(len=*), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            character(len=len(array)) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            character(len=*), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            character(len=len(array)) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            character(len=*), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            character(len=len(array)), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            character(len=*), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            character(len=len(array)), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            character(len=*), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            character(len=len(array)) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine char_sort_index_low


    module subroutine bitset_64_sort_index_low( array, index, work, iwork, reverse )
! A modification of `bitset_64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(bitset_64), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        type(bitset_64), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(bitset_64), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            type(bitset_64) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            type(bitset_64) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            type(bitset_64), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(bitset_64), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(bitset_64), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            type(bitset_64) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine bitset_64_sort_index_low


    module subroutine bitset_large_sort_index_low( array, index, work, iwork, reverse )
! A modification of `bitset_large_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

        type(bitset_large), intent(inout)         :: array(0:)
        integer(int_index_low), intent(out)           :: index(0:)
        type(bitset_large), intent(out), optional :: work(0:)
        integer(int_index_low), intent(out), optional :: iwork(0:)
        logical, intent(in), optional :: reverse

        type(bitset_large), allocatable :: buf(:)
        integer(int_index_low), allocatable :: ibuf(:)
        integer(int_index) :: array_size, i, stat

        array_size = size(array, kind=int_index)

        if ( array_size > huge(index)) then
            error stop "Too many entries for the kind of index."
        end if

        if ( array_size > size(index, kind=int_index) ) then
            error stop "Too many entries for the size of index."
        end if

        do i = 0, array_size-1
            index(i) = int(i+1, kind=int_index_low)
        end do

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

! If necessary allocate buffers to serve as scratch memory.
        if ( present(work) ) then
            if ( size(work, kind=int_index) < array_size/2 ) then
                error stop "work array is too small."
            end if
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, work, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, work, ibuf )
            end if
        else
            allocate( buf(0:array_size/2-1), stat=stat )
            if ( stat /= 0 ) error stop "Allocation of array buffer failed."
            if ( present(iwork) ) then
                if ( size(iwork, kind=int_index) < array_size/2 ) then
                    error stop "iwork array is too small."
                endif
                call merge_sort( array, index, buf, iwork )
            else
                allocate( ibuf(0:array_size/2-1), stat=stat )
                if ( stat /= 0 ) error stop "Allocation of index buffer failed."
                call merge_sort( array, index, buf, ibuf )
            end if
        end if

        if ( optval(reverse, .false.) ) then
            call reverse_segment( array, index )
        end if

    contains

        pure function calc_min_run( n ) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
            integer(int_index)             :: min_run
            integer(int_index), intent(in) :: n

            integer(int_index) :: num, r

            num = n
            r = 0_int_index

            do while( num >= 64 )
                r = ior( r, iand(num, 1_int_index) )
                num = ishft(num, -1_int_index)
            end do
            min_run = num + r

        end function calc_min_run


        pure subroutine insertion_sort( array, index )
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index) :: i, j
            integer(int_index_low) :: key_index
            type(bitset_large) :: key

            do j=1, size(array, kind=int_index)-1
                key = array(j)
                key_index = index(j)
                i = j - 1
                do while( i >= 0 )
                    if ( array(i) <= key ) exit
                    array(i+1) = array(i)
                    index(i+1) = index(i)
                    i = i - 1
                end do
                array(i+1) = key
                index(i+1) = key_index
            end do

        end subroutine insertion_sort


        pure function collapse( runs ) result ( r )
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
            integer(int_index) :: r
            type(run_type), intent(in), target :: runs(0:)

            integer(int_index) :: n
            logical :: test

            n = size(runs, kind=int_index)
            test = .false.
            if (n >= 2) then
                if ( runs( n-1 ) % base == 0 .or. &
                     runs( n-2 ) % len <= runs(n-1) % len ) then
                    test = .true.
                else if ( n >= 3 ) then ! X exists
                    if ( runs(n-3) % len <= &
                         runs(n-2) % len + runs(n-1) % len ) then
                        test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                    else if( n >= 4 ) then
                        if ( runs(n-4) % len <= &
                             runs(n-3) % len + runs(n-2) % len ) then
                            test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                        end if
                    end if
                end if
            end if
            if ( test ) then
! By default merge Y & Z, rho2 or rho3
                if ( n >= 3 ) then
                    if ( runs(n-3) % len < runs(n-1) % len ) then
                        r = n - 3
! |X| < |Z| => merge X & Y, rho1
                        return
                    end if
                end if
                r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
                return
            else
                r = -1
            end if

        end function collapse


        pure subroutine insert_head( array, index )
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            type(bitset_large) :: tmp
            integer(int_index) :: i
            integer(int_index_low) :: tmp_index

            tmp = array(0)
            tmp_index = index(0)
            find_hole: do i=1, size(array, kind=int_index)-1
                if ( array(i) >= tmp ) exit find_hole
                array(i-1) = array(i)
                index(i-1) = index(i)
            end do find_hole
            array(i-1) = tmp
            index(i-1) = tmp_index

        end subroutine insert_head


        subroutine merge_sort( array, index, buf, ibuf )
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            type(bitset_large), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_size, finish, min_run, r, r_count, &
                start
            type(run_type) :: runs(0:max_merge_stack-1), left, right

            array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
            min_run = calc_min_run( array_size )

            if ( array_size <= min_run ) then
                if ( array_size >= 2 ) call insertion_sort( array, index )
                return
            end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
            r_count = 0
            finish = array_size - 1
            do while ( finish >= 0 )
! Find the next natural run, and reverse it if it's strictly descending.
                start = finish
                if ( start > 0 ) then
                    start = start - 1
                    if ( array(start+1) < array(start) ) then
                        Descending: do while ( start > 0 )
                            if ( array(start) >= array(start-1) ) &
                                exit Descending
                            start = start - 1
                        end do Descending
                        call reverse_segment( array(start:finish), &
                                            index(start:finish) )
                    else
                        Ascending: do while( start > 0 )
                            if ( array(start) < array(start-1) ) exit Ascending
                            start = start - 1
                        end do Ascending
                    end if
                end if

! If the run is too short insert some more elements using an insertion sort.
                Insert: do while ( start > 0 )
                    if ( finish - start >= min_run - 1 ) exit Insert
                    start = start - 1
                    call insert_head( array(start:finish), index(start:finish) )
                end do Insert
                if ( start == 0 .and. finish == array_size - 1 ) return

                runs(r_count) = run_type( base = start, &
                                          len = finish - start + 1 )
                finish = start-1
                r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
                Merge_loop: do
                    r = collapse( runs(0:r_count - 1) )
                    if ( r < 0 .or. r_count <= 1 ) exit Merge_loop
                    left = runs( r + 1 )
                    right = runs( r )
                    call merge( array( left % base: &
                                       right % base + right % len - 1 ), &
                                left % len, buf, &
                                index( left % base: &
                                     right % base + right % len - 1 ), ibuf )

                    runs(r) = run_type( base = left % base, &
                                        len = left % len + right % len )
                    if ( r == r_count - 3 ) runs(r+1) = runs(r+2)
                    r_count = r_count - 1

                end do Merge_loop
            end do
            if ( r_count /= 1 ) &
                error stop "MERGE_SORT completed without RUN COUNT == 1."

        end subroutine merge_sort


        pure subroutine merge( array, mid, buf, index, ibuf )
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index), intent(in)  :: mid
            type(bitset_large), intent(inout) :: buf(0:)
            integer(int_index_low), intent(inout) :: index(0:)
            integer(int_index_low), intent(inout) :: ibuf(0:)

            integer(int_index) :: array_len, i, j, k

            array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

            if ( mid <= array_len - mid ) then ! The left run is shorter.
                buf(0:mid-1) = array(0:mid-1)
                ibuf(0:mid-1) = index(0:mid-1)
                i = 0
                j = mid
                merge_lower: do k = 0, array_len-1
                    if ( buf(i) <= array(j) ) then
                        array(k) = buf(i)
                        index(k) = ibuf(i)
                        i = i + 1
                        if ( i >= mid ) exit merge_lower
                    else
                        array(k) = array(j)
                        index(k) = index(j)
                        j = j + 1
                        if ( j >= array_len ) then
                            array(k+1:) = buf(i:mid-1)
                            index(k+1:) = ibuf(i:mid-1)
                            exit merge_lower
                        end if
                    end if
                end do merge_lower
            else ! The right run is shorter
                buf(0:array_len-mid-1) = array(mid:array_len-1)
                ibuf(0:array_len-mid-1) = index(mid:array_len-1)
                i = mid - 1
                j = array_len - mid -1
                merge_upper: do k = array_len-1, 0, -1
                    if ( buf(j) >= array(i) ) then
                        array(k) = buf(j)
                        index(k) = ibuf(j)
                        j = j - 1
                        if ( j < 0 ) exit merge_upper
                    else
                        array(k) = array(i)
                        index(k) = index(i)
                        i = i - 1
                        if ( i < 0 ) then
                            array(0:k-1) = buf(0:j)
                            index(0:k-1) = ibuf(0:j)
                            exit merge_upper
                        end if
                    end if
                end do merge_upper
            end if
        end subroutine merge


        pure subroutine reverse_segment( array, index )
! Reverse a segment of an array in place
            type(bitset_large), intent(inout) :: array(0:)
            integer(int_index_low), intent(inout) :: index(0:)

            integer(int_index_low) :: itemp
            integer(int_index) :: lo, hi
            type(bitset_large) :: temp

            lo = 0
            hi = size( array, kind=int_index ) - 1
            do while( lo < hi )
                temp = array(lo)
                array(lo) = array(hi)
                array(hi) = temp
                itemp = index(lo)
                index(lo) = index(hi)
                index(hi) = itemp
                lo = lo + 1
                hi = hi - 1
            end do

        end subroutine reverse_segment

    end subroutine bitset_large_sort_index_low


end submodule stdlib_sorting_sort_index
