#include "macros.inc"


module test_sorting

    use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit
    use stdlib_kinds, only: int8, int16, int32, int64, dp, sp, xdp, qp
    use stdlib_sorting, only: sort, sort_index, sort_adjoint, ord_sort, radix_sort, int_index, int_index_low
    use stdlib_string_type, only: string_type, assignment(=), operator(>), &
        operator(<), write(formatted)
#if STDLIB_BITSETS
    use stdlib_bitsets, only: bitset_64, bitset_large, &
        assignment(=), operator(>), operator(<)
#endif
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    integer(int32), parameter :: test_power = 16
    integer(int32), parameter :: char_set_size = 16
    integer(int32), parameter :: test_size = 2_int32**test_power
    integer(int32), parameter :: char_size = char_set_size**4
    integer(int32), parameter :: string_size = char_set_size**3
#if STDLIB_BITSETS
    integer(int32), parameter :: bitset_size = char_set_size**3
#endif
    integer(int32), parameter :: block_size = test_size/6
    integer, parameter        :: repeat = 1

    integer(int32) ::             &
        blocks(0:test_size-1),    &
        decrease(0:test_size-1),  &
        identical(0:test_size-1), &
        increase(0:test_size-1),  &
        rand0(0:test_size-1),     &
        rand1(0:test_size-1),     &
        rand2(0:test_size-1),     &
        rand3(0:test_size-1),     &
        rand10(0:test_size-1)
    real(sp) :: rand_r32(0:test_size-1)
    character(len=4) ::               &
        char_decrease(0:char_size-1), &
        char_increase(0:char_size-1), &
        char_rand(0:char_size-1)
    type(string_type) ::                  &
        string_decrease(0:string_size-1), &
        string_increase(0:string_size-1), &
        string_rand(0:string_size-1)
#if STDLIB_BITSETS
    type(bitset_large) ::                  &
        bitsetl_decrease(0:bitset_size-1), &
        bitsetl_increase(0:bitset_size-1), &
        bitsetl_rand(0:bitset_size-1)
    type(bitset_64) ::                  &
        bitset64_decrease(0:bitset_size-1), &
        bitset64_increase(0:bitset_size-1), &
        bitset64_rand(0:bitset_size-1)
#endif

    integer(int32)          :: dummy(0:test_size-1)
    real(sp)                :: real_dummy(0:test_size-1)
    character(len=4)        :: char_dummy(0:char_size-1)
    type(string_type)       :: string_dummy(0:string_size-1)
#if STDLIB_BITSETS
    type(bitset_large)      :: bitsetl_dummy(0:bitset_size-1)
    type(bitset_64)         :: bitset64_dummy(0:bitset_size-1)
#endif
    integer(int_index)      :: index_default(0:max(test_size, char_size, string_size)-1)
    integer(int_index_low)  :: index_low(0:max(test_size, char_size, string_size)-1)
    integer(int32)          :: work(0:test_size/2-1)
    character(len=4)        :: char_work(0:char_size/2-1)
    type(string_type)       :: string_work(0:string_size/2-1)
#if STDLIB_BITSETS
    type(bitset_large)      :: bitsetl_work(0:bitset_size/2-1)
    type(bitset_64)         :: bitset64_work(0:bitset_size/2-1)
#endif
    integer(int_index)      :: iwork_default(0:max(test_size, char_size, &
                                     string_size)/2-1)
    integer(int_index_low)  :: iwork_low(0:max(test_size, char_size, &
                                         string_size)/2-1)
    integer                 :: count, i, index1, index2, j, k, l, temp
    real(sp)                :: arand, brand
    character(*), parameter :: filename = 'test_sorting.txt'
    integer                 :: lun
    character(len=4)        :: char_temp
    type(string_type)       :: string_temp
#if STDLIB_BITSETS
    type(bitset_large)      :: bitsetl_temp
    type(bitset_64)         :: bitset64_temp
#endif
    logical                 :: ltest, ldummy
    character(32)           :: bin32
    character(64)           :: bin64

contains

    !> Collect all exported unit tests
    subroutine collect_sorting(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('char_ord_sorts', test_char_ord_sorts), &
            new_unittest('string_ord_sorts', test_string_ord_sorts), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_ord_sorts', test_bitsetl_ord_sorts), &
            new_unittest('bitset_64_ord_sorts', test_bitset64_ord_sorts), &
#endif
            new_unittest('int_radix_sorts', test_int_radix_sorts), &
            new_unittest('real_radix_sorts', test_real_radix_sorts), &
            new_unittest('int_sorts', test_int_sorts), &
            new_unittest('char_sorts', test_char_sorts), &
            new_unittest('string_sorts', test_string_sorts), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sorts', test_bitsetl_sorts), &
            new_unittest('bitset_64_sorts', test_bitset64_sorts), &
#endif
            new_unittest('int_sort_indexes_default', test_int_sort_indexes_default), &
            new_unittest('char_sort_indexes_default', test_char_sort_indexes_default), &
            new_unittest('string_sort_indexes_default', test_string_sort_indexes_default), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_indexes_default', test_bitsetl_sort_indexes_default), &
            new_unittest('bitset_64_sort_indexes_default', test_bitset64_sort_indexes_default), &
#endif
            new_unittest('int_sort_indexes_low', test_int_sort_indexes_low), &
            new_unittest('char_sort_indexes_low', test_char_sort_indexes_low), &
            new_unittest('string_sort_indexes_low', test_string_sort_indexes_low), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_indexes_low', test_bitsetl_sort_indexes_low), &
            new_unittest('bitset_64_sort_indexes_low', test_bitset64_sort_indexes_low), &
#endif
            new_unittest('int_sort_adjointes_int8', test_int_sort_adjointes_int8), &
            new_unittest('char_sort_adjointes_int8', test_char_sort_adjointes_int8), &
            new_unittest('string_sort_adjointes_int8', test_string_sort_adjointes_int8), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_adjointes_int8', test_bitsetl_sort_adjointes_int8), &
            new_unittest('bitset_64_sort_adjointes_int8', test_bitset64_sort_adjointes_int8), &
#endif
            new_unittest('int_sort_adjointes_int16', test_int_sort_adjointes_int16), &
            new_unittest('char_sort_adjointes_int16', test_char_sort_adjointes_int16), &
            new_unittest('string_sort_adjointes_int16', test_string_sort_adjointes_int16), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_adjointes_int16', test_bitsetl_sort_adjointes_int16), &
            new_unittest('bitset_64_sort_adjointes_int16', test_bitset64_sort_adjointes_int16), &
#endif
            new_unittest('int_sort_adjointes_int32', test_int_sort_adjointes_int32), &
            new_unittest('char_sort_adjointes_int32', test_char_sort_adjointes_int32), &
            new_unittest('string_sort_adjointes_int32', test_string_sort_adjointes_int32), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_adjointes_int32', test_bitsetl_sort_adjointes_int32), &
            new_unittest('bitset_64_sort_adjointes_int32', test_bitset64_sort_adjointes_int32), &
#endif
            new_unittest('int_sort_adjointes_int64', test_int_sort_adjointes_int64), &
            new_unittest('char_sort_adjointes_int64', test_char_sort_adjointes_int64), &
            new_unittest('string_sort_adjointes_int64', test_string_sort_adjointes_int64), &
#if STDLIB_BITSETS
            new_unittest('bitset_large_sort_adjointes_int64', test_bitsetl_sort_adjointes_int64), &
            new_unittest('bitset_64_sort_adjointes_int64', test_bitset64_sort_adjointes_int64), &
#endif
            new_unittest('real_sort_adjointes_sp', test_real_sort_adjointes_sp), &
            new_unittest('real_sort_adjointes_dp', test_real_sort_adjointes_dp), &
            new_unittest('int_ord_sorts', test_int_ord_sorts) &
        ]

    end subroutine collect_sorting


    subroutine initialize_tests()

        ! Create the test arrays
        identical(:) = 10
        do i=0, test_size-1
            increase(i) = i
            decrease(i) = test_size - 1 - i
            call random_number( arand )
            rand0(i) = int( floor( 4 * arand * test_size ), kind=int32 )
            rand1(i) = int( floor( arand * test_size / 4 ), kind=int32 )
        end do
        blocks(:) = increase(:)
        blocks(0:block_size-1) = increase(4*block_size:5*block_size-1)
        blocks(block_size:2*block_size-1) = increase(0:block_size-1)
        blocks(2*block_size:3*block_size-1) = increase(2*block_size:3*block_size-1)
        blocks(3*block_size:4*block_size-1) = increase(block_size:2*block_size-1)
        blocks(4*block_size:5*block_size-1) = increase(3*block_size:4*block_size-1)
        rand2(:) = increase(:)
        do i=0, test_size-1
            call random_number( arand )
            index1 = int( floor( arand * test_size ), kind=int32 )
            temp = rand2(i)
            rand2(i) = rand2(index1)
            rand2(index1) = temp
        end do
        rand3(:) = increase(:)
        do i=0, 2
            call random_number( arand )
            call random_number( brand )
            index1 = int( floor( arand * test_size ), kind=int32 )
            index2 = int( floor( brand * test_size ), kind=int32 )
            temp = rand3(index1)
            rand3(index1) = rand3(index2)
            rand3(index2) = temp
        end do
        rand10(:) = increase(:)
        do i=test_size-10, test_size-1
            call random_number( arand )
            rand10(i) = int( floor( arand * test_size ), kind=int32 )
        end do

        call random_number(rand_r32)
        rand_r32 = rand_r32 - 0.5 ! to test both positive and negative numbers

        count = 0
        do i=0, char_set_size-1
            do j=0, char_set_size-1
                do k=0, char_set_size-1
                    do l=0, char_set_size-1
                        char_increase(count) = achar(97+i) // achar(97+j) // &
                            achar(97+k) // achar(97+l)
                        count = count + 1
                    end do
                end do
            end do
        end do

        do i=0, char_size-1
            char_decrease(char_size-1-i) = char_increase(i)
        end do

        char_rand(:) = char_increase(:)
        do i=0, char_size-1
            call random_number( arand )
            index1 = int( floor( arand * char_size ), kind=int32 )
            char_temp = char_rand(i)
            char_rand(i) = char_rand(index1)
            char_rand(index1) = char_temp
        end do

        count = 0
        do i=0, char_set_size-1
            do j=0, char_set_size-1
                do k=0, char_set_size-1
                    string_increase(count) = achar(97+i) // achar(97+j) // &
                        achar(97+k)
                    count = count + 1
                end do
            end do
        end do

        do i=0, string_size-1
            string_decrease(string_size - 1 - i) = string_increase(i)
        end do

        string_rand(:) = string_increase(:)
        do i=0, string_size-1
            call random_number( arand )
            index1 = int( floor( arand * string_size ), kind=int32 )
            string_temp = string_rand(i)
            string_rand(i) = string_rand(index1)
            string_rand(index1) = string_temp
        end do

#if STDLIB_BITSETS
        do i = 0, bitset_size-1
            write(bin32,'(b32.32)') i
            call bitsetl_increase(i)%from_string(bin32)
        end do
        do i=0, bitset_size-1
            bitsetl_decrease(bitset_size-1-i) = bitsetl_increase(i)
        end do

        bitsetl_rand(:) = bitsetl_increase(:)
        do i=0, bitset_size-1
            call random_number( arand )
            index1 = int( floor( arand * bitset_size ), kind=int32 )
            bitsetl_temp = bitsetl_rand(i)
            bitsetl_rand(i) = bitsetl_rand(index1)
            bitsetl_rand(index1) = bitsetl_temp
        end do

        do i = 0, bitset_size-1
            write(bin64,'(b64.64)') i
            call bitset64_increase(i)%from_string(bin64)
        end do
        do i=0, bitset_size-1
            bitset64_decrease(bitset_size-1-i) = bitset64_increase(i)
        end do

        bitset64_rand(:) = bitset64_increase(:)
        do i=0, bitset_size-1
            call random_number( arand )
            index1 = int( floor( arand * bitset_size ), kind=int32 )
            bitset64_temp = bitset64_rand(i)
            bitset64_rand(i) = bitset64_rand(index1)
            bitset64_rand(index1) = bitset64_temp
        end do
#endif

        ! Create and intialize file to report the results of the sortings
        open( newunit=lun, file=filename, access='sequential', action='write', &
            form='formatted', status='replace' )
        write( lun, '(a)' ) trim(compiler_version())
        write( lun, * )
        write( lun, '("|     Type     | Elements |    Array Name   |    Method ' // &
            '  |  Time (s) |")' )
        write( lun, '("|--------------|----------|-----------------|-----------' // &
            '--|-----------|")' )

    end subroutine initialize_tests


    subroutine test_int_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)       :: i
        integer, allocatable :: d1(:)
        logical              :: ltest

        call test_int_ord_sort( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_ord_sort( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_ord_sort( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_ord_sort( increase, "Increasing", ltest )
        call check(error, ltest)

        if (allocated(error)) return
        call test_int_ord_sort( rand1, "Random dense", ltest )
        call check(error, ltest)

        if (allocated(error)) return
        call test_int_ord_sort( rand2, "Random order", ltest )
        call check(error, ltest)

        if (allocated(error)) return
        call test_int_ord_sort( rand0, "Random sparse", ltest )
        call check(error, ltest)

        if (allocated(error)) return
        call test_int_ord_sort( rand3, "Random 3", ltest )
        call check(error, ltest)

        if (allocated(error)) return
        call test_int_ord_sort( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        !triggered an issue in insertion_sort
        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        call ord_sort( d1 )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_ord_sorts


    subroutine test_int_ord_sort( a, a_name, ltest )
        integer(int32), intent(in) :: a(:)
        character(*), intent(in)   :: a_name
        logical, intent(out)       :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            call system_clock( t0, rate )
            call ord_sort( dummy, work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        dummy = a
        call ord_sort( dummy, work, reverse = .true.)
        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if

        dummy = a
        call ord_sort( dummy, reverse = .true.)
        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if

    end subroutine test_int_ord_sort

    subroutine test_char_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_ord_sort( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_ord_sort( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_ord_sort( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_ord_sorts

    subroutine test_char_ord_sort( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            call system_clock( t0, rate )
            call ord_sort( char_dummy, char_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        char_dummy = a
        call ord_sort( char_dummy, char_work, reverse = .true. )

        call verify_char_reverse_sort( char_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if

        char_dummy = a
        call ord_sort( char_dummy, reverse = .true. )

        call verify_char_reverse_sort( char_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if

    end subroutine test_char_ord_sort

    subroutine test_string_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical:: ltest

        call test_string_ord_sort( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_ord_sort( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_ord_sort( string_rand, "String Random" , ltest)
        call check(error, ltest)

    end subroutine test_string_ord_sorts

    subroutine test_string_ord_sort( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            call system_clock( t0, rate )
            call ord_sort( string_dummy, string_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        string_dummy = a
        call ord_sort( string_dummy, string_work, reverse = .true. )

        call verify_string_reverse_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if

        string_dummy = a
        call ord_sort( string_dummy, reverse = .true. )

        call verify_string_reverse_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a, 2(1x,a))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if

    end subroutine test_string_ord_sort

#if STDLIB_BITSETS
    subroutine test_bitsetl_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical:: ltest

        call test_bitsetl_ord_sort( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_ord_sort( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_ord_sort( bitsetl_rand, "Bitset Random" , ltest)
        call check(error, ltest)

    end subroutine test_bitsetl_ord_sorts

    subroutine test_bitsetl_ord_sort( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call ord_sort( bitsetl_dummy, bitsetl_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        bitsetl_dummy = a
        call ord_sort( bitsetl_dummy, bitsetl_work, reverse = .true. )

        call verify_bitsetl_reverse_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

        bitsetl_dummy = a
        call ord_sort( bitsetl_dummy, reverse = .true. )

        call verify_bitsetl_reverse_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

    end subroutine test_bitsetl_ord_sort

    subroutine test_bitset64_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical:: ltest

        call test_bitset64_ord_sort( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_ord_sort( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_ord_sort( bitset64_rand, "Bitset Random" , ltest)
        call check(error, ltest)

    end subroutine test_bitset64_ord_sorts

    subroutine test_bitset64_ord_sort( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call ord_sort( bitset64_dummy, bitset64_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        bitset64_dummy = a
        call ord_sort( bitset64_dummy, bitset64_work, reverse = .true. )

        call verify_bitset64_reverse_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

        bitset64_dummy = a
        call ord_sort( bitset64_dummy, reverse = .true. )

        call verify_bitset64_reverse_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

    end subroutine test_bitset64_ord_sort
#endif

    subroutine test_int_radix_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)       :: i
        integer, allocatable :: d1(:)
        logical              :: ltest

        call test_int_radix_sort( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_radix_sort( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        !triggered an issue in  insertion
        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        call sort( d1 )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_radix_sorts

    subroutine test_int_radix_sort( a, a_name, ltest )
        integer(int32), intent(in) :: a(:)
        character(*), intent(in)   :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            call system_clock( t0, rate )
            call radix_sort( dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "RADIX_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Radix_Sort", tdiff/rate

        ! reverse
        dummy = a
        call radix_sort( dummy, reverse = .true.)
        call verify_reverse_sort(dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse RADIX_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if

    end subroutine test_int_radix_sort

    subroutine test_real_radix_sort( a, a_name, ltest )
        real(sp), intent(in) :: a(:)
        character(*), intent(in)   :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            real_dummy = a
            call system_clock( t0, rate )
            call radix_sort( real_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_real_sort( real_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "RADIX_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2f12.5)') 'real_dummy(i-1:i) = ', real_dummy(i-1:i)
        end if
        write( lun, '("|         Real |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Radix_Sort", tdiff/rate

        ! reverse
        real_dummy = a
        call radix_sort( real_dummy, reverse = .true.)
        call verify_real_reverse_sort(real_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse RADIX_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2f12.5)') 'real_dummy(i-1:i) = ', real_dummy(i-1:i)
        end if

    end subroutine test_real_radix_sort

    subroutine test_real_radix_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical              :: ltest

        call test_real_radix_sort( rand_r32, "rand-real32", ltest )
        call check(error, ltest)
        if (allocated(error)) return
    end subroutine test_real_radix_sorts

    subroutine test_int_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)       :: i
        integer, allocatable :: d1(:)
        logical              :: ltest

        call test_int_sort( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        !triggered an issue in  insertion
        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        call sort( d1 )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sorts

    subroutine test_int_sort( a, a_name, ltest )
        integer(int32), intent(in) :: a(:)
        character(*), intent(in)   :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            call system_clock( t0, rate )
            call sort( dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort", tdiff/rate

        ! reverse
        dummy = a
        call sort( dummy, .true.)
        call verify_reverse_sort(dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a12, 2i7)') 'dummy(i-1:i) = ', dummy(i-1:i)
        end if

    end subroutine test_int_sort

    subroutine test_char_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sorts

    subroutine test_char_sort( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            call system_clock( t0, rate )
            call sort( char_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort", tdiff/rate

        !reverse
        char_dummy = a
        call sort( char_dummy, .true.)
        call verify_char_reverse_sort( char_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if

    end subroutine test_char_sort

    subroutine test_string_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sorts

    subroutine test_string_sort( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            call system_clock( t0, rate )
            call sort( string_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort", tdiff/rate

        ! reverse
        string_dummy = a
        call sort( string_dummy, .true.)
        call verify_string_reverse_sort(string_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if


    end subroutine test_string_sort

#if STDLIB_BITSETS
    subroutine test_bitsetl_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sorts

    subroutine test_bitsetl_sort( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call sort( bitsetl_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort", tdiff/rate

        ! reverse
        bitsetl_dummy = a
        call sort( bitsetl_dummy, .true.)
        call verify_bitsetl_reverse_sort(bitsetl_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
    end subroutine test_bitsetl_sort

    subroutine test_bitset64_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sorts

    subroutine test_bitset64_sort( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call sort( bitset64_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort", tdiff/rate

        ! reverse
        bitset64_dummy = a
        call sort( bitset64_dummy, .true.)
        call verify_bitset64_reverse_sort(bitset64_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
    end subroutine test_bitset64_sort
#endif

    subroutine test_int_sort_indexes_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int_index), allocatable         :: index(:)
        logical                     :: ltest

        call test_int_sort_index_default( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_default( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( index(size(d1)) )
        call sort_index( d1, index )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_indexes_default

    subroutine test_int_sort_index_default( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int64)                 :: i
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            call system_clock( t0, rate )
            call sort_index( dummy, index_default, work, iwork_default )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        dummy = a(index_default(0:size(a)-1))
        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(index_default(i-1:i)) = ', a(index_default(i-1:i))
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_Index", tdiff/rate

        dummy = a
        call sort_index( dummy, index_default, work, iwork_default, reverse=.true. )
        dummy = a(index_default(size(a)-1))
        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(index_default(i-1:i)) = ', a(index_default(i-1:i))
        end if

    end subroutine test_int_sort_index_default

    subroutine test_char_sort_indexes_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_index_default( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_index_default( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_index_default( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_indexes_default

    subroutine test_char_sort_index_default( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            call system_clock( t0, rate )

            call sort_index( char_dummy, index_default, char_work, iwork_default )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_char_sort_index_default

    subroutine test_string_sort_indexes_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_index_default( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_index_default( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_index_default( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_indexes_default

    subroutine test_string_sort_index_default( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            call system_clock( t0, rate )
            call sort_index( string_dummy, index_default, string_work, iwork_default )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_string_sort_index_default

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_indexes_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_index_default( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index_default( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index_default( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_indexes_default

    subroutine test_bitsetl_sort_index_default( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitsetl_dummy, index_default, bitsetl_work, iwork_default )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitsetl_sort_index_default

    subroutine test_bitset64_sort_indexes_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_index_default( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index_default( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index_default( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_indexes_default

    subroutine test_bitset64_sort_index_default( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitset64_dummy, index_default, bitset64_work, iwork_default )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitset64_sort_index_default
#endif
    subroutine test_int_sort_indexes_low(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int_index_low), allocatable         :: index(:)
        logical                     :: ltest

        call test_int_sort_index_low( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_index_low( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( index(size(d1)) )
        call sort_index( d1, index )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_indexes_low

    subroutine test_int_sort_index_low( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int64)                 :: i
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            call system_clock( t0, rate )
            call sort_index( dummy, index_low, work, iwork_low )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        dummy = a(index_low(0:size(a)-1))
        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(index_low(i-1:i)) = ', a(index_low(i-1:i))
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_Index", tdiff/rate

        dummy = a
        call sort_index( dummy, index_low, work, iwork_low, reverse=.true. )
        dummy = a(index_low(size(a)-1))
        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(index_low(i-1:i)) = ', a(index_low(i-1:i))
        end if

    end subroutine test_int_sort_index_low

    subroutine test_char_sort_indexes_low(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_index_low( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_index_low( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_index_low( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_indexes_low

    subroutine test_char_sort_index_low( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            call system_clock( t0, rate )

            call sort_index( char_dummy, index_low, char_work, iwork_low )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_char_sort_index_low

    subroutine test_string_sort_indexes_low(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_index_low( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_index_low( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_index_low( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_indexes_low

    subroutine test_string_sort_index_low( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            call system_clock( t0, rate )
            call sort_index( string_dummy, index_low, string_work, iwork_low )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_string_sort_index_low

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_indexes_low(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_index_low( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index_low( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index_low( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_indexes_low

    subroutine test_bitsetl_sort_index_low( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitsetl_dummy, index_low, bitsetl_work, iwork_low )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitsetl_sort_index_low

    subroutine test_bitset64_sort_indexes_low(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_index_low( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index_low( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index_low( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_indexes_low

    subroutine test_bitset64_sort_index_low( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitset64_dummy, index_low, bitset64_work, iwork_low )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitset64_sort_index_low
#endif

    subroutine test_int_sort_adjointes_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int8), allocatable         :: adjoint(:)
        logical                     :: ltest

        call test_int_sort_adjoint_int8( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int8( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( adjoint(size(d1)))
        adjoint = int([(i, i=1, size(d1))], kind=int8)
        call sort_adjoint( d1, adjoint )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_adjointes_int8

    subroutine test_int_sort_adjoint_int8( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int8)                         :: adjoint(size(a))
        integer(int8)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if

    end subroutine test_int_sort_adjoint_int8

    subroutine test_char_sort_adjointes_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_adjoint_int8( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int8( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int8( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_adjointes_int8

    subroutine test_char_sort_adjoint_int8( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int8)         :: adjoint(size(a))
        integer(int8)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
            call system_clock( t0, rate )

            call sort_adjoint( char_dummy, adjoint, char_work, iwork )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJ did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_char_sort_adjoint_int8

    subroutine test_string_sort_adjointes_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_adjoint_int8( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int8( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int8( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_adjointes_int8

    subroutine test_string_sort_adjoint_int8( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int8)         :: adjoint(size(a))
        integer(int8)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
            call system_clock( t0, rate )
            call sort_adjoint( string_dummy, adjoint, string_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_string_sort_adjoint_int8

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_adjointes_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_adjoint_int8( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int8( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int8( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_adjointes_int8

    subroutine test_bitsetl_sort_adjoint_int8( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int8)         :: adjoint(size(a))
        integer(int8)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
            call system_clock( t0, rate )
            call sort_adjoint( bitsetl_dummy, adjoint, bitsetl_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitsetl_sort_adjoint_int8

    subroutine test_bitset64_sort_adjointes_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_adjoint_int8( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int8( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int8( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_adjointes_int8

    subroutine test_bitset64_sort_adjoint_int8( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int8)         :: adjoint(size(a))
        integer(int8)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int8)
            call system_clock( t0, rate )
            call sort_adjoint( bitset64_dummy, adjoint, bitset64_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitset64_sort_adjoint_int8
#endif
    subroutine test_int_sort_adjointes_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int16), allocatable         :: adjoint(:)
        logical                     :: ltest

        call test_int_sort_adjoint_int16( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int16( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( adjoint(size(d1)))
        adjoint = int([(i, i=1, size(d1))], kind=int16)
        call sort_adjoint( d1, adjoint )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_adjointes_int16

    subroutine test_int_sort_adjoint_int16( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int16)                         :: adjoint(size(a))
        integer(int16)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if

    end subroutine test_int_sort_adjoint_int16

    subroutine test_char_sort_adjointes_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_adjoint_int16( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int16( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int16( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_adjointes_int16

    subroutine test_char_sort_adjoint_int16( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int16)         :: adjoint(size(a))
        integer(int16)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
            call system_clock( t0, rate )

            call sort_adjoint( char_dummy, adjoint, char_work, iwork )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJ did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_char_sort_adjoint_int16

    subroutine test_string_sort_adjointes_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_adjoint_int16( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int16( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int16( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_adjointes_int16

    subroutine test_string_sort_adjoint_int16( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int16)         :: adjoint(size(a))
        integer(int16)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
            call system_clock( t0, rate )
            call sort_adjoint( string_dummy, adjoint, string_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_string_sort_adjoint_int16

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_adjointes_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_adjoint_int16( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int16( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int16( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_adjointes_int16

    subroutine test_bitsetl_sort_adjoint_int16( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int16)         :: adjoint(size(a))
        integer(int16)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
            call system_clock( t0, rate )
            call sort_adjoint( bitsetl_dummy, adjoint, bitsetl_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitsetl_sort_adjoint_int16

    subroutine test_bitset64_sort_adjointes_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_adjoint_int16( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int16( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int16( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_adjointes_int16

    subroutine test_bitset64_sort_adjoint_int16( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int16)         :: adjoint(size(a))
        integer(int16)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int16)
            call system_clock( t0, rate )
            call sort_adjoint( bitset64_dummy, adjoint, bitset64_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitset64_sort_adjoint_int16
#endif
    subroutine test_int_sort_adjointes_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int32), allocatable         :: adjoint(:)
        logical                     :: ltest

        call test_int_sort_adjoint_int32( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int32( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( adjoint(size(d1)))
        adjoint = int([(i, i=1, size(d1))], kind=int32)
        call sort_adjoint( d1, adjoint )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_adjointes_int32

    subroutine test_int_sort_adjoint_int32( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int32)                         :: adjoint(size(a))
        integer(int32)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if

    end subroutine test_int_sort_adjoint_int32

    subroutine test_char_sort_adjointes_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_adjoint_int32( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int32( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int32( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_adjointes_int32

    subroutine test_char_sort_adjoint_int32( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int32)         :: adjoint(size(a))
        integer(int32)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
            call system_clock( t0, rate )

            call sort_adjoint( char_dummy, adjoint, char_work, iwork )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJ did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_char_sort_adjoint_int32

    subroutine test_string_sort_adjointes_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_adjoint_int32( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int32( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int32( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_adjointes_int32

    subroutine test_string_sort_adjoint_int32( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int32)         :: adjoint(size(a))
        integer(int32)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
            call system_clock( t0, rate )
            call sort_adjoint( string_dummy, adjoint, string_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_string_sort_adjoint_int32

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_adjointes_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_adjoint_int32( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int32( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int32( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_adjointes_int32

    subroutine test_bitsetl_sort_adjoint_int32( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int32)         :: adjoint(size(a))
        integer(int32)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
            call system_clock( t0, rate )
            call sort_adjoint( bitsetl_dummy, adjoint, bitsetl_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitsetl_sort_adjoint_int32

    subroutine test_bitset64_sort_adjointes_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_adjoint_int32( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int32( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int32( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_adjointes_int32

    subroutine test_bitset64_sort_adjoint_int32( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int32)         :: adjoint(size(a))
        integer(int32)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int32)
            call system_clock( t0, rate )
            call sort_adjoint( bitset64_dummy, adjoint, bitset64_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitset64_sort_adjoint_int32
#endif
    subroutine test_int_sort_adjointes_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int64)              :: i
        integer(int32), allocatable :: d1(:)
        integer(int64), allocatable         :: adjoint(:)
        logical                     :: ltest

        call test_int_sort_adjoint_int64( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_int_sort_adjoint_int64( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
        allocate( adjoint(size(d1)))
        adjoint = int([(i, i=1, size(d1))], kind=int64)
        call sort_adjoint( d1, adjoint )
        call verify_sort( d1, ltest, i )
        call check(error, ltest)

    end subroutine test_int_sort_adjointes_int64

    subroutine test_int_sort_adjoint_int64( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        integer(int64)                         :: adjoint(size(a))
        integer(int64)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        logical                        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if

    end subroutine test_int_sort_adjoint_int64

    subroutine test_char_sort_adjointes_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_char_sort_adjoint_int64( char_decrease, "Char. Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int64( char_increase, "Char. Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_char_sort_adjoint_int64( char_rand, "Char. Random", ltest )
        call check(error, ltest)

    end subroutine test_char_sort_adjointes_int64

    subroutine test_char_sort_adjoint_int64( a, a_name, ltest )
        character(len=4), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out)     :: ltest

        integer(int64)         :: adjoint(size(a))
        integer(int64)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            char_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
            call system_clock( t0, rate )

            call sort_adjoint( char_dummy, adjoint, char_work, iwork )

            call system_clock( t1, rate )

            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_char_sort( char_dummy, valid, i )

        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJ did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'char_dummy(i-1:i) = ', char_dummy(i-1:i)
        end if
        write( lun, '("|    Character |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            char_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_char_sort_adjoint_int64

    subroutine test_string_sort_adjointes_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_string_sort_adjoint_int64( string_decrease, "String Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int64( string_increase, "String Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_string_sort_adjoint_int64( string_rand, "String Random", ltest )
        call check(error, ltest)

    end subroutine test_string_sort_adjointes_int64

    subroutine test_string_sort_adjoint_int64( a, a_name, ltest )
        type(string_type), intent(in) :: a(0:)
        character(*), intent(in) :: a_name
        logical, intent(out) :: ltest

        integer(int64)         :: adjoint(size(a))
        integer(int64)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            string_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
            call system_clock( t0, rate )
            call sort_adjoint( string_dummy, adjoint, string_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_string_sort( string_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a17, 2(1x,a4))') 'string_dummy(i-1:i) = ', &
                string_dummy(i-1:i)
        end if
        write( lun, '("|  String_type |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            string_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_string_sort_adjoint_int64

#if STDLIB_BITSETS
    subroutine test_bitsetl_sort_adjointes_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_adjoint_int64( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int64( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_adjoint_int64( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_adjointes_int64

    subroutine test_bitsetl_sort_adjoint_int64( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64)         :: adjoint(size(a))
        integer(int64)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
            call system_clock( t0, rate )
            call sort_adjoint( bitsetl_dummy, adjoint, bitsetl_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitsetl_sort_adjoint_int64

    subroutine test_bitset64_sort_adjointes_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_adjoint_int64( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int64( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_adjoint_int64( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_adjointes_int64

    subroutine test_bitset64_sort_adjoint_int64( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64)         :: adjoint(size(a))
        integer(int64)         :: iwork(size(a))
        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i, j
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            adjoint = int([(j, j=1_int64, size(a, kind=int64))], kind=int64)
            call system_clock( t0, rate )
            call sort_adjoint( bitset64_dummy, adjoint, bitset64_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_adjoint", tdiff/rate

    end subroutine test_bitset64_sort_adjoint_int64
#endif

    subroutine test_real_sort_adjointes_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical                     :: ltest

        call test_real_sort_adjoint_sp( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_sp( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

    end subroutine test_real_sort_adjointes_sp

    subroutine test_real_sort_adjoint_sp( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        real(sp)                         :: adjoint(size(a))
        real(sp)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        integer(int64)                 :: i_adj
        logical                        :: valid
        logical                        :: valid_adj

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = real(dummy, kind=sp)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        call verify_adjoint(int(adjoint, kind=int32), dummy, valid_adj, i_adj )

        ltest = (ltest .and. valid .and. valid_adj)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        if ( .not. valid_adj ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i_adj = ', i_adj
            write(*,'(a18, 2i7)') 'a(i_adj-1:i_adj) = ', a(i_adj-1:i_adj)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = real(dummy, kind=sp)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        call verify_adjoint(int(adjoint, kind=int32), dummy, valid_adj, i_adj )
        ltest = (ltest .and. valid .and. valid_adj)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        if ( .not. valid_adj ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i_adj = ', i_adj
            write(*,'(a18, 2i7)') 'a(i_adj-1:i_adj) = ', a(i_adj-1:i_adj)
        end if

    end subroutine test_real_sort_adjoint_sp
    subroutine test_real_sort_adjointes_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical                     :: ltest

        call test_real_sort_adjoint_dp( blocks, "Blocks", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( decrease, "Decreasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( identical, "Identical", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( increase, "Increasing", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( rand1, "Random dense", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( rand2, "Random order", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( rand0, "Random sparse", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( rand3, "Random 3", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_real_sort_adjoint_dp( rand10, "Random 10", ltest )
        call check(error, ltest)
        if (allocated(error)) return

    end subroutine test_real_sort_adjointes_dp

    subroutine test_real_sort_adjoint_dp( a, a_name, ltest )
        integer(int32), intent(inout) :: a(:)
        character(*), intent(in)      :: a_name
        logical, intent(out)          :: ltest

        integer(int64)                 :: t0, t1, tdiff
        real(dp)                       :: rate
        real(dp)                         :: adjoint(size(a))
        real(dp)                         :: iwork(size(a))
        integer(int64)                 :: i, j
        integer(int64)                 :: i_adj
        logical                        :: valid
        logical                        :: valid_adj

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            dummy = a
            adjoint = real(dummy, kind=dp)
            call system_clock( t0, rate )
            call sort_adjoint( dummy, adjoint, work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_sort( dummy, valid, i )
        call verify_adjoint(int(adjoint, kind=int32), dummy, valid_adj, i_adj )

        ltest = (ltest .and. valid .and. valid_adj)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        if ( .not. valid_adj ) then
            write( *, * ) "SORT_ADJOINT did not sort " // a_name // "."
            write(*,*) 'i_adj = ', i_adj
            write(*,'(a18, 2i7)') 'a(i_adj-1:i_adj) = ', a(i_adj-1:i_adj)
        end if
        write( lun, '("|      Integer |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            test_size, a_name, "Sort_adjoint", tdiff/rate

        !reverse
        dummy = a
        adjoint = real(dummy, kind=dp)
        call sort_adjoint( dummy, adjoint, work, iwork, reverse=.true. )

        call verify_reverse_sort( dummy, valid, i )
        call verify_adjoint(int(adjoint, kind=int32), dummy, valid_adj, i_adj )
        ltest = (ltest .and. valid .and. valid_adj)
        if ( .not. valid ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i = ', i
            write(*,'(a18, 2i7)') 'a(i-1:i) = ', a(i-1:i)
        end if
        if ( .not. valid_adj ) then
            write( *, * ) "SORT_ADJOINT did not reverse sort " // &
                a_name // "."
            write(*,*) 'i_adj = ', i_adj
            write(*,'(a18, 2i7)') 'a(i_adj-1:i_adj) = ', a(i_adj-1:i_adj)
        end if

    end subroutine test_real_sort_adjoint_dp

    subroutine verify_sort( a, valid, i )
        integer(int32), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_sort

    subroutine verify_adjoint( a, true, valid, i )
        integer(int32), intent(in) :: a(:)
        integer(int32), intent(in) :: true(:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n
            if ( a(i) /= true(i) ) return
        end do
        valid = .true.

    end subroutine verify_adjoint

    subroutine verify_real_sort( a, valid, i )
        real(sp), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_real_sort

    subroutine verify_string_sort( a, valid, i )
        type(string_type), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_string_sort

#if STDLIB_BITSETS
    subroutine verify_bitsetl_sort( a, valid, i )
        type(bitset_large), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitsetl_sort

    subroutine verify_bitset64_sort( a, valid, i )
        type(bitset_64), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitset64_sort
#endif    

    subroutine verify_char_sort( a, valid, i )
        character(len=4), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_char_sort

    subroutine verify_char_reverse_sort( a, valid, i )
        character(len=4), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_char_reverse_sort

    subroutine verify_reverse_sort( a, valid, i )
        integer(int32), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_reverse_sort

    subroutine verify_real_reverse_sort( a, valid, i )
        real(sp), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_real_reverse_sort

    subroutine verify_string_reverse_sort( a, valid, i )
        type(string_type), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_string_reverse_sort

#if STDLIB_BITSETS
    subroutine verify_bitsetl_reverse_sort( a, valid, i )
        type(bitset_large), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitsetl_reverse_sort

    subroutine verify_bitset64_reverse_sort( a, valid, i )
        type(bitset_64), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitset64_reverse_sort
#endif
end module test_sorting


program tester
    use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit
    use testdrive, only: new_testsuite, run_testsuite, testsuite_type
    use test_sorting, only: initialize_tests, collect_sorting

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    call initialize_tests()

    stat = 0

    testsuites = [ &
        new_testsuite("sorting", collect_sorting) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
