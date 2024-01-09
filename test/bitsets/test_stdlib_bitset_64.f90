module test_stdlib_bitset_64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use :: stdlib_kinds, only : int8, int16, int32, int64
    use stdlib_bitsets
    implicit none
    private

    public :: collect_stdlib_bitset_64

    character(*), parameter :: &
        bitstring_0   = '000000000000000000000000000000000', &
        bitstring_33  = '100000000000000000000000000000000', &
        bitstring_all = '111111111111111111111111111111111'

contains

    !> Collect all exported unit tests
    subroutine collect_stdlib_bitset_64(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("string-operations-0", test_string_operations_0), &
            new_unittest("string-operations-1", test_string_operations_1), &
            new_unittest("string-operations-3", test_string_operations_3), &
            new_unittest("string-operations-4", test_string_operations_4), &
            new_unittest("io", test_io), &
            new_unittest("initialization", test_initialization), &
            new_unittest("bitset-inquiry", test_bitset_inquiry), &
            new_unittest("bit-operations", test_bit_operations), &
            new_unittest("bitset-comparisons", test_bitset_comparisons), &
            new_unittest("bitset-operations-and", test_bitset_operations_and), &
            new_unittest("bitset-operations-nand", test_bitset_operations_nand), &
            new_unittest("bitset-operations-or", test_bitset_operations_or), &
            new_unittest("bitset-operations-xor", test_bitset_operations_xor) &
            ]

    end subroutine collect_stdlib_bitset_64

    subroutine test_string_operations_0(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set
        character(:), allocatable :: string0

        call set%from_string(bitstring_0)

        call check(error, bits(set), 33)
        if (allocated(error)) return

        call check(error, set%none())
        if (allocated(error)) return

        call check(error, .not.set%any())
        if (allocated(error)) return

        call set%to_string(string0)

        call check(error, string0, bitstring_0)
        if (allocated(error)) return

        call set%write_bitset(string0)

        call check(error, string0, ('S33B' // bitstring_0))
        if (allocated(error)) return

    end subroutine test_string_operations_0

    subroutine test_string_operations_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set
        character(:), allocatable :: string0

        call set%from_string(bitstring_all)

        call check(error, bits(set), 33)
        if (allocated(error)) return

        call check(error, .not.set%none() )
        if (allocated(error)) return

        call check(error, set%any() )
        if (allocated(error)) return

        call check(error, set%all() )
        if (allocated(error)) return

        call set%to_string(string0)

        call check(error, string0, bitstring_all)
        if (allocated(error)) return

        call set%write_bitset(string0)

        call check(error, string0, ('S33B' // bitstring_all))
        if (allocated(error)) return

    end subroutine test_string_operations_1

    subroutine test_string_operations_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set
        integer         :: status

        call set%read_bitset(bitstring_0, status)

        call check(error, status /= success)
        if (allocated(error)) return

        call set%read_bitset('s33b' // bitstring_0, status)

        call check(error, bits(set), 33)
        if (allocated(error)) return

        call check(error, set%none())
        if (allocated(error)) return

    end subroutine test_string_operations_3

    subroutine test_string_operations_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set

        call set%read_bitset('s33b' // bitstring_all )

        call check(error, bits(set), 33)
        if (allocated(error)) return

        call check(error, .not.set%none())
        if (allocated(error)) return

        call check(error, set%any())
        if (allocated(error)) return

        call check(error, set%all())
        if (allocated(error)) return

    end subroutine test_string_operations_4

    subroutine test_io(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: unit
        type(bitset_64) :: set0, set1, set2, set3, set4, set5

        call set0%from_string(bitstring_0)
        call set1%from_string(bitstring_all)
        call set2%from_string(bitstring_33)

        open( newunit=unit, status='scratch', form='formatted', &
              action='readwrite' )
        call set2%write_bitset(unit)
        call set1%write_bitset(unit)
        call set0%write_bitset(unit)
        rewind( unit )
        call set3%read_bitset(unit)
        call set5%read_bitset(unit)
        call set4%read_bitset(unit)

        call check(error, set4 == set0 .and. set5 == set1 .and. set3 == set2, &
            'transfer to and from units using bitset literals failed.')
        if (.not.allocated(error)) then
            rewind( unit )

            call set2%write_bitset(unit, advance='no')
            call set1%write_bitset(unit, advance='no')
            call set0%write_bitset(unit)
            rewind( unit )
            call set3%read_bitset(unit, advance='no')
            call set4%read_bitset(unit, advance='no')
            call set5%read_bitset(unit)

            call check(error, set5 == set0 .and. set4 == set1 .and. set3 == set2, &
                'transfer to and from units using bitset literals with advance="no" failed.')
        end if
        close(unit)
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', status='scratch', &
              action='readwrite' )
        call set2%output(unit)
        call set1%output(unit)
        call set0%output(unit)
        rewind( unit )
        call set5%input(unit)
        call set4%input(unit)
        call set3%input(unit)
        close( unit )

        call check(error, set3 == set0 .and. set4 == set1 .and. set5 == set2, &
            'transfer to and from units using output and input failed.')
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', access='stream', &
              status='scratch', action='readwrite' )
        call set2%output(unit)
        call set1%output(unit)
        call set0%output(unit)
        rewind( unit )
        call set5%input(unit)
        call set4%input(unit)
        call set3%input(unit)
        close( unit )

        call check(error, set3 == set0 .and. set4 == set1 .and. set5 == set2, &
            'transfer to and from units using stream output and input failed.')
        if (allocated(error)) return

    end subroutine test_io

    subroutine test_initialization(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        logical(int8)  :: log1(64) = .true.
        logical(int16) :: log2(31) = .false.
        logical(int32) :: log3(15) = .true.
        logical(int64) :: log4(33) = .false.
        logical(int8), allocatable  :: log5(:)
        logical(int16), allocatable :: log6(:)
        logical(int32), allocatable :: log7(:)
        logical(int64), allocatable :: log8(:)

        type(bitset_64) :: set4, set5

        !The following block triggers an issue in gfortran 11 and 12
        block
        type(bitset_64) :: set6
        call check(error, set6 % bits(), 0, &
            'set6 % bits() returned non-zero value '//&
            'even though set6 was not initialized.')
        if (allocated(error)) return
        end block

        set5 = log1
        call check(error, set5%bits(), 64, &
            'initialization with logical(int8) failed to set the right size.')
        if (allocated(error)) return
        call check(error, set5%all(), &
            'initialization with logical(int8) failed to set the right values.')
        if (allocated(error)) return

        set5 = log2
        call check(error, set5%bits(), 31, &
            'initialization with logical(int16) failed to set the right size.')
        if (allocated(error)) return
        call check(error, set5%none(), &
            'initialization with logical(int16) failed to set the right values.')
        if (allocated(error)) return

        set5 = log3
        call check(error, set5%bits(), 15, &
            'initialization with logical(int32) failed to set the right size.')
        if (allocated(error)) return
        call check(error, set5%all(), &
            'initialization with logical(int32) failed to set the right values.')
        if (allocated(error)) return

        set5 = log4
        call check(error, set5%bits(), 33, &
            'initialization with logical(int64) failed to set the right size.')
        if (allocated(error)) return
        call check(error, set5%none(), &
            'initialization with logical(int64) failed to set the right values.')
        if (allocated(error)) return

        set5 = log1
        call extract( set4, set5, 1_bits_kind, 33_bits_kind )
        call check(error, set4%bits(), 33, &
            'initialization with extract failed to set the right size.')
        if (allocated(error)) return
        call check(error, set4%all(), &
            'initialization with extract failed to set the right values.')
        if (allocated(error)) return

        set4 = set5
        call check(error, set4%bits(), 64, &
            'initialization with simple assignment failed to set the right size.')
        if (allocated(error)) return
        call check(error, set4%all(), &
            'initialization with simple assignment failed to set the right values.')
        if (allocated(error)) return

        log5 = set5
        call check(error, size(log5), 64, &
            'initialization of logical(int8) with assignment failed to set the right size.')
        if (allocated(error)) return
        call check(error, all(log5) .eqv. .true., &  ! FIXME
            'initialization of logical(int8) with assignment failed to set the right values.')
        if (allocated(error)) return

        log6 = set5
        call check(error, size(log6), 64, &
            'initialization of logical(int16) with assignment failed to set the right size.')
        if (allocated(error)) return
        call check(error, all(log6) .eqv. .true., &  ! FIXME
            'initialization of logical(int16) with assignment failed to set the right values.')
        if (allocated(error)) return

        log7 = set5
        call check(error, size(log7), 64, &
            'initialization of logical(int32) with assignment failed to set the right size.')
        if (allocated(error)) return
        call check(error, all(log7), &
            'initialization of logical(int32) with assignment failed to set the right values.')
        if (allocated(error)) return

        log8 = set5
        call check(error, size(log8), 64, &
            'initialization of logical(int64) with assignment failed to set the right size.')
        if (allocated(error)) return
        call check(error, merge(.true., .false., all(log8)), &  ! FIXME
            'initialization of logical(int64) with assignment failed to set the right values.')
        if (allocated(error)) return

    end subroutine test_initialization

    subroutine test_bitset_inquiry(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(bits_kind) :: i

        type(bitset_64) :: set0, set1

        call set0%from_string(bitstring_0)
        call set1%from_string(bitstring_all)

        call check(error, set0%none(), 'set0 did not have none set which ' // &
            'was unexpected')
        if (allocated(error)) return
        call check(error, .not. set0%any(), 'set0 had some bits set which ' // &
            'was unexpected.')
        if (allocated(error)) return

        call set0%not()
        call check(error, set0%all(), 'set0 did not have all bits set ' // &
            'which was unexpected')
        if (allocated(error)) return
        call check(error, set0%any(), 'set0 had no bits set which ' // &
            'was unexpected.')
        if (allocated(error)) return

        call check(error, set1%any(), 'set1 had no bits set ' // &
            'which was unexpected')
        if (allocated(error)) return
        call check(error, set1%all(), 'set1 did not have all bits set ' // &
            'which was unexpected.')
        if (allocated(error)) return

        call set0%not()
        do i=0, set0%bits() - 1
            call check(error, .not. set0%test(i), &
                'against expectations set0 has at least 1 bit set.')
        end do

        do i=0, set1%bits() - 1
            call check(error, set1%test(i), &
                'against expectations set1 has at least 1 bit unset.')
        end do

        do i=0, set0%bits() - 1
            call check(error, .not.( set0%value(i) /= 0), &
                'against expectations set0 has at least 1 bit set.')
        end do

        do i=0, set1%bits() - 1
            call check(error, .not.( set1%value(i) /= 1), &
                'against expectations set1 has at least 1 bit unset.')
        end do

        call check(error, set0%bits() == 33, 'et0 unexpectedly does not have 33 bits.')
        if (allocated(error)) return

    end subroutine test_bitset_inquiry

    subroutine test_bit_operations(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set1

        call set1%from_string(bitstring_all)

        call check(error, set1%all(), 'set1 is not all set.')
        if (allocated(error)) return

        call set1%clear(0_bits_kind)
        call check(error, .not. set1%test(0_bits_kind), 'did not clear the first bit in set1.')
        if (allocated(error)) return
        call check(error, set1%test(1_bits_kind), 'cleared more than one bit in set1.')
        if (allocated(error)) return

        call set1%clear(1_bits_kind, 32_bits_kind)
        call check(error, set1%none(), 'did not clear remaining bits in set1.')
        if (allocated(error)) return

        call set1%flip(0_bits_kind)
        call check(error, set1%test(0_bits_kind), 'did not flip the first bit in set1.')
        if (allocated(error)) return
        call check(error, .not. set1%test(1_bits_kind), 'flipped more than one bit in set1.')
        if (allocated(error)) return

        call set1%flip(1_bits_kind, 32_bits_kind)
        call check(error, set1%all(), 'did not flip remaining bits in set1.')
        if (allocated(error)) return

        call set1%not()
        call check(error, set1%none(), 'did not unset bits in set1.')
        if (allocated(error)) return

        call set1%set(0_bits_kind)
        call check(error, set1%test(0_bits_kind), 'did not set the first bit in set1.')
        if (allocated(error)) return
        call check(error, .not. set1%test(1_bits_kind), 'set more than one bit in set1.')
        if (allocated(error)) return

        call set1%set(1_bits_kind, 32_bits_kind)
        call check(error, set1%all(), 'did not set the remaining bits in set1.')
        if (allocated(error)) return

    end subroutine test_bit_operations

    subroutine test_bitset_comparisons(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set0, set1, set2

        call set0%from_string(bitstring_0)
        call set1%from_string(bitstring_all)
        call set2%from_string(bitstring_33)

        call check(error, set0 == set0 .and. set1 == set1 .and. set2 == set2 .and. &
            .not. set0 == set1 .and. .not. set0 == set2 .and. .not.   &
            set1 == set2, 'failed 64 bit equality tests.')
        if (allocated(error)) return

        call check(error, set0 /= set1 .and. set1 /= set2 .and. set0 /= set2 .and. &
            .not. set0 /= set0 .and. .not. set1 /= set1 .and. .not.   &
            set2 /= set2, 'failed 64 bit inequality tests.')
        if (allocated(error)) return

        call check(error, set1 > set0 .and. set2 > set0 .and. set1 > set2 .and. &
            .not. set0 > set1 .and. .not. set1 > set1 .and. .not.  &
            set2 > set1, 'failed 64 bit greater than tests.')
        if (allocated(error)) return

        call check(error, set1 >= set0 .and. set1 >= set2 .and. set2 >= set2 .and. &
            .not. set0 >= set1 .and. .not. set0 >= set1 .and. .not.  &
            set2 >= set1, 'failed 64 bit greater than or equal tests.')
        if (allocated(error)) return

        call check(error, set0 < set1 .and. set0 < set1 .and. set2 < set1 .and. &
            .not. set1 < set0 .and. .not. set0 < set0 .and. .not.  &
            set1 < set2, 'failed 64 bit less than tests.')
        if (allocated(error)) return

        call check(error, set0 <= set1 .and. set2 <= set1 .and. set2 <= set2 .and. &
            .not. set1 <= set0 .and. .not. set2 <= set0 .and. .not.  &
            set1 <= set2, 'failed 64 bit less than or equal tests.')
        if (allocated(error)) return

    end subroutine test_bitset_comparisons

    subroutine test_bitset_operations_and(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set3, set4, set0

        call set0%from_string( bitstring_all )
        call set4%from_string( bitstring_all )
        call and( set0, set4 ) ! all all
        call check(error, set0%all(), 'first test of AND failed.')
        if (allocated(error)) return

        call set4%from_string( bitstring_0 )
        call set3%from_string( bitstring_all )
        call and( set3, set4 ) ! all none
        call check(error, set3%none(), 'second test of AND failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_all )
        call set4%from_string( bitstring_0 )
        call and( set4, set3 ) ! none all
        call check(error, set4%none(), 'third test of AND failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_0 )
        call and( set4, set3 ) ! none none
        call check(error, set4%none(), 'fourth test of AND failed.')
        if (allocated(error)) return

    end subroutine test_bitset_operations_and

    subroutine test_bitset_operations_nand(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set3, set4

        call set3%from_string( bitstring_all )
        call set4%from_string( bitstring_all )
        call and_not( set4, set3 ) ! all all
        call check(error, set4%none(), 'first test of AND_NOT failed.')
        if (allocated(error)) return

        call set4%from_string( bitstring_0 )
        call and_not( set4, set3 ) ! none all
        call check(error, set4%none(), 'second test of AND_NOT failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_all )
        call set4%from_string( bitstring_0 )
        call and_not( set3, set4 ) ! all none
        call check(error, set3%all(), 'third  test of AND_NOT failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_0 )
        call set4%from_string( bitstring_0 )
        call and_not( set3, set4 ) ! none none
        call check(error, set3%none(), 'fourth  test of AND_NOT failed.')
        if (allocated(error)) return

    end subroutine test_bitset_operations_nand

    subroutine test_bitset_operations_or(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set3, set4

        call set3%from_string( bitstring_all )
        call set4%from_string( bitstring_all )
        call or( set3, set4 ) ! all all
        call check(error, set3%all(), 'first test of OR failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_0 )
        call or( set4, set3 ) ! all none
        call check(error, set4%all(), 'second test of OR failed.')
        if (allocated(error)) return

        call or( set3, set4 ) ! none all
        call check(error, set3%all(), 'third test of OR failed.')
        if (allocated(error)) return

        call set3%from_string( bitstring_0 )
        call set4%from_string( bitstring_0 )
        call or( set4, set3 ) !none none
        call check(error, set4%none(), 'fourth test of OR failed.')
        if (allocated(error)) return

    end subroutine test_bitset_operations_or

    subroutine test_bitset_operations_xor(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_64) :: set3, set4

        call set3%from_string( bitstring_0 )
        call set4%from_string( bitstring_0 )
        call xor( set3, set4 ) ! none none
        call check(error, set3%none(), 'first test of XOR failed.')
        if (allocated(error)) return

        call set4%from_string( bitstring_all )
        call xor( set3, set4 ) ! none all
        call check(error, set3%all(), 'second test of XOR failed.')
        if (allocated(error)) return

        call set4%from_string( bitstring_0 )
        call xor( set3, set4 ) ! all none
        call check(error, set3%all(), 'third test of XOR failed.')
        if (allocated(error)) return

        call set4%from_string( bitstring_all )
        call xor( set3, set4 ) ! all all
        call check(error, set3%none(), 'fourth test of XOR failed.')
        if (allocated(error)) return

    end subroutine test_bitset_operations_xor


end module test_stdlib_bitset_64


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stdlib_bitset_64, only : collect_stdlib_bitset_64
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stdlib-bitset-64", collect_stdlib_bitset_64) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
