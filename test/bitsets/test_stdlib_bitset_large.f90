module test_stdlib_bitset_large
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use :: stdlib_kinds, only : int8, int16, int32, int64
    use stdlib_bitsets, only: bitset_large, bits_kind&
                              , bits &
                              , success &
                              , and, and_not, or, xor&
                              , extract&
                              , assignment(=)&
                              , operator(<), operator(<=)&
                              , operator(>), operator(>=)&
                              , operator(/=), operator(==)
    implicit none
    character(*), parameter :: &
        bitstring_0   = '000000000000000000000000000000000', &
        bitstring_33  = '100000000000000000000000000000000', &
        bitstring_all = '111111111111111111111111111111111'

contains


    !> Collect all exported unit tests
    subroutine collect_stdlib_bitset_large(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("string-operations", test_string_operations), &
            new_unittest("io", test_io), &
            new_unittest("initialization", test_initialization), &
            new_unittest("bitset-assignment-array", test_assignment_array), &
            new_unittest("bitset-inquiry", test_bitset_inquiry), &
            new_unittest("bit-operations", test_bit_operations), &
            new_unittest("bitset-comparisons", test_bitset_comparisons), &
            new_unittest("bitset-operations", test_bitset_operations) &
            ]

    end subroutine collect_stdlib_bitset_large


    subroutine test_string_operations(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer            :: status
        character(:), allocatable :: string0
        type(bitset_large) :: set0, set1, set3, set4
        type(bitset_large) :: set10, set11, set13, set14

        call set0 % from_string( bitstring_0 )
        call check(error, bits(set0), 33, &
            'from_string failed to interpret bitstring_0 size properly.')
        if (allocated(error)) return
        call check(error, set0 % none(), &
            'failed to interpret bitstring_0 value properly.')
        if (allocated(error)) return
        call check(error, .not. set0 % any(), &
            'failed to interpret bitstring_0 value properly.')
        if (allocated(error)) return

        call set10 % from_string( bitstring_0 // bitstring_0 )
        call check(error, bits(set10), 66, &
            'from_string failed to interpret bitstring_0 // bitstring_0 size properly.')
        if (allocated(error)) return
        call check(error, set10 % none(), &
            'failed to interpret bitstring_0 // bitstring_0 value properly.')
        if (allocated(error)) return
        call check(error, .not. set10 % any(), &
            'failed to interpret bitstring_0 // bitstring_0 value properly.')
        if (allocated(error)) return

        call set1 % from_string( bitstring_all )
        call check(error, bits(set1), 33, &
            'from_string failed to interpret bitstring_all size properly.')
        if (allocated(error)) return
        call check(error, .not. set1 % none(), &
            'failed to interpret bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set1 % any(), &
            'failed to interpret bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set1 % all(), &
            'failed to interpret bitstring_all value properly.')
        if (allocated(error)) return

        call set11 % from_string( bitstring_all // bitstring_all  )
        call check(error, bits(set11), 66, &
            'from_string failed to interpret bitstring_all // bitstring_all size properly.')
        if (allocated(error)) return
        call check(error, .not. set11 % none(), &
            'failed to interpret bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set11 % any(), &
            'failed to interpret bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set11 % all(), &
            'failed to interpret bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return

        call set3 % read_bitset( bitstring_0, status )
        call check(error, status /= success, &
            'read_bitset_string did not fail with bitstring_0 as expected.')
        if (allocated(error)) return

        call set13 % read_bitset( bitstring_0 // bitstring_0, status )
        call check(error, status /= success, &
            'read_bitset_string did not fail with bitstring_0 // bitstring_0 as expected.')
        if (allocated(error)) return

        call set3 % read_bitset( 's33b' // bitstring_0, status )
        call check(error, bits(set3), 33, &
            'read_bitset_string failed to interpret "s33b" // bitstring_0 size properly.')
        if (allocated(error)) return
        call check(error, set3 % none(), &
            'failed to interpret "s33b" // bitstring_0 value properly.')
        if (allocated(error)) return

        call set13 % read_bitset( 's66b' // bitstring_0 // bitstring_0, &
            status )
        call check(error, bits(set13), 66, 'read_bitset_string failed to ' // &
            'interpret "s66b" // bitstring_0 // bitstring_0 size properly.')
        if (allocated(error)) return
        call check(error, set13 % none(), &
            'failed to interpret "s66b" // bitstring_0 // bitstring_0 value properly.')
        if (allocated(error)) return

        call set4 % read_bitset( 's33b' // bitstring_all )
        call check(error, bits(set4), 33, &
            'read_bitset_string failed to interpret "s33b" // bitstring_all size properly.')
        if (allocated(error)) return
        call check(error, .not. set4 % none(), &
            'read_bitset_string failed to interpret "s33b" // bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set4 % any(), &
            'read_bitset_string failed to // interpret "s33b" bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set4 % all(), &
            'read_bitset_string failed to // interpret "s33b" bitstring_all value properly.')
        if (allocated(error)) return

        call set14 % read_bitset( 's66b' // bitstring_all // bitstring_all )
        call check(error, bits(set14), 66, &
            'read_bitset_string failed to ' // &
            'interpret "s66b" // bitstring_all // bitstring_all size properly.')
        if (allocated(error)) return
        call check(error, .not. set14 % none(), 'read_bitset_string failed to ' // &
            'interpret "s66b" // bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set14 % any(), 'read_bitset_string failed to // ' // &
            'interpret "s66b" bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return
        call check(error, set14 % all(), 'read_bitset_string failed to // ' // &
            'interpret "s66b" bitstring_all // bitstring_all value properly.')
        if (allocated(error)) return

        call set0 % to_string( string0 )
        call check(error, bitstring_0, string0, &
            'to_string failed to convert set0 value properly.')
        if (allocated(error)) return

        call set10 % to_string( string0 )
        call check(error, bitstring_0 // bitstring_0, string0, &
            'to_string failed to convert set10 value properly.')
        if (allocated(error)) return

        call set1 % to_string( string0 )
        call check(error, bitstring_all, string0, &
            'to_string failed to convert set1 value properly.')
        if (allocated(error)) return

        call set11 % to_string( string0 )
        call check(error, bitstring_all // bitstring_all, string0, &
            'to_string failed to convert set11 value properly.')
        if (allocated(error)) return

        call set0 % write_bitset( string0 )
        call check(error, ('S33B' // bitstring_0), string0, &
            'write_bitset_string failed to convert set2 value properly.')
        if (allocated(error)) return

        call set10 % write_bitset( string0 )
        call check(error, ('S66B' // bitstring_0 // bitstring_0), string0, &
            'write_bitset_string failed to convert set10 value properly.')
        if (allocated(error)) return

        call set1 % write_bitset( string0 )
        call check(error, ('S33B' // bitstring_all), string0, &
            'write_bitset_string failed to convert set1 value properly.')
        if (allocated(error)) return

        call set11 % write_bitset( string0 )
        call check(error, ('S66B' // bitstring_all // bitstring_all), string0, &
            'write_bitset_string failed to convert set11 value properly.')
        if (allocated(error)) return

    end subroutine test_string_operations

    subroutine test_io(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: unit
        type(bitset_large) :: set0, set1, set2, set3, set4, set5
        type(bitset_large) :: set10, set11, set12, set13, set14, set15

        call set0 % from_string( bitstring_0 )
        call set1 % from_string( bitstring_all )
        call set2 % from_string( bitstring_33 )
        open( newunit=unit, status='scratch', form='formatted', &
              action='readwrite' )
        call set2 % write_bitset(unit)
        call set1 % write_bitset(unit)
        call set0 % write_bitset(unit)
        rewind( unit )
        call set3 % read_bitset(unit)
        call set5 % read_bitset(unit)
        call set4 % read_bitset(unit)
        call check(error, set4 == set0 .and. set5 == set1 .and. set3 == set2, &
            'transfer to and from units using bitset literals failed.')

        if (.not.allocated(error)) then
            rewind( unit )

            call set10 % from_string( bitstring_0 // bitstring_0 )
            call set11 % from_string( bitstring_all // bitstring_all  )
            call set12 % from_string( bitstring_33 // bitstring_33 )

            call set12 % write_bitset(unit)
            call set11 % write_bitset(unit)
            call set10 % write_bitset(unit)
            rewind( unit )
            call set13 % read_bitset(unit)
            call set15 % read_bitset(unit)
            call set14 % read_bitset(unit)
            call check(error, set14 == set10 .and. set15 == set11 .and. set3 == set12, &
                'transfer to and from units using bitset literals for bits > 64 failed.')
        end if
        if (.not.allocated(error)) then

            rewind( unit )

            call set2 % write_bitset(unit, advance='no')
            call set1 % write_bitset(unit, advance='no')
            call set0 % write_bitset(unit)
            rewind( unit )
            call set3 % read_bitset(unit, advance='no')
            call set4 % read_bitset(unit, advance='no')
            call set5 % read_bitset(unit)
            call check(error, set5 == set0 .and. set4 == set1 .and. set3 == set2, &
                'transfer to and from units using bitset literals with advance == "no" failed.')
        end if
        if (.not.allocated(error)) then

            rewind( unit )

            call set12 % write_bitset(unit, advance='no')
            call set11 % write_bitset(unit, advance='no')
            call set10 % write_bitset(unit)
            rewind( unit )
            call set13 % read_bitset(unit, advance='no')
            call set14 % read_bitset(unit, advance='no')
            call set15 % read_bitset(unit)
            call check(error, set15 == set10 .and. set14 == set11 .and. set13 == set12, &
                'transfer to and from units using  bitset literals for bitss > 64 with advance == "no" failed.')
        end if

        close(unit)
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', status='scratch', &
            action='readwrite' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        rewind( unit )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        call check(error, set3 == set0 .and. set4 == set1 .and. set5 == set2, &
            'transfer to and from units using output and input failed.')

        close( unit )
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', access='stream', &
            status='scratch', action='readwrite' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        rewind( unit )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        call check(error, set3 == set0 .and. set4 == set1 .and. set5 == set2, &
            'transfer to and from units using stream output and input failed.')

        close( unit )
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', status='scratch', &
            action='readwrite' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        rewind( unit )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        call check(error, set13 == set10 .and. set14 == set11 .and. set15 == set12, &
            'transfer to and from units using output and input failed for bits . 64.')
        close(unit)
        if (allocated(error)) return

        open( newunit=unit, form='unformatted', access='stream', &
            status='scratch', action='readwrite' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        rewind( unit )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        call check(error, set13 == set10 .and. set14 == set11 .and. set15 == set12, &
            'transfer to and from units using stream output and input failed for bits . 64.')
        close(unit)
        if (allocated(error)) return

    end subroutine test_io

    subroutine test_initialization(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        logical(int8)  :: log1(64) = .true.
        logical(int16) :: log2(31) = .false.
        logical(int32) :: log3(15) = .true.
        logical(int64) :: log4(33) = .false.
        logical(int8)  :: log11(66) = .true.
        logical(int16) :: log12(99) = .false.
        logical(int32) :: log13(132) = .true.
        logical(int64) :: log14(165) = .false.
        logical(int8), allocatable  :: log5(:)
        logical(int16), allocatable :: log6(:)
        logical(int32), allocatable :: log7(:)
        logical(int64), allocatable :: log8(:)
        type(bitset_large) :: set4, set5

        !The following triggers an issue in gfortran 11 and 12
        block
        type(bitset_large) :: set6
        call check(error, set6 % bits(), 0, &
            'set6 % bits() returned non-zero value '//&
            'even though set6 was not initialized.')
        if (allocated(error)) return
        end block

        set5 = log1
        call check(error, set5 % bits(), 64, &
            ' initialization with logical(int8) failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set5 % all(), &
            ' initialization with' // &
            ' logical(int8) failed to set the right values.')
        if (allocated(error)) return

        set5 = log11
        call check(error, set5 % bits(), 66, &
            ' initialization with logical(int8) failed to set' // &
            ' the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, set5 % all(), &
            ' initialization with' // &
            ' logical(int8) failed to set the right values.')
        if (allocated(error)) return

        set5 = log2
        call check(error, set5 % bits(), 31, &
            ' initialization with logical(int16) failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set5 % none(), &
            ' initialization with logical(int16) failed to set' // &
            ' the right values.')
        if (allocated(error)) return

        set5 = log12
        call check(error, set5 % bits(), 99, &
            ' initialization with logical(int16) failed to set' // &
            ' the right size > 64 bits .')
        if (allocated(error)) return
        call check(error, set5 % none(), &
            ' initialization with logical(int16) failed to set' // &
            ' the right values > 64 bits .')
        if (allocated(error)) return

        set5 = log3
        call check(error, set5 % bits(), 15, &
            ' initialization with logical(int32) failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set5 % all(), &
            ' initialization with logical(int32) failed to set' // &
            ' the right values.')
        if (allocated(error)) return

        set5 = log13
        call check(error, set5 % bits(), 132, &
            ' initialization with logical(int32) failed to set' // &
            ' the right size > 64 bits .')
        if (allocated(error)) return
        call check(error, set5 % all(), &
            ' initialization with logical(int32) failed to set' // &
            ' the right values > 64 bits .')
        if (allocated(error)) return

        set5 = log4
        call check(error, set5 % bits(), 33, &
            ' initialization with logical(int64) failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set5 % none(), &
            ' initialization with logical(int64) failed to set' // &
            ' the right values.')
        if (allocated(error)) return

        set5 = log14
        call check(error, set5 % bits(), 165, &
            ' initialization with logical(int64) failed to set' // &
            ' the right size > 64 bits .')
        if (allocated(error)) return
        call check(error, set5 % none(), &
            ' initialization with logical(int64) failed to set' // &
            ' the right values > 64 bits .')
        if (allocated(error)) return

        set5 = log1
        call extract( set4, set5, 1_bits_kind, 33_bits_kind )
        call check(error, set4 % bits(), 33, &
            ' initialization with extract failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set4 % all(), &
            ' initialization with extract failed to set' // &
            ' the right values.')
        if (allocated(error)) return

        set5 = log11
        call extract( set4, set5, 1_bits_kind, 65_bits_kind )
        call check(error, set4 % bits(), 65, &
            ' initialization with extract failed to set' // &
            ' the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, set4 % all(), &
            ' initialization with extract failed to set' // &
            ' the right values > 64 bits.')
        if (allocated(error)) return

        set5 = log1
        set4 = set5
        call check(error, set4 % bits(), 64, &
            ' initialization with simple assignment failed to set' // &
            ' the right size.')
        if (allocated(error)) return
        call check(error, set4 % all(), &
            ' initialization with simple assignment failed to set' // &
            ' the right values.')
        if (allocated(error)) return

        set5 = log11
        set4 = set5
        call check(error, set4 % bits(), 66, &
            ' initialization with simple assignment failed to set' // &
            ' the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, set4 % all(), &
            ' initialization with simple assignment failed to set' // &
            ' the right values > 64 bits.')
        if (allocated(error)) return

        set5 = log1
        log5 = set5
        call check(error, size(log5), 64, &
            ' initialization of logical(int8) with assignment failed' // &
            ' to set the right size.')
        if (allocated(error)) return
        call check(error, all(log5) .eqv. .true., &  ! FIXME
            ' initialization of logical(int8) with assignment failed' // &
            ' to set the right values.')
        if (allocated(error)) return

        set5 = log11
        log5 = set5
        call check(error, size(log5), 66, &
            ' initialization of logical(int8) with assignment failed' // &
            ' to set the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, all(log5) .eqv. .true., &  ! FIXME
            ' initialization of logical(int8) with assignment failed' // &
            ' to set the right values > 64 bits.')
        if (allocated(error)) return

        set5 = log1
        log6 = set5
        call check(error, size(log6), 64, &
            ' initialization of logical(int16) with assignment failed' // &
            ' to set the right size.')
        if (allocated(error)) return
        call check(error, all(log6) .eqv. .true., &  ! FIXME
            ' initialization of logical(int16) with assignment failed' // &
            ' to set the right values.')
        if (allocated(error)) return

        set5 = log11
        log6 = set5
        call check(error, size(log6), 66, &
            ' initialization of logical(int16) with assignment failed' // &
            ' to set the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, all(log6) .eqv. .true., &  ! FIXME
            ' initialization of logical(int16) with assignment failed' // &
            ' to set the right values > 64 bits.')
        if (allocated(error)) return

        set5 = log1
        log7 = set5
        call check(error, size(log7), 64, &
            ' initialization of logical(int32) with assignment failed' // &
            ' to set the right size.')
        if (allocated(error)) return
        call check(error, all(log7), &
            ' initialization of logical(int32) with assignment failed' // &
            ' to set the right values.')
        if (allocated(error)) return

        set5 = log11
        log7 = set5
        call check(error, size(log7), 66, &
            ' initialization of logical(int32) with assignment failed' // &
            ' to set the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, all(log7), &
            ' initialization of logical(int32) with assignment failed' // &
            ' to set the right values > 64 bits.')
        if (allocated(error)) return

        set5 = log1
        log8 = set5
        call check(error, size(log8), 64, &
            ' initialization of logical(int64) with assignment failed' // &
            ' to set the right size.')
        if (allocated(error)) return
        call check(error, merge(.true., .false., all(log8)), &  ! FIXME
            ' initialization of logical(int64) with assignment failed' // &
            ' to set the right values.')
        if (allocated(error)) return

        set5 = log11
        log8 = set5
        call check(error, size(log8), 66, &
            ' initialization of logical(int64) with assignment failed' // &
            ' to set the right size > 64 bits.')
        if (allocated(error)) return
        call check(error, merge(.true., .false., all(log8)), &  ! FIXME
            ' initialization of logical(int64) with assignment failed' // &
            ' to set the right values > 64 bits.')
        if (allocated(error)) return

    end subroutine test_initialization

    subroutine test_assignment_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        logical(int8)  :: log1(64) = .true.
        
        integer :: i
        type(bitset_large) :: set1(0:4)

        do i = 0, size(set1) - 1
           set1(i) = log1
        enddo

        do i = 0, size(set1) - 1
            call check(error, set1(i) % bits(), 64, &
                ' initialization with logical(int8) failed to set' // &
                ' the right size in a bitset array.')
            if (allocated(error)) return
        enddo

        !Test added following issue https://github.com/fortran-lang/stdlib/issues/726
        set1(0) = set1(0)

        call check(error, set1(0) % bits(), 64, &
            ' initialization from bitset_large failed to set' // &
            ' the right size in a bitset array.')
        if (allocated(error)) return

    end subroutine test_assignment_array

    subroutine test_bitset_inquiry(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(bits_kind) :: i
        type(bitset_large) :: set0, set1
        type(bitset_large) :: set10, set11

        call set0 % from_string( bitstring_0 )
        call set1 % from_string( bitstring_all )

        call check(error, set0 % none(), &
            ' set0 did not have none set which was unexpected')
        if (allocated(error)) return

        call check(error, .not. set0 % any(), &
            ' set0 had some bits set which was unexpected.')
        if (allocated(error)) return

        call set0 % not()

        call check(error, set0 % all(), &
            ' set0 did not have all bits set which was unexpected')
        if (allocated(error)) return

        call check(error, set0 % any(), &
            ' set0 had no bits set which was unexpected.')
        if (allocated(error)) return

        call check(error, set1 % any(), &
            ' set1 had none bits set which was unexpected')
        if (allocated(error)) return

        call check(error, set1 % all(), &
            ' set1 did not have all bits set which was unexpected.')
        if (allocated(error)) return

        call set0 % not()
        do i=0, set0 % bits() - 1
            call check(error, .not. set0 % test(i), &
                'against expectations set0 has at least 1 bit set.')
            if (allocated(error)) return
        end do

        do i=0, set1 % bits() - 1
            call check(error, set1 % test(i), &
                'against expectations set0 has at least 1 bit unset.')
            if (allocated(error)) return
        end do

        do i=0, set0 % bits() - 1
            call check(error, set0 % value(i), 0, &
                'against expectations set0 has at least 1 bit set.')
            if (allocated(error)) return
        end do

        do i=0, set1 % bits() - 1
            call check(error, set1 % value(i), 1, &
                'against expectations set0 has at least 1 bit unset.')
            if (allocated(error)) return
        end do

        call check(error, set0 % bits() == 33, &
            'set0 unexpectedly does not have 33 bits.')
        if (allocated(error)) return

! > 64 bit inquiries
        call set10 % from_string( bitstring_0 // bitstring_0 // bitstring_0 )
        call check(error, set10 % none(), &
            ' set10 did not have none set which was unexpected')
        if (allocated(error)) return

        call check(error, .not. set10 % any(), &
            ' set10 had some bits set which was unexpected.')
        if (allocated(error)) return

        call set10 % not()

        call check(error, set10 % all(), &
            ' set10 did not have all bits set which was unexpected')
        if (allocated(error)) return

        call check(error, set10 % any(), &
            ' set10 had no bits set which was unexpected.')
        if (allocated(error)) return

        call set11 % from_string( bitstring_all // bitstring_all // &
            bitstring_all )
        call check(error, set11 % any(), &
            ' set11 had none bits set which was unexpected')
        if (allocated(error)) return

        call check(error, set11 % all(), &
            ' set11 did not have all bits set which was unexpected.')
        if (allocated(error)) return

        call set10 % not()
        do i=0, set10 % bits() - 1
            call check(error, .not. set10 % test(i), &
                'against expectations set10 has at least 1 bit set.')
            if (allocated(error)) return
        end do

        do i=0, set11 % bits() - 1
            call check(error, set11 % test(i), &
                'against expectations set11 has at least 1 bit unset.')
            if (allocated(error)) return
        end do

        do i=0, set10 % bits() - 1
            call check(error, set10 % value(i), 0, &
                'against expectations set10 has at least 1 bit set.')
            if (allocated(error)) return
        end do

        do i=0, set11 % bits() - 1
            call check(error, set11 % value(i), 1, &
                'against expectations set11 has at least 1 bit unset.')
            if (allocated(error)) return
        end do

        call check(error, set0 % bits() == 33, &
            'set0 unexpectedly does not have 33 bits.')
        if (allocated(error)) return

        call check(error, set10 % bits() == 99, &
            'set10 unexpectedly does not have 99 bits.')
        if (allocated(error)) return

    end subroutine test_bitset_inquiry

    subroutine test_bit_operations(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_large) :: set1, set11

        call set1 % from_string( bitstring_all )

        call check(error, set1 % all(), &
            'set1 is not all set.')
        if (allocated(error)) return

        call set1 % clear(0_bits_kind)
        call check(error, .not. set1 % test(0_bits_kind), &
            'did not clear the first bit in set1.')
        if (allocated(error)) return

        call check(error, set1 % test(1_bits_kind), &
            'cleared more than one bit in set1.')
        if (allocated(error)) return

        call set1 % clear(1_bits_kind, 32_bits_kind)
        call check(error, set1 % none(), &
            'did not clear remaining bits in set1.')
        if (allocated(error)) return

        call set1 % flip(0_bits_kind)
        call check(error, set1 % test(0_bits_kind), &
            'did not flip the first bit in set1.')
        if (allocated(error)) return

        call check(error, .not. set1 % test(1_bits_kind), &
            'flipped more than one bit in set1.')
        if (allocated(error)) return

        call set1 % flip(1_bits_kind, 32_bits_kind)
        call check(error, set1 % all(), &
            'did not flip remaining bits in set1.')
        if (allocated(error)) return

        call set1 % not()
        call check(error, set1 % none(), &
            'did not unset bits in set1.')
        if (allocated(error)) return

        call set1 % set(0_bits_kind)
        call check(error, set1 % test(0_bits_kind), &
            'did not set the first bit in set1.')
        if (allocated(error)) return

        call check(error, .not. set1 % test(1_bits_kind), &
            'set more than one bit in set1.')
        if (allocated(error)) return

        call set1 % set(1_bits_kind, 32_bits_kind)
        call check(error, set1 % all(), &
            'did not set the remaining bits in set1.')
        if (allocated(error)) return

        call set11 % init( 166_bits_kind )
        call set11 % not()
        call check(error, set11 % all(), &
            'set11 is not all set.')
        if (allocated(error)) return

        call set11 % clear(0_bits_kind)
        call check(error, .not. set11 % test(0_bits_kind), &
            'did not clear the first bit in set11.')
        if (allocated(error)) return

        call check(error, set11 % test(1_bits_kind), &
            'cleared more than one bit in set11.')
        if (allocated(error)) return

        call set11 % clear(165_bits_kind)
        call check(error, .not. set11 % test(165_bits_kind), &
            'did not clear the last bit in set11.')
        if (allocated(error)) return

        call check(error, set11 % test(164_bits_kind), &
            'cleared more than one bit in set11.')
        if (allocated(error)) return

        call set11 % clear(1_bits_kind, 164_bits_kind)
        call check(error, set11 % none(), &
            'did not clear remaining bits in set11.')
        if (allocated(error)) return

        call set11 % flip(0_bits_kind)
        call check(error, set11 % test(0_bits_kind), &
            'did not flip the first bit in set11.')
        if (allocated(error)) return

        call check(error, .not. set11 % test(1_bits_kind), &
            'flipped more than one bit in set11.')
        if (allocated(error)) return

        call set11 % flip(165_bits_kind)
        call check(error, set11 % test(165_bits_kind), &
            'did not flip the last bit in set11.')
        if (allocated(error)) return

        call check(error, .not. set11 % test(164_bits_kind), &
            'flipped more than one bit in set11.')
        if (allocated(error)) return

        call set11 % flip(1_bits_kind, 164_bits_kind)
        call check(error, set11 % all(), &
            'did not flip remaining bits in set11.')
        if (allocated(error)) return

        call set11 % not()
        call check(error, set11 % none(), &
            'did not unset bits in set11.')
        if (allocated(error)) return

        call set11 % set(0_bits_kind)
        call check(error, set11 % test(0_bits_kind), &
            'did not set the first bit in set11.')
        if (allocated(error)) return

        call check(error, .not. set11 % test(1_bits_kind), &
            'set more than one bit in set11.')
        if (allocated(error)) return

        call set11 % set(165_bits_kind)
        call check(error, set11 % test(165_bits_kind), &
            'did not set the last bit in set11.')
        if (allocated(error)) return

        call check(error, .not. set11 % test(164_bits_kind), &
            'set more than one bit in set11.')
        if (allocated(error)) return

        call set11 % set(1_bits_kind, 164_bits_kind)
        call check(error, set11 % all(), &
            'did not set the remaining bits in set11.')
        if (allocated(error)) return

    end subroutine test_bit_operations

    subroutine test_bitset_comparisons(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_large) :: set0, set1, set2
        type(bitset_large) :: set10, set11, set12, set13, set14

        call set0 % from_string( bitstring_0 )
        call set1 % from_string( bitstring_all )
        call set2 % from_string( bitstring_33 )

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

        call set10 % init(166_bits_kind)
        call set11 % init(166_bits_kind)
        call set11 % not()
        call set12 % init(166_bits_kind)
        call set12 % set(165_bits_kind)
        call set13 % init(166_bits_kind)
        call set13 % set(65_bits_kind)
        call set14 % init(166_bits_kind)
        call set14 % set(0_bits_kind)
        call check(error, set10 == set10 .and. set11 == set11 .and. set12 == set12 .and. &
            set13 == set13 .and. set14 == set14 .and.                      &
            .not. set13 == set14 .and. .not. set12 == set13 .and.          &
            .not. set10 == set11 .and. .not. set10 == set12 .and. .not.    &
            set11 == set12, 'failed > 64 bit equality tests.')
        if (allocated(error)) return

        call check(error, set10 /= set11 .and. set11 /= set12 .and. set10 /= set12 .and. &
            set13 /= set12 .and. set14 /= set13 .and. set14 /= set12 .and. &
            .not. set13 /= set13 .and. .not. set12 /= set12 .and.          &
            .not. set10 /= set10 .and. .not. set11 /= set11 .and. .not.    &
            set12 /= set12, 'failed > 64 bit inequality tests.')
        if (allocated(error)) return

        call check(error, set11 > set10 .and. set12 > set10 .and. set11 > set12 .and. &
            set13 > set14 .and. set12 > set13 .and. set12 > set14 .and. &
            .not. set14 > set12 .and. .not. set12 > set11 .and.         &
            .not. set10 > set11 .and. .not. set11 > set11 .and. .not.   &
            set12 > set11, 'failed > 64 bit greater than tests.')
        if (allocated(error)) return

        call check(error, set11 >= set10 .and. set11 >= set12 .and. set12 >= set12 .and. &
            set13 >= set14 .and. set12 >= set13 .and. set12 >= set14 .and. &
            .not. set14 >= set12 .and. .not. set12 >= set11 .and.          &
            .not. set10 >= set11 .and. .not. set10 >= set11 .and. .not.    &
            set12 >= set11, 'failed 64 bit greater than or equal tests.')
        if (allocated(error)) return

        call check(error, set10 < set11 .and. set10 < set11 .and. set12 < set11 .and. &
            set14 < set13 .and. set13 < set12 .and. set14 < set12 .and. &
            .not. set12 < set14 .and. .not. set11 < set12 .and.         &
            .not. set11 < set10 .and. .not. set10 < set10 .and. .not.    &
            set11 < set12, 'failed > 64 bit less than tests.')
        if (allocated(error)) return

        call check(error, set10 <= set11 .and. set12 <= set11 .and. set12 <= set12 .and. &
            set14 <= set13 .and. set13 <= set12 .and. set14 <= set12 .and. &
            .not. set12 <= set14 .and. .not. set11 <= set12 .and.          &
            .not. set11 <= set10 .and. .not. set12 <= set10 .and. .not.     &
            set11 <= set12, 'failed > 64 bit less than or equal tests.')
        if (allocated(error)) return

    end subroutine test_bitset_comparisons

    subroutine test_bitset_operations(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(bitset_large) :: set0, set3, set4

        call set0 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call and( set0, set4 ) ! all all
        call check(error, set0 % all(), 'first test of < 64 bit AND failed.')
        if (allocated(error)) return

        call set4 % from_string( bitstring_0 )
        call and( set0, set4 ) ! all none
        call check(error, set0 % none(), 'second test of < 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none all
        call check(error, set4 % none(), 'third test of < 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none none
        call check(error, set4 % none(), 'fourth test of < 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call and_not( set4, set3 ) ! all all
        call check(error, set4 % none(), 'first test of < 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set4 % from_string( bitstring_0 )
        call and_not( set4, set3 ) ! none all
        call check(error, set4 % none(), 'second test of < 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! all none
        call check(error, set3 % all(), 'third  test of < 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! none none
        call check(error, set3 % none(), 'fourth  test of < 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call or( set3, set4 ) ! all all
        call check(error, set3 % all(), 'first test of < 64 bit OR failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_0 )
        call or( set4, set3 ) ! all none
        call check(error, set4 % all(), 'second test of < 64 bit OR failed.')
        if (allocated(error)) return

        call or( set3, set4 ) ! none all
        call check(error, set3 % all(), 'third test of < 64 bit OR failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call or( set4, set3 ) !none none
        call check(error, set4 % none(), 'fourth test of < 64 bit OR failed.')
        if (allocated(error)) return

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! none none
        call check(error, set3 % none(), 'first test of < 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! none all
        call check(error, set3 % all(), 'second test of < 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! all none
        call check(error, set3 % all(), 'third test of < 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! all all
        call check(error, set3 % none(), 'fourth test of < 64 bit XOR failed.')
        if (allocated(error)) return

        call set0 % init(166_bits_kind)
        call set0 % not()
        call set4 % init(166_bits_kind)
        call set4 % not()
        call and( set0, set4 ) ! all all
        call check(error, set0 % all(), 'first test of > 64 bit AND failed.')
        if (allocated(error)) return

        call set4 % init(166_bits_kind)
        call and( set0, set4 ) ! all none
        call check(error, set0 % none(), 'second test of > 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % init(166_bits_kind)
        call set3 % not()
        call and( set4, set3 ) ! none all
        call check(error, set4 % none(), 'third test of > 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % init(166_bits_kind)
        call and( set4, set3 ) ! none none
        call check(error, set4 % none(), 'fourth test of > 64 bit AND failed.')
        if (allocated(error)) return

        call set3 % not()
        call set4 % not()
        call and_not( set4, set3 ) ! all all
        call check(error, set4 % none(), 'first test of > 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call and_not( set4, set3 ) ! none all
        call check(error, set4 % none(), 'second test of > 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call and_not( set3, set4 ) ! all none
        call check(error, set3 % all(), 'third  test of > 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set3 % not()
        call and_not( set3, set4 ) ! none none
        call check(error, set3 % none(), 'fourth  test of > 64 bit AND_NOT failed.')
        if (allocated(error)) return

        call set3 % init(166_bits_kind)
        call set3 % not()
        call set4 % init(166_bits_kind)
        call set4 % not()
        call or( set3, set4 ) ! all all
        call check(error, set3 % all(), 'first test of > 64 bit OR failed.')
        if (allocated(error)) return

        call set3 % init(166_bits_kind)
        call or( set4, set3 ) ! all none
        call check(error, set4 % all(), 'second test of > 64 bit OR failed.')
        if (allocated(error)) return

        call or( set3, set4 ) ! none all
        call check(error, set3 % all(), 'third test of > 64 bit OR failed.')
        if (allocated(error)) return

        call set3 % init(166_bits_kind)
        call set4 % init(166_bits_kind)
        call or( set4, set3 ) !none none
        call check(error, set4 % none(), 'fourth test of > 64 bit OR failed.')
        if (allocated(error)) return

        call xor( set3, set4 ) ! none none
        call check(error, set3 % none(), 'first test of > 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % not()
        call xor( set3, set4 ) ! none all
        call check(error, set3 % all(), 'second test of > 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % not()
        call xor( set3, set4 ) ! all none
        call check(error, set3 % all(), 'third test of > 64 bit XOR failed.')
        if (allocated(error)) return

        call set4 % not()
        call xor( set3, set4 ) ! all all
        call check(error, set3 % none(), 'fourth test of > 64 bit XOR failed.')
        if (allocated(error)) return

    end subroutine test_bitset_operations


end module test_stdlib_bitset_large


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stdlib_bitset_large, only : collect_stdlib_bitset_large
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stdlib-bitset-large", collect_stdlib_bitset_large) &
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
