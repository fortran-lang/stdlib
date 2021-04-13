program test_stdlib_bitset_64
    use :: stdlib_kinds, only : int8, int16, int32, int64
    use stdlib_bitsets
    character(*), parameter :: &
        bitstring_0   = '000000000000000000000000000000000', &
        bitstring_33  = '100000000000000000000000000000000', &
        bitstring_all = '111111111111111111111111111111111'
    type(bitset_64) :: set0, set1, set2, set3, set4, set5
    integer         :: status
    character(:), allocatable :: string0

    call test_string_operations()

    call test_io()

    call test_initialization()

    call test_bitset_inquiry()

    call test_bit_operations()

    call test_bitset_comparisons()

    call test_bitset_operations()

contains

    subroutine test_string_operations()
        character(*), parameter:: procedure = 'TEST_STRING_OPERATIONS'

        write(*,'(/a)') 'Test string operations: from_string, ' // &
            'read_bitset, to_string, and write_bitset'

        call set0 % from_string( bitstring_0 )
        if ( bits(set0) /= 33 ) then
            error stop procedure // ' from_string failed to interpret ' // &
                'bitstring_0 size properly.'
        else if ( .not. set0 % none() ) then
            error stop procedure // ' failed to interpret bitstring_0 ' // &
                'value properly.'
        else if ( set0 % any() ) then
            error stop procedure // ' failed to interpret bitstring_0 ' // &
                'value properly.'
        else
            write(*,*) 'from_string transferred bitstring_0 properly into set0'
        end if

        call set1 % from_string( bitstring_all )
        if ( bits(set1) /= 33 ) then
            error stop procedure // ' from_string failed to interpret ' // &
                'bitstring_all size properly.'
        else if ( set1 % none() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                'value properly.'
        else if ( .not. set1 % any() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                'value properly.'
        else if ( .not. set1 % all() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                'value properly.'
        else
            write(*,*) 'from_string transferred bitstring_all properly ' // &
                'into set1'
        end if

        call set3 % read_bitset( bitstring_0, status )
        if ( status /= success ) then
            write(*,*) 'read_bitset_string failed with bitstring_0 as expected.'
        else
            error stop procedure // ' read_bitset_string did not fail ' // &
                'with bitstring_0 as expected.'
        end if

        call set3 % read_bitset( 's33b' // bitstring_0, status )

        if ( bits(set3) /= 33 ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s33b" // bitstring_0 size properly.'
        else if ( .not. set3 % none() ) then
            error stop procedure // ' failed to interpret "s33b" // ' // &
                'bitstring_0 value properly.'
        else
            write(*,*) 'read_bitset_string transferred "s33b" // ' // &
                'bitstring_0 properly into set3'
        end if

        call set4 % read_bitset( 's33b' // bitstring_all )
        if ( bits(set4) /= 33 ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s33b" // bitstring_all size properly.'
        else if ( set4 % none() ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s33b" // bitstring_all value properly.'
        else if ( .not. set4 % any() ) then
            error stop procedure // ' read_bitset_string failed to // ' // &
                'interpret "s33b" bitstring_all value properly.'
        else if ( .not. set4 % all() ) then
            error stop procedure // ' read_bitset_string failed to // ' // &
                'interpret "s33b" bitstring_all value properly.'
        else
            write(*,*) 'read_bitset_string transferred "s33b" // ' // &
                'bitstring_all properly into set4.'
        end if

        call set0 % to_string( string0 )
        if ( bitstring_0 /= string0 ) then
            error stop procedure // ' to_string failed to convert set0 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set0 value'
        end if

        call set1 % to_string( string0 )
        if ( bitstring_all /= string0 ) then
            error stop procedure // ' to_string failed to convert set1 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set1 value'
        end if

        call set0 % write_bitset( string0 )
        if ( ('S33B' // bitstring_0) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set0 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set0 value'
        end if

        call set1 % write_bitset( string0 )
        if ( ('S33B' // bitstring_all) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set1 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set1 value'
        end if

        return
    end subroutine test_string_operations

    subroutine test_io()
        character(*), parameter:: procedure = 'TEST_IO'

        integer :: unit

        write(*,*)
        write(*,*) 'Test bitset I/O: input, read_bitset, output, and ' // &
            'write_bitset'

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

        if ( set4 /= set0 .or. set5 /= set1 .or. set3 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'bitset literals failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'plain write_bitset_unit and read_bitset_unit succeeded.'
        end if

        rewind( unit )

        call set2 % write_bitset(unit, advance='no')
        call set1 % write_bitset(unit, advance='no')
        call set0 % write_bitset(unit)
        rewind( unit )
        call set3 % read_bitset(unit, advance='no')
        call set4 % read_bitset(unit, advance='no')
        call set5 % read_bitset(unit)

        if ( set5 /= set0 .or. set4 /= set1 .or. set3 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'bitset literals with advance == "no" failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'write_bitset_unit and read_bitset_unit with ' // &
                'advance=="no" succeeded.'
        end if
        close(unit)

        open( newunit=unit, form='unformatted', status='scratch', &
              action='readwrite' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        rewind( unit )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        close( unit )

        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded.'
        end if

        open( newunit=unit, form='unformatted', access='stream', &
              status='scratch', action='readwrite' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        rewind( unit )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        close( unit )

        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'stream output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'stream output and input succeeded.'
        end if

    end subroutine test_io

    subroutine test_initialization()
        character(*), parameter:: procedure = 'TEST_INITIALIZATION'
        logical(int8)  :: log1(64) = .true.
        logical(int16) :: log2(31) = .false.
        logical(int32) :: log3(15) = .true.
        logical(int64) :: log4(33) = .false.
        logical(int8), allocatable  :: log5(:)
        logical(int16), allocatable :: log6(:)
        logical(int32), allocatable :: log7(:)
        logical(int64), allocatable :: log8(:)

        write(*,*)
        write(*,*) 'Test initialization: assignment, extract, and init'

        set5 = log1
        if ( set5 % bits() /= 64 ) then
            error stop procedure // &
                ' initialization with logical(int8) failed to set' // &
                ' the right size.'
        else if ( .not. set5 % all() ) then
            error stop procedure // ' initialization with' // &
                ' logical(int8) failed to set the right values.'
        else
            write(*,*) 'Initialization with logical(int8) succeeded.'
        end if

        set5 = log2
        if ( set5 % bits() /= 31 ) then
            error stop procedure // &
                ' initialization with logical(int16) failed to set' // &
                ' the right size.'
        else if ( .not. set5 % none() ) then
            error stop procedure // &
                ' initialization with logical(int16) failed to set' // &
                ' the right values.'
        else
            write(*,*) 'Initialization with logical(int16) succeeded.'
        end if

        set5 = log3
        if ( set5 % bits() /= 15 ) then
            error stop procedure // &
                ' initialization with logical(int32) failed to set' // &
                ' the right size.'
        else if ( .not. set5 % all() ) then
            error stop procedure // &
                ' initialization with logical(int32) failed to set' // &
                ' the right values.'
        else
            write(*,*) 'Initialization with logical(int32) succeeded.'
        end if

        set5 = log4
        if ( set5 % bits() /= 33 ) then
            error stop procedure // &
                ' initialization with logical(int64) failed to set' // &
                ' the right size.'
        else if ( .not. set5 % none() ) then
            error stop procedure // &
                ' initialization with logical(int64) failed to set' // &
                ' the right values.'
        else
            write(*,*) 'Initialization with logical(int64) succeeded.'
        end if

        set5 = log1
        call extract( set4, set5, 1_bits_kind, 33_bits_kind )
        if ( set4 % bits() /= 33 ) then
            error stop procedure // &
                ' initialization with extract failed to set' // &
                ' the right size.'
        else if ( .not. set4 % all() ) then
            error stop procedure // &
                ' initialization with extract failed to set' // &
                ' the right values.'
        else
            write(*,*) 'Initialization with extract succeeded.'
        end if

        set4 = set5
        if ( set4 % bits() /= 64 ) then
            write(*,*) 'Bits = ', set4 % bits()
            error stop procedure // &
                ' initialization with simple assignment failed to set' // &
                ' the right size.'
        else if ( .not. set4 % all() ) then
            error stop procedure // &
                ' initialization with simple assignment failed to set' // &
                ' the right values.'
        else
            write(*,*) 'Initialization with simple assignment succeeded.'
        end if

        log5 = set5
        if ( size(log5) /= 64 ) then
            error stop procedure // &
                ' initialization of logical(int8) with assignment failed' // &
                ' to set the right size.'
        else if ( .not. all(log5) ) then
            error stop procedure // &
                ' initialization of logical(int8) with assignment failed' // &
                ' to set the right values.'
        else
            write(*,*) 'Initialization of logical(int8) succeeded.'
        end if

        log6 = set5
        if ( size(log6) /= 64 ) then
            error stop procedure // &
                ' initialization of logical(int16) with assignment failed' // &
                ' to set the right size.'
        else if ( .not. all(log6) ) then
            error stop procedure // &
                ' initialization of logical(int16) with assignment failed' // &
                ' to set the right values.'
        else
            write(*,*) 'Initialization of logical(int16) succeeded.'
        end if

        log7 = set5
        if ( size(log7) /= 64 ) then
            error stop procedure // &
                ' initialization of logical(int32) with assignment failed' // &
                ' to set the right size.'
        else if ( .not. all(log7) ) then
            error stop procedure // &
                ' initialization of logical(int32) with assignment failed' // &
                ' to set the right values.'
        else
            write(*,*) 'Initialization of logical(int32) succeeded.'
        end if

        log8 = set5
        if ( size(log8) /= 64 ) then
            error stop procedure // &
                ' initialization of logical(int64) with assignment failed' // &
                ' to set the right size.'
        else if ( .not. all(log8) ) then
            error stop procedure // &
                ' initialization of logical(int64) with assignment failed' // &
                ' to set the right values.'
        else
            write(*,*) 'Initialization of logical(int64) succeeded.'
        end if

    end subroutine test_initialization

    subroutine test_bitset_inquiry()
        character(*), parameter:: procedure = 'TEST_BITSET_INQUIRY'
        integer(bits_kind) :: i

        write(*,*)
        write(*,*) 'Test bitset inquiry: all, any, bits, none, test, and value'

        if ( set0 % none() ) then
            if ( .not. set0 % any() ) then
                write(*,*) 'As expected set0 has no bits set'
            else
                error stop procedure // ' set0 had some bits set which ' // &
                    'was unexpected.'
            end if
        else
            error stop procedure // ' set0 did not have none set which ' // &
                'was unexpected'
        end if

        call set0 % not()
        if ( set0 % all() ) then
            if ( set0 % any() ) then
                write(*,*) 'As expected set0 now has all bits set'
            else
                error stop procedure // ' set0 had no bits set which ' // &
                    'was unexpected.'
            end if
        else
            error stop procedure // ' set0 did not have all bits set ' // &
                'which was unexpected'
        end if

        if ( set1 % any() ) then
            if ( set1 % all() ) then
                write(*,*) 'As expected set1 has all bits set'
            else
                error stop procedure // ' set1 did not have all bits set ' // &
                    'which was unexpected.'
            end if
        else
            error stop procedure // ' set1 had no bits set ' // &
                'which was unexpected'
        end if

        call set0 % not()
        do i=0, set0 % bits() - 1
            if ( set0 % test(i) ) then
                error stop procedure // ' against expectations set0 has ' // &
                    'at least 1 bit set.'
            end if
        end do

        write(*,*) 'As expected set0 had no bits set.'

        do i=0, set1 % bits() - 1
            if ( .not. set1 % test(i) ) then
                error stop procedure // ' against expectations set1 has ' // &
                    'at least 1 bit unset.'
            end if
        end do

        write(*,*) 'As expected set1 had all bits set.'

        do i=0, set0 % bits() - 1
            if ( set0 % value(i) /= 0 ) then
                error stop procedure // ' against expectations set0 has ' // &
                    'at least 1 bit set.'
            end if
        end do

        write(*,*) 'As expected set0 had no bits set.'

        do i=0, set1 % bits() - 1
            if ( set1 % value(i) /= 1 ) then
                error stop procedure // ' against expectations set1 has ' // &
                    'at least 1 bit unset.'
            end if
        end do

        write(*,*) 'As expected set1 had all bits set.'

        if ( set0 % bits() == 33 ) then
            write(*,*) 'set0 has 33 bits as expected.'
        else
            error stop procedure // 'set0 unexpectedly does not have 33 bits.'
        end if

    end subroutine test_bitset_inquiry

    subroutine test_bit_operations()
        character(*), parameter:: procedure = 'TEST_BIT_OPERATIONS'

        write(*,*)
        write(*,*) 'Test bit operations: clear, flip, not, and set'

        if ( .not. set1 % all() ) then
            error stop procedure // ' set1 is not all set.'
        end if

        call set1 % clear(0_bits_kind)
        if ( .not. set1 % test(0_bits_kind) ) then
            if ( set1 % test(1_bits_kind) ) then
                write(*,*) 'Cleared one bit in set1 as expected.'
            else
                error stop procedure // ' cleared more than one bit in set1.'
            end if
        else
            error stop procedure // ' did not clear the first bit in set1.'
        end if

        call set1 % clear(1_bits_kind, 32_bits_kind)
        if ( set1 % none() ) then
            write(*,*) 'Cleared remaining bits in set1 as expected.'
        else
            error stop procedure // ' did not clear remaining bits ' // &
                'in set1.'
        end if

        call set1 % flip(0_bits_kind)
        if ( set1 % test(0_bits_kind) ) then
            if ( .not. set1 % test(1_bits_kind) ) then
                write(*,*) 'Flipped one bit in set1 as expected.'
            else
                error stop procedure // ' flipped more than one bit in set1.'
            end if
        else
            error stop procedure // ' did not flip the first bit in set1.'
        end if

        call set1 % flip(1_bits_kind, 32_bits_kind)
        if ( set1 % all() ) then
            write(*,*) 'Flipped remaining bits in set1 as expected.'
        else
            error stop procedure // ' did not flip remaining bits ' // &
                'in set1.'
        end if

        call set1 % not()
        if ( set1 % none() ) then
            write(*,*) 'Unset bits in set1 as expected.'
        else
            error stop procedure // ' did not unset bits in set1.'
        end if

        call set1 % set(0_bits_kind)
        if ( set1 % test(0_bits_kind) ) then
            if ( .not. set1 % test(1_bits_kind) ) then
                write(*,*) 'Set first bit in set1 as expected.'
            else
                error stop procedure // ' set more than one bit in set1.'
            end if
        else
            error stop procedure // ' did not set the first bit in set1.'
        end if

        call set1 % set(1_bits_kind, 32_bits_kind)
        if ( set1 % all() ) then
            write(*,*) 'Set the remaining bits in set1 as expected.'
        else
            error stop procedure // ' did not set the remaining bits ' // &
                'in set1.'
        end if

    end subroutine test_bit_operations

    subroutine test_bitset_comparisons()
        character(*), parameter:: procedure = 'TEST_BITSET_COMPARISON'

        write(*,*)
        write(*,*) 'Test bitset comparisons: ==, /=, <, <=, >, and >='

        if ( set0 == set0 .and. set1 == set1 .and. set2 == set2 .and. &
            .not. set0 == set1 .and. .not. set0 == set2 .and. .not.   &
            set1 == set2 ) then
            write(*,*) 'Passed 64 bit equality tests.'
        else
            error stop procedure // ' failed 64 bit equality tests.'
        end if

        if ( set0 /= set1 .and. set1 /= set2 .and. set0 /= set2 .and. &
            .not. set0 /= set0 .and. .not. set1 /= set1 .and. .not.   &
            set2 /= set2 ) then
            write(*,*) 'Passed 64 bit inequality tests.'
        else
            error stop procedure // ' failed 64 bit inequality tests.'
        end if

        if ( set1 > set0 .and. set2 > set0 .and. set1 > set2 .and. &
            .not. set0 > set1 .and. .not. set1 > set1 .and. .not.  &
            set2 > set1 ) then
            write(*,*) 'Passed 64 bit greater than tests.'
        else
            error stop procedure // ' failed 64 bit greater than tests.'
        end if

        if ( set1 >= set0 .and. set1 >= set2 .and. set2 >= set2 .and. &
            .not. set0 >= set1 .and. .not. set0 >= set1 .and. .not.  &
            set2 >= set1 ) then
            write(*,*) 'Passed 64 bit greater than or equal tests.'
        else
            error stop procedure // ' failed 64 bit greater than or ' // &
                'equal tests.'
        end if

        if ( set0 < set1 .and. set0 < set1 .and. set2 < set1 .and. &
            .not. set1 < set0 .and. .not. set0 < set0 .and. .not.  &
            set1 < set2 ) then
            write(*,*) 'Passed 64 bit less than tests.'
        else
            error stop procedure // ' failed 64 bit less than tests.'
        end if

        if ( set0 <= set1 .and. set2 <= set1 .and. set2 <= set2 .and. &
            .not. set1 <= set0 .and. .not. set2 <= set0 .and. .not.  &
            set1 <= set2 ) then
            write(*,*) 'Passed 64 bit less than or equal tests.'
        else
            error stop procedure // ' failed 64 bit less than or ' // &
                'equal tests.'
        end if

    end subroutine test_bitset_comparisons

    subroutine test_bitset_operations()
        character(*), parameter:: procedure = 'TEST_BITSET_OPERATIONS'

        write(*,*)
        write(*,*) 'Test bitset operations: and, and_not, or, and xor'

        call set0 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call and( set0, set4 ) ! all all
        if ( set0 % all() ) then
            write(*,*) 'First test of AND worked.'
        else
            error stop procedure // ' first test of AND failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call set3 % from_string( bitstring_all )
        call and( set3, set4 ) ! all none
        if ( set3 % none() ) then
            write(*,*) 'Second test of AND worked.'
        else
            error stop procedure // ' second test of AND failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Third test of AND worked.'
        else
            error stop procedure // ' third test of AND failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of AND worked.'
        else
            error stop procedure // ' fourth test of AND failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call and_not( set4, set3 ) ! all all
        if ( set4 % none() ) then
            write(*,*) 'First test of AND_NOT worked.'
        else
            error stop procedure // ' first test of AND_NOT failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call and_not( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Second test of AND_NOT worked.'
        else
            error stop procedure // ' second test of AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of AND_NOT worked.'
        else
            error stop procedure // ' third  test of AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of AND_NOT worked.'
        else
            error stop procedure // ' fourth  test of AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call or( set3, set4 ) ! all all
        if ( set3 % all() ) then
            write(*,*) 'First test of OR worked.'
        else
            error stop procedure // ' first test of OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call or( set4, set3 ) ! all none
        if ( set4 % all() ) then
            write(*,*) 'Second test of OR worked.'
        else
            error stop procedure // ' second test of OR failed.'
        end if

        call or( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Third test of OR worked.'
        else
            error stop procedure // ' third test of OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call or( set4, set3 ) !none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of OR worked.'
        else
            error stop procedure // ' fourth test of OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'First test of XOR worked.'
        else
            error stop procedure // ' first test of XOR failed.'
        end if

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Second test of XOR worked.'
        else
            error stop procedure // ' second test of XOR failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of XOR worked.'
        else
            error stop procedure // ' third test of XOR failed.'
        end if

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! all all
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of XOR worked.'
        else
            error stop procedure // ' fourth test of XOR failed.'
        end if

    end subroutine test_bitset_operations


end program test_stdlib_bitset_64
