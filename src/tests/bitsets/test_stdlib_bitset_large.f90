program test_stdlib_bitset_large
    use :: stdlib_kinds, only : int8, int16, int32, int64
    use stdlib_bitsets
    implicit none
    character(*), parameter :: &
        bitstring_0   = '000000000000000000000000000000000', &
        bitstring_33  = '100000000000000000000000000000000', &
        bitstring_all = '111111111111111111111111111111111'
    type(bitset_large) :: set0, set1, set2, set3, set4, set5
    type(bitset_large) :: set10, set11, set12, set13, set14, set15
    integer            :: status
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

        call set10 % from_string( bitstring_0 // bitstring_0 )
        if ( bits(set10) /= 66 ) then
            error stop procedure // ' from_string failed to interpret ' // &
                'bitstring_0 // bitstring_0 size properly.'
        else if ( .not. set10 % none() ) then
            error stop procedure // ' failed to interpret bitstring_0 ' // &
                '// bitstring_0 value properly.'
        else if ( set10 % any() ) then
            error stop procedure // ' failed to interpret bitstring_0 ' // &
                '// bitstring_0 value properly.'
        else
            write(*,*) 'from_string transferred bitstring_0//bitstring_0' // &
                ' properly into set10'
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
            write(*,*) 'from_string transferred bitstring_1 properly into set1'
        end if

        call set11 % from_string( bitstring_all // bitstring_all  )
        if ( bits(set11) /= 66 ) then
            error stop procedure // ' from_string failed to interpret ' // &
                'bitstring_all // bitstring_all size properly.'
        else if ( set11 % none() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                '// bitstring_all value properly.'
        else if ( .not. set11 % any() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                '// bitstring_all value properly.'
        else if ( .not. set11 % all() ) then
            error stop procedure // ' failed to interpret bitstring_all ' // &
                '// bitstring_all value properly.'
        else
            write(*,*) 'from_string transferred bitstring_all // ' // &
                'bitstring_all properly into set11'
        end if

        call set3 % read_bitset( bitstring_0, status )
        if ( status /= success ) then
            write(*,*) 'read_bitset_string failed with bitstring_0 as expected.'
        else
            error stop procedure // ' read_bitset_string did not fail ' // &
                'with bitstring_0 as expected.'
        end if

        call set13 % read_bitset( bitstring_0 // bitstring_0, status )
        if ( status /= success ) then
            write(*,*) 'read_bitset_string failed with bitstring_0 ' // &
                '// bitstring_0 as expected.'
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

        call set13 % read_bitset( 's66b' // bitstring_0 // bitstring_0, &
            status )
        if ( bits(set13) /= 66 ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s66b" // bitstring_0 // bitstring_0 size properly.'
        else if ( .not. set13 % none() ) then
            error stop procedure // ' failed to interpret "s66b" // ' // &
                'bitstring_0 // bitstring_0 value properly.'
        else
            write(*,*) 'read_bitset_string transferred "s66b" // ' // &
                'bitstring_0 // bitstring_0 properly into set13'
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

        call set14 % read_bitset( 's66b' // bitstring_all &
            // bitstring_all )
        if ( bits(set14) /= 66 ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s66b" // bitstring_all // bitstring_all ' // &
                'size properly.'
        else if ( set14 % none() ) then
            error stop procedure // ' read_bitset_string failed to ' // &
                'interpret "s66b" // bitstring_all // bitstring_all ' // &
                'value properly.'
        else if ( .not. set14 % any() ) then
            error stop procedure // ' read_bitset_string failed to // ' // &
                'interpret "s66b" bitstring_all // bitstring_all ' // &
                'value properly.'
        else if ( .not. set14 % all() ) then
            error stop procedure // ' read_bitset_string failed to // ' // &
                'interpret "s66b" bitstring_all // bitstring_all ' // &
                'value properly.'
        else
            write(*,*) 'read_bitset_string transferred "s66b" // ' // &
                'bitstring_all // bitstring_all properly into set14.'
        end if

        call set0 % to_string( string0 )
        if ( bitstring_0 /= string0 ) then
            error stop procedure // ' to_string failed to convert set0 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set0 value'
        end if

        call set10 % to_string( string0 )
        if ( bitstring_0 // bitstring_0 /= string0 ) then
            error stop procedure // ' to_string failed to convert set10 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set10 value'
        end if

        call set1 % to_string( string0 )
        if ( bitstring_all /= string0 ) then
            error stop procedure // ' to_string failed to convert set1 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set1 value'
        end if

        call set11 % to_string( string0 )
        if ( bitstring_all // bitstring_all /= string0 ) then
            error stop procedure // ' to_string failed to convert set11 ' // &
                'value properly.'
        else
            write(*,*) 'to_string properly converted the set11 value'
        end if

        call set0 % write_bitset( string0 )
        if ( ('S33B' // bitstring_0) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set2 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set0 value'
        end if

        call set10 % write_bitset( string0 )
        if ( ('S66B' // bitstring_0 // bitstring_0) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set10 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set10 value'
        end if

        call set1 % write_bitset( string0 )
        if ( ('S33B' // bitstring_all) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set1 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set1 value'
        end if

        call set11 % write_bitset( string0 )
        if ( ('S66B' // bitstring_all // bitstring_all) /= string0 ) then
            error stop procedure // ' write_bitset_string failed to ' // &
                'convert set11 value properly.'
        else
            write(*,*) 'write_bitset_string properly converted the set11 value'
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

        call set12 % from_string( bitstring_33 // bitstring_33 )
        call set12 % write_bitset(unit)
        call set11 % write_bitset(unit)
        call set10 % write_bitset(unit)
        rewind( unit )
        call set13 % read_bitset(unit)
        call set15 % read_bitset(unit)
        call set14 % read_bitset(unit)
        if ( set14 /= set10 .or. set15 /= set11 .or. set3 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'bitset literals for bits > 64 failed.'
        else
            write(*,*) 'Transfer  bits > 64 to and from units using ' // &
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

        rewind( unit )

        call set12 % write_bitset(unit, advance='no')
        call set11 % write_bitset(unit, advance='no')
        call set10 % write_bitset(unit)
        rewind( unit )
        call set13 % read_bitset(unit, advance='no')
        call set14 % read_bitset(unit, advance='no')
        call set15 % read_bitset(unit)
        if ( set15 /= set10 .or. set14 /= set11 .or. set13 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' bitset literals for bitss > 64 with advance == "no" failed.'
        else
            write(*,*) 'Transfer bits > 64 to and from units using ' // &
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
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded.'
        end if

        close( unit )

        open( newunit=unit, form='unformatted', access='stream', &
              status='scratch', action='readwrite' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        rewind( unit )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'stream output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'stream output and input succeeded.'
        end if

        close( unit )

        open( newunit=unit, form='unformatted', status='scratch', &
              action='readwrite' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        rewind( unit )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        if ( set13 /= set10 .or. set14 /= set11 .or. set15 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'output and input failed for bits . 64.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded for bits > 64.'
        end if
        close(unit)

        open( newunit=unit, form='unformatted', access='stream', &
              status='scratch', action='readwrite' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        rewind( unit )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        if ( set13 /= set10 .or. set14 /= set11 .or. set15 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                'stream output and input failed for bits . 64.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'stream output and input succeeded for bits > 64.'
        end if
        close(unit)

    end subroutine test_io

    subroutine test_initialization()
        character(*), parameter:: procedure = 'TEST_INITIALIZATION'
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

        set5 = log11
        if ( set5 % bits() /= 66 ) then
            error stop procedure // &
                ' initialization with logical(int8) failed to set' // &
                ' the right size > 64 bits.'
        else if ( .not. set5 % all() ) then
            error stop procedure // ' initialization with' // &
                ' logical(int8) failed to set the right values.'
        else
            write(*,*) 'Initialization > 64 bits  with logical(int8)succeeded.'
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

        set5 = log12
        if ( set5 % bits() /= 99 ) then
            error stop procedure // &
                ' initialization with logical(int16) failed to set' // &
                ' the right size > 64 bits .'
        else if ( .not. set5 % none() ) then
            error stop procedure // &
                ' initialization with logical(int16) failed to set' // &
                ' the right values > 64 bits .'
        else
            write(*,*) 'Initialization > 64 bits  with logical(int16) ' // &
                'succeeded.'
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

        set5 = log13
        if ( set5 % bits() /= 132 ) then
            error stop procedure // &
                ' initialization with logical(int32) failed to set' // &
                ' the right size > 64 bits .'
        else if ( .not. set5 % all() ) then
            error stop procedure // &
                ' initialization with logical(int32) failed to set' // &
                ' the right values > 64 bits .'
        else
            write(*,*) 'Initialization > 64 bits  with logical(int32) ' // &
                'succeeded.'
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

        set5 = log14
        if ( set5 % bits() /= 165 ) then
            error stop procedure // &
                ' initialization with logical(int64) failed to set' // &
                ' the right size > 64 bits .'
        else if ( .not. set5 % none() ) then
            error stop procedure // &
                ' initialization with logical(int64) failed to set' // &
                ' the right values > 64 bits .'
        else
            write(*,*) 'Initialization > 64 bits  with logical(int64) ' // &
                'succeeded.'
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

        set5 = log11
        call extract( set4, set5, 1_bits_kind, 65_bits_kind )
        if ( set4 % bits() /= 65 ) then
            error stop procedure // &
                ' initialization with extract failed to set' // &
                ' the right size > 64 bits.'
        else if ( .not. set4 % all() ) then
            error stop procedure // &
                ' initialization with extract failed to set' // &
                ' the right values > 64 bits.'
        else
            write(*,*) 'Initialization with extract succeeded.'
        end if

        set5 = log1
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

        set5 = log11
        set4 = set5
        if ( set4 % bits() /= 66 ) then
            write(*,*) 'Bits = ', set4 % bits()
            error stop procedure // &
                ' initialization with simple assignment failed to set' // &
                ' the right size > 64 bits.'
        else if ( .not. set4 % all() ) then
            error stop procedure // &
                ' initialization with simple assignment failed to set' // &
                ' the right values > 64 bits.'
        else
            write(*,*) 'Initialization > 64 bits with simple assignment ' // &
                'succeeded.'
        end if

        set5 = log1
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

        set5 = log11
        log5 = set5
        if ( size(log5) /= 66 ) then
            error stop procedure // &
                ' initialization of logical(int8) with assignment failed' // &
                ' to set the right size > 64 bits.'
        else if ( .not. all(log5) ) then
            error stop procedure // &
                ' initialization of logical(int8) with assignment failed' // &
                ' to set the right values > 64 bits.'
        else
            write(*,*) 'Initialization > 64 bits of logical(int8) succeeded.'
        end if

        set5 = log1
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

        set5 = log11
        log6 = set5
        if ( size(log6) /= 66 ) then
            error stop procedure // &
                ' initialization of logical(int16) with assignment failed' // &
                ' to set the right size > 64 bits.'
        else if ( .not. all(log6) ) then
            error stop procedure // &
                ' initialization of logical(int16) with assignment failed' // &
                ' to set the right values > 64 bits.'
        else
            write(*,*) 'Initialization > 64 bits of logical(int16) succeeded.'
        end if

        set5 = log1
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

        set5 = log11
        log7 = set5
        if ( size(log7) /= 66 ) then
            error stop procedure // &
                ' initialization of logical(int32) with assignment failed' // &
                ' to set the right size > 64 bits.'
        else if ( .not. all(log7) ) then
            error stop procedure // &
                ' initialization of logical(int32) with assignment failed' // &
                ' to set the right values > 64 bits.'
        else
            write(*,*) 'Initialization > 64 bits of logical(int32) succeeded.'
        end if

        set5 = log1
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

        set5 = log11
        log8 = set5
        if ( size(log8) /= 66 ) then
            error stop procedure // &
                ' initialization of logical(int64) with assignment failed' // &
                ' to set the right size > 64 bits.'
        else if ( .not. all(log8) ) then
            error stop procedure // &
                ' initialization of logical(int64) with assignment failed' // &
                ' to set the right values > 64 bits.'
        else
            write(*,*) 'Initialization > 64 bits of logical(int64) succeeded.'
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
            error stop procedure // ' set1 had none bits set ' // &
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
                error stop procedure // ' against expectations set0 has ' // &
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
                error stop procedure // ' against expectations set0 has ' // &
                    'at least 1 bit unset.'
            end if
        end do

        write(*,*) 'As expected set1 had all bits set.'

        if ( set0 % bits() == 33 ) then
            write(*,*) 'set0 has 33 bits as expected.'
        else
            error stop procedure // 'set0 unexpectedly does not have 33 bits.'
        end if

! > 64 bit inquiries
        call set10 % from_string( bitstring_0 // bitstring_0 // bitstring_0 )
        if ( set10 % none() ) then
            if ( .not. set10 % any() ) then
                write(*,*) 'As expected set10 has no bits set'
            else
                error stop procedure // ' set10 had some bits set which ' // &
                    'was unexpected.'
            end if
        else
            error stop procedure // ' set10 did not have none set which ' // &
                'was unexpected'
        end if

        call set10 % not()

        if ( set10 % all() ) then
            if ( set10 % any() ) then
                write(*,*) 'As expected set10 now has all bits set'
            else
                error stop procedure // ' set10 had no bits set which ' // &
                    'was unexpected.'
            end if
        else
            error stop procedure // ' set10 did not have all bits set ' // &
                'which was unexpected'
        end if

        call set11 % from_string( bitstring_all // bitstring_all // &
            bitstring_all )
        if ( set11 % any() ) then
            if ( set11 % all() ) then
                write(*,*) 'As expected set11 has all bits set'
            else
                error stop procedure // ' set11 did not have all bits set ' // &
                    'which was unexpected.'
            end if
        else
            error stop procedure // ' set11 had none bits set ' // &
                'which was unexpected'
        end if

        call set10 % not()
        do i=0, set10 % bits() - 1
            if ( set10 % test(i) ) then
                error stop procedure // ' against expectations set10 has ' // &
                    'at least 1 bit set.'
            end if
        end do

        write(*,*) 'As expected set10 had no bits set.'

        do i=0, set11 % bits() - 1
            if ( .not. set11 % test(i) ) then
                error stop procedure // ' against expectations set11 has ' // &
                    'at least 1 bit unset.'
            end if
        end do

        write(*,*) 'As expected set11 had all bits set.'

        do i=0, set10 % bits() - 1
            if ( set10 % value(i) /= 0 ) then
                error stop procedure // ' against expectations set10 has ' // &
                    'at least 1 bit set.'
            end if
        end do

        write(*,*) 'As expected set10 had no bits set.'

        do i=0, set11 % bits() - 1
            if ( set11 % value(i) /= 1 ) then
                error stop procedure // ' against expectations set11 has ' // &
                    'at least 1 bit unset.'
            end if
        end do

        write(*,*) 'As expected set11 had all bits set.'

        if ( set0 % bits() == 33 ) then
            write(*,*) 'set0 has 33 bits as expected.'
        else
            error stop procedure // 'set0 unexpectedly does not have 33 bits.'
        end if

        if ( set10 % bits() == 99 ) then
            write(*,*) 'set10 has 99 bits as expected.'
        else
            error stop procedure // 'set10 unexpectedly does not have 99 bits.'
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

        call set11 % init( 166_bits_kind )
        call set11 % not()
        if ( .not. set11 % all() ) then
            error stop procedure // ' set11 is not all set.'
        end if

        call set11 % clear(0_bits_kind)
        if ( .not. set11 % test(0_bits_kind) ) then
            if ( set11 % test(1_bits_kind) ) then
                write(*,*) 'Cleared one bit in set11 as expected.'
            else
                error stop procedure // ' cleared more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not clear the first bit in set11.'
        end if

        call set11 % clear(165_bits_kind)
        if ( .not. set11 % test(165_bits_kind) ) then
            if ( set11 % test(164_bits_kind) ) then
                write(*,*) 'Cleared the last bit in set11 as expected.'
            else
                error stop procedure // ' cleared more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not clear the last bit in set11.'
        end if

        call set11 % clear(1_bits_kind, 164_bits_kind)
        if ( set11 % none() ) then
            write(*,*) 'Cleared remaining bits in set11 as expected.'
        else
            error stop procedure // ' did not clear remaining bits ' // &
                'in set11.'
        end if

        call set11 % flip(0_bits_kind)
        if ( set11 % test(0_bits_kind) ) then
            if ( .not. set11 % test(1_bits_kind) ) then
                write(*,*) 'Flipped one bit in set11 as expected.'
            else
                error stop procedure // ' flipped more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not flip the first bit in set11.'
        end if

        call set11 % flip(165_bits_kind)
        if ( set11 % test(165_bits_kind) ) then
            if ( .not. set11 % test(164_bits_kind) ) then
                write(*,*) 'Flipped last bit in set11 as expected.'
            else
                error stop procedure // ' flipped more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not flip the last bit in set11.'
        end if

        call set11 % flip(1_bits_kind, 164_bits_kind)
        if ( set11 % all() ) then
            write(*,*) 'Flipped remaining bits in set11 as expected.'
        else
            error stop procedure // ' did not flip remaining bits ' // &
                'in set11.'
        end if

        call set11 % not()
        if ( set11 % none() ) then
            write(*,*) 'Unset bits in set11 as expected.'
        else
            error stop procedure // ' did not unset bits in set11.'
        end if

        call set11 % set(0_bits_kind)
        if ( set11 % test(0_bits_kind) ) then
            if ( .not. set11 % test(1_bits_kind) ) then
                write(*,*) 'Set first bit in set11 as expected.'
            else
                error stop procedure // ' set more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not set the first bit in set11.'
        end if

        call set11 % set(165_bits_kind)
        if ( set11 % test(165_bits_kind) ) then
            if ( .not. set11 % test(164_bits_kind) ) then
                write(*,*) 'Set last bit in set11 as expected.'
            else
                error stop procedure // ' set more than one bit in set11.'
            end if
        else
            error stop procedure // ' did not set the last bit in set11.'
        end if

        call set11 % set(1_bits_kind, 164_bits_kind)
        if ( set11 % all() ) then
            write(*,*) 'Set the remaining bits in set11 as expected.'
        else
            error stop procedure // ' did not set the remaining bits ' // &
                'in set11.'
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

        call set10 % init(166_bits_kind)
        call set11 % init(166_bits_kind)
        call set11 % not()
        call set12 % init(166_bits_kind)
        call set12 % set(165_bits_kind)
        call set13 % init(166_bits_kind)
        call set13 % set(65_bits_kind)
        call set14 % init(166_bits_kind)
        call set14 % set(0_bits_kind)
        if ( set10 == set10 .and. set11 == set11 .and. set12 == set12 .and. &
             set13 == set13 .and. set14 == set14 .and.                      &
             .not. set13 == set14 .and. .not. set12 == set13 .and.          &
             .not. set10 == set11 .and. .not. set10 == set12 .and. .not.    &
             set11 == set12 ) then
            write(*,*) 'Passed > 64 bit equality tests.'
        else
            error stop procedure // ' failed > 64 bit equality tests.'
        end if

        if ( set10 /= set11 .and. set11 /= set12 .and. set10 /= set12 .and. &
             set13 /= set12 .and. set14 /= set13 .and. set14 /= set12 .and. &
             .not. set13 /= set13 .and. .not. set12 /= set12 .and.          &
             .not. set10 /= set10 .and. .not. set11 /= set11 .and. .not.    &
             set12 /= set12 ) then
            write(*,*) 'Passed > 64 bit inequality tests.'
        else
            error stop procedure // ' failed > 64 bit inequality tests.'
        end if

        if ( set11 > set10 .and. set12 > set10 .and. set11 > set12 .and. &
             set13 > set14 .and. set12 > set13 .and. set12 > set14 .and. &
             .not. set14 > set12 .and. .not. set12 > set11 .and.         &
             .not. set10 > set11 .and. .not. set11 > set11 .and. .not.   &
            set12 > set11 ) then
            write(*,*) 'Passed > 64 bit greater than tests.'
        else
            error stop procedure // ' failed > 64 bit greater than tests.'
        end if

        if ( set11 >= set10 .and. set11 >= set12 .and. set12 >= set12 .and. &
             set13 >= set14 .and. set12 >= set13 .and. set12 >= set14 .and. &
             .not. set14 >= set12 .and. .not. set12 >= set11 .and.          &
             .not. set10 >= set11 .and. .not. set10 >= set11 .and. .not.    &
             set12 >= set11 ) then
            write(*,*) 'Passed > 64 bit greater than or equal tests.'
        else
            error stop procedure // ' failed 64 bit greater than or ' // &
                'equal tests.'
        end if

        if ( set10 < set11 .and. set10 < set11 .and. set12 < set11 .and. &
             set14 < set13 .and. set13 < set12 .and. set14 < set12 .and. &
             .not. set12 < set14 .and. .not. set11 < set12 .and.         &
            .not. set11 < set10 .and. .not. set10 < set10 .and. .not.    &
            set11 < set12 ) then
            write(*,*) 'Passed > 64 bit less than tests.'
        else
            error stop procedure // ' failed > 64 bit less than tests.'
        end if

        if ( set10 <= set11 .and. set12 <= set11 .and. set12 <= set12 .and. &
             set14 <= set13 .and. set13 <= set12 .and. set14 <= set12 .and. &
             .not. set12 <= set14 .and. .not. set11 <= set12 .and.          &
            .not. set11 <= set10 .and. .not. set12 <= set10 .and. .not.     &
            set11 <= set12 ) then
            write(*,*) 'Passed > 64 bit less than or equal tests.'
        else
            error stop procedure // ' failed > 64 bit less than or ' // &
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
            write(*,*) 'First test of < 64 bit AND worked.'
        else
            error stop procedure // ' first test of < 64 bit AND failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call and( set0, set4 ) ! all none
        if ( set0 % none() ) then
            write(*,*) 'Second test of < 64 bit AND worked.'
        else
            error stop procedure // ' second test of < 64 bit AND failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Third test of < 64 bit AND worked.'
        else
            error stop procedure // ' third test of < 64 bit AND failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call and( set4, set3 ) ! none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of < 64 bit AND worked.'
        else
            error stop procedure // ' fourth test of < 64 bit AND failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call and_not( set4, set3 ) ! all all
        if ( set4 % none() ) then
            write(*,*) 'First test of < 64 bit AND_NOT worked.'
        else
            error stop procedure // ' first test of < 64 bit AND_NOT failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call and_not( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Second test of < 64 bit AND_NOT worked.'
        else
            error stop procedure // ' second test of < 64 bit AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of < 64 bit AND_NOT worked.'
        else
            error stop procedure // ' third  test of < 64 bit AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call and_not( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of < 64 bit AND_NOT worked.'
        else
            error stop procedure // ' fourth  test of < 64 bit AND_NOT failed.'
        end if

        call set3 % from_string( bitstring_all )
        call set4 % from_string( bitstring_all )
        call or( set3, set4 ) ! all all
        if ( set3 % all() ) then
            write(*,*) 'First test of < 64 bit OR worked.'
        else
            error stop procedure // ' first test of < 64 bit OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call or( set4, set3 ) ! all none
        if ( set4 % all() ) then
            write(*,*) 'Second test of < 64 bit OR worked.'
        else
            error stop procedure // ' second test of < 64 bit OR failed.'
        end if

        call or( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Third test of < 64 bit OR worked.'
        else
            error stop procedure // ' third test of < 64 bit OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call or( set4, set3 ) !none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of < 64 bit OR worked.'
        else
            error stop procedure // ' fourth test of < 64 bit OR failed.'
        end if

        call set3 % from_string( bitstring_0 )
        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'First test of < 64 bit XOR worked.'
        else
            error stop procedure // ' first test of < 64 bit XOR failed.'
        end if

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Second test of < 64 bit XOR worked.'
        else
            error stop procedure // ' second test of < 64 bit XOR failed.'
        end if

        call set4 % from_string( bitstring_0 )
        call xor( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of < 64 bit XOR worked.'
        else
            error stop procedure // ' third test of < 64 bit XOR failed.'
        end if

        call set4 % from_string( bitstring_all )
        call xor( set3, set4 ) ! all all
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of < 64 bit XOR worked.'
        else
            error stop procedure // ' fourth test of < 64 bit XOR failed.'
        end if

        call set0 % init(166_bits_kind)
        call set0 % not()
        call set4 % init(166_bits_kind)
        call set4 % not()
        call and( set0, set4 ) ! all all
        if ( set0 % all() ) then
            write(*,*) 'First test of > 64 bit AND worked.'
        else
            error stop procedure // ' first test of > 64 bit AND failed.'
        end if

        call set4 % init(166_bits_kind)
        call and( set0, set4 ) ! all none
        if ( set0 % none() ) then
            write(*,*) 'Second test of > 64 bit AND worked.'
        else
            error stop procedure // ' second test of > 64 bit AND failed.'
        end if

        call set3 % init(166_bits_kind)
        call set3 % not()
        call and( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Third test of > 64 bit AND worked.'
        else
            error stop procedure // ' third test of > 64 bit AND failed.'
        end if

        call set3 % init(166_bits_kind)
        call and( set4, set3 ) ! none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of > 64 bit AND worked.'
        else
            error stop procedure // ' fourth test of > 64 bit AND failed.'
        end if

        call set3 % not()
        call set4 % not()
        call and_not( set4, set3 ) ! all all
        if ( set4 % none() ) then
            write(*,*) 'First test of > 64 bit AND_NOT worked.'
        else
            error stop procedure // ' first test of > 64 bit AND_NOT failed.'
        end if

        call and_not( set4, set3 ) ! none all
        if ( set4 % none() ) then
            write(*,*) 'Second test of > 64 bit AND_NOT worked.'
        else
            error stop procedure // ' second test of > 64 bit AND_NOT failed.'
        end if

        call and_not( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of > 64 bit AND_NOT worked.'
        else
            error stop procedure // ' third  test of > 64 bit AND_NOT failed.'
        end if

        call set3 % not()
        call and_not( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of > 64 bit AND_NOT worked.'
        else
            error stop procedure // ' fourth  test of > 64 bit AND_NOT failed.'
        end if

        call set3 % init(166_bits_kind)
        call set3 % not()
        call set4 % init(166_bits_kind)
        call set4 % not()
        call or( set3, set4 ) ! all all
        if ( set3 % all() ) then
            write(*,*) 'First test of > 64 bit OR worked.'
        else
            error stop procedure // ' first test of > 64 bit OR failed.'
        end if

        call set3 % init(166_bits_kind)
        call or( set4, set3 ) ! all none
        if ( set4 % all() ) then
            write(*,*) 'Second test of > 64 bit OR worked.'
        else
            error stop procedure // ' second test of > 64 bit OR failed.'
        end if

        call or( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Third test of > 64 bit OR worked.'
        else
            error stop procedure // ' third test of > 64 bit OR failed.'
        end if

        call set3 % init(166_bits_kind)
        call set4 % init(166_bits_kind)
        call or( set4, set3 ) !none none
        if ( set4 % none() ) then
            write(*,*) 'Fourth test of > 64 bit OR worked.'
        else
            error stop procedure // ' fourth test of > 64 bit OR failed.'
        end if

        call xor( set3, set4 ) ! none none
        if ( set3 % none() ) then
            write(*,*) 'First test of > 64 bit XOR worked.'
        else
            error stop procedure // ' first test of > 64 bit XOR failed.'
        end if

        call set4 % not()
        call xor( set3, set4 ) ! none all
        if ( set3 % all() ) then
            write(*,*) 'Second test of > 64 bit XOR worked.'
        else
            error stop procedure // ' second test of > 64 bit XOR failed.'
        end if

        call set4 % not()
        call xor( set3, set4 ) ! all none
        if ( set3 % all() ) then
            write(*,*) 'Third test of > 64 bit XOR worked.'
        else
            error stop procedure // ' third test of > 64 bit XOR failed.'
        end if

        call set4 % not()
        call xor( set3, set4 ) ! all all
        if ( set3 % none() ) then
            write(*,*) 'Fourth test of > 64 bit XOR worked.'
        else
            error stop procedure // ' fourth test of > 64 bit XOR failed.'
        end if

    end subroutine test_bitset_operations


end program test_stdlib_bitset_large
