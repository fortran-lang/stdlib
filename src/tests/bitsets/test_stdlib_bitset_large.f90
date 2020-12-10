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
        open( newunit=unit, file='test123.txt', status='replace', &
            form='formatted', action='write' )
        call set2 % write_bitset(unit)
        call set1 % write_bitset(unit)
        call set0 % write_bitset(unit)
        close( unit )

        ! Wait 10 seconds, in the hope that the file gets saved before we open it
        call sleep(10)

        open( newunit=unit, file='test123.txt', status='old', &
            form='formatted', action='read' )
        print *, "1"
        call set3 % read_bitset(unit)
        print *, "2"
        call set5 % read_bitset(unit)
        print *, "3"
        call set4 % read_bitset(unit)
        print *, "4"
        if ( set4 /= set0 .or. set5 /= set1 .or. set3 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' bitset literals failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'plain write_bitset_unit and read_bitset_unit succeeded.'
        end if

        close( unit )

    end subroutine test_io

end program test_stdlib_bitset_large
