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

    call test_io()

contains

    subroutine test_io()
        character(*), parameter:: procedure = 'TEST_IO'

        integer :: unit

        write(*,*)
        write(*,*) 'Test bitset I/O: input, read_bitset, output, and ' // &
            'write_bitset'

        call set2 % from_string( bitstring_33 )
        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set2 % write_bitset(unit)
        call set1 % write_bitset(unit)
        call set0 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
        call set3 % read_bitset(unit)
        call set5 % read_bitset(unit)
        call set4 % read_bitset(unit)
        if ( set4 /= set0 .or. set5 /= set1 .or. set3 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' bitset literals failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'plain write_bitset_unit and read_bitset_unit succeeded.'
        end if

        close( unit )

        call set12 % from_string( bitstring_33 // bitstring_33 )
        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set12 % write_bitset(unit)
        call set11 % write_bitset(unit)
        call set10 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
        call set13 % read_bitset(unit)
        call set15 % read_bitset(unit)
        call set14 % read_bitset(unit)
        if ( set14 /= set10 .or. set15 /= set11 .or. set3 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' bitset literals for bits > 64 failed.'
        else
            write(*,*) 'Transfer  bits > 64 to and from units using ' // &
                'plain write_bitset_unit and read_bitset_unit succeeded.'
        end if

        close( unit )

        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set2 % write_bitset(unit, advance='no')
        call set1 % write_bitset(unit, advance='no')
        call set0 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
        call set3 % read_bitset(unit, advance='no')
        call set4 % read_bitset(unit, advance='no')
        call set5 % read_bitset(unit)
        if ( set5 /= set0 .or. set4 /= set1 .or. set3 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' bitset literals with advance == "no" failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'write_bitset_unit and read_bitset_unit with ' // &
                'advance=="no" succeeded.'
        end if

        close( unit )

        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set12 % write_bitset(unit, advance='no')
        call set11 % write_bitset(unit, advance='no')
        call set10 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
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

        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', action='write' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', action='read' )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded.'
        end if

        close( unit )

        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', access='stream', action='write' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', access='stream', action='read' )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' stream output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'stream output and input succeeded.'
        end if

        close( unit )

        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', action='write' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', action='read' )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        if ( set13 /= set10 .or. set14 /= set11 .or. set15 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' output and input failed for bits . 64.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded for bits > 64.'
        end if
        close(unit)

        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', access='stream', action='write' )
        call set12 % output(unit)
        call set11 % output(unit)
        call set10 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', access='stream', action='read' )
        call set15 % input(unit)
        call set14 % input(unit)
        call set13 % input(unit)
        if ( set13 /= set10 .or. set14 /= set11 .or. set15 /= set12 ) then
            error stop procedure // ' transfer to and from units using ' // &
                ' stream output and input failed for bits . 64.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'stream output and input succeeded for bits > 64.'
        end if
        close(unit)

    end subroutine test_io


end program test_stdlib_bitset_large
