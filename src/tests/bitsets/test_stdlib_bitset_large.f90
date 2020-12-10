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
    end subroutine test_io


end program test_stdlib_bitset_large
