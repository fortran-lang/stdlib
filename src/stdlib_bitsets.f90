module stdlib_bitsets
!! Implements zero based bitsets of size up to `huge(0_int32)`.
!! The current code uses 64 bit integers to store the bits and uses all 64 bits.
!! The code assumes two's complement integers, and treats negative integers as
!! having the sign bit set.
!!([Specification](../page/specs/stdlib_bitsets.html))

    use :: stdlib_kinds, only:  &
           bits_kind  => int32, & ! If changed change also max_digits, and
           block_kind => int64, & ! overflow_bits
           int8,                &
           int16,               &
           int32,               &
           int64
    use stdlib_optval, only : optval

    use, intrinsic ::          &
        iso_fortran_env, only: &
        error_unit

    implicit none

    private

    integer(bits_kind), parameter ::        &
        block_size  = bit_size(0_block_kind)

    public :: max_digits, overflow_bits
    integer, parameter ::                   &
        max_digits = 10 ! bits_kind == int32
!        max_digits = 19 ! bits_kind == int64

    integer(bits_kind), parameter ::     &
        overflow_bits = 2_bits_kind**30/5 ! bits_kind == int32
!        overflow_bits = 2_bits_kind**62/5 ! bits_kind == int64

    integer(block_kind), parameter :: all_zeros  = 0_block_kind
    integer(block_kind), parameter :: all_ones   = not(all_zeros)

    character(*), parameter :: module_name = "STDLIB_BITSETS"
    integer, parameter ::    &
        ia0   = iachar('0'), &
        ia9   = iachar('9')

    integer, parameter, public :: success = 0
!! Error flag indicating no errors
    integer, parameter, public :: alloc_fault = 1
!! Error flag indicating a memory allocation failure
    integer, parameter, public :: array_size_invalid_error = 2
!! Error flag indicating an invalid bits value
    integer, parameter, public :: char_string_invalid_error = 3
!! Error flag indicating an invalid character string
    integer, parameter, public :: char_string_too_large_error = 4
!! Error flag indicating a too large character string
    integer, parameter, public :: char_string_too_small_error = 5
!! Error flag indicating a too small character string
    integer, parameter, public :: eof_failure = 6
!! Error flag indicating unexpected End-of-File on a READ
    integer, parameter, public :: index_invalid_error = 7
!! Error flag indicating an invalid index
    integer, parameter, public :: integer_overflow_error = 8
!! Error flag indicating integer overflow
    integer, parameter, public :: read_failure = 9
!! Error flag indicating failure of a READ statement
    integer, parameter, public :: write_failure = 10
!! Error flag indicating a failure on a WRITE statement

    public :: bits_kind
! Public constant

    public ::         &
        bitset_type,  &
        bitset_large, &
        bitset_64

! Public types

    public ::          &
        assignment(=), &
        and,           &
        and_not,       &
        bits,          &
        extract,       &
        operator(==),  &
        operator(/=),  &
        operator(>),   &
        operator(>=),  &
        operator(<),   &
        operator(<=),  &
        or,            &
        xor
!! Public procedures

    public :: error_handler

    type, abstract :: bitset_type
!! version: experimental
!!
!! Parent type for bitset_64 and bitset_large ([Specification](../page/specs/stdlib_bitsets.html#the-stdlib_bitsets-derived-types))

        private
        integer(bits_kind) :: num_bits = 0_bits_kind

    contains

        procedure(all_abstract), deferred, pass(self)         :: all
        procedure(any_abstract), deferred, pass(self)         :: any
        procedure(bit_count_abstract), deferred, pass(self)   :: bit_count
        procedure, pass(self)                                 :: bits
        procedure(clear_bit_abstract), deferred, pass(self)   :: clear_bit
        procedure(clear_range_abstract), deferred, pass(self) :: clear_range
        generic :: clear => clear_bit, clear_range
        procedure(flip_bit_abstract), deferred, pass(self)    :: flip_bit
        procedure(flip_range_abstract), deferred, pass(self)  :: flip_range
        generic :: flip => flip_bit, flip_range
        procedure(from_string_abstract), deferred, pass(self) :: from_string
        procedure(init_zero_abstract), deferred, pass(self)   :: init_zero
        generic :: init => init_zero
        procedure(input_abstract), deferred, pass(self)       :: input
        procedure(none_abstract), deferred, pass(self)        :: none
        procedure(not_abstract), deferred, pass(self)         :: not
        procedure(output_abstract), deferred, pass(self)      :: output
        procedure(read_bitset_string_abstract), deferred, pass(self) :: &
            read_bitset_string
        procedure(read_bitset_unit_abstract), deferred, pass(self) :: &
            read_bitset_unit
        generic :: read_bitset => read_bitset_string, read_bitset_unit
        procedure(set_bit_abstract), deferred, pass(self)     :: set_bit
        procedure(set_range_abstract), deferred, pass(self)   :: set_range
        generic :: set => set_bit, set_range
        procedure(test_abstract), deferred, pass(self)        :: test
        procedure(to_string_abstract), deferred, pass(self)   :: to_string
        procedure(value_abstract), deferred, pass(self)       :: value
        procedure(write_bitset_string_abstract), deferred, pass(self) :: &
            write_bitset_string
        procedure(write_bitset_unit_abstract), deferred, pass(self) :: &
            write_bitset_unit
        generic :: write_bitset => write_bitset_string, write_bitset_unit

    end type bitset_type


    abstract interface

        elemental function all_abstract( self ) result(all)
!! Version: experimental
!!
!! Returns `.true.` if all bits in `self` are 1, `.false.` otherwise.
!!
!!#### Example
!!
!!```fortran
!!    program example_all
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_all = '111111111111111111111111111111111'
!!        type(bitset_64) :: set0
!!        call set0 % from_string( bits_all )
!!        if ( bits(set0) /= 33 ) then
!!            error stop "FROM_STRING failed to interpret " // &
!!                'BITS_ALL's size properly."
!!        else if ( .not. set0 % all() ) then
!!            error stop "FROM_STRING failed to interpret" // &
!!                "BITS_ALL's value properly."
!!        else
!!            write(*,*) "FROM_STRING transferred BITS_ALL properly" // &
!!                " into set0."
!!        end if
!!    end program example_all
!!```
            import :: bitset_type
            logical                        :: all
            class(bitset_type), intent(in) :: self
        end function all_abstract

        elemental function any_abstract(self) result(any)
!! Version: experimental
!!
!! Returns `.true.` if any bit in `self` is 1, `.false.` otherwise.
!!
!!#### Example
!!
!!```fortran
!!    program example_any
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_0 = '0000000000000000000'
!!        type(bitset_64) :: set0
!!        call set0 % from_string( bits_0 )
!!        if ( .not. set0 % any() ) then
!!            write(*,*) "FROM_STRING interpreted " // &
!!                "BITS_0's value properly."
!!        end if
!!        call set0 % set(5)
!!        if ( set0 % any() ) then
!!            write(*,*) "ANY interpreted SET0's value properly."
!!        end if
!!    end program example_any
!!```
            import :: bitset_type
            logical                        :: any
            class(bitset_type), intent(in) :: self
        end function any_abstract

        elemental function bit_count_abstract(self) result(bit_count)
!! Version: experimental
!!
!! Returns the number of non-zero bits in `self`.
!!
!!#### Example
!!
!!```fortran
!!    program example_bit_count
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_0 = '0000000000000000000'
!!        type(bitset_64) :: set0
!!        call set0 % from_string( bits_0 )
!!        if ( set0 % bit_count() == 0 ) then
!!            write(*,*) "FROM_STRING interpreted " // &
!!                "BITS_0's value properly."
!!        end if
!!        call set0 % set(5)
!!        if ( set0 % bit_count() == 1 ) then
!!            write(*,*) "BIT_COUNT interpreted SET0's value properly."
!!        end if
!!    end program example_bit_count
!!```
            import :: bitset_type, bits_kind
            integer(bits_kind)             ::  bit_count
            class(bitset_type), intent(in) :: self
        end function bit_count_abstract

        elemental subroutine clear_bit_abstract(self, pos)
!! Version: experimental
!!
!! Sets to zero the `pos` position in `self`. If `pos` is less than zero or
!! greater than `bits(self)-1` it is ignored.
!!
!!#### Example
!!
!!```fortran
!!    program example_clear
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        call set0 % not()
!!        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
!!        call set0 % clear(165)
!!        if ( .not. set0 % test(165) ) write(*,*) 'Bit 165 is cleared.'
!!        call set0 % clear(0,164)
!!        if ( set0 % none() ) write(*,*) 'All bits are cleared.'
!!    end program example_clear
!!```
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: pos
        end subroutine clear_bit_abstract

        pure subroutine clear_range_abstract(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets to zero all bits from the `start_pos` to `stop_pos` positions in `set`.
!! If `stop_pos < start_pos` then no bits are modified. Positions outside
!! the range 0 to `bits(self)-1` are ignored.
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: start_pos, stop_pos
        end subroutine clear_range_abstract

        elemental subroutine flip_bit_abstract(self, pos)
!! Version: experimental
!!
!! Flips the value at the `pos` position in `self`, provided the position is
!! valid. If `pos` is less than 0 or greater than `bits(self)-1`, no value is
!! changed.
!!
!!#### Example
!!
!!```fortran
!!    program example_flip
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
!!        call set0 % flip(165)
!!        if ( set0 % test(165) ) write(*,*) 'Bit 165 is flipped.'
!!        call set0 % flip(0,164)
!!        if ( set0 % all() ) write(*,*) 'All bits are flipped.'
!!    end program example_flip
!!```
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: pos
        end subroutine flip_bit_abstract

        pure subroutine flip_range_abstract(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Flips all valid bits from the `start_pos` to the `stop_pos` positions in
!! `self`. If `stop_pos < start_pos` no bits are flipped. Positions less than
!! 0 or greater than `bits(self)-1` are ignored.
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: start_pos, stop_pos
        end subroutine flip_range_abstract

        subroutine from_string_abstract(self, string, status)
!! Version: experimental
!!
!! Initializes the bitset `self` treating `string` as a binary literal
!! `status` may have the values:
!! * `success` - if no problems were found,
!! * `alloc_fault` - if allocation of the bitset failed
!! * `char_string_too_large_error` - if `string` was too large, or
!! * `char_string_invalid_error` - if string had an invalid character.
!!
!!#### Example
!!
!!```fortran
!!    program example_from_string
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_all = '111111111111111111111111111111111'
!!        type(bitset_64) :: set0
!!        call set0 % from_string( bits_all )
!!        if ( bits(set0) /= 33 ) then
!!            error stop "FROM_STRING failed to interpret " // &
!!                'BITS_ALL's size properly."
!!        else if ( .not. set0 % all() ) then
!!            error stop "FROM_STRING failed to interpret" // &
!!                "BITS_ALL's value properly."
!!        else
!!            write(*,*) "FROM_STRING transferred BITS_ALL properly" // &
!!                " into set0."
!!        end if
!!    end program example_from_string
!!```
            import :: bitset_type
            class(bitset_type), intent(out) :: self
            character(*), intent(in)        :: string
            integer, intent(out), optional  :: status
        end subroutine from_string_abstract

        subroutine init_zero_abstract(self, bits, status)
!! Creates the bitset, `self`, of size `bits`, with all bits initialized to
!! zero. `bits` must be non-negative.  If an error occurs and `status` is
!! absent then processing stops with an informative stop code. `status`
!! will have one of the values;
!! * `success` -  if no problems were found,
!! * `alloc_fault` - if memory allocation failed
!! * `array_size_invalid_error` - if `bits` is either negative or larger
!!   than 64 with `self` of class `bitset_64`, or
!!
!!#### Example
!!
!!```fortran
!!    program example_init
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        if ( set0 % bits() == 166 ) &
!!            write(*,*) `SET0 has the proper size.'
!!        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
!!    end program example_init
!!```
            import :: bitset_type, bits_kind
            class(bitset_type), intent(out) :: self
            integer(bits_kind), intent(in)  :: bits
            integer, intent(out), optional  :: status
        end subroutine init_zero_abstract

        subroutine input_abstract(self, unit, status)
!! Version: experimental
!!
!! Reads the components of the bitset, `self`, from the unformatted I/O
!! unit, `unit`, assuming that the components were written using `output`.
!! If an error occurs and `status` is absent then processing stops with
!! an informative stop code. `status` has one of the values:
!! * `success` - if no problem was found
!! * `alloc_fault` - if it failed allocating memory for `self`, or
!! * `array_size_invalid_error` if the `bits(self)` in `unit` is negative
!!   or greater than 64 for a `bitset_64` input.
!! * `read_failure` - if it failed during the reads from `unit`
!!
!!#### Example
!!
!!```fortran
!!    program example_input
!!        character(*), parameter :: &
!!            bits_0   = '000000000000000000000000000000000', &
!!            bits_1   = '000000000000000000000000000000001', &
!!            bits_33  = '100000000000000000000000000000000'
!!        integer :: unit
!!        type(bitset_64) :: set0, set1, set2, set3, set4, set5
!!        call set0 % from_string( bits_0 )
!!        call set1 % from_string( bits_1 )
!!        call set2 % from_string( bits_33 )
!!        open( newunit=unit, file='test.bin', status='replace', &
!!            form='unformatted', action='write' )
!!        call set2 % output(unit)
!!        call set1 % output(unit)
!!        call set0 % output(unit)
!!        close( unit )
!!        open( newunit=unit, file='test.bin', status='old', &
!!            form='unformatted', action='read' )
!!        call set5 % input(unit)
!!        call set4 % input(unit)
!!        call set3 % input(unit)
!!        close( unit )
!!        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
!!            error stop 'Transfer to and from units using ' // &
!!                ' output and input failed.'
!!        else
!!            write(*,*) 'Transfer to and from units using ' // &
!!                'output and input succeeded.'
!!        end if
!!    end program example_input
!!```
            import :: bitset_type
            class(bitset_type), intent(out) :: self
            integer, intent(in)             :: unit
            integer, intent(out), optional  :: status
        end subroutine input_abstract

        elemental function none_abstract(self) result(none)
!! Version: experimental
!!
!! Returns `.true.` if none of the bits in `self` have the value 1.
!!
!!#### Example
!!
!!```fortran
!!    program example_none
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_0 = '0000000000000000000'
!!        type(bitset_large) :: set0
!!        call set0 % from_string( bits_0 )
!!        if ( set0 % none() ) then
!!            write(*,*) "FROM_STRING interpreted " // &
!!                "BITS_0's value properly."
!!        end if
!!        call set0 % set(5)
!!        if ( .not. set0 % none() ) then
!!            write(*,*) "NONE interpreted SET0's value properly."
!!        end if
!!    end program example_none
!!```
            import :: bitset_type
            logical ::  none
            class(bitset_type), intent(in) :: self
        end function none_abstract

        elemental subroutine not_abstract(self)
!! Version: experimental
!!
!! Sets the bits in `self` to their logical complement
!!
!!#### Example
!!
!!```fortran
!!    program example_not
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init( 155 )
!!        if ( set0 % none() ) then
!!            write(*,*) "FROM_STRING interpreted " // &
!!                "BITS_0's value properly."
!!        end if
!!        call set0 % not()
!!        if ( set0 % all() ) then
!!            write(*,*) "ALL interpreted SET0's value properly."
!!        end if
!!    end program example_not
!!```
            import :: bitset_type
            class(bitset_type), intent(inout) :: self
        end subroutine not_abstract

        subroutine output_abstract(self, unit, status)
!! Version: experimental
!!
!! Writes the components of the bitset, `self`, to the unformatted I/O
!! unit, `unit`, in a unformatted sequence compatible with `input`. If
!! `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success`, or the value `write_failure` if the write failed.
!!
!!#### Example
!!
!!```fortran
!!    program example_output
!!        character(*), parameter :: &
!!            bits_0   = '000000000000000000000000000000000', &
!!            bits_1   = '000000000000000000000000000000001', &
!!            bits_33  = '100000000000000000000000000000000'
!!        integer :: unit
!!        type(bitset_64) :: set0, set1, set2, set3, set4, set5
!!        call set0 % from_string( bits_0 )
!!        call set1 % from_string( bits_1 )
!!        call set2 % from_string( bits_33 )
!!        open( newunit=unit, file='test.bin', status='replace', &
!!            form='unformatted', action='write' )
!!        call set2 % output(unit)
!!        call set1 % output(unit)
!!        call set0 % output(unit)
!!        close( unit )
!!        open( newunit=unit, file='test.bin', status='old', &
!!            form='unformatted', action='read' )
!!        call set5 % input(unit)
!!        call set4 % input(unit)
!!        call set3 % input(unit)
!!        close( unit )
!!        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
!!            error stop 'Transfer to and from units using ' // &
!!                ' output and input failed.'
!!        else
!!            write(*,*) 'Transfer to and from units using ' // &
!!                'output and input succeeded.'
!!        end if
!!    end program example_output
!!```
            import :: bitset_type
            class(bitset_type), intent(in) :: self
            integer, intent(in)            :: unit
            integer, intent(out), optional :: status
        end subroutine output_abstract

        subroutine read_bitset_string_abstract(self, string, status)
!! Version: experimental
!!
!! Uses the bitset literal in the default character `string`, to define
!! the bitset, `self`. The literal may be preceded by an an arbitrary
!! sequence of blank characters. If `status` is absent an error results
!! in an error stop with an informative stop code. If `status`
!! is present it has one of the values
!! * `success` - if no problems occurred,
!! * `alloc_fault` - if allocation of memory for SELF failed,
!! * `array_size_invalid_error - if `bits(self)` in `string` is greater
!!   than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the bitset literal has an invalid
!!   character,
!! * `char_string_too_small_error - if the string ends before all the bits
!!   are read.
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
!!
!!#### Example
!!
!!```fortran
!!    program example_read_bitset
!!        character(*), parameter :: &
!!            bits_0   = 'S33B000000000000000000000000000000000', &
!!            bits_1   = 'S33B000000000000000000000000000000001', &
!!            bits_33  = 'S33B100000000000000000000000000000000'
!!        character(:), allocatable :: test_0, test_1, test_2
!!        integer :: unit
!!        type(bitset_64) :: set0, set1, set2, set3, set4, set5
!!        call set0 % read_bitset( bits_0, status )
!!        call set1 % read_bitset( bits_1, status )
!!        call set2 % read_bitset( bits_2, status )
!!        call set0 % write_bitset( test_0, status )
!!        call set1 % write_bitset( test_1, status )
!!        call set2 % write_bitset( test_2, status )
!!        if ( bits_0 == test_0 .and. bits_1 == test_1 .and. &
!!            bits_2 == test_2 ) then
!!            write(*,*) 'READ_BITSET to WRITE_BITSET strings worked.'
!!        end if
!!        open( newunit=unit, file='test.txt', status='replace', &
!!            form='formatted', action='write' )
!!        call set2 % write_bitset(unit, advance='no')
!!        call set1 % write_bitset(unit, advance='no')
!!        call set0 % write_bitset(unit)
!!        close( unit )
!!        open( newunit=unit, file='test.txt', status='old', &
!!            form='formatted', action='read' )
!!        call set3 % read_bitset(unit, advance='no')
!!        call set4 % read_bitset(unit, advance='no')
!!        call set5 % read_bitset(unit)
!!        if ( set3 == set0 .and. set4 == set1 .and. set5 == set2 ) then
!!            write(*,*) WRITE_BITSET to READ_BITSET through unit worked.'
!!        end if
!!    end program example_read_bitset
!!```
            import :: bitset_type
            class(bitset_type), intent(out) :: self
            character(len=*), intent(in)    :: string
            integer, intent(out), optional  :: status
        end subroutine read_bitset_string_abstract

        subroutine read_bitset_unit_abstract(self, unit, advance, status)
!! Version: experimental
!!
!! Uses the bitset literal at the current position in the formatted
!! file with I/O unit, `unit`, to define the bitset, `self`. The literal
!! may be preceded by an an arbitrary sequence of blank characters.
!! If `advance` is present it must be either 'YES' or 'NO'. If absent
!! it has the default value of 'YES' to determine whether advancing
!! I/O occurs. If `status` is absent an error results in an error stop
!! with an informative stop code. If `status` is present it has one of
!! the values:
!! * `success` - if no problem occurred,
!! * `alloc_fault` - if allocation of `self` failed,
!! * `array_size_invalid_error` - if `bits(self)` in the bitset literal
!!   is greater than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the read of the bitset literal found
!!   an invalid character,
!! * `eof_failure` - if a `read` statement reached an end-of-file before
!!   completing the read of the bitset literal,
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
!! * `read_failure` - if a `read` statement fails,
!
            import :: bitset_type
            class(bitset_type), intent(out)    :: self
            integer, intent(in)                :: unit
            character(*), intent(in), optional :: advance
            integer, intent(out), optional     :: status
        end subroutine read_bitset_unit_abstract

        elemental subroutine set_bit_abstract(self, pos)
!! Version: experimental
!!
!! Sets the value at the `pos` position in `self`, provided the position is
!! valid. If the position is less than 0 or greater than `bits(self)-1`
!! then `self` is unchanged.
!!
!!#### Example
!!
!!```fortran
!!    program example_set
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
!!        call set0 % set(165)
!!        if ( set0 % test(165) ) write(*,*) 'Bit 165 is set.'
!!        call set0 % set(0,164)
!!        if ( set0 % all() ) write(*,*) 'All bits are set.'
!!    end program example_set
!!```
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: pos
        end subroutine set_bit_abstract

        pure subroutine set_range_abstract(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets all valid bits to 1 from the `start_pos` to the `stop_pos` positions
!! in `self`. If `stop_pos < start_pos` no bits are changed. Positions outside
!! the range 0 to `bits(self)-1` are ignored.
            import :: bitset_type, bits_kind
            class(bitset_type), intent(inout) :: self
            integer(bits_kind), intent(in)    :: start_pos, stop_pos
        end subroutine set_range_abstract

        elemental function test_abstract(self, pos) result(test)
!! Version: experimental
!!
!! Returns `.true.` if the `pos` position is set, `.false.` otherwise. If `pos`
!! is negative or greater than `bits(self) - 1` the result is `.false.`.
!!
!!#### Example
!!
!!```fortran
!!    program example_test
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        call set0 % not()
!!        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
!!        call set0 % clear(165)
!!        if ( .not. set0 % test(165) ) write(*,*) 'Bit 165 is cleared.'
!!        call set0 % set(165)
!!        if ( set0 % test(165) ) write(*,*) 'Bit 165 is set.'
!!    end program example_test
!!```
            import :: bitset_type, bits_kind
            logical ::  test
            class(bitset_type), intent(in) :: self
            integer(bits_kind), intent(in) :: pos
        end function test_abstract

        subroutine to_string_abstract(self, string, status)
!! Version: experimental
!!
!! Represents the value of `self` as a binary literal in `string`
!! Status may have the values `success` or `alloc_fault`.
!!
!!#### Example
!!
!!```fortran
!!    program example_to_string
!!        use stdlib_bitsets
!!        character(*), parameter :: &
!!            bits_all = '111111111111111111111111111111111'
!!        type(bitset_64) :: set0
!!        character(:), allocatable :: new_string
!!        call set0 % init(33)
!!        call set0 % not()
!!        call set0 % to_string( new_string )
!!        if ( new_string == bits_all ) then
!!            write(*,*) "TO_STRING transferred BITS0 properly" // &
!!                " into NEW_STRING."
!!        end if
!!    end program example_to_string
!!```
            import :: bitset_type
            class(bitset_type), intent(in)         :: self
            character(:), allocatable, intent(out) :: string
            integer, intent(out), optional         :: status
        end subroutine to_string_abstract

        elemental function value_abstract(self, pos) result(value)
!! Version: experimental
!!
!! Returns 1 if the `pos` position is set, 0 otherwise. If `pos` is negative
!! or greater than `bits(set) - 1` the result is 0.
!!
!!#### Example
!!
!!```fortran
!!    program example_value
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0
!!        call set0 % init(166)
!!        call set0 % not()
!!        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
!!        call set0 % clear(165)
!!        if ( set0 % value(165) == 0 ) write(*,*) 'Bit 165 is cleared.'
!!        call set0 % set(165)
!!        if ( set0 % value(165) == 1 ) write(*,*) 'Bit 165 is set.'
!!    end program example_value
!!```
            import :: bitset_type, bits_kind
            integer ::  value
            class(bitset_type), intent(in) :: self
            integer(bits_kind), intent(in) :: pos
        end function value_abstract

        subroutine write_bitset_string_abstract(self, string, status)
!! Version: experimental
!!
!! Writes a bitset literal to the allocatable default character `string`,
!! representing the individual bit values in the `bitset_type`, `self`.
!! If `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success`, or the value `alloc_fault` if allocation of
!! the output string failed.
!!
!!#### Example
!!
!!```fortran
!!    program example_write_bitset
!!        character(*), parameter :: &
!!            bits_0   = 'S33B000000000000000000000000000000000', &
!!            bits_1   = 'S33B000000000000000000000000000000001', &
!!            bits_33  = 'S33B100000000000000000000000000000000'
!!        character(:), allocatable :: test_0, test_1, test_2
!!        integer :: unit
!!        type(bitset_64) :: set0, set1, set2, set3, set4, set5
!!        call set0 % read_bitset( bits_0, status )
!!        call set1 % read_bitset( bits_1, status )
!!        call set2 % read_bitset( bits_2, status )
!!        call set0 % write_bitset( test_0, status )
!!        call set1 % write_bitset( test_1, status )
!!        call set2 % write_bitset( test_2, status )
!!        if ( bits_0 == test_0 .and. bits_1 == test_1 .and. &
!!            bits_2 == test_2 ) then
!!            write(*,*) 'READ_BITSET to WRITE_BITSET strings worked.'
!!        end if
!!        open( newunit=unit, file='test.txt', status='replace', &
!!            form='formatted', action='write' )
!!        call set2 % write_bitset(unit, advance='no')
!!        call set1 % write_bitset(unit, advance='no')
!!        call set0 % write_bitset(unit)
!!        close( unit )
!!        open( newunit=unit, file='test.txt', status='old', &
!!            form='formatted', action='read' )
!!        call set3 % read_bitset(unit, advance='no')
!!        call set4 % read_bitset(unit, advance='no')
!!        call set5 % read_bitset(unit)
!!        if ( set3 == set0 .and. set4 == set1 .and. set5 == set2 ) then
!!            write(*,*) WRITE_BITSET to READ_BITSET through unit worked.'
!!        end if
!!    end program example_write_bitset
!!```
            import :: bitset_type
            class(bitset_type), intent(in)             :: self
            character(len=:), allocatable, intent(out) :: string
            integer, intent(out), optional             :: status
        end subroutine write_bitset_string_abstract

        subroutine write_bitset_unit_abstract(self, unit, advance, &
            status)
!! Version: experimental
!!
!! Writes a bitset literal to the I/O unit, `unit`, representing the
!! individual bit values in the `bitset_t`, `self`. If an error occurs then
!! processing stops with a message to `error_unit`. By default or if
!! `advance` is present with the value 'YES', advancing output is used.
!! If `advance` is present with the value 'NO', then the current record
!! is not advanced by the write. If `status` is absent, an error results
!! in an error stop with an informative stop code. If `status` is
!! present it has the default value of `success`, the value
!! `alloc_fault` if allocation of the output string failed,
!! `write_failure` if the `write` statement outputting the literal failed.
            import :: bitset_type
            class(bitset_type), intent(in)         :: self
            integer, intent(in)                    :: unit
            character(len=*), intent(in), optional :: advance
            integer, intent(out), optional         :: status
        end subroutine write_bitset_unit_abstract

    end interface

    type, extends(bitset_type) :: bitset_large
!! Version: experimental
!!
!! Type for bitsets with more than 64 bits ([Specification](../page/specs/stdlib_bitsets.html#the-stdlib_bitsets-derived-types))

        private
        integer(block_kind), private, allocatable :: blocks(:)

    contains

        procedure, pass(self)  :: all => all_large
        procedure, pass(self)  :: any => any_large
        procedure, pass(self)  :: bit_count => bit_count_large
        procedure, pass(self)  :: clear_bit => clear_bit_large
        procedure, pass(self)  :: clear_range => clear_range_large
        procedure, pass(self)  :: flip_bit => flip_bit_large
        procedure, pass(self)  :: flip_range => flip_range_large
        procedure, pass(self)  :: from_string => from_string_large
        procedure, pass(self)  :: init_zero => init_zero_large
        procedure, pass(self)  :: input => input_large
        procedure, pass(self)  :: none => none_large
        procedure, pass(self)  :: not => not_large
        procedure, pass(self)  :: output => output_large
        procedure, pass(self)  :: &
            read_bitset_string => read_bitset_string_large
        procedure, pass(self)  :: read_bitset_unit => read_bitset_unit_large
        procedure, pass(self)  :: set_bit => set_bit_large
        procedure, pass(self)  :: set_range => set_range_large
        procedure, pass(self)  :: test => test_large
        procedure, pass(self)  :: to_string => to_string_large
        procedure, pass(self)  :: value => value_large
        procedure, pass(self)  :: &
            write_bitset_string => write_bitset_string_large
        procedure, pass(self)  :: write_bitset_unit => write_bitset_unit_large

    end type bitset_large


    interface

        elemental module function all_large( self ) result(all)
!! Version: experimental
!!
!! Returns `.true.` if all bits in `self` are 1, `.false.` otherwise.
            logical                         :: all
            class(bitset_large), intent(in) :: self
        end function all_large

        elemental module function any_large(self) result(any)
!! Version: experimental
!!
!! Returns `.true.` if any bit in `self` is 1, `.false.` otherwise.
            logical                         :: any
            class(bitset_large), intent(in) :: self
        end function any_large

        elemental module function bit_count_large(self) result(bit_count)
!! Version: experimental
!!
!! Returns the number of non-zero bits in `self`.
            integer(bits_kind)              ::  bit_count
            class(bitset_large), intent(in) :: self
        end function bit_count_large

        elemental module subroutine clear_bit_large(self, pos)
!! Version: experimental
!!
!! Sets to zero the bit at `pos` position in `self`. If `pos` is less than
!! zero or greater than `bits(self)-1` it is ignored.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: pos
        end subroutine clear_bit_large

        pure module subroutine clear_range_large(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets to zero all bits from the `start_pos` to `stop_pos` positions in `self`.
!! If `stop_pos < start_pos` then no bits are modified. Positions outside
!! the range 0 to `bits(set)-1` are ignored.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: start_pos, stop_pos
        end subroutine clear_range_large

        elemental module subroutine flip_bit_large(self, pos)
!! Version: experimental
!!
!! Flips the bit value at the `pos` position in `self`, provided the position is
!! valid. If `pos` is less than 0 or greater than `bits(self)-1`, no value is
!! changed.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: pos
        end subroutine flip_bit_large

        pure module subroutine flip_range_large(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Flips all valid bits from the `start_pos` to the `stop_pos` positions in
!! `self`. If `stop_pos < start_pos` no bits are flipped. Positions less than
!! 0 or greater than `bits(self)-1` are ignored.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: start_pos, stop_pos
        end subroutine flip_range_large

        module subroutine from_string_large(self, string, status)
!! Version: experimental
!!
!! Initializes the bitset `self` treating `string` as a binary literal
!! `status` may have the values:
!! * `success` - if no problems were found,
!! * `alloc_fault` - if allocation of the bitset failed
!! * `char_string_too_large_error` - if `string` was too large, or
!! * `char_string_invalid_error` - if string had an invalid character.
            class(bitset_large), intent(out) :: self
            character(*), intent(in)         :: string
            integer, intent(out), optional   :: status
        end subroutine from_string_large

        module subroutine init_zero_large(self, bits, status)
!! Version: experimental
!!
!! Creates the bitset, `self`, of size `bits`, with all bits initialized to
!! zero. `bits` must be non-negative.  If an error occurs and `status` is
!! absent then processing stops with an informative stop code. `status`
!! will have one of the values;
!! * `success` -  if no problems were found,
!! * `alloc_fault` - if memory allocation failed
!! * `array_size_invalid_error` - if `bits` is either negative or larger
!!   than 64 with `self` of class `bitset_64`, or
            class(bitset_large), intent(out) :: self
            integer(bits_kind), intent(in)   :: bits
            integer, intent(out), optional   :: status
        end subroutine init_zero_large

        module subroutine input_large(self, unit, status)
!! Version: experimental
!!
!! Reads the components of the bitset, `self`, from the unformatted I/O
!! unit, `unit`, assuming that the components were written using `output`.
!! If an error occurs and `status` is absent then processing stops with
!! an informative stop code. `status` has one of the values:
!! * `success` - if no problem was found
!! * `alloc_fault` - if it failed allocating memory for `self`, or
!! * `array_size_invalid_error` if the `bits(self)` in `unit` is negative
!!   or greater than 64 for a `bitset_64` input.
!! * `read_failure` - if it failed during the reads from `unit`
            class(bitset_large), intent(out) :: self
            integer, intent(in)              :: unit
            integer, intent(out), optional   :: status
        end subroutine input_large

        elemental module function none_large(self) result(none)
!! Version: experimental
!!
!! Returns `.true.` if none of the bits in `self` have the value 1.
            logical                         ::  none
            class(bitset_large), intent(in) :: self
        end function none_large

        elemental module subroutine not_large(self)
!! Version: experimental
!!
!! Sets the bits in `self` to their logical complement
            class(bitset_large), intent(inout) :: self
        end subroutine not_large

        module subroutine output_large(self, unit, status)
!! Version: experimental
!!
!! Writes the components of the bitset, `self`, to the unformatted I/O
!! unit, `unit`, in a unformatted sequence compatible with `input`. If
!! `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success`, or the value `write_failure` if the write failed.
            class(bitset_large), intent(in) :: self
            integer, intent(in)             :: unit
            integer, intent(out), optional  :: status
        end subroutine output_large

        module subroutine read_bitset_string_large(self, string, status)
!! Version: experimental
!!
!! Uses the bitset literal in the default character `string`, to define
!! the bitset, `self`. The literal may be preceded by an an arbitrary
!! sequence of blank characters. If `status` is absent an error results
!! in an error stop with an informative stop code. If `status`
!! is present it has one of the values
!! * `success` - if no problems occurred,
!! * `alloc_fault` - if allocation of memory for SELF failed,
!! * `array_size_invalid_error - if `bits(self)` in `string` is greater
!!   than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the bitset literal has an invalid
!!   character,
!! * `char_string_too_small_error - if the string ends before all the bits
!!   are read.
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
            class(bitset_large), intent(out) :: self
            character(len=*), intent(in)     :: string
            integer, intent(out), optional   :: status
        end subroutine read_bitset_string_large

        module subroutine read_bitset_unit_large(self, unit, advance, status)
!! Version: experimental
!!
!! Uses the bitset literal at the current position in the formatted
!! file with I/O unit, `unit`, to define the bitset, `self`. The literal
!! may be preceded by an an arbitrary sequence of blank characters.
!! If `advance` is present it must be either 'YES' or 'NO'. If absent
!! it has the default value of 'YES' to determine whether advancing
!! I/O occurs. If `status` is absent an error results in an error stop
!! with an informative stop code. If `status` is present it has one of
!! the values:
!! * `success` - if no problem occurred,
!! * `alloc_fault` - if allocation of `self` failed,
!! * `array_size_invalid_error` - if `bits(self)` in the bitset literal
!!   is greater than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the read of the bitset literal found
!!   an invalid character,
!! * `eof_failure` - if a `read` statement reached an end-of-file before
!!   completing the read of the bitset literal,
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
!! * `read_failure` - if a `read` statement fails,
            class(bitset_large), intent(out)   :: self
            integer, intent(in)                :: unit
            character(*), intent(in), optional :: advance
            integer, intent(out), optional     :: status
        end subroutine read_bitset_unit_large

        elemental module subroutine set_bit_large(self, pos)
!! Version: experimental
!!
!! Sets the value at the `pos` position in `self`, provided the position is
!! valid. If the position is less than 0 or greater than `bits(self)-1`
!! then `self` is unchanged.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: pos
        end subroutine set_bit_large

        pure module subroutine set_range_large(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets all valid bits to 1 from the `start_pos` to the `stop_pos` positions
!! in `self`. If `stop_pos < start_pos` no bits are changed. Positions outside
!! the range 0 to `bits(self)-1` are ignored.
            class(bitset_large), intent(inout) :: self
            integer(bits_kind), intent(in)     :: start_pos, stop_pos
        end subroutine set_range_large

        elemental module function test_large(self, pos) result(test)
!! Version: experimental
!!
!! Returns `.true.` if the `pos` position is set, `.false.` otherwise. If `pos`
!! is negative or greater than `bits(self) - 1` the result is `.false.`.
            logical ::  test
            class(bitset_large), intent(in) :: self
            integer(bits_kind), intent(in)  :: pos
        end function test_large

        module subroutine to_string_large(self, string, status)
!! Version: experimental
!!
!! Represents the value of `self` as a binary literal in `string`
!! Status may have the values `success` or `alloc_fault`.
            class(bitset_large), intent(in)            :: self
            character(len=:), allocatable, intent(out) :: string
            integer, intent(out), optional             :: status
        end subroutine to_string_large

        elemental module function value_large(self, pos) result(value)
!! Version: experimental
!!
!! Returns 1 if the `pos` position is set, 0 otherwise. If `pos` is negative
!! or greater than `bits(set) - 1` the result is 0.
            integer ::  value
            class(bitset_large), intent(in) :: self
            integer(bits_kind), intent(in)  :: pos
        end function value_large

        module subroutine write_bitset_string_large(self, string, status)
!! Version: experimental
!!
!! Writes a bitset literal to the allocatable default character `string`,
!! representing the individual bit values in the bitset_large, `self`.
!! If `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success, or the value `alloc_fault` if allocation of
!! the output string failed.
            class(bitset_large), intent(in)            :: self
            character(len=:), allocatable, intent(out) :: string
            integer, intent(out), optional             :: status
        end subroutine write_bitset_string_large

        module subroutine write_bitset_unit_large(self, unit, advance, status)
!! Version: experimental
!!
!! Writes a bitset literal to the I/O unit, `unit`, representing the
!! individual bit values in the bitset, `self`. By default or if
!! `advance` is present with the value 'YES', advancing output is used.
!! If `advance` is present with the value 'NO', then the current record
!! is not advanced by the write. If `status` is absent an error results
!! in an error stop with an informative stop code. If `status` is
!! present it has the default value of `success`, the value
!! `alloc_fault` if allocation of the output string failed, or
!! `write_failure` if the `write` statement outputting the literal failed.
            class(bitset_large), intent(in)        :: self
            integer, intent(in)                    :: unit
            character(len=*), intent(in), optional :: advance
            integer, intent(out), optional         :: status
        end subroutine write_bitset_unit_large

    end interface


    interface assignment(=)
!! Version: experimental
!!
!! Used to define assignment for `bitset_large`.
!! ([Specification](../page/specs/stdlib_bitsets.html#-compare-two-bitsets-to-determine-whether-the-bits-have-the-same-value))
!!
!!#### Example
!!
!!```fortran
!!    program example_assignment
!!        use stdlib_bitsets
!!        logical(int8)  :: logical1(64) = .true.
!!        logical(int32), allocatable :: logical2(:)
!!        type(bitset_64) :: set0, set1
!!        set0 = logical1
!!        if ( set0 % bits() /= 64 ) then
!!            error stop procedure // &
!!                ' initialization with logical(int8) failed to set' // &
!!                ' the right size.'
!!        else if ( .not. set0 % all() ) then
!!            error stop procedure // ' initialization with' // &
!!                ' logical(int8) failed to set the right values.'
!!        else
!!            write(*,*) 'Initialization with logical(int8) succeeded.'
!!        end if
!!        set1 = set0
!!        if ( set1 == set0 ) &
!!            write(*,*) 'Initialization by assignment succeeded'
!!        logical2 = set1
!!        if ( all( logical2 ) ) then
!!            write(*,*) 'Initialization of logical(int32) succeeded.'
!!        end if
!!    end program example_assignment
!!```


        pure module subroutine assign_logint8_large( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int8)` to a
!! `bitset_large`.
            type(bitset_large), intent(out) :: self
            logical(int8), intent(in)       :: logical_vector(:)
        end subroutine assign_logint8_large

        pure module subroutine logint8_assign_large( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int8)` from a
!! `bitset_large`.
            logical(int8), intent(out), allocatable :: logical_vector(:)
            type(bitset_large), intent(in)          :: set
        end subroutine logint8_assign_large
        pure module subroutine assign_logint16_large( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int16)` to a
!! `bitset_large`.
            type(bitset_large), intent(out) :: self
            logical(int16), intent(in)       :: logical_vector(:)
        end subroutine assign_logint16_large

        pure module subroutine logint16_assign_large( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int16)` from a
!! `bitset_large`.
            logical(int16), intent(out), allocatable :: logical_vector(:)
            type(bitset_large), intent(in)          :: set
        end subroutine logint16_assign_large
        pure module subroutine assign_logint32_large( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int32)` to a
!! `bitset_large`.
            type(bitset_large), intent(out) :: self
            logical(int32), intent(in)       :: logical_vector(:)
        end subroutine assign_logint32_large

        pure module subroutine logint32_assign_large( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int32)` from a
!! `bitset_large`.
            logical(int32), intent(out), allocatable :: logical_vector(:)
            type(bitset_large), intent(in)          :: set
        end subroutine logint32_assign_large
        pure module subroutine assign_logint64_large( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int64)` to a
!! `bitset_large`.
            type(bitset_large), intent(out) :: self
            logical(int64), intent(in)       :: logical_vector(:)
        end subroutine assign_logint64_large

        pure module subroutine logint64_assign_large( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int64)` from a
!! `bitset_large`.
            logical(int64), intent(out), allocatable :: logical_vector(:)
            type(bitset_large), intent(in)          :: set
        end subroutine logint64_assign_large

    end interface assignment(=)


    type, extends(bitset_type) :: bitset_64
!! Version: experimental
!!
!! Type for bitsets with no more than 64 bits ([Specification](../page/specs/stdlib_bitsets.html#the-stdlib_bitsets-derived-types))
        private
        integer(block_kind), private :: block = 0

    contains

        procedure, pass(self)  :: all => all_64
        procedure, pass(self)  :: any => any_64
        procedure, pass(self)  :: bit_count => bit_count_64
        procedure, pass(self)  :: clear_bit => clear_bit_64
        procedure, pass(self)  :: clear_range => clear_range_64
        procedure, pass(self)  :: flip_bit => flip_bit_64
        procedure, pass(self)  :: flip_range => flip_range_64
        procedure, pass(self)  :: from_string => from_string_64
        procedure, pass(self)  :: init_zero => init_zero_64
        procedure, pass(self)  :: input => input_64
        procedure, pass(self)  :: none => none_64
        procedure, pass(self)  :: not => not_64
        procedure, pass(self)  :: output => output_64
        procedure, pass(self)  :: read_bitset_string => read_bitset_string_64
        procedure, pass(self)  :: read_bitset_unit => read_bitset_unit_64
        procedure, pass(self)  :: set_bit => set_bit_64
        procedure, pass(self)  :: set_range => set_range_64
        procedure, pass(self)  :: test => test_64
        procedure, pass(self)  :: to_string => to_string_64
        procedure, pass(self)  :: value => value_64
        procedure, pass(self)  :: write_bitset_string => write_bitset_string_64
        procedure, pass(self)  :: write_bitset_unit => write_bitset_unit_64

    end type bitset_64


    interface

        elemental module function all_64( self ) result(all)
!! Version: experimental
!!
!! Returns `.true.` if all bits in `self` are 1, `.false.` otherwise.
            logical                      :: all
            class(bitset_64), intent(in) :: self
        end function all_64

        elemental module function any_64(self) result(any)
!! Version: experimental
!!
!! Returns `.true.` if any bit in `self` is 1, `.false.` otherwise.
            logical                      :: any
            class(bitset_64), intent(in) :: self
        end function any_64

        elemental module function bit_count_64(self) result(bit_count)
!! Version: experimental
!!
!! Returns the number of non-zero bits in `self`.
            integer(bits_kind)           :: bit_count
            class(bitset_64), intent(in) :: self
        end function bit_count_64

        elemental module subroutine clear_bit_64(self, pos)
!! Version: experimental
!!
!! Sets to zero the bit at `pos` position in `self`. If `pos` is less than
!! zero or greater than `bits(self)-1` it is ignored.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: pos
        end subroutine clear_bit_64

        pure module subroutine clear_range_64(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets to zero all bits from the `start_pos` to `stop_pos` positions in `self`.
!! If `stop_pos < start_pos` then no bits are modified. Positions outside
!! the range 0 to `bits(set)-1` are ignored.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: start_pos, stop_pos
        end subroutine clear_range_64

        elemental module subroutine flip_bit_64(self, pos)
!! Version: experimental
!!
!! Flips the bit value at the `pos` position in `self`, provided the position is
!! valid. If `pos` is less than 0 or greater than `bits(self)-1`, no value is
!! changed.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: pos
        end subroutine flip_bit_64

        pure module subroutine flip_range_64(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Flips all valid bits from the `start_pos` to the `stop_pos` positions in
!! `self`. If `stop_pos < start_pos` no bits are flipped. Positions less than
!! 0 or greater than `bits(self)-1` are ignored.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: start_pos, stop_pos
        end subroutine flip_range_64

        module subroutine from_string_64(self, string, status)
!! Version: experimental
!!
!! Initializes the bitset `self` treating `string` as a binary literal
!! `status` may have the values:
!! * `success` - if no problems were found,
!! * `alloc_fault` - if allocation of the bitset failed
!! * `char_string_too_large_error` - if `string` was too large, or
!! * `char_string_invalid_error` - if string had an invalid character.
            class(bitset_64), intent(out)  :: self
            character(*), intent(in)       :: string
            integer, intent(out), optional :: status
        end subroutine from_string_64

        module subroutine init_zero_64(self, bits, status)
!! Version: experimental
!!
!! Creates the bitset, `self`, of size `bits`, with all bits initialized to
!! zero. `bits` must be non-negative.  If an error occurs and `status` is
!! absent then processing stops with an informative stop code. `status`
!! will have one of the values:
!! * `success` -  if no problems were found,
!! * `alloc_fault` - if memory allocation failed
!! * `array_size_invalid_error` - if `bits` is either negative or larger
!!   than 64 with `self` of class `bitset_64`.
            class(bitset_64), intent(out)   :: self
            integer(bits_kind), intent(in) :: bits
            integer, intent(out), optional :: status
        end subroutine init_zero_64

        module subroutine input_64(self, unit, status)
!! Version: experimental
!!
!! Reads the components of the bitset, `self`, from the unformatted I/O
!! unit, `unit`, assuming that the components were written using `output`.
!! If an error occurs and `status` is absent then processing stops with
!! an informative stop code. `status` has one of the values:
!! * `success` - if no problem was found
!! * `alloc_fault` - if it failed allocating memory for `self`, or
!! * `array_size_invalid_error` if the `bits(self)` in `unit` is negative
!!   or greater than 64 for a `bitset_64` input.
!! * `read_failure` - if it failed during the reads from `unit`
            class(bitset_64), intent(out)  :: self
            integer, intent(in)            :: unit
            integer, intent(out), optional :: status
        end subroutine input_64

        elemental module function none_64(self) result(none)
!! Version: experimental
!!
!! Returns `.true.` if none of the bits in `self` have the value 1.
            logical ::  none
            class(bitset_64), intent(in) :: self
        end function none_64

        elemental module subroutine not_64(self)
!! Version: experimental
!!
!! Sets the bits in `self` to their logical complement.
            class(bitset_64), intent(inout) :: self
        end subroutine not_64

        module subroutine output_64(self, unit, status)
!! Version: experimental
!!
!! Writes the components of the bitset, `self`, to the unformatted I/O
!! unit, `unit`, in a unformatted sequence compatible with `input`. If
!! `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success`, or the value `write_failure` if the write failed.
            class(bitset_64), intent(in)   :: self
            integer, intent(in)            :: unit
            integer, intent(out), optional :: status
        end subroutine output_64

        module subroutine read_bitset_string_64(self, string, status)
!! Version: experimental
!!
!! Uses the bitset literal in the default character `string`, to define
!! the bitset, `self`. The literal may be preceded by an an arbitrary
!! sequence of blank characters. If `status` is absent an error results
!! in an error stop with an informative stop code. If `status`
!! is present it has one of the values:
!! * `success` - if no problems occurred,
!! * `alloc_fault` - if allocation of memory for SELF failed,
!! * `array_size_invalid_error - if `bits(self)` in `string` is greater
!!   than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the bitset literal has an invalid
!!   character,
!! * `char_string_too_small_error - if the string ends before all the bits
!!   are read.
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
            class(bitset_64), intent(out)  :: self
            character(len=*), intent(in)   :: string
            integer, intent(out), optional :: status
        end subroutine read_bitset_string_64

        module subroutine read_bitset_unit_64(self, unit, advance, status)
!! Version: experimental
!!
!! Uses the bitset literal at the current position in the formatted
!! file with I/O unit, `unit`, to define the bitset, `self`. The literal
!! may be preceded by an an arbitrary sequence of blank characters.
!! If `advance` is present it must be either 'YES' or 'NO'. If absent
!! it has the default value of 'YES' to determine whether advancing
!! I/O occurs. If `status` is absent an error results in an error stop
!! with an informative stop code. If `status` is present it has one of
!! the values:
!! * `success` - if no problem occurred,
!! * `alloc_fault` - if allocation of `self` failed,
!! * `array_size_invalid_error` - if `bits(self)` in the bitset literal
!!   is greater than 64 for a `bitset_64`,
!! * `char_string_invalid_error` - if the read of the bitset literal found
!!   an invalid character,
!! * `eof_failure` - if a `read` statement reached an end-of-file before
!!   completing the read of the bitset literal,
!! * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!!   value too large to be represented,
!! * `read_failure` - if a `read` statement fails,
            class(bitset_64), intent(out)      :: self
            integer, intent(in)                :: unit
            character(*), intent(in), optional :: advance
            integer, intent(out), optional     :: status
        end subroutine read_bitset_unit_64

        elemental module subroutine set_bit_64(self, pos)
!! Version: experimental
!!
!! Sets the value at the `pos` position in `self`, provided the position is
!! valid. If the position is less than 0 or greater than `bits(self)-1`
!! then `self` is unchanged.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: pos
        end subroutine set_bit_64

        pure module subroutine set_range_64(self, start_pos, stop_pos)
!! Version: experimental
!!
!! Sets all valid bits to 1 from the `start_pos` to the `stop_pos` positions
!! in `self`. If `stop_pos < start_pos` no bits are changed. Positions outside
!! the range 0 to `bits(self)-1` are ignored.
            class(bitset_64), intent(inout) :: self
            integer(bits_kind), intent(in)  :: start_pos, stop_pos
        end subroutine set_range_64

        elemental module function test_64(self, pos) result(test)
!! Version: experimental
!!
!! Returns `.true.` if the `pos` position is set, `.false.` otherwise. If `pos`
!! is negative or greater than `bits(self)-1` the result is `.false.`.
            logical ::  test
            class(bitset_64), intent(in)   :: self
            integer(bits_kind), intent(in) :: pos
        end function test_64

        module subroutine to_string_64(self, string, status)
!! Version: experimental
!!
!! Represents the value of `self` as a binary literal in `string`.
!! Status may have the values `success` or `alloc_fault`
            class(bitset_64), intent(in)               :: self
            character(len=:), allocatable, intent(out) :: string
            integer, intent(out), optional             :: status
        end subroutine to_string_64

        elemental module function value_64(self, pos) result(value)
!! Version: experimental
!!
!! Returns 1 if the `pos` position is set, 0 otherwise. If `pos` is negative
!! or greater than `bits(set)-1` the result is 0.
            integer ::  value
            class(bitset_64), intent(in)   :: self
            integer(bits_kind), intent(in) :: pos
        end function value_64

        module subroutine write_bitset_string_64(self, string, status)
!! Version: experimental
!!
!! Writes a bitset literal to the allocatable default character `string`,
!! representing the individual bit values in the `bitset_64`, `self`.
!! If `status` is absent an error results in an error stop with an
!! informative stop code. If `status` is present it has the default
!! value of `success`, or the value `alloc_fault` if allocation of
!! the output string failed.
            class(bitset_64), intent(in)               :: self
            character(len=:), allocatable, intent(out) :: string
            integer, intent(out), optional             :: status
        end subroutine write_bitset_string_64

        module subroutine write_bitset_unit_64(self, unit, advance, status)
!! Version: experimental
!!
!! Writes a bitset literal to the I/O unit, `unit`, representing the
!! individual bit values in the bitset, `self`. By default or if
!! `advance` is present with the value 'YES', advancing output is used.
!! If `advance` is present with the value 'NO', then the current record
!! is not advanced by the write. If `status` is absent an error results
!! in an error stop with an informative stop code. If `status` is
!! present it has the default value of `success`, the value
!! `alloc_fault` if allocation of the output string failed, or
!! `write_failure` if the `write` statement outputting the literal failed.
            class(bitset_64), intent(in)           :: self
            integer, intent(in)                    :: unit
            character(len=*), intent(in), optional :: advance
            integer, intent(out), optional         :: status
        end subroutine write_bitset_unit_64

    end interface


    interface assignment(=)


        module subroutine assign_logint8_64( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int8)` to a
!! `bitset_64`.
            type(bitset_64), intent(out) :: self
            logical(int8), intent(in)    :: logical_vector(:)
        end subroutine assign_logint8_64

        pure module subroutine logint8_assign_64( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int8)` from a
!! `bitset_64`.
            logical(int8), intent(out), allocatable :: logical_vector(:)
            type(bitset_64), intent(in)             :: set
        end subroutine logint8_assign_64
        module subroutine assign_logint16_64( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int16)` to a
!! `bitset_64`.
            type(bitset_64), intent(out) :: self
            logical(int16), intent(in)    :: logical_vector(:)
        end subroutine assign_logint16_64

        pure module subroutine logint16_assign_64( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int16)` from a
!! `bitset_64`.
            logical(int16), intent(out), allocatable :: logical_vector(:)
            type(bitset_64), intent(in)             :: set
        end subroutine logint16_assign_64
        module subroutine assign_logint32_64( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int32)` to a
!! `bitset_64`.
            type(bitset_64), intent(out) :: self
            logical(int32), intent(in)    :: logical_vector(:)
        end subroutine assign_logint32_64

        pure module subroutine logint32_assign_64( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int32)` from a
!! `bitset_64`.
            logical(int32), intent(out), allocatable :: logical_vector(:)
            type(bitset_64), intent(in)             :: set
        end subroutine logint32_assign_64
        module subroutine assign_logint64_64( self, logical_vector )
!! Version: experimental
!!
!! Used to define assignment from an array of type `logical(int64)` to a
!! `bitset_64`.
            type(bitset_64), intent(out) :: self
            logical(int64), intent(in)    :: logical_vector(:)
        end subroutine assign_logint64_64

        pure module subroutine logint64_assign_64( logical_vector, set )
!! Version: experimental
!!
!! Used to define assignment to an array of type `logical(int64)` from a
!! `bitset_64`.
            logical(int64), intent(out), allocatable :: logical_vector(:)
            type(bitset_64), intent(in)             :: set
        end subroutine logint64_assign_64

    end interface assignment(=)


    interface and
!! Version: experimental
!!
!! Sets the bits in `set1` to the bitwise `and` of the original bits in `set1`
!! and `set2`. The sets must have the same number of bits
!! otherwise the result is undefined.
!! ([Specification](../page/specs/stdlib_bitsets.html#and-bitwise-and-of-the-bits-of-two-bitsets))
!!
!!#### Example
!!
!!```fortran
!!    program example_and
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0, set1
!!        call set0 % init(166)
!!        call set1 % init(166)
!!        call and( set0, set1 ) ! none none
!!        if ( none(set0) ) write(*,*) 'First test of AND worked.'
!!        call set0 % not()
!!        call and( set0, set1 ) ! all none
!!        if ( none(set0) ) write(*,*) 'Second test of AND worked.'
!!        call set1 % not()
!!        call and( set0, set1 ) ! none all
!!        if ( none(set0) ) write(*,*) 'Third test of AND worked.'
!!        call set0 % not()
!!        call and( set0, set1 ) ! all all
!!        if ( all(set0) ) write(*,*) 'Fourth test of AND worked.'
!!    end program example_and
!!```
        elemental module subroutine and_large(set1, set2)
            type(bitset_large), intent(inout) :: set1
            type(bitset_large), intent(in)    :: set2
        end subroutine and_large

        elemental module subroutine and_64(set1, set2)
            type(bitset_64), intent(inout) :: set1
            type(bitset_64), intent(in)    :: set2
        end subroutine and_64

    end interface and


    interface and_not
!! Version: experimental
!!
!! Sets the bits in `set1` to the bitwise and of the original bits in `set1`
!! with the bitwise negation of `set2`. The sets must have the same
!! number of bits otherwise the result is undefined.
!!
!! ([Specification](../page/specs/stdlib_bitsets.html#and_not-bitwise-and-of-one-bitset-with-the-negation-of-another))
!!
!!#### Example
!!
!!```fortran
!!    program example_and_not
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0, set1
!!        call set0 % init(166)
!!        call set1 % init(166)
!!        call and_not( set0, set1 ) ! none none
!!        if ( none(set0) ) write(*,*) 'First test of AND_NOT worked.'
!!        call set0 % not()
!!        call and_not( set0, set1 ) ! all none
!!        if ( all(set0) ) write(*,*) 'Second test of AND_NOT worked.'
!!        call set0 % not()
!!        call set1 % not()
!!        call and_not( set0, set1 ) ! none all
!!        if ( none(set0) ) write(*,*) 'Third test of AND_NOT worked.'
!!        call set0 % not()
!!        call and_not( set0, set1 ) ! all all
!!        if ( none(set0) ) write(*,*) 'Fourth test of AND_NOT worked.'
!!    end program example_and_not
!!```

        elemental module subroutine and_not_large(set1, set2)
            type(bitset_large), intent(inout) :: set1
            type(bitset_large), intent(in)    :: set2
        end subroutine and_not_large

        elemental module subroutine and_not_64(set1, set2)
            type(bitset_64), intent(inout) :: set1
            type(bitset_64), intent(in)    :: set2
        end subroutine and_not_64

    end interface and_not

    interface extract
!! Version: experimental
!!
!! Creates a new bitset, `new`, from a range, `start_pos` to `stop_pos`, in
!! bitset `old`. If `start_pos` is greater than `stop_pos` the new bitset is
!! empty. If `start_pos` is less than zero or `stop_pos` is greater than
!! `bits(old)-1` then if `status` is present it has the value
!! `index_invalid_error` and `new` is undefined, otherwise processing stops
!! with an informative message.
!! ([Specification](../page/specs/stdlib_bitsets.html#extract-create-a-new-bitset-from-a-range-in-an-old-bitset))
!!
!!#### Example
!!
!!```fortran
!!    program example_extract
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0, set1
!!        call set0 % init(166)
!!        call set0 % set(100,150)
!!        call extract( set1, set0, 100, 150)
!!        if ( set1 % bits() == 51 ) &
!!            write(*,*) 'SET1 has the proper size.'
!!        if ( set1 % all() ) write(*,*) 'SET1 has the proper values.'
!!    end program example_extract
!!```

        module subroutine extract_large(new, old, start_pos, stop_pos, status)
            type(bitset_large), intent(out) :: new
            type(bitset_large), intent(in)  :: old
            integer(bits_kind), intent(in)  :: start_pos, stop_pos
            integer, intent(out), optional  :: status
        end subroutine extract_large

       module subroutine extract_64(new, old, start_pos, stop_pos, status)
            type(bitset_64), intent(out)   :: new
            type(bitset_64), intent(in)    :: old
            integer(bits_kind), intent(in) :: start_pos, stop_pos
            integer, intent(out), optional :: status
        end subroutine extract_64

    end interface extract


    interface or
!! Version: experimental
!!
!! Sets the bits in `set1` to the bitwise `or` of the original bits in `set1`
!! and `set2`. The sets must have the same number of bits otherwise
!! the result is undefined.
!! ([Specification](../page/specs/stdlib_bitsets.html#or-bitwise-or-of-the-bits-of-two-bitsets))
!!
!!#### Example
!!
!!```fortran
!!    program example_or
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0, set1
!!        call set0 % init(166)
!!        call set1 % init(166)
!!        call or( set0, set1 ) ! none none
!!        if ( none(set0) ) write(*,*) 'First test of OR worked.'
!!        call set0 % not()
!!        call or( set0, set1 ) ! all none
!!        if ( all(set0) ) write(*,*) 'Second test of OR worked.'
!!        call set0 % not()
!!        call set1 % not()
!!        call or( set0, set1 ) ! none all
!!        if ( all(set0) ) write(*,*) 'Third test of OR worked.'
!!        call set0 % not()
!!        call or( set0, set1 ) ! all all
!!        if ( all(set0) ) write(*,*) 'Fourth test of OR worked.'
!!    end program example_or
!!```
        elemental module subroutine or_large(set1, set2)
            type(bitset_large), intent(inout) :: set1
            type(bitset_large), intent(in)    :: set2
        end subroutine or_large

        elemental module subroutine or_64(set1, set2)
            type(bitset_64), intent(inout) :: set1
            type(bitset_64), intent(in)    :: set2
        end subroutine or_64

    end interface or


    interface xor
!! Version: experimental
!!
!! Sets the bits in `set1` to the bitwise `xor` of the original bits in `set1`
!! and `set2`. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#xor-bitwise-exclusive-or))
!!
!!#### Example
!!
!!```fortran
!!    program example_xor
!!        use stdlib_bitsets
!!        type(bitset_large) :: set0, set1
!!        call set0 % init(166)
!!        call set1 % init(166)
!!        call xor( set0, set1 ) ! none none
!!        if ( none(set0) ) write(*,*) 'First test of XOR worked.'
!!        call set0 % not()
!!        call xor( set0, set1 ) ! all none
!!        if ( all(set0) ) write(*,*) 'Second test of XOR worked.'
!!        call set0 % not()
!!        call set1 % not()
!!        call xor( set0, set1 ) ! none all
!!        if ( all(set0) ) write(*,*) 'Third test of XOR worked.'
!!        call set0 % not()
!!        call xor( set0, set1 ) ! all all
!!        if ( none(set0) ) write(*,*) 'Fourth test of XOR worked.'
!!    end program example_xor
!!```
        elemental module subroutine xor_large(set1, set2)
            type(bitset_large), intent(inout) :: set1
            type(bitset_large), intent(in)    :: set2
        end subroutine xor_large

        elemental module subroutine xor_64(set1, set2)
            type(bitset_64), intent(inout) :: set1
            type(bitset_64), intent(in)    :: set2
        end subroutine xor_64

    end interface xor


    interface operator(==)
!! Version: experimental
!!
!! Returns `.true.` if all bits in `set1` and `set2` have the same value,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#-compare-two-bitsets-to-determine-whether-the-bits-have-the-same-value))
!!
!!#### Example
!!
!!```fortran
!!    program example_equality
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set0 == set0 .and. set1 == set1 .and. set2 == set2 .and. &
!!            .not. set0 == set1 .and. .not. set0 == set2 .and. .not.   &
!!            set1 == set2 ) then
!!            write(*,*) 'Passed 64 bit equality tests.'
!!        else
!!            error stop 'Failed 64 bit equality tests.'
!!        end if
!!    end program example_equality
!!```
        elemental module function eqv_large(set1, set2) result(eqv)
            logical                        :: eqv
            type(bitset_large), intent(in) :: set1, set2
        end function eqv_large

        elemental module function eqv_64(set1, set2) result(eqv)
            logical                     :: eqv
            type(bitset_64), intent(in) :: set1, set2
        end function eqv_64

    end interface operator(==)


    interface operator(/=)
!! Version: experimental
!!
!! Returns `.true.` if not all bits in `set1` and `set2` have the same value,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#-compare-two-bitsets-to-determine-whether-any-bits-differ-in-value))
!!
!!#### Example
!!
!!```fortran
!!    program example_inequality
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set0 /= set1 .and. set0 /= set2 .and. set1 /= set2 .and. &
!!            .not. set0 /= set0 .and. .not. set1 /= set1 .and. .not.   &
!!            set2 /= set2 ) then
!!            write(*,*) 'Passed 64 bit inequality tests.'
!!        else
!!            error stop 'Failed 64 bit inequality tests.'
!!        end if
!!    end program example_inequality
!!```
        elemental module function neqv_large(set1, set2) result(neqv)
            logical                        :: neqv
            type(bitset_large), intent(in) :: set1, set2
        end function neqv_large

        elemental module function neqv_64(set1, set2) result(neqv)
            logical                     :: neqv
            type(bitset_64), intent(in) :: set1, set2
        end function neqv_64

    end interface operator(/=)


    interface operator(>)
!! Version: experimental
!!
!! Returns `.true.` if the bits in `set1` and `set2` differ and the
!! highest order different bit is set to 1 in `set1` and to 0 in `set2`,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#gt-compare-two-bitsets-to-determine-whether-the-first-is-greater-than-the-other))
!!
!!#### Example
!!
!!```fortran
!!    program example_gt
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set1 > set0 .and. set2 > set1 .and. set2 > set0 .and. &
!!            .not. set0 > set0 .and. .not. set0 > set1 .and. .not.   &
!!            set1 > set2 ) then
!!            write(*,*) 'Passed 64 bit greater than tests.'
!!        else
!!            error stop 'Failed 64 bit greater than tests.'
!!        end if
!!    end program example_gt
!!```
        elemental module function gt_large(set1, set2) result(gt)
            logical                        :: gt
            type(bitset_large), intent(in) :: set1, set2
        end function gt_large

        elemental module function gt_64(set1, set2) result(gt)
            logical                     :: gt
            type(bitset_64), intent(in) :: set1, set2
        end function gt_64

    end interface operator(>)


    interface operator(>=)
!! Version: experimental
!!
!! Returns `.true.` if the bits in `set1` and `set2` are the same or the
!! highest order different bit is set to 1 in `set1` and to 0 in `set2`,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!! ([Specification](../page/specs/stdlib_bitsets.html#gt-compare-two-bitsets-to-determine-whether-the-first-is-greater-than-or-equal-to-the-second))
!!
!!#### Example
!!
!!```fortran
!!    program example_ge
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set1 >= set0 .and. set2 >= set1 .and. set2 >= set0 .and. &
!!            set0 >= set0 .and. set1 >= set1 .and. set2 >= set2 .and. &
!!            .not. set0 >= set1 .and. .not. set0 >= set2 .and. .not.   &
!!            set1 >= set2 ) then
!!            write(*,*) 'Passed 64 bit greater than or equals tests.'
!!        else
!!            error stop 'Failed 64 bit greater than or equals tests.'
!!        end if
!!    end program example_ge
!!```
        elemental module function ge_large(set1, set2) result(ge)
            logical                        :: ge
            type(bitset_large), intent(in) :: set1, set2
        end function ge_large

        elemental module function ge_64(set1, set2) result(ge)
            logical                     :: ge
            type(bitset_64), intent(in) :: set1, set2
        end function ge_64

    end interface operator(>=)


    interface operator(<)
!! Version: experimental
!!
!! Returns `.true.` if the bits in `set1` and `set2` differ and the
!! highest order different bit is set to 0 in `set1` and to 1 in `set2`,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#lt-compare-two-bitsets-to-determine-whether-the-first-is-less-than-the-other))
!!
!!#### Example
!!
!!```fortran
!!    program example_lt
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set0 < set1 .and. set1 < set2 .and. set0 < set2 .and. &
!!            .not. set0 < set0 .and. .not. set2 < set0 .and. .not.   &
!!            set2 < set1 ) then
!!            write(*,*) 'Passed 64 bit less than tests.'
!!        else
!!            error stop 'Failed 64 bit less than tests.'
!!        end if
!!    end program example_lt
!!```
        elemental module function lt_large(set1, set2) result(lt)
            logical                        :: lt
            type(bitset_large), intent(in) :: set1, set2
        end function lt_large

        elemental module function lt_64(set1, set2) result(lt)
            logical                     :: lt
            type(bitset_64), intent(in) :: set1, set2
        end function lt_64

    end interface operator(<)


    interface operator(<=)
!! Version: experimental
!!
!! Returns `.true.` if the bits in `set1` and `set2` are the same or the
!! highest order different bit is set to 0 in `set1` and to 1 in `set2`,
!! `.false.`  otherwise. The sets must have the same number of bits
!! otherwise the result is undefined.
!!([Specification](../page/specs/stdlib_bitsets.html#lt-compare-two-bitsets-to-determine-whether-the-first-is-less-than-or-equal-to-the-other))
!!
!!#### Example
!!
!!```fortran
!!    program example_le
!!        use stdlib_bitsets
!!        type(bitset_64) :: set0, set1, set2
!!        call set0 % init( 33 )
!!        call set1 % init( 33 )
!!        call set2 % init( 33 )
!!        call set1 % set( 0 )
!!        call set2 % set( 32 )
!!        if ( set0 <= set1 .and. set1 <= set2 .and. set0 <= set2 .and. &
!!            set0 <= set0 .and. set1 <= set1 .and. set2 <= set2 .and. &
!!            .not. set1 <= set0 .and. .not. set2 <= set0 .and. .not.   &
!!            set2 <= set1 ) then
!!            write(*,*) 'Passed 64 bit less than or equal tests.'
!!        else
!!            error stop 'Failed 64 bit less than or equal tests.'
!!        end if
!!    end program example_le
!!```
        elemental module function le_large(set1, set2) result(le)
            logical                        :: le
            type(bitset_large), intent(in) :: set1, set2
        end function le_large

        elemental module function le_64(set1, set2) result(le)
            logical                     :: le
            type(bitset_64), intent(in) :: set1, set2
        end function le_64

    end interface operator(<=)

    interface error_handler
        module subroutine error_handler( message, error, status, &
            module, procedure )
            character(*), intent(in)           :: message
            integer, intent(in)                :: error
            integer, intent(out), optional     :: status
            character(*), intent(in), optional :: module
            character(*), intent(in), optional :: procedure
        end subroutine error_handler
    end interface error_handler

contains

    elemental function bits(self)
!! Version: experimental
!!
!! Returns the number of bit positions in `self`.
       integer(bits_kind)             :: bits
       class(bitset_type), intent(in) :: self

       bits = self % num_bits

       return
    end function bits

    module subroutine error_handler( message, error, status, module, procedure )
        character(*), intent(in)           :: message
        integer, intent(in)                :: error
        integer, intent(out), optional     :: status
        character(*), intent(in), optional :: module
        character(*), intent(in), optional :: procedure

        if ( present(status) ) then
            status = error
        else
            if ( present(module) ) then
                if ( present(procedure) ) then
                    write(error_unit, '(a)') trim(module) // ' % ' // &
                        trim(procedure) // ': ' // trim(message)
                else
                    write(error_unit, '(a)') trim(module) // ' % N/A: ' // &
                        trim(message)
                end if
            else if ( present(procedure) ) then
                write(error_unit, '(a)') trim(procedure) // ': ' // &
                    trim(message)
            else
                write(error_unit, '(a)') trim(message)
            end if
            select case(error)
            case( alloc_fault )
                error stop 'A memory allocation failed.'
            case( array_size_invalid_error )
                error stop "An array size was invalid."
            case( char_string_invalid_error )
                error stop "A character string had an invalid character."
            case( char_string_too_large_error )
                error stop "A character string was too large."
            case( char_string_too_small_error )
                error stop "A character string was too small."
            case( eof_failure )
                error stop "An End-Of-File failure occurred on a READ " // &
                    "statement."
            case( index_invalid_error )
                error stop "An index was invalid."
            case( integer_overflow_error )
                error stop "An integer overflow error occurred."
            case( read_failure )
                error stop "A failure occurred in a READ statement."
            case( write_failure )
                error stop "A failure occurred on a WRITE statement."
            end select
        end if
    end subroutine error_handler


end module stdlib_bitsets
