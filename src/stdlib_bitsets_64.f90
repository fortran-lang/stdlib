submodule(stdlib_bitsets) stdlib_bitsets_64
    implicit none

contains

    elemental module function all_64( self ) result(all)
!     Returns .TRUE. if all bits in SELF are 1, .FALSE. otherwise.
        logical                      :: all
        class(bitset_64), intent(in) :: self

        intrinsic :: btest
        integer(bits_kind) :: pos

        do pos=0, self % num_bits - 1
            if ( .not. btest(self % block, pos) ) then
                all = .false.
                return
            end if
        end do
        all = .true.

    end function all_64


    elemental module subroutine and_64(set1, set2)
!
!     Sets the bits in SET1 to the bitwise AND of the original bits in SET1
!     and SET2. It is required that SET1 have the same number of bits as
!     SET2 otherwise the result is undefined.
!
        type(bitset_64), intent(inout) :: set1
        type(bitset_64), intent(in)    :: set2

!         The set2 extent includes the entire extent of set1.
!         The (zeroed) region past the end of set1 is unaffected by
!         the iand.
            set1 % block = iand( set1 % block, &
                                  set2 % block )

    end subroutine and_64


    elemental module subroutine and_not_64(set1, set2)
!
!     Sets the bits in SET1 to the bitwise and of the original bits in SET1
!     with the bitwise negation of SET2.  SET1 and SET2 must have the same
!     number of bits otherwise the result is undefined.
!
        type(bitset_64), intent(inout) :: set1
        type(bitset_64), intent(in)    :: set2

!     The not with iand means that the zero'ed regions past the end of each set
!     do not interact with the in set regions
        set1 % block = iand( set1 % block, not( set2 % block ) )

    end subroutine and_not_64


    elemental module function any_64(self) result(any)
!     Returns .TRUE. if any bit in SELF is 1, .FALSE. otherwise.
        logical                      :: any
        class(bitset_64), intent(in) :: self

        if ( self % block /= 0 ) then
            any = .true.
            return
        else
            any = .false.
        end if

    end function any_64




    module subroutine assign_logint8_64( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_64
        type(bitset_64), intent(out) :: self
        logical(int8), intent(in)    :: logical_vector(:)

        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        if ( log_size > 64 ) then
            error stop module_name // ' % ' // 'ASSIGNMENT' // " has " // &
                "SIZE(LOGICAL_VECTOR) > 64 with assignment to a BITSET_64."
        end if
        self % num_bits = log_size
        self % block = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                self % block = ibset( self % block, index )
            end if
        end do

    end subroutine assign_logint8_64


    pure module subroutine logint8_assign_64( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_64
        logical(int8), intent(out), allocatable :: logical_vector(:)
        type(bitset_64), intent(in)             :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine logint8_assign_64
    module subroutine assign_logint16_64( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_64
        type(bitset_64), intent(out) :: self
        logical(int16), intent(in)    :: logical_vector(:)

        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        if ( log_size > 64 ) then
            error stop module_name // ' % ' // 'ASSIGNMENT' // " has " // &
                "SIZE(LOGICAL_VECTOR) > 64 with assignment to a BITSET_64."
        end if
        self % num_bits = log_size
        self % block = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                self % block = ibset( self % block, index )
            end if
        end do

    end subroutine assign_logint16_64


    pure module subroutine logint16_assign_64( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_64
        logical(int16), intent(out), allocatable :: logical_vector(:)
        type(bitset_64), intent(in)             :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine logint16_assign_64
    module subroutine assign_logint32_64( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_64
        type(bitset_64), intent(out) :: self
        logical(int32), intent(in)    :: logical_vector(:)

        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        if ( log_size > 64 ) then
            error stop module_name // ' % ' // 'ASSIGNMENT' // " has " // &
                "SIZE(LOGICAL_VECTOR) > 64 with assignment to a BITSET_64."
        end if
        self % num_bits = log_size
        self % block = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                self % block = ibset( self % block, index )
            end if
        end do

    end subroutine assign_logint32_64


    pure module subroutine logint32_assign_64( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_64
        logical(int32), intent(out), allocatable :: logical_vector(:)
        type(bitset_64), intent(in)             :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine logint32_assign_64
    module subroutine assign_logint64_64( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_64
        type(bitset_64), intent(out) :: self
        logical(int64), intent(in)    :: logical_vector(:)

        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        if ( log_size > 64 ) then
            error stop module_name // ' % ' // 'ASSIGNMENT' // " has " // &
                "SIZE(LOGICAL_VECTOR) > 64 with assignment to a BITSET_64."
        end if
        self % num_bits = log_size
        self % block = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                self % block = ibset( self % block, index )
            end if
        end do

    end subroutine assign_logint64_64


    pure module subroutine logint64_assign_64( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_64
        logical(int64), intent(out), allocatable :: logical_vector(:)
        type(bitset_64), intent(in)             :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine logint64_assign_64


    elemental module function bit_count_64(self) result(bit_count)
!     Returns the number of non-zero bits in SELF.
        integer(bits_kind)           ::  bit_count
        class(bitset_64), intent(in) :: self

        integer(bits_kind) :: pos

        bit_count = 0

        do pos = 0, self % num_bits - 1
            if ( btest( self % block, pos ) ) bit_count = bit_count + 1
        end do

    end function bit_count_64


    elemental module subroutine clear_bit_64(self, pos)
!
!     Sets to zero the POS position in SELF. If POS is less than zero or
!     greater than BITS(SELF)-1 it is ignored.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in)  :: pos

        if ( pos < 0 .OR. (pos > self % num_bits-1) ) &
            return
        self % block = ibclr( self % block, pos )

    end subroutine clear_bit_64


    pure module subroutine clear_range_64(self, start_pos, stop_pos)
!
!     Sets to zero all bits from the START_POS to STOP_POS positions in SELF.
!     If STOP_POS < START_POS then no bits are modified. Positions outside
!     the range 0 to BITS(SELF)-1 are ignored.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in)  :: start_pos, stop_pos

        integer(bits_kind) :: true_first, true_last

        true_first = max( 0_bits_kind, start_pos )
        true_last  = min( self % num_bits-1, stop_pos )
        if ( true_last < true_first ) return

        call mvbits( all_zeros,                  &
                     true_first,                 &
                     true_last - true_first + 1, &
                     self % block,               &
                     true_first )

    end subroutine clear_range_64


    elemental module function eqv_64(set1, set2) result(eqv)
!
!     Returns .TRUE. if all bits in SET1 and SET2 have the same value,
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: eqv
        type(bitset_64), intent(in) :: set1, set2

        eqv = set1 % block == set2 % block

    end function eqv_64


    module subroutine extract_64(new, old, start_pos, stop_pos, status)
!    Creates a new bitset, NEW, from a range, START_POS to STOP_POS, in bitset
!    OLD. If START_POS is greater than STOP_POS the new bitset is empty.
!    If START_POS is less than zero or STOP_POS is greater than BITS(OLD)-1
!    then if STATUS is present it has the value INDEX_INVALID_ERROR,
!    otherwise processing stops with an informative message.
        type(bitset_64), intent(out)   :: new
        type(bitset_64), intent(in)    :: old
        integer(bits_kind), intent(in) :: start_pos, stop_pos
        integer, intent(out), optional :: status

        integer(bits_kind) :: bits, i, k
        character(*), parameter :: procedure = 'EXTRACT'

        if ( start_pos < 0 )  then
            call error_handler( 'had a START_POS less than 0.', &
                                index_invalid_error, status,    &
                                module_name, procedure )
            return
        end if
        if ( stop_pos >= old % num_bits ) then
            call error_handler( 'had a STOP_POS greater than BITS(OLD)-1.', &
                                index_invalid_error, status,                &
                                module_name, procedure )
            return
        end if
        bits = stop_pos - start_pos + 1

        if ( bits <= 0 ) then
            new % num_bits = 0
            new % block = 0
            return
        else
            new % num_bits = bits
            do i=0, bits-1
                k = start_pos + i
                if ( btest( old % block, k ) ) &
                    new % block = ibset(new % block, i)
            end do
        end if

        if ( present(status) ) status = success

    end subroutine extract_64


    elemental module subroutine flip_bit_64(self, pos)
!
!     Flips the value at the POS position in SELF, provided the position is
!     valid. If POS is less than 0 or greater than BITS(SELF)-1, no value is
!     changed.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in)  :: pos

        if ( pos < 0 .OR. pos > self % num_bits-1 ) return

        if ( btest( self % block, pos ) ) then
            self % block = ibclr( self % block, pos )
        else
            self % block = ibset( self % block, pos )
        end if

    end subroutine flip_bit_64


    pure module subroutine flip_range_64(self, start_pos, stop_pos)
!
!     Flips all valid bits from the START_POS to the STOP_POS positions in
!     SELF. If STOP_POS < START_POS no bits are flipped. Positions less than
!     0 or greater than BITS(SELF)-1 are ignored.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in)  :: start_pos, stop_pos

        integer(bits_kind) :: end_bit, start_bit

        start_bit = max( 0_bits_kind, start_pos )
        end_bit   = min( stop_pos , self % num_bits-1 )
        call mvbits( not(self % block),       &
                     start_bit,               &
                     end_bit - start_bit + 1, &
                     self % block,            &
                     start_bit )

    end subroutine flip_range_64


    module subroutine from_string_64(self, string, status)
!     Initializes the bitset `self` treating `string` as a binary literal
!     `status` may have the values:
!         `success` - if no problems were found,
!         `alloc_fault` - if allocation of the bitset failed
!         `char_string_too_large_error` - if `string` was too large, or
!         `char_string_invalid_error` - if string had an invalid character.
        class(bitset_64), intent(out)  :: self
        character(*), intent(in)       :: string
        integer, intent(out), optional :: status

        character(*), parameter :: procedure = 'FROM_STRING'
        integer(int64) :: bit
        integer(int64) :: bits
        character(1)   :: char

        bits = len(string, kind=int64)
        if ( bits > 64 ) then
            call error_handler( 'STRING was too long for a ' //      &
                                'BITSET_64 SELF.',                   &
                                char_string_too_large_error, status, &
                                module_name, procedure )
            return
        end if
        self % num_bits = bits
        do bit = 1, bits
            char = string(bit:bit)
            if ( char == '0' ) then
                call self % clear( int(bits-bit, kind=bits_kind) )
            else if ( char == '1' ) then
                call self % set( int(bits-bit, kind=bits_kind) )
            else
                call error_handler( 'STRING had a character other than ' // &
                                    '0 or 1.',                              &
                                    char_string_invalid_error, status,      &
                                    module_name, procedure )
                return
            end if
        end do

        if ( present(status) ) status = success

    end subroutine from_string_64


    elemental module function ge_64(set1, set2) result(ge)
!
!     Returns .TRUE. if the bits in SET1 and SET2 are the same or the
!     highest order different bit is set to 1 in SET1 and to 0 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: ge
        type(bitset_64), intent(in) :: set1, set2

        ge = bge( set1 % block, set2 % block )

    end function ge_64


    elemental module function gt_64(set1, set2) result(gt)
!
!     Returns .TRUE. if the bits in SET1 and SET2 differ and the
!     highest order different bit is set to 1 in SET1 and to 0 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: gt
        type(bitset_64), intent(in) :: set1, set2

        gt = bgt( set1 % block, set2 % block )

    end function gt_64


    module subroutine init_zero_64(self, bits, status)
!
!  Creates the bitset, `self`, of size `bits`, with all bits initialized to
!  zero. `bits` must be non-negative.  If an error occurs and `status` is
!  absent then processing stops with an informative stop code. `status`
!  will have one of the values:
!  * `success` -  if no problems were found,
!  * `array_size_invalid_error` - if `bits` is either negative or larger
!    than 64 with `self` of class `bitset_64`, or
!  * `alloc_fault` - if memory allocation failed
!
        class(bitset_64), intent(out)  :: self
        integer(bits_kind), intent(in) :: bits
        integer, intent(out), optional :: status

        character(*), parameter :: procedure = "INIT"

        if ( bits < 0 ) then
            call error_handler( 'BITS had a negative value.',    &
                                array_size_invalid_error, status, &
                                module_name, procedure )
            return
        end if
        if ( bits > 64 ) then
            call error_handler( 'BITS had a value greater than 64.', &
                                array_size_invalid_error, status,    &
                                module_name, procedure )
            return
        end if

        self % num_bits = bits
        self % block = all_zeros

        if ( present(status) ) status = success

    end subroutine init_zero_64


    module subroutine input_64(self, unit, status)
!
! Reads the components of the bitset, `self`, from the unformatted I/O
! unit, `unit`, assuming that the components were written using `output`.
! If an error occurs and `status` is absent then processing stops with
! an informative stop code. `status` has one of the values:
! * `success` - if no problem was found
! * `alloc_fault` - if it failed during allocation of memory for `self`, or
! * `array_size_invalid_error` if the `bits(self)` in `unit` is negative
!   or greater than 64 for a `bitset_64` input.
! * `read_failure` - if it failed during the reads from `unit`
!
        class(bitset_64), intent(out)  :: self
        integer, intent(in)            :: unit
        integer, intent(out), optional :: status

        integer(bits_kind)      :: bits
        integer                 :: ierr
        character(len=120)      :: message
        character(*), parameter :: procedure = 'INPUT'
        integer                 :: stat

        read(unit, iostat=ierr, iomsg=message) bits
        if (ierr /= 0) then
            call error_handler( 'Failure on a READ statement for UNIT.', &
                                read_failure, status, module_name, procedure )
            return
        end if
        if ( bits < 0 ) then
            call error_handler( 'BITS in UNIT had a negative value.', &
                                array_size_invalid_error, status,     &
                                module_name, procedure )
            return
        end if
        if ( bits > 64 ) then
            call error_handler( 'BITS in UNIT had a value greater than 64.', &
                                array_size_invalid_error, status,     &
                                module_name, procedure )
            return
        end if

        call self % init(bits, stat)
        if (stat /= success) then
            call error_handler( 'Allocation failure for SELF.', &
                                alloc_fault, status, module_name, procedure )
            return
        end if

        if (bits < 1) return

        read(unit, iostat=ierr, iomsg=message) self % block
        if (ierr /= 0) then
            call error_handler( 'Failure on a READ statement for UNIT.', &
                                read_failure, status, module_name, procedure )
            return
        end if

        if ( present(status) ) status = success

    end subroutine input_64


    elemental module function le_64(set1, set2) result(le)
!
!     Returns .TRUE. if the bits in SET1 and SET2 are the same or the
!     highest order different bit is set to 0 in SET1 and to 1 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: le
        type(bitset_64), intent(in) :: set1, set2

        le = ble( set1 % block, set2 % block )

    end function le_64


    elemental module function lt_64(set1, set2) result(lt)
!
!     Returns .TRUE. if the bits in SET1 and SET2 differ and the
!     highest order different bit is set to 0 in SET1 and to 1 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: lt
        type(bitset_64), intent(in) :: set1, set2

        lt = blt( set1 % block, set2 % block )

    end function lt_64


    elemental module function neqv_64(set1, set2) result(neqv)
!
!     Returns .TRUE. if all bits in SET1 and SET2 have the same value,
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                     :: neqv
        type(bitset_64), intent(in) :: set1, set2

        neqv = set1 % block /= set2 % block

    end function neqv_64


    elemental module function none_64(self) result(none)
!
!     Returns .TRUE. if none of the bits in SELF have the value 1.
!
        logical ::  none
        class(bitset_64), intent(in) :: self

        none = .true.
        if (self % block /= 0) then
            none = .false.
            return
        end if

    end function none_64


    elemental module subroutine not_64(self)
!
!     Sets the bits in SELF to their logical complement
!
        class(bitset_64), intent(inout) :: self

        integer(bits_kind) :: bit

        if ( self % num_bits == 0 ) return

        do bit=0, self % num_bits - 1
            if ( btest( self % block, bit ) ) then
                self % block = ibclr( self % block, bit )
            else
                self % block = ibset( self % block, bit )
            end if
        end do

    end subroutine not_64


    elemental module subroutine or_64(set1, set2)
!
!     Sets the bits in SET1 to the bitwise OR of the original bits in SET1
!     and SET2. If SET1 has fewer bits than SET2 then the additional bits
!     in SET2 are ignored. If SET1 has more bits than SET2, then the
!     absent SET2 bits are treated as if present with zero value.
!
        type(bitset_64), intent(inout) :: set1
        type(bitset_64), intent(in)    :: set2

        if ( set1 % num_bits >= set2 % num_bits ) then
            set1 % block = ior( set1 % block, &
                                 set2 % block )
        else
!         The set1 extent ends before set2 => set2 bits must not affect bits in
!         set1 beyond its extent => set those bits to zero while keeping proper
!         values of other bits in set2
           set1 % block =                &
               ior( set1 % block,        &
                    ibits( set2 % block, &
                           0,            &
                           set1 % num_bits ) )
        end if

    end subroutine or_64


    module subroutine output_64(self, unit, status)
!
!     Writes the components of the bitset, SELF, to the unformatted I/O
!     unit, UNIT, in a unformatted sequence compatible with INPUT. If
!     STATUS is absent an error results in an error stop with an
!     informative stop code. If STATUS is present it has the default
!     value of SUCCESS, or the value WRITE_FAILURE if the write failed.
!
        class(bitset_64), intent(in)   :: self
        integer, intent(in)            :: unit
        integer, intent(out), optional :: status

        integer                 :: ierr
        character(len=120)      :: message
        character(*), parameter :: procedure = "OUTPUT"

        write(unit, iostat=ierr, iomsg=message) self % num_bits
        if (ierr /= 0) go to 999

        if (self % num_bits < 1) return
        write(unit, iostat=ierr, iomsg=message) self % block
        if (ierr /= 0) go to 999

        return

999     call error_handler( 'Failure on a WRITE statement for UNIT.', &
                            write_failure, status, module_name, procedure )

    end subroutine output_64


    module subroutine read_bitset_string_64(self, string, status)
!
!     Uses the bitset literal in the default character `string`, to define
!     the bitset, `self`. The literal may be preceded by an an arbitrary
!     sequence of blank characters. If `status` is absent an error results
!     in an error stop with an informative stop code. If `status`
!     is present it has one of the values
!     * `success` - if no problems occurred,
!     * `alloc_fault` - if allocation of memory for SELF failed,
!     * `array_size_invalid_error - if `bits(self)` in `string` is greater
!       than 64 for a `bitset_64`,
!     * `char_string_invalid_error` - if the bitset literal has an invalid
!       character,
!     * `char_string_too_small_error - if the string ends before all the bits
!       are read.
!     * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!       value too large to be represented,
!
        class(bitset_64), intent(out)  :: self
        character(len=*), intent(in)   :: string
        integer, intent(out), optional :: status

        integer(bits_kind)      :: bit, bits
        integer(bits_kind)      :: digits, pos
        character(*), parameter :: procedure = "READ_BITSET"
        integer                 :: stat

        pos = 1
        find_start: do pos=1, len(string)
            if ( string(pos:pos) /= ' ' ) exit
        end do find_start

        if ( pos > len(string) - 8 ) go to 999

        if ( string(pos:pos) /= 's' .AND. string(pos:pos) /= 'S' ) go to 999

        pos = pos + 1
        bits = 0
        digits = 0

        do
            select case( iachar( string(pos:pos) ) )
            case(ia0:ia9)
                digits = digits + 1
                if (  digits == max_digits .AND. bits > overflow_bits ) &
                    go to 996
                if ( digits > max_digits ) go to 996
                bits = bits*10 + iachar( string(pos:pos) ) - ia0
                if ( bits < 0 ) go to 996
            case(iachar('b'), iachar('B'))
                exit
            case default
                go to 999
            end select

            pos = pos + 1

        end do

        if ( bits > 64 ) then
            call error_handler( 'BITS in STRING was greater than 64.', &
                                char_string_too_large_error, status,   &
                                module_name, procedure )
            return
        end if
        if ( bits + pos > len(string) ) then
            call error_handler( 'STRING was too small for the number of ' // &
                                'bits specified by STRING.',                 &
                                char_string_too_small_error, status,         &
                                module_name, procedure )
            return
        end if
        call self % init( bits, stat )
        if (stat /= success) then
            call error_handler( 'There was an allocation fault for SELF.', &
                                alloc_fault, status, module_name, procedure )
            return
        end if

        pos = pos + 1
        bit = bits - 1
        do
            if ( string(pos:pos) == '0' ) then
                call self % clear( bit ) ! this may not be needed
            else if ( string(pos:pos) == '1' ) then
                call self % set( bit )
            else
                go to 999
            end if
            pos = pos + 1
            bit = bit - 1
            if ( bit < 0 ) exit
        end do

        if ( present(status) ) status = success

        return

996     call error_handler( 'There was an integer overflow in reading' // &
                            'size of bitset literal from UNIT',           &
                            integer_overflow_error, status,               &
                            module_name, procedure )
        return

999     call error_handler( 'There was an invalid character in STRING', &
                            char_string_invalid_error, status,          &
                            module_name, procedure )

    end subroutine read_bitset_string_64


    module subroutine read_bitset_unit_64(self, unit, advance, status)
!
!     Uses the bitset literal at the current position in the formatted
!     file with I/O unit, `unit`, to define the bitset, `self`. The literal
!     may be preceded by an arbitrary sequence of blank characters.
!     If `advance` is present it must be either 'YES' or 'NO'. If absent
!     it has the default value of 'YES' to determine whether advancing
!     I/O occurs. If `status` is absent an error results in an error stop
!     with an informative stop code. If `status` is present it has one of
!     the values:
!     * `success` - if no problem occurred,
!     * `alloc_fault` - if allocation of `self` failed,
!     * `array_size_invalid_error` - if `bits(self)` in the bitset literal
!       is greater than 64 for a `bitset_64`.
!     * `char_string_invalid_error` - if the read of the bitset literal found
!       an invalid character,
!     * `eof_failure` - if a `read` statement reaches an end-of-file before
!       completing the read of the bitset literal,
!     * `integer_overflow_error` - if the bitset literal has a `bits(self)`
!       value too large to be represented,
!     * `read_failure` - if a `read` statement fails,
!
        class(bitset_64), intent(out)      :: self
        integer, intent(in)                :: unit
        character(*), intent(in), optional :: advance
        integer, intent(out), optional     :: status

        integer(bits_kind)            :: bit, bits, digits
        integer                       :: ierr
        character(len=128)            :: message
        character(*), parameter       :: procedure = "READ_BITSET"
        character(len=1)              :: char

        do
            read( unit,         &
                  advance='NO', &
                  FMT='(A1)',   &
                  err=997,      &
                  end=998,      &
                  iostat=ierr,  &
                  iomsg=message ) char
            select case( char )
            case( ' ' )
                cycle
            case( 's', 'S' )
                exit
            case default
                go to 999
            end select
        end do

        bits = 0
        digits = 0
        do
            read( unit,         &
                  advance='NO', &
                  FMT='(A1)',   &
                  err=998,      &
                  end=999,      &
                  iostat=ierr,  &
                  iomsg=message ) char
            if ( char == 'b' .or. char == 'B' ) exit
            select case( char )
            case( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' )
                digits = digits + 1
                if (  digits == max_digits .AND. bits > overflow_bits ) &
                    go to 996
                if ( digits > max_digits ) go to 996
                bits = 10*bits + iachar(char) - iachar('0')
                if ( bits < 0 ) go to 996
            case default
                go to 999
            end select
        end do

        if ( bits < 0 .OR. digits == 0 .OR. digits > max_digits ) go to 999

        if ( bits > 64 ) then
            call error_handler( 'BITS in UNIT was greater than 64.', &
                                array_size_invalid_error, status,    &
                                module_name, procedure )
            return
        end if
        call self % init( bits )
        do bit = 1, bits-1
            read( unit,         &
                  advance='NO', &
                  FMT='(A1)',   &
                  err=997,      &
                  end=998,      &
                  iostat=ierr,  &
                  iomsg=message ) char
            if ( char == '0' ) then
                call self % clear( bits-bit )
            else if ( char == '1' ) then
                call self % set( bits-bit )
            else
                go to 999
            end if
        end do

        read( unit,                           &
              advance=optval(advance, 'YES'), &
              FMT='(A1)',                     &
              err=997,                        &
              end=998,                        &
              iostat=ierr,                    &
              iomsg=message ) char
        if ( char == '0' ) then
            call self % clear( bits-bit )
        else if ( char == '1' ) then
            call self % set( bits-bit )
        else
            go to 999
        end if

        if ( present(status) ) status = success

        return

996     call error_handler( 'Integer overflow in reading size of ' // &
                            'bitset literal from UNIT.',              &
                            read_failure, status, module_name, procedure )
        return

997     call error_handler( 'Failure on read of UNIT.',                   &
                            read_failure, status, module_name, procedure )
        return

998     call error_handler( 'End of File of UNIT before finishing a ' // &
                            'bitset literal.',                           &
                            eof_failure, status, module_name, procedure )
        return

999     call error_handler( 'Invalid character in bitset literal in UNIT ', &
                            char_string_invalid_error, status,              &
                            module_name, procedure )

    end subroutine read_bitset_unit_64


    elemental module subroutine set_bit_64(self, pos)
!
!     Sets the value at the POS position in SELF, provided the position is
!     valid. If the position is less than 0 or greater than BITS(SELF)-1
!     then SELF is unchanged.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in)  :: pos
        integer(block_kind) :: dummy

        if ( pos < 0 .OR. pos > self % num_bits-1 ) return
        dummy = ibset( self % block, pos )
        self % block = dummy

    end subroutine set_bit_64


    pure module subroutine set_range_64(self, start_pos, stop_pos)
!
!     Sets all valid bits to 1 from the START_POS to the STOP_POS positions
!     in SELF. If STOP_POA < START_POS no bits are changed. Positions outside
!     the range 0 to BITS(SELF)-1 are ignored.
!
        class(bitset_64), intent(inout) :: self
        integer(bits_kind), intent(in) :: start_pos, stop_pos

        integer(bits_kind) :: end_bit, start_bit

        start_bit = max( 0_bits_kind, start_pos )
        end_bit   = min( stop_pos, self % num_bits-1 )
        if ( end_bit < start_bit ) return

!         FIRST and LAST are in the same block
        call mvbits( all_ones,                &
                     start_bit,               &
                     end_bit - start_bit + 1, &
                     self % block,            &
                     start_bit )

    end subroutine set_range_64


    elemental module function test_64(self, pos) result(test)
!
!     Returns .TRUE. if the POS position is set, .FALSE. otherwise. If POS
!     is negative or greater than BITS(SELF) - 1 the result is .FALSE..
!
        logical                        :: test
        class(bitset_64), intent(in)   :: self
        integer(bits_kind), intent(in) :: pos

        if ( pos < 0 .or. pos >= self % num_bits ) then
            test = .false.
        else
            test = btest( self % block, pos )
        end if

    end function test_64


    module subroutine to_string_64(self, string, status)
!
!     Represents the value of SELF as a binary literal in STRING
!     Status may have the values SUCCESS or ALLOC_FAULT
!
        class(bitset_64), intent(in)               :: self
        character(len=:), allocatable, intent(out) :: string
        integer, intent(out), optional             :: status

        character(*), parameter :: procedure = 'TO_STRING'
        integer :: bit, bit_count, pos, stat

        bit_count = self % num_bits
        allocate( character(len=bit_count)::string, stat=stat )
        if ( stat > 0 ) then
            call error_handler( 'There was an allocation fault for STRING.', &
                                alloc_fault, status, module_name, procedure )
            return
        end if
        do bit=0, bit_count-1
            pos = bit_count - bit
            if ( btest( self % block, bit ) ) then
                string( pos:pos ) = '1'
            else
                string( pos:pos ) = '0'
            end if
        end do

        if ( present(status) ) status = success

    end subroutine to_string_64


    elemental module function value_64(self, pos) result(value)
!
!     Returns 1 if the POS position is set, 0 otherwise. If POS is negative
!     or greater than BITS(SELF) - 1 the result is 0.
!
        integer ::  value
        class(bitset_64), intent(in) :: self
        integer(bits_kind), intent(in)  :: pos

        if ( pos < 0 .or. pos >= self % num_bits ) then
            value = 0

        else
            if ( btest( self % block, pos ) ) then
                value = 1

            else
                value = 0

            end if

        end if

    end function value_64


    module subroutine write_bitset_string_64(self, string, status)
!
!     Writes a bitset literal to the allocatable default character STRING,
!     representing the individual bit values in the bitset_t, SELF.
!     If STATUS is absent an error results in an error stop with an
!     informative stop code. If STATUS is present it has the default
!     value of SUCCESS, or the value ALLOC_FAULT if allocation of
!     the output string failed.
!
        class(bitset_64), intent(in)               :: self
        character(len=:), allocatable, intent(out) :: string
        integer, intent(out), optional             :: status

        integer(bits_kind) :: bit,          &
                              bit_count,    &
                              count_digits, &
                              pos
        integer :: stat

        character(*), parameter :: procedure = 'WRITE_BITSET'

        bit_count = bits(self)

        call digit_count( self % num_bits, count_digits )

        allocate( character(len=count_digits+bit_count+2)::string, stat=stat )
        if ( stat > 0 ) then
            call error_handler( 'There was an allocation fault for STRING.', &
                                alloc_fault, status, module_name, procedure )
            return
        end if
        write( string, "('S', i0)" ) self % num_bits

        string( count_digits + 2:count_digits + 2 ) = "B"
        do bit=0, bit_count-1
            pos = count_digits + 2 + bit_count - bit
            if ( btest( self % block, bit ) ) then
                string( pos:pos ) = '1'
            else
                string( pos:pos ) = '0'
            end if
        end do

        if ( present(status) ) status = success

    contains

        subroutine digit_count( bits, digits )
            integer(bits_kind), intent(in)  :: bits
            integer(bits_kind), intent(out) :: digits

            integer(bits_kind) :: factor

            factor = bits

            if ( factor <= 0 ) then
                digits = 1
                return
            end if

            do digits = 1, 127
                factor = factor / 10
                if ( factor == 0 ) return
            end do

        end subroutine digit_count

    end subroutine write_bitset_string_64


    module subroutine write_bitset_unit_64(self, unit, advance, status)
!
!     Writes a bitset literal to the I/O unit, UNIT, representing the
!     individual bit values in the bitset_t, SELF. By default or if
!     ADVANCE is present with the value 'YES', advancing output is used.
!     If ADVANCE is present with the value 'NO', then the current record
!     is not advanced by the write. If STATUS is absent an error results
!     in an error stop with an informative stop code. If STATUS is
!     present it has the default value of SUCCESS, the value
!     ALLOC_FAULT if allocation of the output string failed, or
!     WRITE_FAILURE if the WRITE statement outputting the literal failed.
!
        class(bitset_64), intent(in)           :: self
        integer, intent(in)                    :: unit
        character(len=*), intent(in), optional :: advance
        integer, intent(out), optional         :: status

        integer                   :: ierr
        character(:), allocatable :: string
        character(len=120)      :: message
        character(*), parameter :: procedure = "WRITE_BITSET"

        call self % write_bitset(string, status)

        if ( present(status) ) then
            if (status /= success ) return
        end if


        write( unit,                           &
               FMT='(A)',                      &
               advance=optval(advance, 'YES'), &
               iostat=ierr,                    &
               iomsg=message )                 &
               string
        if (ierr /= 0) then
            call error_handler( 'Failure on a WRITE statement for UNIT.', &
                                write_failure, status, module_name, procedure )
            return
        endif

    end subroutine write_bitset_unit_64


    elemental module subroutine xor_64(set1, set2)
!
!     Sets the bits in SET1 to the bitwise XOR of the original bits in SET1
!     and SET2. SET1 and SET2 must have the same number of bits otherwise
!     the result is undefined.
!
        type(bitset_64), intent(inout) :: set1
        type(bitset_64), intent(in)    :: set2

        set1 % block = ieor( set1 % block, &
                             set2 % block )

    end subroutine xor_64


end submodule stdlib_bitsets_64
