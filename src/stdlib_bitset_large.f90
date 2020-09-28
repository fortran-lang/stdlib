submodule(stdlib_bitsets) stdlib_bitset_large
    implicit none

contains


    elemental module function all_large( self ) result(all)
!     Returns .TRUE. if all bits in SELF are 1, .FALSE. otherwise.
        logical                         :: all
        class(bitset_large), intent(in) :: self

        integer(bits_kind) :: block, full_blocks, pos

        all = .true.
        full_blocks = bits(self)/block_size
        do block = 1, full_blocks
            if ( self % blocks(block) /= -1_block_kind ) then
                all = .false.
                return
            end if
        end do

        if ( full_blocks == size(self % blocks) ) return

        do pos=0, modulo( bits(self), block_size )-1
            if ( .not. btest(self % blocks(full_blocks+1), pos) ) then
                all = .false.
                return
            end if
        end do

    end function all_large


    elemental module subroutine and_large(set1, set2)
!
!     Sets the bits in SET1 to the bitwise AND of the original bits in SET1
!     and SET2. It is required that SET1 have the same number of bits as
!     SET2 otherwise the result is undefined.
!
        type(bitset_large), intent(inout) :: set1
        type(bitset_large), intent(in)    :: set2

        integer(bits_kind) :: ablock

        do ablock = 1, size(set1 % blocks)
            set1 % blocks(ablock) = iand( set1 % blocks(ablock), &
                                          set2 % blocks(ablock) )
        end do

    end subroutine and_large


    elemental module subroutine and_not_large(set1, set2)
!
!     Sets the bits in SET1 to the bitwise and of the original bits in SET1
!     with the bitwise negation of SET2. SET1 and SET2 must have the same
!     number of bits otherwise the result is undefined.
!
        type(bitset_large), intent(inout) :: set1
        type(bitset_large), intent(in)    :: set2

        integer(bits_kind) :: ablock

        do ablock = 1, size( set1 % blocks )
            set1 % blocks(ablock) =                                      &
                iand( set1 % blocks(ablock), not( set2 % blocks(ablock) ) )
        end do

    end subroutine and_not_large


    elemental module function any_large(self) result(any)
!     Returns .TRUE. if any bit in SELF is 1, .FALSE. otherwise.
        logical                         :: any
        class(bitset_large), intent(in) :: self

        integer(bits_kind) :: ablock

        do ablock = 1, size(self % blocks)
            if ( self % blocks(ablock) /= 0 ) then
                any = .true.
                return
            end if
        end do
        any = .false.

    end function any_large


    pure module subroutine assign_large( set1, set2 )
!     Used to define assignment for bitset_large
        type(bitset_large), intent(out) :: set1
        type(bitset_large), intent(in)  :: set2

        set1 % num_bits = set2 % num_bits
        allocate( set1 % blocks( size( set2 % blocks, kind=bits_kind ) ) )
        set1 % blocks(:) = set2 % blocks(:)

    end subroutine assign_large


    pure module subroutine assign_log8_large( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_large
        type(bitset_large), intent(out) :: self
        logical(int8), intent(in)       :: logical_vector(:)

        integer(bits_kind) :: blocks
        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        self % num_bits = log_size
        if ( log_size == 0 ) then
            blocks = 0
        else
            blocks = (log_size-1)/block_size + 1
        end if
        allocate( self % blocks( blocks ) )
        self % blocks(:) = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                call self % set( index )
            end if
        end do

    end subroutine assign_log8_large


    pure module subroutine assign_log16_large( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_large
        type(bitset_large), intent(out) :: self
        logical(int16), intent(in)      :: logical_vector(:)

        integer(bits_kind) :: blocks
        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        self % num_bits = log_size
        if ( log_size == 0 ) then
            blocks = 0
        else
            blocks = (log_size-1)/block_size + 1
        end if
        allocate( self % blocks( blocks ) )
        self % blocks(:) = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                call self % set( index )
            end if
        end do

    end subroutine assign_log16_large


    pure module subroutine assign_log32_large( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_large
        type(bitset_large), intent(out) :: self
        logical(int32), intent(in)      :: logical_vector(:)

        integer(bits_kind) :: blocks
        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        self % num_bits = log_size
        if ( log_size == 0 ) then
            blocks = 0
        else
            blocks = (log_size-1)/block_size + 1
        end if
        allocate( self % blocks( blocks ) )
        self % blocks(:) = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                call self % set( index )
            end if
        end do

    end subroutine assign_log32_large


    pure module subroutine assign_log64_large( self, logical_vector )
!     Used to define assignment from an array of type logical for bitset_large
        type(bitset_large), intent(out) :: self
        logical(int64), intent(in)      :: logical_vector(:)

        integer(bits_kind) :: blocks
        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( logical_vector, kind=bits_kind )
        self % num_bits = log_size
        if ( log_size == 0 ) then
            blocks = 0
        else
            blocks = (log_size-1)/block_size + 1
        end if
        allocate( self % blocks( blocks ) )
        self % blocks(:) = 0

        do index=0, log_size-1
            if ( logical_vector(index+1) ) then
                call self % set( index )
            end if
        end do

    end subroutine assign_log64_large


    pure module subroutine log8_assign_large( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_large
        logical(int8), intent(out), allocatable :: logical_vector(:)
        type(bitset_large), intent(in)          :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine log8_assign_large


    pure module subroutine log16_assign_large( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_large
        logical(int16), intent(out), allocatable :: logical_vector(:)
        type(bitset_large), intent(in)           :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine log16_assign_large


    pure module subroutine log32_assign_large( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_large
        logical(int32), intent(out), allocatable :: logical_vector(:)
        type(bitset_large), intent(in)           :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine log32_assign_large


    pure module subroutine log64_assign_large( logical_vector, set )
!     Used to define assignment to an array of type logical for bitset_large
        logical(int64), intent(out), allocatable :: logical_vector(:)
        type(bitset_large), intent(in)           :: set

        integer(bits_kind) :: index

        allocate( logical_vector( set % num_bits ) )
        do index=0, set % num_bits-1
            if ( set % value( index ) == 1 ) then
                logical_vector(index+1) = .true.
            else
                logical_vector(index+1) = .false.
            end if
        end do

    end subroutine log64_assign_large


    elemental module function bit_count_large(self) result(bit_count)
!     Returns the number of non-zero bits in SELF.
        integer(bits_kind)              ::  bit_count
        class(bitset_large), intent(in) :: self

        integer(bits_kind) :: ablock, pos

        bit_count = 0
        do ablock = 1, size(self % blocks) - 1
            do pos = 0, block_size-1
                if ( btest( self % blocks(ablock), pos ) ) &
                    bit_count = bit_count + 1
            end do

        end do

        do pos = 0, self % num_bits - (ablock-1)*block_size - 1
            if ( btest( self % blocks(ablock), pos ) ) bit_count = bit_count + 1
        end do

    end function bit_count_large


    elemental module subroutine clear_bit_large(self, pos)
!
!     Sets to zero the POS position in SELF. If POS is less than zero or
!     greater than BITS(SELF)-1 it is ignored.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in)     :: pos

        integer :: clear_block, block_bit

        if ( pos < 0 .OR. (pos > self % num_bits-1) ) return
        clear_block = pos / block_size + 1
        block_bit   = pos - (clear_block - 1) * block_size
        self % blocks(clear_block) = &
            ibclr( self % blocks(clear_block), block_bit )

    end subroutine clear_bit_large


    pure module subroutine clear_range_large(self, start_pos, stop_pos)
!
!     Sets to zero all bits from the START_POS to STOP_POS positions in SELF.
!     If STOP_POS < START_POS then no bits are modified. Positions outside
!     the range 0 to BITS(SELF)-1 are ignored.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in)     :: start_pos, stop_pos

        integer(bits_kind) :: bit, ablock, first_block, last_block, &
                              true_first, true_last

        true_first = max( 0, start_pos )
        true_last  = min( self % num_bits-1, stop_pos )
        if ( true_last < true_first ) return

        first_block = true_first / block_size + 1
        last_block  = true_last / block_size + 1
        if ( first_block == last_block ) then
!     TRUE_FIRST and TRUE_LAST are in the same block
            call mvbits( all_zeros,                               &
                         true_first - (first_block-1)*block_size, &
                         true_last - true_first + 1,              &
                         self % blocks(first_block),              &
                         true_first - (first_block-1)*block_size )
            return
        end if

!     Do "partial" black containing FIRST
        bit = true_first - (first_block-1)*block_size
        call mvbits( all_zeros,                  &
                     bit,                        &
                     block_size - bit,           &
                     self % blocks(first_block), &
                     bit )

!     Do "partial" black containing LAST
        bit = true_last - (last_block-1)*block_size
        call mvbits( all_zeros,                 &
                     0,                         &
                     bit+1,                     &
                     self % blocks(last_block), &
                     0 )

!     Do intermediate blocks
        do ablock = first_block+1, last_block-1
            self % blocks(ablock) = all_zeros
        end do

    end subroutine clear_range_large


    elemental module function eqv_large(set1, set2) result(eqv)
!
!     Returns .TRUE. if all bits in SET1 and SET2 have the same value,
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: eqv
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: block, common_blocks

        eqv = .false.
        common_blocks = size(set1 % blocks)
        do block = 1, common_blocks
            if ( set1 % blocks(block) /= set2 % blocks(block) ) return
        end do
        eqv = .true.

    end function eqv_large


    module subroutine extract_large(new, old, start_pos, stop_pos, status)
!    Creates a new bitset, NEW, from a range, START_POS to STOP_POS, in bitset
!    OLD. If START_POS is greater than STOP_POS the new bitset is empty.
!    If START_POS is less than zero or STOP_POS is greater than BITS(OLD)-1
!    then if STATUS is present it has the value INDEX_INVALID_ERROR,
!    otherwise processing stops with an informative message.
        type(bitset_large), intent(out) :: new
        type(bitset_large), intent(in)  :: old
        integer(bits_kind), intent(in)  :: start_pos, stop_pos
        integer, intent(out), optional  :: status

        integer(bits_kind) :: bits, blocks, ex_block, i, j, k, old_block
        character(*), parameter :: procedure = 'EXTRACT'

        if ( start_pos < 0 ) go to 999
        if ( stop_pos >= old % num_bits ) go to 998
        bits = stop_pos - start_pos + 1

        if ( bits <= 0 ) then
            new % num_bits = 0
            allocate( new % blocks(0) )
            return
        end if

        blocks = ((bits-1) / block_size) + 1

        new % num_bits = bits
        allocate( new % blocks(blocks) )
        new % blocks(:) = 0

        do i=0, bits-1
            ex_block = i / block_size + 1
            j = i - (ex_block-1) * block_size
            old_block = (start_pos + i) / block_size + 1
            k = (start_pos + i) - (old_block-1) * block_size
            if ( btest( old % blocks(old_block), k ) ) then
                new % blocks(ex_block) = ibset(new % blocks(ex_block), j)
            end if
        end do

        if ( present(status) ) status = success

        return

998     if ( present(status) ) then
            status = index_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'STOP_POS greater than BITS(OLD)-1.'
        end if

999     if ( present(status) ) then
            status = index_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'START_POS less than 0.'
        end if

    end subroutine extract_large


    elemental module subroutine flip_bit_large(self, pos)
!
!     Flips the value at the POS position in SELF, provided the position is
!     valid. If POS is less than 0 or greater than BITS(SELF)-1, no value is
!     changed.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in)     :: pos

        integer :: flip_block, block_bit

        if ( pos < 0 .OR. pos > self % num_bits-1 ) return

        flip_block = pos / block_size + 1
        block_bit  = pos - (flip_block - 1) * block_size
        if ( btest( self % blocks(flip_block), block_bit ) ) then
            self % blocks(flip_block) = ibclr( self % blocks(flip_block), &
                                               block_bit )
        else
            self % blocks(flip_block) = ibset( self % blocks(flip_block), &
                                               block_bit )
        end if

    end subroutine flip_bit_large


    pure module subroutine flip_range_large(self, start_pos, stop_pos)
!
!     Flips all valid bits from the START_POS to the STOP_POS positions in
!     SELF. If STOP_POS < START_POS no bits are flipped. Positions less than
!     0 or greater than BITS(SELF)-1 are ignored.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in)     :: start_pos, stop_pos

        integer(bits_kind) :: bit, ablock, end_bit, first_block, last_block, &
                              start_bit

        start_bit = max( 0, start_pos )
        end_bit   = min( stop_pos , self % num_bits-1 )
        if ( end_bit < start_bit ) return

        first_block = start_bit / block_size + 1
        last_block  = end_bit / block_size + 1
        if (first_block == last_block) then
!         FIRST and LAST are in the same block
            call mvbits( not(self % blocks(first_block)),        &
                         start_bit - (first_block-1)*block_size, &
                         end_bit - start_bit + 1,                &
                         self % blocks(first_block),             &
                         start_bit - (first_block-1)*block_size )
            return
        end if

!     Do "partial" black containing FIRST
        bit = start_bit - (first_block-1)*block_size
        call mvbits( not(self % blocks(first_block) ), &
                     bit,                              &
                     block_size - bit,                 &
                     self % blocks(first_block),       &
                     bit )

!     Do "partial" black containing LAST
        bit = end_bit - (last_block-1)*block_size
        call mvbits( not( self % blocks(last_block) ), &
                     0,                                &
                     bit+1,                            &
                     self % blocks(last_block),        &
                     0 )

!     Do remaining blocks
        do ablock = first_block+1, last_block-1
            self % blocks(ablock) = not( self % blocks(ablock) )
        end do

    end subroutine flip_range_large

    module subroutine from_string_large(self, string, status)
!     Initializes the bitset SELF treating STRING as a binary literal
!     STATUS may have the values SUCCESS, ALLOC_FAULT,
!     ARRAY_SIZE_INVALID_ERROR, or CHAR_STRING_INVALID.
        class(bitset_large), intent(out) :: self
        character(*), intent(in)         :: string
        integer, intent(out), optional   :: status

        character(*), parameter :: procedure = 'FROM_STRING'
        integer(bits_kind) :: bit
        integer(int64)     :: bits
        character(1)       :: char

        bits = len(string, kind=int64)
        if ( bits > huge(0_bits_kind) ) go to 998

        call init_zero_large( self, int(bits, kind=bits_kind), status )

        if ( present(status) ) then
            if ( status /= success ) return
        end if

        do bit = 1_bits_kind, bits
            char = string(bit:bit)
            if ( char == '0' ) then
                call self % clear( int(bits, kind=bits_kind)-bit )
            else if ( char == '1' ) then
                call self % set( int(bits, kind=bits_kind)-bit )
            else
                go to 999
            end if
        end do

        if ( present(status) ) status = success

        return

998     if ( present(status) ) then
            status = array_size_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' STRING ' // &
                'was too long for a BITSET_64 SELF.'
        end if

999     if ( present(status) ) then
            status = char_string_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' STRING ' // &
                'had a character other than "0" or "1",'
        end if

    end subroutine from_string_large


    elemental module function ge_large(set1, set2) result(ge)
!
!     Returns .TRUE. if the bits in SET1 and SET2 are the same or the
!     highest order different bit is set to 1 in SET1 and to 0 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: ge
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: ablock

        do ablock = size(set1 % blocks), 1, -1
            if ( set1 % blocks(ablock) == set2 % blocks(ablock) ) then
                cycle
            else if ( bgt(set1 % blocks(ablock), set2 % blocks(ablock) ) ) then
                ge = .true.
                return
            else
                ge = .false.
                return
            end if
        end do
        ge = .true.

    end function ge_large


    elemental module function gt_large(set1, set2) result(gt)
!
!     Returns .TRUE. if the bits in SET1 and SET2 differ and the
!     highest order different bit is set to 1 in SET1 and to 0 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: gt
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: ablock

        do ablock = size(set1 % blocks), 1, -1
            if ( set1 % blocks(ablock) == set2 % blocks(ablock) ) then
                cycle
            else if ( bgt( set1 % blocks(ablock), set2 % blocks(ablock) ) ) then
                gt = .true.
                return
            else
                gt = .false.
                return
            end if
        end do
        gt = .false.

    end function gt_large


    module subroutine init_zero_large(self, bits, status)
!
!     Creates the bitset, SELF, of size BITS, with all bits initialized to
!     zero. BITS must be non-negative.  If an error occurs and STATUS is
!     absent then processing stops with an informative stop code. STATUS
!     has a default value of SUCCESS. If an error occurs it has the value
!     ARRAY_SIZE_INVALID_ERROR if BITS is either negative larger than 64
!     if SELF is of type BITSET_64, or the value ALLOC_FAULT if it failed
!     during allocation of memory for SELF.
!
        class(bitset_large), intent(out) :: self
        integer(bits_kind), intent(in)   :: bits
        integer, intent(out), optional   :: status

        character(len=120)      :: message
        character(*), parameter :: procedure = "INIT"
        integer                 :: blocks, ierr

        message = ''
        if ( bits < 0 ) go to 999

        if (bits == 0) then
            self % num_bits = 0
            allocate( self % blocks(0), stat=ierr, errmsg=message )
            if (ierr /= 0) go to 998
            return
        else
            blocks = ((bits-1) / block_size) + 1
        end if

        self % num_bits = bits
        allocate( self % blocks(blocks), stat=ierr, errmsg=message )
        if (ierr /= 0) go to 998

        self % blocks(:) = all_zeros

        if ( present(status) ) status = success

        return

998     if ( present(status) ) then
            status = alloc_fault
            return
        else
            error stop module_name // ' % ' // procedure // ' allocation ' // &
                'failure for SELF.'
        end if

999     if ( present(status) ) then
            status = array_size_invalid_error
            return
        else
            error stop module_name // ' %' // procedure // ' BITS had ' // &
                'a negative value.'
        end if

    end subroutine init_zero_large


    module subroutine input_large(self, unit, status)
!
!     Reads the components of the bitset, SELF, from the unformatted I/O
!     unit, UNIT, assuming that the components were written using OUTPUT.
!     If an error occurs and STATUS is absent then processing stops with
!     an informative stop code. STATUS has a default value of SUCCESS.
!     If an error occurs it has the value READ_FAILURE if it failed
!     during the reads from UNIT or the value ALLOC_FAULT if it failed
!     during allocation of memory for SELF, or the value
!     ARRAY_SIZE_INVALID_ERROR if the BITS(SELF) in UNIT is less than 0
!     or greater than 64 for a BITSET_64 input.
!
        class(bitset_large), intent(out) :: self
        integer, intent(in)              :: unit
        integer, intent(out), optional   :: status

        integer(bits_kind)      :: bits
        integer                 :: ierr
        character(len=120)      :: message
        character(*), parameter :: procedure = 'INPUT'
        integer                 :: stat

        read(unit, iostat=ierr, iomsg=message) bits
        if (ierr /= 0) go to 999
        if ( bits < 0 ) go to 997

        call self % init(bits, stat)
        if (stat /= success) go to 998

        if (bits < 1) return

        read(unit, iostat=ierr, iomsg=message) self % blocks(:)
        if (ierr /= 0) go to 999

        if ( present(status) ) status = success

        return

997     if ( present(status) ) then
            status = array_size_invalid_error
            return
        else
            error stop module_name // ' %' // procedure // ' BITS in ' // &
                'UNIT had a negative value.'
        end if

998     if ( present(status) ) then
            status = alloc_fault
            return
        else
            error stop module_name // ' % ' // procedure // ' had an ' // &
                'alloction fault for SELF.'
        end if

999     if ( present(status) ) then
            status = read_failure
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'failure on a READ statement for UNIT.'
        end if

    end subroutine input_large


    elemental module function le_large(set1, set2) result(le)
!
!     Returns .TRUE. if the bits in SET1 and SET2 are the same or the
!     highest order different bit is set to 0 in SET1 and to 1 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: le
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: ablock

        do ablock = size(set1 % blocks), 1, -1
            if ( set1 % blocks(ablock) == set2 % blocks(ablock) ) then
                cycle
            else if ( blt( set1 % blocks(ablock), set2 % blocks(ablock) ) ) then
                le = .true.
                return
            else
                le = .false.
                return
            end if
        end do

        le = .true.

    end function le_large


    elemental module function lt_large(set1, set2) result(lt)
!
!     Returns .TRUE. if the bits in SET1 and SET2 differ and the
!     highest order different bit is set to 0 in SET1 and to 1 in set2.
!     .FALSE.  otherwise.  The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: lt
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: ablock

        do ablock = size(set1 % blocks), 1, -1
            if ( set1 % blocks(ablock) == set2 % blocks(ablock) ) then
                cycle
            else if ( blt( set1 % blocks(ablock), set2 % blocks(ablock) ) ) then
                lt = .true.
                return
            else
                lt = .false.
                return
            end if
        end do
        lt = .false.

    end function lt_large


    elemental module function neqv_large(set1, set2) result(neqv)
!
!     Returns .TRUE. if any bits in SET1 and SET2 differ in  value,
!     .FALSE.  otherwise. The sets must have the same number of bits
!     otherwise the results are undefined.
!
        logical                        :: neqv
        type(bitset_large), intent(in) :: set1, set2

        integer(bits_kind) :: block

        neqv = .true.
        do block = 1, size(set1 % blocks)
            if ( set1 % blocks(block) /= set2 % blocks(block) ) return
        end do
        neqv = .false.

    end function neqv_large


    elemental module function none_large(self) result(none)
!
!     Returns .TRUE. if none of the bits in SELF have the value 1.
!
        logical ::  none
        class(bitset_large), intent(in) :: self

        integer :: block

        none = .true.
        do block = 1, size(self % blocks)
            if (self % blocks(block) /= 0) then
                none = .false.
                return
            end if
        end do

    end function none_large


    elemental module subroutine not_large(self)
!
!     Sets the bits in SELF to their logical complement
!
        class(bitset_large), intent(inout) :: self

        integer(bits_kind) :: bit, full_blocks, block, remaining_bits

        if ( self % num_bits == 0 ) return
        full_blocks = self % num_bits  / block_size
        do block = 1, full_blocks
            self % blocks(block) = not( self % blocks(block) )
        end do
        remaining_bits = self % num_bits - full_blocks * block_size

        do bit=0, remaining_bits - 1
            if ( btest( self % blocks( block ), bit ) ) then
                self % blocks( block ) = ibclr( self % blocks(block), bit )
            else
                self % blocks( block ) = ibset( self % blocks(block), bit )
            end if
        end do

    end subroutine not_large


    elemental module subroutine or_large(set1, set2)
!
!     Sets the bits in SET1 to the bitwise OR of the original bits in SET1
!     and SET2. SET1 and SET2 must have the same number of bits otherwise
!     the result is undefined.
!
        type(bitset_large), intent(inout) :: set1
        type(bitset_large), intent(in)    :: set2

        integer(bits_kind) :: ablock

        do ablock = 1, size( set1 % blocks )
            set1 % blocks(ablock) = ior( set1 % blocks(ablock), &
                                         set2 % blocks(ablock) )
        end do

    end subroutine or_large


    module subroutine output_large(self, unit, status)
!
!     Writes the components of the bitset, SELF, to the unformatted I/O
!     unit, UNIT, in a unformatted sequence compatible with INPUT. If
!     STATUS is absent an error results in an error stop with an
!     informative stop code. If STATUS is present it has the default
!     value of SUCCESS, or the value WRITE_FAILURE if the write failed.
!
        class(bitset_large), intent(in) :: self
        integer, intent(in)             :: unit
        integer, intent(out), optional  :: status

        integer                 :: ierr
        character(len=120)      :: message
        character(*), parameter :: procedure = "OUTPUT"

        write(unit, iostat=ierr, iomsg=message) self % num_bits
        if (ierr /= 0) go to 999

        if (self % num_bits < 1) return
        write(unit, iostat=ierr, iomsg=message) self % blocks(:)
        if (ierr /= 0) go to 999

        return

999     if ( present(status) ) then
            status = write_failure
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'failure in the write to UNIT.'
        end if

    end subroutine output_large


    module subroutine read_bitset_string_large(self, string, status)
!
!     Uses the bitset literal in the default character STRING, to define
!     the bitset, SELF. The literal may be preceded by an an arbitrary
!     sequence of blank characters. If STATUS is absent an error results
!     in an error stop with an informative stop code. If STATUS
!     is present it has the default value of SUCCESS, the value
!     INTEGER_OVERFLOW_ERROR if the bitset literal has a BITS(SELF) value
!     too large to be represented, the value ALLOC_FAULT if allocation of
!     memory for SELF failed, or CHAR_STRING_INVALID_ERROR if the bitset
!     literal has an invalid character, or ARRAY_SIZE_INVALID_ERROR if
!     BITS(SELF) in STRING is greater than 64 for a BITSET_64, or
!     CHAR_STRING_TOO_SMALL_ERROR if the string ends before all the bits
!     are read.
!
        class(bitset_large), intent(out) :: self
        character(len=*), intent(in)    :: string
        integer, intent(out), optional  :: status

        integer(bits_kind)      :: bit, bits
        integer(int64)          :: bits_needed
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
                if ( digits == 10 .AND. bits > 2_bits_kind**30/5 ) go to 996
!! May not be quite right
                if ( digits > 10 ) go to 996
                bits = bits*10 + iachar( string(pos:pos) ) - ia0
                if ( bits < 0 ) go to 996
            case(iachar('b'), iachar('B'))
                go to 100
            case default
                go to 999
            end select

            pos = pos + 1
        end do

100     if ( bits + pos > len(string) ) go to 994
        call self % init( bits, stat )
        if (stat /= success) go to 998

        pos = pos + 1
        bit = bits - 1
        do
            if ( string(pos:pos) == '0' ) then
                call self % clear( bit )
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

994     if ( present(status) ) then
            status = char_string_too_small_error
            return
        else
            error stop module_name // ' % ' // procedure // ' STRING ' // &
                'was too small for the BITS specified by the STRING.'
        end if

996     if ( present(status) ) then
            status = integer_overflow_error
            return
        else
            error stop module_name // ' % ' // procedure // ' failed on ' // &
                'integer overflow in reading size of bitset literal from ' // &
                'UNIT.'
        end if

998     if ( present(status) ) then
            status = alloc_fault
            return
        else
            error stop module_name // ' % ' // procedure // ' failed in ' // &
                'allocating memory for the bitset.'
        end if

999     if ( present(status) ) then
            status = char_string_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' failed due ' // &
                'to an invalid character in STRING.'
        end if

    end subroutine read_bitset_string_large


    module subroutine read_bitset_unit_large(self, unit, advance, status)
!
!     Uses the bitset literal at the current position in the formatted
!     file with I/O unit, UNIT, to define the bitset, SELF. The literal
!     may be preceded by an an arbitrary sequence of blank characters.
!     If ADVANCE is present it must be either 'YES' or 'NO'. If absent
!     it has the default value of 'YES' to determine whether advancing
!     I/O occurs. If STATUS is absent an error results in an error stop
!     with an informative stop code. If STATUS is present it has the
!     default value of SUCCESS, the value INTEGER_OVERFLOW_ERROR if the
!     bitset literal has a BITS(SELF) value too large to be
!     represented, the value READ_FAILURE if a READ statement fails,
!     EOF_FAILURE if a READ statement reach an end-of-file before
!     completing the read of the bitset literal, or the value
!     CHAR_STRING_INVALID_ERROR if the read of the bitset literal found
!     an invalid character, or ARRAY_SIZE_INVALID_ERROR if BITS(SELF)
!     in STRING is greater than 64 for a BITSET_64.
!
        class(bitset_large), intent(out)   :: self
        integer, intent(in)                :: unit
        character(*), intent(in), optional :: advance
        integer, intent(out), optional     :: status

        integer(bits_kind)            :: bit, bits, digits
        integer                       :: ierr
        character(len=128)            :: message
        character(len=:), allocatable :: literal
        integer(bits_kind)            :: pos
        character(*), parameter       :: procedure = "READ_BITSET"
        integer                       :: stat
        character(len=1)              :: char, quote

        do
            read( unit,         &
                  advance='NO', &
                  FMT='(A1)',   &
                  err=997,      &
                  end=998,      &
                  iostat=ierr, &
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
                if ( digits == 10 .AND. bits > 2_bits_kind**30/5 ) go to 996
!! May not be quite right
                if ( digits > 10 ) go to 996
                bits = 10*bits + iachar(char) - iachar('0')
                if ( bits < 0 ) go to 996
            case default
                go to 999
            end select
        end do

        if ( bits < 0 .OR. digits == 0 .OR. digits > 10 ) go to 999

        call self % init(  bits )
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

        if ( present(advance) ) then
            read( unit,            &
                  advance=advance, &
                  FMT='(A1)',      &
                  err=997,         &
                  end=998,         &
                  iostat=ierr,     &
                  iomsg=message ) char
        else
            read( unit,          &
                  advance='YES', &
                  FMT='(A1)',    &
                  err=997,       &
                  end=998,       &
                  iostat=ierr,   &
                  iomsg=message ) char
        end if

        if ( char == '0' ) then
            call self % clear( bits-bit )
        else if ( char == '1' ) then
            call self % set( bits-bit )
        else
            go to 999
        end if

        if ( present(status) ) status = success

        return

996     if ( present(status) ) then
            status = integer_overflow_error
            return
        else
            error stop module_name // ' % ' // procedure // ' failed on ' // &
                'integer overflow in reading size of bitset literal from ' // &
                'UNIT.'
        end if


997     if ( present(status) ) then
            status = read_failure
            return
        else
            error stop module_name // ' % ' // procedure // ' failed on ' // &
                'read of UNIT.'
        end if

998     if ( present(status) ) then
            status = eof_failure
            return
        else
            error stop module_name // ' % ' // procedure // ' reached ' // &
                'End of File of UNIT before finding a bitset literal.'
        end if

999     if ( present(status) ) then
            status = char_string_invalid_error
            return
        else
            error stop module_name // ' % ' // procedure // ' found an ' // &
                'invalid bitset literal in UNIT.'
        end if

    end subroutine read_bitset_unit_large


    elemental module subroutine set_bit_large(self, pos)
!
!     Sets the value at the POS position in SELF, provided the position is
!     valid. If the position is less than 0 or greater than BITS(SELF)-1
!     then SELF is unchanged.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in)     :: pos

        integer(bits_kind) :: set_block, block_bit

        if ( pos < 0 .OR. pos > self % num_bits-1 ) return

        set_block = pos / block_size + 1
        block_bit = pos - (set_block - 1) * block_size
        self % blocks(set_block) = ibset( self % blocks(set_block), block_bit )

    end subroutine set_bit_large


    pure module subroutine set_range_large(self, start_pos, stop_pos)
!
!     Sets all valid bits to 1 from the START_POS to the STOP_POS positions
!     in SELF. If STOP_POA < START_POS no bits are changed. Positions outside
!     the range 0 to BITS(SELF)-1 are ignored.
!
        class(bitset_large), intent(inout) :: self
        integer(bits_kind), intent(in) :: start_pos, stop_pos

        integer(bits_kind) :: bit, ablock, end_bit, first_block, last_block, &
                              start_bit

        start_bit = max( 0, start_pos )
        end_bit   = min( stop_pos, self % num_bits-1 )
        if ( end_bit < start_bit ) return

        first_block = start_bit / block_size + 1
        last_block  = end_bit / block_size + 1
        if ( first_block == last_block ) then
!         FIRST and LAST are in the same block
            call mvbits( all_ones,                               &
                         start_bit - (first_block-1)*block_size, &
                         end_bit - start_bit + 1,                &
                         self % blocks(first_block),             &
                         start_bit - (first_block-1)*block_size )
            return
        end if

!     Do "partial" black containing FIRST
        bit = start_bit - (first_block-1)*block_size
        call mvbits( all_ones,                   &
                     bit,                        &
                     block_size - bit,           &
                     self % blocks(first_block), &
                     bit )

!     Do "partial" black containing LAST
        bit = end_bit - (last_block-1)*block_size
        call mvbits( all_ones,                  &
                     0,                         &
                     bit+1,                     &
                     self % blocks(last_block), &
                     0 )

! Do remaining blocks
        do ablock = first_block+1, last_block-1
            self % blocks(ablock) = all_ones
        end do

    end subroutine set_range_large


    elemental module function test_large(self, pos) result(test)
!
!     Returns .TRUE. if the POS position is set, .FALSE. otherwise. If POS
!     is negative or greater than BITS(SELF) - 1 the result is .FALSE..
!
        logical ::  test
        class(bitset_large), intent(in)    :: self
        integer(bits_kind), intent(in) :: pos

        integer(bits_kind) :: bit_block

        if ( pos < 0 .or. pos >= self % num_bits ) then
            test = .false.
        else
            bit_block = pos / block_size + 1
            test = btest( self % blocks(bit_block), &
                          pos - ( bit_block-1 ) * block_size )
        end if

    end function test_large


    module subroutine to_string_large(self, string, status)
!
!     Represents the value of SELF as a binary literal in STRING
!     Status may have the values SUCCESS or ALLOC_FAULT
!
        class(bitset_large), intent(in)            :: self
        character(len=:), allocatable, intent(out) :: string
        integer, intent(out), optional             :: status

        character(*), parameter :: procedure = 'TO_STRING'
        integer(bits_kind) :: bit, bit_count, pos
        integer :: stat

        bit_count = self % num_bits
        allocate( character(len=bit_count)::string, stat=stat )
        if ( stat > 0 ) go to 999
        do bit=0, bit_count-1
            pos = bit_count - bit
            if ( self % test( bit) ) then
                string( pos:pos ) = '1'
            else
                string( pos:pos ) = '0'
            end if
        end do

        if ( present(status) ) status = success

        return

999     if ( present(status) ) then
            status = alloc_fault
            return
        else
            error stop module_name // ' % ' // procedure // ' allocation ' // &
                'of STRING failed.'
        end if

    end subroutine to_string_large


    elemental module function value_large(self, pos) result(value)
!
!     Returns 1 if the POS position is set, 0 otherwise. If POS is negative
!     or greater than BITS(SELF) - 1 the result is 0.
!
        integer ::  value
        class(bitset_large), intent(in) :: self
        integer(bits_kind), intent(in)  :: pos

        integer :: bit_block

        if ( pos < 0 .or. pos >= self % num_bits ) then
            value = 0
        else
            bit_block = pos / block_size + 1
            if ( btest( self % blocks(bit_block), &
                pos - ( bit_block-1 ) * block_size ) ) then
                value = 1
            else
                value = 0
            end if
        end if

    end function value_large


    module subroutine write_bitset_string_large(self, string, status)
!
!     Writes a bitset literal to the allocatable default character STRING,
!     representing the individual bit values in the bitset_t, SELF.
!     If STATUS is absent an error results in an error stop with an
!     informative stop code. If STATUS is present it has the default
!     value of SUCCESS, or the value ALLOC_FAULT if allocation of
!     the output string failed.
!
        class(bitset_large), intent(in)            :: self
        character(len=:), allocatable, intent(out) :: string
        integer, intent(out), optional             :: status

        integer(bits_kind) :: bit,          &
                              bit_count,    &
                              count_digits, &
                              digit,        &
                              digits,       &
                              max_bit,      &
                              pos,          &
                              processed,    &
                              val
        integer :: stat

        character(*), parameter :: procedure = 'WRITE_BITSET'

        bit_count = bits(self)

        call digit_count( self % num_bits, count_digits )

        allocate( character(len=count_digits+bit_count+2)::string, stat=stat )
        if ( stat > 0 ) go to 999

        write( string, "('S', i0)" ) self % num_bits

        string( count_digits + 2:count_digits + 2 ) = "B"
        do bit=0, bit_count-1
            pos = count_digits + 2 + bit_count - bit
            if ( self % test( bit) ) then
                string( pos:pos ) = '1'
            else
                string( pos:pos ) = '0'
            end if
        end do

        if ( present(status) ) status = success

        return

999     if ( present(status) ) then
            status = alloc_fault
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'memory sllocation failure for a string.'
        end if

    contains

        subroutine digit_count( bits, digits )
            integer(bits_kind), intent(in)  :: bits
            integer(bits_kind), intent(out) :: digits

            select case ( bits )
            case ( 0:9 )
                digits = 1

            case ( 10:99 )
                digits = 2

            case ( 100:999 )
                digits = 3

            case ( 1000:9999 )
                digits = 4

            case ( 10000:99999 )
                digits = 5

            case ( 100000:999999 )
                digits = 6

            case ( 1000000:9999999 )
                digits = 7

            case ( 10000000:99999999 )
                digits = 8

            case ( 100000000:999999999 )
                digits = 9

            case ( 1000000000:min(2147483647, huge( self % num_bits ) ) )
                digits = 10

            case default
                error stop module_name // ' % ' // procedure //  &
                    ' internal consistency fault was found.'

            end select

        end subroutine digit_count

    end subroutine write_bitset_string_large


    module subroutine write_bitset_unit_large(self, unit, advance, status)
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
        class(bitset_large), intent(in)        :: self
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


        if ( present( advance ) ) then
            write( unit,            &
                   FMT='(A)',       &
                   advance=advance, &
                   iostat=ierr,     &
                   iomsg=message )  &
                   string
        else
            write( unit,           &
                   FMT='(A)',      &
                   advance='YES',  &
                   iostat=ierr,    &
                   iomsg=message ) &
                   string
        end if
        if (ierr /= 0) go to 999

        return

999     if ( present(status) ) then
            status = write_failure
            return
        else
            error stop module_name // ' % ' // procedure // ' had a ' // &
                'failure on a WRITE statement.'
        end if

    end subroutine write_bitset_unit_large


    elemental module subroutine xor_large(set1, set2)
!
!     Sets the bits in SET1 to the bitwise XOR of the original bits in SET1
!     and SET2. SET1 and SET2 must have the same number of bits otherwise
!     the result is undefined.
!
        type(bitset_large), intent(inout) :: set1
        type(bitset_large), intent(in)    :: set2

        integer(bits_kind) :: ablock

        do ablock = 1, size(set1 % blocks)
            set1 % blocks(ablock) = ieor( set1 % blocks(ablock), &
                                          set2 % blocks(ablock) )
        end do

    end subroutine xor_large

end submodule stdlib_bitset_large
