! stdlib_stringlist_type.f90 --
!     Module for storing and manipulating list of strings
!     The strings may have arbitrary lengths, not necessarily the same
!
!     insert AT:      Inserts an element BEFORE the element present currently at the asked index
!                       for forward indexes
!                     Inserts an element AFTER the element present currently at the asked index
!                       for backward indexes
!                     In other words, after insertion the element will be present at the asked index
!                       for both forward and backward indexes                    
!     insert BEFORE:  Inserts an element BEFORE the element present currently at the asked index
!     insert AFTER:   Inserts an element AFTER the element present currently at the asked index
!
!     Note the distinction between AT and BEFORE in the whole module. Care has been taken to 
!     keep this terminology consistent throughout the module.
!
module stdlib_stringlist_type
    use stdlib_string_type, only: string_type, operator(/=), move
    use stdlib_math, only: clip
    implicit none
    private

    public :: stringlist_type, operator(//), operator(==), operator(/=)
    public :: list_head, list_tail, fidx, bidx, stringlist_index_type

    type stringlist_index_type
        private
        logical :: forward
        integer :: offset

    end type stringlist_index_type

    type(stringlist_index_type), parameter :: list_head     = stringlist_index_type( .true. , 1 )   ! fidx(1)
    type(stringlist_index_type), parameter :: list_tail     = stringlist_index_type( .false., 1 )   ! bidx(1)

    !> Version: experimental
    !> 
    !> Returns an instance of type 'stringlist_index_type' representing forward index
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#fidx)
    interface fidx
        module procedure forward_index
    end interface

    !> Version: experimental
    !> 
    !> Returns an instance of type 'stringlist_index_type' representing backward index
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#bidx)
    interface bidx
        module procedure backward_index
    end interface

    type stringlist_type
        private
        type(string_type), dimension(:), allocatable :: stringarray
    
    contains
        private

        procedure, public :: clear                          =>  clear_list

        procedure, public :: len                            =>  length_list

        procedure         :: to_future_at_idxn

        procedure         :: to_current_idxn

        procedure         :: insert_at_char_idx
        procedure         :: insert_at_string_idx
        procedure         :: insert_at_stringlist_idx
        procedure         :: insert_at_chararray_idx
        procedure         :: insert_at_stringarray_idx
        generic, public   :: insert_at                      =>  insert_at_char_idx,             &
                                                                insert_at_string_idx,           &
                                                                insert_at_stringlist_idx,       &
                                                                insert_at_chararray_idx,        &
                                                                insert_at_stringarray_idx

        procedure         :: insert_before_string_idxn
        procedure         :: insert_before_stringlist_idxn
        procedure         :: insert_before_chararray_idxn
        procedure         :: insert_before_stringarray_idxn
        generic           :: insert_before                  =>  insert_before_string_idxn,      &
                                                                insert_before_stringlist_idxn,  &
                                                                insert_before_chararray_idxn,   &
                                                                insert_before_stringarray_idxn

        procedure         :: get_idx
        procedure         :: get_range_idx
        generic, public   :: get                            =>  get_idx,                        &
                                                                get_range_idx
        
        procedure         :: pop_idx
        procedure         :: pop_range_idx
        generic, public   :: pop                            =>  pop_idx,                        &
                                                                pop_range_idx

        procedure         :: drop_idx
        procedure         :: drop_range_idx
        generic, public   :: drop                           =>  drop_idx,                       &
                                                                drop_range_idx

    end type stringlist_type

    !> Version: experimental
    !>
    !> Constructor for stringlist
    !> Returns an instance of type stringlist_type
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#stringlist_type)
    interface stringlist_type
        module procedure new_stringlist
        module procedure new_stringlist_carray
        module procedure new_stringlist_sarray
    end interface

    !> Version: experimental
    !> 
    !> Concatenates stringlist with the input entity
    !> Returns a new stringlist
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#append-operator)
    interface operator(//)
        module procedure append_char
        module procedure append_string
        module procedure prepend_char
        module procedure prepend_string
        module procedure append_stringlist
        module procedure append_carray
        module procedure append_sarray
        module procedure prepend_carray
        module procedure prepend_sarray
    end interface

    !> Version: experimental
    !> 
    !> Compares stringlist for equality with the input entity
    !> Returns a logical
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#equality-operator)
    interface operator(==)
        module procedure eq_stringlist
        module procedure eq_stringlist_carray
        module procedure eq_stringlist_sarray
        module procedure eq_carray_stringlist
        module procedure eq_sarray_stringlist
    end interface

    !> Version: experimental
    !> 
    !> Compares stringlist for inequality with the input entity
    !> Returns a logical
    !> [Specifications](../page/specs/stdlib_stringlist_type.html#inequality-operator)
    interface operator(/=)
        module procedure ineq_stringlist
        module procedure ineq_stringlist_carray
        module procedure ineq_stringlist_sarray
        module procedure ineq_carray_stringlist
        module procedure ineq_sarray_stringlist
    end interface

contains

  ! constructor for stringlist_type:

    !> Constructor with no argument
    !> Returns a new instance of type stringlist 
    pure function new_stringlist()
        type(stringlist_type)                           :: new_stringlist

    end function new_stringlist

    !> Constructor to convert chararray to stringlist
    !> Returns a new instance of type stringlist
    pure function new_stringlist_carray( array )
        character(len=*), dimension(:), intent(in)      :: array
        type(stringlist_type)                           :: new_stringlist_carray

        type(string_type), allocatable                  :: sarray(:)
        integer                                         :: i

        allocate( sarray( size(array) ) )
        do i = 1, size(array)
            sarray(i) = string_type( array(i) )
        end do

        call move_alloc( sarray, new_stringlist_carray%stringarray )
        
    end function new_stringlist_carray

    !> Constructor to convert stringarray to stringlist
    !> Returns a new instance of type stringlist
    pure function new_stringlist_sarray( array )
        type(string_type), dimension(:), intent(in)     :: array
        type(stringlist_type)                           :: new_stringlist_sarray

        new_stringlist_sarray%stringarray = array

    end function new_stringlist_sarray

  ! constructor for stringlist_index_type:

    !> Returns an instance of type 'stringlist_index_type' representing forward index 'idx'
    pure function forward_index( idx )
        integer, intent(in) :: idx
        type(stringlist_index_type) :: forward_index

        forward_index = stringlist_index_type( .true., idx )

    end function forward_index

    !> Returns an instance of type 'stringlist_index_type' representing backward index 'idx'
    pure function backward_index( idx )
        integer, intent(in) :: idx
        type(stringlist_index_type) :: backward_index

        backward_index = stringlist_index_type( .false., idx )

    end function backward_index

  ! concatenation operator:

    !> Appends character scalar 'rhs' to the stringlist 'list'
    !> Returns a new stringlist
    pure function append_char( lhs, rhs )
        type(stringlist_type), intent(in) :: lhs
        character(len=*), intent(in)      :: rhs
        type(stringlist_type)             :: append_char

        append_char = lhs ! Intent: creating a full, deep copy
        call append_char%insert_at( list_tail, rhs )

    end function append_char

    !> Appends string 'rhs' to the stringlist 'list'
    !> Returns a new stringlist
    pure function append_string( lhs, rhs )
        type(stringlist_type), intent(in) :: lhs
        type(string_type), intent(in)     :: rhs
        type(stringlist_type)             :: append_string

        append_string = lhs ! Intent: creating a full, deep copy
        call append_string%insert_at( list_tail, rhs )

    end function append_string

    !> Prepends character scalar 'lhs' to the stringlist 'rhs'
    !> Returns a new stringlist
    pure function prepend_char( lhs, rhs )
        character(len=*), intent(in)      :: lhs
        type(stringlist_type), intent(in) :: rhs
        type(stringlist_type)             :: prepend_char

        prepend_char = rhs ! Intent: creating a full, deep copy
        call prepend_char%insert_at( list_head, lhs )

    end function prepend_char

    !> Prepends string 'lhs' to the stringlist 'rhs'
    !> Returns a new stringlist
    pure function prepend_string( lhs, rhs )
        type(string_type), intent(in)     :: lhs
        type(stringlist_type), intent(in) :: rhs
        type(stringlist_type)             :: prepend_string

        prepend_string = rhs ! Intent: creating a full, deep copy
        call prepend_string%insert_at( list_head, lhs )

    end function prepend_string

    !> Appends stringlist 'rhs' to the stringlist 'lhs'
    !> Returns a new stringlist
    pure function append_stringlist( lhs, rhs )
        type(stringlist_type), intent(in) :: lhs
        type(stringlist_type), intent(in) :: rhs
        type(stringlist_type)             :: append_stringlist

        append_stringlist = lhs ! Intent: creating a full, deep copy
        call append_stringlist%insert_at( list_tail, rhs )

    end function append_stringlist

    !> Appends chararray 'rhs' to the stringlist 'lhs'
    !> Returns a new stringlist
    pure function append_carray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        character(len=*), dimension(:), intent(in)  :: rhs
        type(stringlist_type)                       :: append_carray

        append_carray = lhs ! Intent: creating a full, deep copy
        call append_carray%insert_at( list_tail, rhs )

    end function append_carray

    !> Appends stringarray 'rhs' to the stringlist 'lhs'
    !> Returns a new stringlist
    pure function append_sarray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        type(string_type), dimension(:), intent(in) :: rhs
        type(stringlist_type)                       :: append_sarray

        append_sarray = lhs ! Intent: creating a full, deep copy
        call append_sarray%insert_at( list_tail, rhs )

    end function append_sarray

    !> Prepends chararray 'lhs' to the stringlist 'rhs'
    !> Returns a new stringlist
    pure function prepend_carray( lhs, rhs )
        character(len=*), dimension(:), intent(in) :: lhs
        type(stringlist_type), intent(in)          :: rhs
        type(stringlist_type)                      :: prepend_carray

        prepend_carray = rhs ! Intent: creating a full, deep copy
        call prepend_carray%insert_at( list_head, lhs )

    end function prepend_carray

    !> Prepends stringarray 'lhs' to the stringlist 'rhs'
    !> Returns a new stringlist
    pure function prepend_sarray( lhs, rhs )
        type(string_type), dimension(:), intent(in) :: lhs
        type(stringlist_type), intent(in)           :: rhs
        type(stringlist_type)                       :: prepend_sarray

        prepend_sarray = rhs ! Intent: creating a full, deep copy
        call prepend_sarray%insert_at( list_head, lhs )

    end function prepend_sarray

  ! equality operator:

    !> Compares stringlist 'lhs' for equality with stringlist 'rhs'
    !> Returns a logical
    pure logical function eq_stringlist( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        type(stringlist_type), intent(in)           :: rhs
        integer                                     :: i

        eq_stringlist = .false.
        if ( lhs%len() == rhs%len() ) then
            eq_stringlist = .true.
            do i = 1, lhs%len()
                if ( lhs%stringarray(i) /= rhs%stringarray(i) ) then
                    eq_stringlist = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist

    !> Compares stringlist 'lhs' for equality with chararray 'rhs'
    !> Returns a logical
    pure logical function eq_stringlist_carray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        character(len=*), dimension(:), intent(in)  :: rhs
        integer                                     :: i

        eq_stringlist_carray = .false.
        if ( lhs%len() == size( rhs ) ) then
            eq_stringlist_carray = .true.
            do i = 1, lhs%len()
                if ( lhs%stringarray(i) /= rhs(i) ) then
                    eq_stringlist_carray = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist_carray

    !> Compares stringlist 'lhs' for equality with stringarray 'rhs'
    !> Returns a logical
    pure logical function eq_stringlist_sarray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        type(string_type), dimension(:), intent(in) :: rhs
        integer                                     :: i

        eq_stringlist_sarray = .false.
        if ( lhs%len() == size( rhs ) ) then
            eq_stringlist_sarray = .true.
            do i = 1, lhs%len()
                if ( lhs%stringarray(i) /= rhs(i) ) then
                    eq_stringlist_sarray = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist_sarray

    !> Compares chararray 'lhs' for equality with stringlist 'rhs'
    !> Returns a logical
    pure logical function eq_carray_stringlist( lhs, rhs )
        character(len=*), dimension(:), intent(in)  :: lhs
        type(stringlist_type), intent(in)           :: rhs

        eq_carray_stringlist = ( rhs == lhs )

    end function eq_carray_stringlist

    !> Compares stringarray 'lhs' for equality with stringlist 'rhs'
    !> Returns a logical
    pure logical function eq_sarray_stringlist( lhs, rhs )
        type(string_type), dimension(:), intent(in) :: lhs
        type(stringlist_type), intent(in)           :: rhs

        eq_sarray_stringlist = ( rhs == lhs )

    end function eq_sarray_stringlist

  ! inequality operator:

    !> Compares stringlist 'lhs' for inequality with stringlist 'rhs'
    !> Returns a logical
    pure logical function ineq_stringlist( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        type(stringlist_type), intent(in)           :: rhs

        ineq_stringlist = .not.( lhs == rhs )

    end function ineq_stringlist

    !> Compares stringlist 'lhs' for inequality with chararray 'rhs'
    !> Returns a logical
    pure logical function ineq_stringlist_carray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        character(len=*), dimension(:), intent(in)  :: rhs

        ineq_stringlist_carray = .not.( lhs == rhs ) 

    end function ineq_stringlist_carray

    !> Compares stringlist 'lhs' for inequality with stringarray 'rhs'
    !> Returns a logical
    pure logical function ineq_stringlist_sarray( lhs, rhs )
        type(stringlist_type), intent(in)           :: lhs
        type(string_type), dimension(:), intent(in) :: rhs

        ineq_stringlist_sarray = .not.( lhs == rhs ) 

    end function ineq_stringlist_sarray

    !> Compares chararray 'lhs' for inequality with stringlist 'rhs'
    !> Returns a logical
    pure logical function ineq_carray_stringlist( lhs, rhs )
        character(len=*), dimension(:), intent(in)  :: lhs
        type(stringlist_type), intent(in)           :: rhs

        ineq_carray_stringlist = .not.( lhs == rhs)

    end function ineq_carray_stringlist

    !> Compares stringarray 'lhs' for inequality with stringlist 'rhs'
    !> Returns a logical
    pure logical function ineq_sarray_stringlist( lhs, rhs )
        type(string_type), dimension(:), intent(in) :: lhs
        type(stringlist_type), intent(in)           :: rhs

        ineq_sarray_stringlist = .not.( lhs == rhs )

    end function ineq_sarray_stringlist

  ! clear:

    !> Version: experimental
    !>
    !> Resets stringlist 'list' to an empy stringlist of len 0
    !> Modifies the input stringlist 'list'
    pure subroutine clear_list( list )
        class(stringlist_type), intent(inout) :: list

        if ( allocated( list%stringarray ) ) then
            deallocate( list%stringarray )
        end if

    end subroutine clear_list

  ! len:

    !> Version: experimental
    !>
    !> Returns the len (length) of the list
    !> Returns an integer
    pure integer function length_list( list )
        class(stringlist_type), intent(in) :: list

        length_list = 0
        if ( allocated( list%stringarray ) ) then
            length_list = size( list%stringarray )
        end if

    end function length_list

  ! to_future_at_idxn:

    !> Version: experimental
    !>
    !> Converts a forward index OR a backward index to an integer index at
    !> which the new element will be present post insertion (i.e. in future)
    !> Returns an integer
    pure integer function to_future_at_idxn( list, idx )
        !> Not a part of public API
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx

        ! Formula: merge( fidx( x ) - ( list_head - 1 ), len - bidx( x ) + ( list_tail - 1 ) + 2, ... )
        to_future_at_idxn = merge( idx%offset, list%len() - idx%offset + 2 , idx%forward )

    end function to_future_at_idxn

  ! to_current_idxn:

    !> Version: experimental
    !>
    !> Converts a forward index OR backward index to its equivalent integer index
    !> Returns an integer
    pure integer function to_current_idxn( list, idx )
        !> Not a part of public API
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx

        ! Formula: merge( fidx( x ) - ( list_head - 1 ), len + 1 - bidx( x ) + ( list_tail - 1 ), ... )
        to_current_idxn = merge( idx%offset, list%len() - idx%offset + 1, idx%forward )

    end function to_current_idxn

  ! insert_at:

    !> Version: experimental
    !>
    !> Inserts character scalar 'string' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_at_char_idx( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), intent(in)                :: string

        call list%insert_at( idx, string_type( string ) )

    end subroutine insert_at_char_idx

    !> Version: experimental
    !>
    !> Inserts string 'string' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_at_string_idx( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), intent(in)               :: string

        call list%insert_before( list%to_future_at_idxn( idx ), string )

    end subroutine insert_at_string_idx

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_at_stringlist_idx( list, idx, slist )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(stringlist_type), intent(in)           :: slist

        call list%insert_before( list%to_future_at_idxn( idx ), slist )

    end subroutine insert_at_stringlist_idx

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_at_chararray_idx( list, idx, carray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), dimension(:), intent(in)  :: carray

        call list%insert_before( list%to_future_at_idxn( idx ), carray )

    end subroutine insert_at_chararray_idx

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_at_stringarray_idx( list, idx, sarray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), dimension(:), intent(in) :: sarray

        call list%insert_before( list%to_future_at_idxn( idx ), sarray )

    end subroutine insert_at_stringarray_idx

    !> Version: experimental
    !>
    !> Inserts 'positions' number of empty positions BEFORE integer index 'idxn'
    !> Modifies the input stringlist 'list'
    pure subroutine insert_before_engine( list, idxn, positions )
        !> Not a part of public API
        type(stringlist_type), intent(inout)            :: list
        integer, intent(inout)                          :: idxn
        integer, intent(in)                             :: positions

        integer                                         :: i, inew
        integer                                         :: new_len, old_len
        type(string_type), dimension(:), allocatable    :: new_stringarray

        if (positions > 0) then

            idxn    = clip( idxn, 1, list%len() + 1 )
            old_len = list%len()
            new_len = old_len + positions

            allocate( new_stringarray(new_len) )

            do i = 1, idxn - 1
                call move( list%stringarray(i), new_stringarray(i) )
            end do
            do i = idxn, old_len
                inew = i + positions
                call move( list%stringarray(i), new_stringarray(inew) )
            end do

            call move_alloc( new_stringarray, list%stringarray )

        end if

    end subroutine insert_before_engine

    !> Version: experimental
    !>
    !> Inserts string 'string' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    pure subroutine insert_before_string_idxn( list, idxn, string )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(string_type), intent(in)                   :: string

        integer                                         :: work_idxn

        work_idxn = idxn
        call insert_before_engine( list, work_idxn, 1 )

        list%stringarray(work_idxn) = string

    end subroutine insert_before_string_idxn

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    pure subroutine insert_before_stringlist_idxn( list, idxn, slist )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(stringlist_type), intent(in)               :: slist

        integer                                         :: i
        integer                                         :: work_idxn, inew
        integer                                         :: pre_length, post_length

        pre_length  = slist%len()
        if (pre_length > 0) then
            work_idxn = idxn

            call insert_before_engine( list, work_idxn, pre_length )
            post_length = slist%len()

            inew = work_idxn
            do i = 1, min( work_idxn - 1, pre_length )
                list%stringarray(inew) = slist%stringarray(i)
                inew = inew + 1
            end do

            do i = work_idxn + post_length - pre_length, post_length
                list%stringarray(inew) = slist%stringarray(i)
                inew = inew + 1
            end do
        end if

    end subroutine insert_before_stringlist_idxn

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    pure subroutine insert_before_chararray_idxn( list, idxn, carray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        character(len=*), dimension(:), intent(in)   :: carray

        integer                                      :: i, inew
        integer                                      :: work_idxn

        work_idxn = idxn
        call insert_before_engine( list, work_idxn, size( carray ) )

        do i = 1, size( carray )
            inew = work_idxn + i - 1
            list%stringarray(inew) = string_type( carray(i) )
        end do

    end subroutine insert_before_chararray_idxn

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    pure subroutine insert_before_stringarray_idxn( list, idxn, sarray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        type(string_type), dimension(:), intent(in)  :: sarray

        integer                                      :: i, inew
        integer                                      :: work_idxn

        work_idxn = idxn
        call insert_before_engine( list, work_idxn, size( sarray ) )

        do i = 1, size( sarray )
            inew = work_idxn + i - 1
            list%stringarray(inew) = sarray(i)
        end do

    end subroutine insert_before_stringarray_idxn

  ! get:

    !> Version: experimental
    !>
    !> Returns strings present at stringlist_indexes in interval ['first', 'last']
    !> Stores requested strings in array 'capture_strings'
    !> No return
    pure subroutine get_engine( list, first, last, capture_strings )
        type(stringlist_type), intent(in)                       :: list
        type(stringlist_index_type), intent(in)                 :: first, last
        type(string_type), allocatable, intent(out)             :: capture_strings(:)

        integer                                                 :: from, to
        integer                                                 :: i, inew

        from = max( list%to_current_idxn( first ), 1 )
        to  = min( list%to_current_idxn( last ), list%len() )

        ! out of bounds indexes won't be captured in 'capture_strings'
        if ( from <= to ) then
            allocate( capture_strings( to - from + 1 ) )
            
            inew = 1
            do i = from, to
                capture_strings(inew) = list%stringarray(i)
                inew = inew + 1
            end do

        else
            allocate( capture_strings(0) )
        end if

    end subroutine get_engine

    !> Version: experimental
    !>
    !> Returns the string present at stringlist_index 'idx' in stringlist 'list'
    !> Returns string_type instance
    pure function get_idx( list, idx )
        class(stringlist_type), intent(in)                      :: list
        type(stringlist_index_type), intent(in)                 :: idx

        type(string_type)                                       :: get_idx
        type(string_type), allocatable                          :: capture_strings(:)

        call get_engine( list, idx, idx, capture_strings )

        ! if index 'idx' is out of bounds, returns an empty string
        if ( size(capture_strings) == 1 ) then
            call move( capture_strings(1), get_idx )
        end if

    end function get_idx

    !> Version: experimental
    !>
    !> Returns strings present at stringlist_indexes in interval ['first', 'last']
    !> Returns array of string_type instances
    pure function get_range_idx( list, first, last )
        class(stringlist_type), intent(in)                      :: list
        type(stringlist_index_type), intent(in)                 :: first, last

        type(string_type), allocatable                          :: get_range_idx(:)

        call get_engine( list, first, last, get_range_idx )

    end function get_range_idx

  ! pop & drop:

    !> Version: experimental
    !>
    !> Removes strings present at indexes in interval ['first', 'last']
    !> Stores popped strings in array 'popped_strings'
    !> No return
    pure subroutine pop_drop_engine( list, first, last, popped_strings )
        class(stringlist_type), intent(inout)                   :: list
        type(stringlist_index_type), intent(in)                 :: first, last
        type(string_type), allocatable, intent(out), optional   :: popped_strings(:)

        integer                                                 :: from, to
        integer                                                 :: i, inew, pos, old_len, new_len
        type(string_type), dimension(:), allocatable            :: new_stringarray

        old_len = list%len()
        from    = max( list%to_current_idxn( first ), 1 )
        to      = min( list%to_current_idxn( last ), old_len )
        
        ! out of bounds indexes won't modify stringlist
        if ( from <= to ) then
            pos     = to - from + 1
            new_len = old_len - pos

            allocate( new_stringarray(new_len) )
            do i = 1, from - 1
                call move( list%stringarray(i), new_stringarray(i) )
            end do

            ! capture popped strings
            if ( present(popped_strings) ) then
                allocate( popped_strings(pos) )
                inew = 1
                do i = from, to
                    call move( list%stringarray(i), popped_strings(inew) )
                    inew = inew + 1
                end do
            end if

            inew = from
            do i = to + 1, old_len
                call move( list%stringarray(i), new_stringarray(inew) )
                inew = inew + 1
            end do

            call move_alloc( new_stringarray, list%stringarray )
        else
            if ( present(popped_strings) ) then
                allocate( popped_strings(0) )
            end if
        end if

    end subroutine pop_drop_engine

    !> Version: experimental
    !>
    !> Removes the string present at stringlist_index 'idx' in stringlist 'list'
    !> Returns the removed string
    function pop_idx( list, idx )
        class(stringlist_type), intent(inout)           :: list
        type(stringlist_index_type), intent(in)         :: idx
        type(string_type)                               :: pop_idx

        type(string_type), dimension(:), allocatable    :: popped_strings

        call pop_drop_engine( list, idx, idx, popped_strings )

        if ( size(popped_strings) == 1 ) then
            call move( pop_idx, popped_strings(1) )
        end if

    end function pop_idx

    !> Version: experimental
    !>
    !> Removes strings present at stringlist_indexes in interval ['first', 'last']
    !> in stringlist 'list'
    !> Returns removed strings
    function pop_range_idx( list, first, last )
        class(stringlist_type), intent(inout)           :: list
        type(stringlist_index_type), intent(in)         :: first, last

        type(string_type), dimension(:), allocatable    :: pop_range_idx

        call pop_drop_engine( list, first, last, pop_range_idx )

    end function pop_range_idx

    !> Version: experimental
    !>
    !> Removes the string present at stringlist_index 'idx' in stringlist 'list'
    !> Doesn't return the removed string 
    pure subroutine drop_idx( list, idx )
        class(stringlist_type), intent(inout)           :: list
        type(stringlist_index_type), intent(in)         :: idx

        call pop_drop_engine( list, idx, idx )

    end subroutine drop_idx

    !> Version: experimental
    !>
    !> Removes strings present at stringlist_indexes in interval ['first', 'last']
    !> in stringlist 'list'
    !> Doesn't return removed strings
    pure subroutine drop_range_idx( list, first, last )
        class(stringlist_type), intent(inout)           :: list
        type(stringlist_index_type), intent(in)         :: first, last

        call pop_drop_engine( list, first, last )

    end subroutine drop_range_idx

end module stdlib_stringlist_type
