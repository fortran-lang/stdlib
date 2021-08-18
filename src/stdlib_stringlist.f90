! stdlib_stringlist.f90 --
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
!     Note the distinction between AT and BEFORE in the module. Care has been taken to keep it consistent
!     throughout the PR
!
module stdlib_stringlist
    use stdlib_string_type, only: string_type, operator(/=) !, move
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
    !> [Specifications](../page/specs/stdlib_stringlist.html#fidx)
    interface fidx
        module procedure forward_index
    end interface

    !> Version: experimental
    !> 
    !> Returns an instance of type 'stringlist_index_type' representing backward index
    !> [Specifications](../page/specs/stdlib_stringlist.html#bidx)
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

        procedure         :: to_future_at_idxn              =>  convert_to_future_at_idxn

        procedure, public :: to_current_idxn                =>  convert_to_current_idxn

        procedure         :: insert_at_char_idx             =>  insert_at_char_idx_wrap
        procedure         :: insert_at_string_idx           =>  insert_at_string_idx_wrap
        procedure         :: insert_at_stringlist_idx       =>  insert_at_stringlist_idx_wrap
        procedure         :: insert_at_chararray_idx        =>  insert_at_chararray_idx_wrap
        procedure         :: insert_at_stringarray_idx      =>  insert_at_stringarray_idx_wrap
        generic, public   :: insert_at                      =>  insert_at_char_idx,         &
                                                                insert_at_string_idx,       &
                                                                insert_at_stringlist_idx,   &
                                                                insert_at_chararray_idx,    &
                                                                insert_at_stringarray_idx

        procedure         :: insert_before_string_int       =>  insert_before_string_int_impl
        procedure         :: insert_before_stringlist_int   =>  insert_before_stringlist_int_impl
        procedure         :: insert_before_chararray_int    =>  insert_before_chararray_int_impl
        procedure         :: insert_before_stringarray_int  =>  insert_before_stringarray_int_impl
        generic           :: insert_before                  =>  insert_before_string_int,       &
                                                                insert_before_stringlist_int,   &
                                                                insert_before_chararray_int,    &
                                                                insert_before_stringarray_int

        procedure         :: get_string_idx         => get_string_idx_wrap
        generic, public   :: get                    => get_string_idx

    end type stringlist_type

    !> Version: experimental
    !>
    !> Constructor for stringlist
    !> Returns an instance of type stringlist_type
    !> [Specifications](../page/specs/stdlib_stringlist.html#stringlist_type)
    interface stringlist_type
        module procedure new_stringlist
        module procedure new_stringlist_carray
    end interface

    !> Version: experimental
    !> 
    !> Concatenates stringlist with the input entity
    !> Returns a new stringlist
    !> [Specifications](../page/specs/stdlib_stringlist.html#append-operator)
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
    !> [Specifications](../page/specs/stdlib_stringlist.html#equality-operator)
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
    !> [Specifications](../page/specs/stdlib_stringlist.html#inequality-operator)
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
        type(string_type), dimension(0)                 :: sarray

        new_stringlist = stringlist_type( sarray )

    end function new_stringlist

    !> Constructor to convert chararray to stringlist
    !> Returns a new instance of type stringlist
    pure function new_stringlist_carray( array )
        character(len=*), dimension(:), intent(in)      :: array
        type(stringlist_type)                           :: new_stringlist_carray
        type(string_type), dimension( size(array) )     :: sarray
        integer                                         :: i

        do i = 1, size(array)
            sarray(i) = string_type( array(i) )
        end do

        new_stringlist_carray = stringlist_type( sarray )
        
    end function new_stringlist_carray

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

    !> Appends character scalar 'string' to the stringlist 'list'
    !> Returns a new stringlist
    function append_char( list, string )
        type(stringlist_type), intent(in) :: list
        character(len=*), intent(in)      :: string
        type(stringlist_type)             :: append_char

        append_char = list // string_type( string )

    end function append_char

    !> Appends string 'string' to the stringlist 'list'
    !> Returns a new stringlist
    function append_string( list, string )
        type(stringlist_type), intent(in) :: list
        type(string_type), intent(in)     :: string
        type(stringlist_type)             :: append_string

        append_string = list ! Intent: creating a full, deep copy
        call append_string%insert_at( list_tail, string )

    end function append_string

    !> Prepends character scalar 'string' to the stringlist 'list'
    !> Returns a new stringlist
    function prepend_char( string, list )
        character(len=*), intent(in)      :: string
        type(stringlist_type), intent(in) :: list
        type(stringlist_type)             :: prepend_char

        prepend_char = string_type( string ) // list

    end function prepend_char

    !> Prepends string 'string' to the stringlist 'list'
    !> Returns a new stringlist
    function prepend_string( string, list )
        type(string_type), intent(in)     :: string
        type(stringlist_type), intent(in) :: list
        type(stringlist_type)             :: prepend_string

        prepend_string = list ! Intent: creating a full, deep copy
        call prepend_string%insert_at( list_head, string )

    end function prepend_string

    !> Appends stringlist 'slist' to the stringlist 'list'
    !> Returns a new stringlist
    function append_stringlist( list, slist )
        type(stringlist_type), intent(in) :: list
        type(stringlist_type), intent(in) :: slist
        type(stringlist_type)             :: append_stringlist

        append_stringlist = list ! Intent: creating a full, deep copy
        call append_stringlist%insert_at( list_tail, slist )

    end function append_stringlist

    !> Appends chararray 'carray' to the stringlist 'list'
    !> Returns a new stringlist
    function append_carray( list, carray )
        type(stringlist_type), intent(in)           :: list
        character(len=*), dimension(:), intent(in)  :: carray
        type(stringlist_type)                       :: append_carray

        append_carray = list ! Intent: creating a full, deep copy
        call append_carray%insert_at( list_tail, carray )

    end function append_carray

    !> Appends stringarray 'sarray' to the stringlist 'list'
    !> Returns a new stringlist
    function append_sarray( list, sarray )
        type(stringlist_type), intent(in)           :: list
        type(string_type), dimension(:), intent(in) :: sarray
        type(stringlist_type)                       :: append_sarray

        append_sarray = list ! Intent: creating a full, deep copy
        call append_sarray%insert_at( list_tail, sarray )

    end function append_sarray

    !> Prepends chararray 'carray' to the stringlist 'list'
    !> Returns a new stringlist
    function prepend_carray( carray, list )
        character(len=*), dimension(:), intent(in) :: carray
        type(stringlist_type), intent(in)          :: list
        type(stringlist_type)                      :: prepend_carray

        prepend_carray = list ! Intent: creating a full, deep copy
        call prepend_carray%insert_at( list_head, carray )

    end function prepend_carray

    !> Prepends stringarray 'sarray' to the stringlist 'list'
    !> Returns a new stringlist
    function prepend_sarray( sarray, list )
        type(string_type), dimension(:), intent(in) :: sarray
        type(stringlist_type), intent(in)           :: list
        type(stringlist_type)                       :: prepend_sarray

        prepend_sarray = list ! Intent: creating a full, deep copy
        call prepend_sarray%insert_at( list_head, sarray )

    end function prepend_sarray

  ! equality operator:

    !> Compares stringlist 'list' for equality with stringlist 'slist'
    !> Returns a logical
    pure logical function eq_stringlist( list, slist )
        type(stringlist_type), intent(in)           :: list
        type(stringlist_type), intent(in)           :: slist
        integer                                     :: i

        eq_stringlist = .false.
        if ( list%len() == slist%len() ) then
            eq_stringlist = .true.
            do i = 1, list%len()
                if ( list%stringarray(i) /= slist%stringarray(i) ) then
                    eq_stringlist = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist

    !> Compares stringlist 'list' for equality with chararray 'carray'
    !> Returns a logical
    pure logical function eq_stringlist_carray( list, carray )
        type(stringlist_type), intent(in)           :: list
        character(len=*), dimension(:), intent(in)  :: carray
        integer                                     :: i

        eq_stringlist_carray = .false.
        if ( list%len() == size( carray ) ) then
            eq_stringlist_carray = .true.
            do i = 1, list%len()
                if ( list%stringarray(i) /= carray(i) ) then
                    eq_stringlist_carray = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist_carray

    !> Compares stringlist 'list' for equality with stringarray 'sarray'
    !> Returns a logical
    pure logical function eq_stringlist_sarray( list, sarray )
        type(stringlist_type), intent(in)           :: list
        type(string_type), dimension(:), intent(in) :: sarray
        integer                                     :: i

        eq_stringlist_sarray = .false.
        if ( list%len() == size( sarray ) ) then
            eq_stringlist_sarray = .true.
            do i = 1, list%len()
                if ( list%stringarray(i) /= sarray(i) ) then
                    eq_stringlist_sarray = .false.
                    exit
                end if
            end do
        end if

    end function eq_stringlist_sarray

    !> Compares stringlist 'list' for equality with chararray 'carray'
    !> Returns a logical
    pure logical function eq_carray_stringlist( carray, list )
        character(len=*), dimension(:), intent(in)  :: carray
        type(stringlist_type), intent(in)           :: list

        eq_carray_stringlist = ( list == carray )

    end function eq_carray_stringlist

    !> Compares stringlist 'list' for equality with stringarray 'sarray'
    !> Returns a logical
    pure logical function eq_sarray_stringlist( sarray, list )
        type(string_type), dimension(:), intent(in) :: sarray
        type(stringlist_type), intent(in)           :: list

        eq_sarray_stringlist = ( list == sarray )

    end function eq_sarray_stringlist

  ! inequality operator:

    !> Compares stringlist 'list' for inequality with stringlist 'slist'
    !> Returns a logical
    pure logical function ineq_stringlist( list, slist )
        type(stringlist_type), intent(in)           :: list
        type(stringlist_type), intent(in)           :: slist

        ineq_stringlist = .not.( list == slist )

    end function ineq_stringlist

    !> Compares stringlist 'list' for inequality with chararray 'carray'
    !> Returns a logical
    pure logical function ineq_stringlist_carray( list, carray )
        type(stringlist_type), intent(in)           :: list
        character(len=*), dimension(:), intent(in)  :: carray

        ineq_stringlist_carray = .not.( list == carray ) 

    end function ineq_stringlist_carray

    !> Compares stringlist 'list' for inequality with stringarray 'sarray'
    !> Returns a logical
    pure logical function ineq_stringlist_sarray( list, sarray )
        type(stringlist_type), intent(in)           :: list
        type(string_type), dimension(:), intent(in) :: sarray

        ineq_stringlist_sarray = .not.( list == sarray ) 

    end function ineq_stringlist_sarray

    !> Compares stringlist 'list' for inequality with chararray 'carray'
    !> Returns a logical
    pure logical function ineq_carray_stringlist( carray, list )
        character(len=*), dimension(:), intent(in)  :: carray
        type(stringlist_type), intent(in)           :: list

        ineq_carray_stringlist = .not.( carray == list)

    end function ineq_carray_stringlist

    !> Compares stringlist 'list' for inequality with stringarray 'sarray'
    !> Returns a logical
    pure logical function ineq_sarray_stringlist( sarray, list )
        type(string_type), dimension(:), intent(in) :: sarray
        type(stringlist_type), intent(in)           :: list

        ineq_sarray_stringlist = .not.( sarray == list )

    end function ineq_sarray_stringlist

  ! clear:

    !> Version: experimental
    !>
    !> Resets stringlist 'list' to an empy stringlist of len 0
    !> Modifies the input stringlist 'list'
    subroutine clear_list( list )
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
    pure integer function convert_to_future_at_idxn( list, idx )
        !> Not a part of public API
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx

        ! Formula: merge( fidx( x ) - ( list_head - 1 ), len - bidx( x ) + ( list_tail - 1 ) + 2, ... )
        convert_to_future_at_idxn = merge( idx%offset, list%len() - idx%offset + 2 , idx%forward )

    end function convert_to_future_at_idxn

  ! to_current_idxn:

    !> Version: experimental
    !>
    !> Converts a forward index OR backward index to its equivalent integer index idxn
    !> Returns an integer
    pure integer function convert_to_current_idxn( list, idx )
        !> Not a part of public API
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx

        ! Formula: merge( fidx( x ) - ( list_head - 1 ), len + 1 - bidx( x ) + ( list_tail - 1 ), ... )
        convert_to_current_idxn = merge( idx%offset, list%len() - idx%offset + 1, idx%forward )

    end function convert_to_current_idxn

  ! insert_at:

    !> Version: experimental
    !>
    !> Inserts character scalar 'string' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_at_char_idx_wrap( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), intent(in)                :: string

        call list%insert_at( idx, string_type( string ) )

    end subroutine insert_at_char_idx_wrap

    !> Version: experimental
    !>
    !> Inserts string 'string' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_at_string_idx_wrap( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), intent(in)               :: string

        call list%insert_before( list%to_future_at_idxn( idx ), string )

    end subroutine insert_at_string_idx_wrap

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_at_stringlist_idx_wrap( list, idx, slist )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(stringlist_type), intent(in)           :: slist

        call list%insert_before( list%to_future_at_idxn( idx ), slist )

    end subroutine insert_at_stringlist_idx_wrap

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_at_chararray_idx_wrap( list, idx, carray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), dimension(:), intent(in)  :: carray

        call list%insert_before( list%to_future_at_idxn( idx ), carray )

    end subroutine insert_at_chararray_idx_wrap

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' AT stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_at_stringarray_idx_wrap( list, idx, sarray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), dimension(:), intent(in) :: sarray

        call list%insert_before( list%to_future_at_idxn( idx ), sarray )

    end subroutine insert_at_stringarray_idx_wrap

    !> Version: experimental
    !>
    !> Inserts 'positions' number of empty positions BEFORE integer index 'idxn'
    !> Modifies the input stringlist 'list'
    subroutine insert_before_empty_positions( list, idxn, positions )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
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
                ! TODO: can be improved by move
                new_stringarray(i) = list%stringarray(i)
            end do
            do i = idxn, old_len
                inew = i + positions
                ! TODO: can be improved by move
                new_stringarray(inew) = list%stringarray(i)
            end do

            call move_alloc( new_stringarray, list%stringarray )

        end if

    end subroutine insert_before_empty_positions

    !> Version: experimental
    !>
    !> Inserts string 'string' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    subroutine insert_before_string_int_impl( list, idxn, string )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(string_type), intent(in)                   :: string

        integer                                         :: work_idxn

        work_idxn = idxn
        call insert_before_empty_positions( list, work_idxn, 1 )

        list%stringarray(work_idxn) = string

    end subroutine insert_before_string_int_impl

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    subroutine insert_before_stringlist_int_impl( list, idxn, slist )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(stringlist_type), intent(in)               :: slist

        integer                                         :: i
        integer                                         :: work_idxn, idxnew
        integer                                         :: pre_length, post_length

        work_idxn   = idxn
        pre_length  = slist%len()
        call insert_before_empty_positions( list, work_idxn, pre_length )
        post_length = slist%len()

        do i = 1, min( work_idxn - 1, pre_length )
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = slist%stringarray(i)
        end do

        do i = work_idxn + post_length - pre_length, post_length
            idxnew = work_idxn + i - post_length + pre_length - 1
            list%stringarray(idxnew) = slist%stringarray(i)
        end do

    end subroutine insert_before_stringlist_int_impl

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    subroutine insert_before_chararray_int_impl( list, idxn, carray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        character(len=*), dimension(:), intent(in)   :: carray

        integer                                      :: i
        integer                                      :: work_idxn, idxnew

        work_idxn = idxn
        call insert_before_empty_positions( list, work_idxn, size( carray ) )

        do i = 1, size( carray )
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = string_type( carray(i) )
        end do

    end subroutine insert_before_chararray_int_impl

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' BEFORE integer index 'idxn' in the underlying stringarray
    !> Modifies the input stringlist 'list'
    subroutine insert_before_stringarray_int_impl( list, idxn, sarray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        type(string_type), dimension(:), intent(in)  :: sarray

        integer                                      :: i
        integer                                      :: work_idxn, idxnew

        work_idxn = idxn
        call insert_before_empty_positions( list, work_idxn, size( sarray ) )

        do i = 1, size( sarray )
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = sarray(i)
        end do

    end subroutine insert_before_stringarray_int_impl

  ! get:

    !> Version: experimental
    !>
    !> Returns the string present at stringlist_index 'idx' in stringlist 'list'
    !> Returns string_type instance
    pure function get_string_idx_wrap( list, idx )
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx
        type(string_type)                       :: get_string_idx_wrap

        integer                                 :: idxn

        idxn = list%to_current_idxn( idx )

        ! if the index is out of bounds, return a string_type equivalent to empty string
        if ( 1 <= idxn .and. idxn <= list%len() ) then
            get_string_idx_wrap = list%stringarray(idxn)

        end if

    end function get_string_idx_wrap


end module stdlib_stringlist
