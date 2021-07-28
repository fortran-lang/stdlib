! stdlib_stringlist.f90 --
!     Module for storing and manipulating list of strings
!     The strings may have arbitrary lengths, not necessarily the same
!
!     Note: very preliminary
!
!     TODO:
!     insert( list_end, ... ) in an empty list?
!     concatenate two string lists
!
!     Not implemented yet:
!     insert a list or an array of character strings
!     replace a string, list or an array of character strings
!     concatenate a list with another list or an array
!
!     Limited to implemented routines
!
module stdlib_stringlist
    use stdlib_string_type, only: string_type !, move
    use stdlib_math, only: clip
    use stdlib_optval, only: optval
    implicit none
    private

    public :: stringlist_type, operator(//)
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
        integer :: size = 0
        type(string_type), dimension(:), allocatable :: stringarray
    
    contains
        private
        procedure         :: copy                   => create_copy

        procedure, public :: destroy                => destroy_list

        procedure, public :: len                    => length_list

        procedure         :: capacity               => capacity_list

        procedure         :: to_idxn                => convert_to_idxn

        procedure         :: insert_char_idx        => insert_char_idx_wrap
        procedure         :: insert_string_idx      => insert_string_idx_wrap
        procedure         :: insert_stringlist_idx  => insert_stringlist_idx_wrap
        procedure         :: insert_chararray_idx   => insert_chararray_idx_wrap
        procedure         :: insert_stringarray_idx => insert_stringarray_idx_wrap
        generic, public   :: insert                 => insert_char_idx,         &
                                                       insert_string_idx,       &
                                                       insert_stringlist_idx,   &
                                                       insert_chararray_idx,  &
                                                       insert_stringarray_idx

        procedure         :: insert_string_int      => insert_string_int_impl
        procedure         :: insert_stringlist_int  => insert_stringlist_int_impl
        procedure         :: insert_chararray_int   => insert_chararray_int_impl
        procedure         :: insert_stringarray_int => insert_stringarray_int_impl

        ! procedure         :: get_string_int         => get_string_int_impl
        procedure         :: get_string_idx         => get_string_idx_wrap
        generic, public   :: get                    => get_string_idx
                                                        ! get_string_int

    end type stringlist_type

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
        module procedure append_stringarray
        module procedure prepend_stringarray
    end interface

contains

    !> Returns an instance of type 'stringlist_index_type' representing forward index 'idx'
    pure function forward_index(idx)
        integer, intent(in) :: idx
        type(stringlist_index_type) :: forward_index

        forward_index = stringlist_index_type( .true., idx )

    end function forward_index

    !> Returns an instance of type 'stringlist_index_type' representing backward index 'idx'
    pure function backward_index(idx)
        integer, intent(in) :: idx
        type(stringlist_index_type) :: backward_index

        backward_index = stringlist_index_type( .false., idx )

    end function backward_index

    pure function create_copy( original )
        class(stringlist_type), intent(in)  :: original
        type(stringlist_type)               :: create_copy

        create_copy = original

    end function create_copy

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

        append_string = list%copy()
        call append_string%insert( list_tail, string )

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

        prepend_string = list%copy()
        call prepend_string%insert( list_head, string )

    end function prepend_string

    !> Appends stringlist 'slist' to the stringlist 'list'
    !> Returns a new stringlist
    function append_stringlist( list, slist )
        type(stringlist_type), intent(in) :: list
        type(stringlist_type), intent(in) :: slist
        type(stringlist_type)             :: append_stringlist

        append_stringlist = list%copy()
        call append_stringlist%insert( list_tail, slist )

    end function append_stringlist

    !> Appends stringarray 'sarray' to the stringlist 'list'
    !> Returns a new stringlist
    function append_stringarray( list, sarray )
        type(stringlist_type), intent(in)          :: list
        character(len=*), dimension(:), intent(in) :: sarray
        type(stringlist_type)                      :: append_stringarray

        append_stringarray = list%copy()
        call append_stringarray%insert( list_tail, sarray )

    end function append_stringarray

    !> Prepends stringarray 'sarray' to the stringlist 'list'
    !> Returns a new stringlist
    function prepend_stringarray( sarray, list )
        character(len=*), dimension(:), intent(in) :: sarray
        type(stringlist_type), intent(in)          :: list
        type(stringlist_type)                      :: prepend_stringarray

        prepend_stringarray = list%copy()
        call prepend_stringarray%insert( list_head, sarray )

    end function prepend_stringarray

  ! destroy:

    !> Version: experimental
    !>
    !> Resets stringlist 'list' to an empy stringlist of len 0
    !> Modifies the input stringlist 'list'
    subroutine destroy_list( list )
        !> TODO: needs a better name?? like clear_list or reset_list
        class(stringlist_type), intent(out) :: list

        list%size = 0
        deallocate( list%stringarray )

    end subroutine destroy_list

  ! len:

    !> Version: experimental
    !>
    !> Returns the len (length) of the list
    !> Returns an integer
    pure integer function length_list( list )
        class(stringlist_type), intent(in) :: list

        length_list = list%size

    end function length_list

  ! capacity:

    !> Version: experimental
    !>
    !> Returns the capacity of the list
    !> Returns an integer
    pure integer function capacity_list( list )
        !> Not a part of public API
        class(stringlist_type), intent(in) :: list

        capacity_list = 0
        if ( allocated( list%stringarray ) ) then
            capacity_list = size( list%stringarray )
        end if

    end function capacity_list

  ! to_idxn:

    !> Version: experimental
    !>
    !> Converts a forward index or backward index to its equivalent integer index
    !> Returns an integer
    pure integer function convert_to_idxn( list, idx )
        !> Not a part of public API
        class(stringlist_type), intent(in)      :: list
        type(stringlist_index_type), intent(in) :: idx

        convert_to_idxn = merge( idx%offset, list%len() + 2 - idx%offset, idx%forward )

    end function convert_to_idxn

  ! insert:

    !> Version: experimental
    !>
    !> Inserts character scalar 'string' at stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_char_idx_wrap( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), intent(in)                :: string

        call list%insert( idx, string_type( string ) )

    end subroutine insert_char_idx_wrap

    !> Version: experimental
    !>
    !> Inserts string 'string' at stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_string_idx_wrap( list, idx, string )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), intent(in)               :: string

        call list%insert_string_int( list%to_idxn( idx ), string )

    end subroutine insert_string_idx_wrap

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' at stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_stringlist_idx_wrap( list, idx, slist )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(stringlist_type), intent(in)           :: slist

        call list%insert_stringlist_int( list%to_idxn( idx ), slist )

    end subroutine insert_stringlist_idx_wrap

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' at stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_chararray_idx_wrap( list, idx, carray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        character(len=*), dimension(:), intent(in)  :: carray

        call list%insert_chararray_int( list%to_idxn( idx ), carray )

    end subroutine insert_chararray_idx_wrap

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' at stringlist_index 'idx' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_stringarray_idx_wrap( list, idx, sarray )
        class(stringlist_type), intent(inout)       :: list
        type(stringlist_index_type), intent(in)     :: idx
        type(string_type), dimension(:), intent(in) :: sarray

        call list%insert_stringarray_int( list%to_idxn( idx ), sarray )

    end subroutine insert_stringarray_idx_wrap

    !> Version: experimental
    !>
    !> Inserts 'positions' number of empty positions at integer index 'idxn'
    !> Modifies the input stringlist 'list'
    subroutine insert_empty_positions( list, idxn, positions )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(inout)                          :: idxn
        integer, intent(inout)                          :: positions

        integer                                         :: i, inew
        integer                                         :: new_len, old_len
        type(string_type), dimension(:), allocatable    :: new_stringarray

        if (positions > 0) then

            idxn     = clip( idxn, 1, list%len() + 1 )
            old_len = list%len()
            new_len = old_len + positions

            if ( list%capacity() < new_len ) then

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

            else
                do i = old_len, idxn, -1
                    inew = i + positions
                    ! TODO: can be improved by move
                    list%stringarray(inew) = list%stringarray(i)
                end do
            end if

            list%size = new_len

        else
            positions = 0
        end if

    end subroutine insert_empty_positions

! insert_char_int_impl --
!     Insert a new string into the list - specific implementation
!
! subroutine insert_char_int_impl( list, idx, string )
!     type(stringlist_type), intent(inout)            :: list
!     integer, intent(in)                             :: idx
!     character(len=*), intent(in)                    :: string

!     call insert( list, idx, string_type( string ) )

! end subroutine insert_char_int_impl

    !> Version: experimental
    !>
    !> Inserts string 'string' at integer index 'idxn' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_string_int_impl( list, idxn, string )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(string_type), intent(in)                   :: string

        integer                                         :: work_idxn
        integer                                         :: positions

        work_idxn = idxn
        positions = 1
        call insert_empty_positions( list, work_idxn, positions )

        list%stringarray(work_idxn) = string

    end subroutine insert_string_int_impl

    !> Version: experimental
    !>
    !> Inserts stringlist 'slist' at integer index 'idxn' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_stringlist_int_impl( list, idxn, slist )
        !> Not a part of public API
        class(stringlist_type), intent(inout)           :: list
        integer, intent(in)                             :: idxn
        type(stringlist_type), intent(in)               :: slist

        integer                                         :: i
        integer                                         :: work_idxn, idxnew
        integer                                         :: positions

        work_idxn = idxn
        positions = slist%len()
        call insert_empty_positions( list, work_idxn, positions )

        do i = 1, slist%len()
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = slist%stringarray(i)
        end do

    end subroutine insert_stringlist_int_impl

    !> Version: experimental
    !>
    !> Inserts chararray 'carray' at integer index 'idxn' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_chararray_int_impl( list, idxn, carray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        character(len=*), dimension(:), intent(in)   :: carray

        integer                                      :: i
        integer                                      :: work_idxn, idxnew
        integer                                      :: positions

        work_idxn = idxn
        positions = size( carray )
        call insert_empty_positions( list, work_idxn, positions )

        do i = 1, size( carray )
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = string_type( carray(i) )
        end do

    end subroutine insert_chararray_int_impl

    !> Version: experimental
    !>
    !> Inserts stringarray 'sarray' at integer index 'idxn' in stringlist 'list'
    !> Modifies the input stringlist 'list'
    subroutine insert_stringarray_int_impl( list, idxn, sarray )
        !> Not a part of public API
        class(stringlist_type), intent(inout)        :: list
        integer, intent(in)                          :: idxn
        type(string_type), dimension(:), intent(in)  :: sarray

        integer                                      :: i
        integer                                      :: work_idxn, idxnew
        integer                                      :: positions

        work_idxn = idxn
        positions = size( sarray )
        call insert_empty_positions( list, work_idxn, positions )

        do i = 1, size( sarray )
            idxnew = work_idxn + i - 1
            list%stringarray(idxnew) = sarray(i)
        end do

    end subroutine insert_stringarray_int_impl

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

        idxn = list%to_idxn( idx )

        ! - if the index is out of bounds, return a string_type equivalent to empty string
        if ( 1 <= idxn .and. idxn <= list%len() ) then
            get_string_idx_wrap = list%stringarray(idxn)

        end if

    end function get_string_idx_wrap


end module stdlib_stringlist
