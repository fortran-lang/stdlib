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
    public :: list_head, list_tail, fidx, bidx, stringlist_index_type !, operator(+), operator(-)

    type stringlist_index_type
        private
        logical :: forward
        integer :: offset

    end type stringlist_index_type

    type(stringlist_index_type), parameter :: list_head     = stringlist_index_type( .true. , 1 )   ! fidx(1)
    type(stringlist_index_type), parameter :: list_tail     = stringlist_index_type( .false., 1 )   ! bidx(1)
    ! type(stringlist_index_type), parameter :: list_end       = stringlist_index_type( .false., 0 )
    ! type(stringlist_index_type), parameter :: list_after_end = stringlist_index_type( .false., 1 )

    ! interface operator(+)
    !     module procedure stringlist_index_add
    ! end interface

    ! interface operator(-)
    !     module procedure stringlist_index_subtract
    ! end interface

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


        ! procedure, public :: sort                   => sort_list
        ! procedure, public :: index                  => index_of_string, index_of_char
        ! procedure, public :: index_sub              => index_of_substring, index_of_subchar
        ! procedure         :: delete_strings_int_int => delete_strings_int_int_impl
        ! procedure         :: delete_strings_idx_int => delete_strings_idx_int_wrap
        ! procedure         :: delete_strings_int_idx => delete_strings_int_idx_wrap
        ! procedure         :: delete_strings_idx_idx => delete_strings_idx_idx_wrap
        ! generic, public   :: delete                 => delete_strings_int_int, delete_strings_idx_int, &
        !                                                delete_strings_int_idx, delete_strings_idx_idx
        ! procedure         :: range_list_int_int     => range_list_int_int_impl
        ! procedure         :: range_list_idx_int     => range_list_idx_int_wrap
        ! procedure         :: range_list_int_idx     => range_list_int_idx_wrap
        ! procedure         :: range_list_idx_idx     => range_list_idx_idx_wrap
        ! generic, public   :: range                  => range_list_int_int, range_list_idx_idx, &
        !                                                range_list_int_idx, range_list_idx_int
        ! procedure         :: replace_string_idx          => replace_string_idx_wrap
        ! procedure         :: replace_string_int          => replace_string_int_impl
        ! procedure         :: replace_string_int_int      => replace_string_int_int_impl
        ! procedure         :: replace_stringarray_int_int => replace_stringarray_int_int_impl
        ! procedure         :: replace_stringlist_int_int  => replace_stringlist_int_int_impl
        ! procedure         :: replace_string_idx_idx      => replace_string_idx_idx_wrap
        ! procedure         :: replace_stringarray_idx_idx => replace_stringarray_idx_idx_wrap
        ! procedure         :: replace_stringlist_idx_idx  => replace_stringlist_idx_idx_wrap
        ! procedure         :: replace_string_idx_int      => replace_string_idx_int_wrap
        ! procedure         :: replace_stringarray_idx_int => replace_stringarray_idx_int_wrap
        ! procedure         :: replace_stringlist_idx_int  => replace_stringlist_idx_int_wrap
        ! procedure         :: replace_string_int_idx      => replace_string_int_idx_wrap
        ! procedure         :: replace_stringarray_int_idx => replace_stringarray_int_idx_wrap
        ! procedure         :: replace_stringlist_int_idx  => replace_stringlist_int_idx_wrap
        ! generic, public   :: replace                     => replace_string_int_int, replace_stringarray_int_int, &
        !                                                     replace_stringlist_int_int, &
        !                                                     replace_string_idx, replace_string_int, &
        !                                                     replace_string_idx_idx, replace_stringarray_idx_idx, &
        !                                                     replace_stringlist_idx_idx, &
        !                                                     replace_string_idx_int, replace_stringarray_idx_int, &
        !                                                     replace_stringlist_idx_int, &
        !                                                     replace_string_int_idx, replace_stringarray_int_idx, &
        !                                                     replace_stringlist_int_idx

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


! stringlist_index_add --
!     Add an integer offset to the special index
!
! Arguments:
!     index                 Special index
!     offset                Offset to be added
!
! function stringlist_index_add( index, offset )
!     type(stringlist_index_type), intent(in) :: index
!     integer, intent(in)                     :: offset

!     type(stringlist_index_type)             :: stringlist_index_add

!     stringlist_index_add        = index
!     stringlist_index_add%offset = stringlist_index_add%offset + offset

! end function stringlist_index_add

! stringlist_index_subtract --
!     Subtract an integer offset to the special index
!
! Arguments:
!     index                 Special index
!     offset                Offset to be subtracted
!
! function stringlist_index_subtract( index, offset )
!     type(stringlist_index_type), intent(in) :: index
!     integer, intent(in)                     :: offset

!     type(stringlist_index_type)             :: stringlist_index_subtract

!     stringlist_index_subtract        = index
!     stringlist_index_subtract%offset = stringlist_index_subtract%offset - offset

! end function stringlist_index_subtract

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

! pure function get_string_int_impl( list, idxn )
!     class(stringlist_type), intent(in) :: list
!     integer, intent(in)                :: idxn
!     type(string_type)                  :: get_string_int_impl

!     !
!     ! Examine the actual index:
!     ! - if the index is out of bounds, return a string_type equivalent to empty string
!     !
!     if ( 1 <= idxn .and. idxn <= list%len() ) then
!         get_string_int_impl = list%stringarray(idxn)
!     end if

! end function get_string_int_impl

! ! sort_list --
! !     Sort the list and return the result as a new list
! !
! ! Arguments:
! !     list                   The list of strings to retrieve the string from
! !     ascending              Whether to sort as ascending (true) or not (false)
! !
! function sort_list( list, ascending )
!     class(stringlist_type), intent(in)  :: list
!     logical, intent(in), optional       :: ascending

!     integer                             :: i
!     integer, dimension(:), allocatable  :: idx
!     class(stringlist_type), allocatable :: sort_list
!     logical                             :: ascending_order

!     !
!     ! Allocate and fill the index array, then sort the indices
!     ! based on the strings
!     !
!     idx = [ (i ,i=1,list%size) ]

!     ascending_order = .true.
!     if ( present(ascending) ) then
!         ascending_order = ascending
!     endif

!     if ( ascending_order ) then
!         idx = sort_ascending( idx )
!     else
!         idx = sort_descending( idx )
!     endif

!     allocate( sort_list )
!     allocate( sort_list%string(list%size) )

!     do i = 1,list%size
!         sort_list%string(i) = list%string(idx(i))
!     enddo
!     sort_list%size = list%size

! contains
! recursive function sort_ascending( idx ) result(idxnew)
!     integer, dimension(:) :: idx
!     integer, dimension(size(idx)) :: idxnew

!     if ( size(idx) > 1 ) then
!         idxnew = [ sort_ascending( pack( idx, list%string(idx) < list%string(idx(1)) ) ), &
!                    pack( idx, list%string(idx) == list%string(idx(1)) )                 , &
!                    sort_ascending( pack( idx, list%string(idx) > list%string(idx(1)) ) ) ]
!     else
!         idxnew = idx
!     endif
! end function sort_ascending

! recursive function sort_descending( idx ) result(idxnew)
!     integer, dimension(:) :: idx
!     integer, dimension(size(idx)) :: idxnew

!     if ( size(idx) > 1 ) then
!         idxnew = [ sort_descending( pack( idx, list%string(idx) > list%string(idx(1)) ) ), &
!                    pack( idx, list%string(idx) == list%string(idx(1)) )                  , &
!                    sort_descending( pack( idx, list%string(idx) < list%string(idx(1)) ) ) ]
!     else
!         idxnew = idx
!     endif
! end function sort_descending

! end function sort_list

! ! index_of_char --
! !     Return the index in the list of a particular string
! !
! ! Arguments:
! !     list                   The list of strings in which to search the string
! !     string                 The string to be found
! !     back                   Whether to search from the end (true) or not (false, default)
! !
! integer function index_of_char( list, string, back )
!     class(stringlist_type), intent(in)  :: list
!     character(len=*), intent(in)        :: string
!     logical, intent(in), optional       :: back

!     integer                             :: idx
!     integer                             :: i, first, last, stride

!     if ( optval(back, .false.) ) then
!         first = list%size
!         last = 1
!         stride = -1
!     else
!         first = 1
!         last = list%size
!         stride = 1
!     end if

!     idx = 0
!     do i = first, last, stride
!         if ( list%stringarray(i) == string ) then
!             idx = i
!             exit
!         end if
!     end do

!     index_of_string = idx

! end function index_of_char

! integer function index_of_string( list, string, back )
!     class(stringlist_type), intent(in)  :: list
!     type(string_type), intent(in)       :: string
!     logical, intent(in), optional       :: back

!     index_of_string = index_of_char(list, char(string), back)

! end function index_of_string

! ! index_of_substring --
! !     Return the index in the list of a string containing a particular substring
! !
! ! Arguments:
! !     list                   The list of strings in which to search the string
! !     substring              The substring to be found
! !     back                   Whether to search from the end (true) or not (false, default)
! !
! integer function index_of_subchar( list, substring, back )
!     class(stringlist_type), intent(in)  :: list
!     character(len=*), intent(in)        :: substring
!     logical, intent(in), optional       :: back

!     integer                             :: idx
!     integer                             :: i, first, last, stride

!     if ( optval(back, .false.) ) then
!         first = list%size
!         last = 1
!         stride = -1
!     else
!         first = 1
!         last = list%size
!         stride = 1
!     end if

!     idx = 0
!     do i = first, last, stride
!         if ( find( list%stringarray(i), substring ) > 0 ) then
!             idx = i
!             exit
!         end if
!     end do

!     index_of_substring = idx

! end function index_of_subchar

! integer function index_of_substring( list, string, back )
!     class(stringlist_type), intent(in)  :: list
!     type(string_type), intent(in)       :: string
!     logical, intent(in), optional       :: back

!     index_of_substring = index_of_subchar(list, char(string), back)

! end function index_of_substring

! ! delete_strings --
! !     Delete one or more strings from the list
! !
! ! Arguments:
! !     list                   The list of strings in which to search the string
! !     first                  The position of the first string to be deleted
! !     last                   The position of the last string to be deleted
! !
! ! Note:
! !     If the range defined by first and last has a zero length or first > last,
! !     then nothing happens.
! !
! subroutine delete_strings_idx_idx_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     type(stringlist_index_type), intent(in) :: first
!     type(stringlist_index_type), intent(in) :: last

!     integer                                 :: firstpos
!     integer                                 :: lastpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )
!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%delete( firstpos, lastpos )
! end subroutine delete_strings_idx_idx_wrap

! subroutine delete_strings_int_idx_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     integer, intent(in)                     :: first
!     type(stringlist_index_type), intent(in) :: last

!     integer                                 :: firstpos
!     integer                                 :: lastpos

!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%delete( firstpos, lastpos )
! end subroutine delete_strings_int_idx_wrap

! subroutine delete_strings_idx_int_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     type(stringlist_index_type), intent(in) :: first
!     integer, intent(in)                     :: last

!     integer                                 :: firstpos
!     integer                                 :: lastpos

!     firstpos = merge( first%offset,  list%size + first%offset,  first%head )

!     call list%delete( firstpos, lastpos )
! end subroutine delete_strings_idx_int_wrap

! subroutine delete_strings_int_int_impl( list, first, last )
!     class(stringlist_type), intent(inout) :: list
!     integer, intent(in)                   :: first
!     integer, intent(in)                   :: last

!     integer                               :: firstpos
!     integer                               :: lastpos
!     integer                               :: i
!     integer                               :: j

!     if ( first > list%size .or. last < 1 ) then
!         return
!     endif

!     firstpos = max( 1, min(list%size, first ) )
!     lastpos  = max( 1, min(list%size, last ) )

!     if ( firstpos > lastpos ) then
!         return
!     else
!         do i = lastpos+1,list%size
!             j = firstpos + i - lastpos - 1
!             call move_alloc( list%string(i)%value, list%string(j)%value )
!         enddo
!         do i = list%size - (lastpos-firstpos), list%size
!             list%string(i)%value = ''
!         enddo

!         list%size = list%size - (lastpos-firstpos + 1)
!     endif
! end subroutine delete_strings_int_int_impl

! ! range_list --
! !     Return a sublist given by the first and last position
! !
! ! Arguments:
! !     list                   The list of strings in which to search the string
! !     first                  The position of the first string to be deleted
! !     last                   The position of the last string to be deleted
! !
! ! Note:
! !     If the range defined by first and last has a zero length or first > last,
! !     then return an empty list
! !
! function range_list_idx_idx_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     type(stringlist_index_type), intent(in) :: first
!     type(stringlist_index_type), intent(in) :: last
!     class(stringlist_type), allocatable     :: range_list_idx_idx_wrap

!     integer                                 :: firstpos
!     integer                                 :: lastpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )
!     lastpos = merge( last%offset,  list%size + last%offset,  last%head )

!     range_list_idx_idx_wrap = list%range( firstpos, lastpos )

! end function range_list_idx_idx_wrap

! function range_list_int_idx_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     integer, intent(in)                     :: first
!     type(stringlist_index_type), intent(in) :: last
!     class(stringlist_type), allocatable     :: range_list_int_idx_wrap

!     integer                                 :: lastpos

!     lastpos = merge( last%offset,  list%size + last%offset,  last%head )

!     range_list_int_idx_wrap = list%range( first, lastpos )

! end function range_list_int_idx_wrap

! function range_list_idx_int_wrap( list, first, last )
!     class(stringlist_type), intent(inout)   :: list
!     type(stringlist_index_type), intent(in) :: first
!     integer, intent(in)                     :: last
!     class(stringlist_type), allocatable     :: range_list_idx_int_wrap

!     integer                                 :: firstpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )

!     range_list_idx_int_wrap = list%range( firstpos, last )

! end function range_list_idx_int_wrap

! function range_list_int_int_impl( list, first, last )
!     class(stringlist_type), intent(inout) :: list
!     integer, intent(in)                   :: first
!     integer, intent(in)                   :: last
!     class(stringlist_type), allocatable   :: range_list_int_int_impl

!     integer                               :: firstpos
!     integer                               :: lastpos

!     allocate( range_list_int_int_impl )

!     if ( first > list%size .or. last < 1 ) then
!         allocate( range_list_int_int_impl%string(0) )
!         return
!     endif

!     firstpos = max( 1, min(list%size, first ) )
!     lastpos  = max( 1, min(list%size, last ) )

!     if ( firstpos > lastpos ) then
!         allocate( range_list_int_int_impl%string(0) )
!         return
!     else
!         range_list_int_int_impl%size   = lastpos - firstpos + 1
!         range_list_int_int_impl%string = list%string(firstpos:lastpos)
!     endif
! end function range_list_int_int_impl


! ! replace_string --
! !     Replace a string in the list
! !
! ! Arguments:
! !     list                   The list of strings in which to replace a string (or a range of strings)
! !     first                  First index of the string(s) to be replaced
! !     last                   Last index of the string(s) to be replaced
! !     string                 The string in question (array of strings or another string list)
! !
! ! Note:
! !     For convenience a version that simply replaces a single string is provided
! !
! subroutine replace_string_idx_wrap( list, idx, string )
!     class(stringlist_type), intent(inout)        :: list
!     type(stringlist_index_type), intent(in)      :: idx
!     character(len=*), intent(in)                 :: string

!     integer                                      :: idxpos

!     idxpos = merge( idx%offset, list%size + idx%offset, idx%head )

!     call list%replace( idxpos, string )
! end subroutine replace_string_idx_wrap

! subroutine replace_string_int_impl( list, idx, string )
!     class(stringlist_type), intent(inout)        :: list
!     integer, intent(in)                          :: idx
!     character(len=*), intent(in)                 :: string

!     integer                                      :: idxpos

!     if ( idx < 1 .or. idx > list%size ) then
!         return
!     endif

!     list%string(idx)%value = string
! end subroutine replace_string_int_impl

! subroutine replace_string_idx_idx_wrap( list, first, last, string )
!     class(stringlist_type), intent(inout)        :: list
!     type(stringlist_index_type), intent(in)      :: first
!     type(stringlist_index_type), intent(in)      :: last
!     character(len=*), intent(in)                 :: string

!     integer                                      :: firstpos, lastpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )
!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( firstpos, lastpos, string )
! end subroutine replace_string_idx_idx_wrap

! subroutine replace_string_int_idx_wrap( list, first, last, string )
!     class(stringlist_type), intent(inout)        :: list
!     integer, intent(in)                          :: first
!     type(stringlist_index_type), intent(in)      :: last
!     character(len=*), intent(in)                 :: string

!     integer                                      :: lastpos

!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( first, lastpos, string )
! end subroutine replace_string_int_idx_wrap

! subroutine replace_string_idx_int_wrap( list, first, last, string )
!     class(stringlist_type), intent(inout)        :: list
!     type(stringlist_index_type), intent(in)      :: first
!     integer, intent(in)                          :: last
!     character(len=*), intent(in)                 :: string

!     integer                                      :: firstpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )

!     call list%replace( firstpos, last, string )
! end subroutine replace_string_idx_int_wrap

! subroutine replace_string_int_int_impl( list, first, last, string )
!     class(stringlist_type), intent(inout)        :: list
!     integer, intent(in)                          :: first
!     integer, intent(in)                          :: last
!     character(len=*), intent(in)                 :: string

!     if ( first > list%size .or. last < 1 ) then
!         return
!     endif
!     if ( first > last ) then
!         return
!     endif

!     call list%delete( first, last )
!     call list%insert( first, string )
! end subroutine replace_string_int_int_impl


! subroutine replace_stringlist_idx_idx_wrap( list, first, last, slist )
!     class(stringlist_type), intent(inout)        :: list
!     type(stringlist_index_type), intent(in)      :: first
!     type(stringlist_index_type), intent(in)      :: last
!     class(stringlist_type), intent(in)           :: slist

!     integer                                      :: firstpos, lastpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )
!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( firstpos, lastpos, slist )
! end subroutine replace_stringlist_idx_idx_wrap

! subroutine replace_stringlist_int_idx_wrap( list, first, last, slist )
!     class(stringlist_type), intent(inout)        :: list
!     integer, intent(in)                          :: first
!     type(stringlist_index_type), intent(in)      :: last
!     class(stringlist_type), intent(in)           :: slist

!     integer                                      :: lastpos

!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( first, lastpos, slist )
! end subroutine replace_stringlist_int_idx_wrap

! subroutine replace_stringlist_idx_int_wrap( list, first, last, slist )
!     class(stringlist_type), intent(inout)        :: list
!     type(stringlist_index_type), intent(in)      :: first
!     integer, intent(in)                          :: last
!     class(stringlist_type), intent(in)           :: slist

!     integer                                      :: firstpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )

!     call list%replace( firstpos, last, slist )
! end subroutine replace_stringlist_idx_int_wrap

! subroutine replace_stringlist_int_int_impl( list, first, last, slist )
!     class(stringlist_type), intent(inout)        :: list
!     integer, intent(in)                          :: first
!     integer, intent(in)                          :: last
!     class(stringlist_type), intent(in)           :: slist

!     if ( first > list%size .or. last < 1 ) then
!         return
!     endif
!     if ( first > last ) then
!         return
!     endif

!     call list%delete( first, last )
!     call list%insert( first, slist )
! end subroutine replace_stringlist_int_int_impl


! subroutine replace_stringarray_idx_idx_wrap( list, first, last, sarray )
!     class(stringlist_type), intent(inout)         :: list
!     type(stringlist_index_type), intent(in)       :: first
!     type(stringlist_index_type), intent(in)       :: last
!     character(len=*), dimension(:), intent(in)    :: sarray

!     integer                                       :: firstpos, lastpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )
!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( firstpos, lastpos, sarray )
! end subroutine replace_stringarray_idx_idx_wrap

! subroutine replace_stringarray_int_idx_wrap( list, first, last, sarray )
!     class(stringlist_type), intent(inout)         :: list
!     integer, intent(in)                           :: first
!     type(stringlist_index_type), intent(in)       :: last
!     character(len=*), dimension(:), intent(in)    :: sarray

!     integer                                       :: lastpos

!     lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

!     call list%replace( first, lastpos, sarray )
! end subroutine replace_stringarray_int_idx_wrap

! subroutine replace_stringarray_idx_int_wrap( list, first, last, sarray )
!     class(stringlist_type), intent(inout)         :: list
!     type(stringlist_index_type), intent(in)       :: first
!     integer, intent(in)                           :: last
!     character(len=*), dimension(:), intent(in)    :: sarray

!     integer                                       :: firstpos

!     firstpos = merge( first%offset, list%size + first%offset, first%head )

!     call list%replace( firstpos, last, sarray )
! end subroutine replace_stringarray_idx_int_wrap

! subroutine replace_stringarray_int_int_impl( list, first, last, sarray )
!     class(stringlist_type), intent(inout)         :: list
!     integer, intent(in)                           :: first
!     integer, intent(in)                           :: last
!     character(len=*), dimension(:), intent(in)    :: sarray

!     if ( first > list%size .or. last < 1 ) then
!         return
!     endif
!     if ( first > last ) then
!         return
!     endif

!     call list%delete( first, last )
!     call list%insert( first, sarray )
! end subroutine replace_stringarray_int_int_impl

end module stdlib_stringlist
