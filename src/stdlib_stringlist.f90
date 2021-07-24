! stdlib_stringlist.f90 --
!     Module for storing and manipulating lists of strings
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
    implicit none

    private
    public :: stringlist_type
    public :: operator(//)
    public :: operator(+)
    public :: operator(-)
    public :: list_end

    type stringlist_index_type
        private
        logical :: head
        integer :: offset
    end type stringlist_index_type

    type(stringlist_index_type), parameter :: list_head      = stringlist_index_type( .true., 1 )
    type(stringlist_index_type), parameter :: list_end       = stringlist_index_type( .false., 0 )
    type(stringlist_index_type), parameter :: list_after_end = stringlist_index_type( .false., 1 )

    interface operator(+)
        module procedure stringlist_index_add
    end interface

    interface operator(-)
        module procedure stringlist_index_subtract
    end interface

    type string_type
        character(len=:), allocatable :: value
    end type string_type

    type stringlist_type
        private
        integer :: size = 0
        type(string_type), dimension(:), allocatable :: string
    contains
        private
        procedure, public :: destroy                => destroy_list
        procedure         :: insert_string_idx      => insert_string_idx_wrap
        procedure         :: insert_string_int      => insert_string_int_impl
        procedure         :: insert_stringlist_idx  => insert_stringlist_idx_wrap
        procedure         :: insert_stringlist_int  => insert_stringlist_int_impl
        procedure         :: insert_stringarray_idx => insert_stringarray_idx_wrap
        procedure         :: insert_stringarray_int => insert_stringarray_int_impl
        generic, public   :: insert                 => insert_string_int,      insert_string_idx,     &
                                                       insert_stringlist_int,  insert_stringlist_idx, &
                                                       insert_stringarray_int, insert_stringarray_idx
        procedure         :: get_string_int         => get_string_int_impl
        procedure         :: get_string_idx         => get_string_idx_wrap
        generic, public   :: get                    => get_string_int, get_string_idx
        procedure, public :: length                 => length_list
        procedure, public :: sort                   => sort_list
        procedure, public :: index                  => index_of_string
        procedure, public :: index_sub              => index_of_substring
        procedure         :: delete_strings_int_int => delete_strings_int_int_impl
        procedure         :: delete_strings_idx_int => delete_strings_idx_int_wrap
        procedure         :: delete_strings_int_idx => delete_strings_int_idx_wrap
        procedure         :: delete_strings_idx_idx => delete_strings_idx_idx_wrap
        generic, public   :: delete                 => delete_strings_int_int, delete_strings_idx_int, &
                                                       delete_strings_int_idx, delete_strings_idx_idx
        procedure         :: range_list_int_int     => range_list_int_int_impl
        procedure         :: range_list_idx_int     => range_list_idx_int_wrap
        procedure         :: range_list_int_idx     => range_list_int_idx_wrap
        procedure         :: range_list_idx_idx     => range_list_idx_idx_wrap
        generic, public   :: range                  => range_list_int_int, range_list_idx_idx, &
                                                       range_list_int_idx, range_list_idx_int
        procedure         :: replace_string_idx          => replace_string_idx_wrap
        procedure         :: replace_string_int          => replace_string_int_impl
        procedure         :: replace_string_int_int      => replace_string_int_int_impl
        procedure         :: replace_stringarray_int_int => replace_stringarray_int_int_impl
        procedure         :: replace_stringlist_int_int  => replace_stringlist_int_int_impl
        procedure         :: replace_string_idx_idx      => replace_string_idx_idx_wrap
        procedure         :: replace_stringarray_idx_idx => replace_stringarray_idx_idx_wrap
        procedure         :: replace_stringlist_idx_idx  => replace_stringlist_idx_idx_wrap
        procedure         :: replace_string_idx_int      => replace_string_idx_int_wrap
        procedure         :: replace_stringarray_idx_int => replace_stringarray_idx_int_wrap
        procedure         :: replace_stringlist_idx_int  => replace_stringlist_idx_int_wrap
        procedure         :: replace_string_int_idx      => replace_string_int_idx_wrap
        procedure         :: replace_stringarray_int_idx => replace_stringarray_int_idx_wrap
        procedure         :: replace_stringlist_int_idx  => replace_stringlist_int_idx_wrap
        generic, public   :: replace                     => replace_string_int_int, replace_stringarray_int_int, &
                                                            replace_stringlist_int_int, &
                                                            replace_string_idx, replace_string_int, &
                                                            replace_string_idx_idx, replace_stringarray_idx_idx, &
                                                            replace_stringlist_idx_idx, &
                                                            replace_string_idx_int, replace_stringarray_idx_int, &
                                                            replace_stringlist_idx_int, &
                                                            replace_string_int_idx, replace_stringarray_int_idx, &
                                                            replace_stringlist_int_idx
    end type stringlist_type

    interface operator(<)
        module procedure string_lower
    end interface

    interface operator(>)
        module procedure string_greater
    end interface

    interface operator(==)
        module procedure string_equal
    end interface

    interface operator(//)
        module procedure append_string
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
function stringlist_index_add( index, offset )
    type(stringlist_index_type), intent(in) :: index
    integer, intent(in)                     :: offset

    type(stringlist_index_type)             :: stringlist_index_add

    stringlist_index_add        = index
    stringlist_index_add%offset = stringlist_index_add%offset + offset
end function stringlist_index_add

! stringlist_index_subtract --
!     Subtract an integer offset to the special index
!
! Arguments:
!     index                 Special index
!     offset                Offset to be subtracted
!
function stringlist_index_subtract( index, offset )
    type(stringlist_index_type), intent(in) :: index
    integer, intent(in)                     :: offset

    type(stringlist_index_type)             :: stringlist_index_subtract

    stringlist_index_subtract        = index
    stringlist_index_subtract%offset = stringlist_index_subtract%offset - offset
end function stringlist_index_subtract

! compare string_type derived types
!     Required by sorting functions
!
elemental logical function string_lower( string1, string2 )
     type(string_type), intent(in) :: string1
     type(string_type), intent(in) :: string2

     string_lower = string1%value < string2%value
end function string_lower

elemental logical function string_greater( string1, string2 )
     type(string_type), intent(in) :: string1
     type(string_type), intent(in) :: string2

     string_greater = string1%value > string2%value
end function string_greater

elemental logical function string_equal( string1, string2 )
     type(string_type), intent(in) :: string1
     type(string_type), intent(in) :: string2

     string_equal = string1%value == string2%value
end function string_equal

function append_string( list, string )
     type(stringlist_type), intent(in) :: list
     character(len=*), intent(in)      :: string
     type(stringlist_type)             :: append_string

     append_string = list
     call append_string%insert( list_after_end, string )
end function append_string

function prepend_string( string, list )
     character(len=*), intent(in)      :: string
     type(stringlist_type), intent(in) :: list
     type(stringlist_type)             :: prepend_string

     prepend_string = list
     call prepend_string%insert( list_head, string )
end function prepend_string

function append_stringlist( list, slist )
    type(stringlist_type), intent(in) :: list
    type(stringlist_type), intent(in) :: slist
    type(stringlist_type)             :: append_stringlist

    append_stringlist = list
    call append_stringlist%insert( list_after_end, slist )

end function append_stringlist

function append_stringarray( list, sarray )
     type(stringlist_type), intent(in)          :: list
     character(len=*), dimension(:), intent(in) :: sarray
     type(stringlist_type)                      :: append_stringarray

     append_stringarray = list
     call append_stringarray%insert( list_after_end, sarray )
end function append_stringarray

function prepend_stringarray( sarray, list )
     character(len=*), dimension(:), intent(in) :: sarray
     type(stringlist_type), intent(in)          :: list
     type(stringlist_type)                      :: prepend_stringarray

     prepend_stringarray = list
     call prepend_stringarray%insert( list_head, sarray )
end function prepend_stringarray


! destroy_list --
!     Destroy the contetns of the list
!
! Arguments:
!     list                   The list of strings in question
!
subroutine destroy_list( list )
    class(stringlist_type), intent(inout) :: list

    list%size = 0
    deallocate( list%string )
end subroutine destroy_list

! length_list --
!     Return the size (length) of the list
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!
integer function length_list( list )
    class(stringlist_type), intent(in) :: list

    length_list = list%size
end function length_list

! insert_string --
!     Insert a new string (or an array of strings of another list) into the list
!
! Arguments:
!     list                   The list of strings where the new string(s) should be inserted
!     idx                    Index at which to insert the string
!     string                 The string in question
!
subroutine insert_string_idx_wrap( list, idx, string )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: idx
    character(len=*), intent(in)                 :: string

    integer                                      :: idxabs

    idxabs = merge( idx%offset, list%size + idx%offset, idx%head )

    call list%insert( idxabs, string )
end subroutine insert_string_idx_wrap

subroutine insert_stringlist_idx_wrap( list, idx, slist )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: idx
    class(stringlist_type), intent(in)           :: slist

    integer                                      :: idxabs

    idxabs = merge( idx%offset, list%size + idx%offset, idx%head )

    call list%insert( idxabs, slist )
end subroutine insert_stringlist_idx_wrap

subroutine insert_stringarray_idx_wrap( list, idx, sarray )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: idx
    character(len=*), dimension(:), intent(in)   :: sarray

    integer                                      :: idxabs

    idxabs = merge( idx%offset, list%size + idx%offset, idx%head )

    call list%insert( idxabs, sarray )
end subroutine insert_stringarray_idx_wrap

! insert_empty_positions
!     Insert a number of positions for new strings
!
! Arguments:
!     list                   The list of strings where the empty positions should be inserted
!     idxn                   Index at which the positions should be inserted
!     number                 Number of positions
!
subroutine insert_empty_positions( list, idxn, number )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(inout)                       :: idxn
    integer, intent(in)                          :: number

    integer                                      :: i, inew
    integer                                      :: lastidx
    type(string_type), dimension(:), allocatable :: new_string

    !
    ! Clip the index between 1 and size+1
    !
    idxn = max( 1, min(list%size+1, idxn ) )

    !
    ! Check if the array list%string is large enough
    ! Make room in any case
    !
    if ( .not. allocated(list%string) ) then
        allocate(list%string(1) )
    endif

    lastidx = list%size + number

    !
    ! Do we need a copy?
    !
    if ( size(list%string) < lastidx ) then
        allocate( new_string(lastidx) )

        do i = 1,idxn-1
            call move_alloc( list%string(i)%value, new_string(i)%value )
        enddo

        do i = idxn, list%size
            inew = i + number
            call move_alloc( list%string(i)%value, new_string(inew)%value )
        enddo
        call move_alloc( new_string, list%string )
    else
        do i = idxn, list%size
            inew = i + number
            call move_alloc( list%string(i)%value, list%string(inew)%value )
        enddo
    endif

    list%size = list%size + number

end subroutine insert_empty_positions

! insert_string_int_impl --
!     Insert a new string into the list - specific implementation
!
subroutine insert_string_int_impl( list, idx, string )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    character(len=*), intent(in)                 :: string

    integer                                      :: idxn
    type(string_type)                            :: new_element
    type(string_type), dimension(:), allocatable :: new_string

    idxn = idx
    call insert_empty_positions( list, idxn, 1 )

    list%string(idxn)%value = string

end subroutine insert_string_int_impl

! insert_stringlist_int_impl --
!     Insert a list of strings into the list - specific implementation
!
subroutine insert_stringlist_int_impl( list, idx, slist )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    class(stringlist_type), intent(in)           :: slist

    integer                                      :: i
    integer                                      :: idxn, idxnew

    idxn = idx
    call insert_empty_positions( list, idxn, slist%size )

    do i = 1, slist%size
       idxnew = max( 1, idxn ) + i - 1
        list%string(idxnew)%value = slist%string(i)%value
    enddo

end subroutine insert_stringlist_int_impl

! insert_stringarray_int_impl --
!     Insert an array of strings into the list - specific implementatinon
!
subroutine insert_stringarray_int_impl( list, idx, sarray )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    character(len=*), dimension(:), intent(in)   :: sarray

    integer                                      :: i
    integer                                      :: idxn, idxnew

    idxn = idx
    call insert_empty_positions( list, idxn, size(sarray) )

    do i = 1, size(sarray)
       idxnew = max( 1, idxn ) + i - 1
        list%string(idxnew)%value = sarray(i)
    enddo

end subroutine insert_stringarray_int_impl

! get_string --
!     Get the string at a particular index
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     idx                    Index after which to insert the string
!
function get_string_idx_wrap( list, idx )
    class(stringlist_type), intent(in)      :: list
    type(stringlist_index_type), intent(in) :: idx
    character(len=:), allocatable           :: get_string_idx_wrap

    integer                                 :: idxabs

    idxabs = merge( idx%offset, list%size + idx%offset, idx%head )

    get_string_idx_wrap = list%get( idxabs )
end function get_string_idx_wrap

function get_string_int_impl( list, idx )
    class(stringlist_type), intent(in) :: list
    integer, intent(in)                :: idx
    character(len=:), allocatable      :: get_string_int_impl

    integer                            :: idxnew

    !
    ! Examine the actual index:
    ! - if the index is larger than the size, return an empty string
    ! - if the index is equal to list_head, interpret it as index 1
    ! - if the index is negative, calculate the absolute index
    !
    if ( idx > list%size .or. idx < 1 ) then
        get_string_int_impl = ''
    else
        get_string_int_impl = list%string(idx)%value
    endif
end function get_string_int_impl

! sort_list --
!     Sort the list and return the result as a new list
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     ascending              Whether to sort as ascending (true) or not (false)
!
function sort_list( list, ascending )
    class(stringlist_type), intent(in)  :: list
    logical, intent(in), optional       :: ascending

    integer                             :: i
    integer, dimension(:), allocatable  :: idx
    class(stringlist_type), allocatable :: sort_list
    logical                             :: ascending_order

    !
    ! Allocate and fill the index array, then sort the indices
    ! based on the strings
    !
    idx = [ (i ,i=1,list%size) ]

    ascending_order = .true.
    if ( present(ascending) ) then
        ascending_order = ascending
    endif

    if ( ascending_order ) then
        idx = sort_ascending( idx )
    else
        idx = sort_descending( idx )
    endif

    allocate( sort_list )
    allocate( sort_list%string(list%size) )

    do i = 1,list%size
        sort_list%string(i) = list%string(idx(i))
    enddo
    sort_list%size = list%size

contains
recursive function sort_ascending( idx ) result(idxnew)
    integer, dimension(:) :: idx
    integer, dimension(size(idx)) :: idxnew

    if ( size(idx) > 1 ) then
        idxnew = [ sort_ascending( pack( idx, list%string(idx) < list%string(idx(1)) ) ), &
                   pack( idx, list%string(idx) == list%string(idx(1)) )                 , &
                   sort_ascending( pack( idx, list%string(idx) > list%string(idx(1)) ) ) ]
    else
        idxnew = idx
    endif
end function sort_ascending

recursive function sort_descending( idx ) result(idxnew)
    integer, dimension(:) :: idx
    integer, dimension(size(idx)) :: idxnew

    if ( size(idx) > 1 ) then
        idxnew = [ sort_descending( pack( idx, list%string(idx) > list%string(idx(1)) ) ), &
                   pack( idx, list%string(idx) == list%string(idx(1)) )                  , &
                   sort_descending( pack( idx, list%string(idx) < list%string(idx(1)) ) ) ]
    else
        idxnew = idx
    endif
end function sort_descending

end function sort_list

! index_of_string --
!     Return the index in the list of a particular string
!
! Arguments:
!     list                   The list of strings in which to search the string
!     string                 The string to be found
!     back                   Whether to search from the end (true) or not (false, default)
!
integer function index_of_string( list, string, back )
    class(stringlist_type), intent(in)  :: list
    character(len=*), intent(in)        :: string
    logical, intent(in), optional       :: back

    integer                             :: idx
    integer                             :: i
    logical                             :: start_backwards

    start_backwards = .false.
    if ( present(back) ) then
        start_backwards = back
    endif

    idx = 0
    if ( start_backwards) then
        do i = list%size,1,-1
            if ( list%string(i)%value == string ) then
                idx = i
                exit
            endif
        enddo
    else
        do i = 1,list%size
            if ( list%string(i)%value == string ) then
                idx = i
                exit
            endif
        enddo
    endif

    index_of_string = idx
end function index_of_string

! index_of_substring --
!     Return the index in the list of a string containing a particular substring
!
! Arguments:
!     list                   The list of strings in which to search the string
!     substring              The substring to be found
!     back                   Whether to search from the end (true) or not (false, default)
!
integer function index_of_substring( list, substring, back )
    class(stringlist_type), intent(in)  :: list
    character(len=*), intent(in)        :: substring
    logical, intent(in), optional       :: back

    integer                             :: idx
    integer                             :: i
    logical                             :: start_backwards

    start_backwards = .false.
    if ( present(back) ) then
        start_backwards = back
    endif

    idx = 0
    if ( start_backwards) then
        do i = list%size,1,-1
            if ( index(list%string(i)%value, substring) > 0 ) then
                idx = i
                exit
            endif
        enddo
    else
        do i = 1,list%size
            if ( index(list%string(i)%value, substring) > 0 ) then
                idx = i
                exit
            endif
        enddo
    endif

    index_of_substring = idx
end function index_of_substring

! delete_strings --
!     Delete one or more strings from the list
!
! Arguments:
!     list                   The list of strings in which to search the string
!     first                  The position of the first string to be deleted
!     last                   The position of the last string to be deleted
!
! Note:
!     If the range defined by first and last has a zero length or first > last,
!     then nothing happens.
!
subroutine delete_strings_idx_idx_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: first
    type(stringlist_index_type), intent(in) :: last

    integer                                 :: firstpos
    integer                                 :: lastpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )
    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%delete( firstpos, lastpos )
end subroutine delete_strings_idx_idx_wrap

subroutine delete_strings_int_idx_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    integer, intent(in)                     :: first
    type(stringlist_index_type), intent(in) :: last

    integer                                 :: firstpos
    integer                                 :: lastpos

    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%delete( firstpos, lastpos )
end subroutine delete_strings_int_idx_wrap

subroutine delete_strings_idx_int_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: first
    integer, intent(in)                     :: last

    integer                                 :: firstpos
    integer                                 :: lastpos

    firstpos = merge( first%offset,  list%size + first%offset,  first%head )

    call list%delete( firstpos, lastpos )
end subroutine delete_strings_idx_int_wrap

subroutine delete_strings_int_int_impl( list, first, last )
    class(stringlist_type), intent(inout) :: list
    integer, intent(in)                   :: first
    integer, intent(in)                   :: last

    integer                               :: firstpos
    integer                               :: lastpos
    integer                               :: i
    integer                               :: j

    if ( first > list%size .or. last < 1 ) then
        return
    endif

    firstpos = max( 1, min(list%size, first ) )
    lastpos  = max( 1, min(list%size, last ) )

    if ( firstpos > lastpos ) then
        return
    else
        do i = lastpos+1,list%size
            j = firstpos + i - lastpos - 1
            call move_alloc( list%string(i)%value, list%string(j)%value )
        enddo
        do i = list%size - (lastpos-firstpos), list%size
            list%string(i)%value = ''
        enddo

        list%size = list%size - (lastpos-firstpos + 1)
    endif
end subroutine delete_strings_int_int_impl

! range_list --
!     Return a sublist given by the first and last position
!
! Arguments:
!     list                   The list of strings in which to search the string
!     first                  The position of the first string to be deleted
!     last                   The position of the last string to be deleted
!
! Note:
!     If the range defined by first and last has a zero length or first > last,
!     then return an empty list
!
function range_list_idx_idx_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: first
    type(stringlist_index_type), intent(in) :: last
    class(stringlist_type), allocatable     :: range_list_idx_idx_wrap

    integer                                 :: firstpos
    integer                                 :: lastpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )
    lastpos = merge( last%offset,  list%size + last%offset,  last%head )

    range_list_idx_idx_wrap = list%range( firstpos, lastpos )

end function range_list_idx_idx_wrap

function range_list_int_idx_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    integer, intent(in)                     :: first
    type(stringlist_index_type), intent(in) :: last
    class(stringlist_type), allocatable     :: range_list_int_idx_wrap

    integer                                 :: lastpos

    lastpos = merge( last%offset,  list%size + last%offset,  last%head )

    range_list_int_idx_wrap = list%range( first, lastpos )

end function range_list_int_idx_wrap

function range_list_idx_int_wrap( list, first, last )
    class(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: first
    integer, intent(in)                     :: last
    class(stringlist_type), allocatable     :: range_list_idx_int_wrap

    integer                                 :: firstpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )

    range_list_idx_int_wrap = list%range( firstpos, last )

end function range_list_idx_int_wrap

function range_list_int_int_impl( list, first, last )
    class(stringlist_type), intent(inout) :: list
    integer, intent(in)                   :: first
    integer, intent(in)                   :: last
    class(stringlist_type), allocatable   :: range_list_int_int_impl

    integer                               :: firstpos
    integer                               :: lastpos

    allocate( range_list_int_int_impl )

    if ( first > list%size .or. last < 1 ) then
        allocate( range_list_int_int_impl%string(0) )
        return
    endif

    firstpos = max( 1, min(list%size, first ) )
    lastpos  = max( 1, min(list%size, last ) )

    if ( firstpos > lastpos ) then
        allocate( range_list_int_int_impl%string(0) )
        return
    else
        range_list_int_int_impl%size   = lastpos - firstpos + 1
        range_list_int_int_impl%string = list%string(firstpos:lastpos)
    endif
end function range_list_int_int_impl


! replace_string --
!     Replace a string in the list
!
! Arguments:
!     list                   The list of strings in which to replace a string (or a range of strings)
!     first                  First index of the string(s) to be replaced
!     last                   Last index of the string(s) to be replaced
!     string                 The string in question (array of strings or another string list)
!
! Note:
!     For convenience a version that simply replaces a single string is provided
!
subroutine replace_string_idx_wrap( list, idx, string )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: idx
    character(len=*), intent(in)                 :: string

    integer                                      :: idxpos

    idxpos = merge( idx%offset, list%size + idx%offset, idx%head )

    call list%replace( idxpos, string )
end subroutine replace_string_idx_wrap

subroutine replace_string_int_impl( list, idx, string )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    character(len=*), intent(in)                 :: string

    integer                                      :: idxpos

    if ( idx < 1 .or. idx > list%size ) then
        return
    endif

    list%string(idx)%value = string
end subroutine replace_string_int_impl

subroutine replace_string_idx_idx_wrap( list, first, last, string )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: first
    type(stringlist_index_type), intent(in)      :: last
    character(len=*), intent(in)                 :: string

    integer                                      :: firstpos, lastpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )
    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( firstpos, lastpos, string )
end subroutine replace_string_idx_idx_wrap

subroutine replace_string_int_idx_wrap( list, first, last, string )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: first
    type(stringlist_index_type), intent(in)      :: last
    character(len=*), intent(in)                 :: string

    integer                                      :: lastpos

    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( first, lastpos, string )
end subroutine replace_string_int_idx_wrap

subroutine replace_string_idx_int_wrap( list, first, last, string )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: first
    integer, intent(in)                          :: last
    character(len=*), intent(in)                 :: string

    integer                                      :: firstpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )

    call list%replace( firstpos, last, string )
end subroutine replace_string_idx_int_wrap

subroutine replace_string_int_int_impl( list, first, last, string )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: first
    integer, intent(in)                          :: last
    character(len=*), intent(in)                 :: string

    if ( first > list%size .or. last < 1 ) then
        return
    endif
    if ( first > last ) then
        return
    endif

    call list%delete( first, last )
    call list%insert( first, string )
end subroutine replace_string_int_int_impl


subroutine replace_stringlist_idx_idx_wrap( list, first, last, slist )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: first
    type(stringlist_index_type), intent(in)      :: last
    class(stringlist_type), intent(in)           :: slist

    integer                                      :: firstpos, lastpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )
    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( firstpos, lastpos, slist )
end subroutine replace_stringlist_idx_idx_wrap

subroutine replace_stringlist_int_idx_wrap( list, first, last, slist )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: first
    type(stringlist_index_type), intent(in)      :: last
    class(stringlist_type), intent(in)           :: slist

    integer                                      :: lastpos

    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( first, lastpos, slist )
end subroutine replace_stringlist_int_idx_wrap

subroutine replace_stringlist_idx_int_wrap( list, first, last, slist )
    class(stringlist_type), intent(inout)        :: list
    type(stringlist_index_type), intent(in)      :: first
    integer, intent(in)                          :: last
    class(stringlist_type), intent(in)           :: slist

    integer                                      :: firstpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )

    call list%replace( firstpos, last, slist )
end subroutine replace_stringlist_idx_int_wrap

subroutine replace_stringlist_int_int_impl( list, first, last, slist )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: first
    integer, intent(in)                          :: last
    class(stringlist_type), intent(in)           :: slist

    if ( first > list%size .or. last < 1 ) then
        return
    endif
    if ( first > last ) then
        return
    endif

    call list%delete( first, last )
    call list%insert( first, slist )
end subroutine replace_stringlist_int_int_impl


subroutine replace_stringarray_idx_idx_wrap( list, first, last, sarray )
    class(stringlist_type), intent(inout)         :: list
    type(stringlist_index_type), intent(in)       :: first
    type(stringlist_index_type), intent(in)       :: last
    character(len=*), dimension(:), intent(in)    :: sarray

    integer                                       :: firstpos, lastpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )
    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( firstpos, lastpos, sarray )
end subroutine replace_stringarray_idx_idx_wrap

subroutine replace_stringarray_int_idx_wrap( list, first, last, sarray )
    class(stringlist_type), intent(inout)         :: list
    integer, intent(in)                           :: first
    type(stringlist_index_type), intent(in)       :: last
    character(len=*), dimension(:), intent(in)    :: sarray

    integer                                       :: lastpos

    lastpos  = merge( last%offset,  list%size + last%offset,  last%head )

    call list%replace( first, lastpos, sarray )
end subroutine replace_stringarray_int_idx_wrap

subroutine replace_stringarray_idx_int_wrap( list, first, last, sarray )
    class(stringlist_type), intent(inout)         :: list
    type(stringlist_index_type), intent(in)       :: first
    integer, intent(in)                           :: last
    character(len=*), dimension(:), intent(in)    :: sarray

    integer                                       :: firstpos

    firstpos = merge( first%offset, list%size + first%offset, first%head )

    call list%replace( firstpos, last, sarray )
end subroutine replace_stringarray_idx_int_wrap

subroutine replace_stringarray_int_int_impl( list, first, last, sarray )
    class(stringlist_type), intent(inout)         :: list
    integer, intent(in)                           :: first
    integer, intent(in)                           :: last
    character(len=*), dimension(:), intent(in)    :: sarray

    if ( first > list%size .or. last < 1 ) then
        return
    endif
    if ( first > last ) then
        return
    endif

    call list%delete( first, last )
    call list%insert( first, sarray )
end subroutine replace_stringarray_int_int_impl

end module stdlib_stringlist
