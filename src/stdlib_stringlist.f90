! stdlib_stringlist.f90 --
!     Module for storing and manipulating lists of strings
!     The strings may have arbitrary lengths, not necessarily the same
!
!     Note: very preliminary
!
!     TODO:
!     insert( list_end, ... ) in an empty list?
!
!     Not implemented yet:
!     insert a list or an array of character strings
!     replace a string, list or an array of character strings
!     concatenate a list with another list or an array
!
module stdlib_stringlist
    implicit none

    private
    integer, parameter, public :: list_head = 0
    integer, parameter, public :: list_end  = -1
    public :: stringlist_type
    public :: operator(//)


    integer, parameter :: initial_size  = 20

    type string_type
        character(len=:), allocatable :: value
    end type string_type

    type stringlist_type
        private
        integer :: size = 0
        type(string_type), dimension(:), allocatable :: string
    contains
        procedure :: destroy   => destroy_list
        procedure :: insert    => insert_string
        procedure :: get       => get_string
        procedure :: length    => length_list
        procedure :: sort      => sort_list
        procedure :: index     => index_of_string
        procedure :: index_sub => index_of_substring
        procedure :: delete    => delete_strings
        procedure :: range     => range_list
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
    end interface
contains

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
     call append_string%insert( list_end, string )
end function append_string

function prepend_string( string, list )
     character(len=*), intent(in)      :: string
     type(stringlist_type), intent(in) :: list
     type(stringlist_type)             :: prepend_string

     prepend_string = list
     call prepend_string%insert( list_head, string )
end function prepend_string

! TODO: concatenate two string lists

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

! abspos --
!     Return the absolute position in the list
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     pos                    Possibly relative position
!
! Note:
!     This is an auxiliary function only intended for internal use
!
integer function abspos( list, pos )
    class(stringlist_type), intent(in) :: list
    integer, intent(in)                :: pos

    if ( pos >= list_head .or. pos <= list%size ) then
        abspos = pos
    elseif ( pos <= list_end ) then
         abspos = list%size - (abs(pos) - 1)
    endif
end function abspos

! inserstring_type --
!     Insert a new string into the list
!
! Arguments:
!     list                   The list of strings where the new string should be inserted
!     idx                    Index after which to insert the string
!     string                 The string in question
!
subroutine insert_string( list, idx, string )
    class(stringlist_type), intent(inout)        :: list
    integer, intent(in)                       :: idx
    character(len=*), intent(in)              :: string

    integer                                   :: i
    integer                                   :: idxnew
    type(string_type)                            :: new_element
    type(string_type), dimension(:), allocatable :: empty_strings

    !
    ! Initialise the list if necessary
    !
    if ( .not. allocated(list%string) ) then
        allocate( list%string(initial_size) )
        do i = 1,size(list%string)
            list%string(i)%value = ''
        enddo
    endif

    !
    ! Check the index:
    ! - if the index is list_head, then shift the entire array
    ! - if the index is list_end or negative in general, determine the absolute index
    ! - if the index is large than the registered size, expand the list
    ! - shift everything after the absolute index
    !
    new_element%value = string

    if ( idx == list_head ) then
        list%size   = list%size + 1
        list%string = [new_element, list%string]
    else
        idxnew = idx
        if ( idx <= list_end ) then
            idxnew = list%size - (abs(idx) - 1)
            if ( idxnew <= 0 ) then
                idxnew = 0
            endif
        endif

        if ( idxnew <= size(list%string) ) then
            list%size   = max( idxnew+1, list%size + 1 )
            list%string = [list%string(1:idxnew), new_element, list%string(idxnew+1:)]
        else
            allocate( empty_strings(idxnew-size(list%string)) )
            do i = 1,size(empty_strings)
                empty_strings(i)%value = ''
            enddo
            list%string            = [list%string, empty_strings, new_element]
            list%size              = idxnew + 1
        endif
    endif
end subroutine insert_string

! get_string_type --
!     Get the string at a particular index
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     idx                    Index after which to insert the string
!
function get_string( list, idx )
    class(stringlist_type), intent(inout) :: list
    integer, intent(in)                :: idx
    character(len=:), allocatable      :: get_string

    integer                            :: idxnew

    !
    ! Examine the actual index:
    ! - if the index is larger than the size, return an empty string
    ! - if the index is equal to list_head, interpret it as index 1
    ! - if the index is negative, calculate the absolute index
    !
    if ( idx > list%size ) then
        get_string = ''
    else
        idxnew = idx
        if ( idx == list_head ) then
            idxnew = 1
        elseif ( idx <= list_end ) then
            idxnew = list%size - (abs(idx) - 1)
        endif

        if ( idxnew < 1 ) then
            get_string = ''
        else
            get_string = list%string(idxnew)%value
        endif
    endif
end function get_string

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

    idx = -1
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

    idx = -1
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
subroutine delete_strings( list, first, last )
    class(stringlist_type), intent(inout) :: list
    integer, intent(in)                   :: first
    integer, intent(in)                   :: last

    integer                               :: firstpos
    integer                               :: lastpos
    integer                               :: i
    integer                               :: j

    firstpos = abspos( list, first )
    lastpos  = min( abspos( list, last ), list%size )

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
end subroutine delete_strings

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
function range_list( list, first, last )
    class(stringlist_type), intent(inout) :: list
    integer, intent(in)                   :: first
    integer, intent(in)                   :: last
    class(stringlist_type), allocatable   :: range_list

    integer                               :: firstpos
    integer                               :: lastpos
    integer                               :: i
    integer                               :: j

    allocate( range_list )

    firstpos = abspos( list, first )
    lastpos  = min( abspos( list, last ), list%size )

    if ( firstpos > lastpos ) then
        allocate( range_list%string(0) )
        return
    else
        range_list%size   = lastpos - firstpos + 1
        range_list%string = list%string(firstpos:lastpos)
    endif
end function range_list

end module stdlib_stringlist
