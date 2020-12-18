! stringlist.f90 --
!     Module for storing and manipulating lists of strings
!     The strings may have arbitrary lengths, not necessarily the same
!
!     Note: very preliminary
!
module stdlib_stringlists
    implicit none

    private
    integer, parameter, public :: list_head = 0
    integer, parameter, public :: list_end  = -1
    public :: t_stringlist


    integer, parameter :: initial_size  = 20

    type t_string
        character(len=:), allocatable :: value
    end type t_string

    type t_stringlist
        private
        integer :: size = 0
        type(t_string), dimension(:), allocatable :: string
    contains
        procedure :: insert => insert_string
        procedure :: get    => get_string
        procedure :: length => length_list
    end type t_stringlist

contains

! length_list --
!     Return the size (length) of the list
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!
integer function length_list( list )
    class(t_stringlist), intent(in) :: list

    length_list = list%size
end function length_list

! insert_string --
!     Insert a new string into the list
!
! Arguments:
!     list                   The list of strings where the new string should be inserted
!     idx                    Index after which to insert the string
!     string                 The string in question
!
subroutine insert_string( list, idx, string )
    class(t_stringlist), intent(inout)        :: list
    integer, intent(in)                       :: idx
    character(len=*), intent(in)              :: string

    integer                                   :: i
    integer                                   :: idxnew
    type(t_string)                            :: new_element
    type(t_string), dimension(:), allocatable :: empty_strings

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

! get_string --
!     Get the string at a particular index
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     idx                    Index after which to insert the string
!
function get_string( list, idx )
    class(t_stringlist), intent(inout) :: list
    integer, intent(in)                :: idx
    character(len=:), allocatable      :: get_string

    integer                            :: idxnew
    type(t_string)                     :: new_element

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

end module stdlib_stringlists
