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
    use stdlib_string_type, only: string_type, move, assignment(=) !, char, operator(==)
    use stdlib_math, only: clip
    use stdlib_optval, only: optval
    implicit none
    private

    public :: stringlist_type, operator(//)
    public :: list_head, list_tail, fidx, bidx !, operator(+), operator(-)

    type stringlist_index_type
        private
        logical :: head
        integer :: offset

    end type stringlist_index_type

    type(stringlist_index_type), parameter :: list_head      = forward_index(1)
    ! type(stringlist_index_type), parameter :: list_end       = stringlist_index_type( .false., 0 )
    ! type(stringlist_index_type), parameter :: list_after_end = stringlist_index_type( .false., 1 )
    type(stringlist_index_type), parameter :: list_tail      = backward_index(1)

    ! interface operator(+)
    !     module procedure stringlist_index_add
    ! end interface

    ! interface operator(-)
    !     module procedure stringlist_index_subtract
    ! end interface

    interface fidx
        module procedure forward_index
    end interface

    interface bidx
        module procedure backward_index
    end interface

    type stringlist_type
        private
        integer :: size = 0
        type(string_type), dimension(:), allocatable :: stringarray
    
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
        procedure, public :: len                    => length_list
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

pure function forward_index(idx)
    integer, intent(in) :: idx
    type(stringlist_index_type) :: forward_index

    forward_index = stringlist_index_type( .true., idx )

end function forward_index

pure function backward_index(idx)
    integer, intent(in) :: idx
    type(stringlist_index_type) :: backward_index

    backward_index = stringlist_index_type( .false., idx )

end function backward_index

pure function append_char( list, string )
    type(stringlist_type), intent(in) :: list
    character(len=*), intent(in)      :: string
    type(stringlist_type)             :: append_char

    append_char = list // string_type( string )

end function append_char

pure function append_string( list, string )
    type(stringlist_type), intent(in) :: list
    type(string_type), intent(in)     :: string
    type(stringlist_type)             :: append_string

    append_string = list
    call append_string%insert( list_tail, string )

end function append_string

pure function prepend_char( string, list )
    character(len=*), intent(in)      :: string
    type(stringlist_type), intent(in) :: list
    type(stringlist_type)             :: prepend_char

    prepend_char = string_type( string ) // list

end function prepend_char

pure function prepend_string( string, list )
    type(string_type), intent(in)     :: string
    type(stringlist_type), intent(in) :: list
    type(stringlist_type)             :: prepend_string

    prepend_string = list
    call prepend_string%insert( list_head, string )

end function prepend_string

<<<<<<< Updated upstream
function append_stringlist( list, slist )
=======
pure function append_stringlist( list, slist )
>>>>>>> Stashed changes
    type(stringlist_type), intent(in) :: list
    type(stringlist_type), intent(in) :: slist
    type(stringlist_type)             :: append_stringlist

    append_stringlist = list
<<<<<<< Updated upstream
    call append_stringlist%insert( list_after_end, slist )
=======
    call append_stringlist%insert( list_tail, slist )
>>>>>>> Stashed changes

end function append_stringlist

pure function append_stringarray( list, sarray )
    type(stringlist_type), intent(in)          :: list
    character(len=*), dimension(:), intent(in) :: sarray
    type(stringlist_type)                      :: append_stringarray

    append_stringarray = list
    call append_stringarray%insert( list_tail, sarray )

end function append_stringarray

pure function prepend_stringarray( sarray, list )
    character(len=*), dimension(:), intent(in) :: sarray
    type(stringlist_type), intent(in)          :: list
    type(stringlist_type)                      :: prepend_stringarray

    prepend_stringarray = list
    call prepend_stringarray%insert( list_head, sarray )

end function prepend_stringarray


! destroy_list --
!     Destroy the contents of the list
!
! Arguments:
!     list                   The list of strings in question
!
subroutine destroy_list( list )
    type(stringlist_type), intent(out) :: list

    list%size = 0
    deallocate( list%stringarray )

end subroutine destroy_list

! length_list --
!     Return the size (length) of the list
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!
pure integer function length_list( list )
    type(stringlist_type), intent(in) :: list

    length_list = list%size

end function length_list

pure integer function capacity( list )
    type(stringlist_type), intent(in) :: list

    capacity = 0
    if ( allocated(list%stringarray) ) then
        capacity = size(list%stringarray)
    end if

end function capacity

! Convert fidx and bidx to the equivalent integer
pure integer function to_idxn( idx )
    type(stringlist_index_type), intent(in) :: idx
    integer                                 :: to_idxn

    to_idxn = merge( idx%offset, list%size + 2 - idx%offset, idx%head )

end function to_idxn

! insert_string --
!     Insert a new string (or an array of strings of another list) into the list
!
! Arguments:
!     list                   The list of strings where the new string(s) should be inserted
!     idx                    Index at which to insert the string
!     string                 The string in question
!
subroutine insert_char_idx_wrap( list, idx, string )
    type(stringlist_type), intent(inout)    :: list
    type(stringlist_index_type), intent(in) :: idx
    character(len=*), intent(in)            :: string

    call list%insert( idx, string_type( string ) )

end subroutine insert_char_idx_wrap

subroutine insert_string_idx_wrap( list, idx, string )
    type(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: idx
    type(type_string), intent(in)           :: string

    call list%insert( to_idxn(idx), string )

end subroutine insert_string_idx_wrap

subroutine insert_stringlist_idx_wrap( list, idx, slist )
    type(stringlist_type), intent(inout)   :: list
    type(stringlist_index_type), intent(in) :: idx
    type(stringlist_type), intent(in)      :: slist

    call list%insert( to_idxn(idx), slist )

end subroutine insert_stringlist_idx_wrap

subroutine insert_stringarray_idx_wrap( list, idx, sarray )
    type(stringlist_type), intent(inout)      :: list
    type(stringlist_index_type), intent(in)    :: idx
    character(len=*), dimension(:), intent(in) :: sarray

    call list%insert( to_idxn(idx), sarray )

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
    type(stringlist_type), intent(inout)         :: list
    integer, intent(inout)                       :: idxn
    integer, intent(inout)                       :: number

    integer                                      :: i, inew
    integer                                      :: new_size, old_size
    type(string_type), dimension(:), allocatable :: new_stringarray

    if (number > 0) then

        idxn     = clip( idxn, 1, len(list) + 1 )
        old_size = len(list)
        new_size = old_size + number

        if ( capacity(list) < new_size ) then

            allocate( new_stringarray(new_size) )

            do i = 1, idxn - 1
                call move( list%stringarray(i), new_stringarray(i) )
            end do
            do i = idxn, old_size
                inew = i + number
                call move( list%stringarray(i), new_stringarray(inew) )
            end do

            call move_alloc( new_stringarray, list%stringarray )

        else
            do i = old_size, idxn, -1
                inew = i + number
                call move( list%stringarray(i), list%stringarray(inew) )
            end do
        end if

        list%size = new_size

    else
        number = 0
    end if

end subroutine insert_empty_positions

! insert_char_int_impl --
!     Insert a new string into the list - specific implementation
!
subroutine insert_char_int_impl( list, idx, string )
    type(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    character(len=*), intent(in)                 :: string

    call insert( list, idx, string_type(string) )

end subroutine insert_char_int_impl

! insert_string_int_impl --
!     Insert a new string into the list - specific implementation
!
subroutine insert_string_int_impl( list, idx, string )
    type(stringlist_type), intent(inout)        :: list
    integer, intent(in)                          :: idx
    type(string_type), intent(in)                :: string

    integer                                      :: idxn

    idxn = idx
    call insert_empty_positions( list, idxn, 1 )

    list%stringarray(idxn) = string

end subroutine insert_string_int_impl

! insert_stringlist_int_impl --
!     Insert a list of strings into the list - specific implementation
!
subroutine insert_stringlist_int_impl( list, idx, slist )
    type(stringlist_type), intent(inout)         :: list
    integer, intent(in)                          :: idx
    type(stringlist_type), intent(in)            :: slist

    integer                                      :: i
    integer                                      :: idxn, idxnew

    idxn = idx
    call insert_empty_positions( list, idxn, len(list) )

    do i = 1, len(list)
        idxnew = idxn + i - 1
        list%stringarray(idxnew) = slist%stringarray(i)
    end do

end subroutine insert_stringlist_int_impl

! insert_chararray_int_impl --
!     Insert an array of chars into the list - specific implementatinon
!
subroutine insert_chararray_int_impl( list, idx, sarray )
    type(stringlist_type), intent(inout)         :: list
    integer, intent(in)                          :: idx
    character(len=*), dimension(:), intent(in)   :: sarray

    integer                                      :: i
    integer                                      :: idxn, idxnew

    idxn = idx
    call insert_empty_positions( list, idxn, size(sarray) )

    do i = 1, size(sarray)
        idxnew = idxn + i - 1
        list%stringarray(idxnew) = string_type(sarray(i))
    end do

end subroutine insert_chararray_int_impl

! insert_stringarray_int_impl --
!     Insert an array of strings into the list - specific implementatinon
!
subroutine insert_stringarray_int_impl( list, idx, sarray )
    type(stringlist_type), intent(inout)         :: list
    integer, intent(in)                          :: idx
    type(string_type), dimension(:), intent(in)  :: sarray

    integer                                      :: i
    integer                                      :: idxn, idxnew

    idxn = idx
    call insert_empty_positions( list, idxn, size(sarray) )

    do i = 1, size(sarray)
        idxnew = idxn + i - 1
        list%stringarray(idxnew) = sarray(i)
    end do

end subroutine insert_stringarray_int_impl

! get_string --
!     Get the string at a particular index
!
! Arguments:
!     list                   The list of strings to retrieve the string from
!     idx                    Index after which to insert the string
!
pure function get_string_idx_wrap( list, idx )
    type(stringlist_type), intent(in)       :: list
    type(stringlist_index_type), intent(in) :: idx
    type(string_type)                       :: get_string_idx_wrap

    get_string_idx_wrap = list%get( to_idxn(idx) )

end function get_string_idx_wrap

pure function get_string_int_impl( list, idx )
    type(stringlist_type), intent(in)  :: list
    integer, intent(in)                :: idx
    type(string_type)                  :: get_string_int_impl

    integer                            :: idxnew

    !
    ! Examine the actual index:
    ! - if the index is out of bounds, return a string_type equivalent to empty string
    !
    if ( 1 <= idx .or. idx <= len(list) ) then
        get_string_int_impl = list%stringarray(idx)
    end if

end function get_string_int_impl

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
