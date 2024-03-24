! test_linked_list.f90 --
!     Tests for the linked list module
!
!     TODO:
!     - absorb
!     - splice <-- Crash
!
!     Also: attention to large lists with multiple parent nodes
!
!     Note:
!     Methods set_size, set_number_of_parent_nodes should be private!
!     Perhaps even number_of_parent_nodes
!
!     Note:
!     slice probably does not work correctly with large lists!
!
program test_linked_list
    use stdlib_error, only: check
    use stdlib_linked_list

    implicit none

    integer :: iunit

    open( newunit = iunit, file = 'test_linked_list.out' )
    write( iunit, '(a)') 'Tests for the linked list module'
    write( iunit, '(a)') ''

    call test_size
    call test_get
    call test_pop
    call test_insert
    call test_reverse
    call test_clear
    call test_replace
    call test_concat
    call test_remove
    call test_slice
    call test_absorb
    call test_splice

    !call test_absorb

    !
    ! Tests with large lists
    !
    call test_size_large

    write( iunit, '(a)') ''
    write( iunit, '(a)') 'Tests completed'
contains

! test_size --
!     Check that the size as returned is correct
!
subroutine test_size
    type(linked_list_type) :: list
    integer           :: i

    !
    ! An empty list should return zero
    !
    write(iunit, '(/,a)') 'Test: sizes'
    write(iunit, '(a,i0)') 'Size of empty list: ', list%size(); flush(iunit)
    call check( 0 == list%size(), "Empty list does not return a zero size", warn=.true. )

    do i = 1,10
        call list%push( i )
        write(iunit, '(a,i0,a,i0)') 'Size of list with ', i, ' elements: ', list%size(); flush(iunit)
        call check( i == list%size(), "List does not return the right size", warn=.true.  )
    enddo

    call list%clear
    write(iunit, '(a,i0)') 'Size of cleared list: ', list%size(); flush(iunit)
    call check( 0 == list%size(), "Cleared list does not return a zero size", warn=.true.  )
end subroutine test_size

! test_get --
!     Check that stored elements are returned correctly via the get function
!
subroutine test_get
    type(linked_list_type) :: list
    integer           :: i
    integer           :: int_val
    real              :: real_val
    character(len=20) :: string_val

    !
    ! Note: the list does not store arrays, so hide it
    !
    type real_array_type
        real :: array(20)
    end type real_array_type

    type(real_array_type) :: ra

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: get'
    !
    ! Store the elements
    !
    int_val    = 1
    real_val   = 2.0
    string_val = "three"
    ra%array = [(real(i), i = 1,size(ra%array))]

    call list%push( int_val )
    call list%push( real_val )
    call list%push( string_val )
    call list%push( ra )

    !
    ! Retrieve them in reverse order (just for fun)
    !
    do i = 4,1,-1
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( i == 1, "List item 1 not an integer", warn=.true. )
            type is (real)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is real with value ', d; flush(iunit)
                call check( i == 2, "List item 2 not a real", warn=.true. )
            type is (character(*))
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is string with value ', d; flush(iunit)
                call check( i == 3, "List item 3 not a string", warn=.true. )
            type is (real_array_type)
                write(iunit, '(a,i0,a,*(g0,1x))') 'Item ', i, ' is derived type containing a real array with values ', d%array
                flush(iunit)
                call check( i == 4, "List item 4 not a derived type 'real_array'", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    !
    ! Change the value of the item - it is a pointer after all.
    !
    data => list%get(1)

    select type ( d => data )
        type is (integer)
            d = 101
    end select

    data => list%get(1)

    select type ( d => data )
        type is (integer)
            write(iunit, '(a,i0,a,g0)') 'Value of item ', i, ' changed to ', d; flush(iunit)
            call check( d == 101, "List item 1 does not have the right value (101)", warn=.true. )
    end select

    call list%clear
end subroutine test_get

! test_pop --
!     Check that stored elements are popped off the list correctly
!
subroutine test_pop
    type(linked_list_type) :: list
    integer           :: i, last

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: pop'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Pop the list (remove the last element) one by one
    !
    do i = 10,1,-1
        call list%pop

        last = list%size()
        data => list%get(last)
        write(iunit, '(a,i0,a,g0)') 'Size after popping item is ', last; flush(iunit)
        call check( last == i-1, "List size is not correct after popping an element", warn=.true. )

        if ( associated(data) ) then
             select type ( d => data )
                type is (integer)
                    write(iunit, '(a,i0,a,g0)') 'Last item ', last, ' is integer with value ', d; flush(iunit)
                    call check( i-1 == d, "List item does not have the right value", warn=.true. )
                class default
                    write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                    call check( .false., "List item encountered of unknown type", warn=.true. )
            end select
        else
            write(iunit, '(a,i0,a,g0)') 'Empty list - pointer is dissociated'; flush(iunit)
        endif
    enddo

    call list%clear
end subroutine test_pop

! test_insert --
!     Check that an element is inserted at the given position (so that the original
!     element is shifted down).
!
subroutine test_insert
    type(linked_list_type) :: list
    integer           :: i
    integer           :: expected(1:2)

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: insert'
    !
    ! Store the elements
    !
    expected = [500, 5]

    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Insert a new value at the fifth position - the fifth element now comes at position 6.
    !
    call list%insert( 500, 5 )

    write(iunit, '(a,i0,a,g0)') 'Size after inserting a new item is ', list%size(); flush(iunit)
    call check( list%size() == 11, "List size is not correct after inserting an element", warn=.true. )


    do i = 5,6
        data => list%get( i )
        if ( associated(data) ) then
            select type ( d => data )
                type is (integer)
                    write(iunit, '(a,i0,a,g0)') 'Item at position ', i, ' is integer with value ', d; flush(iunit)
                    call check( d == expected(i-4), "List item does not have the right value", warn=.true. )
                class default
                    write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                    call check( .false., "List item encountered of unknown type", warn=.true. )
            end select
        else
            write(iunit, '(a,i0,a)') 'No such element ', i, ' - pointer is dissociated'; flush(iunit)
            call check( .false., "List item is missing", warn=.true. )
        endif
    enddo

    !
    ! Insert at the beginning and after the end
    !
    call list%insert( 1000,  -1)
    call list%insert( 2000, 100)

    expected = [1000, 2000]

    do i = 1,2
        if ( i == 1 ) then
            data => list%get( 1 )
        else
            data => list%get( list%size() )
        endif

        if ( associated(data) ) then
            select type ( d => data )
                type is (integer)
                    write(iunit, '(a,i0,a,g0)') 'Item at position ', i, ' is integer with value ', d; flush(iunit)
                    call check( d == expected(i), "List item does not have the right value", warn=.true. )
                class default
                    write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                    call check( .false., "List item encountered of unknown type", warn=.true. )
            end select
        else
            write(iunit, '(a,i0,a)') 'No such element ', i, ' - pointer is dissociated'; flush(iunit)
            call check( .false., "List item is missing", warn=.true. )
        endif
    enddo


    call list%clear
end subroutine test_insert

! test_reverse --
!     Check that a list is properly reversed
!
subroutine test_reverse
    type(linked_list_type) :: list
    integer           :: i

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: reverse'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Reverse the list and check the elements
    !
    call list%reverse

    write(iunit, '(a,i0,a,g0)') 'Size after reversing is ', list%size(); flush(iunit)
    call check( list%size() == 10, "List size is not correct after reversing", warn=.true. )


    do i = 1,list%size()
        data => list%get( i )
        if ( associated(data) ) then
            select type ( d => data )
                type is (integer)
                    write(iunit, '(a,i0,a,g0)') 'Item at position ', i, ' is integer with value ', d; flush(iunit)
                    call check( d == list%size()+1-i, "List item does not have the right value", warn=.true. )
                class default
                    write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                    call check( .false., "List item encountered of unknown type", warn=.true. )
            end select
        else
            write(iunit, '(a,i0,a)') 'No such element ', i, ' - pointer is dissociated'; flush(iunit)
            call check( .false., "List item is missing", warn=.true. )
        endif
    enddo

    call list%clear
end subroutine test_reverse

! test_clear --
!     Check that a cleared list does not have any elements
!
subroutine test_clear
    type(linked_list_type) :: list
    integer           :: i

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: clear'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Reverse the list and check the elements
    !
    call list%clear

    write(iunit, '(a,i0,a,g0)') 'Size after clearing is ', list%size(); flush(iunit)
    call check( list%size() == 0, "List size is not correct after clearing", warn=.true. )

    data => list%get( 1 )

    write(iunit, '(2a)') 'Element 1 exists? - pointer is ', merge( 'associated ', 'dissociated', associated(data) ); flush(iunit)
    call check( .not. associated(data), "There should be no list item returned", warn=.true. )

    call list%clear
end subroutine test_clear

! test_replace --
!     Check that an element is properly replaced
!
subroutine test_replace
    type(linked_list_type) :: list
    integer           :: i
    integer           :: int_val
    character(len=20) :: string_val

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: replace'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Replace element 2 by a string
    !
    call list%replace( "TWO", 2 )

    !
    ! Check the list
    !
    write(iunit, '(a,i0,a,g0)') 'Size after replacing is ', list%size(); flush(iunit)
    call check( list%size() == 10, "List size is not correct after replacing", warn=.true. )


    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( i /= 2, "List item is not an integer", warn=.true. )
            type is (character(*))
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is string with value ', d; flush(iunit)
                call check( i == 2, "List item 2 is not a string", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear
end subroutine test_replace


!
! Tests for large lists
!

! test_size_large --
!     Check that the size of large lists as returned is correct
!
subroutine test_size_large
    type(linked_list_type) :: list
    integer           :: i

    write(iunit, '(/,a)') 'Test: size of large lists'

    do i = 1,100000
        call list%push( i )

        if ( mod(i,5001) == 1 ) then
            write(iunit, '(a,i0,a,i0)') 'Size of list with ', i, ' elements: ', list%size(); flush(iunit)
            write(iunit, '(a,i0,a,i0)') 'Number of parent nodes: ', list%number_of_parent_nodes(); flush(iunit)
            call check( i == list%size(), "List does not return the right size", warn=.true.  )
        endif
    enddo

    call list%clear
end subroutine test_size_large

! test_concat --
!     Check that a list is correctly concatenated to the end of the original list
!
subroutine test_concat
    type(linked_list_type) :: list, sublist
    integer           :: i
    character(len=20) :: string_val

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: concat'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    call sublist%push( 'ONE' )
    call sublist%push( 'TWO' )
    call sublist%push( 'THREE' )
    call sublist%push( 'FOUR' )

    !
    ! Concatenate the sublist
    !
    call list%concat( sublist )

    write(iunit, '(a,i0,a,i0)') 'Size of concatenated list is: ', list%size(); flush(iunit)
    call check( list%size() == 14, "Concatenated list does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( i <= 10, "Item in concatenated list is not an integer", warn=.true. )
            type is (character(*))
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is string with value ', d
                call check( i > 10, "item in concatenated list is not a string", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear
end subroutine test_concat

! test_absorb --
!     Check that an abosrbed list is correctly moved to the absorbing list
!
subroutine test_absorb
    type(linked_list_type) :: list, sublist
    integer           :: i
    character(len=20) :: string_val

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: absorb'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    call sublist%push( 'ONE' )
    call sublist%push( 'TWO' )
    call sublist%push( 'THREE' )
    call sublist%push( 'FOUR' )

    !
    ! Concatenate the sublist and
    ! check that the absorbed list is now empty
    !
    call list%absorb( sublist )

    write(iunit, '(a,i0,a,i0)') 'Size of absorbing list is: ', list%size(); flush(iunit)
    call check( list%size() == 14, "Absorbing list does not return the right size", warn=.true.  )

    write(iunit, '(a,i0,a,i0)') 'Size of absorbed list is: ', sublist%size(); flush(iunit)
    call check( sublist%size() == 0, "Absorbed list does not return the right size", warn=.true.  )

    !
    ! Check the contents of the absorbing list
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( i <= 10, "Item in concatenated list is not an integer", warn=.true. )
            type is (character(*))
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is string with value ', d; flush(iunit)
                call check( i > 10, "item in concatenated list is not a string", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo


    call list%clear
end subroutine test_absorb

! test_remove --
!     Check that a list element is properly removed
!
subroutine test_remove
    type(linked_list_type) :: list
    integer           :: i

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: remove'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Remove the first and the last elements
    !
    call list%remove( 10 )
    call list%remove( 1 )

    write(iunit, '(a,i0,a,i0)') 'Size of list with two removed is: ', list%size(); flush(iunit)
    call check( list%size() == 8, "List with removed elements does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( d > 1 .and. d < 10, "Item in list is out of range", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear
end subroutine test_remove

! test_slice --
!     Check that the proper slice of a list is returned
!
subroutine test_slice
    type(linked_list_type) :: list, slice
    integer           :: i

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: slice'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Get a slice of the list
    !
    slice = list%slice( 2, 4 )

    write(iunit, '(a,i0,a,i0)') 'Size of slice is: ', slice%size(); flush(iunit)
    call check( slice%size() == 3, "Slice does not return the right size", warn=.true.  )

    write(iunit, '(a,i0,a,i0)') 'Size of original list is: ', list%size(); flush(iunit)
    call check( list%size() == 10, "Original list does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,slice%size()
        data => slice%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( d >= 2 .and. d <= 4, "Item in list is out of range", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear
    call slice%clear
end subroutine test_slice

! test_splice --
!     Check that a list is properly spliced (a piece is removed)
!
subroutine test_splice
    type(linked_list_type) :: list
    integer           :: i

    !
    ! Variable returned can be of any type
    !
    class(*), pointer :: data

    write(iunit, '(/,a)') 'Test: splice'
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Remove the middle part of the list
    !
    call list%splice( 2, 9 )

    write(iunit, '(a,i0,a,i0)') 'Size of spliced list is: ', list%size(); flush(iunit)
    call check( list%size() == 2, "Spliced list does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( d == 1 .or. d == 10, "Item in list is out of range", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear

    ! Further tests: remove first half, remove last half
    !
    ! Store the elements
    !
    do i = 1,10
        call list%push( i )
    enddo

    !
    ! Remove the first half
    !
    call list%splice( -1, 5 ) !<== Removing the first element gives trouble!

    write(iunit, '(a,i0,a,i0)') 'Size of spliced list (first half removed) is: ', list%size(); flush(iunit)
    call check( list%size() == 5, "Spliced list (first half removed) does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( d > 5, "Item in list is out of range", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    !
    ! Remove the second half
    !
    call list%splice( 3, 10 )

    write(iunit, '(a,i0,a,i0)') 'Size of spliced list (second half removed) is: ', list%size(); flush(iunit)
    call check( list%size() == 2, "Spliced list (second half removed) does not return the right size", warn=.true.  )

    !
    ! Check the contents
    !
    do i = 1,list%size()
        data => list%get(i)

        select type ( d => data )
            type is (integer)
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is integer with value ', d; flush(iunit)
                call check( d >= 6 .and. d <= 7, "Item in list is out of range", warn=.true. )
            class default
                write(iunit, '(a,i0,a,g0)') 'Item ', i, ' is of unknown type'; flush(iunit)
                call check( .false., "List item encountered of unknown type", warn=.true. )
        end select
    enddo

    call list%clear
end subroutine test_splice

end program test_linked_list
