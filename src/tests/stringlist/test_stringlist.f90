! test_stringlist.f90 --
!
!
! test_stringlists
!     Straightforward test program: try to cover "all" special cases
!
program test_stringlists
    use stdlib_stringlist

    implicit none

    type(stringlist_type) :: list, list_sorted, list_appended, list_to_search
    type(stringlist_type) :: list_to_delete, sublist
    integer               :: i

    !
    ! The straightforward cases: insert a few strings and retrieve them
    !
    call list%insert( list_head, "C" )
    call list%insert( list_head, "B" )
    call list%insert( list_head, "A" )

    !
    ! Now insert a string at an arbitrary location - it will be at position 11
    !
    call list%insert( 10, "Number 10" )

    !
    ! Append a string to the end of the list - that will be position 12
    !
    call list%insert( list_end, "Appended" )

    !
    ! Insert a string near the end of the list - that will be position 12-2+1
    !
    call list%insert( list_end-2, "Appended 2" )

    !
    ! Insert a string beyond the beginning - that should appear at the start
    !
    call list%insert( list_end-30, "Effectively prepended" )

    !
    ! Print the result
    !
    do i = 1,list%length()
        write(*,*) i, '>', list%get(i), '<'
    enddo

    !
    ! And select two elements at the end
    !
    write(*,*) 'end-1' , '>', list%get(list_end-1), '<'
    write(*,*) 'end'   , '>', list%get(list_end), '<'
    write(*,*) 'end-30', '>', list%get(list_end-30), '<'

    !
    ! Okay, finally set an element really far and read it back
    !
    call list%insert( 40, "Really far away" )
    write(*,*) '40', '>', list%get(40), '<'
    write(*,*) '41', '>', list%get(41), '<'
    write(*,*) list%length()

    list_appended = list // "Another string"

    write(*,*) 'List with appended string:'
    do i = 1,list_appended%length()
        write(*,*) i, '>', list_appended%get(i), '<'
    enddo

    !
    ! Sort the list and print the result
    !
    write(*,*) 'Sorted list:'
    list_sorted = list%sort( .false. )
    do i = 1,list_sorted%length()
        write(*,*) i, '>', list_sorted%get(i), '<'
    enddo

    !
    ! Searching strings
    !
    call list_to_search%insert( 0, 'A'   )
    call list_to_search%insert( 1, 'A  ' )
    call list_to_search%insert( 2, 'BB'  )
    call list_to_search%insert( 3, 'CB'  )

    write(*,*) 'First "A": ', list_to_search%index( 'A' )
    write(*,*) 'Last "A":  ', list_to_search%index( 'A', back =.true. )
    write(*,*) 'First "B": ', list_to_search%index_sub( 'B' )
    write(*,*) 'Last "B":  ', list_to_search%index_sub( 'B', back =.true. )

    !
    ! Deleting a string
    !
    write(*,*) 'Deleting a string: '
    list_to_delete = list_to_search
    call list_to_delete%delete( 1, 2 )

    do i = 1,list_to_delete%length()
        write(*,*) '>', list_to_delete%get(i), '<'
    enddo

    !
    ! Get a range
    !
    write(*,*) 'Get a sublist: '
    sublist = list_to_search%range( 2, 3 )

    do i = 1,sublist%length()
        write(*,*) '>', sublist%get(i), '<'
    enddo

end program test_stringlists
