! example_slice.f90 --
!     Demonstrate the slice method
!
program example_slice
    use stdlib_linked_list
    use linked_list_aux

    implicit none

    type(linked_list) :: list, sublist

    !
    ! Add a few elements to the list
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )
    call list%insert( 5, 4 )
    call list%insert( 6, 5 )

    write(*,*) 'Full list:'
    call print_list( list )

    !
    ! Now construct a sublist via the slice method
    !
    sublist = list%slice( 2, 4 )

    !
    ! Print the resulting list
    !
    write(*,*) 'Original list:'
    call print_list( list )

    !
    ! Print the second list
    write(*,*) 'Sublist:'
    call print_list( sublist)

end program example_slice
