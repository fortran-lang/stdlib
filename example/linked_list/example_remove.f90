! example_remove.f90 --
!     Demonstrate the remove method
!
program example_remove
    use stdlib_linked_list
    use linked_list_aux

    implicit none

    type(linked_list) :: list

    !
    ! Add a few elements
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )

    call print_list( list )

    !
    ! Now remove the second element
    !

    call list%remove( 2 )

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

end program example_remove
