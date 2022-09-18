! example_replace.f90 --
!     Demonstrate the replace method
!
include 'linked_list_aux.f90'

program example_replace
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
    ! Now replace the second element by a string
    !

    call list%replace( "Another string", 2 )

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

end program example_replace
