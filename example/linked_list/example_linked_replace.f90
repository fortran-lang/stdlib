! example_replace.f90 --
!     Demonstrate the replace method
!
program example_replace
    use stdlib_linked_list

    implicit none

    type(linked_list_type) :: list

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

contains
include 'linked_list_aux.inc'

end program example_replace
