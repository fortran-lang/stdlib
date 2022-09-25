! example_reverse.f90 --
!     Demonstrate the reverse method
!
program example_reverse
    use stdlib_linked_list

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
    ! Now reverse the whole list
    !

    call list%reverse

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

contains
include 'linked_list_aux.inc'

end program example_reverse
