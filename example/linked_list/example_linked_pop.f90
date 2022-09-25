! example_pop.f90 --
!     Demonstrate the pop method
!
program example_pop
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
    ! Now pop the last element from the list
    !

    call list%pop

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

contains
include 'linked_list_aux.inc'

end program example_pop
