! example_push.f90 --
!     Demonstrate the push method
!
program example_push
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
    ! Now push a new element to the end
    !

    call list%push( 3 )

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

contains
include 'linked_list_aux.f90'

end program example_push
