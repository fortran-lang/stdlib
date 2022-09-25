! example_splice.f90 --
!     Demonstrate the splice method
!

program example_splice
    use stdlib_linked_list

    implicit none

    type(linked_list) :: list

    !
    ! Add a few elements to the list
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )
    call list%insert( 5, 1 )
    call list%insert( 6, 2 )

    write(*,*) 'Full list:'
    call print_list( list )

    !
    ! Now remove a part of the list via the splice method
    !

    write(*,*) 'splicing ...'
    call list%splice( 2, 4 )

    !
    ! Print the resulting list
    !
    write(*,*) 'New list:'
    call print_list( list )

contains
include 'linked_list_aux.inc'

end program example_splice
