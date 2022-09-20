! example_clear.f90 --
!     Demonstrate the clear method
!
program example_clear
    use stdlib_linked_list

    implicit none

    type(linked_list) :: list

    !
    ! Add a few elements
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )

    !
    ! Clean up the list
    !
    call list%clear()

    !
    ! The program should print 0
    !
    write(*,*) 'Size of the list: ', list%size()

end program example_clear
