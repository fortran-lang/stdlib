! example_size.f90 --
!     Demonstrate the size method
!
program example_size
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
    ! The program should print 3
    !
    write(*,*) 'Size of the list: ', list%size()

end program example_size
