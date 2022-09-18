! example_absorb.f90 --
!     Demonstrate the absorb method
!
include 'linked_list_aux.f90'

program example_absorb
    use stdlib_linked_list
    use linked_list_aux

    implicit none

    type(linked_list) :: list, list_to_absorb

    !
    ! Add a few elements to the two lists
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )

    call list_to_absorb%insert( 5, 1 )
    call list_to_absorb%insert( 6, 2 )

    write(*,*) 'List 1:'
    call print_list( list )
    write(*,*) 'List 2:'
    call print_list( list_to_absorb )

    !
    ! Now absorb the second list to the first one
    !

    call list%absorb( list_to_absorb )

    !
    ! Print the resulting list
    !
    write(*,*) 'New list:'
    call print_list( list )

    !
    ! Print the second list (it is untouched)
    write(*,*) 'List that was absorbed (should be empty):'
    call print_list( list_to_absorb )

end program example_absorb
