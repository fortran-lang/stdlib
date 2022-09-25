! example_concat.f90 --
!     Demonstrate the concat method
!
program example_concat
    use stdlib_linked_list

    implicit none

    type(linked_list) :: list, list_to_concat

    !
    ! Add a few elements to the two lists
    !
    call list%insert( "String element", 1 )
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )

    call list_to_concat%insert( 5, 1 )
    call list_to_concat%insert( 6, 2 )

    write(*,*) 'List 1:'
    call print_list( list )
    write(*,*) 'List 2:'
    call print_list( list_to_concat )

    !
    ! Now concat the second list to the first one
    !

    call list%concat( list_to_concat )

    !
    ! Print the resulting list
    !
    write(*,*) 'New list:'
    call print_list( list )

    !
    ! Print the second list (it is untouched)
    write(*,*) 'List that was concatenated (remains intact):'
    call print_list( list_to_concat )

contains
include 'linked_list_aux.inc'

end program example_concat
