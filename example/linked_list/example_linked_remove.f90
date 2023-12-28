! example_remove.f90 --
!     Demonstrate the remove method
!
program example_remove
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
    ! Now remove the second element
    !

    call list%remove( 2 )

    !
    ! Print the list
    !
    write(*,*) 'New list:'
    call print_list( list )

contains
!include 'linked_list_aux.inc'
subroutine print_list( list )
    type(linked_list_type), intent(in) :: list

    integer           :: i
    class(*), pointer :: list_item

    do i = 1,list%size()
        list_item => list%get(i)

        select type( item => list_item )
            type is (integer)
                write(*,*) i, item, ' (integer)'

            type is (real)
                write(*,*) i, item, ' (real)'

            type is (character(*))
                write(*,*) i, ' >', item, '< (string)'

            class default
                write(*,*) i, ' (type unknown)'
        end select
    enddo
end subroutine print_list

end program example_remove
