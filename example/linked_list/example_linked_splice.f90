! example_splice.f90 --
!     Demonstrate the splice method
!

program example_splice
    use stdlib_linked_list

    implicit none

    type(linked_list_type) :: list

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

end program example_splice
