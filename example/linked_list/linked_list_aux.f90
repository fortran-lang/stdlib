! linked_list_aux.f90 --
!     Auxiliary module for printing the contents of a linked list
!
module linked_list_aux
    use stdlib_linked_list

    implicit none

contains
subroutine print_list( list )
    type(linked_list), intent(in) :: list

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

end module linked_list_aux

