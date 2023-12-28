! example_get.f90 --
!     Demonstrate the get method
!
program example_get
    use stdlib_linked_list

    implicit none

    type(linked_list_type) :: list
    class(*), pointer      :: list_item
    integer                :: i

    !
    ! Add a few elements
    !
    call list%insert( "String element   ", 1 ) ! Note the trailing blanks
    call list%insert( 2, 2 )
    call list%insert( 3.3, 3 )

    !
    ! Print the contents of the list
    !
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

end program example_get
