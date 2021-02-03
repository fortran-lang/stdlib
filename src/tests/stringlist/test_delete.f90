! test_delete.f90 --
!     Test the delete routine
!
program test_deletion
    use stdlib_stringlist

    type(stringlist_type)           :: list


    call list%insert( 1, ["A", "B", "C", "D", "E", "F"] )

    call list%delete( 1, 1 )

    write(*,*) 'Expected: B, C, D, E, F (5)'
    call print_list( list )

    call list%delete( list_end, list_end )

    write(*,*) 'Expected: B, C, D, E (4)'
    call print_list( list )

    call list%delete( list_end+1, list_end+1 )

    write(*,*) 'Expected: B, C, D, E (4)'
    call print_list( list )

    call list%delete( 3, 2 )

    write(*,*) 'Expected: B, C, D, E (4)'
    call print_list( list )

    call list%delete( 2, 3 )

    write(*,*) 'Expected: B, E (2)'
    call print_list( list )

contains
subroutine renew_list( list )
    type(stringlist_type), intent(inout) :: list

    call list%destroy
    call list%insert( 1, "A" )
    call list%insert( 2, "B" )
    call list%insert( 3, "C" )
end subroutine renew_list

subroutine print_list( list )
    type(stringlist_type), intent(in) :: list

    write(*,*) list%length()

    do i = 1,list%length()
        write(*,*) '>', list%get(i), '<'
    enddo
end subroutine print_list

end program test_deletion
