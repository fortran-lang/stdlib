! test_insert.f90 --
!     Test the insertion routine
!
program test_insertion
    use stdlib_stringlist, only: stringlist_type, fidx, bidx, stringlist_index_type, &
    & list_head, list_tail
    use stdlib_string_type, only: string_type, char

    type(stringlist_type)           :: list, second_list
    character(len=10), dimension(3) :: sarray


    call list%insert_at( fidx(1), "C" )
    call list%insert_at( fidx(1), "B" )
    call list%insert_at( fidx(1), "A" )

    write(*,*) 'Expected: A, B, C (3)'
    call print_list( list )

    call list%insert_at( fidx(1), "D" )

    write(*,*) 'Expected: A, B, C, D (4)'
    call print_list( list )

    call list%insert_at( bidx(1), "X" )

    write(*,*) 'Expected: X, A, B, C, D (5)'
    call print_list( list )

    call list%insert_at( bidx(2), "Y" )

    write(*,*) 'Expected: X, A, B, Y, C, D (6)'
    call print_list( list )

    call list%insert_at( list_tail, "Z" )

    write(*,*) 'Expected: X, A, B, Y, C, D, Z (7)'
    call print_list( list )

    !
    ! Try inserting a second list
    !
    call renew_list( list )

    call second_list%insert_at( fidx(1), "SecondA" )
    call second_list%insert_at( fidx(2), "SecondB" )

    call list%insert_at( fidx(2), second_list )
    call print_list( list )

    call renew_list( list )

    call list%insert_at( list_tail, second_list )
    call print_list( list )

    !
    ! Try inserting an array
    !
    call renew_list( list )

    sarray(1) = "ThirdA"
    sarray(2) = "ThirdB"
    sarray(3) = "ThirdC"

    call list%insert_at( list_head, sarray )
    call print_list( list )

    call renew_list( list )

    call list%insert_at( fidx(2), sarray )
    call print_list( list )

contains
subroutine renew_list( list )
    type(stringlist_type), intent(inout) :: list

    call list%destroy()
    call list%insert_at( fidx(1), "A" )
    call list%insert_at( fidx(2), "B" )
    call list%insert_at( fidx(3), "C" )
end subroutine renew_list

subroutine print_list( list )
    type(stringlist_type), intent(in) :: list
    integer                           :: i

    write(*,*) list%len()

    do i = 1, list%len()
        write(*,*) '>', char( list%get( fidx(i) ) ), '<'
    enddo
end subroutine print_list

end program test_insertion
