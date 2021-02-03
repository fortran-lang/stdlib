! test_find.f90 --
!     Test the various retrieval routines
!
program test_find
    use stdlib_stringlist

    type(stringlist_type)           :: list, sublist
    character(len=:), allocatable   :: string

    call list%insert( 1, ["A", "B", "C", "D", "E", "F"] )

    write(*,*) 'Expected: A'
    write(*,*) list%get(1)
    write(*,*) list%get(list_head)
    write(*,*) 'Expected: B'
    write(*,*) list%get(list_head+1)
    write(*,*) 'Expected: F'
    write(*,*) list%get(list_end)
    write(*,*) 'Expected: (nothing)'
    write(*,*) list%get(list_end+1)

    call list%destroy
    call list%insert( 1, ["AA", "BA", "CA", "AA", "BA", "CA"] )
    write(*,*) 'Expected: 1'
    write(*,*) list%index("AA")
    write(*,*) 'Expected: 4'
    write(*,*) list%index("AA", .true.)
    write(*,*) 'Expected: 0'
    write(*,*) list%index("XXXX")

    write(*,*) 'Expected: 2'
    write(*,*) list%index_sub("B")
    write(*,*) 'Expected: 5'
    write(*,*) list%index_sub("B", .true.)
    write(*,*) 'Expected: 0'
    write(*,*) list%index_sub("X")

    write(*,*) 'Expected: 6', list%length()

    sublist = list%range(1, 2)
    write(*,*) 'Expected: AA, BA'
    call print_list( sublist )

    sublist = list%range(list_end-1, list_end+2)
    write(*,*) 'Expected: BA, CA'
    call print_list( sublist )

    sublist = list%range(-1, 3)
    write(*,*) 'Expected: AA, BA, CA'
    call print_list( sublist )

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

end program test_find
