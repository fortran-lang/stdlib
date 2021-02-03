! test_replace_append.f90 --
!     Test the replace and append routines
!
program test_replace_append
    use stdlib_stringlist

    type(stringlist_type)           :: list, newlist

    call list%insert( 1, ["A", "B", "C", "D", "E", "F"] )

    newlist = 'Long string' // list

    write(*,*) 'Expected: "Long string, A, B, C, D, E, F (7)'
    call print_list( newlist )

    newlist = list // 'Long string'

    write(*,*) 'Expected: A, B, C, D, E, F, "Long string" (7)'
    call print_list( newlist )

    newlist = list // list

    write(*,*) 'Expected: A, B, C, D, E, F (twice, 12 elements)'
    call print_list( newlist )

    newlist = ['AA', 'BB'] // list
    write(*,*) 'Expected: AA, BB, A, B, C, D, E, F (8)'
    call print_list( newlist )

    newlist = list // ['AA', 'BB']
    write(*,*) 'Expected: A, B, C, D, E, F, AA, BB (8)'
    call print_list( newlist )

    !
    ! Replace ... quite a variety
    !
    newlist = list
    call newlist%replace( 1, "New string" )
    write(*,*) 'Expected: "New string", B, C, D, E, F (6)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( list_head, "New string" )
    write(*,*) 'Expected: "New string", B, C, D, E, F (6)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( list_end, "New string" )
    write(*,*) 'Expected: A, B, C, D, E, F, "New string" (6)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( 5, list_end, "X" )
    write(*,*) 'Expected: A, B, C, D, X (5)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( 5, list_end-2, "X" )
    write(*,*) 'Expected: A, B, C, D, E, F (6 - no change)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( 1, 2, ["WW", "XX", "YY", "ZZ"] )
    write(*,*) 'Expected: WW, XX, YY, ZZ, C, D, E, F (8)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( list_end-1, list_end, ["WW", "XX", "YY", "ZZ"] )
    write(*,*) 'Expected:  A, B, C, D, WW, XX, YY, ZZ (8)'
    call print_list( newlist )

    newlist = list
    call newlist%replace( list_end-1, list_end, list )
    write(*,*) 'Expected:  A, B, C, D, A, B, C, D, E, F (10)'
    call print_list( newlist )

contains
subroutine print_list( list )
    type(stringlist_type), intent(in) :: list

    write(*,*) list%length()

    do i = 1,list%length()
        write(*,*) '>', list%get(i), '<'
    enddo
end subroutine print_list

end program test_replace_append
