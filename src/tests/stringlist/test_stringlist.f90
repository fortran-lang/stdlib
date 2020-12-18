! test_stringlist.f90 --
!     Test program for the stdlib_stringlist module
!
!     Straightforward test program: try to cover "all" special cases
!
program test_stringlists
    use stdlib_stringlists

    implicit none

    type(t_stringlist) :: list
    integer            :: i

    !
    ! The straightforward cases: insert a few strings and retrieve them
    !
    call list%insert( list_head, "C" )
    call list%insert( list_head, "B" )
    call list%insert( list_head, "A" )

    !
    ! Now insert a string at an arbitrary location - it will be at position 11
    !
    call list%insert( 10, "Number 10" )

    !
    ! Append a string to the end of the list - that will be position 12
    !
    call list%insert( list_end, "Appended" )

    !
    ! Insert a string near the end of the list - that will be position 12-2+1
    !
    call list%insert( list_end-2, "Appended 2" )

    !
    ! Insert a string beyond the beginning - that should appear at the start
    !
    call list%insert( list_end-30, "Effectively prepended" )

    !
    ! Print the result
    !
    do i = 1,list%length()
        write(*,*) i, '>', list%get(i), '<'
    enddo

    !
    ! And select two elements at the end
    !
    write(*,*) 'end-1' , '>', list%get(list_end-1), '<'
    write(*,*) 'end'   , '>', list%get(list_end), '<'
    write(*,*) 'end-30', '>', list%get(list_end-30), '<'

    !
    ! Okay, finally set an element really far and read it back
    !
    call list%insert( 40, "Really far away" )
    write(*,*) '40', '>', list%get(40), '<'
    write(*,*) '41', '>', list%get(41), '<'
    write(*,*) list%length()

end program test_stringlists
