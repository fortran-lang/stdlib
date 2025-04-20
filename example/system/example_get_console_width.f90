program example_get_console_width

    use stdlib_system, only: get_console_width
    implicit none

    integer :: width

    !> Get console width
    width = get_console_width()

    print "(a,i0)", "Console width is ", width
    print "(a)", repeat("*", width)

end program example_get_console_width
