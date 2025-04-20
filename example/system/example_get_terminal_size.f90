program example_get_terminal_size

    use stdlib_system, only: get_terminal_size
    implicit none

    integer :: columns, lines

    !> Get terminal size
    call get_terminal_size(columns, lines)

    print "(2(a,i0))", "Terminal size is ", columns, 'x', lines
    print "(a)", repeat("*", columns)

end program example_get_terminal_size
