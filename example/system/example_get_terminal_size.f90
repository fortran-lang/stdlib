program example_get_terminal_size

    use stdlib_system, only: get_terminal_size
    use stdlib_error, only: state_type
    implicit none

    integer :: columns, lines
    type(state_type) :: err

    !> Get terminal size
    call get_terminal_size(columns, lines, err)

    print "(2(a,i0))", "Terminal size is ", columns, 'x', lines
    if (err%ok()) print "(a)", repeat("*", columns)

end program example_get_terminal_size
