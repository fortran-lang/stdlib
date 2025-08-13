! Illustrate the usage of `abs_path`, `is_abs_path`
program example_path_abs
    use stdlib_system, only: abs_path, is_abs_path
    use stdlib_error, only: state_type
    implicit none

    character(*), parameter :: path = "path/to/check"
    character(:), allocatable :: absolute_path
    type(state_type) :: err

    if (is_abs_path(path)) then
        print *, "Path is absolute!"
        ! terminate the program since path is already absolute
        stop
    else
        print *, "Path is not absolute!"
    end if

    ! get the absolute path
    absolute_path = abs_path(path, err)

    if (err%error()) then
        ! there was an error! print it
        print *, "error converting to absolute path: " // err%print()
    else
        print *, "absolute path => " // absolute_path
    end if
end program example_path_abs

