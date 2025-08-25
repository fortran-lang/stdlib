program example_get_file_size
    use, intrinsic :: iso_fortran_env, only: int64
    use stdlib_system, only: get_file_size
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err
    integer(int64) :: size
    
    character(*), parameter :: path = "path/to/check"

    size = get_file_size(path, err)

    if (err%ok()) then
        print *, "Size in bytes = ", size
    else
        print *, "Error!: ", err%print()
    end if

end program example_get_file_size

