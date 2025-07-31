! Illustrate the usage of `get_cwd`, `set_cwd`
program example_cwd
    use stdlib_system, only: get_cwd, set_cwd
    use stdlib_error, only: state_type
    implicit none 

    character(len=:), allocatable :: path
    type(state_type) :: err

    call get_cwd(path, err)

    if (err%error()) then
        print *, "Error getting current working directory: "//err%print()
    end if
    
    print *, "CWD: "//path

    call set_cwd("./src", err)

    if (err%error()) then
        print *, "Error setting current working directory: "//err%print()
    end if

    call get_cwd(path, err)

    if (err%error()) then
        print *, "Error getting current working directory after using set_cwd: "//err%print()
    end if
    
    print *, "CWD: "//path
end program example_cwd

