! Illustrate the usage of `make_directory`
program example_make_directory
    use stdlib_system, only: make_directory
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err

    call make_directory("temp_dir", err)

    if (err%error()) then
        print *, err%print()
    else
        print *, "directory created sucessfully"
    end if

end program example_make_directory
