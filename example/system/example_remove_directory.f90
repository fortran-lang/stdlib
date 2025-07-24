! Illustrate the usage of `remove_directory`
program example_remove_directory
    use stdlib_system, only: remove_directory
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err

    call remove_directory("directory_to_be_removed", err)

    if (err%error()) then
        print *, err%print()
    else
        print *, "directory removed successfully"
    end if

end program example_remove_directory
