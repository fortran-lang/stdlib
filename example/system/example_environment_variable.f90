! Illustrate the usage of `set_environment_variable`, `delete_environment_variable`
program example_environment_variable
    use stdlib_system, only: set_environment_variable, delete_environment_variable
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err
    character(len=32) :: value
    integer :: length, stat

    call set_environment_variable("STDLIB_EXAMPLE", "hello", err=err)
    if (err%error()) then
        print *, "Error setting the variable: "//err%print()
        stop 1
    end if

    call get_environment_variable("STDLIB_EXAMPLE", value, length, stat)
    print *, "STDLIB_EXAMPLE is "//value(:length)

    ! `overwrite` is `.true.` by default. Asking for `.false.` keeps whatever
    ! value the variable already has, and is not an error.
    call set_environment_variable("STDLIB_EXAMPLE", "goodbye", overwrite=.false., err=err)
    call get_environment_variable("STDLIB_EXAMPLE", value, length, stat)
    print *, "after a refused overwrite it is still "//value(:length)

    call delete_environment_variable("STDLIB_EXAMPLE", err=err)
    if (err%error()) then
        print *, "Error deleting the variable: "//err%print()
        stop 1
    end if

    call get_environment_variable("STDLIB_EXAMPLE", value, length, stat)
    print *, "after deleting it, the lookup status is", stat
end program example_environment_variable
