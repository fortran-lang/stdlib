! Illustrate the usage of `exists`
program example_exists
    use stdlib_system, only: exists, type_unknown, type_regular_file, &
        type_directory, type_symlink
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err

    ! Path to check
    character(*), parameter :: path = "path"
    ! To get the type of the path
    integer :: t

    t = exists(path, err)

    if (err%error()) then
        ! An error occured, print it
        print *, err%print()
    end if

    ! switching on the types returned by `exists`
    select case (t)
    case (type_unknown); print *, "Unknown type!"
    case (type_regular_file); print *, "Regular File!"
    case (type_directory); print *, "Directory!"
    case (type_symlink); print *, "Symbolic Link!"
    end select
end program example_exists

