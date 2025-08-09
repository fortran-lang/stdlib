! Illustrate the usage of `exists`
program example_exists
    use stdlib_system, only: exists, fs_type_unknown, fs_type_regular_file, &
        fs_type_directory, fs_type_symlink
    use stdlib_error, only: state_type
    implicit none

    type(state_type) :: err

    ! Path to check
    character(*), parameter :: path = "path/to/check"
    ! To get the type of the path
    integer :: t

    t = exists(path, err)

    if (err%error()) then
        ! An error occured, print it
        print *, err%print()
    end if

    ! switching on the type returned by `exists`
    select case (t)
    case (fs_type_unknown); print *, "Unknown type!"
    case (fs_type_regular_file); print *, "Regular File!"
    case (fs_type_directory); print *, "Directory!"
    case (fs_type_symlink); print *, "Symbolic Link!"
    end select
end program example_exists

