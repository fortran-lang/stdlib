! Demonstrate usage of `FS_ERROR`, `FS_ERROR_CODE`
program example_fs_error
    use stdlib_system, only: FS_ERROR, FS_ERROR_CODE
    use stdlib_error, only: state_type, STDLIB_FS_ERROR
    implicit none

    type(state_type) :: err0, err1

    err0 = FS_ERROR("Could not create directory", "`temp.dir`", "- Already exists")

    if (err0%state == STDLIB_FS_ERROR) then
        ! Error encountered: Filesystem Error: Could not create directory `temp.dir` - Already exists
        print *, err0%print() 
    end if

    err1 = FS_ERROR_CODE(1, "Could not create directory", "`temp.dir`", "- Already exists")

    if (err1%state == STDLIB_FS_ERROR) then
        ! Error encountered: Filesystem Error: code - 1, Could not create directory `temp.dir` - Already exists
        print *, err1%print()
    end if

end program example_fs_error
