! Demonstrate usage of `fs_error`, `fs_error_code`
program example_fs_error
    use stdlib_system, only: fs_error, fs_error_code
    use stdlib_error, only: state_type, STDLIB_FS_ERROR
    implicit none

    type(state_type) :: err0, err1

    err0 = fs_error("Could not create directory", "`temp.dir`", "- Already exists")

    if (err0%state == STDLIB_FS_ERROR) then
        ! Error encountered: Filesystem Error: Could not create directory `temp.dir` - Already exists
        print *, err0%print() 
    end if

    err1 = fs_error_code(1, "Could not create directory", "`temp.dir`", "- Already exists")

    if (err1%state == STDLIB_FS_ERROR) then
        ! Error encountered: Filesystem Error: code - 1, Could not create directory `temp.dir` - Already exists
        print *, err1%print()
    end if

end program example_fs_error
