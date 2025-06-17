program example_fs_error
    use stdlib_system, only: fs_error
    implicit none

    type(fs_error) :: err, err0

    err = fs_error(1, 'Operation not permitted') ! EPERM

    ! Print message
    print *, err%print()

    ! Check success
    print *, 'Check error: ',err%error()
    print *, 'Check flag : ',err%code /= 0

    call err%handle(err0)

    ! Print flag
    print *, err0%print()

    ! Check success
    print *, 'Check error: ',err0%error()
    print *, 'Check flag : ',err0%code /= 0

    ! call err%handle() ! stops the program

end program example_fs_error
