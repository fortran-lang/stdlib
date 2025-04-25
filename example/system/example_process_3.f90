! Process example 3: Run with many arguments, and check runtime
program run_with_args
    use stdlib_system, only: process_type, run, elapsed, wait
    implicit none

    type(process_type) :: p
    character(len=15), allocatable :: args(:)

    ! Define arguments for the `echo` command
    allocate(args(2))
    args(1) = "echo"
    args(2) = "Hello, Fortran!"

    ! Run the command with arguments (synchronous)
    p = run(args)

    ! Print the runtime of the process
    print *, "Process runtime:", elapsed(p), "seconds."

    ! Clean up
    deallocate(args)
end program run_with_args
