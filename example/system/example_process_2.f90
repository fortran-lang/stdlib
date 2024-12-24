! Process example 2: Run an Asynchronous Command and check its status
program run_async
    use stdlib_system, only: process_type, run, is_running, wait
    implicit none

    type(process_type) :: p

    ! Run an asynchronous process to sleep for 5 seconds
    p = run("sleep 3", wait=.false.)

    ! Check if the process is running
    if (is_running(p)) then
        print *, "Process is running."
    else
        print *, "Process has already completed."
    end if

    ! Wait for the process to complete
    call wait(p)
    print *, "Process has now completed."
end program run_async
