! Process example 4: Kill a running process
program example_process_kill
    use stdlib_system, only: process_type, run, is_running, kill, elapsed, has_win32, sleep
    implicit none
    type(process_type) :: process
    logical :: running, success

    print *, "Starting a long-running process..."
    if (has_win32()) then
        process = run("ping -n 10 127.0.0.1", wait=.false.)
    else
        process = run("ping -c 10 127.0.0.1", wait=.false.)
    endif

    ! Verify the process is running
    running = is_running(process)
    print *, "Process running:", running

    ! Wait a bit before killing the process
    call sleep(millisec=250) 

    print *, "Killing the process..."
    call kill(process, success)

    if (success) then
        print *, "Process killed successfully."
    else
        print *, "Failed to kill the process."
    endif

    ! Verify the process is no longer running
    running = is_running(process)
    print *, "Process running after kill:", running,' runtime=',elapsed(process)

    stop 0

end program example_process_kill
