! Process example 4: send a posix signal to a running process
! no-op on Windows
program example_process_send_signal
    use stdlib_system, only: process_type, runasync, is_running, send_signal, is_windows, sleep
    implicit none
    type(process_type) :: process
    logical :: running, success

    integer, parameter :: SIGTERM = 15

    if (is_windows()) then
        print *, "This is a no-op on Windows"
        stop 0
    end if

    print *, "Starting a long-running process..."
    ! choosing ping as SIGTERM causes it to exit
    process = runasync("ping -c 10 127.0.0.1")

    ! Verify the process is running
    running = is_running(process)
    print *, "Process running:", running

    ! Wait a bit before sending a signal
    call sleep(250) 

    print *, "Sending SIGTERM to the process"
    call send_signal(process, SIGTERM, success)

    if (success) then
        print *, "Signal sent successfully"
    else
        print *, "Failed to send signal SIGTERM"
    endif

    ! wait a bit to see if process is running
    !call sleep(1) 

    ! Verify the process is no longer running
    running = is_running(process)
    print *, "Process running after signal SIGTERM:", running

end program example_process_send_signal
