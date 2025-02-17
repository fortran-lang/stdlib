! Process example 5: Object-oriented interface
program example_process_5
    use stdlib_system, only: process_type, runasync, is_windows, sleep, update
    implicit none
    type(process_type) :: process

    if (is_windows()) then
        process = runasync("ping -n 10 127.0.0.1")
    else
        process = runasync("ping -c 10 127.0.0.1")
    endif

    ! Verify the process is running
    do while (process%is_running())
        
        ! Update process state
        call update(process)
        
        ! Wait a bit before killing the process
        call sleep(millisec=1500)
        
        print *, "Process has been running for ",process%elapsed()," seconds..."

    end do
    
    print *, "Process ",process%pid()," completed in ",process%elapsed()," seconds."
    
end program example_process_5
