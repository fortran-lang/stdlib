! Process example 1: Run a Command Synchronously and Capture Output
program run_sync
    use stdlib_system, only: run, is_completed, is_windows, process_type
    implicit none

    type(process_type) :: p
    logical :: completed

    ! Run a synchronous process to list directory contents
    if (is_windows()) then
        p = run("dir", want_stdout=.true.)
    else
        p = run("ls -l", want_stdout=.true.)
    end if

    ! Check if the process is completed (should be true since wait=.true.)
    if (is_completed(p)) then
        print *, "Process completed successfully. The current directory: "
        print *, p%stdout
    else
        print *, "Process is still running (unexpected)."
    end if
    
end program run_sync
