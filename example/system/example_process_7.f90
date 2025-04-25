! Process example 7: Usage of `kill`
program example_process_7
    use stdlib_system, only: process_type, runasync, kill
    implicit none

    type(process_type) :: p
    logical :: success

    ! Start a process asynchronously
    p = runasync("sleep 10")

    ! Attempt to kill the process
    call kill(p, success)

    if (success) then
        print *, "Process successfully killed."
    else
        print *, "Failed to kill the process."
    end if
    
end program example_process_7    
