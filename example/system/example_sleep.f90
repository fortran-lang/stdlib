! Usage of `sleep`
program example_sleep
    use stdlib_system, only: sleep
    implicit none

    print *, "Starting sleep..."

    ! Sleep for 500 milliseconds
    call sleep(500)

    print *, "Finished sleeping!"
    
end program example_sleep    
