! Demonstrate OS detection
program example_os_type
    use stdlib_system, only: OS_TYPE, OS_NAME
    implicit none

    integer :: current_os

    ! Cached OS detection
    current_os = OS_TYPE() 
    print *, "Current OS Type: ", OS_NAME(current_os) 

end program example_os_type
