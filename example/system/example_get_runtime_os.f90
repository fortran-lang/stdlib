! Demonstrate usage of (non-cached) runtime OS query
program example_get_runtime_os
    use stdlib_system, only: OS_NAME, get_runtime_os
    implicit none

    ! Runtime OS detection (full inspection)
    print *, "Runtime OS Type: ", OS_NAME(get_runtime_os())

end program example_get_runtime_os
