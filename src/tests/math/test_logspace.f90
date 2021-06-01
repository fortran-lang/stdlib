program test_logspace

    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int8, int16, int32, int64
    use stdlib_math, only: logspace 
    
    implicit none
    
        logical :: warn = .false. 
    
        ! Testing logspace
        !
        ! logspace should return a rank 1 array of values equally logarithmically spaced 
        ! from the base**start to base**end, using Euler's number e as the base. If no length
        ! is specified, return a rank 1 array with 50 elements.
    
        open(unit=9, file="test_logspace_log.txt", status="unknown") ! Log the results of the function
    
        call test_logspace_sp
        call test_logspace_dp
        call test_logspace_default
        call test_logspace_base_2
    
        close(unit=9)    
    
    contains
    
        subroutine test_logspace_sp
    
            integer :: n = 20
            real(sp) :: start = 0.0_sp
            real(sp) :: end = 2.0_sp
            
            real(sp), allocatable :: x(:)
    
            logical :: cond_1, cond_2
    
            x = logspace(start, end, n)
    
    
            ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
            ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)
    
            ! call global_logger%add_log_file("test_logspace_log_file.txt", unit=unit)
            ! call global_logger%log_message("x = logspace(0.0_sp, 2.0_sp, 20)")
            ! 99 format(G11.5, X)
    
            write(unit=9, fmt=*) "logspace(0.0_sp, 2.0_sp, 20): "
            write(unit=9,fmt=99)
            write(unit=9,fmt="(20(F7.3, 2X))") x
            write(9,*)
            write(9,*)
                  
            99 format(70("="))
    
        end subroutine 
    
        subroutine test_logspace_dp
    
            integer :: n = 10
            real(dp) :: start = 1.0_dp
            real(dp) :: end = 0.0_dp
            
            real(dp), allocatable :: x(:)
    
            logical :: cond_1, cond_2
    
            x = logspace(start, end, n)
    
    
            ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
            ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)
    
            ! call global_logger%add_log_file("test_logspace_log_file.txt", unit=unit)
            ! call global_logger%log_message("x = logspace(0.0_sp, 2.0_sp, 20)")
            ! 99 format(G11.5, X)
    
            write(unit=9, fmt=*) "logspace(1.0_dp, 0.0_dp, 10): "
            write(unit=9,fmt=99)
            write(unit=9,fmt="(10(F7.3, 2X))") x
            write(9,*)
            write(9,*)
                  
            99 format(70("="))
    
        end subroutine 
    
        subroutine test_logspace_default
    
            real(dp) :: start = 0.0_dp
            real(dp) :: end = 1.0_dp
            
            real(dp), allocatable :: x(:)
        
            x = logspace(start, end)    
    
            ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
            ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
            call check(size(x) == 50, msg="Array not allocated to appropriate size", warn=warn)
    
            ! call global_logger%add_log_file("test_logspace_log_file.txt", unit=unit)
            ! call global_logger%log_message("x = logspace(0.0_sp, 2.0_sp, 20)")
            ! 99 format(G11.5, X)
    
            write(unit=9, fmt=*) "logspace(0.0_dp, 1.0_dp): "
            write(unit=9,fmt=99)
            write(unit=9,fmt="(50(F7.3, 2X))") x
            write(9,*)
            write(9,*)
                  
            99 format(70("="))
    
        end subroutine 

        subroutine test_logspace_base_2
    
            integer :: n = 10
            real(dp) :: start = 1.0_dp
            real(dp) :: end = 10.0_dp
            integer :: base = 2
            
            real(dp), allocatable :: x(:)
    
            logical :: cond_1, cond_2
    
            x = logspace(start, end, n, base)
    
    
            ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
            ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)
    
            ! call global_logger%add_log_file("test_logspace_log_file.txt", unit=unit)
            ! call global_logger%log_message("x = logspace(0.0_sp, 2.0_sp, 20)")
            ! 99 format(G11.5, X)
    
            write(unit=9, fmt=*) "logspace(1.0_dp, 10.0_dp, 10, 2): "
            write(unit=9,fmt=99)
            write(unit=9,fmt="(10(F9.3, 2X))") x
            write(9,*)
            write(9,*)
                  
            99 format(70("="))
    
        end subroutine 
    
    
    end program