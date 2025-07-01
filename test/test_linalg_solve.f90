! Test linear system solver
module test_linalg_solve
    use stdlib_linalg_constants
    use stdlib_linalg_state
    use stdlib_linalg, only: solve
    use testdrive, only: error_type, check, new_unittest, unittest_type    

    implicit none (type,external)
    private
    
    public :: test_linear_systems

    contains

    !> Solve real and complex linear systems
    subroutine test_linear_systems(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)

        allocate(tests(0))

        call add_test(tests,new_unittest("solve_s",test_s_solve))
        call add_test(tests,new_unittest("solve_s_multiple",test_s_solve_multiple))
        call add_test(tests,new_unittest("solve_d",test_d_solve))
        call add_test(tests,new_unittest("solve_d_multiple",test_d_solve_multiple))

        call add_test(tests,new_unittest("solve_complex_c",test_c_solve))
        call add_test(tests,new_unittest("solve_2x2_complex_c",test_2x2_c_solve))
        call add_test(tests,new_unittest("solve_complex_z",test_z_solve))
        call add_test(tests,new_unittest("solve_2x2_complex_z",test_2x2_z_solve))

    end subroutine test_linear_systems    
    
    !> Simple linear system
    subroutine test_s_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        real(sp) :: A(3,3) = transpose(reshape([real(sp) :: 1, 3, 3, &
                                                            1, 3, 4, &
                                                            1, 4, 3], [3,3]))
        real(sp) :: b  (3) = [real(sp) :: 1, 4, -1]
        real(sp) :: res(3) = [real(sp) :: -2, -2, 3]
        real(sp) :: x(3)

        x = solve(a,b,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(x-res)<abs(res*epsilon(0.0_sp))), 'results match expected')
        if (allocated(error)) return

    end subroutine test_s_solve

    !> Simple linear system with multiple right hand sides
    subroutine test_s_solve_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        real(sp) :: A(3,3) = transpose(reshape([real(sp) :: 1,-1, 2, &
                                                            0, 1, 1, &
                                                            1,-1, 3], [3,3]))
        real(sp) :: b(3,3) = transpose(reshape([real(sp) :: 0, 1, 2, &
                                                            1,-2,-1, &
                                                            2, 3,-1], [3,3]))
        real(sp) :: res(3,3) = transpose(reshape([real(sp) ::-5,-7,10, &
                                                           -1,-4, 2, &
                                                            2, 2,-3], [3,3]))
        real(sp) :: x(3,3)

        x = solve(a,b,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res*epsilon(0.0_sp))), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_s_solve_multiple
    !> Simple linear system
    subroutine test_d_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        real(dp) :: A(3,3) = transpose(reshape([real(dp) :: 1, 3, 3, &
                                                            1, 3, 4, &
                                                            1, 4, 3], [3,3]))
        real(dp) :: b  (3) = [real(dp) :: 1, 4, -1]
        real(dp) :: res(3) = [real(dp) :: -2, -2, 3]
        real(dp) :: x(3)

        x = solve(a,b,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(x-res)<abs(res*epsilon(0.0_dp))), 'results match expected')
        if (allocated(error)) return

    end subroutine test_d_solve

    !> Simple linear system with multiple right hand sides
    subroutine test_d_solve_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        real(dp) :: A(3,3) = transpose(reshape([real(dp) :: 1,-1, 2, &
                                                            0, 1, 1, &
                                                            1,-1, 3], [3,3]))
        real(dp) :: b(3,3) = transpose(reshape([real(dp) :: 0, 1, 2, &
                                                            1,-2,-1, &
                                                            2, 3,-1], [3,3]))
        real(dp) :: res(3,3) = transpose(reshape([real(dp) ::-5,-7,10, &
                                                           -1,-4, 2, &
                                                            2, 2,-3], [3,3]))
        real(dp) :: x(3,3)

        x = solve(a,b,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res*epsilon(0.0_dp))), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_d_solve_multiple

    !> Complex linear system
    !> Militaru, Popa, "On the numerical solving of complex linear systems",
    !> Int J Pure Appl Math 76(1), 113-122, 2012.
    subroutine test_c_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        complex(sp) :: A(5,5), b(5), res(5), x(5)

        ! Fill in linear system
        A = (0.0_sp,0.0_sp)

        A(1:2,1) = [(19.73_sp,0.0_sp),(0.0_sp,-0.51_sp)]
        A(1:3,2) = [(12.11_sp,-1.0_sp),(32.3_sp,7.0_sp),(0.0_sp,-0.51_sp)]
        A(1:4,3) = [(0.0_sp,5.0_sp),(23.07_sp,0.0_sp),(70.0_sp,7.3_sp),(1.0_sp,1.1_sp)]
        A(2:5,4) = [(0.0_sp,1.0_sp),(3.95_sp,0.0_sp),(50.17_sp,0.0_sp),(0.0_sp,-9.351_sp)]
        A(3:5,5) = [(19.0_sp,31.83_sp),(45.51_sp,0.0_sp),(55.0_sp,0.0_sp)]

        b = [(77.38_sp,8.82_sp),(157.48_sp,19.8_sp),(1175.62_sp,20.69_sp),(912.12_sp,-801.75_sp),(550.0_sp,-1060.4_sp)]

        ! Exact result
        res = [(3.3_sp,-1.0_sp),(1.0_sp,0.17_sp),(5.5_sp,0.0_sp),(9.0_sp,0.0_sp),(10.0_sp,-17.75_sp)]

        x = solve(a,b,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res)*1.0e-3_sp), 'results match expected')
        if (allocated(error)) return        

    end subroutine test_c_solve

    !> 2x2 Complex linear system
    subroutine test_2x2_c_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        
        complex(sp), parameter :: i = (0.0_sp,1.0_sp)

        complex(sp) :: A(2,2), b(2), res(2), x(2)

        ! Fill in linear system
        A(1,:) = [ 1+2*i, 2-i]
        A(2,:) = [ 2+i  , i]

        b      = [1,-1]

        ! Exact result
        res = [(-0.28_sp,-0.04_sp),(0.36_sp,0.48_sp)]

        x = solve(a,b,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res)*sqrt(epsilon(0.0_sp))), 'results match expected')
        if (allocated(error)) return        


    end subroutine test_2x2_c_solve
    !> Complex linear system
    !> Militaru, Popa, "On the numerical solving of complex linear systems",
    !> Int J Pure Appl Math 76(1), 113-122, 2012.
    subroutine test_z_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        complex(dp) :: A(5,5), b(5), res(5), x(5)

        ! Fill in linear system
        A = (0.0_dp,0.0_dp)

        A(1:2,1) = [(19.73_dp,0.0_dp),(0.0_dp,-0.51_dp)]
        A(1:3,2) = [(12.11_dp,-1.0_dp),(32.3_dp,7.0_dp),(0.0_dp,-0.51_dp)]
        A(1:4,3) = [(0.0_dp,5.0_dp),(23.07_dp,0.0_dp),(70.0_dp,7.3_dp),(1.0_dp,1.1_dp)]
        A(2:5,4) = [(0.0_dp,1.0_dp),(3.95_dp,0.0_dp),(50.17_dp,0.0_dp),(0.0_dp,-9.351_dp)]
        A(3:5,5) = [(19.0_dp,31.83_dp),(45.51_dp,0.0_dp),(55.0_dp,0.0_dp)]

        b = [(77.38_dp,8.82_dp),(157.48_dp,19.8_dp),(1175.62_dp,20.69_dp),(912.12_dp,-801.75_dp),(550.0_dp,-1060.4_dp)]

        ! Exact result
        res = [(3.3_dp,-1.0_dp),(1.0_dp,0.17_dp),(5.5_dp,0.0_dp),(9.0_dp,0.0_dp),(10.0_dp,-17.75_dp)]

        x = solve(a,b,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res)*1.0e-3_dp), 'results match expected')
        if (allocated(error)) return        

    end subroutine test_z_solve

    !> 2x2 Complex linear system
    subroutine test_2x2_z_solve(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        
        complex(dp), parameter :: i = (0.0_dp,1.0_dp)

        complex(dp) :: A(2,2), b(2), res(2), x(2)

        ! Fill in linear system
        A(1,:) = [ 1+2*i, 2-i]
        A(2,:) = [ 2+i  , i]

        b      = [1,-1]

        ! Exact result
        res = [(-0.28_dp,-0.04_dp),(0.36_dp,0.48_dp)]

        x = solve(a,b,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-res)<abs(res)*sqrt(epsilon(0.0_dp))), 'results match expected')
        if (allocated(error)) return        


    end subroutine test_2x2_z_solve
    
    ! gcc-15 bugfix utility
    subroutine add_test(tests,new_test)
        type(unittest_type), allocatable, intent(inout) :: tests(:)    
        type(unittest_type), intent(in) :: new_test
        
        integer :: n
        type(unittest_type), allocatable :: new_tests(:)
        
        if (allocated(tests)) then 
            n = size(tests)
        else
            n = 0
        end if
        
        allocate(new_tests(n+1))
        if (n>0) new_tests(1:n) = tests(1:n)
                 new_tests(1+n) = new_test
        call move_alloc(from=new_tests,to=tests)        
    
    end subroutine add_test
    
end module test_linalg_solve

program test_solve
     use, intrinsic :: iso_fortran_env, only : error_unit
     use testdrive, only : run_testsuite, new_testsuite, testsuite_type
     use test_linalg_solve, only : test_linear_systems
     implicit none
     integer :: stat, is
     type(testsuite_type), allocatable :: testsuites(:)
     character(len=*), parameter :: fmt = '("#", *(1x, a))'

     stat = 0

     testsuites = [ &
         new_testsuite("linalg_solve", test_linear_systems) &
         ]

     do is = 1, size(testsuites)
         write(error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
     end do

     if (stat > 0) then
         write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
         error stop
     end if
end program test_solve

