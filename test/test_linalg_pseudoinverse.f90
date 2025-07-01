! Test Moore-Penrose pseudo matrix inverse
module test_linalg_pseudoinverse
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg
    use stdlib_linalg_constants

    implicit none (type,external)
    private
    
    public :: test_pseudoinverse_matrix

    contains

    !> Matrix pseudo-inversion tests
    subroutine test_pseudoinverse_matrix(tests)
        !> Collertion of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))

        call add_test(tests,new_unittest("s_eye_pseudoinverse",test_s_eye_pseudoinverse))       
        call add_test(tests,new_unittest("d_eye_pseudoinverse",test_d_eye_pseudoinverse))       
        call add_test(tests,new_unittest("s_square_pseudoinverse",test_s_square_pseudoinverse))
        call add_test(tests,new_unittest("s_tall_pseudoinverse",test_s_tall_pseudoinverse))
        call add_test(tests,new_unittest("s_wide_pseudoinverse",test_s_wide_pseudoinverse))
        call add_test(tests,new_unittest("s_singular_pseudoinverse",test_s_singular_pseudoinverse))
        call add_test(tests,new_unittest("d_square_pseudoinverse",test_d_square_pseudoinverse))
        call add_test(tests,new_unittest("d_tall_pseudoinverse",test_d_tall_pseudoinverse))
        call add_test(tests,new_unittest("d_wide_pseudoinverse",test_d_wide_pseudoinverse))
        call add_test(tests,new_unittest("d_singular_pseudoinverse",test_d_singular_pseudoinverse))
        call add_test(tests,new_unittest("c_square_pseudoinverse",test_c_square_pseudoinverse))
        call add_test(tests,new_unittest("c_tall_pseudoinverse",test_c_tall_pseudoinverse))
        call add_test(tests,new_unittest("c_wide_pseudoinverse",test_c_wide_pseudoinverse))
        call add_test(tests,new_unittest("c_singular_pseudoinverse",test_c_singular_pseudoinverse))
        call add_test(tests,new_unittest("z_square_pseudoinverse",test_z_square_pseudoinverse))
        call add_test(tests,new_unittest("z_tall_pseudoinverse",test_z_tall_pseudoinverse))
        call add_test(tests,new_unittest("z_wide_pseudoinverse",test_z_wide_pseudoinverse))
        call add_test(tests,new_unittest("z_singular_pseudoinverse",test_z_singular_pseudoinverse))

    end subroutine test_pseudoinverse_matrix

    !> Invert identity matrix
    subroutine test_s_eye_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i,j
        integer(ilp), parameter :: n = 15_ilp
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))

        real(sp) :: a(n,n),inva(n,n)

        do concurrent (i=1:n,j=1:n)
          a(i,j) = merge(1.0_sp,0.0_sp,i==j)
        end do

        !> Invert funrtion
        inva = pinv(a,err=state)
        
        call check(error,state%ok(),'s pseudoinverse (eye, function): '//state%print())
        if (allocated(error)) return        
        call check(error,all(abs(a-inva)<tol),'s pseudoinverse (eye, function): data convergence')
        if (allocated(error)) return          
        
        !> Inverse subroutine
        call pseudoinvert(a,inva,err=state)
        
        call check(error,state%ok(),'s pseudoinverse (eye, subroutine): '//state%print())
        if (allocated(error)) return        
        call check(error,all(abs(a-inva)<tol),'s pseudoinverse (eye, subroutine): data convergence')
        if (allocated(error)) return                  
        
        !> Operator 
        inva = .pinv.a
        
        call check(error,all(abs(a-inva)<tol),'s pseudoinverse (eye, operator): data convergence')
        if (allocated(error)) return                  

    end subroutine test_s_eye_pseudoinverse

    subroutine test_d_eye_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i,j
        integer(ilp), parameter :: n = 15_ilp
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))

        real(dp) :: a(n,n),inva(n,n)

        do concurrent (i=1:n,j=1:n)
          a(i,j) = merge(1.0_dp,0.0_dp,i==j)
        end do

        !> Invert funrtion
        inva = pinv(a,err=state)
        
        call check(error,state%ok(),'d pseudoinverse (eye, function): '//state%print())
        if (allocated(error)) return        
        call check(error,all(abs(a-inva)<tol),'d pseudoinverse (eye, function): data convergence')
        if (allocated(error)) return          
        
        !> Inverse subroutine
        call pseudoinvert(a,inva,err=state)
        
        call check(error,state%ok(),'d pseudoinverse (eye, subroutine): '//state%print())
        if (allocated(error)) return        
        call check(error,all(abs(a-inva)<tol),'d pseudoinverse (eye, subroutine): data convergence')
        if (allocated(error)) return                  
        
        !> Operator 
        inva = .pinv.a
        
        call check(error,all(abs(a-inva)<tol),'d pseudoinverse (eye, operator): data convergence')
        if (allocated(error)) return                  

    end subroutine test_d_eye_pseudoinverse



    !> Test edge case: square matrix
    subroutine test_s_square_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n, n), inva(n, n)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'s pseudoinverse (square): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'s pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'s pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               

    end subroutine test_s_square_pseudoinverse

    !> Test edge case: tall matrix
    subroutine test_s_tall_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 20, n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        real(sp) :: a(m, n), inva(n, m)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'s pseudoinverse (tall): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'s pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'s pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_s_tall_pseudoinverse

    !> Test edge case: wide matrix
    subroutine test_s_wide_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 10, n = 20
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        real(sp) :: a(m, n), inva(n, m)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'s pseudoinverse (wide): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'s pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'s pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_s_wide_pseudoinverse

    !> Test edge case: singular matrix
    subroutine test_s_singular_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n, n), inva(n, n)
        
        call random_number(a)
        
        ! Make the matrix singular
        a(:, 1) = a(:, 2)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'s pseudoinverse (singular): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'s pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'s pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_s_singular_pseudoinverse


    !> Test edge case: square matrix
    subroutine test_d_square_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n, n), inva(n, n)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'d pseudoinverse (square): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'d pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'d pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               

    end subroutine test_d_square_pseudoinverse

    !> Test edge case: tall matrix
    subroutine test_d_tall_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 20, n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        real(dp) :: a(m, n), inva(n, m)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'d pseudoinverse (tall): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'d pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'d pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_d_tall_pseudoinverse

    !> Test edge case: wide matrix
    subroutine test_d_wide_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 10, n = 20
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        real(dp) :: a(m, n), inva(n, m)
        
        call random_number(a)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'d pseudoinverse (wide): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'d pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'d pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_d_wide_pseudoinverse

    !> Test edge case: singular matrix
    subroutine test_d_singular_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n, n), inva(n, n)
        
        call random_number(a)
        
        ! Make the matrix singular
        a(:, 1) = a(:, 2)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'d pseudoinverse (singular): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'d pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'d pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_d_singular_pseudoinverse


    !> Test edge case: square matrix
    subroutine test_c_square_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n, n), inva(n, n)
        real(sp) :: rea(n, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=sp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'c pseudoinverse (square): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'c pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'c pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               

    end subroutine test_c_square_pseudoinverse

    !> Test edge case: tall matrix
    subroutine test_c_tall_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 20, n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(m, n), inva(n, m)
        real(sp) :: rea(m, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=sp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'c pseudoinverse (tall): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'c pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'c pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_c_tall_pseudoinverse

    !> Test edge case: wide matrix
    subroutine test_c_wide_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 10, n = 20
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(m, n), inva(n, m)
        real(sp) :: rea(m, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=sp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'c pseudoinverse (wide): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'c pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'c pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_c_wide_pseudoinverse

    !> Test edge case: singular matrix
    subroutine test_c_singular_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(sp), parameter :: tol = 1000*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n, n), inva(n, n)
        real(sp) :: rea(n, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=sp)
        
        ! Make the matrix singular
        a(:, 1) = a(:, 2)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'c pseudoinverse (singular): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'c pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'c pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_c_singular_pseudoinverse


    !> Test edge case: square matrix
    subroutine test_z_square_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n, n), inva(n, n)
        real(dp) :: rea(n, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=dp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'z pseudoinverse (square): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'z pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'z pseudoinverse (square, convergence): '//state%print())
        if (allocated(error)) return               

    end subroutine test_z_square_pseudoinverse

    !> Test edge case: tall matrix
    subroutine test_z_tall_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 20, n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(m, n), inva(n, m)
        real(dp) :: rea(m, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=dp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'z pseudoinverse (tall): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'z pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'z pseudoinverse (tall, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_z_tall_pseudoinverse

    !> Test edge case: wide matrix
    subroutine test_z_wide_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: m = 10, n = 20
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(m, n), inva(n, m)
        real(dp) :: rea(m, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=dp)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'z pseudoinverse (wide): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'z pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'z pseudoinverse (wide, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_z_wide_pseudoinverse

    !> Test edge case: singular matrix
    subroutine test_z_singular_pseudoinverse(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: failed
        integer(ilp), parameter :: n = 10
        real(dp), parameter :: tol = 1000*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n, n), inva(n, n)
        real(dp) :: rea(n, n, 2)
        
        call random_number(rea)
        a = cmplx(rea(:, :, 1), rea(:, :, 2), kind=dp)
        
        ! Make the matrix singular
        a(:, 1) = a(:, 2)
        
        inva = pinv(a, err=state)
        call check(error,state%ok(),'z pseudoinverse (singular): '//state%print())
        if (allocated(error)) return       
        
        failed = count(abs(a - matmul(a, matmul(inva, a))) > tol)
        call check(error,failed==0,'z pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return               
        
        failed = count(abs(inva - matmul(inva, matmul(a, inva))) > tol)
        call check(error,failed==0,'z pseudoinverse (singular, convergence): '//state%print())
        if (allocated(error)) return  

    end subroutine test_z_singular_pseudoinverse


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

end module test_linalg_pseudoinverse

program test_inv
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_pseudoinverse, only : test_pseudoinverse_matrix
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_pseudoinverse", test_pseudoinverse_matrix) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_inv

