! Test the matrix exponential.
module test_linalg_expm
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_constants
    use stdlib_linalg_constants
    use stdlib_linalg, only: expm, eye, norm, matrix_exp
    use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR

    implicit none (type,external)
    
    public :: test_expm_computation

    contains

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

    !> Exponent of matrix tests
    subroutine test_expm_computation(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        call add_test(tests,new_unittest("expm",test_expm))
        call add_test(tests,new_unittest("error_handling_expm",test_error_handling_expm))

    end subroutine test_expm_computation

    !> Matrix exponential with analytic expression.
    subroutine test_expm(error)
        type(error_type), allocatable, intent(out) :: error
        ! Problem dimension.
        integer(ilp), parameter :: n = 5, m = 6
        ! Test matrix.
        integer(ilp) :: i, j

        block
            real(sp) :: A(n, n), E(n, n), Eref(n, n)
            real(sp) :: err
            
            ! Initialize matrix.
            A = zero_sp
            do i = 1, n-1
                A(i, i+1) = m*one_sp
            enddo

            ! Reference with analytical exponential
            Eref = eye(n, mold=one_sp)
            do i = 1, n-1
                do j = 1, n-i
                    Eref(i, i+j) = Eref(i, i+j-1)*m/j
                enddo
            enddo

            ! Compute matrix exponential.
            E = expm(A)

            ! Check result.
            err = norm(Eref - E, "inf")
            print *, err , (n**2)*epsilon(1.0_sp)
            call check(error, err < (n**2)*epsilon(1.0_sp), "Analytical matrix exponential.")
            if (allocated(error)) return
        end block     
        block
            real(dp) :: A(n, n), E(n, n), Eref(n, n)
            real(dp) :: err
            
            ! Initialize matrix.
            A = zero_dp
            do i = 1, n-1
                A(i, i+1) = m*one_dp
            enddo

            ! Reference with analytical exponential
            Eref = eye(n, mold=one_dp)
            do i = 1, n-1
                do j = 1, n-i
                    Eref(i, i+j) = Eref(i, i+j-1)*m/j
                enddo
            enddo

            ! Compute matrix exponential.
            E = expm(A)

            ! Check result.
            err = norm(Eref - E, "inf")
            print *, err , (n**2)*epsilon(1.0_dp)
            call check(error, err < (n**2)*epsilon(1.0_dp), "Analytical matrix exponential.")
            if (allocated(error)) return
        end block     
        block
            complex(sp) :: A(n, n), E(n, n), Eref(n, n)
            real(sp) :: err
            
            ! Initialize matrix.
            A = zero_sp
            do i = 1, n-1
                A(i, i+1) = m*one_sp
            enddo

            ! Reference with analytical exponential
            Eref = eye(n, mold=one_sp)
            do i = 1, n-1
                do j = 1, n-i
                    Eref(i, i+j) = Eref(i, i+j-1)*m/j
                enddo
            enddo

            ! Compute matrix exponential.
            E = expm(A)

            ! Check result.
            err = norm(Eref - E, "inf")
            print *, err , (n**2)*epsilon(1.0_sp)
            call check(error, err < (n**2)*epsilon(1.0_sp), "Analytical matrix exponential.")
            if (allocated(error)) return
        end block     
        block
            complex(dp) :: A(n, n), E(n, n), Eref(n, n)
            real(dp) :: err
            
            ! Initialize matrix.
            A = zero_dp
            do i = 1, n-1
                A(i, i+1) = m*one_dp
            enddo

            ! Reference with analytical exponential
            Eref = eye(n, mold=one_dp)
            do i = 1, n-1
                do j = 1, n-i
                    Eref(i, i+j) = Eref(i, i+j-1)*m/j
                enddo
            enddo

            ! Compute matrix exponential.
            E = expm(A)

            ! Check result.
            err = norm(Eref - E, "inf")
            print *, err , (n**2)*epsilon(1.0_dp)
            call check(error, err < (n**2)*epsilon(1.0_dp), "Analytical matrix exponential.")
            if (allocated(error)) return
        end block     
    end subroutine test_expm

    !> Test error handler.
    subroutine test_error_handling_expm(error)
        type(error_type), allocatable, intent(out) :: error
        ! Problem dimension.
        integer(ilp), parameter :: n = 5, m = 6
        ! Test matrix.
        
        type(linalg_state_type) :: err
        integer(ilp) :: i

        block
            real(sp) :: A(n, n), E(n, n)
            ! Initialize matrix.
            A = zero_sp
            do i = 1, n-1
                A(i, i+1) = m*one_sp
            enddo

            ! Compute matrix exponential.
            call matrix_exp(A, E, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            call matrix_exp(A, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A, E(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return
        end block
        block
            real(dp) :: A(n, n), E(n, n)
            ! Initialize matrix.
            A = zero_dp
            do i = 1, n-1
                A(i, i+1) = m*one_dp
            enddo

            ! Compute matrix exponential.
            call matrix_exp(A, E, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            call matrix_exp(A, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A, E(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return
        end block
        block
            complex(sp) :: A(n, n), E(n, n)
            ! Initialize matrix.
            A = zero_sp
            do i = 1, n-1
                A(i, i+1) = m*one_sp
            enddo

            ! Compute matrix exponential.
            call matrix_exp(A, E, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            call matrix_exp(A, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A, E(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return
        end block
        block
            complex(dp) :: A(n, n), E(n, n)
            ! Initialize matrix.
            A = zero_dp
            do i = 1, n-1
                A(i, i+1) = m*one_dp
            enddo

            ! Compute matrix exponential.
            call matrix_exp(A, E, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            call matrix_exp(A, order=-1, err=err)
            ! Check result.
            call check(error, err%error(), "Negative Pade order")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A, E(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return

            ! Compute matrix exponential.
            call matrix_exp(A(:n, :n-1), err=err)
            ! Check result.
            call check(error, err%error(), "Invalid matrix size")
            if (allocated(error)) return
        end block
    end subroutine test_error_handling_expm

end module test_linalg_expm

program test_expm
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_expm, only : test_expm_computation
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_expm", test_expm_computation) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_expm
