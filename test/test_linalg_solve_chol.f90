! Test solve_chol, solve_lower_chol, solve_upper_chol
module test_linalg_solve_chol
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg, only: cholesky, solve_chol, solve_lower_chol, solve_upper_chol
    use stdlib_linalg_state, only: linalg_state_type

    implicit none (type,external)
    private
    
    public :: test_solve_chol_factorization

    contains

    !> Cholesky solve tests
    subroutine test_solve_chol_factorization(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)

        allocate(tests(0))
        
        call add_test(tests,new_unittest("solve_lower_chol_s",test_solve_lower_chol_s))
        call add_test(tests,new_unittest("solve_upper_chol_s",test_solve_upper_chol_s))
        call add_test(tests,new_unittest("solve_chol_s",test_solve_chol_s))
        call add_test(tests,new_unittest("solve_chol_upper_s",test_solve_chol_upper_s))
        call add_test(tests,new_unittest("solve_chol_overwrite_s",test_solve_chol_overwrite_s))
        call add_test(tests,new_unittest("solve_lower_chol_multi_rhs_s",test_solve_lower_chol_multi_rhs_s))
        call add_test(tests,new_unittest("solve_chol_indefinite_s",test_solve_chol_indefinite_s))
        call add_test(tests,new_unittest("solve_chol_semidefinite_s",test_solve_chol_semidefinite_s))
        call add_test(tests,new_unittest("solve_lower_chol_d",test_solve_lower_chol_d))
        call add_test(tests,new_unittest("solve_upper_chol_d",test_solve_upper_chol_d))
        call add_test(tests,new_unittest("solve_chol_d",test_solve_chol_d))
        call add_test(tests,new_unittest("solve_chol_upper_d",test_solve_chol_upper_d))
        call add_test(tests,new_unittest("solve_chol_overwrite_d",test_solve_chol_overwrite_d))
        call add_test(tests,new_unittest("solve_lower_chol_multi_rhs_d",test_solve_lower_chol_multi_rhs_d))
        call add_test(tests,new_unittest("solve_chol_indefinite_d",test_solve_chol_indefinite_d))
        call add_test(tests,new_unittest("solve_chol_semidefinite_d",test_solve_chol_semidefinite_d))
        call add_test(tests,new_unittest("solve_lower_chol_c",test_solve_lower_chol_c))
        call add_test(tests,new_unittest("solve_upper_chol_c",test_solve_upper_chol_c))
        call add_test(tests,new_unittest("solve_chol_c",test_solve_chol_c))
        call add_test(tests,new_unittest("solve_chol_upper_c",test_solve_chol_upper_c))
        call add_test(tests,new_unittest("solve_chol_overwrite_c",test_solve_chol_overwrite_c))
        call add_test(tests,new_unittest("solve_lower_chol_multi_rhs_c",test_solve_lower_chol_multi_rhs_c))
        call add_test(tests,new_unittest("solve_chol_indefinite_c",test_solve_chol_indefinite_c))
        call add_test(tests,new_unittest("solve_chol_semidefinite_c",test_solve_chol_semidefinite_c))
        call add_test(tests,new_unittest("solve_lower_chol_z",test_solve_lower_chol_z))
        call add_test(tests,new_unittest("solve_upper_chol_z",test_solve_upper_chol_z))
        call add_test(tests,new_unittest("solve_chol_z",test_solve_chol_z))
        call add_test(tests,new_unittest("solve_chol_upper_z",test_solve_chol_upper_z))
        call add_test(tests,new_unittest("solve_chol_overwrite_z",test_solve_chol_overwrite_z))
        call add_test(tests,new_unittest("solve_lower_chol_multi_rhs_z",test_solve_lower_chol_multi_rhs_z))
        call add_test(tests,new_unittest("solve_chol_indefinite_z",test_solve_chol_indefinite_z))
        call add_test(tests,new_unittest("solve_chol_semidefinite_z",test_solve_chol_semidefinite_z))

    end subroutine test_solve_chol_factorization

    !> Test solve_lower_chol with pre-computed lower Cholesky factors
    subroutine test_solve_lower_chol_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), l(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_s
    
    subroutine test_solve_lower_chol_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), l(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_d
    
    subroutine test_solve_lower_chol_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), l(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_c
    
    subroutine test_solve_lower_chol_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), l(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_z
    

    !> Test solve_upper_chol with pre-computed upper Cholesky factors
    subroutine test_solve_upper_chol_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), u(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization (upper triangular: A = U^T * U)
        call cholesky(a, u, lower=.false., err=state)
        
        call check(error, state%ok(), 'cholesky factorization (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using upper Cholesky factors
        call solve_upper_chol(u, b, x, err=state)
        
        call check(error, state%ok(), 'solve_upper_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_upper_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_upper_chol_s
    
    subroutine test_solve_upper_chol_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), u(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization (upper triangular: A = U^T * U)
        call cholesky(a, u, lower=.false., err=state)
        
        call check(error, state%ok(), 'cholesky factorization (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using upper Cholesky factors
        call solve_upper_chol(u, b, x, err=state)
        
        call check(error, state%ok(), 'solve_upper_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_upper_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_upper_chol_d
    
    subroutine test_solve_upper_chol_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), u(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization (upper triangular: A = U^T * U)
        call cholesky(a, u, lower=.false., err=state)
        
        call check(error, state%ok(), 'cholesky factorization (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using upper Cholesky factors
        call solve_upper_chol(u, b, x, err=state)
        
        call check(error, state%ok(), 'solve_upper_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_upper_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_upper_chol_c
    
    subroutine test_solve_upper_chol_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), u(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization (upper triangular: A = U^T * U)
        call cholesky(a, u, lower=.false., err=state)
        
        call check(error, state%ok(), 'cholesky factorization (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using upper Cholesky factors
        call solve_upper_chol(u, b, x, err=state)
        
        call check(error, state%ok(), 'solve_upper_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_upper_chol: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_upper_chol_z
    

    !> Test solve_chol (one-shot) - default preserves A
    subroutine test_solve_chol_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with default overwrite_a=.false. (A should be preserved)
        call solve_chol(a, b, x, lower=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol: A should be preserved by default')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_s
    
    subroutine test_solve_chol_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with default overwrite_a=.false. (A should be preserved)
        call solve_chol(a, b, x, lower=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol: A should be preserved by default')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_d
    
    subroutine test_solve_chol_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with default overwrite_a=.false. (A should be preserved)
        call solve_chol(a, b, x, lower=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol: A should be preserved by default')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_c
    
    subroutine test_solve_chol_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with default overwrite_a=.false. (A should be preserved)
        call solve_chol(a, b, x, lower=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol: A should be preserved by default')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_z
    

    !> Test solve_chol with upper triangular (lower=.false.)
    subroutine test_solve_chol_upper_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with upper triangular (A should be preserved by default)
        call solve_chol(a, b, x, lower=.false., err=state)
        
        call check(error, state%ok(), 'solve_chol (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol (upper): solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol (upper): A should be preserved')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_upper_s
    
    subroutine test_solve_chol_upper_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with upper triangular (A should be preserved by default)
        call solve_chol(a, b, x, lower=.false., err=state)
        
        call check(error, state%ok(), 'solve_chol (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol (upper): solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol (upper): A should be preserved')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_upper_d
    
    subroutine test_solve_chol_upper_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with upper triangular (A should be preserved by default)
        call solve_chol(a, b, x, lower=.false., err=state)
        
        call check(error, state%ok(), 'solve_chol (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol (upper): solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol (upper): A should be preserved')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_upper_c
    
    subroutine test_solve_chol_upper_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with upper triangular (A should be preserved by default)
        call solve_chol(a, b, x, lower=.false., err=state)
        
        call check(error, state%ok(), 'solve_chol (upper) failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol (upper): solution mismatch')
        if (allocated(error)) return
        
        ! Check that A is preserved (default behavior)
        call check(error, all(abs(a - a_copy) < tol), 'solve_chol (upper): A should be preserved')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_upper_z
    

    !> Test solve_chol with overwrite_a=.true.
    subroutine test_solve_chol_overwrite_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with overwrite_a=.true. (A will be destroyed)
        call solve_chol(a, b, x, lower=.true., overwrite_a=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol overwrite failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol overwrite: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A was overwritten (not equal to original)
        call check(error, any(abs(a - a_copy) > tol), 'solve_chol: A should be overwritten with overwrite_a=.true.')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_overwrite_s
    
    subroutine test_solve_chol_overwrite_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with overwrite_a=.true. (A will be destroyed)
        call solve_chol(a, b, x, lower=.true., overwrite_a=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol overwrite failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol overwrite: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A was overwritten (not equal to original)
        call check(error, any(abs(a - a_copy) > tol), 'solve_chol: A should be overwritten with overwrite_a=.true.')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_overwrite_d
    
    subroutine test_solve_chol_overwrite_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with overwrite_a=.true. (A will be destroyed)
        call solve_chol(a, b, x, lower=.true., overwrite_a=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol overwrite failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol overwrite: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A was overwritten (not equal to original)
        call check(error, any(abs(a - a_copy) > tol), 'solve_chol: A should be overwritten with overwrite_a=.true.')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_overwrite_c
    
    subroutine test_solve_chol_overwrite_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), a_copy(n,n), b(n), x(n), x_expected(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        a_copy = a
        
        ! Known solution
        x_expected = [1, 2, 3]
        
        ! Compute RHS: b = A * x_expected
        b = matmul(a, x_expected)
        
        ! One-shot solve with overwrite_a=.true. (A will be destroyed)
        call solve_chol(a, b, x, lower=.true., overwrite_a=.true., err=state)
        
        call check(error, state%ok(), 'solve_chol overwrite failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_chol overwrite: solution mismatch')
        if (allocated(error)) return
        
        ! Check that A was overwritten (not equal to original)
        call check(error, any(abs(a - a_copy) > tol), 'solve_chol: A should be overwritten with overwrite_a=.true.')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_overwrite_z
    

    !> Test solve_lower_chol with multiple RHS
    subroutine test_solve_lower_chol_multi_rhs_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        integer(ilp), parameter :: nrhs = 2_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(n,n), l(n,n), b(n,nrhs), x(n,nrhs), x_expected(n,nrhs)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solutions (two RHS)
        x_expected(:,1) = [1, 2, 3]
        x_expected(:,2) = [4, 5, 6]
        
        ! Compute RHS: B = A * X_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol multi-rhs failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol multi-rhs: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_multi_rhs_s
    
    subroutine test_solve_lower_chol_multi_rhs_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        integer(ilp), parameter :: nrhs = 2_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(n,n), l(n,n), b(n,nrhs), x(n,nrhs), x_expected(n,nrhs)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solutions (two RHS)
        x_expected(:,1) = [1, 2, 3]
        x_expected(:,2) = [4, 5, 6]
        
        ! Compute RHS: B = A * X_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol multi-rhs failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol multi-rhs: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_multi_rhs_d
    
    subroutine test_solve_lower_chol_multi_rhs_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        integer(ilp), parameter :: nrhs = 2_ilp
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(n,n), l(n,n), b(n,nrhs), x(n,nrhs), x_expected(n,nrhs)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solutions (two RHS)
        x_expected(:,1) = [1, 2, 3]
        x_expected(:,2) = [4, 5, 6]
        
        ! Compute RHS: B = A * X_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol multi-rhs failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol multi-rhs: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_multi_rhs_c
    
    subroutine test_solve_lower_chol_multi_rhs_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        integer(ilp), parameter :: nrhs = 2_ilp
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(n,n), l(n,n), b(n,nrhs), x(n,nrhs), x_expected(n,nrhs)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive definite matrix
        a(1,:) = [4, 2, 2]
        a(2,:) = [2, 5, 1]
        a(3,:) = [2, 1, 6]
        
        ! Known solutions (two RHS)
        x_expected(:,1) = [1, 2, 3]
        x_expected(:,2) = [4, 5, 6]
        
        ! Compute RHS: B = A * X_expected
        b = matmul(a, x_expected)
        
        ! Compute Cholesky factorization
        call cholesky(a, l, lower=.true., err=state)
        
        call check(error, state%ok(), 'cholesky factorization failed: '//state%print())
        if (allocated(error)) return
        
        ! Solve using lower Cholesky factors
        call solve_lower_chol(l, b, x, err=state)
        
        call check(error, state%ok(), 'solve_lower_chol multi-rhs failed: '//state%print())
        if (allocated(error)) return
        
        ! Check solution
        call check(error, all(abs(x - x_expected) < tol), 'solve_lower_chol multi-rhs: solution mismatch')
        if (allocated(error)) return
        
    end subroutine test_solve_lower_chol_multi_rhs_z
    

    !> Test solve_chol with symmetric indefinite matrix (should fail)
    subroutine test_solve_chol_indefinite_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        real(sp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric INDEFINITE matrix (eigenvalues: 3, -1)
        a(1,:) = [1, 2]
        a(2,:) = [2, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for indefinite matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not positive definite)
        call check(error, state%error(), 'solve_chol should fail for indefinite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_indefinite_s
    
    subroutine test_solve_chol_indefinite_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        real(dp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric INDEFINITE matrix (eigenvalues: 3, -1)
        a(1,:) = [1, 2]
        a(2,:) = [2, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for indefinite matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not positive definite)
        call check(error, state%error(), 'solve_chol should fail for indefinite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_indefinite_d
    
    subroutine test_solve_chol_indefinite_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        complex(sp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric INDEFINITE matrix (eigenvalues: 3, -1)
        a(1,:) = [1, 2]
        a(2,:) = [2, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for indefinite matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not positive definite)
        call check(error, state%error(), 'solve_chol should fail for indefinite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_indefinite_c
    
    subroutine test_solve_chol_indefinite_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        complex(dp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric INDEFINITE matrix (eigenvalues: 3, -1)
        a(1,:) = [1, 2]
        a(2,:) = [2, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for indefinite matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not positive definite)
        call check(error, state%error(), 'solve_chol should fail for indefinite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_indefinite_z
    

    !> Test solve_chol with symmetric positive semi-definite matrix (should fail)
    subroutine test_solve_chol_semidefinite_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        real(sp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive SEMI-DEFINITE matrix (eigenvalues: 2, 0 - singular)
        a(1,:) = [1, 1]
        a(2,:) = [1, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for semi-definite (singular) matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not strictly positive definite)
        call check(error, state%error(), 'solve_chol should fail for positive semi-definite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_semidefinite_s
    
    subroutine test_solve_chol_semidefinite_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        real(dp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive SEMI-DEFINITE matrix (eigenvalues: 2, 0 - singular)
        a(1,:) = [1, 1]
        a(2,:) = [1, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for semi-definite (singular) matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not strictly positive definite)
        call check(error, state%error(), 'solve_chol should fail for positive semi-definite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_semidefinite_d
    
    subroutine test_solve_chol_semidefinite_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        complex(sp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive SEMI-DEFINITE matrix (eigenvalues: 2, 0 - singular)
        a(1,:) = [1, 1]
        a(2,:) = [1, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for semi-definite (singular) matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not strictly positive definite)
        call check(error, state%error(), 'solve_chol should fail for positive semi-definite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_semidefinite_c
    
    subroutine test_solve_chol_semidefinite_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 2_ilp
        complex(dp) :: a(n,n), b(n), x(n)
        type(linalg_state_type) :: state
        
        ! Set symmetric positive SEMI-DEFINITE matrix (eigenvalues: 2, 0 - singular)
        a(1,:) = [1, 1]
        a(2,:) = [1, 1]
        
        ! Arbitrary RHS
        b = [1, 1]
        
        ! solve_chol should fail for semi-definite (singular) matrix
        call solve_chol(a, b, x, lower=.true., err=state)
        
        ! Check that it failed (not strictly positive definite)
        call check(error, state%error(), 'solve_chol should fail for positive semi-definite matrix')
        if (allocated(error)) return
        
    end subroutine test_solve_chol_semidefinite_z
    

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

end module test_linalg_solve_chol

program test_solve_chol
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_solve_chol, only : test_solve_chol_factorization
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_solve_chol", test_solve_chol_factorization) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_solve_chol
