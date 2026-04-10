module test_specialmatrices
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds
    use stdlib_linalg, only: hermitian
    use stdlib_linalg_state, only: linalg_state_type
    use stdlib_math, only: all_close, is_close
    use stdlib_specialmatrices
    use stdlib_strings, only: to_string
    implicit none
contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('tridiagonal', test_tridiagonal), &
            new_unittest('tridiagonal error handling', test_tridiagonal_error_handling), &
            new_unittest('sym_tridiagonal', test_sym_tridiagonal), &
            new_unittest('sym_tridiagonal error handling', test_sym_tridiagonal_error_handling), &
            new_unittest('symmetric tridiagonal 1x1 dense', test_sym_tridiagonal_1x1), &
            new_unittest('symmetric tridiagonal arithmetic', test_sym_tridiagonal_arithmetic), &
            new_unittest('tridiagonal 1x1 edge case', test_tridiagonal_1x1), &
            new_unittest('tridiagonal arithmetic', test_tridiagonal_arithmetic) &
        ]
    end subroutine

    subroutine test_tridiagonal(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(tridiagonal_sp_type) :: A
            real(sp), allocatable :: Amat(:,:), dl(:), dv(:), du(:)
            real(sp), allocatable :: x(:)
            real(sp), allocatable :: y1(:), y2(:)
            real(sp) :: alpha, beta

            integer :: i, j
            real(sp), parameter :: coeffs(3) = [-1.0_wp, 0.0_wp, 1.0_wp]

            ! Initialize matrix.
            allocate(dl(n-1), dv(n), du(n-1))
            call random_number(dl)
            call random_number(dv)
            call random_number(du)
            A = tridiagonal(dl, dv, du)
            Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)
            call random_number(x)
            allocate(y1(n), source = 0.0_wp)
            allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x)
            call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A*x")
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp
            y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x)
            call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A.T*x")
            if (allocated(error)) return


            ! Test y = alpha * A @ x + beta * y for alpha,beta in {-1,0,1}
            do i = 1, 3
                do j = 1,3
                    alpha = coeffs(i)
                    beta = coeffs(j)

                    y1 = 0.0_wp
                    call random_number(y2)
                    y1 = alpha * matmul(Amat, x) + beta * y2
                    call spmv(A, x, y2, alpha=alpha, beta=beta)
                    call check(error, all_close(y1, y2), .true.,&
                        "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
                    if (allocated(error)) return
                end do
            end do

            ! Test y = alpha * A @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(Amat, x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return

            ! Test y = alpha * A.T @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(transpose(Amat), x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A.T*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return


        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(tridiagonal_dp_type) :: A
            real(dp), allocatable :: Amat(:,:), dl(:), dv(:), du(:)
            real(dp), allocatable :: x(:)
            real(dp), allocatable :: y1(:), y2(:)
            real(dp) :: alpha, beta

            integer :: i, j
            real(dp), parameter :: coeffs(3) = [-1.0_wp, 0.0_wp, 1.0_wp]

            ! Initialize matrix.
            allocate(dl(n-1), dv(n), du(n-1))
            call random_number(dl)
            call random_number(dv)
            call random_number(du)
            A = tridiagonal(dl, dv, du)
            Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)
            call random_number(x)
            allocate(y1(n), source = 0.0_wp)
            allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x)
            call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A*x")
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp
            y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x)
            call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A.T*x")
            if (allocated(error)) return


            ! Test y = alpha * A @ x + beta * y for alpha,beta in {-1,0,1}
            do i = 1, 3
                do j = 1,3
                    alpha = coeffs(i)
                    beta = coeffs(j)

                    y1 = 0.0_wp
                    call random_number(y2)
                    y1 = alpha * matmul(Amat, x) + beta * y2
                    call spmv(A, x, y2, alpha=alpha, beta=beta)
                    call check(error, all_close(y1, y2), .true.,&
                        "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
                    if (allocated(error)) return
                end do
            end do

            ! Test y = alpha * A @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(Amat, x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return

            ! Test y = alpha * A.T @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(transpose(Amat), x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A.T*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return


        end block
    end subroutine

    subroutine test_sym_tridiagonal(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(sym_tridiagonal_sp_type) :: A
            real(sp), allocatable :: Amat(:,:), du(:), dv(:)
            real(sp), allocatable :: x(:)
            real(sp), allocatable :: y1(:), y2(:)
            real(sp) :: alpha, beta

            integer :: i, j
            real(sp), parameter :: coeffs(3) = [-1.0_wp, 0.0_wp, 1.0_wp]

            ! Initialize matrix.
            allocate(du(n-1), dv(n))
            call random_number(du)
            call random_number(dv)
            A = sym_tridiagonal(du, dv)
            Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)
            call random_number(x)
            allocate(y1(n), source = 0.0_wp)
            allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x)
            call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A*x")
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp
            y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x)
            call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A.T*x")
            if (allocated(error)) return


            ! Test y = alpha * A @ x + beta * y for alpha,beta in {-1,0,1}
            do i = 1, 3
                do j = 1,3
                    alpha = coeffs(i)
                    beta = coeffs(j)

                    y1 = 0.0_wp
                    call random_number(y2)
                    y1 = alpha * matmul(Amat, x) + beta * y2
                    call spmv(A, x, y2, alpha=alpha, beta=beta)
                    call check(error, all_close(y1, y2), .true.,&
                        "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
                    if (allocated(error)) return
                end do
            end do

            ! Test y = alpha * A @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(Amat, x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return

            ! Test y = alpha * A.T @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(transpose(Amat), x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A.T*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return


        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(sym_tridiagonal_dp_type) :: A
            real(dp), allocatable :: Amat(:,:), du(:), dv(:)
            real(dp), allocatable :: x(:)
            real(dp), allocatable :: y1(:), y2(:)
            real(dp) :: alpha, beta

            integer :: i, j
            real(dp), parameter :: coeffs(3) = [-1.0_wp, 0.0_wp, 1.0_wp]

            ! Initialize matrix.
            allocate(du(n-1), dv(n))
            call random_number(du)
            call random_number(dv)
            A = sym_tridiagonal(du, dv)
            Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)
            call random_number(x)
            allocate(y1(n), source = 0.0_wp)
            allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x)
            call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A*x")
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp
            y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x)
            call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = A.T*x")
            if (allocated(error)) return


            ! Test y = alpha * A @ x + beta * y for alpha,beta in {-1,0,1}
            do i = 1, 3
                do j = 1,3
                    alpha = coeffs(i)
                    beta = coeffs(j)

                    y1 = 0.0_wp
                    call random_number(y2)
                    y1 = alpha * matmul(Amat, x) + beta * y2
                    call spmv(A, x, y2, alpha=alpha, beta=beta)
                    call check(error, all_close(y1, y2), .true.,&
                        "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
                    if (allocated(error)) return
                end do
            end do

            ! Test y = alpha * A @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(Amat, x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta)
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return

            ! Test y = alpha * A.T @ x + beta * y for random values of alpha and beta
            y1 = 0.0_wp
            call random_number(alpha)
            call random_number(beta)
            call random_number(y2)
            y1 = alpha * matmul(transpose(Amat), x) + beta * y2
            call spmv(A, x, y2, alpha=alpha, beta=beta, op="T")
            call check(error, all_close(y1, y2), .true.,&
                "spmv(fail): y = alpha*A.T*x + beta*y, alpha: "//to_string(alpha)//", beta: "//to_string(beta))
            if (allocated(error)) return


        end block
    end subroutine

    subroutine test_tridiagonal_error_handling(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(tridiagonal_sp_type) :: A
            type(linalg_state_type) :: state
            
            real(sp), allocatable :: dl(:), du(:), dv(:)
            integer :: i

            !> Test constructor from arrays.
            dl = [(1.0_wp, i = 1, n-2)]
            du = dl
            dv = [(2.0_wp, i = 1, n)]
            A = tridiagonal(dl, dv, du, state)
            call check(error, state%ok(), .false., "Tridiagonal constructor from arrays failed")
            if (allocated(error)) return

            !> Test constructor from scalars.
            A = tridiagonal(dl(1), dv(1), du(1), -n, state)
            call check(error, state%ok(), .false., "Tridiagonal constructor from scalars failed")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(tridiagonal_dp_type) :: A
            type(linalg_state_type) :: state
            
            real(dp), allocatable :: dl(:), du(:), dv(:)
            integer :: i

            !> Test constructor from arrays.
            dl = [(1.0_wp, i = 1, n-2)]
            du = dl
            dv = [(2.0_wp, i = 1, n)]
            A = tridiagonal(dl, dv, du, state)
            call check(error, state%ok(), .false., "Tridiagonal constructor from arrays failed")
            if (allocated(error)) return

            !> Test constructor from scalars.
            A = tridiagonal(dl(1), dv(1), du(1), -n, state)
            call check(error, state%ok(), .false., "Tridiagonal constructor from scalars failed")
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_sym_tridiagonal_error_handling(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(sym_tridiagonal_sp_type) :: A
            real(sp), allocatable :: du(:), dv(:)
            type(linalg_state_type) :: state
            integer :: i

            !> Test constructor from arrays.
            du = [(1.0_wp, i = 1, n-2)]
            dv = [(2.0_wp, i = 1, n)]
            A = sym_tridiagonal(du, dv, state)
            call check(error, state%ok(), .false.,&
                "Symmetric tridiagonal constructor from arrays failed")
            if (allocated(error)) return

            !> Test constructor from scalars.
            A = sym_tridiagonal(du(1), dv(1), -n, state)
            call check(error, state%ok(), .false.,&
                "Symmetric tridiagonal constructor from scalars failed")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(sym_tridiagonal_dp_type) :: A
            real(dp), allocatable :: du(:), dv(:)
            type(linalg_state_type) :: state
            integer :: i

            !> Test constructor from arrays.
            du = [(1.0_wp, i = 1, n-2)]
            dv = [(2.0_wp, i = 1, n)]
            A = sym_tridiagonal(du, dv, state)
            call check(error, state%ok(), .false.,&
                "Symmetric tridiagonal constructor from arrays failed")
            if (allocated(error)) return

            !> Test constructor from scalars.
            A = sym_tridiagonal(du(1), dv(1), -n, state)
            call check(error, state%ok(), .false.,&
                "Symmetric tridiagonal constructor from scalars failed")
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_sym_tridiagonal_1x1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(sym_tridiagonal_sp_type) :: A
            real(sp), allocatable :: B(:, :)
            real(sp), allocatable :: dv(:), du(:)
            real(sp) :: C(1,1)

            allocate(dv(1), du(0))
            dv = [5.0_wp]

            A = sym_tridiagonal(du, dv)
            B = dense(A)
            C(1,1) = 5.0_wp

            call check(error, all_close(B, C), .true., &
                "Symmetric tridiagonal dense function failed (n=1)")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(sym_tridiagonal_dp_type) :: A
            real(dp), allocatable :: B(:, :)
            real(dp), allocatable :: dv(:), du(:)
            real(dp) :: C(1,1)

            allocate(dv(1), du(0))
            dv = [5.0_wp]

            A = sym_tridiagonal(du, dv)
            B = dense(A)
            C(1,1) = 5.0_wp

            call check(error, all_close(B, C), .true., &
                "Symmetric tridiagonal dense function failed (n=1)")
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_sym_tridiagonal_arithmetic(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(sym_tridiagonal_sp_type) :: A, B, C
            real(sp), allocatable :: dv(:), du(:)

            dv = [1.0_wp, 5.0_wp, 9.0_wp, 13.0_wp]
            du = [2.0_wp, 6.0_wp, 10.0_wp]
            A = sym_tridiagonal(du, dv)

            dv = [3.0_wp, 7.0_wp, 11.0_wp, 14.0_wp]
            du = [4.0_wp, 8.0_wp, 12.0_wp]
            B = sym_tridiagonal(du, dv)

            C = A + B
            call check(error, all_close(dense(C), dense(A) + dense(B)), .true., &
                "Symmetric tridiagonal operator + failed")
            if (allocated(error)) return

            C = A - B
            call check(error, all_close(dense(C), dense(A) - dense(B)), .true., &
                "Symmetric tridiagonal operator - failed")
            if (allocated(error)) return

            C = 5.0_wp * A
            call check(error, all_close(dense(C), 5.0_wp * dense(A)), .true., &
                "Symmetric tridiagonal operator * failed")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(sym_tridiagonal_dp_type) :: A, B, C
            real(dp), allocatable :: dv(:), du(:)

            dv = [1.0_wp, 5.0_wp, 9.0_wp, 13.0_wp]
            du = [2.0_wp, 6.0_wp, 10.0_wp]
            A = sym_tridiagonal(du, dv)

            dv = [3.0_wp, 7.0_wp, 11.0_wp, 14.0_wp]
            du = [4.0_wp, 8.0_wp, 12.0_wp]
            B = sym_tridiagonal(du, dv)

            C = A + B
            call check(error, all_close(dense(C), dense(A) + dense(B)), .true., &
                "Symmetric tridiagonal operator + failed")
            if (allocated(error)) return

            C = A - B
            call check(error, all_close(dense(C), dense(A) - dense(B)), .true., &
                "Symmetric tridiagonal operator - failed")
            if (allocated(error)) return

            C = 5.0_wp * A
            call check(error, all_close(dense(C), 5.0_wp * dense(A)), .true., &
                "Symmetric tridiagonal operator * failed")
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_tridiagonal_1x1(error)
        !> Test 1x1 matrix edge case for dense conversion
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(tridiagonal_sp_type) :: A
            real(sp), allocatable :: Amat(:,:)
            
            real(sp), parameter :: dl(0) = [real(sp) ::]
            real(sp), parameter :: du(0) = [real(sp) ::]
            real(sp), parameter :: dv(1) = [5.0_wp]
            
            A = tridiagonal(dl, dv, du) 
            Amat = dense(A)

            ! Check if the 1x1 matrix converted properly at runtime without segfaulting
            call check(error, size(Amat, 1) == 1, .true.)
            if (allocated(error)) return
            call check(error, size(Amat, 2) == 1, .true.)
            if (allocated(error)) return
            call check(error, is_close(Amat(1,1), 5.0_wp), .true.)
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(tridiagonal_dp_type) :: A
            real(dp), allocatable :: Amat(:,:)
            
            real(dp), parameter :: dl(0) = [real(dp) ::]
            real(dp), parameter :: du(0) = [real(dp) ::]
            real(dp), parameter :: dv(1) = [5.0_wp]
            
            A = tridiagonal(dl, dv, du) 
            Amat = dense(A)

            ! Check if the 1x1 matrix converted properly at runtime without segfaulting
            call check(error, size(Amat, 1) == 1, .true.)
            if (allocated(error)) return
            call check(error, size(Amat, 2) == 1, .true.)
            if (allocated(error)) return
            call check(error, is_close(Amat(1,1), 5.0_wp), .true.)
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_tridiagonal_arithmetic(error)
        !> Test arithmetic operations and optimization
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 3
            type(tridiagonal_sp_type) :: A, B, C
            
            real(sp), parameter :: dl1(n-1) = [1.0_wp, 1.0_wp]
            real(sp), parameter :: dv1(n)   = [2.0_wp, 2.0_wp, 2.0_wp]
            real(sp), parameter :: du1(n-1) = [3.0_wp, 3.0_wp]
            
            real(sp), parameter :: dl2(n-1) = [4.0_wp, 4.0_wp]
            real(sp), parameter :: dv2(n)   = [5.0_wp, 5.0_wp, 5.0_wp]
            real(sp), parameter :: du2(n-1) = [6.0_wp, 6.0_wp]
            
            A = tridiagonal(dl1, dv1, du1)
            B = tridiagonal(dl2, dv2, du2)
            
            ! Addition test - use dense() to bypass private component restrictions
            C = A + B
            call check(error, all_close(dense(C), dense(A) + dense(B)), .true.)
            if (allocated(error)) return
            
            ! Subtraction test
            C = A - B
            call check(error, all_close(dense(C), dense(A) - dense(B)), .true.)
            if (allocated(error)) return
            
            ! Scalar multiplication test
            C = 3.0_wp * A
            call check(error, all_close(dense(C), 3.0_wp * dense(A)), .true.)
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 3
            type(tridiagonal_dp_type) :: A, B, C
            
            real(dp), parameter :: dl1(n-1) = [1.0_wp, 1.0_wp]
            real(dp), parameter :: dv1(n)   = [2.0_wp, 2.0_wp, 2.0_wp]
            real(dp), parameter :: du1(n-1) = [3.0_wp, 3.0_wp]
            
            real(dp), parameter :: dl2(n-1) = [4.0_wp, 4.0_wp]
            real(dp), parameter :: dv2(n)   = [5.0_wp, 5.0_wp, 5.0_wp]
            real(dp), parameter :: du2(n-1) = [6.0_wp, 6.0_wp]
            
            A = tridiagonal(dl1, dv1, du1)
            B = tridiagonal(dl2, dv2, du2)
            
            ! Addition test - use dense() to bypass private component restrictions
            C = A + B
            call check(error, all_close(dense(C), dense(A) + dense(B)), .true.)
            if (allocated(error)) return
            
            ! Subtraction test
            C = A - B
            call check(error, all_close(dense(C), dense(A) - dense(B)), .true.)
            if (allocated(error)) return
            
            ! Scalar multiplication test
            C = 3.0_wp * A
            call check(error, all_close(dense(C), 3.0_wp * dense(A)), .true.)
            if (allocated(error)) return
        end block
    end subroutine

end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_specialmatrices, only : collect_suite
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("special_matrices", collect_suite) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program