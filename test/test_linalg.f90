
module test_linalg
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_linalg, only: diag, eye, trace, outer_product, cross_product, kronecker_product, hermitian
    use stdlib_linalg_state, only: linalg_state_type, LINALG_SUCCESS, linalg_error_handling

    implicit none

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)



contains


    !> Collect all exported unit tests
    subroutine collect_linalg(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("eye", test_eye), &
            new_unittest("diag_rsp", test_diag_rsp), &
            new_unittest("diag_rsp_k", test_diag_rsp_k), &
            new_unittest("diag_rdp", test_diag_rdp), &
            new_unittest("diag_rqp", test_diag_rqp), &
            new_unittest("diag_csp", test_diag_csp), &
            new_unittest("diag_cdp", test_diag_cdp), &
            new_unittest("diag_cqp", test_diag_cqp), &
            new_unittest("diag_int8", test_diag_int8), &
            new_unittest("diag_int16", test_diag_int16), &
            new_unittest("diag_int32", test_diag_int32), &
            new_unittest("diag_int64", test_diag_int64), &
            new_unittest("trace_rsp", test_trace_rsp), &
            new_unittest("trace_rsp_nonsquare", test_trace_rsp_nonsquare), &
            new_unittest("trace_rdp", test_trace_rdp), &
            new_unittest("trace_rdp_nonsquare", test_trace_rdp_nonsquare), &
            new_unittest("trace_rqp", test_trace_rqp), &
            new_unittest("trace_csp", test_trace_csp), &
            new_unittest("trace_cdp", test_trace_cdp), &
            new_unittest("trace_cqp", test_trace_cqp), &
            new_unittest("trace_int8", test_trace_int8), &
            new_unittest("trace_int16", test_trace_int16), &
            new_unittest("trace_int32", test_trace_int32), &
            new_unittest("trace_int64", test_trace_int64), &
            new_unittest("kronecker_product_rsp", test_kronecker_product_rsp), &
            new_unittest("kronecker_product_rdp", test_kronecker_product_rdp), &
            new_unittest("kronecker_product_csp", test_kronecker_product_csp), &
            new_unittest("kronecker_product_cdp", test_kronecker_product_cdp), &
            new_unittest("kronecker_product_iint8", test_kronecker_product_iint8), &
            new_unittest("kronecker_product_iint16", test_kronecker_product_iint16), &
            new_unittest("kronecker_product_iint32", test_kronecker_product_iint32), &
            new_unittest("kronecker_product_iint64", test_kronecker_product_iint64), &
            new_unittest("hermitian_csp", test_hermitian_csp), &
            new_unittest("hermitian_cdp", test_hermitian_cdp), &
            new_unittest("outer_product_rsp", test_outer_product_rsp), &
            new_unittest("outer_product_rdp", test_outer_product_rdp), &
            new_unittest("outer_product_rqp", test_outer_product_rqp), &
            new_unittest("outer_product_csp", test_outer_product_csp), &
            new_unittest("outer_product_cdp", test_outer_product_cdp), &
            new_unittest("outer_product_cqp", test_outer_product_cqp), &
            new_unittest("outer_product_int8", test_outer_product_int8), &
            new_unittest("outer_product_int16", test_outer_product_int16), &
            new_unittest("outer_product_int32", test_outer_product_int32), &
            new_unittest("outer_product_int64", test_outer_product_int64), &
            new_unittest("cross_product_rsp", test_cross_product_rsp), &
            new_unittest("cross_product_rdp", test_cross_product_rdp), &
            new_unittest("cross_product_rqp", test_cross_product_rqp), &
            new_unittest("cross_product_csp", test_cross_product_csp), &
            new_unittest("cross_product_cdp", test_cross_product_cdp), &
            new_unittest("cross_product_cqp", test_cross_product_cqp), &
            new_unittest("cross_product_int8", test_cross_product_int8), &
            new_unittest("cross_product_int16", test_cross_product_int16), &
            new_unittest("cross_product_int32", test_cross_product_int32), &
            new_unittest("cross_product_int64", test_cross_product_int64), &
            new_unittest("state_handling", test_state_handling) &
            ]

    end subroutine collect_linalg


    subroutine test_eye(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: rye(:,:)
        complex(sp) :: cye(7,7)
        integer :: i

        call check(error, all(eye(3,3) == diag([(1,i=1,3)])), &
            "all(eye(3,3) == diag([(1,i=1,3)])) failed.")
        if (allocated(error)) return

        rye = eye(3,4)
        call check(error, sum(abs(rye(:,1:3) - diag([(1.0_sp,i=1,3)]))) < sptol, &
            "sum(abs(rye(:,1:3) - diag([(1.0_sp,i=1,3)]))) < sptol failed")
        if (allocated(error)) return

        call check(error, all(eye(5) == diag([(1,i=1,5)])), &
            "all(eye(5) == diag([(1,i=1,5)] failed.")
        if (allocated(error)) return

        rye = eye(6)
        call check(error, sum(rye - diag([(1.0_sp,i=1,6)])) < sptol, &
            "sum(rye - diag([(1.0_sp,i=1,6)])) < sptol failed.")
        if (allocated(error)) return

        cye = eye(7)
        call check(error, abs(trace(cye) - cmplx(7.0_sp,0.0_sp,kind=sp)) < sptol, &
            "abs(trace(cye) - cmplx(7.0_sp,0.0_sp,kind=sp)) < sptol failed.")

    end subroutine test_eye


    subroutine test_diag_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        real(sp) :: v(n), a(n,n), b(n,n)
        integer :: i,j

        v = [(i,i=1,n)]
        a = diag(v)
        b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
        call check(error, all(a == b), &
            "all(a == b) failed.")
        if (allocated(error)) return

        call check(error, all(diag(3*a) == 3*v), &
            "all(diag(3*a) == 3*v) failed.")
    end subroutine test_diag_rsp

    subroutine test_diag_rsp_k(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        real(sp) :: a(n,n), b(n,n)
        integer :: i,j


        a = diag([(1._sp,i=1,n-1)],-1)

        b = reshape([((merge(1,0,i==j+1), i=1,n), j=1,n)], [n,n])

        call check(error, all(a == b), &
            "all(a == b) failed.")
        if (allocated(error)) return

        call check(error, sum(diag(a,-1)) - (n-1) < sptol, &
            "sum(diag(a,-1)) - (n-1) < sptol failed.")
        if (allocated(error)) return

        call check(error, all(a == transpose(diag([(1._sp,i=1,n-1)],1))), &
            "all(a == transpose(diag([(1._sp,i=1,n-1)],1))) failed")
        if (allocated(error)) return

        call random_number(a)
        do i = 1, n
            call check(error, size(diag(a,i)) == n-i, &
                "size(diag(a,i)) == n-i failed.")
            if (allocated(error)) return
        end do
        call check(error, size(diag(a,n+1)) == 0, &
            "size(diag(a,n+1)) == 0 failed.")
    end subroutine test_diag_rsp_k

    subroutine test_diag_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        real(dp) :: v(n), a(n,n), b(n,n)
        integer :: i,j

        v = [(i,i=1,n)]
        a = diag(v)
        b = reshape([((merge(i,0,i==j), i=1,n), j=1,n)], [n,n])
        call check(error, all(a == b), &
            "all(a == b) failed.")
        if (allocated(error)) return

        call check(error, all(diag(3*a) == 3*v), &
            "all(diag(3*a) == 3*v) failed.")
    end subroutine test_diag_rdp

    subroutine test_diag_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_diag_rqp

    subroutine test_diag_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        complex(sp) :: a(n,n), b(n,n)
        complex(sp), parameter :: i_ = cmplx(0,1,kind=sp)
        integer :: i,j

        a = diag([(i,i=1,n)]) + diag([(i_,i=1,n)])
        b = reshape([((merge(i + 1*i_,0*i_,i==j), i=1,n), j=1,n)], [n,n])
        call check(error, all(a == b), &
            "all(a == b) failed.")
        if (allocated(error)) return

        call check(error, all(abs(real(diag(a)) - [(i,i=1,n)]) < sptol), &
            "all(abs(real(diag(a)) - [(i,i=1,n)]) < sptol)")
        if (allocated(error)) return
        call check(error, all(abs(aimag(diag(a)) - [(1,i=1,n)]) < sptol), &
            "all(abs(aimag(diag(a)) - [(1,i=1,n)]) < sptol)")
    end subroutine test_diag_csp

    subroutine test_diag_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        complex(dp) :: a(n,n)
        complex(dp), parameter :: i_ = cmplx(0,1,kind=dp)

        a = diag([i_],-2) + diag([i_],2)
        call check(error, a(3,1) == i_ .and. a(1,3) == i_, &
            "a(3,1) == i_ .and. a(1,3) == i_ failed.")
    end subroutine test_diag_cdp

    subroutine test_diag_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_diag_cqp

    subroutine test_diag_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int8), allocatable :: a(:,:)
        integer :: i
        logical, allocatable :: mask(:,:)

        a = reshape([(i,i=1,n**2)],[n,n])
        mask = merge(.true.,.false.,eye(n) == 1)
        call check(error, all(diag(a) == pack(a,mask)), &
            "all(diag(a) == pack(a,mask)) failed.")
        if (allocated(error)) return
        call check(error, all(diag(diag(a)) == merge(a,0_int8,mask)), &
            "all(diag(diag(a)) == merge(a,0_int8,mask)) failed.")
    end subroutine test_diag_int8
    subroutine test_diag_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        integer(int16), allocatable :: a(:,:)
        integer :: i
        logical, allocatable :: mask(:,:)

        a = reshape([(i,i=1,n**2)],[n,n])
        mask = merge(.true.,.false.,eye(n) == 1)
        call check(error, all(diag(a) == pack(a,mask)), &
            "all(diag(a) == pack(a,mask))")
        if (allocated(error)) return
        call check(error, all(diag(diag(a)) == merge(a,0_int16,mask)), &
            "all(diag(diag(a)) == merge(a,0_int16,mask)) failed.")
    end subroutine test_diag_int16
    subroutine test_diag_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int32) :: a(n,n)
        logical :: mask(n,n)
        integer :: i, j

        mask = reshape([((merge(.true.,.false.,i==j+1), i=1,n), j=1,n)], [n,n])
        a = 0
        a = unpack([1_int32,1_int32],mask,a)
        call check(error, all(diag([1,1],-1) == a), &
            "all(diag([1,1],-1) == a) failed.")
        if (allocated(error)) return
        call check(error, all(diag([1,1],1) == transpose(a)), &
            "all(diag([1,1],1) == transpose(a)) failed.")
    end subroutine test_diag_int32
    subroutine test_diag_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        integer(int64) :: a(n,n), c(0:2*n-1)
        logical :: mask(n,n)
        integer :: i, j



        mask = reshape([((merge(.true.,.false.,i+1==j), i=1,n), j=1,n)], [n,n])
        a = 0
        a = unpack([1_int64,1_int64,1_int64],mask,a)

        call check(error, all(diag([1,1,1],1) == a), &
            "all(diag([1,1,1],1) == a) failed.")
        if (allocated(error)) return
        call check(error, all(diag([1,1,1],-1) == transpose(a)), &
            "all(diag([1,1,1],-1) == transpose(a)) failed.")
        if (allocated(error)) return


        ! Fill array c with Catalan numbers
        do i = 0, 2*n-1
            c(i) = catalan_number(i)
        end do
        ! Symmetric Hankel matrix filled with Catalan numbers (det(H) = 1)
        do i = 1, n
            do j = 1, n
                a(i,j) = c(i-1 + (j-1))
            end do
        end do
        call check(error, all(diag(a,-2) == diag(a,2)), &
            "all(diag(a,-2) == diag(a,2))")
    end subroutine test_diag_int64




    subroutine test_trace_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 5
        real(sp) :: a(n,n)
        integer :: i

        a = reshape([(i,i=1,n**2)],[n,n])
        call check(error, abs(trace(a) - sum(diag(a))) < sptol, &
            "abs(trace(a) - sum(diag(a))) < sptol failed.")
    end subroutine test_trace_rsp

    subroutine test_trace_rsp_nonsquare(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        real(sp) :: a(n,n+1), ans
        integer :: i


        ! 1   5   9  13  17
        ! 2   6  10  14  18
        ! 3   7  11  15  19
        ! 4   8  12  16  20
        a = reshape([(i,i=1,n*(n+1))],[n,n+1])
        ans = sum([1._sp,6._sp,11._sp,16._sp])

        call check(error, abs(trace(a) - ans) < sptol, &
            "abs(trace(a) - ans) < sptol failed.")
    end subroutine test_trace_rsp_nonsquare

    subroutine test_trace_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        real(dp) :: a(n,n)
        integer :: i

        a = reshape([(i,i=1,n**2)],[n,n])
        call check(error, abs(trace(a) - sum(diag(a))) < dptol, &
            "abs(trace(a) - sum(diag(a))) < dptol failed.")
    end subroutine test_trace_rdp

    subroutine test_trace_rdp_nonsquare(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 4
        real(dp) :: a(n,n-1), ans
        integer :: i


        !  1   25   81
        !  4   36  100
        !  9   49  121
        ! 16   64  144
        a = reshape([(i**2,i=1,n*(n-1))],[n,n-1])
        ans = sum([1._dp,36._dp,121._dp])

        call check(error, abs(trace(a) - ans) < dptol, &
            "abs(trace(a) - ans) < dptol failed.")
    end subroutine test_trace_rdp_nonsquare

    subroutine test_trace_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_trace_rqp


    subroutine test_trace_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 5
        real(sp) :: re(n,n), im(n,n)
        complex(sp) :: a(n,n), b(n,n)
        complex(sp), parameter :: i_ = cmplx(0,1,kind=sp)

        call random_number(re)
        call random_number(im)
        a = re + im*i_

        call random_number(re)
        call random_number(im)
        b = re + im*i_

        ! tr(A + B) = tr(A) + tr(B)
        call check(error, abs(trace(a+b) - (trace(a) + trace(b))) < sptol, &
            "abs(trace(a+b) - (trace(a) + trace(b))) < sptol failed.")
    end subroutine test_trace_csp

    subroutine test_trace_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        complex(dp) :: a(n,n), ans
        complex(dp), parameter :: i_ = cmplx(0,1,kind=dp)
        integer :: j


        a = reshape([(j + (n**2 - (j-1))*i_,j=1,n**2)],[n,n])
        ans = cmplx(15,15,kind=dp) !(1 + 5 + 9) + (9 + 5 + 1)i

        call check(error, abs(trace(a) - ans) < dptol, &
            "abs(trace(a) - ans) < dptol failed.")
    end subroutine test_trace_cdp

    subroutine test_trace_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_trace_cqp


    subroutine test_trace_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int8) :: a(n,n)
        integer :: i

        a = reshape([(i**2,i=1,n**2)],[n,n])
        call check(error, trace(a) == (1 + 25 + 81), &
            "trace(a) == (1 + 25 + 81) failed.")
    end subroutine test_trace_int8

    subroutine test_trace_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int16) :: a(n,n)
        integer :: i

        a = reshape([(i**3,i=1,n**2)],[n,n])
        call check(error, trace(a) == (1 + 125 + 729), &
            "trace(a) == (1 + 125 + 729) failed.")
    end subroutine test_trace_int16

    subroutine test_trace_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int32) :: a(n,n)
        integer :: i

        a = reshape([(i**4,i=1,n**2)],[n,n])
        call check(error, trace(a) == (1 + 625 + 6561), &
            "trace(a) == (1 + 625 + 6561) failed.")
    end subroutine test_trace_int32

    subroutine test_trace_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 5
        integer, parameter :: nd = 2*n-1 ! number of diagonals
        integer :: i, j
        integer(int64) :: c(0:nd), H(n,n)


        ! Fill array with Catalan numbers
        do i = 0, nd
            c(i) = catalan_number(i)
        end do

        ! Symmetric Hankel matrix filled with Catalan numbers (det(H) = 1)
        do i = 1, n
            do j = 1, n
                H(i,j) = c(i-1 + (j-1))
            end do
        end do

        call check(error, trace(h) == sum(c(0:nd:2)), &
            "trace(h) == sum(c(0:nd:2)) failed.")

    end subroutine test_trace_int64



    subroutine test_kronecker_product_rsp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      real(sp), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      real(sp), parameter :: tol = 1.e-6

      real(sp) :: A(m1,n1), B(m2,n2)
      real(sp) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_rsp
    subroutine test_kronecker_product_rdp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      real(dp), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      real(dp), parameter :: tol = 1.e-6

      real(dp) :: A(m1,n1), B(m2,n2)
      real(dp) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_rdp
    subroutine test_kronecker_product_csp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      complex(sp), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      complex(sp), parameter :: tol = 1.e-6

      complex(sp) :: A(m1,n1), B(m2,n2)
      complex(sp) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_csp
    subroutine test_kronecker_product_cdp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      complex(dp), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      complex(dp), parameter :: tol = 1.e-6

      complex(dp) :: A(m1,n1), B(m2,n2)
      complex(dp) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_cdp
    subroutine test_kronecker_product_iint8(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      integer(int8), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      integer(int8), parameter :: tol = 1.e-6

      integer(int8) :: A(m1,n1), B(m2,n2)
      integer(int8) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_iint8
    subroutine test_kronecker_product_iint16(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      integer(int16), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      integer(int16), parameter :: tol = 1.e-6

      integer(int16) :: A(m1,n1), B(m2,n2)
      integer(int16) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_iint16
    subroutine test_kronecker_product_iint32(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      integer(int32), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      integer(int32), parameter :: tol = 1.e-6

      integer(int32) :: A(m1,n1), B(m2,n2)
      integer(int32) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_iint32
    subroutine test_kronecker_product_iint64(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
      integer(int64), dimension(m1*m2,n1*n2), parameter :: expected &
           = transpose(reshape([1,2,3, 2,4,6, 2,4,6, 4,8,12], [m2*n2, m1*n1]))
      integer(int64), parameter :: tol = 1.e-6

      integer(int64) :: A(m1,n1), B(m2,n2)
      integer(int64) :: C(m1*m2,n1*n2), diff(m1*m2,n1*n2)

      integer :: i,j

      do j = 1, n1
         do i = 1, m1
            A(i,j) = i*j ! A = [1, 2]
         end do
      end do

      do j = 1, n2
         do i = 1, m2
            B(i,j) = i*j ! B = [[1, 2, 3], [2, 4, 6]]
         end do
      end do

      C = kronecker_product(A,B)

      diff = C - expected

      call check(error, all(abs(diff) .le. abs(tol)), "all(abs(diff) .le. abs(tol)) failed")
      ! Expected: C = [1*B, 2*B] = [[1,2,3, 2,4,6], [2,4,6, 4, 8, 12]]

    end subroutine test_kronecker_product_iint64

    subroutine test_hermitian_csp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m = 2, n = 3      
      complex(sp), dimension(m,n) :: A
      complex(sp), dimension(n,m) :: AT, expected, diff
      real(sp), parameter :: tol = 1.e-6_sp

      integer :: i,j
      
      do concurrent (i=1:m,j=1:n) 
        A       (i,j) = cmplx(i,-j,kind=sp)
        expected(j,i) = cmplx(i,+j,kind=sp)
      end do 


      AT = hermitian(A)

      diff = AT - expected

      call check(error, all(abs(diff) < abs(tol)), "hermitian: all(abs(diff) < abs(tol)) failed")

    end subroutine test_hermitian_csp
    subroutine test_hermitian_cdp(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: m = 2, n = 3      
      complex(dp), dimension(m,n) :: A
      complex(dp), dimension(n,m) :: AT, expected, diff
      real(dp), parameter :: tol = 1.e-6_dp

      integer :: i,j
      
      do concurrent (i=1:m,j=1:n) 
        A       (i,j) = cmplx(i,-j,kind=dp)
        expected(j,i) = cmplx(i,+j,kind=dp)
      end do 


      AT = hermitian(A)

      diff = AT - expected

      call check(error, all(abs(diff) < abs(tol)), "hermitian: all(abs(diff) < abs(tol)) failed")

    end subroutine test_hermitian_cdp

    subroutine test_outer_product_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        real(sp) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1.,2.]
        v = [1.,3.]
        expected = reshape([1.,2.,3.,6.],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) < sptol), &
            "all(abs(diff) < sptol) failed.")
    end subroutine test_outer_product_rsp

    subroutine test_outer_product_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        real(dp) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1.,2.]
        v = [1.,3.]
        expected = reshape([1.,2.,3.,6.],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) < dptol), &
            "all(abs(diff) < dptol) failed.")
    end subroutine test_outer_product_rdp

    subroutine test_outer_product_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_outer_product_rqp

    subroutine test_outer_product_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        complex(sp) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [cmplx(1.,1.),cmplx(2.,0.)]
        v = [cmplx(1.,0.),cmplx(3.,1.)]
        expected = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(2.,4.),cmplx(6.,2.)],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) < sptol), &
            "all(abs(diff) < sptol) failed.")
    end subroutine test_outer_product_csp

    subroutine test_outer_product_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        complex(dp) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [cmplx(1.,1.),cmplx(2.,0.)]
        v = [cmplx(1.,0.),cmplx(3.,1.)]
        expected = reshape([cmplx(1.,1.),cmplx(2.,0.),cmplx(2.,4.),cmplx(6.,2.)],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) < dptol), &
            "all(abs(diff) < dptol) failed.")
    end subroutine test_outer_product_cdp

    subroutine test_outer_product_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_outer_product_cqp

    subroutine test_outer_product_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        integer(int8) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1,2]
        v = [1,3]
        expected = reshape([1,2,3,6],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) == 0), &
            "all(abs(diff) == 0) failed.")
    end subroutine test_outer_product_int8

    subroutine test_outer_product_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        integer(int16) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1,2]
        v = [1,3]
        expected = reshape([1,2,3,6],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) == 0), &
            "all(abs(diff) == 0) failed.")
    end subroutine test_outer_product_int16

    subroutine test_outer_product_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        integer(int32) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1,2]
        v = [1,3]
        expected = reshape([1,2,3,6],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) == 0), &
            "all(abs(diff) == 0) failed.")
    end subroutine test_outer_product_int32

    subroutine test_outer_product_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 2
        integer(int64) :: u(n), v(n), expected(n,n), diff(n,n)

        u = [1,2]
        v = [1,3]
        expected = reshape([1,2,3,6],[n,n])
        diff = expected - outer_product(u,v)
        call check(error, all(abs(diff) == 0), &
            "all(abs(diff) == 0) failed.")
    end subroutine test_outer_product_int64

    subroutine test_cross_product_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int8) :: u(n), v(n), expected(n), diff(n)

        u = [1,0,0]
        v = [0,1,0]
        expected = [0,0,1]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) == 0), &
             "cross_product(u,v) == expected failed.")
    end subroutine test_cross_product_int8

    subroutine test_cross_product_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int16) :: u(n), v(n), expected(n), diff(n)

        u = [1,0,0]
        v = [0,1,0]
        expected = [0,0,1]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) == 0), &
             "cross_product(u,v) == expected failed.")
    end subroutine test_cross_product_int16

    subroutine test_cross_product_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int32) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_int32"
        u = [1,0,0]
        v = [0,1,0]
        expected = [0,0,1]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) == 0), &
             "cross_product(u,v) == expected failed.")
    end subroutine test_cross_product_int32

    subroutine test_cross_product_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        integer(int64) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_int64"
        u = [1,0,0]
        v = [0,1,0]
        expected = [0,0,1]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) == 0), &
             "cross_product(u,v) == expected failed.")
    end subroutine test_cross_product_int64

    subroutine test_cross_product_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        real(sp) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_rsp"
        u = [1.1_sp,2.5_sp,2.4_sp]
        v = [0.5_sp,1.5_sp,2.5_sp]
        expected = [2.65_sp,-1.55_sp,0.4_sp]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) < sptol), &
             "all(abs(cross_product(u,v)-expected)) < sptol failed.")
    end subroutine test_cross_product_rsp

    subroutine test_cross_product_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        real(dp) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_rdp"
        u = [1.1_dp,2.5_dp,2.4_dp]
        v = [0.5_dp,1.5_dp,2.5_dp]
        expected = [2.65_dp,-1.55_dp,0.4_dp]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) < dptol), &
             "all(abs(cross_product(u,v)-expected)) < dptol failed.")
    end subroutine test_cross_product_rdp

    subroutine test_cross_product_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_cross_product_rqp

    subroutine test_cross_product_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        complex(sp) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_csp"
        u = [cmplx(0,1,sp),cmplx(1,0,sp),cmplx(0,0,sp)]
        v = [cmplx(1,1,sp),cmplx(0,0,sp),cmplx(1,0,sp)]
        expected = [cmplx(1,0,sp),cmplx(0,-1,sp),cmplx(-1,-1,sp)]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) < sptol), &
             "all(abs(cross_product(u,v)-expected)) < sptol failed.")
    end subroutine test_cross_product_csp

    subroutine test_cross_product_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 3
        complex(dp) :: u(n), v(n), expected(n), diff(n)
        write(*,*) "test_cross_product_cdp"
        u = [cmplx(0,1,dp),cmplx(1,0,dp),cmplx(0,0,dp)]
        v = [cmplx(1,1,dp),cmplx(0,0,dp),cmplx(1,0,dp)]
        expected = [cmplx(1,0,dp),cmplx(0,-1,dp),cmplx(-1,-1,dp)]
        diff = expected - cross_product(u,v)
        call check(error, all(abs(diff) < dptol), &
             "all(abs(cross_product(u,v)-expected)) < dptol failed.")
    end subroutine test_cross_product_cdp

    subroutine test_cross_product_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_cross_product_cqp

    subroutine test_state_handling(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state,state_out

        state = linalg_state_type(LINALG_SUCCESS,' 32-bit real: ',1.0_sp)
        call check(error, &
        state%message==' 32-bit real: 1.00000000E+00', &
        "malformed state message with 32-bit reals.")
        if (allocated(error)) return

        state = linalg_state_type(LINALG_SUCCESS,' 64-bit real: ',1.0_dp)
        call check(error, &
        state%message==' 64-bit real: 1.0000000000000000E+000', &
        "malformed state message with 64-bit reals.")
        if (allocated(error)) return


        state = linalg_state_type(LINALG_SUCCESS,' 32-bit complex: ',(1.0_sp,1.0_sp))
        call check(error, &
        state%message==' 32-bit complex: (1.00000000E+00,1.00000000E+00)', &
        "malformed state message with 32-bit complex: "//trim(state%message))
        if (allocated(error)) return

        state = linalg_state_type(LINALG_SUCCESS,' 64-bit complex: ',(1.0_dp,1.0_dp))
        call check(error, &
        state%message==' 64-bit complex: (1.0000000000000000E+000,1.0000000000000000E+000)', &
        "malformed state message with 64-bit complex.")
        if (allocated(error)) return


        state = linalg_state_type(LINALG_SUCCESS,' 32-bit array: ',[(1.0_sp,0.0_sp),(0.0_sp,1.0_sp)])
        call check(error, state%message== &
        ' 32-bit array: [(1.00000000E+00,0.00000000E+00) (0.00000000E+00,1.00000000E+00)]', &
        "malformed state message with 32-bit real array.")
        if (allocated(error)) return

        !> State flag with location
        state = linalg_state_type('test_formats',LINALG_SUCCESS,' 32-bit real: ',1.0_sp)
        call check(error, &
        state%print()=='[test_formats] returned Success!', &
        "malformed state message with 32-bit real and location.")
        if (allocated(error)) return

        !> Test error handling procedure
        call linalg_error_handling(state,state_out)
        call check(error, state%print()==state_out%print(), &
        "malformed state message on return from error handling procedure.")

    end subroutine test_state_handling


    pure recursive function catalan_number(n) result(value)
        integer, intent(in) :: n
        integer :: value
        integer :: i
        if (n <= 1) then
            value = 1
        else
            value = 0
            do i = 0, n-1
                value = value + catalan_number(i)*catalan_number(n-i-1)
            end do
        end if
    end function

end module


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg, only : collect_linalg
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg", collect_linalg) &
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
