submodule (stdlib_linalg) stdlib_linalg_matrix_functions
    use stdlib_constants
    use stdlib_linalg_constants
    use stdlib_linalg_blas, only: gemm
    use stdlib_linalg_lapack, only: gesv, lacpy
    use stdlib_linalg_lapack_aux, only: handle_gesv_info
    use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
    implicit none(type, external)

    character(len=*), parameter :: this = "matrix_exponential"

contains

    module function stdlib_linalg_s_expm_fun(A, order) result(E)
        !> Input matrix A(n, n).
        real(sp), intent(in) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> Exponential of the input matrix E = exp(A).
        real(sp), allocatable :: E(:, :)

        E = A
        call stdlib_linalg_s_expm_inplace(E, order)
    end function stdlib_linalg_s_expm_fun

    module subroutine stdlib_linalg_s_expm(A, E, order, err)
        !> Input matrix A(n, n).
        real(sp), intent(in) :: A(:, :)
        !> Exponential of the input matrix E = exp(A).
        real(sp), intent(out) :: E(:, :)
         !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
       
        type(linalg_state_type) :: err0
        integer(ilp) :: lda, n, lde, ne
         
        ! Check E sizes
        lda = size(A, 1, kind=ilp) ; n = size(A, 2, kind=ilp)
        lde = size(E, 1, kind=ilp) ; ne = size(E, 2, kind=ilp)
          
        if (lda<1 .or. n<1 .or. lda/=n .or. lde/=n .or. ne/=n) then     
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: A must be square (lda=', lda, ', n=', n, ')', &
                                     ' E must be square (lde=', lde, ', ne=', ne, ')')
        else
            call lacpy("n", n, n, A, n, E, n) ! E = A
            call stdlib_linalg_s_expm_inplace(E, order, err0)
        endif
        
        ! Process output and return
        call linalg_error_handling(err0,err)

        return
    end subroutine stdlib_linalg_s_expm

    module subroutine stdlib_linalg_s_expm_inplace(A, order, err)
        !> Input matrix A(n, n) / Output matrix exponential.
        real(sp), intent(inout) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Internal variables.
        real(sp)                   :: A2(size(A, 1), size(A, 2)), Q(size(A, 1), size(A, 2))
        real(sp)                   :: X(size(A, 1), size(A, 2)), X_tmp(size(A, 1), size(A, 2))
        real(sp)             :: a_norm, c
        integer(ilp)            :: m, n, ee, k, s, order_, i, j
        logical(lk)             :: p
        type(linalg_state_type) :: err0

        ! Deal with optional args.
        order_ = 10 ; if (present(order)) order_ = order

        ! Problem's dimension.
        m = size(A, dim=1, kind=ilp) ; n = size(A, dim=2, kind=ilp)

        if (m /= n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'Invalid matrix size A=',[m, n])
        else if (order_ < 0) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'Order of Pade approximation &
                                    needs to be positive, order=', order_)
        else
            ! Compute the L-infinity norm.
            a_norm = mnorm(A, "inf")

            ! Determine scaling factor for the matrix.
            ee = int(log(a_norm) / log2_sp, kind=ilp) + 1
            s  = max(0, ee+1)

            ! Scale the input matrix & initialize polynomial.
            A2 = A/2.0_sp**s
            call lacpy("n", n, n, A2, n, X, n) ! X = A2

            ! First step of the Pade approximation.
            c = 0.5_sp
            do concurrent(i=1:n, j=1:n)
                A(i, j) = merge(1.0_sp + c*A2(i, j), c*A2(i, j), i == j)
                Q(i, j) = merge(1.0_sp - c*A2(i, j), -c*A2(i, j), i == j)
            enddo

            ! Iteratively compute the Pade approximation.
            p = .true.
            do k = 2, order_
                c = c * (order_ - k + 1) / (k * (2*order_ - k + 1))
                call lacpy("n", n, n, X, n, X_tmp, n) ! X_tmp = X
                call gemm("N", "N", n, n, n, one_sp, A2, n, X_tmp, n, zero_sp, X, n)
                do concurrent(i=1:n, j=1:n)
                    A(i, j) = A(i, j) + c*X(i, j)       ! E = E + c*X
                    Q(i, j) = merge(Q(i, j) + c*X(i, j), Q(i, j) - c*X(i, j), p)
                enddo
                p = .not. p
            enddo

            block
                integer(ilp) :: ipiv(n), info
                call gesv(n, n, Q, n, ipiv, A, n, info) ! E = inv(Q) @ E
                call handle_gesv_info(this, info, n, n, n, err0)
            end block

            ! Matrix squaring.
            do k = 1, s
                call lacpy("n", n, n, A, n, X, n) ! X = A
                call gemm("N", "N", n, n, n, one_sp, X, n, X, n, zero_sp, A, n)
            enddo
        endif
        
        call linalg_error_handling(err0, err)

        return
    end subroutine stdlib_linalg_s_expm_inplace
    module function stdlib_linalg_d_expm_fun(A, order) result(E)
        !> Input matrix A(n, n).
        real(dp), intent(in) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> Exponential of the input matrix E = exp(A).
        real(dp), allocatable :: E(:, :)

        E = A
        call stdlib_linalg_d_expm_inplace(E, order)
    end function stdlib_linalg_d_expm_fun

    module subroutine stdlib_linalg_d_expm(A, E, order, err)
        !> Input matrix A(n, n).
        real(dp), intent(in) :: A(:, :)
        !> Exponential of the input matrix E = exp(A).
        real(dp), intent(out) :: E(:, :)
         !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
       
        type(linalg_state_type) :: err0
        integer(ilp) :: lda, n, lde, ne
         
        ! Check E sizes
        lda = size(A, 1, kind=ilp) ; n = size(A, 2, kind=ilp)
        lde = size(E, 1, kind=ilp) ; ne = size(E, 2, kind=ilp)
          
        if (lda<1 .or. n<1 .or. lda/=n .or. lde/=n .or. ne/=n) then     
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: A must be square (lda=', lda, ', n=', n, ')', &
                                     ' E must be square (lde=', lde, ', ne=', ne, ')')
        else
            call lacpy("n", n, n, A, n, E, n) ! E = A
            call stdlib_linalg_d_expm_inplace(E, order, err0)
        endif
        
        ! Process output and return
        call linalg_error_handling(err0,err)

        return
    end subroutine stdlib_linalg_d_expm

    module subroutine stdlib_linalg_d_expm_inplace(A, order, err)
        !> Input matrix A(n, n) / Output matrix exponential.
        real(dp), intent(inout) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Internal variables.
        real(dp)                   :: A2(size(A, 1), size(A, 2)), Q(size(A, 1), size(A, 2))
        real(dp)                   :: X(size(A, 1), size(A, 2)), X_tmp(size(A, 1), size(A, 2))
        real(dp)             :: a_norm, c
        integer(ilp)            :: m, n, ee, k, s, order_, i, j
        logical(lk)             :: p
        type(linalg_state_type) :: err0

        ! Deal with optional args.
        order_ = 10 ; if (present(order)) order_ = order

        ! Problem's dimension.
        m = size(A, dim=1, kind=ilp) ; n = size(A, dim=2, kind=ilp)

        if (m /= n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'Invalid matrix size A=',[m, n])
        else if (order_ < 0) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'Order of Pade approximation &
                                    needs to be positive, order=', order_)
        else
            ! Compute the L-infinity norm.
            a_norm = mnorm(A, "inf")

            ! Determine scaling factor for the matrix.
            ee = int(log(a_norm) / log2_dp, kind=ilp) + 1
            s  = max(0, ee+1)

            ! Scale the input matrix & initialize polynomial.
            A2 = A/2.0_dp**s
            call lacpy("n", n, n, A2, n, X, n) ! X = A2

            ! First step of the Pade approximation.
            c = 0.5_dp
            do concurrent(i=1:n, j=1:n)
                A(i, j) = merge(1.0_dp + c*A2(i, j), c*A2(i, j), i == j)
                Q(i, j) = merge(1.0_dp - c*A2(i, j), -c*A2(i, j), i == j)
            enddo

            ! Iteratively compute the Pade approximation.
            p = .true.
            do k = 2, order_
                c = c * (order_ - k + 1) / (k * (2*order_ - k + 1))
                call lacpy("n", n, n, X, n, X_tmp, n) ! X_tmp = X
                call gemm("N", "N", n, n, n, one_dp, A2, n, X_tmp, n, zero_dp, X, n)
                do concurrent(i=1:n, j=1:n)
                    A(i, j) = A(i, j) + c*X(i, j)       ! E = E + c*X
                    Q(i, j) = merge(Q(i, j) + c*X(i, j), Q(i, j) - c*X(i, j), p)
                enddo
                p = .not. p
            enddo

            block
                integer(ilp) :: ipiv(n), info
                call gesv(n, n, Q, n, ipiv, A, n, info) ! E = inv(Q) @ E
                call handle_gesv_info(this, info, n, n, n, err0)
            end block

            ! Matrix squaring.
            do k = 1, s
                call lacpy("n", n, n, A, n, X, n) ! X = A
                call gemm("N", "N", n, n, n, one_dp, X, n, X, n, zero_dp, A, n)
            enddo
        endif
        
        call linalg_error_handling(err0, err)

        return
    end subroutine stdlib_linalg_d_expm_inplace
    module function stdlib_linalg_c_expm_fun(A, order) result(E)
        !> Input matrix A(n, n).
        complex(sp), intent(in) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> Exponential of the input matrix E = exp(A).
        complex(sp), allocatable :: E(:, :)

        E = A
        call stdlib_linalg_c_expm_inplace(E, order)
    end function stdlib_linalg_c_expm_fun

    module subroutine stdlib_linalg_c_expm(A, E, order, err)
        !> Input matrix A(n, n).
        complex(sp), intent(in) :: A(:, :)
        !> Exponential of the input matrix E = exp(A).
        complex(sp), intent(out) :: E(:, :)
         !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
       
        type(linalg_state_type) :: err0
        integer(ilp) :: lda, n, lde, ne
         
        ! Check E sizes
        lda = size(A, 1, kind=ilp) ; n = size(A, 2, kind=ilp)
        lde = size(E, 1, kind=ilp) ; ne = size(E, 2, kind=ilp)
          
        if (lda<1 .or. n<1 .or. lda/=n .or. lde/=n .or. ne/=n) then     
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: A must be square (lda=', lda, ', n=', n, ')', &
                                     ' E must be square (lde=', lde, ', ne=', ne, ')')
        else
            call lacpy("n", n, n, A, n, E, n) ! E = A
            call stdlib_linalg_c_expm_inplace(E, order, err0)
        endif
        
        ! Process output and return
        call linalg_error_handling(err0,err)

        return
    end subroutine stdlib_linalg_c_expm

    module subroutine stdlib_linalg_c_expm_inplace(A, order, err)
        !> Input matrix A(n, n) / Output matrix exponential.
        complex(sp), intent(inout) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Internal variables.
        complex(sp)                   :: A2(size(A, 1), size(A, 2)), Q(size(A, 1), size(A, 2))
        complex(sp)                   :: X(size(A, 1), size(A, 2)), X_tmp(size(A, 1), size(A, 2))
        real(sp)             :: a_norm, c
        integer(ilp)            :: m, n, ee, k, s, order_, i, j
        logical(lk)             :: p
        type(linalg_state_type) :: err0

        ! Deal with optional args.
        order_ = 10 ; if (present(order)) order_ = order

        ! Problem's dimension.
        m = size(A, dim=1, kind=ilp) ; n = size(A, dim=2, kind=ilp)

        if (m /= n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'Invalid matrix size A=',[m, n])
        else if (order_ < 0) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'Order of Pade approximation &
                                    needs to be positive, order=', order_)
        else
            ! Compute the L-infinity norm.
            a_norm = mnorm(A, "inf")

            ! Determine scaling factor for the matrix.
            ee = int(log(a_norm) / log2_sp, kind=ilp) + 1
            s  = max(0, ee+1)

            ! Scale the input matrix & initialize polynomial.
            A2 = A/2.0_sp**s
            call lacpy("n", n, n, A2, n, X, n) ! X = A2

            ! First step of the Pade approximation.
            c = 0.5_sp
            do concurrent(i=1:n, j=1:n)
                A(i, j) = merge(1.0_sp + c*A2(i, j), c*A2(i, j), i == j)
                Q(i, j) = merge(1.0_sp - c*A2(i, j), -c*A2(i, j), i == j)
            enddo

            ! Iteratively compute the Pade approximation.
            p = .true.
            do k = 2, order_
                c = c * (order_ - k + 1) / (k * (2*order_ - k + 1))
                call lacpy("n", n, n, X, n, X_tmp, n) ! X_tmp = X
                call gemm("N", "N", n, n, n, one_csp, A2, n, X_tmp, n, zero_csp, X, n)
                do concurrent(i=1:n, j=1:n)
                    A(i, j) = A(i, j) + c*X(i, j)       ! E = E + c*X
                    Q(i, j) = merge(Q(i, j) + c*X(i, j), Q(i, j) - c*X(i, j), p)
                enddo
                p = .not. p
            enddo

            block
                integer(ilp) :: ipiv(n), info
                call gesv(n, n, Q, n, ipiv, A, n, info) ! E = inv(Q) @ E
                call handle_gesv_info(this, info, n, n, n, err0)
            end block

            ! Matrix squaring.
            do k = 1, s
                call lacpy("n", n, n, A, n, X, n) ! X = A
                call gemm("N", "N", n, n, n, one_csp, X, n, X, n, zero_csp, A, n)
            enddo
        endif
        
        call linalg_error_handling(err0, err)

        return
    end subroutine stdlib_linalg_c_expm_inplace
    module function stdlib_linalg_z_expm_fun(A, order) result(E)
        !> Input matrix A(n, n).
        complex(dp), intent(in) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> Exponential of the input matrix E = exp(A).
        complex(dp), allocatable :: E(:, :)

        E = A
        call stdlib_linalg_z_expm_inplace(E, order)
    end function stdlib_linalg_z_expm_fun

    module subroutine stdlib_linalg_z_expm(A, E, order, err)
        !> Input matrix A(n, n).
        complex(dp), intent(in) :: A(:, :)
        !> Exponential of the input matrix E = exp(A).
        complex(dp), intent(out) :: E(:, :)
         !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
       
        type(linalg_state_type) :: err0
        integer(ilp) :: lda, n, lde, ne
         
        ! Check E sizes
        lda = size(A, 1, kind=ilp) ; n = size(A, 2, kind=ilp)
        lde = size(E, 1, kind=ilp) ; ne = size(E, 2, kind=ilp)
          
        if (lda<1 .or. n<1 .or. lda/=n .or. lde/=n .or. ne/=n) then     
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: A must be square (lda=', lda, ', n=', n, ')', &
                                     ' E must be square (lde=', lde, ', ne=', ne, ')')
        else
            call lacpy("n", n, n, A, n, E, n) ! E = A
            call stdlib_linalg_z_expm_inplace(E, order, err0)
        endif
        
        ! Process output and return
        call linalg_error_handling(err0,err)

        return
    end subroutine stdlib_linalg_z_expm

    module subroutine stdlib_linalg_z_expm_inplace(A, order, err)
        !> Input matrix A(n, n) / Output matrix exponential.
        complex(dp), intent(inout) :: A(:, :)
        !> [optional] Order of the Pade approximation.
        integer(ilp), optional, intent(in) :: order
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Internal variables.
        complex(dp)                   :: A2(size(A, 1), size(A, 2)), Q(size(A, 1), size(A, 2))
        complex(dp)                   :: X(size(A, 1), size(A, 2)), X_tmp(size(A, 1), size(A, 2))
        real(dp)             :: a_norm, c
        integer(ilp)            :: m, n, ee, k, s, order_, i, j
        logical(lk)             :: p
        type(linalg_state_type) :: err0

        ! Deal with optional args.
        order_ = 10 ; if (present(order)) order_ = order

        ! Problem's dimension.
        m = size(A, dim=1, kind=ilp) ; n = size(A, dim=2, kind=ilp)

        if (m /= n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'Invalid matrix size A=',[m, n])
        else if (order_ < 0) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'Order of Pade approximation &
                                    needs to be positive, order=', order_)
        else
            ! Compute the L-infinity norm.
            a_norm = mnorm(A, "inf")

            ! Determine scaling factor for the matrix.
            ee = int(log(a_norm) / log2_dp, kind=ilp) + 1
            s  = max(0, ee+1)

            ! Scale the input matrix & initialize polynomial.
            A2 = A/2.0_dp**s
            call lacpy("n", n, n, A2, n, X, n) ! X = A2

            ! First step of the Pade approximation.
            c = 0.5_dp
            do concurrent(i=1:n, j=1:n)
                A(i, j) = merge(1.0_dp + c*A2(i, j), c*A2(i, j), i == j)
                Q(i, j) = merge(1.0_dp - c*A2(i, j), -c*A2(i, j), i == j)
            enddo

            ! Iteratively compute the Pade approximation.
            p = .true.
            do k = 2, order_
                c = c * (order_ - k + 1) / (k * (2*order_ - k + 1))
                call lacpy("n", n, n, X, n, X_tmp, n) ! X_tmp = X
                call gemm("N", "N", n, n, n, one_cdp, A2, n, X_tmp, n, zero_cdp, X, n)
                do concurrent(i=1:n, j=1:n)
                    A(i, j) = A(i, j) + c*X(i, j)       ! E = E + c*X
                    Q(i, j) = merge(Q(i, j) + c*X(i, j), Q(i, j) - c*X(i, j), p)
                enddo
                p = .not. p
            enddo

            block
                integer(ilp) :: ipiv(n), info
                call gesv(n, n, Q, n, ipiv, A, n, info) ! E = inv(Q) @ E
                call handle_gesv_info(this, info, n, n, n, err0)
            end block

            ! Matrix squaring.
            do k = 1, s
                call lacpy("n", n, n, A, n, X, n) ! X = A
                call gemm("N", "N", n, n, n, one_cdp, X, n, X, n, zero_cdp, A, n)
            enddo
        endif
        
        call linalg_error_handling(err0, err)

        return
    end subroutine stdlib_linalg_z_expm_inplace

end submodule stdlib_linalg_matrix_functions
