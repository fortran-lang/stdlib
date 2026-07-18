
submodule(stdlib_linalg_iterative_solvers) stdlib_linalg_iterative_gmres
    use stdlib_kinds
    use stdlib_sparse
    use stdlib_constants
    use stdlib_optval, only: optval
    use stdlib_linalg_blas, only: gemv
    use stdlib_linalg_lapack, only: lartg, lasr, trtrs
    use stdlib_linalg_constants, only: ilp
    implicit none

contains

    module subroutine stdlib_solve_gmres_kernel_sp(A,M,b,x,rtol,atol,maxiter,kdim,workspace,compact)
        class(stdlib_linop_sp_type), intent(in) :: A
        class(stdlib_linop_sp_type), intent(in) :: M
        real(sp), intent(in) :: b(:), rtol, atol
        real(sp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter, kdim
        type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace
        logical, intent(in) :: compact
        integer :: i, iter, j, j_final, iorth, jz, info
        real(sp) :: beta, hnext, htmp, norm_sq, norm_sq0, temp, tolsq
        real(sp), allocatable :: cs(:), g(:), h(:,:), sn(:), y(:)

        allocate(h(kdim+1, kdim), cs(kdim), sn(kdim), g(kdim+1), y(kdim) )

        associate( r => workspace%tmp(:,1), &
               w => workspace%tmp(:,2), &
               v => workspace%tmp(:,3:kdim+3), &
               z => workspace%tmp(:,kdim + 4:) )
            
            iter = 0
            ! Initialize convergence targets from the right-hand side norm.
            norm_sq0 = A%inner_product(b, b)
            tolsq = max(rtol*rtol*norm_sq0, atol*atol)

            ! Form the initial residual and report the starting iterate.
            r = b
            call A%matvec(x, r, alpha=-one_sp, beta=one_sp, op='N')
            norm_sq = A%inner_product(r, r)
            if (associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)

            if (norm_sq <= tolsq) return

            do while (iter < maxiter .and. norm_sq >= tolsq)
                iter = iter + 1
                ! Start a new GMRES cycle from the current residual.
                beta = sqrt(max(norm_sq, zero_sp))
                if (beta <= epsilon(one_sp)) exit

                h = zero_sp
                cs = zero_sp
                sn = zero_sp
                g = zero_sp
                y = zero_sp

                ! Initialize the Krylov basis and least-squares right-hand side.
                v(:,1) = r / beta
                g(1) = beta

                j_final = 0
                do j = 1, kdim
                    ! Run Arnoldi with the preconditioned basis vector.
                    jz = merge(1, j, compact)
                    call M%matvec(v(:,j), z(:,jz), alpha=one_sp, beta=zero_sp, op='N')
                    call A%matvec(z(:,jz), w, alpha=one_sp, beta=zero_sp, op='N')

                    ! Modified Gram Schmidt (MGSR) 
                    do iorth = 1, 2 ! reorthogonalization
                        do i = 1, j
                            htmp   = A%inner_product(v(:,i), w)
                            h(i,j) = h(i,j) + htmp
                            w      = w - htmp*v(:,i)
                        end do
                    end do

                    hnext = sqrt(max(A%inner_product(w, w), zero_sp))
                    h(j+1,j) = hnext
                    if (hnext > epsilon(one_sp)) then
                        v(:,j+1) = w / hnext
                    else
                        v(:,j+1) = zero_sp
                    end if

                    ! Apply previous rotations to the new column, then generate the next one.
                    call apply_givens_rotation(h(1:j+1,j), cs, sn)

                    temp = cs(j) * g(j) + sn(j) * g(j+1)
                    g(j+1) = -sn(j) * g(j) + cs(j) * g(j+1)
                    g(j) = temp

                    ! Cheap residual-norm estimate; no solution rebuild needed.
                    norm_sq = g(j+1) * g(j+1)
                    j_final = j

                    if (norm_sq < tolsq .or. hnext <= epsilon(one_sp)) exit
                end do

                ! Cycle-end update from the least-squares correction.
                if (j_final > 0) then
                    call upper_triangular_solve(h, g, y, j_final, info)
                    if(info /= 0) exit

                    if (compact) then
                        call gemv('N', m=size(x), n=j_final, alpha=one_sp, &
                            a=v, lda=size(v,1), &
                            x=y, incx=1, &
                            beta=zero_sp, y=w, incy=1)
                        call M%matvec(w, z(:,1), alpha=one_sp, beta=zero_sp, op='N')
                        x = x + z(:,1)
                    else
                        call gemv('N', m=size(x), n=j_final, alpha=one_sp, &
                            a=z, lda=size(z,1), &
                            x=y, incx=1, &
                            beta=one_sp, y=x, incy=1)
                    end if
                end if

                ! Refresh the true residual so the convergence test per restart cycle and the logged
                ! value use the true ||b - A*x||, not the Hessenberg estimate.
                r = b
                call A%matvec(x, r, alpha=-one_sp, beta=one_sp, op='N')
                norm_sq = A%inner_product(r, r)

                if (associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end associate

    contains

        subroutine apply_givens_rotation(hcol, c, s)
            ! implementation inspired by https://github.com/nekStab/LightKrylov
            real(sp), target, contiguous, intent(inout) :: hcol(:)
            real(sp), intent(inout) :: c(:), s(:)
            integer :: k
            real(sp) :: r
            real(sp), pointer :: hmat(:, :)
            ! Size of the column.
            k = int(size(hcol) - 1, kind=ilp)
            ! Apply previous Givens rotations to this new column.
            hmat(1:k, 1:1) => hcol(:k)
            call lasr('L', 'V', 'F', k, 1_ilp, c(:k-1), s(:k-1), hmat, k)
            ! Compute the sine and cosine components for the next rotation.
            call lartg(hcol(k), hcol(k+1), c(k), s(k), r)
            ! Eliminate H(k+1, k).
            hcol(k) = r
            hcol(k+1) = zero_sp
        end subroutine

        subroutine upper_triangular_solve(h, g, y, n, info)
            real(sp), intent(in) :: h(:,:), g(:)
            real(sp), target, contiguous, intent(inout) :: y(:)
            integer, intent(in) :: n
            integer, intent(out) :: info
            integer(ilp) :: n_, lda_
            real(sp), pointer :: rhs(:, :)

            y(1:n) = g(1:n)

            n_ = int(n, kind=ilp)
            lda_ = int(size(h,1), kind=ilp)

            rhs(1:n,1:1) => y(:n)

            call trtrs('U','N','N', n_, 1_ilp, h, lda_, rhs, n_, info)
        end subroutine
    end subroutine
    module subroutine stdlib_solve_gmres_kernel_dp(A,M,b,x,rtol,atol,maxiter,kdim,workspace,compact)
        class(stdlib_linop_dp_type), intent(in) :: A
        class(stdlib_linop_dp_type), intent(in) :: M
        real(dp), intent(in) :: b(:), rtol, atol
        real(dp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter, kdim
        type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace
        logical, intent(in) :: compact
        integer :: i, iter, j, j_final, iorth, jz, info
        real(dp) :: beta, hnext, htmp, norm_sq, norm_sq0, temp, tolsq
        real(dp), allocatable :: cs(:), g(:), h(:,:), sn(:), y(:)

        allocate(h(kdim+1, kdim), cs(kdim), sn(kdim), g(kdim+1), y(kdim) )

        associate( r => workspace%tmp(:,1), &
               w => workspace%tmp(:,2), &
               v => workspace%tmp(:,3:kdim+3), &
               z => workspace%tmp(:,kdim + 4:) )
            
            iter = 0
            ! Initialize convergence targets from the right-hand side norm.
            norm_sq0 = A%inner_product(b, b)
            tolsq = max(rtol*rtol*norm_sq0, atol*atol)

            ! Form the initial residual and report the starting iterate.
            r = b
            call A%matvec(x, r, alpha=-one_dp, beta=one_dp, op='N')
            norm_sq = A%inner_product(r, r)
            if (associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)

            if (norm_sq <= tolsq) return

            do while (iter < maxiter .and. norm_sq >= tolsq)
                iter = iter + 1
                ! Start a new GMRES cycle from the current residual.
                beta = sqrt(max(norm_sq, zero_dp))
                if (beta <= epsilon(one_dp)) exit

                h = zero_dp
                cs = zero_dp
                sn = zero_dp
                g = zero_dp
                y = zero_dp

                ! Initialize the Krylov basis and least-squares right-hand side.
                v(:,1) = r / beta
                g(1) = beta

                j_final = 0
                do j = 1, kdim
                    ! Run Arnoldi with the preconditioned basis vector.
                    jz = merge(1, j, compact)
                    call M%matvec(v(:,j), z(:,jz), alpha=one_dp, beta=zero_dp, op='N')
                    call A%matvec(z(:,jz), w, alpha=one_dp, beta=zero_dp, op='N')

                    ! Modified Gram Schmidt (MGSR) 
                    do iorth = 1, 2 ! reorthogonalization
                        do i = 1, j
                            htmp   = A%inner_product(v(:,i), w)
                            h(i,j) = h(i,j) + htmp
                            w      = w - htmp*v(:,i)
                        end do
                    end do

                    hnext = sqrt(max(A%inner_product(w, w), zero_dp))
                    h(j+1,j) = hnext
                    if (hnext > epsilon(one_dp)) then
                        v(:,j+1) = w / hnext
                    else
                        v(:,j+1) = zero_dp
                    end if

                    ! Apply previous rotations to the new column, then generate the next one.
                    call apply_givens_rotation(h(1:j+1,j), cs, sn)

                    temp = cs(j) * g(j) + sn(j) * g(j+1)
                    g(j+1) = -sn(j) * g(j) + cs(j) * g(j+1)
                    g(j) = temp

                    ! Cheap residual-norm estimate; no solution rebuild needed.
                    norm_sq = g(j+1) * g(j+1)
                    j_final = j

                    if (norm_sq < tolsq .or. hnext <= epsilon(one_dp)) exit
                end do

                ! Cycle-end update from the least-squares correction.
                if (j_final > 0) then
                    call upper_triangular_solve(h, g, y, j_final, info)
                    if(info /= 0) exit

                    if (compact) then
                        call gemv('N', m=size(x), n=j_final, alpha=one_dp, &
                            a=v, lda=size(v,1), &
                            x=y, incx=1, &
                            beta=zero_dp, y=w, incy=1)
                        call M%matvec(w, z(:,1), alpha=one_dp, beta=zero_dp, op='N')
                        x = x + z(:,1)
                    else
                        call gemv('N', m=size(x), n=j_final, alpha=one_dp, &
                            a=z, lda=size(z,1), &
                            x=y, incx=1, &
                            beta=one_dp, y=x, incy=1)
                    end if
                end if

                ! Refresh the true residual so the convergence test per restart cycle and the logged
                ! value use the true ||b - A*x||, not the Hessenberg estimate.
                r = b
                call A%matvec(x, r, alpha=-one_dp, beta=one_dp, op='N')
                norm_sq = A%inner_product(r, r)

                if (associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end associate

    contains

        subroutine apply_givens_rotation(hcol, c, s)
            ! implementation inspired by https://github.com/nekStab/LightKrylov
            real(dp), target, contiguous, intent(inout) :: hcol(:)
            real(dp), intent(inout) :: c(:), s(:)
            integer :: k
            real(dp) :: r
            real(dp), pointer :: hmat(:, :)
            ! Size of the column.
            k = int(size(hcol) - 1, kind=ilp)
            ! Apply previous Givens rotations to this new column.
            hmat(1:k, 1:1) => hcol(:k)
            call lasr('L', 'V', 'F', k, 1_ilp, c(:k-1), s(:k-1), hmat, k)
            ! Compute the sine and cosine components for the next rotation.
            call lartg(hcol(k), hcol(k+1), c(k), s(k), r)
            ! Eliminate H(k+1, k).
            hcol(k) = r
            hcol(k+1) = zero_dp
        end subroutine

        subroutine upper_triangular_solve(h, g, y, n, info)
            real(dp), intent(in) :: h(:,:), g(:)
            real(dp), target, contiguous, intent(inout) :: y(:)
            integer, intent(in) :: n
            integer, intent(out) :: info
            integer(ilp) :: n_, lda_
            real(dp), pointer :: rhs(:, :)

            y(1:n) = g(1:n)

            n_ = int(n, kind=ilp)
            lda_ = int(size(h,1), kind=ilp)

            rhs(1:n,1:1) => y(:n)

            call trtrs('U','N','N', n_, 1_ilp, h, lda_, rhs, n_, info)
        end subroutine
    end subroutine

    module subroutine stdlib_solve_gmres_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,kdim,precond,M,workspace,compact)
        use stdlib_linalg, only: diag
        real(sp), intent(in) :: A(:,:)
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target :: di(:)
        integer, intent(in), optional :: maxiter, kdim
        logical, intent(in), optional :: restart
        integer, intent(in), optional :: precond
        class(stdlib_linop_sp_type), optional, intent(in), target :: M
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        logical, intent(in), optional :: compact
        type(stdlib_linop_sp_type) :: op
        type(stdlib_linop_sp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: kdim_, maxiter_, n, ncols, precond_
        real(sp) :: rtol_, atol_
        logical :: compact_, restart_
        logical(int8), pointer :: di_(:)
        real(sp), allocatable :: diagonal(:)

        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        kdim_ = max(1, min(optval(x=kdim, default=min(30, n)), n))
        restart_ = optval(x=restart, default=.true.)
        compact_ = optval(x=compact, default=.true.)
        rtol_ = optval(x=rtol, default=1.e-5_sp)
        atol_ = optval(x=atol, default=epsilon(one_sp))
        precond_ = optval(x=precond, default=pc_none)
        ncols = stdlib_size_wksp_gmres(kdim_,compact_)

        if (present(M)) then
            M_ => M
        else
            allocate(M_)
            allocate(diagonal(n), source=zero_sp)

            select case(precond_)
            case(pc_jacobi)
                diagonal = diag(A)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal) > epsilon(zero_sp)) diagonal = one_sp / diagonal
        end if

        op%matvec => matvec

        if (present(di)) then
            di_ => di
        else
            allocate(di_(n), source=.false._int8)
        end if

        if (present(workspace)) then
            workspace_ => workspace
        else
            allocate(workspace_)
        end if
        if (.not.allocated(workspace_%tmp)) then
            allocate(workspace_%tmp(n, ncols), source=zero_sp)
        else if (size(workspace_%tmp,1) /= n .or. size(workspace_%tmp,2) < ncols) then
            deallocate(workspace_%tmp)
            allocate(workspace_%tmp(n, ncols), source=zero_sp)
        end if

        if (restart_) x = zero_sp
        x = merge(b, x, di_)
        call stdlib_solve_gmres_kernel(op, M_, b, x, rtol_, atol_, maxiter_, kdim_, workspace_, compact=compact_)

        if (.not.present(di)) deallocate(di_)
        di_ => null()

        if (.not.present(workspace)) then
            deallocate(workspace_%tmp)
            deallocate(workspace_)
        end if
        M_ => null()
        workspace_ => null()

    contains

        subroutine matvec(x,y,alpha,beta,op)
            use stdlib_linalg_blas, only: gemv
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            call gemv(op, m=size(A,1), n=size(A,2), alpha=alpha, a=A, lda=size(A,1), x=x, incx=1, beta=beta, y=y, incy=1)
            y = merge(zero_sp, y, di_)
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_sp, x, di_)
        end subroutine

        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_sp, diagonal * x, di_)
        end subroutine
    end subroutine
    module subroutine stdlib_solve_gmres_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,kdim,precond,M,workspace,compact)
        use stdlib_linalg, only: diag
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target :: di(:)
        integer, intent(in), optional :: maxiter, kdim
        logical, intent(in), optional :: restart
        integer, intent(in), optional :: precond
        class(stdlib_linop_dp_type), optional, intent(in), target :: M
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        logical, intent(in), optional :: compact
        type(stdlib_linop_dp_type) :: op
        type(stdlib_linop_dp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: kdim_, maxiter_, n, ncols, precond_
        real(dp) :: rtol_, atol_
        logical :: compact_, restart_
        logical(int8), pointer :: di_(:)
        real(dp), allocatable :: diagonal(:)

        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        kdim_ = max(1, min(optval(x=kdim, default=min(30, n)), n))
        restart_ = optval(x=restart, default=.true.)
        compact_ = optval(x=compact, default=.true.)
        rtol_ = optval(x=rtol, default=1.e-5_dp)
        atol_ = optval(x=atol, default=epsilon(one_dp))
        precond_ = optval(x=precond, default=pc_none)
        ncols = stdlib_size_wksp_gmres(kdim_,compact_)

        if (present(M)) then
            M_ => M
        else
            allocate(M_)
            allocate(diagonal(n), source=zero_dp)

            select case(precond_)
            case(pc_jacobi)
                diagonal = diag(A)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal) > epsilon(zero_dp)) diagonal = one_dp / diagonal
        end if

        op%matvec => matvec

        if (present(di)) then
            di_ => di
        else
            allocate(di_(n), source=.false._int8)
        end if

        if (present(workspace)) then
            workspace_ => workspace
        else
            allocate(workspace_)
        end if
        if (.not.allocated(workspace_%tmp)) then
            allocate(workspace_%tmp(n, ncols), source=zero_dp)
        else if (size(workspace_%tmp,1) /= n .or. size(workspace_%tmp,2) < ncols) then
            deallocate(workspace_%tmp)
            allocate(workspace_%tmp(n, ncols), source=zero_dp)
        end if

        if (restart_) x = zero_dp
        x = merge(b, x, di_)
        call stdlib_solve_gmres_kernel(op, M_, b, x, rtol_, atol_, maxiter_, kdim_, workspace_, compact=compact_)

        if (.not.present(di)) deallocate(di_)
        di_ => null()

        if (.not.present(workspace)) then
            deallocate(workspace_%tmp)
            deallocate(workspace_)
        end if
        M_ => null()
        workspace_ => null()

    contains

        subroutine matvec(x,y,alpha,beta,op)
            use stdlib_linalg_blas, only: gemv
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            call gemv(op, m=size(A,1), n=size(A,2), alpha=alpha, a=A, lda=size(A,1), x=x, incx=1, beta=beta, y=y, incy=1)
            y = merge(zero_dp, y, di_)
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_dp, x, di_)
        end subroutine

        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_dp, diagonal * x, di_)
        end subroutine
    end subroutine
    module subroutine stdlib_solve_gmres_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,kdim,precond,M,workspace,compact)
        type(CSR_sp_type), intent(in) :: A
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target :: di(:)
        integer, intent(in), optional :: maxiter, kdim
        logical, intent(in), optional :: restart
        integer, intent(in), optional :: precond
        class(stdlib_linop_sp_type), optional, intent(in), target :: M
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        logical, intent(in), optional :: compact
        type(stdlib_linop_sp_type) :: op
        type(stdlib_linop_sp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: kdim_, maxiter_, n, ncols, precond_
        real(sp) :: rtol_, atol_
        logical :: compact_, restart_
        logical(int8), pointer :: di_(:)
        real(sp), allocatable :: diagonal(:)

        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        kdim_ = max(1, min(optval(x=kdim, default=min(30, n)), n))
        restart_ = optval(x=restart, default=.true.)
        compact_ = optval(x=compact, default=.true.)
        rtol_ = optval(x=rtol, default=1.e-5_sp)
        atol_ = optval(x=atol, default=epsilon(one_sp))
        precond_ = optval(x=precond, default=pc_none)
        ncols = stdlib_size_wksp_gmres(kdim_,compact_)

        if (present(M)) then
            M_ => M
        else
            allocate(M_)
            allocate(diagonal(n), source=zero_sp)

            select case(precond_)
            case(pc_jacobi)
                call diag(A, diagonal)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal) > epsilon(zero_sp)) diagonal = one_sp / diagonal
        end if

        op%matvec => matvec

        if (present(di)) then
            di_ => di
        else
            allocate(di_(n), source=.false._int8)
        end if

        if (present(workspace)) then
            workspace_ => workspace
        else
            allocate(workspace_)
        end if
        if (.not.allocated(workspace_%tmp)) then
            allocate(workspace_%tmp(n, ncols), source=zero_sp)
        else if (size(workspace_%tmp,1) /= n .or. size(workspace_%tmp,2) < ncols) then
            deallocate(workspace_%tmp)
            allocate(workspace_%tmp(n, ncols), source=zero_sp)
        end if

        if (restart_) x = zero_sp
        x = merge(b, x, di_)
        call stdlib_solve_gmres_kernel(op, M_, b, x, rtol_, atol_, maxiter_, kdim_, workspace_, compact=compact_)

        if (.not.present(di)) deallocate(di_)
        di_ => null()

        if (.not.present(workspace)) then
            deallocate(workspace_%tmp)
            deallocate(workspace_)
        end if
        M_ => null()
        workspace_ => null()

    contains

        subroutine matvec(x,y,alpha,beta,op)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            call spmv(A, x, y, alpha, beta, op)
            y = merge(zero_sp, y, di_)
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_sp, x, di_)
        end subroutine

        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_sp, diagonal * x, di_)
        end subroutine
    end subroutine
    module subroutine stdlib_solve_gmres_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,kdim,precond,M,workspace,compact)
        type(CSR_dp_type), intent(in) :: A
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target :: di(:)
        integer, intent(in), optional :: maxiter, kdim
        logical, intent(in), optional :: restart
        integer, intent(in), optional :: precond
        class(stdlib_linop_dp_type), optional, intent(in), target :: M
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        logical, intent(in), optional :: compact
        type(stdlib_linop_dp_type) :: op
        type(stdlib_linop_dp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: kdim_, maxiter_, n, ncols, precond_
        real(dp) :: rtol_, atol_
        logical :: compact_, restart_
        logical(int8), pointer :: di_(:)
        real(dp), allocatable :: diagonal(:)

        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        kdim_ = max(1, min(optval(x=kdim, default=min(30, n)), n))
        restart_ = optval(x=restart, default=.true.)
        compact_ = optval(x=compact, default=.true.)
        rtol_ = optval(x=rtol, default=1.e-5_dp)
        atol_ = optval(x=atol, default=epsilon(one_dp))
        precond_ = optval(x=precond, default=pc_none)
        ncols = stdlib_size_wksp_gmres(kdim_,compact_)

        if (present(M)) then
            M_ => M
        else
            allocate(M_)
            allocate(diagonal(n), source=zero_dp)

            select case(precond_)
            case(pc_jacobi)
                call diag(A, diagonal)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal) > epsilon(zero_dp)) diagonal = one_dp / diagonal
        end if

        op%matvec => matvec

        if (present(di)) then
            di_ => di
        else
            allocate(di_(n), source=.false._int8)
        end if

        if (present(workspace)) then
            workspace_ => workspace
        else
            allocate(workspace_)
        end if
        if (.not.allocated(workspace_%tmp)) then
            allocate(workspace_%tmp(n, ncols), source=zero_dp)
        else if (size(workspace_%tmp,1) /= n .or. size(workspace_%tmp,2) < ncols) then
            deallocate(workspace_%tmp)
            allocate(workspace_%tmp(n, ncols), source=zero_dp)
        end if

        if (restart_) x = zero_dp
        x = merge(b, x, di_)
        call stdlib_solve_gmres_kernel(op, M_, b, x, rtol_, atol_, maxiter_, kdim_, workspace_, compact=compact_)

        if (.not.present(di)) deallocate(di_)
        di_ => null()

        if (.not.present(workspace)) then
            deallocate(workspace_%tmp)
            deallocate(workspace_)
        end if
        M_ => null()
        workspace_ => null()

    contains

        subroutine matvec(x,y,alpha,beta,op)
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            call spmv(A, x, y, alpha, beta, op)
            y = merge(zero_dp, y, di_)
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_dp, x, di_)
        end subroutine

        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(dp), intent(in) :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge(zero_dp, diagonal * x, di_)
        end subroutine
    end subroutine

end submodule stdlib_linalg_iterative_gmres