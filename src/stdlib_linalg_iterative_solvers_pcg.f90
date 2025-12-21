
submodule(stdlib_linalg_iterative_solvers) stdlib_linalg_iterative_pcg
    use stdlib_kinds
    use stdlib_sparse
    use stdlib_constants
    use stdlib_optval, only: optval
    implicit none

contains

    module subroutine stdlib_solve_pcg_kernel_sp(A,M,b,x,rtol,atol,maxiter,workspace)
        class(stdlib_linop_sp_type), intent(in) :: A
        class(stdlib_linop_sp_type), intent(in) :: M 
        real(sp), intent(in) :: b(:), rtol, atol
        real(sp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter
        type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace
        !-------------------------
        integer :: iter
        real(sp) :: norm_sq, norm_sq0, norm_sq_old
        real(sp) :: zr1, zr2, zv2, alpha, beta, tolsq
        !-------------------------
        iter = 0
        associate(  R => workspace%tmp(:,1), &
                    S => workspace%tmp(:,2), &
                    P => workspace%tmp(:,3), &
                    Q => workspace%tmp(:,4))
        norm_sq = A%inner_product( b, b )
        norm_sq0 = norm_sq
        if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
        
        if ( norm_sq0 > zero_sp ) then
            
            R = B
            call A%matvec(X, R, alpha= -one_sp, beta=one_sp, op='N') ! R = B - A*X
            
            call M%matvec(R,P, alpha= one_sp, beta=zero_sp, op='N') ! P = M^{-1}*R
            
            tolsq = max(rtol*rtol * norm_sq0, atol*atol)
            
            zr1 = zero_sp
            zr2 = one_sp
            do while ( (iter < maxiter) .AND. (norm_sq >= tolsq) )

                call M%matvec(R,S, alpha= one_sp, beta=zero_sp, op='N') ! S = M^{-1}*R
                zr2 = A%inner_product( R, S )
            
                if (iter>0) then
                    beta = zr2 / zr1
                    P = S + beta * P
                end if

                call A%matvec(P, Q, alpha= one_sp, beta=zero_sp, op='N') ! Q = A*P
                zv2 = A%inner_product( P, Q )
            
                alpha = zr2 / zv2
            
                X = X + alpha * P
                R = R - alpha * Q

                norm_sq = A%inner_product( R, R )
                norm_sq_old = norm_sq

                zr1 = zr2
                iter = iter + 1
                if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end if
        end associate
    end subroutine
    module subroutine stdlib_solve_pcg_kernel_dp(A,M,b,x,rtol,atol,maxiter,workspace)
        class(stdlib_linop_dp_type), intent(in) :: A
        class(stdlib_linop_dp_type), intent(in) :: M 
        real(dp), intent(in) :: b(:), rtol, atol
        real(dp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter
        type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace
        !-------------------------
        integer :: iter
        real(dp) :: norm_sq, norm_sq0, norm_sq_old
        real(dp) :: zr1, zr2, zv2, alpha, beta, tolsq
        !-------------------------
        iter = 0
        associate(  R => workspace%tmp(:,1), &
                    S => workspace%tmp(:,2), &
                    P => workspace%tmp(:,3), &
                    Q => workspace%tmp(:,4))
        norm_sq = A%inner_product( b, b )
        norm_sq0 = norm_sq
        if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
        
        if ( norm_sq0 > zero_dp ) then
            
            R = B
            call A%matvec(X, R, alpha= -one_dp, beta=one_dp, op='N') ! R = B - A*X
            
            call M%matvec(R,P, alpha= one_dp, beta=zero_dp, op='N') ! P = M^{-1}*R
            
            tolsq = max(rtol*rtol * norm_sq0, atol*atol)
            
            zr1 = zero_dp
            zr2 = one_dp
            do while ( (iter < maxiter) .AND. (norm_sq >= tolsq) )

                call M%matvec(R,S, alpha= one_dp, beta=zero_dp, op='N') ! S = M^{-1}*R
                zr2 = A%inner_product( R, S )
            
                if (iter>0) then
                    beta = zr2 / zr1
                    P = S + beta * P
                end if

                call A%matvec(P, Q, alpha= one_dp, beta=zero_dp, op='N') ! Q = A*P
                zv2 = A%inner_product( P, Q )
            
                alpha = zr2 / zv2
            
                X = X + alpha * P
                R = R - alpha * Q

                norm_sq = A%inner_product( R, R )
                norm_sq_old = norm_sq

                zr1 = zr2
                iter = iter + 1
                if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end if
        end associate
    end subroutine

    module subroutine stdlib_solve_pcg_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
        use stdlib_linalg, only: diag
        real(sp), intent(in) :: A(:,:)
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional  :: maxiter
        logical, intent(in), optional  :: restart
        integer, intent(in), optional  :: precond 
        class(stdlib_linop_sp_type), optional , intent(in), target :: M 
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_sp_type) :: op
        type(stdlib_linop_sp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(sp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        ! working data for preconditioner
        integer :: precond_
        real(sp), allocatable :: diagonal(:)

        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_sp)
        atol_    = optval(x=atol,    default=epsilon(one_sp))
        precond_ = optval(x=precond, default=pc_none)
        !-------------------------
        ! internal memory setup
        ! preconditioner
        if(present(M)) then
            M_ => M
        else 
            allocate( M_ )
            allocate(diagonal(n),source=zero_sp)

            select case(precond_)
            case(pc_jacobi)
                diagonal = diag(A)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal)>epsilon(zero_sp)) diagonal = one_sp/diagonal
        end if
        ! matvec for the operator
        op%matvec => matvec
        
        ! direchlet boundary conditions mask
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        ! workspace for the solver
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_pcg) , source = zero_sp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_sp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_pcg_kernel(op,M_,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
        M_ => null()
        workspace_ => null()
        contains
        
        subroutine matvec(x,y,alpha,beta,op)
            use stdlib_linalg_blas, only: gemv
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            call gemv(op,m=size(A,1),n=size(A,2),alpha=alpha,a=A,lda=size(A,1),x=x,incx=1,beta=beta,y=y,incy=1)
            y = merge( zero_sp, y, di_ )
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_sp, x, di_ )
        end subroutine
        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_sp, diagonal * x, di_ ) ! inverted diagonal
        end subroutine
    end subroutine

    module subroutine stdlib_solve_pcg_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
        use stdlib_linalg, only: diag
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional  :: maxiter
        logical, intent(in), optional  :: restart
        integer, intent(in), optional  :: precond 
        class(stdlib_linop_dp_type), optional , intent(in), target :: M 
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_dp_type) :: op
        type(stdlib_linop_dp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(dp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        ! working data for preconditioner
        integer :: precond_
        real(dp), allocatable :: diagonal(:)

        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_dp)
        atol_    = optval(x=atol,    default=epsilon(one_dp))
        precond_ = optval(x=precond, default=pc_none)
        !-------------------------
        ! internal memory setup
        ! preconditioner
        if(present(M)) then
            M_ => M
        else 
            allocate( M_ )
            allocate(diagonal(n),source=zero_dp)

            select case(precond_)
            case(pc_jacobi)
                diagonal = diag(A)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal)>epsilon(zero_dp)) diagonal = one_dp/diagonal
        end if
        ! matvec for the operator
        op%matvec => matvec
        
        ! direchlet boundary conditions mask
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        ! workspace for the solver
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_pcg) , source = zero_dp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_dp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_pcg_kernel(op,M_,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
        M_ => null()
        workspace_ => null()
        contains
        
        subroutine matvec(x,y,alpha,beta,op)
            use stdlib_linalg_blas, only: gemv
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            call gemv(op,m=size(A,1),n=size(A,2),alpha=alpha,a=A,lda=size(A,1),x=x,incx=1,beta=beta,y=y,incy=1)
            y = merge( zero_dp, y, di_ )
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_dp, x, di_ )
        end subroutine
        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_dp, diagonal * x, di_ ) ! inverted diagonal
        end subroutine
    end subroutine

    module subroutine stdlib_solve_pcg_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
        type(CSR_sp_type), intent(in) :: A
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional  :: maxiter
        logical, intent(in), optional  :: restart
        integer, intent(in), optional  :: precond 
        class(stdlib_linop_sp_type), optional , intent(in), target :: M 
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_sp_type) :: op
        type(stdlib_linop_sp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(sp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        ! working data for preconditioner
        integer :: precond_
        real(sp), allocatable :: diagonal(:)

        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_sp)
        atol_    = optval(x=atol,    default=epsilon(one_sp))
        precond_ = optval(x=precond, default=pc_none)
        !-------------------------
        ! internal memory setup
        ! preconditioner
        if(present(M)) then
            M_ => M
        else 
            allocate( M_ )
            allocate(diagonal(n),source=zero_sp)

            select case(precond_)
            case(pc_jacobi)
                call diag(A,diagonal)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal)>epsilon(zero_sp)) diagonal = one_sp/diagonal
        end if
        ! matvec for the operator
        op%matvec => matvec
        
        ! direchlet boundary conditions mask
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        ! workspace for the solver
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_pcg) , source = zero_sp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_sp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_pcg_kernel(op,M_,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
        M_ => null()
        workspace_ => null()
        contains
        
        subroutine matvec(x,y,alpha,beta,op)
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            call spmv( A , x, y , alpha, beta , op)
            y = merge( zero_sp, y, di_ )
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_sp, x, di_ )
        end subroutine
        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_sp, diagonal * x, di_ ) ! inverted diagonal
        end subroutine
    end subroutine

    module subroutine stdlib_solve_pcg_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
        type(CSR_dp_type), intent(in) :: A
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional  :: maxiter
        logical, intent(in), optional  :: restart
        integer, intent(in), optional  :: precond 
        class(stdlib_linop_dp_type), optional , intent(in), target :: M 
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_dp_type) :: op
        type(stdlib_linop_dp_type), pointer :: M_ => null()
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(dp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        ! working data for preconditioner
        integer :: precond_
        real(dp), allocatable :: diagonal(:)

        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_dp)
        atol_    = optval(x=atol,    default=epsilon(one_dp))
        precond_ = optval(x=precond, default=pc_none)
        !-------------------------
        ! internal memory setup
        ! preconditioner
        if(present(M)) then
            M_ => M
        else 
            allocate( M_ )
            allocate(diagonal(n),source=zero_dp)

            select case(precond_)
            case(pc_jacobi)
                call diag(A,diagonal)
                M_%matvec => precond_jacobi
            case default
                M_%matvec => precond_none
            end select
            where(abs(diagonal)>epsilon(zero_dp)) diagonal = one_dp/diagonal
        end if
        ! matvec for the operator
        op%matvec => matvec
        
        ! direchlet boundary conditions mask
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        ! workspace for the solver
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_pcg) , source = zero_dp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_dp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_pcg_kernel(op,M_,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
        M_ => null()
        workspace_ => null()
        contains
        
        subroutine matvec(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            call spmv( A , x, y , alpha, beta , op)
            y = merge( zero_dp, y, di_ )
        end subroutine

        subroutine precond_none(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_dp, x, di_ )
        end subroutine
        subroutine precond_jacobi(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( zero_dp, diagonal * x, di_ ) ! inverted diagonal
        end subroutine
    end subroutine


end submodule stdlib_linalg_iterative_pcg