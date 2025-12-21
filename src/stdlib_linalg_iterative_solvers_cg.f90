
submodule(stdlib_linalg_iterative_solvers) stdlib_linalg_iterative_cg
    use stdlib_kinds
    use stdlib_sparse
    use stdlib_constants
    use stdlib_optval, only: optval
    implicit none

contains

    module subroutine stdlib_solve_cg_kernel_sp(A,b,x,rtol,atol,maxiter,workspace)
        class(stdlib_linop_sp_type), intent(in) :: A
        real(sp), intent(in) :: b(:), rtol, atol
        real(sp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter
        type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace
        !-------------------------
        integer :: iter
        real(sp) :: norm_sq, norm_sq_old, norm_sq0
        real(sp) :: alpha, beta, tolsq
        !-------------------------
        iter = 0
        associate(  P  => workspace%tmp(:,1), &
                    R  => workspace%tmp(:,2), &
                    Ap => workspace%tmp(:,3))
            
            norm_sq0 = A%inner_product(B, B)
            if(associated(workspace%callback)) call workspace%callback(x, norm_sq0, iter)

            R = B
            call A%matvec(X, R, alpha= -one_sp, beta=one_sp, op='N') ! R = B - A*X
            norm_sq = A%inner_product(R, R)
   
            P = R
            
            tolsq = max(rtol*rtol * norm_sq0, atol*atol)
            beta = zero_sp
            if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            do while( norm_sq >= tolsq .and. iter < maxiter)
                call A%matvec(P,Ap, alpha= one_sp, beta=zero_sp, op='N') ! Ap = A*P

                alpha = norm_sq / A%inner_product(P, Ap)
                
                X = X + alpha * P
                R = R - alpha * Ap

                norm_sq_old = norm_sq
                norm_sq = A%inner_product(R, R)
                beta = norm_sq / norm_sq_old
                
                P = R + beta * P

                iter = iter + 1

                if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end associate
    end subroutine
    module subroutine stdlib_solve_cg_kernel_dp(A,b,x,rtol,atol,maxiter,workspace)
        class(stdlib_linop_dp_type), intent(in) :: A
        real(dp), intent(in) :: b(:), rtol, atol
        real(dp), intent(inout) :: x(:)
        integer, intent(in) :: maxiter
        type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace
        !-------------------------
        integer :: iter
        real(dp) :: norm_sq, norm_sq_old, norm_sq0
        real(dp) :: alpha, beta, tolsq
        !-------------------------
        iter = 0
        associate(  P  => workspace%tmp(:,1), &
                    R  => workspace%tmp(:,2), &
                    Ap => workspace%tmp(:,3))
            
            norm_sq0 = A%inner_product(B, B)
            if(associated(workspace%callback)) call workspace%callback(x, norm_sq0, iter)

            R = B
            call A%matvec(X, R, alpha= -one_dp, beta=one_dp, op='N') ! R = B - A*X
            norm_sq = A%inner_product(R, R)
   
            P = R
            
            tolsq = max(rtol*rtol * norm_sq0, atol*atol)
            beta = zero_dp
            if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            do while( norm_sq >= tolsq .and. iter < maxiter)
                call A%matvec(P,Ap, alpha= one_dp, beta=zero_dp, op='N') ! Ap = A*P

                alpha = norm_sq / A%inner_product(P, Ap)
                
                X = X + alpha * P
                R = R - alpha * Ap

                norm_sq_old = norm_sq
                norm_sq = A%inner_product(R, R)
                beta = norm_sq / norm_sq_old
                
                P = R + beta * P

                iter = iter + 1

                if(associated(workspace%callback)) call workspace%callback(x, norm_sq, iter)
            end do
        end associate
    end subroutine

    module subroutine stdlib_solve_cg_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
        real(sp), intent(in) :: A(:,:)
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional :: maxiter
        logical, intent(in), optional  :: restart
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_sp_type) :: op
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(sp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_sp)
        atol_    = optval(x=atol,    default=epsilon(one_sp))

        !-------------------------
        ! internal memory setup
        op%matvec => matvec
        ! op%inner_product => default_dot
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_cg), source = zero_sp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_sp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_cg_kernel(op,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
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
    end subroutine

    module subroutine stdlib_solve_cg_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional :: maxiter
        logical, intent(in), optional  :: restart
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_dp_type) :: op
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(dp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_dp)
        atol_    = optval(x=atol,    default=epsilon(one_dp))

        !-------------------------
        ! internal memory setup
        op%matvec => matvec
        ! op%inner_product => default_dot
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_cg), source = zero_dp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_dp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_cg_kernel(op,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
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
    end subroutine

    module subroutine stdlib_solve_cg_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
        type(CSR_sp_type), intent(in) :: A
        real(sp), intent(in) :: b(:)
        real(sp), intent(inout) :: x(:)
        real(sp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional :: maxiter
        logical, intent(in), optional  :: restart
        type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_sp_type) :: op
        type(stdlib_solver_workspace_sp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(sp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_sp)
        atol_    = optval(x=atol,    default=epsilon(one_sp))

        !-------------------------
        ! internal memory setup
        op%matvec => matvec
        ! op%inner_product => default_dot
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_cg), source = zero_sp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_sp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_cg_kernel(op,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
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
    end subroutine

    module subroutine stdlib_solve_cg_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
        type(CSR_dp_type), intent(in) :: A
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol, atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional :: maxiter
        logical, intent(in), optional  :: restart
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_dp_type) :: op
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(dp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-5_dp)
        atol_    = optval(x=atol,    default=epsilon(one_dp))

        !-------------------------
        ! internal memory setup
        op%matvec => matvec
        ! op%inner_product => default_dot
        if(present(di))then
            di_ => di
        else 
            allocate(di_(n),source=.false._int8)
        end if
        
        if(present(workspace)) then
            workspace_ => workspace
        else
            allocate( workspace_ )
        end if
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_cg), source = zero_dp )
        !-------------------------
        ! main call to the solver
        if(restart_) x = zero_dp
        x = merge( b, x, di_ ) ! copy dirichlet load conditions encoded in B and indicated by di
        call stdlib_solve_cg_kernel(op,b,x,rtol_,atol_,maxiter_,workspace_)

        !-------------------------
        ! internal memory cleanup
        if(.not.present(di)) deallocate(di_)
        di_ => null()
        
        if(.not.present(workspace)) then
            deallocate( workspace_%tmp )
            deallocate( workspace_ )
        end if
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
    end subroutine


end submodule stdlib_linalg_iterative_cg