module custom_solver
    use stdlib_kinds, only: int8, dp
    use stdlib_sparse, only: CSR_dp_type, spmv, diag
    use stdlib_linalg_iterative_solvers, only: stdlib_linop_dp_type, &
                    stdlib_solver_workspace_dp_type, &
                    stdlib_solve_pcg_kernel, &
                    stdlib_size_wksp_pcg
    use stdlib_optval, only: optval
    implicit none
    private
    public :: stdlib_solve_pcg_custom

contains

    subroutine stdlib_solve_pcg_custom(A,b,x,di,rtol,atol,maxiter,restart,workspace)
        type(CSR_dp_type), intent(in) :: A
        real(dp), intent(in) :: b(:)
        real(dp), intent(inout) :: x(:)
        real(dp), intent(in), optional :: rtol
        real(dp), intent(in), optional :: atol
        logical(int8), intent(in), optional, target  :: di(:)
        integer, intent(in), optional  :: maxiter
        logical, intent(in), optional  :: restart
        type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace
        !-------------------------
        type(stdlib_linop_dp_type) :: op
        type(stdlib_linop_dp_type) :: M
        type(stdlib_solver_workspace_dp_type), pointer :: workspace_
        integer :: n, maxiter_
        real(dp) :: rtol_, atol_
        logical :: restart_
        logical(int8), pointer :: di_(:)
        real(dp), allocatable :: diagonal(:)
        real(dp) :: norm_sq0
        !-------------------------
        n = size(b)
        maxiter_ = optval(x=maxiter, default=n)
        restart_ = optval(x=restart, default=.true.)
        rtol_    = optval(x=rtol,    default=1.e-4_dp)
        atol_    = optval(x=atol,    default=0._dp)
        norm_sq0 = 0._dp
        !-------------------------
        ! internal memory setup
        op%matvec => my_matvec
        op%inner_product => my_dot
        M%matvec => my_jacobi_preconditioner
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
        if(.not.allocated(workspace_%tmp)) allocate( workspace_%tmp(n,stdlib_size_wksp_pcg) , source = 0._dp )
        workspace_%callback => my_logger
        !-------------------------
        ! Jacobi preconditioner factorization
        call diag(A,diagonal)
        where(abs(diagonal)>epsilon(0._dp)) diagonal = 1._dp/diagonal
        !-------------------------
        ! main call to the solver
        call stdlib_solve_pcg_kernel(op,M,b,x,rtol_,atol_,maxiter_,workspace_)

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

        subroutine my_matvec(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            call spmv( A , x, y , alpha, beta , op)
            y = merge( 0._dp, y, di_ )
        end subroutine
        subroutine my_jacobi_preconditioner(x,y,alpha,beta,op)
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
            y = merge( 0._dp, diagonal * x , di_ )
        end subroutine
        real(dp) function my_dot(x,y) result(r)
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: y(:)
            r = dot_product(x,y)
        end function
        subroutine my_logger(x,norm_sq,iter)
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: norm_sq
            integer, intent(in) :: iter
            if(iter == 0) norm_sq0 = norm_sq
            print *, "Iteration: ", iter, " Residual: ", sqrt(norm_sq), " Relative: ", sqrt(norm_sq)/sqrt(norm_sq0)
        end subroutine
    end subroutine
    
end module custom_solver


program example_solve_custom
    use custom_solver
    use stdlib_kinds, only: int8, dp
    use stdlib_sparse, only: CSR_dp_type, COO_dp_type, dense2coo, coo2csr
    implicit none

    type(CSR_dp_type) :: laplacian_csr
    type(COO_dp_type) :: COO
    real(dp) :: laplacian(5,5)
    real(dp) :: x(5), rhs(5)
    logical(int8) :: dirichlet(5)

    laplacian = reshape( [1, -1,  0,  0,  0,&
                         -1,  2, -1,  0,  0,&
                          0, -1,  2, -1,  0,&
                          0,  0, -1,  2, -1,&
                          0,  0,  0, -1,  1] , [5,5])
    call dense2coo(laplacian,COO)
    call coo2csr(COO,laplacian_csr)

    x = 0._dp
    rhs = dble( [0,0,5,0,0] )

    dirichlet = .false._int8
    dirichlet([1,5]) = .true._int8

    call stdlib_solve_pcg_custom(laplacian_csr, rhs, x, rtol=1.d-6, di=dirichlet)
    print *, x !> solution: [0.0, 2.5, 5.0, 2.5, 0.0]
    
end program example_solve_custom