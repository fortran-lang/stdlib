!! The `stdlib_linalg_iterative_solvers` module provides interfaces for iterative solvers.
!!
module stdlib_linalg_iterative_solvers
    use stdlib_kinds
    use stdlib_sparse
    implicit none
    private 

    !! workspace sizes: defined by the number of vectors used by the iterative solver.
    enum, bind(c)
        enumerator :: stdlib_size_wksp_cg = 3
        enumerator :: stdlib_size_wksp_pcg = 4
        enumerator :: stdlib_size_wksp_bicgstab = 8
    end enum
    public :: stdlib_size_wksp_cg, stdlib_size_wksp_pcg, stdlib_size_wksp_bicgstab

    enum, bind(c)
        enumerator :: pc_none = 0
        enumerator :: pc_jacobi
    end enum
    public :: pc_none, pc_jacobi
    
    !! version: experimental
    !!
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_linop)
    !!
    !! linop type holding the linear operator and its associated methods.
    !! The `linop` type is used to define the linear operator for the iterative solvers.
    type, public :: stdlib_linop_sp_type
        procedure(vector_sub_sp), nopass, pointer    :: matvec => null()
        procedure(reduction_sub_sp), nopass, pointer :: inner_product => default_dot_sp
    end type
    type, public :: stdlib_linop_dp_type
        procedure(vector_sub_dp), nopass, pointer    :: matvec => null()
        procedure(reduction_sub_dp), nopass, pointer :: inner_product => default_dot_dp
    end type

    !! version: experimental
    !!
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solver_workspace)
    !!
    !! solver_workspace type holding temporal array data for the iterative solvers.
    type, public :: stdlib_solver_workspace_sp_type
        real(sp), allocatable :: tmp(:,:)
        procedure(logger_sub_sp), pointer, nopass :: callback => null()
    end type 

    type, public :: stdlib_solver_workspace_dp_type
        real(dp), allocatable :: tmp(:,:)
        procedure(logger_sub_dp), pointer, nopass :: callback => null()
    end type 


    abstract interface
        subroutine vector_sub_sp(x,y,alpha,beta,op)
            import :: sp
            real(sp), intent(in)  :: x(:)
            real(sp), intent(inout) :: y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        real(sp) function reduction_sub_sp(x,y) result(r)
            import :: sp
            real(sp), intent(in) :: x(:)
            real(sp), intent(in) :: y(:)
        end function
        subroutine logger_sub_sp(x,norm_sq,iter)
            import :: sp
            real(sp), intent(in) :: x(:)
            real(sp), intent(in) :: norm_sq
            integer, intent(in) :: iter
        end subroutine
        subroutine vector_sub_dp(x,y,alpha,beta,op)
            import :: dp
            real(dp), intent(in)  :: x(:)
            real(dp), intent(inout) :: y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        real(dp) function reduction_sub_dp(x,y) result(r)
            import :: dp
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: y(:)
        end function
        subroutine logger_sub_dp(x,norm_sq,iter)
            import :: dp
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: norm_sq
            integer, intent(in) :: iter
        end subroutine
    end interface

    !! version: experimental
    !!
    !! stdlib_solve_cg_kernel interface for the conjugate gradient method.
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_cg_kernel)
    interface stdlib_solve_cg_kernel
        module subroutine stdlib_solve_cg_kernel_sp(A,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_sp_type), intent(in) :: A !! linear operator
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in) :: rtol !! relative tolerance for convergence
            real(sp), intent(in) :: atol !! absolut tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_cg_kernel_dp(A,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_dp_type), intent(in) :: A !! linear operator
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in) :: rtol !! relative tolerance for convergence
            real(dp), intent(in) :: atol !! absolut tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_cg_kernel
    
    !! version: experimental
    !!
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_cg)
    interface stdlib_solve_cg
        module subroutine stdlib_solve_cg_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
            !! linear operator matrix
            real(sp), intent(in) :: A(:,:) 
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_cg_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
            !! linear operator matrix
            real(dp), intent(in) :: A(:,:) 
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_cg_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
            !! linear operator matrix
            type(CSR_sp_type), intent(in) :: A
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_cg_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,workspace)
            !! linear operator matrix
            type(CSR_dp_type), intent(in) :: A
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_cg

    !! version: experimental
    !!
    !! stdlib_solve_pcg_kernel interface for the preconditionned conjugate gradient method.
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_pcg_kernel)
    interface stdlib_solve_pcg_kernel
        module subroutine stdlib_solve_pcg_kernel_sp(A,M,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_sp_type), intent(in) :: A !! linear operator
            class(stdlib_linop_sp_type), intent(in) :: M !! preconditioner linear operator
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in) :: rtol !! relative tolerance for convergence
            real(sp), intent(in) :: atol !! absolute tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_pcg_kernel_dp(A,M,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_dp_type), intent(in) :: A !! linear operator
            class(stdlib_linop_dp_type), intent(in) :: M !! preconditioner linear operator
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in) :: rtol !! relative tolerance for convergence
            real(dp), intent(in) :: atol !! absolute tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_pcg_kernel

    !! version: experimental
    !!
    !! stdlib_solve_bicgstab_kernel interface for the biconjugate gradient stabilized method.
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_bicgstab_kernel)
    interface stdlib_solve_bicgstab_kernel
        module subroutine stdlib_solve_bicgstab_kernel_sp(A,M,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_sp_type), intent(in) :: A !! linear operator
            class(stdlib_linop_sp_type), intent(in) :: M !! preconditioner linear operator
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in) :: rtol !! relative tolerance for convergence
            real(sp), intent(in) :: atol !! absolute tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_sp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_bicgstab_kernel_dp(A,M,b,x,rtol,atol,maxiter,workspace)
            class(stdlib_linop_dp_type), intent(in) :: A !! linear operator
            class(stdlib_linop_dp_type), intent(in) :: M !! preconditioner linear operator
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in) :: rtol !! relative tolerance for convergence
            real(dp), intent(in) :: atol !! absolute tolerance for convergence
            integer, intent(in) :: maxiter !! maximum number of iterations
            type(stdlib_solver_workspace_dp_type), intent(inout) :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_bicgstab_kernel

    !! version: experimental
    !!
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_pcg)
    interface stdlib_solve_pcg
        module subroutine stdlib_solve_pcg_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            real(sp), intent(in) :: A(:,:)
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_sp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_pcg_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            real(dp), intent(in) :: A(:,:)
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_dp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_pcg_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            type(CSR_sp_type), intent(in) :: A
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_sp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_pcg_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            type(CSR_dp_type), intent(in) :: A
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_dp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_pcg 

    !! version: experimental
    !!
    !! [Specifications](../page/specs/stdlib_linalg_iterative_solvers.html#stdlib_solve_bicgstab)
    interface stdlib_solve_bicgstab
        module subroutine stdlib_solve_bicgstab_dense_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            real(sp), intent(in) :: A(:,:)
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_sp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_bicgstab_dense_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            real(dp), intent(in) :: A(:,:)
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_dp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_bicgstab_CSR_sp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            type(CSR_sp_type), intent(in) :: A
            real(sp), intent(in) :: b(:) !! right-hand side vector
            real(sp), intent(inout) :: x(:) !! solution vector and initial guess
            real(sp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(sp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_sp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_sp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
        module subroutine stdlib_solve_bicgstab_CSR_dp(A,b,x,di,rtol,atol,maxiter,restart,precond,M,workspace)
            !! linear operator matrix
            type(CSR_dp_type), intent(in) :: A
            real(dp), intent(in) :: b(:) !! right-hand side vector
            real(dp), intent(inout) :: x(:) !! solution vector and initial guess
            real(dp), intent(in), optional :: rtol !! relative tolerance for convergence
            real(dp), intent(in), optional :: atol !! absolute tolerance for convergence
            logical(int8), intent(in), optional, target  :: di(:) !! dirichlet conditions mask
            integer, intent(in), optional  :: maxiter !! maximum number of iterations
            logical, intent(in), optional :: restart !! restart flag
            integer, intent(in), optional  :: precond !! preconditioner method enumerator
            class(stdlib_linop_dp_type), optional , intent(in), target :: M !! preconditioner linear operator
            type(stdlib_solver_workspace_dp_type), optional, intent(inout), target :: workspace !! workspace for the solver
        end subroutine
    end interface
    public :: stdlib_solve_bicgstab

contains

    !------------------------------------------------------------------
    ! defaults
    !------------------------------------------------------------------
    real(sp) function default_dot_sp(x,y) result(r)
        use stdlib_intrinsics, only: stdlib_dot_product
        real(sp), intent(in) :: x(:)
        real(sp), intent(in) :: y(:)
        r = stdlib_dot_product(x,y)
    end function

    real(dp) function default_dot_dp(x,y) result(r)
        use stdlib_intrinsics, only: stdlib_dot_product
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: y(:)
        r = stdlib_dot_product(x,y)
    end function

    
end module stdlib_linalg_iterative_solvers
