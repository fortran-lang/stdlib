! Demonstrate expert subroutine interface with pre-allocated arrays
program example_constrained_lstsq2
   use stdlib_linalg_constants, only: dp
   use stdlib_linalg, only: solve_constrained_lstsq, constrained_lstsq_space
   implicit none
   integer, parameter :: m = 5, n = 4, p = 3
   !> Least-squares cost.
   real(dp) :: A(m, n), b(m)
   !> Equality constraints.
   real(dp) :: C(p, n), d(p)
   !> Solution.
   real(dp) :: x(n), x_true(n)
   !> Workspace array.
   integer :: lwork
   real(dp), allocatable :: work(:)

   !> Least-squares cost.
   A(1, :) = [1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp]
   A(2, :) = [1.0_dp, 3.0_dp, 1.0_dp, 1.0_dp]
   A(3, :) = [1.0_dp, -1.0_dp, 3.0_dp, 1.0_dp]
   A(4, :) = [1.0_dp, 1.0_dp, 1.0_dp, 3.0_dp]
   A(5, :) = [1.0_dp, 1.0_dp, 1.0_dp, -1.0_dp]

   b = [2.0_dp, 1.0_dp, 6.0_dp, 3.0_dp, 1.0_dp]

   !> Equality constraints.
   C(1, :) = [1.0_dp, 1.0_dp, 1.0_dp, -1.0_dp]
   C(2, :) = [1.0_dp, -1.0_dp, 1.0_dp, 1.0_dp]
   C(3, :) = [1.0_dp, 1.0_dp, -1.0_dp, 1.0_dp]

   d = [1.0_dp, 3.0_dp, -1.0_dp]

   !> Optimal workspace size.
   call constrained_lstsq_space(A, C, lwork)
   allocate (work(lwork))

   ! Compute the solution.
   call solve_constrained_lstsq(A, b, C, d, x, &
                                storage=work, &
                                overwrite_matrices=.true.)
   x_true = [0.5_dp, -0.5_dp, 1.5_dp, 0.5_dp]

   print *, "Exact solution :"
   print *, x_true
   print *
   print *, "Computed solution :"
   print *, x
end program example_constrained_lstsq2
