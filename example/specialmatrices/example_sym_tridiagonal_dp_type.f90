program example_tridiagonal_matrix
   use stdlib_linalg_constants, only: dp
   use stdlib_specialmatrices
   implicit none

   integer, parameter :: n = 5
   type(sym_tridiagonal_dp_type) :: A
   real(dp) :: du(n - 1), dv(n)

   ! Generate random symmteric tridiagonal elements.
   call random_number(du)
   call random_number(dv)

   ! Create the corresponding Tridiagonal matrix.
   A = sym_tridiagonal(du, dv)

end program example_tridiagonal_matrix
