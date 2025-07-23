program example_symtridiagonal_matrix
   use stdlib_linalg_constants, only: dp
   use stdlib_specialmatrices
   implicit none

   integer, parameter :: n = 5
   type(symtridiagonal_dp_type) :: A
   real(dp) :: dv(n), ev(n - 1)

   ! Generate random tridiagonal elements.
   call random_number(dv)
   call random_number(ev)

   ! Create the corresponding Symmetric tridiagonal matrix.
   A = symtridiagonal(dv, ev)

end program example_symtridiagonal_matrix
