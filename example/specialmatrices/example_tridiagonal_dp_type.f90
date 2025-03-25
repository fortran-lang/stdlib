program example_tridiagonal_matrix
   use stdlib_constants, only: dp
   use stdlib_specialmatrices
   implicit none

   integer, parameter :: n = 5
   type(Tridiagonal_dp_type) :: A
   real(dp) :: dl(n), dv(n), du(n)

   ! Generate random tridiagonal elements.
   call random_number(dl)
   call random_number(dv)
   call random_number(du)

   ! Create the corresponding Tridiagonal matrix.
   A = Tridiagonal(dl, dv, du)

end program example_tridiagonal_matrix
