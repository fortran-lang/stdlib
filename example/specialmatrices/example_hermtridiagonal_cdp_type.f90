program example_hermtridiagonal_matrix
   use stdlib_linalg_constants, only: dp
   use stdlib_specialmatrices
   implicit none

   integer, parameter :: n = 5
   type(hermtridiagonal_cdp_type) :: A
   complex(dp) :: dv(n), ev(n - 1)
   real(dp) :: data(n, 2)

   ! Generate random tridiagonal elements.
   call random_number(data); dv = cmplx(data(:, 1), 0*data(:, 2), kind=dp)
   call random_number(data); ev = cmplx(data(:n - 1, 1), data(:n - 1, 2), kind=dp)

   ! Create the corresponding Hermitian tridiagonal matrix.
   A = hermtridiagonal(dv, ev)

end program example_hermtridiagonal_matrix
