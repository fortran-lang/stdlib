program example_tridiagonal_matrix
   use stdlib_linalg_constants, only: dp
   use stdlib_specialmatrices, only: tridiagonal_dp_type, tridiagonal, dense, spmv
   implicit none

   integer, parameter :: n = 5
   type(tridiagonal_dp_type) :: A
   real(dp) :: dl(n - 1), dv(n), du(n - 1)
   real(dp) :: x(n), y(n), y_dense(n)
   integer :: i

   ! Create an arbitrary tridiagonal matrix.
   dl = [(i, i=1, n - 1)]; dv = [(2*i, i=1, n)]; du = [(3*i, i=1, n - 1)]
   A = tridiagonal(dl, dv, du)

   ! Initialize vectors.
   x = 1.0_dp; y = 0.0_dp; y_dense = 0.0_dp

   ! Perform matrix-vector products.
   call spmv(A, x, y)
   y_dense = matmul(dense(A), x)

   print *, 'dense       :', y_dense
   print *, 'Tridiagonal :', y

end program example_tridiagonal_matrix
