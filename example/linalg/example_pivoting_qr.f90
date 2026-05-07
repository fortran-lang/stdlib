program example_pivoting_qr
   use stdlib_linalg, only: qr
   implicit none
   real :: A(104, 32), Q(104, 32), R(32, 32)
   integer :: pivots(32)

   ! Create a random matrix
   call random_number(A)

   ! Compute its QR factorization (reduced)
   call qr(A, Q, R, pivots)

   ! Test factorization: Q*R = A
   print *, maxval(abs(matmul(Q, R) - A(:, pivots)))

end program example_pivoting_qr
