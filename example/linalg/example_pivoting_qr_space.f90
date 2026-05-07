! Pivoting QR example with pre-allocated storage
program example_pivoting_qr_space
   use stdlib_linalg_constants, only: ilp
   use stdlib_linalg, only: qr, qr_space, linalg_state_type
   implicit none
   real :: A(104, 32), Q(104, 32), R(32, 32)
   real, allocatable :: work(:)
   integer(ilp) :: lwork, pivots(32)
   type(linalg_state_type) :: err

   ! Create a random matrix
   call random_number(A)

   ! Prepare QR workspace
   call qr_space(A, lwork, pivoting=.true.)
   allocate (work(lwork))

   ! Compute its QR factorization (reduced)
   call qr(A, Q, R, pivots, storage=work, err=err)

   ! Test factorization: Q*R = A
   print *, maxval(abs(matmul(Q, R) - A(:, pivots)))
   print *, err%print()

end program example_pivoting_qr_space
