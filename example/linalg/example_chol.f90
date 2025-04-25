! Cholesky factorization: function interface 
program example_chol
  use stdlib_linalg, only: chol
  implicit none

  real, allocatable, dimension(:,:) :: A,L,U 

  ! Set real matrix
  A = reshape( [ [6, 15, 55], &
                 [15, 55, 225], &
                 [55, 225, 979] ], [3,3] )

  ! Decompose (lower)
  L = chol(A, lower=.true.)

  ! Compare decomposition 
  print *, maxval(abs(A-matmul(L,transpose(L))))

  ! Decompose (upper)
  U = chol(A, lower=.false.)
  
  ! Compare decomposition 
  print *, maxval(abs(A-matmul(transpose(U),U)))

end program example_chol
